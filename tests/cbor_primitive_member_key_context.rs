#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(feature = "lsp"))]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{
  validate_cbor_from_slice,
  validator::cbor::{Error, ValidationError},
};
use ciborium::value::Value;

fn encode(value: Value) -> Vec<u8> {
  let mut bytes = Vec::new();
  ciborium::ser::into_writer(&value, &mut bytes).unwrap();
  bytes
}

fn one_pair(key: Value, value: Value) -> Value {
  Value::Map(vec![(key, value)])
}

#[test]
fn primitive_identifier_finders_only_run_for_member_keys() {
  let domains = [
    ("tstr", Value::Text("key".into())),
    ("uint", Value::Integer(1.into())),
    ("nint", Value::Integer((-1).into())),
    ("int", Value::Integer(1.into())),
    ("integer", Value::Integer((-1).into())),
    ("unsigned", Value::Integer(1.into())),
    ("float", Value::Float(1.5)),
    ("number", Value::Float(1.5)),
    ("bool", Value::Bool(true)),
    ("null", Value::Null),
    ("bytes", Value::Bytes(vec![0xaa])),
    ("biguint", Value::Tag(2, Box::new(Value::Bytes(vec![1])))),
    ("bignint", Value::Tag(3, Box::new(Value::Bytes(vec![1])))),
    ("bigint", Value::Tag(2, Box::new(Value::Bytes(vec![1])))),
  ];

  for (domain, key) in domains {
    let map = one_pair(key.clone(), Value::Integer(7.into()));

    // RFC 8610 Appendix C requires the data item itself to match the type.
    // A CBOR map (major type 5) is not a primitive value merely because one
    // of its keys belongs to the primitive's domain.
    let result = validate_cbor_from_slice(&format!("m = {domain}"), &encode(map.clone()), None);
    assert!(
      result.is_err(),
      "root {} value searched the enclosing map's keys",
      domain
    );

    let nested = one_pair(Value::Text("outer".into()), map);
    let result =
      validate_cbor_from_slice(&format!("m = {{ outer: {domain} }}"), &encode(nested), None);
    assert!(
      result.is_err(),
      "nested {} value searched its own map's keys",
      domain
    );

    // The same primitive remains a valid map-member key domain.
    let schema = format!("m = {{ {domain} => uint }}");
    let result = validate_cbor_from_slice(
      &schema,
      &encode(one_pair(key, Value::Integer(7.into()))),
      None,
    );
    assert!(
      result.is_ok(),
      "member-key {} was rejected: {:?}",
      domain,
      result
    );
  }
}

#[test]
fn primitive_literal_finders_only_run_for_member_keys() {
  let literals = [
    (r#""key""#, Value::Text("key".into())),
    ("1", Value::Integer(1.into())),
    ("-1", Value::Integer((-1).into())),
    ("1.5", Value::Float(1.5)),
    ("h'AA'", Value::Bytes(vec![0xaa])),
  ];

  for (literal, key) in literals {
    let map = one_pair(key.clone(), Value::Integer(7.into()));
    assert!(
      validate_cbor_from_slice(&format!("m = {literal}"), &encode(map.clone()), None).is_err(),
      "root literal {} searched the enclosing map's keys",
      literal
    );

    let nested = one_pair(Value::Text("outer".into()), map);
    assert!(
      validate_cbor_from_slice(
        &format!("m = {{ outer: {literal} }}"),
        &encode(nested),
        None,
      )
      .is_err(),
      "nested literal {} searched its own map's keys",
      literal
    );

    let result = validate_cbor_from_slice(
      &format!("m = {{ {literal} => uint }}"),
      &encode(one_pair(key, Value::Integer(7.into()))),
      None,
    );
    assert!(
      result.is_ok(),
      "member-key literal {} was rejected",
      literal
    );
  }
}

#[test]
fn repeating_primitive_members_preserve_value_and_key_contexts() {
  let map = one_pair(Value::Text("key".into()), Value::Integer(7.into()));

  assert!(
    validate_cbor_from_slice(
      "m = [+ tstr]",
      &encode(Value::Array(vec![map.clone()])),
      None,
    )
    .is_err(),
    "a repeated ordinary tstr searched the map's keys"
  );
  assert!(
    validate_cbor_from_slice(
      "m = { outer: [+ tstr] }",
      &encode(one_pair(
        Value::Text("outer".into()),
        Value::Array(vec![map]),
      )),
      None,
    )
    .is_err(),
    "a nested repeated ordinary tstr searched its map's keys"
  );

  validate_cbor_from_slice(
    "m = { * tstr => uint }",
    &encode(one_pair(
      Value::Text("key".into()),
      Value::Integer(7.into()),
    )),
    None,
  )
  .unwrap();
}

#[test]
fn map_and_any_values_remain_valid_outside_member_key_context() {
  let map = encode(one_pair(
    Value::Text("key".into()),
    Value::Integer(7.into()),
  ));

  validate_cbor_from_slice("m = any", &map, None).unwrap();
  validate_cbor_from_slice("m = { * any => any }", &map, None).unwrap();
}

#[test]
fn optional_composite_member_misses_skip_their_value_type() {
  let array_key = Value::Array(vec![Value::Integer(1.into())]);
  let map = encode(one_pair(array_key, Value::Text("owned".into())));

  // Once the first optional member owns the only pair, the second member is
  // absent. Its uint value type must not be evaluated against the enclosing
  // map as a surrogate way to discover that absence.
  validate_cbor_from_slice("m = { ? [uint] => tstr, ? [uint] => uint }", &map, None).unwrap();
}

#[test]
fn primitive_member_value_errors_point_to_the_claimed_key() {
  let bad_member = encode(one_pair(
    Value::Text("key".into()),
    Value::Text("bad".into()),
  ));
  let error = validate_cbor_from_slice("m = { tstr => uint }", &bad_member, None).unwrap_err();
  let Error::Validation(errors) = error else {
    panic!("expected validation errors");
  };

  assert!(
    errors
      .iter()
      .any(|error| error.cbor_location == r#"/Text("key")"#),
    "expected a claimed-key location, got {:?}",
    locations(&errors)
  );
  assert!(
    errors
      .iter()
      .all(|error| error.cbor_location != r#"/Text("bad")"#),
    "associated value was used as a path component: {:?}",
    locations(&errors)
  );
}

fn locations(errors: &[ValidationError]) -> Vec<&str> {
  errors
    .iter()
    .map(|error| error.cbor_location.as_str())
    .collect()
}
