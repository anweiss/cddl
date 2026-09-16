#![cfg(feature = "std")]
#![cfg(all(feature = "cbor", feature = "json"))]
#![cfg(not(feature = "lsp"))]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{validate_cbor_from_slice, validate_json_from_str};
use serde_json::{json, Value};

fn assert_validation(schema: &str, value: Value, expected: bool) {
  let mut bytes = Vec::new();
  ciborium::ser::into_writer(&value, &mut bytes).unwrap();

  #[cfg(feature = "additional-controls")]
  let cbor_result = validate_cbor_from_slice(schema, &bytes, None);
  #[cfg(not(feature = "additional-controls"))]
  let cbor_result = validate_cbor_from_slice(schema, &bytes);
  #[cfg(feature = "additional-controls")]
  let json_result = validate_json_from_str(schema, &value.to_string(), None);
  #[cfg(not(feature = "additional-controls"))]
  let json_result = validate_json_from_str(schema, &value.to_string());

  assert!(
    cbor_result.is_ok() == expected && json_result.is_ok() == expected,
    "schema: {}\nvalue: {}\nexpected valid: {}\nCBOR: {:?}\nJSON: {:?}",
    schema,
    value,
    expected,
    cbor_result,
    json_result
  );
}

#[test]
fn group_to_choice_validates_nested_maps_as_types() {
  for schema in [
    // Issue #767: the group-to-choice flag must not turn foo's map fields
    // into another enumeration.
    "root = &(a: { x: foo })\nfoo = { y: uint }",
    "root = &(a: { x: { y: uint } })",
    "root = &(a: wrapper)\nwrapper = { x: foo }\nfoo = { y: uint }",
    "root = &choices\nchoices = (a: { x: foo })\nfoo = { y: uint }",
    "root = &choices<foo>\nchoices<T> = (a: { x: T })\nfoo = { y: uint }",
  ] {
    assert_validation(schema, json!({"x": {"y": 1}}), true);
    for value in [
      json!({"x": {"y": "invalid"}}),
      json!({"x": {}}),
      json!({"x": {"y": 1, "extra": 2}}),
      json!({"x": 1}),
      json!({"y": 1}),
      json!(1),
    ] {
      assert_validation(schema, value, false);
    }
  }
}

#[test]
fn nested_enumerations_preserve_later_group_choices() {
  let schema = "root = &(a: &(zero: 0) // b: 1, c: { x: foo })\nfoo = { y: uint }";

  for value in [json!(0), json!(1), json!({"x": {"y": 1}})] {
    assert_validation(schema, value, true);
  }
  for value in [json!(2), json!({"x": {"y": "invalid"}})] {
    assert_validation(schema, value, false);
  }
}

#[test]
fn group_to_choice_preserves_nested_groups_and_enumerations() {
  let schema = r#"
    root = &(a: { fields })
    fields = (x: foo, kind: &(first: 0, second: 1))
    foo = { y: uint }
  "#;

  for kind in [0, 1] {
    assert_validation(schema, json!({"x": {"y": 1}, "kind": kind}), true);
  }
  assert_validation(schema, json!({"x": {"y": 1}, "kind": 2}), false);
  assert_validation(schema, json!({"x": {"y": 1}}), false);
}
