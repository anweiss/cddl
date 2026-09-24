use cddl_derive::cddl_typegen;
use ciborium::Value;

cddl_typegen!("tests/fixtures/alias_encoding.cddl");

fn cbor<T: serde::Serialize>(value: &T) -> Vec<u8> {
  let mut bytes = Vec::new();
  ciborium::into_writer(value, &mut bytes).unwrap();
  bytes
}

#[test]
fn aliases_preserve_byte_strings_and_tags() {
  let record = Record {
    hash: vec![1, 2],
    when: "2026-09-24T00:00:00Z".into(),
    optional: None,
    hashes: vec![vec![3, 4]],
    links: vec!["https://example.com".into()],
    by_name: [("home".into(), "https://example.com".into())].into(),
    maybe: Some("2026-09-24T00:00:00Z".into()),
  };
  let bytes = cbor(&record);
  let value: Value = ciborium::from_reader(bytes.as_slice()).unwrap();
  let Value::Map(fields) = value else {
    panic!("expected map")
  };
  let field = |key: &str| {
    &fields
      .iter()
      .find(|(k, _)| k == &Value::Text(key.into()))
      .unwrap()
      .1
  };
  assert_eq!(field("hash"), &Value::Bytes(record.hash.clone()));
  assert_eq!(
    field("when"),
    &Value::Tag(0, Box::new(Value::Text(record.when.clone())))
  );
  assert_eq!(
    field("hashes"),
    &Value::Array(vec![Value::Bytes(vec![3, 4])])
  );
  assert_eq!(
    field("links"),
    &Value::Array(vec![Value::Tag(
      32,
      Box::new(Value::Text(record.links[0].clone()))
    )])
  );
  assert_eq!(
    field("by-name"),
    &Value::Map(vec![(
      Value::Text("home".into()),
      Value::Tag(32, Box::new(Value::Text(record.links[0].clone())))
    )])
  );
  assert_eq!(field("maybe"), field("when"));
  let decoded: Record = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.hash, record.hash);
  assert_eq!(decoded.when, record.when);
  assert_eq!(decoded.links, record.links);
  assert_eq!(decoded.by_name, record.by_name);
  assert_eq!(decoded.maybe, record.maybe);
  assert!(decoded.optional.is_none());

  let json = serde_json::to_value(&record).unwrap();
  assert_eq!(json["when"], record.when);
  assert_eq!(json["links"][0], record.links[0]);
  let decoded: Record = serde_json::from_value(json).unwrap();
  assert_eq!(decoded.hash, record.hash);
  let present = Record {
    optional: Some(record.when.clone()),
    ..record
  };
  let decoded: Record = ciborium::from_reader(cbor(&present).as_slice()).unwrap();
  assert_eq!(decoded.optional, present.optional);
}

mod simple {
  use cddl_derive::cddl;
  type Digest = Vec<u8>;
  type Hash = Digest;
  type Created = String;
  type Timestamp = Created;

  #[cddl(path = "tests/fixtures/alias_encoding.cddl", rule = "direct")]
  struct Direct;

  // A separate schema keeps the single-type test independent of generated aliases.
  #[cddl(path = "tests/fixtures/alias_simple.cddl", rule = "record")]
  struct Aliased;

  #[test]
  fn single_type_macro_matches_direct_encoding() {
    let direct = Direct {
      hash: vec![1],
      when: "now".into(),
    };
    let alias = Aliased {
      hash: vec![1],
      when: "now".into(),
    };
    assert_eq!(super::cbor(&direct), super::cbor(&alias));
  }
}
