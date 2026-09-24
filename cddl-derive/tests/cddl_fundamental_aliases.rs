use cddl_derive::{cddl, cddl_typegen};

mod enabled {
  use super::*;
  cddl_typegen!(
    "tests/fixtures/fundamental_aliases.cddl",
    fundamental_aliases = true
  );
}

mod disabled {
  use super::*;
  cddl_typegen!(
    "tests/fixtures/fundamental_aliases.cddl",
    fundamental_aliases = false
  );
}

mod attribute {
  use super::*;
  type Created = String;
  type SecondCreated = Created;
  type LocalValue = serde_json::Value;

  #[cddl(
    path = "tests/fixtures/fundamental_aliases.cddl",
    rule = "record",
    fundamental_aliases = true,
    any_type = "LocalValue"
  )]
  struct First;

  #[cddl(
    path = "tests/fixtures/fundamental_aliases.cddl",
    rule = "record",
    fundamental_aliases = true,
    any_type = "LocalValue"
  )]
  struct Second;
}

mod substituted {
  use super::*;
  cddl_typegen!(
    "tests/fixtures/fundamental_aliases.cddl",
    fundamental_aliases = true,
    any_type = "ciborium::Value",
    substitute("created" = "u64", "record.when" = "String")
  );
}

fn json() -> serde_json::Value {
  serde_json::json!({
    "when": "2026-09-24T12:00:00Z",
    "when2": "2026-09-24T12:00:00Z",
    "maybe": "2026-09-24T12:00:00Z",
    "hash": [1, 2], "blobs": [[3, 4]],
    "names": ["name"], "times": ["2026-09-24T12:00:00Z"], "payload": "value"
  })
}

fn round_trip<T: serde::Serialize + serde::de::DeserializeOwned>(
  json: serde_json::Value,
) -> Vec<u8> {
  let record: T = serde_json::from_value(json.clone()).unwrap();
  assert_eq!(serde_json::to_value(&record).unwrap(), json);
  let mut bytes = Vec::new();
  ciborium::into_writer(&record, &mut bytes).unwrap();
  let decoded: T = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(serde_json::to_value(decoded).unwrap(), json);
  bytes
}

#[test]
fn fundamental_aliases_preserve_wire_encoding_in_both_macros() {
  let _: enabled::Bstr = vec![1, 2];
  let _: enabled::Tdate = "date".into();
  let _: enabled::Tstr = "text".into();
  let _: enabled::Any = serde_json::Value::Null;
  let _: attribute::__cddl_prelude_First::Bstr = vec![1, 2];
  for present in [true, false] {
    let mut input = json();
    if !present {
      input.as_object_mut().unwrap().remove("maybe");
    }
    let expected = round_trip::<disabled::Record>(input.clone());
    assert_eq!(round_trip::<enabled::Record>(input.clone()), expected);
    assert_eq!(round_trip::<attribute::First>(input.clone()), expected);
    assert_eq!(round_trip::<attribute::Second>(input), expected);
    let wire: ciborium::Value = ciborium::from_reader(expected.as_slice()).unwrap();
    let fields = wire.as_map().unwrap();
    let value = |name: &str| {
      &fields
        .iter()
        .find(|(k, _)| k.as_text() == Some(name))
        .unwrap()
        .1
    };
    assert!(matches!(value("hash"), ciborium::Value::Bytes(v) if v == &[1, 2]));
    assert!(matches!(value("when"), ciborium::Value::Tag(0, _)));
    assert!(matches!(value("when2"), ciborium::Value::Tag(0, _)));
    assert!(matches!(
      &value("times").as_array().unwrap()[0],
      ciborium::Value::Tag(0, _)
    ));
  }
}

#[test]
fn fundamental_aliases_respect_custom_types_and_substitutions() {
  let _: substituted::Any = ciborium::Value::Null;
  let _: substituted::SecondCreated = 7_u64;
  let record = substituted::Record {
    when: "untagged".into(),
    when2: 7,
    maybe: None,
    hash: vec![1],
    blobs: vec![],
    names: vec![],
    times: vec![9],
    payload: ciborium::Value::Null,
  };
  let mut bytes = Vec::new();
  ciborium::into_writer(&record, &mut bytes).unwrap();
  let wire: ciborium::Value = ciborium::from_reader(bytes.as_slice()).unwrap();
  let fields = wire.as_map().unwrap();
  let value = |name: &str| {
    &fields
      .iter()
      .find(|(k, _)| k.as_text() == Some(name))
      .unwrap()
      .1
  };
  assert!(matches!(value("when"), ciborium::Value::Text(_)));
  assert!(matches!(value("when2"), ciborium::Value::Integer(_)));
  let decoded: substituted::Record = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.when2, 7);
  assert_eq!(decoded.times, vec![9]);
}
