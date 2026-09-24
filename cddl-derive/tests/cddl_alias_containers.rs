use cddl_derive::{cddl, cddl_typegen};

mod generated {
  use super::*;
  cddl_typegen!("tests/fixtures/alias_containers.cddl");
}

mod attribute {
  use super::*;
  type Timestamps = Vec<String>;
  type Tdate = u64;
  type Pairlist = Vec<(Vec<u8>, String)>;
  #[cddl(
    path = "tests/fixtures/alias_containers.cddl",
    rule = "container-record"
  )]
  struct ContainerRecord;
}

fn assert_round_trip<T: serde::Serialize + serde::de::DeserializeOwned>() {
  let json = serde_json::json!({
    "count": 7,
    "times": ["2026-09-24T00:00:00Z"],
    "pairs": [[[1, 2], "2026-09-24T00:00:00Z"]]
  });
  let record: T = serde_json::from_value(json.clone()).unwrap();
  let mut bytes = Vec::new();
  ciborium::into_writer(&record, &mut bytes).unwrap();
  let wire: ciborium::Value = ciborium::from_reader(bytes.as_slice()).unwrap();
  let fields = wire.as_map().unwrap();
  assert!(fields
    .iter()
    .any(|(k, v)| k.as_text() == Some("count") && matches!(v, ciborium::Value::Integer(_))));
  let value = |name: &str| {
    &fields
      .iter()
      .find(|(k, _)| k.as_text() == Some(name))
      .unwrap()
      .1
  };
  assert!(matches!(
    value("times").as_array().unwrap()[0],
    ciborium::Value::Tag(0, _)
  ));
  let pair = value("pairs").as_array().unwrap()[0].as_array().unwrap();
  assert!(matches!(&pair[0], ciborium::Value::Bytes(b) if b == &[1, 2]));
  assert!(matches!(pair[1], ciborium::Value::Tag(0, _)));
  let decoded: T = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(serde_json::to_value(decoded).unwrap(), json);
}

#[test]
fn container_alias_rhs_retains_tag_and_tuple_metadata() {
  assert_round_trip::<generated::ContainerRecord>();
  assert_round_trip::<attribute::ContainerRecord>();
}
