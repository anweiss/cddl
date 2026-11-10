use cddl_derive::cddl_typegen;
use serde_json::json;
use std::collections::HashMap;

cddl_typegen!("tests/fixtures/enum_map_keys.cddl");

mod fundamental_aliases {
  cddl_derive::cddl_typegen!(
    "tests/fixtures/enum_map_keys.cddl",
    fundamental_aliases = true
  );

  #[cddl_derive::cddl(
    path = "tests/fixtures/enum_map_keys.cddl",
    rule = "label",
    fundamental_aliases = true
  )]
  struct ScopedLabel;

  #[test]
  fn generated_aliases_support_hashable_keys_in_both_macros() {
    let mut keys = std::collections::HashMap::new();
    keys.insert(Label::Int(1), true);
    assert_eq!(keys.get(&Label::Int(1)), Some(&true));
    let mut scoped = std::collections::HashMap::new();
    scoped.insert(ScopedLabel::Tstr("key".into()), true);
    assert_eq!(scoped.get(&ScopedLabel::Tstr("key".into())), Some(&true));
  }
}

#[test]
fn enum_keys_support_lookup_replacement_and_cbor_round_trip() {
  let mut map = M {
    entries: HashMap::from([
      (Label::Int(7), json!("integer")),
      (Label::Tstr("7".into()), json!("text")),
    ]),
  };
  assert_eq!(map.entries.get(&Label::Int(7)), Some(&json!("integer")));
  assert_eq!(
    map.entries.insert(Label::Tstr("7".into()), json!(true)),
    Some(json!("text"))
  );
  assert_eq!(map.entries.len(), 2);

  let mut bytes = Vec::new();
  ciborium::into_writer(&map, &mut bytes).unwrap();
  let decoded: M = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.entries, map.entries);
}

#[test]
fn text_enum_keys_round_trip_through_json() {
  let value = json!({"hello": [1, 2]});
  let map: M = serde_json::from_value(value.clone()).unwrap();
  assert_eq!(
    map.entries.get(&Label::Tstr("hello".into())),
    Some(&json!([1, 2]))
  );
  assert_eq!(serde_json::to_value(map).unwrap(), value);
}

#[test]
fn literal_and_mixed_enum_keys_work_without_changing_wire_values() {
  let literal: LiteralMap = serde_json::from_value(json!({"first": 1})).unwrap();
  assert_eq!(literal.entries.get(&LiteralLabel::First), Some(&1));
  assert_eq!(serde_json::to_value(literal).unwrap(), json!({"first": 1}));

  let mixed = MixedMap {
    entries: HashMap::from([(MixedLabel::First, 1), (MixedLabel::Int(2), 3)]),
  };
  let mut bytes = Vec::new();
  ciborium::into_writer(&mixed, &mut bytes).unwrap();
  let decoded: MixedMap = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.entries, mixed.entries);
}

#[test]
fn non_hashable_choices_still_compile_and_deserialize() {
  assert!(matches!(
    serde_json::from_value::<FloatChoice>(json!(1.5)).unwrap(),
    FloatChoice::Float(value) if value == 1.5
  ));
  assert!(matches!(
    serde_json::from_value::<RecordChoice>(json!({"name": "a"})).unwrap(),
    RecordChoice::Record(record) if record.name == "a"
  ));
  assert!(matches!(
    serde_json::from_value::<AnyChoice>(json!({"a": 1})).unwrap(),
    AnyChoice::Any(value) if value == json!({"a": 1})
  ));
}
