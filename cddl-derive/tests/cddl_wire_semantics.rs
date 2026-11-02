//! Runtime regressions for codegen issues #856 through #860.
use cddl_derive::{cddl, cddl_typegen};
use serde_json::json;
use std::collections::HashMap;

cddl_typegen!("tests/fixtures/wire_semantics.cddl");

#[test]
fn mixed_literals_round_trip_and_win_over_tstr() {
  for (text, expected) in [("created", Action::Created), ("edited", Action::Edited)] {
    assert_eq!(serde_json::to_value(&expected).unwrap(), json!(text));
    let actual: Action = serde_json::from_value(json!(text)).unwrap();
    assert_eq!(
      std::mem::discriminant(&actual),
      std::mem::discriminant(&expected)
    );
    let mut bytes = Vec::new();
    ciborium::into_writer(&expected, &mut bytes).unwrap();
    assert_eq!(
      ciborium::from_reader::<String, _>(bytes.as_slice()).unwrap(),
      text
    );
    let actual: Action = ciborium::from_reader(bytes.as_slice()).unwrap();
    assert_eq!(
      std::mem::discriminant(&actual),
      std::mem::discriminant(&expected)
    );
  }
  assert!(
    matches!(serde_json::from_value::<Action>(json!("future")).unwrap(), Action::Tstr(s) if s == "future")
  );
  assert!(matches!(
    serde_json::from_value::<ReverseAction>(json!("created")).unwrap(),
    ReverseAction::Created
  ));
  assert!(matches!(
    serde_json::from_value::<Mixed>(json!(3)).unwrap(),
    Mixed::Int(3)
  ));
  assert!(matches!(
    serde_json::from_value::<Mixed>(json!(true)).unwrap(),
    Mixed::Bool(true)
  ));
  assert!(serde_json::from_value::<Mixed>(json!("future")).is_err());
  assert!(serde_json::from_value::<Action>(json!(null)).is_err());
  assert_eq!(
    serde_json::to_value(Escaped::SayHi).unwrap(),
    json!("say \"hi\"")
  );
}

#[test]
fn quoted_arrow_keys_are_named_optional_fields() {
  let value = json!({"name": "widget", "count": 3});
  let record: Record = serde_json::from_value(value.clone()).unwrap();
  assert_eq!(record.name, "widget");
  assert_eq!(record.count, 3);
  assert_eq!(record.display_name, None);
  assert_eq!(serde_json::to_value(record).unwrap(), value);
  let record: Record =
    serde_json::from_value(json!({"name": "w", "count": 3, "display-name": "W"})).unwrap();
  assert_eq!(record.display_name.as_deref(), Some("W"));
  let value = json!({"name": "w", "count": 3, "quoted\"key": "escaped"});
  let record: Record = serde_json::from_value(value.clone()).unwrap();
  assert_eq!(record.quoted_key.as_deref(), Some("escaped"));
  assert_eq!(serde_json::to_value(record).unwrap(), value);
}

#[test]
fn wildcard_entries_are_flat_in_both_directions() {
  let value = json!({"name": "w", "color": "red", "count": 3});
  let record: OpenRecord = serde_json::from_value(value.clone()).unwrap();
  assert_eq!(record.entries.len(), 2);
  assert_eq!(record.entries["color"], json!("red"));
  assert_eq!(serde_json::to_value(record).unwrap(), value);
  let empty: OpenRecord = serde_json::from_value(json!({"name": "w"})).unwrap();
  assert!(empty.entries.is_empty());
  let table: Table = serde_json::from_value(json!({"a": 1})).unwrap();
  assert_eq!(table.entries, HashMap::from([("a".into(), 1)]));
  assert_eq!(serde_json::to_value(table).unwrap(), json!({"a": 1}));
  assert!(serde_json::from_value::<Table>(json!({}))
    .unwrap()
    .entries
    .is_empty());
}

#[test]
fn embedded_acronyms_follow_rust_case_conventions() {
  let _: HashedUriMap = HashedUriMap {
    url: "local".into(),
  };
  assert_eq!(
    Status::AssertionHashedUriMismatch.as_str(),
    "assertion.hashedURI.mismatch"
  );
  assert_eq!(
    Status::AssertionCollectionHashInvalidUri.as_str(),
    "assertion.collectionHash.invalidURI"
  );
}

fn tagged_record() -> TaggedRecord {
  TaggedRecord {
    id: vec![0xaa, 0xbb, 0xcc],
    when: "2026-01-01T00:00:00Z".into(),
    optional_id: None,
    ids: vec![vec![1, 2]],
    nested: vec![3, 4],
  }
}

#[test]
fn explicit_tags_survive_aliases_containers_and_byte_encoding() {
  let record = tagged_record();
  let mut bytes = Vec::new();
  ciborium::into_writer(&record, &mut bytes).unwrap();
  let value: ciborium::Value = ciborium::from_reader(bytes.as_slice()).unwrap();
  let fields = value.as_map().unwrap();
  let field = |key: &str| {
    &fields
      .iter()
      .find(|(k, _)| k.as_text() == Some(key))
      .unwrap()
      .1
  };
  assert_eq!(
    field("id"),
    &ciborium::Value::Tag(37, Box::new(ciborium::Value::Bytes(record.id.clone())))
  );
  assert_eq!(
    field("when"),
    &ciborium::Value::Tag(0, Box::new(ciborium::Value::Text(record.when.clone())))
  );
  assert_eq!(
    field("ids"),
    &ciborium::Value::Array(vec![ciborium::Value::Tag(
      37,
      Box::new(ciborium::Value::Bytes(vec![1, 2]))
    )])
  );
  assert_eq!(
    field("nested"),
    &ciborium::Value::Tag(
      100,
      Box::new(ciborium::Value::Tag(
        37,
        Box::new(ciborium::Value::Bytes(vec![3, 4]))
      ))
    )
  );
  let decoded: TaggedRecord = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.id, record.id);
  assert_eq!(decoded.ids, record.ids);
  assert_eq!(decoded.optional_id, None);
  let value = serde_json::to_value(&record).unwrap();
  assert_eq!(value["id"], json!([170, 187, 204]));
  assert_eq!(value["when"], json!(record.when));
  let decoded: TaggedRecord = serde_json::from_value(value).unwrap();
  assert_eq!(decoded.id, record.id);
}

#[test]
fn optional_explicit_tags_round_trip_and_wrong_tags_are_rejected() {
  let record = TaggedRecord {
    optional_id: Some(vec![9]),
    ..tagged_record()
  };
  let mut bytes = Vec::new();
  ciborium::into_writer(&record, &mut bytes).unwrap();
  let decoded: TaggedRecord = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.optional_id, record.optional_id);
  let mut value: ciborium::Value = ciborium::from_reader(bytes.as_slice()).unwrap();
  let fields = value.as_map_mut().unwrap();
  let id = &mut fields
    .iter_mut()
    .find(|(k, _)| k.as_text() == Some("id"))
    .unwrap()
    .1;
  *id = ciborium::Value::Tag(38, Box::new(ciborium::Value::Bytes(vec![1])));
  bytes.clear();
  ciborium::into_writer(&value, &mut bytes).unwrap();
  assert!(ciborium::from_reader::<TaggedRecord, _>(bytes.as_slice()).is_err());
  let fields = value.as_map_mut().unwrap();
  let id = &mut fields
    .iter_mut()
    .find(|(k, _)| k.as_text() == Some("id"))
    .unwrap()
    .1;
  *id = ciborium::Value::Bytes(vec![1]);
  bytes.clear();
  ciborium::into_writer(&value, &mut bytes).unwrap();
  assert_eq!(
    ciborium::from_reader::<TaggedRecord, _>(bytes.as_slice())
      .unwrap()
      .id,
    vec![1]
  );
}

fn cbor<T: serde::Serialize>(value: &T) -> Vec<u8> {
  let mut bytes = Vec::new();
  ciborium::into_writer(value, &mut bytes).unwrap();
  bytes
}

#[test]
fn proxy_helpers_do_not_shadow_schema_types() {
  assert!(matches!(
    serde_json::from_value::<ChoiceShadow>(json!(3)).unwrap(),
    ChoiceShadow::Choice(3)
  ));
  assert!(matches!(
    serde_json::from_value::<ChoiceShadow>(json!("created")).unwrap(),
    ChoiceShadow::Created
  ));
}

#[test]
fn camel_case_boundaries_after_digits_are_kept() {
  let _ = X509Certificate { serial: 1 };
}

#[test]
fn flattened_entries_keep_cbor_tags() {
  let record = UriEntries {
    name: "n".into(),
    entries: HashMap::from([("home".into(), "https://example.com".into())]),
  };
  let bytes = cbor(&record);
  let value: ciborium::Value = ciborium::from_reader(bytes.as_slice()).unwrap();
  let home = &value
    .as_map()
    .unwrap()
    .iter()
    .find(|(k, _)| k.as_text() == Some("home"))
    .unwrap()
    .1;
  assert!(matches!(home, ciborium::Value::Tag(32, _)));
  let decoded: UriEntries = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.name, "n");
  assert_eq!(decoded.entries, record.entries);

  let uuids = UuidEntries {
    entries: HashMap::from([("a".into(), vec![1, 2])]),
  };
  let bytes = cbor(&uuids);
  let value: ciborium::Value = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(
    value.as_map().unwrap()[0].1,
    ciborium::Value::Tag(37, Box::new(ciborium::Value::Bytes(vec![1, 2])))
  );
  let decoded: UuidEntries = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.entries, uuids.entries);
  let json = serde_json::to_value(&uuids).unwrap();
  assert_eq!(
    serde_json::from_value::<UuidEntries>(json).unwrap().entries,
    uuids.entries
  );
}

#[test]
fn differently_keyed_entries_are_partitioned() {
  let record = TypedEntries {
    entries: HashMap::from([("a".into(), 1)]),
    entries_1: HashMap::from([(2, "b".into())]),
  };
  let decoded: TypedEntries = ciborium::from_reader(cbor(&record).as_slice()).unwrap();
  assert_eq!(decoded.entries, record.entries);
  assert_eq!(decoded.entries_1, record.entries_1);
}

#[test]
fn flattened_structs_report_missing_and_duplicate_fields() {
  let missing = serde_json::from_value::<OpenRecord>(json!({"color": "red"})).unwrap_err();
  assert!(missing.to_string().contains("missing field `name`"));
  let duplicate = serde_json::from_str::<OpenRecord>(r#"{"name":"a","name":"b"}"#).unwrap_err();
  assert!(duplicate.to_string().contains("duplicate field `name`"));
}
#[test]
fn literal_proxies_do_not_capture_typed_maps() {
  let value = json!({"created": null});
  let decoded: Shadowed = serde_json::from_value(value.clone()).unwrap();
  assert!(matches!(decoded, Shadowed::Detail(_)));
  assert_eq!(serde_json::to_value(&decoded).unwrap(), value);
  assert!(matches!(
    serde_json::from_value::<Shadowed>(json!("created")).unwrap(),
    Shadowed::Created
  ));
  // Unknown strings still fail rather than silently becoming a literal.
  assert!(serde_json::from_value::<Shadowed>(json!("other")).is_err());
}

#[test]
fn optional_wildcard_groups_distinguish_absent_from_present() {
  let empty: OptionalEntries = serde_json::from_value(json!({"name": "n"})).unwrap();
  assert_eq!(empty.entries, None);
  let full: OptionalEntries = serde_json::from_value(json!({"name": "n", "a": 1})).unwrap();
  assert_eq!(full.entries, Some(HashMap::from([("a".to_string(), 1)])));
  assert_eq!(
    serde_json::to_value(&full).unwrap(),
    json!({"name": "n", "a": 1})
  );
  let bytes = cbor(&full);
  let decoded: OptionalEntries = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.entries, full.entries);
  let decoded: OptionalEntries = ciborium::from_reader(cbor(&empty).as_slice()).unwrap();
  assert_eq!(decoded.entries, None);
}

#[test]
fn byte_string_keys_round_trip_through_cbor() {
  let table = ByteTable {
    entries: HashMap::from([(vec![1, 2], 3u64)]),
  };
  let bytes = cbor(&table);
  let value: ciborium::Value = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(
    value.as_map().unwrap()[0].0,
    ciborium::Value::Bytes(vec![1, 2])
  );
  let decoded: ByteTable = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.entries, table.entries);
}

mod substituted {
  use super::*;
  cddl_typegen!(
    "tests/fixtures/wire_semantics.cddl",
    substitute("table.entries" = "std::collections::BTreeMap<String, i64>")
  );

  #[test]
  fn substituted_entry_maps_round_trip() {
    let table: Table = serde_json::from_value(json!({"b": 2, "a": 1})).unwrap();
    let ordered: Vec<_> = table.entries.keys().cloned().collect();
    assert_eq!(ordered, vec!["a".to_string(), "b".to_string()]);
    assert_eq!(
      serde_json::to_value(&table).unwrap(),
      json!({"a": 1, "b": 2})
    );
  }
}
mod configured {
  use super::*;
  cddl_typegen!(
    "tests/fixtures/wire_semantics.cddl",
    other_variant = true,
    non_exhaustive = true,
    fundamental_aliases = true
  );

  #[test]
  fn mixed_choices_keep_options_and_known_literal_precedence() {
    assert!(matches!(
      serde_json::from_value::<Mixed>(json!("created")).unwrap(),
      Mixed::Created
    ));
    let other: Mixed = serde_json::from_value(json!("future")).unwrap();
    assert!(matches!(&other, Mixed::Other(s) if s == "future"));
    assert_eq!(serde_json::to_value(other).unwrap(), json!("future"));
    let record: TaggedRecord =
      serde_json::from_value(serde_json::to_value(super::tagged_record()).unwrap()).unwrap();
    let mut bytes = Vec::new();
    ciborium::into_writer(&record, &mut bytes).unwrap();
    let decoded: TaggedRecord = ciborium::from_reader(bytes.as_slice()).unwrap();
    assert_eq!(decoded.id, record.id);
  }
}

mod single_rule {
  use super::*;
  #[cddl(path = "tests/fixtures/wire_semantics.cddl", rule = "tagged-record")]
  struct SingleTagged;

  #[test]
  fn single_rule_generation_preserves_explicit_tags() {
    let value = serde_json::to_value(tagged_record()).unwrap();
    let record: SingleTagged = serde_json::from_value(value).unwrap();
    let mut bytes = Vec::new();
    ciborium::into_writer(&record, &mut bytes).unwrap();
    let decoded: SingleTagged = ciborium::from_reader(bytes.as_slice()).unwrap();
    assert_eq!(decoded.id, record.id);
  }
}
