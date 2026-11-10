use cddl_derive::cddl_typegen;

cddl_typegen!("tests/fixtures/integer_map_keys.cddl");

#[test]
fn integer_literal_keys_deserialize_to_their_declared_value_types() {
  let bytes = [0xa2, 0x01, 0x07, 0x04, 0x41, 0xaa];
  let decoded: IntegerKeyRecord = ciborium::from_reader(bytes.as_slice()).unwrap();

  assert_eq!(decoded.n1, Some(7));
  assert_eq!(decoded.n4, Some(vec![0xaa]));
}

#[test]
fn integer_literal_keys_round_trip_as_cbor_integers() {
  let bytes = [0xa2, 0x01, 0x07, 0x04, 0x41, 0xaa];
  let decoded: IntegerKeyRecord = ciborium::from_reader(bytes.as_slice()).unwrap();
  let mut encoded = Vec::new();
  ciborium::into_writer(&decoded, &mut encoded).unwrap();
  let wire: ciborium::Value = ciborium::from_reader(encoded.as_slice()).unwrap();
  assert_eq!(
    wire,
    ciborium::Value::Map(vec![
      (1.into(), 7.into()),
      (4.into(), ciborium::Value::Bytes(vec![0xaa])),
    ])
  );
  let round_trip: IntegerKeyRecord = ciborium::from_reader(encoded.as_slice()).unwrap();
  assert_eq!(round_trip.n1, decoded.n1);
  assert_eq!(round_trip.n4, decoded.n4);
}

#[test]
fn absent_integer_fields_remain_absent_and_duplicates_are_rejected() {
  let decoded: IntegerKeyRecord = ciborium::from_reader([0xa0].as_slice()).unwrap();
  assert_eq!(decoded.n1, None);
  assert_eq!(decoded.n4, None);
  let mut encoded = Vec::new();
  ciborium::into_writer(&decoded, &mut encoded).unwrap();
  let round_trip: IntegerKeyRecord = ciborium::from_reader(encoded.as_slice()).unwrap();
  assert_eq!(round_trip.n1, None);
  assert_eq!(round_trip.n4, None);
  assert!(
    ciborium::from_reader::<IntegerKeyRecord, _>([0xa2, 0x01, 0x07, 0x01, 0x08].as_slice())
      .is_err()
  );
}

#[test]
fn signed_literal_keys_keep_tags_nullable_values_and_wildcards() {
  let wire = ciborium::Value::Map(vec![
    (
      (-1).into(),
      ciborium::Value::Tag(0, Box::new("2026-11-10T00:00:00Z".into())),
    ),
    (2.into(), ciborium::Value::Bytes(vec![0xaa])),
    (3.into(), ciborium::Value::Null),
    ("extra".into(), "value".into()),
  ]);
  let mut bytes = Vec::new();
  ciborium::into_writer(&wire, &mut bytes).unwrap();
  let decoded: MixedIntegerKeys = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.n_neg1, "2026-11-10T00:00:00Z");
  assert_eq!(decoded.n2, Some(vec![0xaa]));
  assert_eq!(decoded.n3, Some(None));
  assert_eq!(decoded.entries.get("extra").unwrap(), "value");
  let mut encoded = Vec::new();
  ciborium::into_writer(&decoded, &mut encoded).unwrap();
  let round_trip: ciborium::Value = ciborium::from_reader(encoded.as_slice()).unwrap();
  assert_eq!(round_trip, wire);
}

#[test]
fn recursive_named_fields_keep_their_boxing() {
  let value = serde_json::json!({"child": {"child": {}, "nested": 2}, "outer": 1});
  let node: RecursiveKeyNode = serde_json::from_value(value.clone()).unwrap();
  assert_eq!(node.child.as_ref().unwrap().entries["nested"], 2);
  assert_eq!(serde_json::to_value(node).unwrap(), value);
}

#[test]
fn integer_key_inline_choices_have_rust_identifiers() {
  let _ = IntegerMapChoice::N1;
}

#[test]
fn numeric_serializers_do_not_shadow_value_aliases() {
  let mut bytes = Vec::new();
  ciborium::into_writer(&LiteralValue { n1: 7 }, &mut bytes).unwrap();
  let decoded: LiteralValue = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.n1, 7);
  bytes.clear();
  ciborium::into_writer(&ContainerValues { n2: vec![1, 2] }, &mut bytes).unwrap();
  let decoded: ContainerValues = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.n2, vec![1, 2]);
}

#[test]
fn numeric_field_collisions_do_not_change_named_wire_keys() {
  let wire = ciborium::Value::Map(vec![
    (
      ciborium::Value::Integer(1.into()),
      ciborium::Value::Integer(7.into()),
    ),
    (
      ciborium::Value::Text("n1".into()),
      ciborium::Value::Text("text".into()),
    ),
    (
      ciborium::Value::Text("n1_1".into()),
      ciborium::Value::Bool(true),
    ),
  ]);
  let mut bytes = Vec::new();
  ciborium::into_writer(&wire, &mut bytes).unwrap();
  let record: CollidingLiteralNames = ciborium::from_reader(bytes.as_slice()).unwrap();
  bytes.clear();
  ciborium::into_writer(&record, &mut bytes).unwrap();
  let decoded: ciborium::Value = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded, wire);
  let wire = ciborium::Value::Map(vec![("n1".into(), "text".into()), (1.into(), 7.into())]);
  bytes.clear();
  ciborium::into_writer(&wire, &mut bytes).unwrap();
  let record: ReversedLiteralNames = ciborium::from_reader(bytes.as_slice()).unwrap();
  bytes.clear();
  ciborium::into_writer(&record, &mut bytes).unwrap();
  let decoded: ciborium::Value = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded, wire);
}
