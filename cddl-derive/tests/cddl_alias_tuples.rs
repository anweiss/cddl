use cddl_derive::cddl_typegen;

cddl_typegen!("tests/fixtures/alias_tuples.cddl");

#[test]
fn nested_tuple_alias_adapters_preserve_structure_and_encoding() {
  let record = TupleRecord {
    nested: ((vec![1, 2], 7), "2026-09-24T00:00:00Z".into()),
    pairs: vec![(vec![3, 4], 9)],
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
  let nested = value("nested").as_array().unwrap();
  assert!(matches!(&nested[0].as_array().unwrap()[0], ciborium::Value::Bytes(v) if v == &[1, 2]));
  assert!(matches!(nested[1], ciborium::Value::Tag(0, _)));
  assert!(
    matches!(&value("pairs").as_array().unwrap()[0].as_array().unwrap()[0], ciborium::Value::Bytes(v) if v == &[3, 4])
  );
  let decoded: TupleRecord = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(decoded.nested, record.nested);
  assert_eq!(decoded.pairs, record.pairs);
  let json = serde_json::to_string(&record).unwrap();
  let decoded: TupleRecord = serde_json::from_str(&json).unwrap();
  assert_eq!(decoded.nested, record.nested);
  assert_eq!(decoded.pairs, record.pairs);
}
