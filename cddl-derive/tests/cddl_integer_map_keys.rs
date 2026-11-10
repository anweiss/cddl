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
