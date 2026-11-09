use cddl_derive::cddl_typegen;

cddl_typegen!("tests/fixtures/integer_map_keys.cddl");

#[test]
fn integer_literal_keys_deserialize_to_their_declared_value_types() {
  let bytes = [0xa2, 0x01, 0x07, 0x04, 0x41, 0xaa];
  let decoded: IntegerKeyRecord = ciborium::from_reader(bytes.as_slice()).unwrap();

  assert_eq!(decoded.n1, Some(7));
  assert_eq!(decoded.n4, Some(vec![0xaa]));
}
