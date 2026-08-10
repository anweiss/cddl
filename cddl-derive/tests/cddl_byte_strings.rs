use cddl_derive::cddl_typegen;

// Regression test for https://github.com/anweiss/cddl/issues/638
//
// `bstr` maps to `Vec<u8>`, which serde encodes as an array of integers. The
// generated code now annotates those fields with serde_with so they encode as
// a real CBOR byte string (major type 2).
cddl_typegen!("tests/fixtures/byte_strings.cddl");

fn to_cbor<T: serde::Serialize>(value: &T) -> Vec<u8> {
  let mut buf = Vec::new();
  ciborium::into_writer(value, &mut buf).unwrap();
  buf
}

#[test]
fn bstr_encodes_as_a_cbor_byte_string() {
  let r = Record {
    payload: vec![1, 2, 3],
    label: "hi".to_string(),
  };

  let encoded = to_cbor(&r);

  // 0x43 = major type 2 (byte string), length 3. If the field were encoded as
  // an array of integers this would be 0x83 (major type 4, length 3).
  assert!(
    encoded.windows(4).any(|w| w == [0x43, 0x01, 0x02, 0x03]),
    "expected a byte string header, got {:02x?}",
    encoded
  );

  let decoded: Record = ciborium::from_reader(encoded.as_slice()).unwrap();
  assert_eq!(decoded.payload, vec![1, 2, 3]);
  assert_eq!(decoded.label, "hi");
}

#[test]
fn optional_bstr_round_trips() {
  let some = OptionalRecord {
    payload: Some(vec![9, 9]),
  };
  let decoded: OptionalRecord = ciborium::from_reader(to_cbor(&some).as_slice()).unwrap();
  assert_eq!(decoded.payload, Some(vec![9, 9]));

  let none = OptionalRecord { payload: None };
  let decoded: OptionalRecord = ciborium::from_reader(to_cbor(&none).as_slice()).unwrap();
  assert_eq!(decoded.payload, None);
}

#[test]
fn nested_bstr_round_trips() {
  let n = NestedRecord {
    chunks: vec![vec![1], vec![2, 3]],
    table: std::collections::HashMap::from([("k".to_string(), vec![4u8, 5])]),
  };

  let encoded = to_cbor(&n);
  let decoded: NestedRecord = ciborium::from_reader(encoded.as_slice()).unwrap();

  assert_eq!(decoded.chunks, vec![vec![1], vec![2, 3]]);
  assert_eq!(decoded.table.get("k"), Some(&vec![4u8, 5]));

  // A byte string inside the array must also use major type 2.
  assert!(
    encoded.windows(2).any(|w| w == [0x41, 0x01]),
    "expected a nested byte string header, got {:02x?}",
    encoded
  );
}
