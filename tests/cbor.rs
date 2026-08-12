#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{
  cddl_from_str,
  validator::{cbor::CBORValidator, cbor_value, validate_cbor_from_slice, Validator},
};
use ciborium::value::Value;
use indoc::indoc;
use serde::{Deserialize, Serialize};
use std::error::Error;

#[rustfmt::skip] 
pub mod cbor {
    // example values from rfc7049 appendix A
    pub const BOOL_FALSE:   &[u8] = b"\xF4";
    pub const BOOL_TRUE:    &[u8] = b"\xF5";
    pub const NULL:         &[u8] = b"\xF6";
    pub const UNDEFINED:    &[u8] = b"\xF7";

    pub const INT_0:        &[u8] = b"\x00";
    pub const INT_1:        &[u8] = b"\x01";
    pub const INT_23:       &[u8] = b"\x17";
    pub const INT_24:       &[u8] = b"\x18\x18";
    pub const NINT_1000:    &[u8] = b"\x39\x03\xe7";  // -1000

    pub const FLOAT_0_0:    &[u8] = b"\xf9\x00\x00";            // #7.25 (f16)
    pub const FLOAT_1_0:    &[u8] = b"\xf9\x3c\x00";            // #7.25 (f16)
    pub const FLOAT_1E5:    &[u8] = b"\xfa\x47\xc3\x50\x00";    // #7.26 (f32)
    pub const FLOAT_1E300:  &[u8] = b"\xfb\x7e\x37\xe4\x3c\x88\x00\x75\x9c"; // #7.27 (f64)

    pub const ARRAY_EMPTY:  &[u8] = b"\x80";              // []
    pub const ARRAY_123:    &[u8] = b"\x83\x01\x02\x03";  // [1,2,3]
    pub const ARRAY_1_23_45:&[u8] = b"\x83\x01\x82\x02\x03\x82\x04\x05";  // [1, [2, 3], [4, 5]]

    pub const TEXT_EMPTY:   &[u8] = b"\x60";
    pub const TEXT_IETF:    &[u8] = b"\x64\x49\x45\x54\x46";
    pub const TEXT_CJK:     &[u8] = b"\x63\xe6\xb0\xb4";    // "水

    pub const BYTES_EMPTY:  &[u8] = b"\x40";
    pub const BYTES_1234:   &[u8] = b"\x44\x01\x02\x03\x04"; // hex 01020304

    // Simple values (major type 7)
    pub const SIMPLE_0:     &[u8] = b"\xe0";        // simple(0) - unassigned
    pub const SIMPLE_19:    &[u8] = b"\xf3";        // simple(19) - unassigned
    pub const SIMPLE_32:    &[u8] = b"\xf8\x20";    // simple(32) - unassigned (two-byte encoding)
    pub const SIMPLE_255:   &[u8] = b"\xf8\xff";    // simple(255) - unassigned (two-byte encoding)
}

// These data structures exist so that we can serialize some more complex
// beyond the RFC examples.
#[derive(Debug, Serialize, Deserialize)]
struct PersonStruct {
  name: String,
  age: u32,
}

#[derive(Debug, Serialize, Deserialize)]
struct PersonTuple(String, u32);

#[derive(Debug, Serialize, Deserialize)]
struct BackwardsTuple(u32, String);

#[derive(Debug, Serialize, Deserialize)]
struct LongTuple(String, u32, u32);

#[derive(Debug, Serialize, Deserialize)]
struct ShortTuple(String);

#[derive(Debug, Serialize, Deserialize)]
struct KitchenSink(String, u32, f64, bool);

#[test]
fn validate_cbor_bool() {
  let cddl_input = r#"thing = true"#;
  validate_cbor_from_slice(cddl_input, cbor::BOOL_TRUE, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::BOOL_FALSE, None).unwrap_err();
  validate_cbor_from_slice(cddl_input, cbor::NULL, None).unwrap_err();
}

#[test]
fn validate_cbor_float() {
  let cddl_input = r#"thing = 0.0"#;
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_0_0, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0, None).unwrap_err();

  let cddl_input = r#"thing = float"#;
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1E5, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1E300, None).unwrap();

  let cddl_input = r#"thing = float16"#;
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0, None).unwrap();

  // "Too small" floats should not cause a validation error.
  // "Canonical CBOR" suggests that floats should be shrunk to the smallest
  // size that can represent the value.  So 1.0 can be stored in 16 bits,
  // even if the CDDL specifies float64.
  let cddl_input = r#"thing = float32"#;
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1E5, None).unwrap();

  let cddl_input = r#"thing = float64"#;
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1E300, None).unwrap();

  // TODO: check that large floats don't validate against a smaller size.
  // E.g. CBOR #7.27 (64-bit) shouldn't validate against "float16" or "float32".
}

#[test]
fn validate_cbor_integer() {
  let cddl_input = r#"thing = 23 / 24"#;
  validate_cbor_from_slice(cddl_input, cbor::INT_23, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::INT_24, None).unwrap();
  let cddl_input = r#"thing = 1"#;
  validate_cbor_from_slice(cddl_input, cbor::NULL, None).unwrap_err();
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0, None).unwrap_err();
  validate_cbor_from_slice(cddl_input, cbor::BOOL_TRUE, None).unwrap_err();
  let cddl_input = r#"thing = int"#;
  validate_cbor_from_slice(cddl_input, cbor::INT_0, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::INT_24, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::NINT_1000, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0, None).unwrap_err();
  let cddl_input = r#"thing = uint"#;
  validate_cbor_from_slice(cddl_input, cbor::INT_0, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::INT_24, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::NINT_1000, None).unwrap_err();
}

#[test]
fn validate_cbor_uint_control_ops() {
  // ensure control ops over uint targets aren't skipped
  const INT_8: &[u8] = b"\x08";
  const INT_255: &[u8] = b"\x18\xff";
  const INT_256: &[u8] = b"\x19\x01\x00";
  for (cddl_input, accept, reject) in [
    ("thing = uint .le 23", cbor::INT_23, cbor::INT_24),
    ("thing = uint .lt 24", cbor::INT_23, cbor::INT_24),
    ("thing = uint .gt 23", cbor::INT_24, cbor::INT_23),
    ("thing = uint .ge 24", cbor::INT_24, cbor::INT_23),
    ("thing = uint .eq 23", cbor::INT_23, cbor::INT_24),
    ("thing = uint .ne 23", cbor::INT_24, cbor::INT_23),
    ("thing = uint .size 1", INT_255, INT_256),
    ("thing = uint .bits 3", INT_8, cbor::INT_23),
  ] {
    validate_cbor_from_slice(cddl_input, accept, None).unwrap();
    validate_cbor_from_slice(cddl_input, reject, None).unwrap_err();
    validate_cbor_from_slice(cddl_input, cbor::NINT_1000, None).unwrap_err();
  }
}

#[test]
fn validate_cbor_textstring() {
  let cddl_input = r#"thing = tstr"#;
  validate_cbor_from_slice(cddl_input, cbor::TEXT_EMPTY, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::TEXT_IETF, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::TEXT_CJK, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::BYTES_EMPTY, None).unwrap_err();
}

#[test]
fn validate_cbor_bytestring() {
  let cddl_input = r#"thing = bstr"#;
  validate_cbor_from_slice(cddl_input, cbor::BYTES_EMPTY, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::BYTES_1234, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::TEXT_EMPTY, None).unwrap_err();
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap_err();
}

#[test]
fn validate_cbor_array() {
  let cddl_input = r#"thing = []"#;
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::NULL, None).unwrap_err();

  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap_err();

  let cddl_input = r#"thing = [1, 2, 3]"#;
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap();
}

#[test]
fn validate_cbor_group() {
  let cddl_input = r#"thing = (* int)"#;
  validate_cbor_from_slice(cddl_input, cbor::INT_0, None).unwrap();
}

#[test]
fn validate_cbor_homogenous_array() {
  let cddl_input = r#"thing = [* int]"#; // zero or more
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap();
  let cddl_input = r#"thing = [+ int]"#; // one or more
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY, None).unwrap_err();
  let cddl_input = r#"thing = [? int]"#; // zero or one
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY, None).unwrap();
  let mut cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&[42], &mut cbor_bytes).unwrap();
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap_err();

  let cddl_input = r#"thing = [* tstr]"#;
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap_err();

  // Alias type.  Note the rule we want to validate must come first.
  let cddl_input = r#"thing = [* zipcode]  zipcode = int"#;
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap();
}

#[test]
fn validate_cbor_array_groups() {
  let cddl_input = r#"thing = [int, (int, int)]"#;
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap();

  let cddl_input = r#"thing = [(int, int, int)]"#;
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap();

  let cddl_input = r#"thing = [* (int)]"#;
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap();

  // Three elements cannot be consumed by repetitions of a two-entry group
  let cddl_input = r#"thing = [* (int, int)]"#;
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap_err();
}

#[test]
fn validate_cbor_array_record() {
  let cddl_input = r#"thing = [a: int, b: int, c: int]"#;
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY, None).unwrap_err();

  let cddl_input = r#"thing = [a: tstr, b: int]"#;

  let input = PersonTuple("Alice".to_string(), 42);
  let mut cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&input, &mut cbor_bytes).unwrap();
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();

  let input = BackwardsTuple(43, "Carol".to_string());
  let mut cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&input, &mut cbor_bytes).unwrap();
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap_err();

  let input = LongTuple("David".to_string(), 44, 45);
  let mut cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&input, &mut cbor_bytes).unwrap();
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap_err();

  let input = ShortTuple("Eve".to_string());
  let mut cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&input, &mut cbor_bytes).unwrap();
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap_err();

  let cddl_input = r#"thing = [a: tstr, b: uint, c: float32, d: bool]"#;

  let input = KitchenSink("xyz".to_string(), 17, 9.9, false);
  let mut cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&input, &mut cbor_bytes).unwrap();
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();

  // FIXME: there isn't any way at present to serialize a struct
  // into a CBOR array. See https://github.com/pyfisch/cbor/issues/107
  // let input = PersonStruct{name: "Bob".to_string(), age: 43};
  // let mut cbor_bytes = Vec::new();
  // ciborium::ser::into_writer(&input, &mut cbor_bytes).unwrap();
  // validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();

  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap_err();
}

#[test]
fn validate_cbor_map() {
  let input = PersonStruct {
    name: "Bob".to_string(),
    age: 43,
  };
  let mut cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&input, &mut cbor_bytes).unwrap();
  let cddl_input = r#"thing = {name: tstr, age: int}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();
  let cddl_input = r#"thing = {name: tstr, ? age: int}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();

  // Ensure that keys are optional if the occurrence is "?" or "*"
  // and required if the occurrence is "+"
  let cddl_input = r#"thing = {name: tstr, age: int, ? minor: bool}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();
  let cddl_input = r#"thing = {name: tstr, age: int, * minor: bool}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();
  let cddl_input = r#"thing = {name: tstr, age: int, + minor: bool}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap_err();

  let cddl_input = r#"thing = {name: tstr, age: tstr}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap_err();

  let cddl_input = r#"thing = {name: tstr}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap_err();

  // "* keytype => valuetype" is the expected syntax for collecting
  // any remaining key/value pairs of the expected type.
  let cddl_input = r#"thing = {* tstr => any}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();
  let cddl_input = r#"thing = {name: tstr, * tstr => any}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();
  let cddl_input = r#"thing = {name: tstr, age: int, * tstr => any}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();
  let cddl_input = r#"thing = {+ tstr => any}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();

  // Should fail because the CBOR input has one entry that can't be
  // collected because the value type doesn't match.
  let cddl_input = r#"thing = {* tstr => int}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap_err();

  // Should fail because the CBOR input has two entries that can't be
  // collected because the key type doesn't match.
  let cddl_input = r#"thing = {* int => any}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap_err();

  let cddl_input = r#"thing = {name: tstr, age: int, minor: bool}"#;
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap_err();

  let cddl_input = r#"thing = {x: int, y: int, z: int}"#;
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap_err();
}

#[test]
fn validate_cbor_map_float_key() {
  // Regression test: float-typed map keys used to match null keys instead
  // of floats in the find-based key-matching paths.
  const MAP_FLOAT_KEY: &[u8] = b"\xa1\xf9\x3e\x00\x01"; // {1.5: 1}
  const MAP_NULL_KEY: &[u8] = b"\xa1\xf6\x01"; // {null: 1}

  let cddl_input = r#"m = { float => uint }"#;
  validate_cbor_from_slice(cddl_input, MAP_FLOAT_KEY, None).unwrap();
  validate_cbor_from_slice(cddl_input, MAP_NULL_KEY, None).unwrap_err();

  let cddl_input = r#"m = { ? float => uint }"#;
  validate_cbor_from_slice(cddl_input, MAP_FLOAT_KEY, None).unwrap();
  validate_cbor_from_slice(cddl_input, MAP_NULL_KEY, None).unwrap_err();
}

#[test]
fn verify_large_tag_values() -> Result<(), Box<dyn Error>> {
  let input = r#"
        thing = #6.8386104246373017956(tstr) / #6.42(tstr)
    "#;

  // Test tag 42 (small tag value)
  let test_str = "test";
  let cbor = Value::Tag(42, Box::new(Value::Text(test_str.to_string())));
  let mut bytes = Vec::new();
  ciborium::ser::into_writer(&cbor, &mut bytes)?;
  assert!(validate_cbor_from_slice(input, &bytes, None).is_ok());

  // Test tag 8386104246373017956 (large tag value)
  let cbor = Value::Tag(
    8386104246373017956,
    Box::new(Value::Text(test_str.to_string())),
  );
  let mut bytes = Vec::new();
  ciborium::ser::into_writer(&cbor, &mut bytes)?;
  assert!(validate_cbor_from_slice(input, &bytes, None).is_ok());

  // Test wrong tag value - should fail
  let cbor = Value::Tag(99, Box::new(Value::Text(test_str.to_string())));
  let mut bytes = Vec::new();
  ciborium::ser::into_writer(&cbor, &mut bytes)?;
  assert!(validate_cbor_from_slice(input, &bytes, None).is_err());

  Ok(())
}

#[test]
fn validate_range_operators() -> Result<(), Box<dyn Error>> {
  let cddl = indoc!(
    r#"
        test = {
            inclusive: 5..10,      ; inclusive-inclusive range 
            exclusive: 5...10,     ; inclusive-exclusive range (per RFC 8610)
        }
        "#
  );

  let cddl = cddl_from_str(cddl, true)?;

  // Test inclusive range (..) and inclusive-exclusive range (...)
  let test = Value::Map(vec![
    (
      Value::Text("inclusive".to_string()),
      Value::Integer(5.into()),
    ),
    (
      Value::Text("exclusive".to_string()),
      Value::Integer(5.into()),
    ),
  ]);
  let test: cbor_value::Value = test.into();
  let mut cv = CBORValidator::new(&cddl, test, None);
  cv.validate()?;

  let test = Value::Map(vec![
    (
      Value::Text("inclusive".to_string()),
      Value::Integer(10.into()),
    ),
    (
      Value::Text("exclusive".to_string()),
      Value::Integer(9.into()),
    ),
  ]);
  let test: cbor_value::Value = test.into();
  let mut cv = CBORValidator::new(&cddl, test, None);
  cv.validate()?;

  // Test fail cases
  let test = Value::Map(vec![
    (
      Value::Text("inclusive".to_string()),
      Value::Integer(10.into()),
    ),
    (
      Value::Text("exclusive".to_string()),
      Value::Integer(10.into()),
    ), // Should fail - 10 is exclusive
  ]);
  let test: cbor_value::Value = test.into();
  let mut cv = CBORValidator::new(&cddl, test, None);
  assert!(
    cv.validate().is_err(),
    "10 should fail inclusive-exclusive range 5...10"
  );

  let test = Value::Map(vec![
    (
      Value::Text("inclusive".to_string()),
      Value::Integer(4.into()),
    ), // Should fail - 4 is out of range
    (
      Value::Text("exclusive".to_string()),
      Value::Integer(5.into()),
    ),
  ]);
  let test: cbor_value::Value = test.into();
  let mut cv = CBORValidator::new(&cddl, test, None);
  assert!(
    cv.validate().is_err(),
    "4 should fail inclusive range 5..10"
  );

  Ok(())
}

#[test]
fn validate_cbor_size_range_with_constant() -> Result<(), Box<dyn Error>> {
  let cddl_input = r#"
        person = {name: tstr .size (1..max_tstr_length), age: uint}
        max_tstr_length = 100
    "#;

  // --- Positive Test (name length within range) ---
  let valid_person = Value::Map(vec![
    (
      Value::Text("name".to_string()),
      Value::Text("Alice".to_string()),
    ), // Length 5
    (Value::Text("age".to_string()), Value::Integer(30.into())),
  ]);
  let mut valid_cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&valid_person, &mut valid_cbor_bytes)?;
  validate_cbor_from_slice(cddl_input, &valid_cbor_bytes, None)
    .expect("Validation should succeed for name length within range");

  // --- Positive Test (name length at max boundary) ---
  let max_len_name = "a".repeat(100);
  let valid_person_max = Value::Map(vec![
    (Value::Text("name".to_string()), Value::Text(max_len_name)), // Length 100
    (Value::Text("age".to_string()), Value::Integer(30.into())),
  ]);
  let mut valid_max_cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&valid_person_max, &mut valid_max_cbor_bytes)?;
  validate_cbor_from_slice(cddl_input, &valid_max_cbor_bytes, None)
    .expect("Validation should succeed for name length at max boundary");

  // --- Negative Test (name length exceeds range) ---
  let long_name = "a".repeat(101);
  let invalid_person_long = Value::Map(vec![
    (Value::Text("name".to_string()), Value::Text(long_name)), // Length 101
    (Value::Text("age".to_string()), Value::Integer(30.into())),
  ]);
  let mut invalid_long_cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&invalid_person_long, &mut invalid_long_cbor_bytes)?;
  validate_cbor_from_slice(cddl_input, &invalid_long_cbor_bytes, None)
    .expect_err("Validation should fail for name length exceeding range");

  // --- Negative Test (name length below range - zero length) ---
  let empty_name = "";
  let invalid_person_empty = Value::Map(vec![
    (
      Value::Text("name".to_string()),
      Value::Text(empty_name.to_string()),
    ), // Length 0
    (Value::Text("age".to_string()), Value::Integer(30.into())),
  ]);
  let mut invalid_empty_cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&invalid_person_empty, &mut invalid_empty_cbor_bytes)?;
  validate_cbor_from_slice(cddl_input, &invalid_empty_cbor_bytes, None)
    .expect_err("Validation should fail for zero-length name");

  Ok(())
}

/// Test for GitHub issue #90: CBOR validation fails for non-standard simple values.
/// CDDL `#7.N` should match CBOR simple value N for unassigned simple values (0-19, 32-255).
#[test]
fn validate_cbor_simple_values() {
  // Simple value 32 (unassigned, two-byte encoded as 0xf8 0x20)
  let cddl_input = r#"thing = #7.32"#;
  validate_cbor_from_slice(cddl_input, cbor::SIMPLE_32, None).unwrap();

  // Wrong simple value should fail
  validate_cbor_from_slice(cddl_input, cbor::SIMPLE_255, None).unwrap_err();

  // Simple value 0 (one-byte encoded)
  let cddl_input = r#"thing = #7.0"#;
  validate_cbor_from_slice(cddl_input, cbor::SIMPLE_0, None).unwrap();

  // Simple value 19 (one-byte encoded)
  let cddl_input = r#"thing = #7.19"#;
  validate_cbor_from_slice(cddl_input, cbor::SIMPLE_19, None).unwrap();

  // Simple value 255 (two-byte encoded)
  let cddl_input = r#"thing = #7.255"#;
  validate_cbor_from_slice(cddl_input, cbor::SIMPLE_255, None).unwrap();

  // Major type 7 without constraint should match any simple value
  let cddl_input = r#"thing = #7"#;
  validate_cbor_from_slice(cddl_input, cbor::SIMPLE_0, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::SIMPLE_32, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::SIMPLE_255, None).unwrap();

  // #7 should also match booleans, null, and floats
  validate_cbor_from_slice(cddl_input, cbor::BOOL_TRUE, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::BOOL_FALSE, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::NULL, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0, None).unwrap();

  // Standard simple values via #7.N
  let cddl_input = r#"thing = #7.20"#;
  validate_cbor_from_slice(cddl_input, cbor::BOOL_FALSE, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::BOOL_TRUE, None).unwrap_err();

  let cddl_input = r#"thing = #7.21"#;
  validate_cbor_from_slice(cddl_input, cbor::BOOL_TRUE, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::BOOL_FALSE, None).unwrap_err();

  let cddl_input = r#"thing = #7.22"#;
  validate_cbor_from_slice(cddl_input, cbor::NULL, None).unwrap();
  validate_cbor_from_slice(cddl_input, cbor::BOOL_TRUE, None).unwrap_err();
}

/// Regression test for https://github.com/anweiss/cddl/issues/465
#[test]
fn validate_cbor_array_record_extra_elements() {
  let cddl_input = r#"thing = [a: tstr, b: int]"#;

  // Exact match: ["testString", 1] should pass
  let input = PersonTuple("testString".to_string(), 1);
  let mut cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&input, &mut cbor_bytes).unwrap();
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap();

  // Extra element: ["testString", 1, 2] must fail
  let input = LongTuple("testString".to_string(), 1, 2);
  let mut cbor_bytes = Vec::new();
  ciborium::ser::into_writer(&input, &mut cbor_bytes).unwrap();
  validate_cbor_from_slice(cddl_input, &cbor_bytes, None).unwrap_err();
}

#[test]
fn validate_decfrac_and_bigfloat() -> Result<(), Box<dyn Error>> {
  // Helper to encode a ciborium Value to CBOR bytes
  fn cbor_encode(val: &Value) -> Vec<u8> {
    let mut bytes = Vec::new();
    ciborium::ser::into_writer(val, &mut bytes).unwrap();
    bytes
  }

  // Valid decfrac: Tag(4, [int, integer]) e.g. 273.15 = 27315 * 10^(-2)
  let cddl_input = r#"temperature = decfrac"#;
  let cbor_val = Value::Tag(
    4,
    Box::new(Value::Array(vec![
      Value::Integer((-2).into()),
      Value::Integer(27315.into()),
    ])),
  );
  let bytes = cbor_encode(&cbor_val);
  validate_cbor_from_slice(cddl_input, &bytes, None)?;

  // Valid bigfloat: Tag(5, [int, integer]) e.g. 1.5 = 3 * 2^(-1)
  let cddl_input = r#"measurement = bigfloat"#;
  let cbor_val = Value::Tag(
    5,
    Box::new(Value::Array(vec![
      Value::Integer((-1).into()),
      Value::Integer(3.into()),
    ])),
  );
  let bytes = cbor_encode(&cbor_val);
  validate_cbor_from_slice(cddl_input, &bytes, None)?;

  // Invalid: wrong tag for decfrac (tag 5 instead of 4)
  let cddl_input = r#"temperature = decfrac"#;
  let cbor_val = Value::Tag(
    5,
    Box::new(Value::Array(vec![
      Value::Integer((-2).into()),
      Value::Integer(27315.into()),
    ])),
  );
  let bytes = cbor_encode(&cbor_val);
  assert!(validate_cbor_from_slice(cddl_input, &bytes, None).is_err());

  // Invalid: wrong tag for bigfloat (tag 4 instead of 5)
  let cddl_input = r#"measurement = bigfloat"#;
  let cbor_val = Value::Tag(
    4,
    Box::new(Value::Array(vec![
      Value::Integer((-1).into()),
      Value::Integer(3.into()),
    ])),
  );
  let bytes = cbor_encode(&cbor_val);
  assert!(validate_cbor_from_slice(cddl_input, &bytes, None).is_err());

  // Invalid: not an array inside tag
  let cddl_input = r#"temperature = decfrac"#;
  let cbor_val = Value::Tag(4, Box::new(Value::Integer(42.into())));
  let bytes = cbor_encode(&cbor_val);
  assert!(validate_cbor_from_slice(cddl_input, &bytes, None).is_err());

  // Invalid: array with wrong types (float instead of int for exponent)
  let cddl_input = r#"temperature = decfrac"#;
  let cbor_val = Value::Tag(
    4,
    Box::new(Value::Array(vec![
      Value::Float(1.5),
      Value::Integer(27315.into()),
    ])),
  );
  let bytes = cbor_encode(&cbor_val);
  assert!(validate_cbor_from_slice(cddl_input, &bytes, None).is_err());

  // Valid: using with explicit tag notation
  let cddl_input = r#"mytype = #6.4([int, integer])"#;
  let cbor_val = Value::Tag(
    4,
    Box::new(Value::Array(vec![
      Value::Integer((-2).into()),
      Value::Integer(27315.into()),
    ])),
  );
  let bytes = cbor_encode(&cbor_val);
  validate_cbor_from_slice(cddl_input, &bytes, None)?;

  // Valid: bigfloat with bignum mantissa (tag 2 biguint)
  let cddl_input = r#"big_measurement = bigfloat"#;
  let cbor_val = Value::Tag(
    5,
    Box::new(Value::Array(vec![
      Value::Integer((-1).into()),
      Value::Tag(2, Box::new(Value::Bytes(vec![0x01, 0x00]))),
    ])),
  );
  let bytes = cbor_encode(&cbor_val);
  validate_cbor_from_slice(cddl_input, &bytes, None)?;

  Ok(())
}

#[test]
fn validate_optional_type_domain_map_entry_absent() -> Result<(), Box<dyn Error>> {
  // `?` permits the entry to be absent (RFC 8610 §3.2), so the empty map is
  // valid against a map whose sole entry is a `?`-marked type-domain entry
  let empty_map = b"\xa0";
  let one_entry = b"\xa1\x61\x61\x01"; // {"a": 1}

  validate_cbor_from_slice(r#"m = { ? tstr => uint }"#, empty_map, None)?;
  // present entry still validates
  validate_cbor_from_slice(r#"m = { ? tstr => uint }"#, one_entry, None)?;
  // present entry with a bad value type still fails
  let bad_value = b"\xa1\x61\x61\x61\x62"; // {"a": "b"}
  assert!(validate_cbor_from_slice(r#"m = { ? tstr => uint }"#, bad_value, None).is_err());

  // other key types take the same code path
  validate_cbor_from_slice(r#"m = { ? int => uint }"#, empty_map, None)?;
  validate_cbor_from_slice(r#"m = { ? bool => uint }"#, empty_map, None)?;
  validate_cbor_from_slice(r#"m = { ? bytes => uint }"#, empty_map, None)?;

  // an occurrence-less entry must still require a match
  assert!(validate_cbor_from_slice(r#"m = { tstr => uint }"#, empty_map, None).is_err());

  // absent optional entry alongside other entries that do consume the keys
  let int_entry = b"\xa1\x01\x02"; // {1: 2}
  let text_and_int = b"\xa2\x61\x61\x01\x01\x02"; // {"a": 1, 1: 2}
  validate_cbor_from_slice(r#"m = { ? tstr => uint, int => int }"#, int_entry, None)?;
  validate_cbor_from_slice(r#"m = { ? tstr => uint, ? int => int }"#, int_entry, None)?;
  validate_cbor_from_slice(r#"m = { ? tstr => uint, int => int }"#, text_and_int, None)?;
  validate_cbor_from_slice(r#"m = { ? tstr => uint, ? int => int }"#, empty_map, None)?;

  Ok(())
}

#[test]
fn validate_single_type_domain_map_entries_do_not_reuse_consumed_keys() {
  let name_only = b"\xa1\x64name\x63bob"; // {"name": "bob"}
  let name_and_a = b"\xa2\x64name\x63bob\x61a\x05"; // {"name": "bob", "a": 5}
  let name_and_bad_a = b"\xa2\x64name\x63bob\x61a\x63bad"; // {"name": "bob", "a": "bad"}

  let optional = r#"m = { name: tstr, ? tstr => uint }"#;
  validate_cbor_from_slice(optional, name_only, None).unwrap();
  validate_cbor_from_slice(optional, name_and_a, None).unwrap();
  validate_cbor_from_slice(optional, name_and_bad_a, None).unwrap_err();

  // Occurrence-less entries use the same finder, but must still require one
  // unconsumed matching key.
  let required = r#"m = { name: tstr, tstr => uint }"#;
  validate_cbor_from_slice(required, name_and_a, None).unwrap();
  validate_cbor_from_slice(required, name_only, None).unwrap_err();
}

#[test]
fn validate_optional_type_domain_miss_skips_composite_value() {
  let empty_map = b"\xa0";
  let name_only = b"\xa1\x64name\x63bob"; // {"name": "bob"}

  // A missing optional member skips the entire entry. Its value type must not
  // be evaluated against the enclosing map or a previously consumed value.
  validate_cbor_from_slice(r#"m = { ? tstr => [uint] }"#, empty_map, None).unwrap();
  validate_cbor_from_slice(r#"m = { ? any => [uint] }"#, empty_map, None).unwrap();
  validate_cbor_from_slice(r#"m = { name: tstr, ? tstr => [uint] }"#, name_only, None).unwrap();
}

#[test]
fn validate_optional_primitive_domains_do_not_reuse_consumed_keys() {
  let cases = [
    (
      r#"m = { ? tstr => tstr, ? tstr => uint }"#,
      Value::Text("k".into()),
    ),
    (
      r#"m = { ? int => tstr, ? int => uint }"#,
      Value::Integer(1.into()),
    ),
    (
      r#"m = { ? bool => tstr, ? bool => uint }"#,
      Value::Bool(true),
    ),
    (r#"m = { ? null => tstr, ? null => uint }"#, Value::Null),
    (
      r#"m = { ? bytes => tstr, ? bytes => uint }"#,
      Value::Bytes(vec![1]),
    ),
    (
      r#"m = { ? float => tstr, ? float => uint }"#,
      Value::Float(1.5),
    ),
    (
      r#"m = { ? biguint => tstr, ? biguint => uint }"#,
      Value::Tag(2, Box::new(Value::Bytes(vec![1]))),
    ),
    (
      r#"m = { ? bignint => tstr, ? bignint => uint }"#,
      Value::Tag(3, Box::new(Value::Bytes(vec![1]))),
    ),
  ];

  for (schema, key) in cases {
    let mut bytes = Vec::new();
    ciborium::ser::into_writer(
      &Value::Map(vec![(key, Value::Text("owned".into()))]),
      &mut bytes,
    )
    .unwrap();

    let result = validate_cbor_from_slice(schema, &bytes, None);
    assert!(result.is_ok(), "schema {} failed: {:?}", schema, result);
  }

  // Bignum finders must treat a genuine optional miss like every other
  // primitive domain; occurrence-less bignum entries remain required.
  let empty_map = b"\xa0";
  validate_cbor_from_slice(r#"m = { ? biguint => uint }"#, empty_map, None).unwrap();
  validate_cbor_from_slice(r#"m = { ? bignint => uint }"#, empty_map, None).unwrap();
  validate_cbor_from_slice(r#"m = { biguint => uint }"#, empty_map, None).unwrap_err();
  validate_cbor_from_slice(r#"m = { bignint => uint }"#, empty_map, None).unwrap_err();
}

#[test]
fn validate_single_type_domain_map_entries_do_not_hide_equivalent_pairs() {
  let schema = r#"m = { ? tstr => tstr, ? tstr => uint }"#;

  // This duplicate-key map is invalid CBOR. Even without uniform
  // validity-boundary rejection, the second pair must remain uncovered rather
  // than disappear behind the first claim.
  let duplicate_text_keys = b"\xa2\x61a\x61x\x61a\x63bad";
  let error = validate_cbor_from_slice(schema, duplicate_text_keys, None).unwrap_err();
  assert!(error.to_string().contains("unexpected key"));

  // A claim for the first text pair must not make an equivalent second pair
  // count as consumed when the following optional member has a disjoint key
  // domain.
  let error = validate_cbor_from_slice(
    r#"m = { ? tstr => tstr, ? int => uint }"#,
    duplicate_text_keys,
    None,
  )
  .unwrap_err();
  assert!(error.to_string().contains("unexpected key"));

  let error = validate_cbor_from_slice(
    r#"m = { a: tstr, ? tstr => uint }"#,
    duplicate_text_keys,
    None,
  )
  .unwrap_err();
  assert!(error.to_string().contains("unexpected key"));

  // A literal lookup must allocate a still-unconsumed equivalent pair rather
  // than adding a second claim for the first pair. Otherwise the final
  // optional entry can mistake two claims for consumption of both pairs and
  // silently hide this invalid byte-string value.
  let duplicate_after_prior_claim = b"\xa2\x61a\x61x\x61a\x41z";
  let error = validate_cbor_from_slice(
    r#"m = { ? tstr => tstr, a: tstr, ? tstr => uint }"#,
    duplicate_after_prior_claim,
    None,
  )
  .unwrap_err();
  assert!(error.to_string().contains("expected type tstr"));

  // A generic child that validates the same map must propagate both claim
  // ledgers. Otherwise the following literal reclaims the child's first pair
  // and the final primitive member mistakes two claims for both physical
  // pairs, reproducing the branch-worsened false accept.
  let error = validate_cbor_from_slice(
    "g<K, V> = (K => V)\nm = { g<tstr, tstr>, a: tstr, ? tstr => uint }",
    duplicate_after_prior_claim,
    None,
  )
  .unwrap_err();
  assert!(error.to_string().contains("expected type tstr"));

  // Once the only pair has been claimed, a later optional literal is absent.
  // A later required literal cannot reuse that pair.
  let one_claimed_literal = b"\xa1\x61a\x61x";
  validate_cbor_from_slice(
    r#"m = { ? tstr => tstr, ? a: uint }"#,
    one_claimed_literal,
    None,
  )
  .unwrap();
  validate_cbor_from_slice(
    r#"m = { ? tstr => tstr, a: uint }"#,
    one_claimed_literal,
    None,
  )
  .unwrap_err();

  // RFC 8949 treats +0 and -0 as equivalent map keys, so this is also an
  // invalid duplicate-key map. The second pair must remain uncovered.
  let equivalent_float_keys = b"\xa2\xf9\x00\x00\x61x\xf9\x80\x00\x63bad";
  let error = validate_cbor_from_slice(
    r#"m = { ? float => tstr, ? float => uint }"#,
    equivalent_float_keys,
    None,
  )
  .unwrap_err();
  assert!(error.to_string().contains("unexpected key"));

  // A generic child shares the parent's physical ledger, so it selects the
  // second pair and exposes that pair's incompatible byte-string value.
  let error = validate_cbor_from_slice(
    "g<K, V> = (K => V)\nm = { ? tstr => tstr, g<tstr, tstr>, ? tstr => uint }",
    duplicate_after_prior_claim,
    None,
  )
  .unwrap_err();
  assert!(
    error.to_string().contains("expected type tstr"),
    "{}",
    error
  );
}

#[test]
fn validate_nan_map_keys_have_physical_pair_identity() {
  let one_nan_text = b"\xa1\xf9\x7e\x00\x61x"; // {NaN: "x"}

  // RFC 8610 Appendix C matches a map by assigning its physical key/value
  // pairs to group entries. A claimed pair remains claimed even though its
  // IEEE NaN key is not reflexively equal in Rust. Encoding width does not
  // affect the identity of the one physical pair.
  let nan_encodings: &[&[u8]] = &[
    one_nan_text,
    b"\xa1\xfa\x7f\xc0\x00\x00\x61x",
    b"\xa1\xfb\x7f\xf8\x00\x00\x00\x00\x00\x00\x61x",
  ];
  for encoded in nan_encodings {
    validate_cbor_from_slice(r#"m = { ? float => tstr }"#, encoded, None).unwrap();
    validate_cbor_from_slice(r#"m = { ? float => tstr, ? float => uint }"#, encoded, None).unwrap();
  }

  // Repeating entries use the same ownership ledger. The first member owns
  // the only pair, leaving width zero for the following repetition.
  validate_cbor_from_slice(r#"m = { * float => tstr }"#, one_nan_text, None).unwrap();
  validate_cbor_from_slice(
    r#"m = { ? float => tstr, * float => uint }"#,
    one_nan_text,
    None,
  )
  .unwrap();
  validate_cbor_from_slice(
    r#"m = { ? float => tstr, + float => uint }"#,
    one_nan_text,
    None,
  )
  .unwrap_err();

  // RFC 8949 Section 5.6.1 makes NaNs with different significands distinct
  // map keys. These two half-precision payloads therefore form a valid map
  // and must be counted as two physical occurrences.
  let two_distinct_nans = b"\xa2\xf9\x7e\x00\x01\xf9\x7e\x01\x02";
  validate_cbor_from_slice(r#"m = { 2*2 float => uint }"#, two_distinct_nans, None).unwrap();

  // Ordinary reflexive float keys remain a control for the same paths.
  let finite_float = b"\xa1\xf9\x3e\x00\x61x"; // {1.5: "x"}
  validate_cbor_from_slice(
    r#"m = { ? float => tstr, ? float => uint }"#,
    finite_float,
    None,
  )
  .unwrap();

  // Owning the NaN pair must not hide a genuinely different, unclaimed pair
  // from the final closed-map check.
  let nan_and_integer = b"\xa2\xf9\x7e\x00\x61x\x01\x02";
  let error =
    validate_cbor_from_slice(r#"m = { ? float => tstr }"#, nan_and_integer, None).unwrap_err();
  assert!(error.to_string().contains("unexpected key Integer"));
}

#[test]
fn validate_nan_composite_map_keys_have_physical_pair_identity() {
  // Value::PartialEq is also non-reflexive for a composite value containing
  // NaN. Ownership must therefore use the outer map entry index, not recursive
  // host-language value equality.
  let array_key = b"\xa1\x81\xf9\x7e\x00\x61x"; // {[NaN]: "x"}
  validate_cbor_from_slice(r#"m = { ? [float] => tstr }"#, array_key, None).unwrap();
  validate_cbor_from_slice(
    r#"m = { ? [float] => tstr, ? [float] => uint }"#,
    array_key,
    None,
  )
  .unwrap();

  // The index ledger is local to each decoded map. It must also account for
  // the NaN pair while validating a nested map used as the outer map's key.
  let map_key = b"\xa1\xa1\xf9\x7e\x00\x01\x61x"; // {{NaN: 1}: "x"}
  validate_cbor_from_slice(r#"m = { ? { float => uint } => tstr }"#, map_key, None).unwrap();
}

#[test]
fn validate_generic_map_claims_share_parent_physical_ownership() {
  let one_text_pair = b"\xa1\x61a\x61x"; // {"a": "x"}
  let zero_or_more = "g<K, V> = (* K => V)\nm = { ? tstr => tstr, g<tstr, tstr> }";
  let one_or_more = "g<K, V> = (+ K => V)\nm = { ? tstr => tstr, g<tstr, tstr> }";

  // A named group has the same matching semantics as its parenthesized
  // definition (RFC 8610 Appendix C). It must see the pair already claimed by
  // the preceding parent member: `*` can take width zero, while `+` cannot.
  validate_cbor_from_slice(zero_or_more, one_text_pair, None).unwrap();
  validate_cbor_from_slice(one_or_more, one_text_pair, None).unwrap_err();

  // A generic child's successful NaN claim must be transferred by physical
  // index so the enclosing closed-map pass recognizes that exact pair.
  let one_nan_text = b"\xa1\xf9\x7e\x00\x61x";
  validate_cbor_from_slice(
    "g<K, V> = (? K => V)\nm = { g<float, tstr> }",
    one_nan_text,
    None,
  )
  .unwrap();
}

#[test]
fn validate_repeating_map_entries_are_greedy() {
  let one_text_uint = b"\xa1\x61a\x01"; // {"a": 1}

  // RFC 8610 Appendix A makes occurrences greedy and possessive. The first
  // entry consumes the only pair, so the required entry cannot match it.
  validate_cbor_from_slice(r#"m = { * any => any, tstr => uint }"#, one_text_uint, None)
    .unwrap_err();
  validate_cbor_from_slice(r#"m = { tstr => uint, * any => any }"#, one_text_uint, None).unwrap();

  // A positive lower bound also consumes the pair and therefore leaves no
  // distinct pair for the required entry.
  validate_cbor_from_slice(
    r#"m = { + any => uint, tstr => uint }"#,
    one_text_uint,
    None,
  )
  .unwrap_err();
  validate_cbor_from_slice(
    r#"m = { 1*1 any => uint, tstr => uint }"#,
    one_text_uint,
    None,
  )
  .unwrap_err();

  // The same wildcard-first allocation remains valid when the direct member
  // is optional.
  validate_cbor_from_slice(
    r#"m = { * any => any, ? tstr => uint }"#,
    one_text_uint,
    None,
  )
  .unwrap();
}

#[test]
fn validate_repeating_map_entries_count_only_unconsumed_matches() {
  let a_only = b"\xa1\x61a\x01"; // {"a": 1}
  let int_only = b"\xa1\x01\x01"; // {1: 1}
  let int_and_bytes = b"\xa2\x01\x01\x41\xaa\x05"; // {1: 1, h'aa': 5}
  let a_b = b"\xa2\x61a\x01\x61b\x02";
  let a_b_c = b"\xa3\x61a\x01\x61b\x02\x61c\x03";
  let a_b_c_d = b"\xa4\x61a\x01\x61b\x02\x61c\x03\x61d\x04";
  let a_b_c_d_e = b"\xa5\x61a\x01\x61b\x02\x61c\x03\x61d\x04\x61e\x05";

  // The occurrence applies to this member's unconsumed matches, not to the
  // enclosing map's total size. This matters for both same-domain keys that
  // an earlier member consumed and disjoint keys left for a later member.
  validate_cbor_from_slice(r#"m = { a: uint, * tstr => uint }"#, a_only, None).unwrap();
  validate_cbor_from_slice(r#"m = { a: uint, + tstr => uint }"#, a_only, None).unwrap_err();
  validate_cbor_from_slice(r#"m = { * tstr => uint, int => uint }"#, int_only, None).unwrap();
  validate_cbor_from_slice(r#"m = { + tstr => uint, int => uint }"#, int_only, None).unwrap_err();

  // Preserve the original mixed-map reproduction for byte-string keys. A
  // zero-width repetition succeeds even though the map itself is nonempty;
  // adding a matching pair still validates and `+` still requires a match.
  validate_cbor_from_slice(r#"m = { 1: uint, * bytes => any }"#, int_only, None).unwrap();
  validate_cbor_from_slice(r#"m = { 1: uint, * bytes => any }"#, int_and_bytes, None).unwrap();
  validate_cbor_from_slice(r#"m = { 1: uint, + bytes => any }"#, int_only, None).unwrap_err();

  // Bounded occurrences count only the pairs left after `a` is consumed.
  let bounded = r#"m = { a: uint, 2*3 tstr => uint }"#;
  validate_cbor_from_slice(bounded, a_b, None).unwrap_err();
  validate_cbor_from_slice(bounded, a_b_c, None).unwrap();
  validate_cbor_from_slice(bounded, a_b_c_d, None).unwrap();
  validate_cbor_from_slice(bounded, a_b_c_d_e, None).unwrap_err();

  validate_cbor_from_slice(r#"m = { a: uint, *2 tstr => uint }"#, a_only, None).unwrap();
  validate_cbor_from_slice(r#"m = { a: uint, *2 tstr => uint }"#, a_b_c, None).unwrap();
  validate_cbor_from_slice(r#"m = { a: uint, *2 tstr => uint }"#, a_b_c_d, None).unwrap_err();
  validate_cbor_from_slice(r#"m = { a: uint, 2* tstr => uint }"#, a_b, None).unwrap_err();
  validate_cbor_from_slice(r#"m = { a: uint, 2* tstr => uint }"#, a_b_c, None).unwrap();

  // A bounded repetition stops at its upper bound. It must not claim all
  // matching pairs and then reject before a later member can own the rest.
  validate_cbor_from_slice(r#"m = { 1*1 tstr => uint, tstr => uint }"#, a_b, None).unwrap();
  validate_cbor_from_slice(r#"m = { *2 tstr => uint, tstr => uint }"#, a_b_c, None).unwrap();
  validate_cbor_from_slice(r#"m = { 2*3 tstr => uint, tstr => uint }"#, a_b_c_d, None).unwrap();
}

#[test]
fn validate_repeating_map_entry_counts_cover_primitive_key_paths() {
  let cases = [
    ("any", "any", "any", Value::Text("a".into())),
    ("tstr", "tstr", "tstr", Value::Text("a".into())),
    ("int", "int", "int", Value::Integer(1.into())),
    ("uint", "uint", "uint", Value::Integer(1.into())),
    ("nint", "nint", "nint", Value::Integer((-1).into())),
    ("number/int", "int", "number", Value::Integer(1.into())),
    ("number/float", "float", "number", Value::Float(1.5)),
    ("bool", "bool", "bool", Value::Bool(true)),
    ("null", "null", "null", Value::Null),
    ("bytes", "bytes", "bytes", Value::Bytes(vec![1])),
    ("float", "float", "float", Value::Float(1.5)),
    (
      "biguint",
      "biguint",
      "biguint",
      Value::Tag(2, Box::new(Value::Bytes(vec![1]))),
    ),
    (
      "bignint",
      "bignint",
      "bignint",
      Value::Tag(3, Box::new(Value::Bytes(vec![1]))),
    ),
    (
      "bigint/tag 2",
      "bigint",
      "bigint",
      Value::Tag(2, Box::new(Value::Bytes(vec![1]))),
    ),
    (
      "bigint/tag 3",
      "bigint",
      "bigint",
      Value::Tag(3, Box::new(Value::Bytes(vec![1]))),
    ),
  ];

  for (case_name, consuming_key_type, repeating_key_type, key) in cases {
    let mut bytes = Vec::new();
    ciborium::ser::into_writer(
      &Value::Map(vec![(key, Value::Integer(1.into()))]),
      &mut bytes,
    )
    .unwrap();

    // The optional member greedily consumes the only pair. The following
    // repetition therefore has width zero regardless of the enclosing map's
    // size: `*` accepts that width and `+` rejects it.
    let zero_or_more = format!(
      "m = {{ ? {} => any, * {} => uint }}",
      consuming_key_type, repeating_key_type
    );
    let one_or_more = format!(
      "m = {{ ? {} => any, + {} => uint }}",
      consuming_key_type, repeating_key_type
    );
    let result = validate_cbor_from_slice(&zero_or_more, &bytes, None);
    assert!(
      result.is_ok(),
      "zero-or-more {} case failed: {:?}",
      case_name,
      result
    );
    let result = validate_cbor_from_slice(&one_or_more, &bytes, None);
    assert!(
      result.is_err(),
      "one-or-more {} case unexpectedly matched",
      case_name
    );
  }
}

#[test]
fn validate_single_map_entry_claims_are_transactional_across_group_choices() {
  let one_text_value = b"\xa1\x61a\x61x"; // {"a": "x"}

  // A failed alternative must not leave a claim that hides the pair from the
  // next alternative or from final closed-map accounting.
  let error = validate_cbor_from_slice(
    r#"m = { tstr => uint // ? tstr => bytes }"#,
    one_text_value,
    None,
  )
  .unwrap_err();
  assert!(error.to_string().contains("unexpected key"));

  // The second alternative can validly own the pair after the first
  // alternative fails its value validation.
  validate_cbor_from_slice(
    r#"m = { tstr => uint // tstr => tstr }"#,
    one_text_value,
    None,
  )
  .unwrap();
}

#[test]
fn validate_optional_map_entries_are_greedy() {
  let cases = [
    (
      r#"m = { ? tstr => any, tstr => any }"#,
      Value::Text("a".into()),
    ),
    (
      r#"m = { ? int => any, int => any }"#,
      Value::Integer(1.into()),
    ),
    (r#"m = { ? bool => any, bool => any }"#, Value::Bool(true)),
    (r#"m = { ? null => any, null => any }"#, Value::Null),
    (
      r#"m = { ? bytes => any, bytes => any }"#,
      Value::Bytes(vec![1]),
    ),
    (r#"m = { ? float => any, float => any }"#, Value::Float(1.5)),
    (
      r#"m = { ? biguint => any, biguint => any }"#,
      Value::Tag(2, Box::new(Value::Bytes(vec![1]))),
    ),
    (
      r#"m = { ? bignint => any, bignint => any }"#,
      Value::Tag(3, Box::new(Value::Bytes(vec![1]))),
    ),
  ];

  for (schema, key) in cases {
    let mut bytes = Vec::new();
    ciborium::ser::into_writer(
      &Value::Map(vec![(key, Value::Integer(1.into()))]),
      &mut bytes,
    )
    .unwrap();

    let result = validate_cbor_from_slice(schema, &bytes, None);
    assert!(result.is_err(), "schema {} unexpectedly matched", schema);
  }

  let text_pair = b"\xa1\x61a\x01"; // {"a": 1}
  validate_cbor_from_slice(r#"m = { ? tstr => any, a: any }"#, text_pair, None).unwrap_err();
  validate_cbor_from_slice(r#"m = { ? a: any, tstr => any }"#, text_pair, None).unwrap_err();
}

#[test]
fn validate_optional_arrow_value_failure_can_fall_through() {
  let text_value = b"\xa1\x61a\x61x"; // {"a": "x"}

  // RFC 8610 §3.5.4: without a cut, failure of the optional entry's value
  // type leaves the pair available to a later matching entry.
  validate_cbor_from_slice(r#"m = { ? tstr => uint, tstr => tstr }"#, text_value, None).unwrap();

  // The colon shortcut includes a cut. Once `a` matches, the bad `uint`
  // value commits the failure and the later type-domain member cannot rescue
  // the pair.
  validate_cbor_from_slice(r#"m = { ? a: uint, tstr => tstr }"#, text_value, None).unwrap_err();

  // This is RFC 8610 §3.5.4's extensible-map example. The arrow form permits
  // the wildcard to cover a known key whose optional value type does not
  // match; the colon form locks the pair and rejects it.
  let extension_value = b"\xa1\x6coptional-key\x68nonsense";
  validate_cbor_from_slice(
    r#"m = { ? "optional-key" => int, * tstr => any }"#,
    extension_value,
    None,
  )
  .unwrap();
  validate_cbor_from_slice(
    r#"m = { ? "optional-key": int, * tstr => any }"#,
    extension_value,
    None,
  )
  .unwrap_err();
}

#[test]
fn validate_optional_single_map_entry_assignment_considers_values() {
  let schema = r#"m = { ? tstr => any, tstr => tstr }"#;

  // Both distinct keys are in the same domain. The order-stable allocation
  // policy assigns the text-valued pair to the required member and the
  // integer-valued pair to the optional member in either CBOR encoding.
  let text_value_first = b"\xa2\x61a\x61x\x61b\x01";
  let integer_value_first = b"\xa2\x61a\x01\x61b\x61x";

  validate_cbor_from_slice(schema, text_value_first, None).unwrap();
  validate_cbor_from_slice(schema, integer_value_first, None).unwrap();

  // Occurrence-less direct members use the same policy. The broad first
  // member can move to the integer-valued pair so the second member owns the
  // text-valued pair.
  validate_cbor_from_slice(
    r#"m = { tstr => any, tstr => tstr }"#,
    text_value_first,
    None,
  )
  .unwrap();

  // A valid allocation can require a cycle longer than one exchange:
  // required takes pair 0, optional 1 moves to pair 1, and optional 2 moves
  // to pair 2. A pairwise-only repair cannot find this assignment.
  let three_way_schema = r#"m = { ? tstr => (int / bool), ? tstr => any, tstr => int }"#;
  let three_way_assignment = b"\xa3\x61a\x01\x61b\xf5\x61c\x41\x00";
  validate_cbor_from_slice(three_way_schema, three_way_assignment, None).unwrap();

  // Generic group children allocate the same physical map. Their direct
  // claims must retain the member schema needed by the parent assignment.
  let generic_schema = "g<K, V> = (? K => V)\nm = { g<tstr, 1..2>, tstr => 1, * any => any }";
  let required_value_first = b"\xa2\x61a\x01\x61b\x02";
  let generic_value_first = b"\xa2\x61b\x02\x61a\x01";
  validate_cbor_from_slice(generic_schema, required_value_first, None).unwrap();
  validate_cbor_from_slice(generic_schema, generic_value_first, None).unwrap();

  // Independent generic children initially select pair 0. The outward merge
  // must relocate the second child to a compatible free pair instead of
  // dropping its member/schema vertex as a duplicate parent claim.
  let two_generic_schema = "g<K, V> = (? K => V)\n\
    h<K, V> = (? K => V)\n\
    m = { g<tstr, any>, h<tstr, any> }";
  let two_generic_pairs = b"\xa2\x61a\x01\x61b\x02";
  validate_cbor_from_slice(two_generic_schema, two_generic_pairs, None).unwrap();

  // The colliding child need not accept the placeholder itself. Matching can
  // move the earlier broad child to that pair and retain pair 0 for the
  // narrower child.
  let two_generic_swap_schema = "g<K, V> = (? K => V)\n\
    h<K, V> = (? K => V)\n\
    m = { g<tstr, any>, h<tstr, int> }";
  let integer_then_bool = b"\xa2\x61a\x01\x61b\xf5";
  validate_cbor_from_slice(two_generic_swap_schema, integer_then_bool, None).unwrap();

  // Two uses of the same generic rule have distinct instantiations. The
  // second claim must not replay with the first use's `any` argument.
  let repeated_generic_schema = "g<K, V> = (? K => V)\n\
    m = { g<tstr, any>, g<tstr, int> }";
  let two_bool_values = b"\xa2\x61a\xf5\x61b\xf4";
  validate_cbor_from_slice(repeated_generic_schema, two_bool_values, None).unwrap_err();

  let repeated_identical_schema = "g<K, V> = (? K => V)\n\
    m = { g<tstr, any>, g<tstr, any> }";
  validate_cbor_from_slice(repeated_identical_schema, two_bool_values, None).unwrap();

  let two_generic_cycle_schema = "g<K, V> = (? K => V)\n\
    h<K, V> = (? K => V)\n\
    m = { g<tstr, (int / bool)>, h<tstr, any>, tstr => int }";
  validate_cbor_from_slice(two_generic_cycle_schema, three_way_assignment, None).unwrap();

  // Parent and generic children share one physical ownership ledger. Either
  // text-key order must leave the byte-key pair for the later required member
  // while assigning the two text pairs to the two generic children.
  let generic_then_disjoint_schema = "g<K, V> = (? K => V)\n\
    h<K, V> = (? K => V)\n\
    m = { g<tstr, int>, h<tstr, int>, bytes => any }";
  let text_byte_text = b"\xa3\x61a\x01\x41\x00\xf5\x61b\x02";
  let text_byte_text_reversed = b"\xa3\x61b\x02\x41\x00\xf5\x61a\x01";
  validate_cbor_from_slice(generic_then_disjoint_schema, text_byte_text, None).unwrap();
  validate_cbor_from_slice(generic_then_disjoint_schema, text_byte_text_reversed, None).unwrap();

  // A failed assignment search is read-only. The next group alternative must
  // start from its checkpoint and can claim both pairs with a wildcard.
  validate_cbor_from_slice(
    r#"m = { (? tstr => tstr, tstr => bytes) // * any => any }"#,
    text_value_first,
    None,
  )
  .unwrap();
}

#[test]
fn validate_map_unexpected_entries_rejected() -> Result<(), Box<dyn Error>> {
  // A map entry unmatched by any group member is an unexpected key, including
  // when the group's only member is `?`-marked and absent
  let empty_map = b"\xa0";
  let k1 = b"\xa1\x61\x6b\x01"; // {"k": 1}
  let a1 = b"\xa1\x61\x61\x01"; // {"a": 1}
  let k1_a2 = b"\xa2\x61\x6b\x01\x61\x61\x02"; // {"k": 1, "a": 2}
  let int_entry = b"\xa1\x01\x02"; // {1: 2}

  validate_cbor_from_slice(r#"m = { ? k: uint }"#, empty_map, None)?;
  validate_cbor_from_slice(r#"m = { ? k: uint }"#, k1, None)?;
  assert!(validate_cbor_from_slice(r#"m = { ? k: uint }"#, a1, None).is_err());
  assert!(validate_cbor_from_slice(r#"m = { ? k: uint }"#, k1_a2, None).is_err());
  // absent optional type-domain entry does not excuse a key of another domain
  assert!(validate_cbor_from_slice(r#"m = { ? tstr => uint }"#, int_entry, None).is_err());

  Ok(())
}

/// Regression: `* any => any` (the RFC 8610 extension-point idiom) must
/// permit unknown extra entries instead of rejecting them as unexpected keys
#[test]
fn validate_map_any_key_permits_extra_entries() -> Result<(), Box<dyn Error>> {
  let empty_map = b"\xa0";
  let k1 = b"\xa1\x61\x6b\x01"; // {"k": 1}
  let k1_z9 = b"\xa2\x61\x6b\x01\x61\x7a\x09"; // {"k": 1, "z": 9}
  let k1_ztext = b"\xa2\x61\x6b\x01\x61\x7a\x61\x61"; // {"k": 1, "z": "a"}
  let int_keyed = b"\xa2\x01\x02\x03\x04"; // {1: 2, 3: 4}

  // Put the specific member before the extension point. RFC 8610 Appendix A
  // makes the leading wildcard form greedy; Section 3.5.3 identifies that
  // general-before-specific overlap as pathological.
  validate_cbor_from_slice(r#"m = { k: uint, * any => any }"#, k1, None)?;
  validate_cbor_from_slice(r#"m = { k: uint, * any => any }"#, k1_z9, None)?;
  assert!(validate_cbor_from_slice(r#"m = { * any => any, k: uint }"#, k1_z9, None).is_err());
  // colon shortcut form
  validate_cbor_from_slice(r#"m = { k: uint, * any: any }"#, k1_z9, None)?;
  validate_cbor_from_slice(r#"m = { * any => any }"#, empty_map, None)?;
  // `any` keys are not limited to text keys
  validate_cbor_from_slice(r#"m = { * any => any }"#, int_keyed, None)?;
  // `+` still requires at least one entry
  assert!(validate_cbor_from_slice(r#"m = { + any => any }"#, empty_map, None).is_err());
  validate_cbor_from_slice(r#"m = { + any => any }"#, k1, None)?;
  // an `any` key does not excuse a value-type mismatch
  assert!(validate_cbor_from_slice(r#"m = { k: uint, * any => uint }"#, k1_ztext, None).is_err());
  // a map without the extension member stays closed
  assert!(validate_cbor_from_slice(r#"m = { k: uint }"#, k1_z9, None).is_err());

  Ok(())
}

/// A wrong value type under a cut-carrying key (`k: uint` — the colon
/// shortcut implies a cut, RFC 8610 §3.5.4) must NOT be rescued by a
/// `* any => any` extension member, and the error must name the value-type
/// mismatch rather than anything about the `any` branch
#[test]
fn validate_map_any_key_does_not_rescue_cut_value_mismatch() {
  let k_wrong_z = b"\xa2\x61\x6b\x61\x73\x61\x7a\x09"; // {"k": "s", "z": 9}
  for schema in [
    r#"m = { k: uint, * any => any }"#,
    r#"m = { k: uint, any => any }"#,
  ] {
    let err = validate_cbor_from_slice(schema, k_wrong_z, None)
      .unwrap_err()
      .to_string();
    assert!(
      err.contains(r#"expected type uint, got Text("s")"#),
      "schema {}: expected a value-type error for k, got: {}",
      schema,
      err
    );
  }
}
