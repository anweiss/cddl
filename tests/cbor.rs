#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{
  cddl_from_str,
  validator::{cbor::CBORValidator, validate_cbor_from_slice, Validator},
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
#[ignore] // FIXME: broken
fn validate_cbor_array_groups() {
  let cddl_input = r#"thing = [int, (int, int)]"#;
  validate_cbor_from_slice(cddl_input, cbor::ARRAY_123, None).unwrap();
  // TODO: try splitting arrays into groups a few other ways:
  // [(int, int, int)]
  // [* (int)]
  // [* (int, int)]
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
  let mut cv = CBORValidator::new(&cddl, test, None);
  assert!(
    cv.validate().is_err(),
    "4 should fail inclusive range 5..10"
  );

  Ok(())
}
