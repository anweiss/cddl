#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(feature = "freezer")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::validator::validate_cbor_from_slice;

fn cbor_text(s: &str) -> Vec<u8> {
  let mut out = Vec::new();
  ciborium::into_writer(&ciborium::value::Value::Text(s.to_string()), &mut out).unwrap();
  out
}

fn cbor_uint(v: u64) -> Vec<u8> {
  let mut out = Vec::new();
  ciborium::into_writer(&ciborium::value::Value::Integer(v.into()), &mut out).unwrap();
  out
}

// =============================================================================
// .pcre tests
// =============================================================================

#[test]
fn pcre_simple_match() {
  let cddl = r#"val = tstr .pcre "[a-z]+""#;
  let cbor = cbor_text("hello");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

#[test]
fn pcre_simple_no_match() {
  let cddl = r#"val = tstr .pcre "[a-z]+""#;
  let cbor = cbor_text("HELLO");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap_err();
}

#[test]
fn pcre_lookahead() {
  // .pcre supports lookahead which .regexp cannot handle
  let cddl = r#"password = tstr .pcre "(?=.*[A-Z])(?=.*[0-9]).{8,}""#;
  let cbor = cbor_text("Password1");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

#[test]
fn pcre_lookahead_no_match() {
  let cddl = r#"password = tstr .pcre "(?=.*[A-Z])(?=.*[0-9]).{8,}""#;
  // No uppercase letter
  let cbor = cbor_text("password1");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap_err();
}

#[test]
fn pcre_lookbehind() {
  // Lookbehind: match "bar" only if preceded by "foo"
  // Use a pattern that matches the full string with lookbehind
  let cddl = r#"val = tstr .pcre "foo(?<=foo)bar""#;
  let cbor = cbor_text("foobar");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

#[test]
fn pcre_lookbehind_no_match() {
  let cddl = r#"val = tstr .pcre "baz(?<=foo)bar""#;
  let cbor = cbor_text("bazbar");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap_err();
}

#[test]
fn pcre_backreference() {
  // Backreferences: match repeated word
  let cddl = "val = tstr .pcre \"([a-z]+)-\\\\1\"";
  let cbor = cbor_text("abc-abc");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

#[test]
fn pcre_backreference_no_match() {
  let cddl = "val = tstr .pcre \"([a-z]+)-\\\\1\"";
  let cbor = cbor_text("abc-def");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap_err();
}

// =============================================================================
// .iregexp tests (RFC 9485)
// =============================================================================

#[test]
fn iregexp_basic() {
  let cddl = r#"email = tstr .iregexp "[^@]+@[^@]+""#;
  let cbor = cbor_text("user@example.com");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

#[test]
fn iregexp_no_match() {
  let cddl = r#"email = tstr .iregexp "[^@]+@[^@]+""#;
  let cbor = cbor_text("no-at-sign");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap_err();
}

#[test]
fn iregexp_digits() {
  let cddl = r#"digits = tstr .iregexp "[0-9]+""#;
  let cbor = cbor_text("12345");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

#[test]
fn iregexp_digits_no_match() {
  let cddl = r#"digits = tstr .iregexp "[0-9]+""#;
  let cbor = cbor_text("abc");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap_err();
}

#[test]
fn iregexp_anchored() {
  // Ensure iregexp is anchored — partial match should fail
  let cddl = r#"val = tstr .iregexp "[0-9]{3}""#;
  let cbor = cbor_text("abc123def");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap_err();
}

#[test]
fn iregexp_full_match() {
  let cddl = r#"val = tstr .iregexp "[0-9]{3}""#;
  let cbor = cbor_text("123");
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

// =============================================================================
// .bitfield tests
// =============================================================================

#[test]
fn bitfield_fits() {
  // 3 bits total: value 0b101 = 5 fits in 3 bits (max 7)
  let cddl = r#"field = uint .bitfield [1, 2]"#;
  let cbor = cbor_uint(5);
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

#[test]
fn bitfield_max_value() {
  // 3 bits total: max value is 7
  let cddl = r#"field = uint .bitfield [1, 2]"#;
  let cbor = cbor_uint(7);
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

#[test]
fn bitfield_exceeds() {
  // 3 bits total: value 8 exceeds 3 bits (max 7)
  let cddl = r#"field = uint .bitfield [1, 2]"#;
  let cbor = cbor_uint(8);
  validate_cbor_from_slice(cddl, &cbor, None).unwrap_err();
}

#[test]
fn bitfield_zero_value() {
  // 0 always fits in any bit width
  let cddl = r#"field = uint .bitfield [4, 4]"#;
  let cbor = cbor_uint(0);
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

#[test]
fn bitfield_single_bit() {
  // 1 bit: max value is 1
  let cddl = r#"field = uint .bitfield [1]"#;
  let cbor = cbor_uint(1);
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

#[test]
fn bitfield_single_bit_exceeds() {
  // 1 bit: value 2 exceeds
  let cddl = r#"field = uint .bitfield [1]"#;
  let cbor = cbor_uint(2);
  validate_cbor_from_slice(cddl, &cbor, None).unwrap_err();
}

#[test]
fn bitfield_eight_bits() {
  // 8 bits total: max value is 255
  let cddl = r#"field = uint .bitfield [1, 1, 2, 4]"#;
  let cbor = cbor_uint(255);
  validate_cbor_from_slice(cddl, &cbor, None).unwrap();
}

#[test]
fn bitfield_eight_bits_exceeds() {
  // 8 bits total: value 256 exceeds
  let cddl = r#"field = uint .bitfield [1, 1, 2, 4]"#;
  let cbor = cbor_uint(256);
  validate_cbor_from_slice(cddl, &cbor, None).unwrap_err();
}
