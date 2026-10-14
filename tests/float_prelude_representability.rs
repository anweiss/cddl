//! RFC 8610 float prelude representability regressions.
//!
//! Section 3.3 defines `float16`, `float32`, and `float64` by the values that
//! their IEEE 754 formats can represent. Section 2.2.3 makes clear that these
//! are value constraints, not requirements on the CBOR encoding width.
#![cfg(feature = "std")]
#![cfg(not(target_arch = "wasm32"))]

#[cfg(feature = "cbor")]
fn validate_cbor(schema: &str, data: &[u8]) -> bool {
  #[cfg(feature = "additional-controls")]
  let result = cddl::validate_cbor_from_slice(schema, data, None);
  #[cfg(not(feature = "additional-controls"))]
  let result = cddl::validate_cbor_from_slice(schema, data);

  result.is_ok()
}

#[cfg(feature = "json")]
fn validate_json(schema: &str, data: &str) -> bool {
  #[cfg(feature = "additional-controls")]
  let result = cddl::validate_json_from_str(schema, data, None);
  #[cfg(not(feature = "additional-controls"))]
  let result = cddl::validate_json_from_str(schema, data);

  result.is_ok()
}

#[cfg(feature = "cbor")]
mod cbor {
  use super::validate_cbor;

  const FLOAT16_1_5: &[u8] = b"\xf9\x3e\x00";
  const FLOAT32_1_1: &[u8] = b"\xfa\x3f\x8c\xcc\xcd";
  const FLOAT64_1_1: &[u8] = b"\xfb\x3f\xf1\x99\x99\x99\x99\x99\x9a";
  const FLOAT64_1_5: &[u8] = b"\xfb\x3f\xf8\x00\x00\x00\x00\x00\x00";
  const FLOAT64_INFINITY: &[u8] = b"\xfb\x7f\xf0\x00\x00\x00\x00\x00\x00";
  const FLOAT64_NAN: &[u8] = b"\xfb\x7f\xf8\x00\x00\x00\x00\x00\x00";

  #[test]
  fn rejects_finite_values_outside_the_named_precision() {
    for schema in ["x = float16", "x = float32", "x = float16-32"] {
      assert!(!validate_cbor(schema, FLOAT64_1_1), "{}", schema);
    }

    assert!(!validate_cbor("x = float16", FLOAT32_1_1));
  }

  #[test]
  fn accepts_representable_values_regardless_of_encoding_width() {
    assert!(validate_cbor("x = float16", FLOAT64_1_5));

    for schema in [
      "x = float16",
      "x = float32",
      "x = float64",
      "x = float16-32",
      "x = float32-64",
      "x = float",
    ] {
      assert!(validate_cbor(schema, FLOAT16_1_5), "{}", schema);
    }
  }

  #[test]
  fn accepts_ieee_special_values_at_each_precision() {
    for schema in [
      "x = float16",
      "x = float32",
      "x = float64",
      "x = float16-32",
      "x = float32-64",
      "x = float",
    ] {
      assert!(
        validate_cbor(schema, FLOAT64_INFINITY),
        "{}: infinity",
        schema
      );
      assert!(validate_cbor(schema, FLOAT64_NAN), "{}: NaN", schema);
    }
  }

  #[test]
  fn enforces_bounds_through_aliases_and_control_targets() {
    let alias = "x = small\nsmall = float16";
    assert!(!validate_cbor(alias, FLOAT64_1_1));
    assert!(validate_cbor(alias, FLOAT64_1_5));
    assert!(validate_cbor(
      "x = either\neither = float16 / float64",
      FLOAT64_1_1,
    ));

    assert!(!validate_cbor("x = float16 .gt 0.0", FLOAT64_1_1));
    assert!(validate_cbor("x = float16 .gt 0.0", FLOAT64_1_5));
    assert!(!validate_cbor(
      "x = bounded .gt 0.0\nbounded = int / float16",
      FLOAT64_1_1,
    ));
  }

  #[test]
  fn enforces_bounds_for_direct_primitive_map_keys() {
    const MAP_FLOAT64_1_1_TO_UINT: &[u8] = b"\xa1\xfb\x3f\xf1\x99\x99\x99\x99\x99\x9a\x01";
    const MAP_FLOAT64_1_5_TO_UINT: &[u8] = b"\xa1\xfb\x3f\xf8\x00\x00\x00\x00\x00\x00\x01";
    const MAP_FLOAT64_1_1_TO_TEXT: &[u8] = b"\xa1\xfb\x3f\xf1\x99\x99\x99\x99\x99\x9a\x61x";

    for schema in [
      "m = { float16 => uint }",
      "m = { float32 => uint }",
      "m = { float16-32 => uint }",
    ] {
      assert!(!validate_cbor(schema, MAP_FLOAT64_1_1_TO_UINT));
    }
    assert!(validate_cbor(
      "m = { float16 => uint }",
      MAP_FLOAT64_1_5_TO_UINT,
    ));
    assert!(!validate_cbor(
      "m = { * float16 => uint }",
      MAP_FLOAT64_1_1_TO_UINT,
    ));
    assert!(validate_cbor(
      "m = { * float16 => uint }",
      MAP_FLOAT64_1_5_TO_UINT,
    ));

    // The optional narrow domain must leave a nonrepresentable key for the
    // disjoint wider domain instead of claiming it and validating its value.
    assert!(validate_cbor(
      "m = { ? float16 => uint, float64 => tstr }",
      MAP_FLOAT64_1_1_TO_TEXT,
    ));
  }
}

#[cfg(feature = "json")]
mod json {
  use super::validate_json;

  #[test]
  fn rejects_finite_values_outside_the_named_precision() {
    for schema in ["x = float16", "x = float32", "x = float16-32"] {
      assert!(!validate_json(schema, "1.1"), "{}", schema);
    }

    assert!(!validate_json("x = float16", "1.100000023841858"));
  }

  #[test]
  fn accepts_values_representable_at_the_named_precision() {
    for schema in [
      "x = float16",
      "x = float32",
      "x = float64",
      "x = float16-32",
      "x = float32-64",
      "x = float",
    ] {
      assert!(validate_json(schema, "1.5"), "{}", schema);
    }

    assert!(validate_json("x = float32", "1.100000023841858"));
  }

  #[test]
  fn enforces_bounds_through_aliases_and_control_targets() {
    let alias = "x = small\nsmall = float16";
    assert!(!validate_json(alias, "1.1"));
    assert!(validate_json(alias, "1.5"));
    assert!(validate_json(
      "x = either\neither = float16 / float64",
      "1.1",
    ));

    assert!(!validate_json("x = float16 .gt 0.0", "1.1"));
    assert!(validate_json("x = float16 .gt 0.0", "1.5"));
    assert!(!validate_json(
      "x = bounded .gt 0.0\nbounded = int / float16",
      "1.1",
    ));
  }

  #[test]
  fn float_preludes_still_reject_integer_values() {
    for schema in [
      "x = float16",
      "x = float32",
      "x = float64",
      "x = float16-32",
      "x = float32-64",
      "x = float",
    ] {
      assert!(!validate_json(schema, "5"), "{}", schema);
    }
  }
}
