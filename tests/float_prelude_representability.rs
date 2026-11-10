//! RFC 8610 float prelude representability regressions.
//!
//! Section 3.3 defines `float16`, `float32`, and `float64` by the values that
//! their IEEE 754 formats can represent. Section 2.2.3 makes clear that these
//! are value constraints, not requirements on the CBOR encoding width.
#![cfg(feature = "std")]
#![cfg(not(feature = "lsp"))]

#[cfg(feature = "cbor")]
fn validate_cbor(schema: &str, data: &[u8]) -> bool {
  #[cfg(feature = "additional-controls")]
  let result = cddl::validator::validate_cbor_from_slice(schema, data, None);
  #[cfg(not(feature = "additional-controls"))]
  let result = cddl::validator::validate_cbor_from_slice(schema, data);

  result.is_ok()
}

#[cfg(feature = "json")]
fn validate_json(schema: &str, data: &str) -> bool {
  #[cfg(feature = "additional-controls")]
  let result = cddl::validator::validate_json_from_str(schema, data, None);
  #[cfg(not(feature = "additional-controls"))]
  let result = cddl::validator::validate_json_from_str(schema, data);

  result.is_ok()
}

// Exercise the full target evaluator, not an identifier-only classification.
const CONTROL_DOMAINS: &[(&str, bool)] = &[
  ("x = custom .gt 0.0\ncustom = 1.1 / float16", true),
  ("x = a .gt 0.0\na = int / float16 / a", false),
  ("x = a .gt 0.0\na = a / int / float16", false),
  ("x = a .gt 0.0\na = b\nb = a / float16", false),
  ("x = custom .gt 0.0\ncustom = (1.1 / float16)", true),
  ("x = custom .gt 0.0\ncustom = (float16 / float64)", true),
  ("x = custom .gt 0.0\ncustom = (float16 / int)", false),
  ("x = custom .gt 0.0\ncustom = float64 .lt 1.0", false),
  (
    "x = custom .gt 0.0\ncustom = (float64 .lt 1.0) / float16",
    false,
  ),
  (
    "x = custom .gt 0.0\ncustom = (float64 .gt 1.0) / float16",
    true,
  ),
  ("x = custom .gt 0.0\ncustom = 1.0..2.0", true),
  ("x = custom .gt 0.0\ncustom = 2.0..3.0", false),
  ("x = limited<float16>\nlimited<t> = t .gt 0.0", false),
  ("x = limited<float64>\nlimited<t> = t .gt 0.0", true),
  ("x = limited<(1.1 / float16)>\nlimited<t> = t .gt 0.0", true),
  (
    "x = limited<(float64 .lt 1.0)>\nlimited<t> = t .gt 0.0",
    false,
  ),
  (
    "x = limited<(float64 .lt 1.0)>\nlimited<t> = t .gt zero\nzero = 0.0",
    false,
  ),
  ("x = a<float16> .gt 0.0\na<t> = t / a<t>", false),
  ("x = a<float16> .gt 0.0\na<t> = a<t> / t", false),
  ("x = a<float16> .gt 0.0\na<t> = t / a<float64>", true),
  ("x = small<float16> .gt 0.0\nsmall<t> = t", false),
  ("x = small<float64> .gt 0.0\nsmall<t> = t", true),
  ("x = custom .gt 0.0\ncustom /= float16\ncustom /= 1.1", true),
  ("x = float16 .eq 1.1", false),
  ("x = float16 .ne 0.0", false),
  ("x = float16 .lt 2.0", false),
  ("x = float16 .le 2.0", false),
  ("x = float16 .ge 0.0", false),
  ("x = float64 .eq 1.1", true),
  ("x = float64 .ne 0.0", true),
  ("x = float64 .ne 1.1", false),
  ("x = float64 .lt 2.0", true),
  ("x = float64 .le 2.0", true),
  ("x = float64 .ge 0.0", true),
];

#[cfg(feature = "cbor")]
mod cbor {
  use super::validate_cbor;

  const FLOAT16_1_5: &[u8] = b"\xf9\x3e\x00";
  const FLOAT32_1_1: &[u8] = b"\xfa\x3f\x8c\xcc\xcd";
  const FLOAT64_1_1: &[u8] = b"\xfb\x3f\xf1\x99\x99\x99\x99\x99\x9a";
  const FLOAT64_1_5: &[u8] = b"\xfb\x3f\xf8\x00\x00\x00\x00\x00\x00";
  const FLOAT64_INFINITY: &[u8] = b"\xfb\x7f\xf0\x00\x00\x00\x00\x00\x00";
  const FLOAT64_NAN: &[u8] = b"\xfb\x7f\xf8\x00\x00\x00\x00\x00\x00";

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn rejects_finite_values_outside_the_named_precision() {
    for schema in ["x = float16", "x = float32", "x = float16-32"] {
      assert!(!validate_cbor(schema, FLOAT64_1_1), "{}", schema);
    }

    assert!(!validate_cbor("x = float16", FLOAT32_1_1));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
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

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
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

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
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

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn evaluates_complete_control_target_domains_without_recursive_overflow() {
    for (schema, expected) in super::CONTROL_DOMAINS {
      assert_eq!(validate_cbor(schema, FLOAT64_1_1), *expected, "{}", schema);
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn exact_precision_boundaries() {
    for (value, half, single) in [
      (0.0, true, true),
      (-0.0, true, true),
      (65504.0, true, true),
      (65520.0, false, true),
      (2.0_f64.powi(-24), true, true),
      (2.0_f64.powi(-25), false, true),
      (f64::from(f32::MAX), false, true),
      (f64::from(f32::from_bits(1)), false, true),
      (2.0_f64.powi(-150), false, false),
      (f64::MAX, false, false),
      (f64::NEG_INFINITY, true, true),
    ] {
      let mut data = vec![0xfb];
      data.extend_from_slice(&value.to_be_bytes());
      assert_eq!(validate_cbor("x = float16", &data), half, "{value}");
      assert_eq!(validate_cbor("x = float32", &data), single, "{value}");
      assert!(validate_cbor("x = float64", &data), "{}", value);
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn literal_alternatives_do_not_widen_a_narrow_float_domain() {
    let value = f64::from_bits(0.1_f64.to_bits() + 1);
    let mut data = vec![0xfb];
    data.extend_from_slice(&value.to_be_bytes());
    assert!(!validate_cbor("x = a .gt 0.0\na = 0.1 / float16", &data));
    assert!(validate_cbor("x = float64 .ne 0.1", &data));
    assert!(!validate_cbor("x = float64 .eq 0.1", &data));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn generic_recursion_can_consume_input_before_reentering() {
    let schema = "x = a<float16>\na<t> = t / [a<t>]";
    assert!(validate_cbor(schema, FLOAT64_1_5));
    let mut nested = vec![0x81, 0x81];
    nested.extend_from_slice(FLOAT64_1_5);
    assert!(validate_cbor(schema, &nested));
    let mut invalid = vec![0x81, 0x81];
    invalid.extend_from_slice(FLOAT64_1_1);
    assert!(!validate_cbor(schema, &invalid));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
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

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn rejects_finite_values_outside_the_named_precision() {
    for schema in ["x = float16", "x = float32", "x = float16-32"] {
      assert!(!validate_json(schema, "1.1"), "{}", schema);
    }

    assert!(!validate_json("x = float16", "1.100000023841858"));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
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

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
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

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn evaluates_complete_control_target_domains_without_recursive_overflow() {
    for (schema, expected) in super::CONTROL_DOMAINS {
      assert_eq!(validate_json(schema, "1.1"), *expected, "{}", schema);
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn literal_alternatives_do_not_widen_a_narrow_float_domain() {
    let value = f64::from_bits(0.1_f64.to_bits() + 1).to_string();
    assert!(!validate_json("x = a .gt 0.0\na = 0.1 / float16", &value));
    assert!(validate_json("x = float64 .ne 0.1", &value));
    assert!(!validate_json("x = float64 .eq 0.1", &value));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn generic_recursion_can_consume_input_before_reentering() {
    let schema = "x = a<float16>\na<t> = t / [a<t>]";
    for data in ["1.5", "[1.5]", "[[1.5]]"] {
      assert!(validate_json(schema, data), "{}", data);
    }
    assert!(!validate_json(schema, "[[1.1]]"));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
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
