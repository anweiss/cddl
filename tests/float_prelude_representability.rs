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
  ("x = float64 .eq float64", true),
  ("x = float64 .ne float64", false),
  ("x = float64 .eq float16", false),
  ("x = float64 .ne float16", true),
  ("x = float64 .eq (float16 / 1.1)", true),
  ("x = float64 .ne (float16 / 1.1)", false),
];

const WRAPPED_GENERIC_SCHEMAS: &[&str] = &[
  "x = a<float16>\na<t> = t / [a<(t)>]",
  "x = a<float16>\na<t> = t / [a<((t))>]",
  "x = a<float16>\na<t> = t / [a<(t .ge 1.0)>]",
];

fn comparison_cases() -> impl Iterator<Item = (String, f64, bool)> {
  IntoIterator::into_iter([
    ("eq", [false, true, false]),
    ("ne", [true, false, true]),
    ("gt", [false, false, true]),
    ("ge", [false, true, true]),
    ("lt", [true, false, false]),
    ("le", [true, true, false]),
  ])
  .flat_map(|(operator, expected)| {
    IntoIterator::into_iter([-1.5, 0.0, 1.5])
      .zip(expected)
      .map(move |(value, expected)| {
        (
          format!("x = float64 .{operator} zero\nzero = float64 .eq 0.0"),
          value,
          expected,
        )
      })
  })
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

  fn encoded_float(value: f64) -> Vec<u8> {
    let mut encoded = vec![0xfb];
    encoded.extend_from_slice(&value.to_be_bytes());
    encoded
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn wrapped_generic_arguments_resolve_in_the_caller() {
    for schema in super::WRAPPED_GENERIC_SCHEMAS {
      for depth in [1, 2, 8] {
        let mut valid = vec![0x81; depth];
        valid.extend_from_slice(FLOAT64_1_5);
        assert!(validate_cbor(schema, &valid), "{}", schema);
        let mut invalid = vec![0x81; depth];
        invalid.extend_from_slice(FLOAT64_1_1);
        assert!(!validate_cbor(schema, &invalid), "{}", schema);
      }
    }
    assert!(!validate_cbor(
      "x = a<float16>\na<t> = t / a<(t)>",
      FLOAT64_1_1,
    ));
    assert!(!validate_cbor(
      "x = a<float64>\na<t> = t / [a<(t .lt 1.0)>]",
      &[vec![0x81], encoded_float(1.5)].concat(),
    ));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn constrained_singleton_controllers_preserve_outer_comparisons() {
    for (schema, value, expected) in super::comparison_cases() {
      assert_eq!(
        validate_cbor(&schema, &encoded_float(value)),
        expected,
        "{}",
        schema
      );
    }
    for zero in [
      "zero = (float64 .eq 0.0) .ge 0.0",
      "zero = singleton<float64, 0.0>\nsingleton<t, v> = t .eq v",
      "zero = 0.0 / zero",
    ] {
      let schema = format!("x = compare<float64, zero>\ncompare<t, c> = t .gt c\n{zero}");
      assert!(validate_cbor(&schema, FLOAT64_1_5), "{}", schema);
      assert!(!validate_cbor(&schema, &encoded_float(0.0)), "{}", schema);
    }
    assert!(!validate_cbor(
      "x = float64 .gt bad\nbad = float16 .eq 1.1",
      FLOAT64_1_5,
    ));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn unconstrained_generic_targets_keep_text_byte_and_bits_controls() {
    for (data, expected) in [
      (&b"\x62ab"[..], false),
      (&b"\x63abc"[..], true),
      (&b"\x64abcd"[..], false),
    ] {
      for controller in ["n", "3"] {
        let schema = format!("x = sized<tstr>\nsized<t> = t .size {controller}\nn = 3");
        assert_eq!(validate_cbor(&schema, data), expected);
      }
    }
    for (data, expected) in [
      (&b"\x42ab"[..], false),
      (&b"\x43abc"[..], true),
      (&b"\x44abcd"[..], false),
    ] {
      assert_eq!(
        validate_cbor("x = sized<bstr>\nsized<t> = t .size n\nn = 3", data),
        expected
      );
    }
    for (data, expected) in [(&b"\x01"[..], false), (&b"\x02"[..], true)] {
      assert_eq!(
        validate_cbor("x = bit<uint>\nbit<t> = t .bits n\nn = 1", data),
        expected
      );
    }
  }

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
  fn wrapped_generic_arguments_resolve_in_the_caller() {
    for schema in super::WRAPPED_GENERIC_SCHEMAS {
      for depth in [1, 2, 8] {
        let valid = format!("{}1.5{}", "[".repeat(depth), "]".repeat(depth));
        let invalid = format!("{}1.1{}", "[".repeat(depth), "]".repeat(depth));
        assert!(validate_json(schema, &valid), "{}", schema);
        assert!(!validate_json(schema, &invalid), "{}", schema);
      }
    }
    assert!(!validate_json("x = a<float16>\na<t> = t / a<(t)>", "1.1"));
    assert!(!validate_json(
      "x = a<float64>\na<t> = t / [a<(t .lt 1.0)>]",
      "[1.5]",
    ));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn constrained_singleton_controllers_preserve_outer_comparisons() {
    for (schema, value, expected) in super::comparison_cases() {
      let data = format!("{value:.1}");
      assert_eq!(validate_json(&schema, &data), expected, "{}", schema);
    }
    for zero in [
      "zero = (float64 .eq 0.0) .ge 0.0",
      "zero = singleton<float64, 0.0>\nsingleton<t, v> = t .eq v",
      "zero = 0.0 / zero",
    ] {
      let schema = format!("x = compare<float64, zero>\ncompare<t, c> = t .gt c\n{zero}");
      assert!(validate_json(&schema, "1.5"), "{}", schema);
      assert!(!validate_json(&schema, "0.0"), "{}", schema);
    }
    assert!(!validate_json(
      "x = float64 .gt bad\nbad = float16 .eq 1.1",
      "1.5"
    ));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn unconstrained_generic_targets_keep_text_size_controls() {
    for (data, expected) in [(r#""ab""#, false), (r#""abc""#, true), (r#""abcd""#, false)] {
      for controller in ["n", "3"] {
        let schema = format!("x = sized<tstr>\nsized<t> = t .size {controller}\nn = 3");
        assert_eq!(validate_json(&schema, data), expected);
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn exact_binary32_decimals_roundtrip_through_json() {
    const EXACT: &str = "956.0343017578125";
    for schema in [
      "x = float32",
      "x = float32 .eq 956.0343017578125",
      "x = 956.0343017578125",
    ] {
      assert!(validate_json(schema, EXACT), "{}", schema);
    }
    assert!(!validate_json("x = float16", EXACT));
    assert!(!validate_json("x = 956.0343017578125", "956.0343017578124"));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn integer_controls_keep_the_full_unsigned_json_domain() {
    for data in ["9223372036854775808", "18446744073709551615"] {
      for target in ["int", "number", "uint"] {
        for bound in ["0", "-1"] {
          let schema = format!("x = {target} .gt {bound}");
          assert!(validate_json(&schema, data), "{}", schema);
        }
      }
      assert!(!validate_json("x = float64 .gt 0.0", data));
    }
    assert!(validate_json("x = int .gt -2", "-1"));
    assert!(validate_json("x = number .gt -2", "-1"));
    assert!(!validate_json("x = uint .gt -2", "-1"));
  }

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
