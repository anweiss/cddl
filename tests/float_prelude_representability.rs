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

fn assert_numeric_case(schema: &str, data: &str, expected: bool) {
  #[cfg(feature = "json")]
  assert_eq!(
    validate_json(schema, data),
    expected,
    "JSON: {schema}, {data}"
  );
  #[cfg(feature = "cbor")]
  {
    let value: serde_json::Value = serde_json::from_str(data).unwrap();
    let mut encoded = Vec::new();
    ciborium::ser::into_writer(&value, &mut encoded).unwrap();
    assert_eq!(
      validate_cbor(schema, &encoded),
      expected,
      "CBOR: {schema}, {data}"
    );
  }
}

#[cfg(not(target_arch = "wasm32"))]
fn isolated_numeric_repro(name: &str) -> bool {
  if std::env::var("CDDL_NUMERIC_REPRO_CHILD").as_deref() == Ok(name) {
    return false;
  }
  let status = std::process::Command::new(std::env::current_exe().unwrap())
    .args(["--exact", name])
    .env("CDDL_NUMERIC_REPRO_CHILD", name)
    .stdout(std::process::Stdio::null())
    .stderr(std::process::Stdio::null())
    .status()
    .unwrap();
  assert!(
    status.success(),
    "isolated numeric regression failed: {}",
    status
  );
  true
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn numeric_target_guards_distinguish_active_instantiations() {
  for (argument, expected) in [("float64", true), ("float16", false)] {
    let schema = format!("x = a<float16>\na<t> = b<t> .gt 0.0\nb<t> = t / a<{argument}>");
    assert_numeric_case(&schema, "1.1", expected);
    assert_numeric_case(&schema, "1.5", true);
  }
  assert_numeric_case(
    "x = a<float16>\na<t> = t<float64> .gt 0.0\nt<u> = u",
    "1.1",
    true,
  );
  assert_numeric_case(
    "x = a<float64>\na<t> = t .gt t<float64>\nt<u> = u .eq 0.0",
    "1.5",
    true,
  );
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn generic_control_substitution_uses_current_binding() {
  #[cfg(not(target_arch = "wasm32"))]
  if isolated_numeric_repro("generic_control_substitution_uses_current_binding") {
    return;
  }
  for controller in ["0.0", "zero"] {
    let schema =
      format!("x = limited<float64>\nlimited<float64> = float64 .gt {controller}\nzero = 0.0");
    assert_numeric_case(&schema, "1.5", true);
    assert_numeric_case(&schema, "0.0", false);
    let schema =
      format!("x = a<float64>\na<t> = t .gt {controller} / [a<(t .lt 1.0)>]\nzero = 0.0");
    assert_numeric_case(&schema, "[1.5]", false);
    assert_numeric_case(&schema, "[0.5]", true);
    assert_numeric_case(&schema, "[[1.5]]", false);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn parameter_guards_distinguish_nested_identity_bindings() {
  for argument in ["float16", "(float16 .gt 0.0)"] {
    let schema = format!("x = identity<identity<{argument}>>\nidentity<t> = t");
    assert_numeric_case(&schema, "1.5", true);
    assert_numeric_case(&schema, "1.1", false);
  }
  assert_numeric_case("x = a\na = a", "1.5", false);
  assert_numeric_case("x = identity<a>\nidentity<t> = t\na = a", "1.5", false);
  assert_numeric_case(
    "x = outer<float16>\nouter<t> = $t .gt 0.0\n$t = float64",
    "1.1",
    true,
  );
  assert_numeric_case(
    "x = outer<float16>\nouter<t> = id<$t>\nid<u> = u\n$t = float64",
    "1.1",
    true,
  );
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn mixed_numeric_ordering_preserves_integer_boundaries() {
  for (operator, negative, zero, positive) in [
    ("lt", true, false, false),
    ("le", true, true, false),
    ("gt", false, false, true),
    ("ge", false, true, true),
  ] {
    for (data, expected) in [("-1", negative), ("0", zero), ("1", positive)] {
      assert_numeric_case(&format!("x = int .{operator} 0.0"), data, expected);
    }
    for (data, expected) in [("-1.5", negative), ("0.0", zero), ("1.5", positive)] {
      assert_numeric_case(&format!("x = float64 .{operator} 0"), data, expected);
    }
  }
  for (schema, data, expected) in [
    (
      "x = int .lt 18446744073709551616.0",
      "18446744073709551615",
      true,
    ),
    (
      "x = int .ge 18446744073709551616.0",
      "18446744073709551615",
      false,
    ),
    (
      "x = int .gt 18446744073709549568.0",
      "18446744073709551615",
      true,
    ),
    (
      "x = int .lt 9223372036854775808.0",
      "9223372036854775807",
      true,
    ),
    (
      "x = int .ge -9223372036854775808.0",
      "-9223372036854775808",
      true,
    ),
    (
      "x = int .gt -9223372036854775808.0",
      "-9223372036854775808",
      false,
    ),
    (
      "x = int .gt -9223372036854777856.0",
      "-9223372036854775808",
      true,
    ),
    ("x = float64 .gt 4294967295", "4294967296.0", true),
    ("x = float64 .le 4294967295", "4294967296.0", false),
    ("x = int .lt 0.5", "0", true),
    ("x = int .gt -0.5", "0", true),
    ("x = number .eq 0", "0.0", false),
    ("x = number .ne 0", "0.0", true),
  ] {
    assert_numeric_case(schema, data, expected);
  }
  // The AST's usize integer literals are limited to 32 bits on wasm32;
  // 64-bit instances versus floating controllers above run on both targets.
  #[cfg(target_pointer_width = "64")]
  for (schema, data, expected) in [
    (
      "x = float64 .gt 18446744073709551615",
      "18446744073709551616.0",
      true,
    ),
    (
      "x = float64 .le 18446744073709551615",
      "18446744073709551616.0",
      false,
    ),
    (
      "x = float64 .eq 18446744073709551615",
      "18446744073709551616.0",
      false,
    ),
    (
      "x = float64 .ne 18446744073709551615",
      "18446744073709551616.0",
      true,
    ),
  ] {
    assert_numeric_case(schema, data, expected);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn singleton_resolution_discards_empty_originating_alternatives() {
  for (empty, definitions) in [
    ("float16 .eq 1.1", ""),
    ("(float64 .lt 1.0) .eq 1.1", ""),
    ("limited<float16>", "\nlimited<t> = t .eq 1.1"),
  ] {
    let schema = format!("x = float64 .gt zero\nzero = 0.0 / ({empty}){definitions}");
    assert_numeric_case(&schema, "1.5", true);
    assert_numeric_case(&schema, "0.0", false);
  }
  assert_numeric_case("x = float64 .gt zero\nzero = 0.0 / 1.1", "1.5", false);
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn changing_controller_arguments_have_a_finite_resolution_budget() {
  #[cfg(not(target_arch = "wasm32"))]
  if isolated_numeric_repro("changing_controller_arguments_have_a_finite_resolution_budget") {
    return;
  }
  for next in ["[t]", "[t, t]"] {
    let schema = format!("x = float64 .gt zero<float64>\nzero<t> = 0.0 / zero<{next}>");
    assert_numeric_case(&schema, "1.5", false);
    assert_numeric_case(
      &format!("x = float64 .ne (float64 .gt zero<float64>)\nzero<t> = 0.0 / zero<{next}>"),
      "1.5",
      false,
    );
  }
  assert_numeric_case("x = float64 .gt zero\nzero = 0.0 .gt zero", "1.5", false);
  for control in ["gt zero", "ne (float64 .gt zero)"] {
    let mut schema =
      format!("x = float64 .{control}\nzero = (bad30 / float64) .eq 0.0\nbad0 = float16 .eq 1.1\n");
    for depth in 1..=30 {
      schema.push_str(&format!(
        "bad{depth} = bad{} / bad{}\n",
        depth - 1,
        depth - 1
      ));
    }
    assert_numeric_case(&schema, "1.5", false);
  }
  assert_numeric_case(
    "x = float64 .gt zero<float64>\nzero<t> = t .eq 0.0",
    "1.5",
    true,
  );
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn completed_singleton_proofs_share_diamond_subgraphs() {
  for base in ["0.0", "(float16 .eq 1.1)", "tstr"] {
    let mut schema = format!("x = float64 .gt c30\nc0 = {base}\n");
    for depth in 1..=30 {
      schema.push_str(&format!("c{depth} = c{} / c{}\n", depth - 1, depth - 1));
    }
    assert_numeric_case(&schema, "1.5", base == "0.0");
  }
  for controller in ["c30 / 0.0", "(c30 / 0.0) .ge 0.0"] {
    let mut schema = format!("x = float64 .gt zero\nzero = {controller}\nc0 = float16 .eq 1.1\n");
    for depth in 1..=30 {
      schema.push_str(&format!("c{depth} = c{} / c{}\n", depth - 1, depth - 1));
    }
    assert_numeric_case(&schema, "1.5", true);
  }
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
  fn mixed_ordering_handles_cbor_special_values_and_extended_negatives() {
    for (value, expected) in [
      (f64::INFINITY, [false, false, true, true]),
      (f64::NEG_INFINITY, [true, true, false, false]),
      (f64::NAN, [false; 4]),
    ] {
      for (operator, expected) in IntoIterator::into_iter(["lt", "le", "gt", "ge"]).zip(expected) {
        assert_eq!(
          validate_cbor(&format!("x = float64 .{operator} 0"), &encoded_float(value)),
          expected,
        );
      }
    }
    let mut negative = vec![0x3b];
    negative.extend_from_slice(&u64::MAX.to_be_bytes());
    for (operator, expected) in [("lt", false), ("le", true), ("gt", false), ("ge", true)] {
      assert_eq!(
        validate_cbor(
          &format!("x = int .{operator} -18446744073709551616.0"),
          &negative
        ),
        expected,
      );
    }
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
