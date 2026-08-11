//! RFC 8610 primitive integer value-domain regressions.
//!
//! Normative Appendix D defines `uint = #0`, `nint = #1`,
//! `int = uint / nint`, and `unsigned = uint / biguint`. The tests below
//! exercise the untagged integer arms in ordinary values and control targets.
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

  const NEGATIVE_ONE: &[u8] = b"\x20";
  const ZERO: &[u8] = b"\x00";
  const ONE: &[u8] = b"\x01";
  const FLOAT_ONE: &[u8] = b"\xf9\x3c\x00";

  #[test]
  fn enforces_direct_primitive_integer_domains() {
    for schema in ["x = uint", "x = unsigned"] {
      assert!(!validate_cbor(schema, NEGATIVE_ONE), "{}: -1", schema);
      assert!(validate_cbor(schema, ZERO), "{}: 0", schema);
      assert!(validate_cbor(schema, ONE), "{}: 1", schema);
      assert!(!validate_cbor(schema, FLOAT_ONE), "{}: float", schema);
    }

    assert!(validate_cbor("x = nint", NEGATIVE_ONE));
    assert!(!validate_cbor("x = nint", ZERO));
    assert!(!validate_cbor("x = nint", ONE));
    assert!(!validate_cbor("x = nint", FLOAT_ONE));

    for schema in ["x = int", "x = integer"] {
      assert!(validate_cbor(schema, NEGATIVE_ONE), "{}: -1", schema);
      assert!(validate_cbor(schema, ZERO), "{}: 0", schema);
      assert!(validate_cbor(schema, ONE), "{}: 1", schema);
      assert!(!validate_cbor(schema, FLOAT_ONE), "{}: float", schema);
    }

    for data in [NEGATIVE_ONE, ZERO, ONE, FLOAT_ONE] {
      assert!(validate_cbor("x = number", data));
    }
  }

  #[test]
  fn preserves_integer_domains_through_value_aliases_and_choices() {
    let unsigned_alias = "x = small\nsmall = unsigned";
    assert!(!validate_cbor(unsigned_alias, NEGATIVE_ONE));
    assert!(validate_cbor(unsigned_alias, ZERO));

    let either_sign = "x = integer-choice\ninteger-choice = uint / nint";
    assert!(validate_cbor(either_sign, NEGATIVE_ONE));
    assert!(validate_cbor(either_sign, ZERO));

    let unsigned_or_float = "x = numeric-choice\nnumeric-choice = unsigned / float";
    assert!(!validate_cbor(unsigned_or_float, NEGATIVE_ONE));
    assert!(validate_cbor(unsigned_or_float, ZERO));
    assert!(validate_cbor(unsigned_or_float, FLOAT_ONE));
  }

  #[test]
  fn gates_control_targets_by_integer_domain() {
    assert!(!validate_cbor("x = unsigned .eq -1", NEGATIVE_ONE));
    assert!(!validate_cbor("x = nint .eq 0", ZERO));

    assert!(validate_cbor("x = unsigned .eq 0", ZERO));
    assert!(validate_cbor("x = nint .eq -1", NEGATIVE_ONE));

    // The target matches, but the controller does not.
    assert!(!validate_cbor("x = unsigned .eq 1", ZERO));
    assert!(!validate_cbor("x = nint .eq -2", NEGATIVE_ONE));
  }

  #[test]
  fn gates_aliased_and_choice_control_targets_semantically() {
    let unsigned_alias = "x = small .eq -1\nsmall = unsigned";
    assert!(!validate_cbor(unsigned_alias, NEGATIVE_ONE));

    let either_sign = "x = integer-choice .eq -1\ninteger-choice = uint / nint";
    assert!(validate_cbor(either_sign, NEGATIVE_ONE));

    // Literal/composite target validation is not reducible to a sign domain.
    let literal_choice = "x = integer-choice .eq 1\ninteger-choice = 1 / nint";
    assert!(validate_cbor(literal_choice, ONE));

    // `any` can admit integers but has no sign-only domain of its own.
    let unbounded_choice = "x = integer-choice .eq -1\ninteger-choice = unsigned / any";
    assert!(validate_cbor(unbounded_choice, NEGATIVE_ONE));
  }
}

#[cfg(feature = "json")]
mod json {
  use super::validate_json;

  #[test]
  fn enforces_direct_primitive_integer_domains() {
    for schema in ["x = uint", "x = unsigned"] {
      assert!(!validate_json(schema, "-1"), "{}: -1", schema);
      assert!(validate_json(schema, "0"), "{}: 0", schema);
      assert!(validate_json(schema, "1"), "{}: 1", schema);
      assert!(!validate_json(schema, "1.0"), "{}: float", schema);
    }

    assert!(validate_json("x = nint", "-1"));
    assert!(!validate_json("x = nint", "0"));
    assert!(!validate_json("x = nint", "1"));
    assert!(!validate_json("x = nint", "1.0"));

    for schema in ["x = int", "x = integer"] {
      assert!(validate_json(schema, "-1"), "{}: -1", schema);
      assert!(validate_json(schema, "0"), "{}: 0", schema);
      assert!(validate_json(schema, "1"), "{}: 1", schema);
      assert!(!validate_json(schema, "1.0"), "{}: float", schema);
    }

    for data in ["-1", "0", "1", "1.0"] {
      assert!(validate_json("x = number", data), "number: {}", data);
    }
  }

  #[test]
  fn preserves_integer_domains_through_value_aliases_and_choices() {
    let unsigned_alias = "x = small\nsmall = unsigned";
    assert!(!validate_json(unsigned_alias, "-1"));
    assert!(validate_json(unsigned_alias, "0"));

    let either_sign = "x = integer-choice\ninteger-choice = uint / nint";
    assert!(validate_json(either_sign, "-1"));
    assert!(validate_json(either_sign, "0"));

    let unsigned_or_float = "x = numeric-choice\nnumeric-choice = unsigned / float";
    assert!(!validate_json(unsigned_or_float, "-1"));
    assert!(validate_json(unsigned_or_float, "0"));
    assert!(validate_json(unsigned_or_float, "1.0"));
  }

  #[test]
  fn gates_control_targets_by_integer_domain() {
    assert!(!validate_json("x = unsigned .eq -1", "-1"));
    assert!(!validate_json("x = nint .eq 0", "0"));

    assert!(validate_json("x = unsigned .eq 0", "0"));
    assert!(validate_json("x = nint .eq -1", "-1"));

    // The target matches, but the controller does not.
    assert!(!validate_json("x = unsigned .eq 1", "0"));
    assert!(!validate_json("x = nint .eq -2", "-1"));
  }

  #[test]
  fn gates_aliased_and_choice_control_targets_semantically() {
    let unsigned_alias = "x = small .eq -1\nsmall = unsigned";
    assert!(!validate_json(unsigned_alias, "-1"));

    let either_sign = "x = integer-choice .eq -1\ninteger-choice = uint / nint";
    assert!(validate_json(either_sign, "-1"));

    // Literal/composite target validation is not reducible to a sign domain.
    let literal_choice = "x = integer-choice .eq 1\ninteger-choice = 1 / nint";
    assert!(validate_json(literal_choice, "1"));

    // `any` can admit integers but has no sign-only domain of its own.
    let unbounded_choice = "x = integer-choice .eq -1\ninteger-choice = unsigned / any";
    assert!(validate_json(unbounded_choice, "-1"));
  }
}
