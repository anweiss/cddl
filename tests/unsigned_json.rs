#![cfg(all(feature = "json", not(feature = "lsp"), not(target_arch = "wasm32")))]

#[test]
fn unsigned_json_rejects_negative_and_fractional_values() {
  for schema in [
    "root = uint",
    "root = unsigned",
    "root = alias\nalias = uint",
  ] {
    for value in ["-1", "-9223372036854775808", "1.5"] {
      #[cfg(feature = "additional-controls")]
      let result = cddl::validate_json_from_str(schema, value, None);
      #[cfg(not(feature = "additional-controls"))]
      let result = cddl::validate_json_from_str(schema, value);
      assert!(
        result.is_err(),
        "{} unexpectedly accepted {}",
        schema,
        value
      );
    }
    for value in ["0", "1", "18446744073709551615"] {
      #[cfg(feature = "additional-controls")]
      let result = cddl::validate_json_from_str(schema, value, None);
      #[cfg(not(feature = "additional-controls"))]
      let result = cddl::validate_json_from_str(schema, value);
      assert!(result.is_ok(), "{} unexpectedly rejected {}", schema, value);
    }
  }
}
