#![cfg(feature = "std")]
#![cfg(feature = "additional-controls")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{parser, validate_json_from_str, validator::json};
use std::fs;

#[test]
fn verify_cddl_compiles() -> std::result::Result<(), String> {
  for file in fs::read_dir("tests/fixtures/cddl/").unwrap() {
    let file = file.unwrap();

    if file.path().extension().unwrap() != "cddl" {
      continue;
    }

    let file_content = fs::read_to_string(file.path()).unwrap();
    match parser::cddl_from_str(&file_content, true) {
      Ok(_) => println!("file: {:#?} ... success", file.path()),
      Err(e) => {
        return Err(e);
      }
    }
  }

  Ok(())
}

#[test]
fn verify_json_validation() -> json::Result {
  validate_json_from_str(
    &fs::read_to_string("tests/fixtures/cddl/reputon.cddl").unwrap(),
    &fs::read_to_string("tests/fixtures/json/reputon.json").unwrap(),
    None,
  )
}

/// Regression test for https://github.com/anweiss/cddl/issues/465
#[test]
fn validate_json_array_record_extra_elements() {
  let cddl_input = r#"thing = [a: tstr, b: int]"#;

  // Exact match should pass
  validate_json_from_str(cddl_input, r#"["testString", 1]"#, None).unwrap();

  // Extra element must fail
  validate_json_from_str(cddl_input, r#"["testString", 1, 2]"#, None).unwrap_err();

  // Too few elements must fail
  validate_json_from_str(cddl_input, r#"["testString"]"#, None).unwrap_err();
}

#[cfg(test)]
mod test_rfc9165_controls {
  use cddl::cddl_from_str;

  #[test]
  fn test_standard_control_operators_parse() {
    // Test that all standard control operators from RFC 8610 can be parsed
    let cddl_text = r#"
size_test = uint .size 4
bits_test = uint .bits 8
regexp_test = tstr .regexp "[a-z]+"
within_test = int .within int
and_test = int .and uint
lt_test = int .lt 100
le_test = int .le 100
gt_test = int .gt 0
ge_test = int .ge 0
eq_test = int .eq 42
ne_test = int .ne 0
default_test = int .default 0
        "#;

    let result = cddl_from_str(cddl_text, true);
    assert!(
      result.is_ok(),
      "Failed to parse CDDL with standard control operators: {:?}",
      result.err()
    );

    let cddl = result.unwrap();
    // Note: The parser includes the CDDL prelude rules, so the count will be higher
    assert!(
      cddl.rules.len() >= 11,
      "Expected at least 11 rules, got {}",
      cddl.rules.len()
    );
  }

  #[test]
  #[cfg(feature = "additional-controls")]
  fn test_rfc9165_additional_operators() {
    // Test RFC 9165 and RFC 9741 additional control operators
    let cddl_text = r#"
cat_test = tstr .cat "suffix"
det_test = tstr .det "test"
plus_test = int .plus 10
b64u_test = bstr .b64u "test"
hex_test = bstr .hex "test"
        "#;

    let result = cddl_from_str(cddl_text, true);
    assert!(
      result.is_ok(),
      "Failed to parse RFC 9165 additional operators: {:?}",
      result.err()
    );

    let cddl = result.unwrap();
    // Note: The parser includes the CDDL prelude rules, so the count will be higher
    assert!(
      cddl.rules.len() >= 5,
      "Expected at least 5 rules, got {}",
      cddl.rules.len()
    );
  }
}

/// Regression: type-domain string keys (`tstr => v`) in objects were rejected
/// even when present, and `?`-marked ones rejected the spec-valid empty object
#[test]
fn validate_json_optional_type_domain_object_key() {
  // `?` permits the entry to be absent (RFC 8610 §3.2)
  validate_json_from_str(r#"m = { ? tstr => uint }"#, r#"{}"#, None).unwrap();
  // present entry validates
  validate_json_from_str(r#"m = { ? tstr => uint }"#, r#"{"a": 1}"#, None).unwrap();
  validate_json_from_str(r#"m = { tstr => uint }"#, r#"{"a": 1}"#, None).unwrap();
  // present entry with a bad value type still fails
  validate_json_from_str(r#"m = { ? tstr => uint }"#, r#"{"a": "b"}"#, None).unwrap_err();
  // an occurrence-less entry still requires a match
  validate_json_from_str(r#"m = { tstr => uint }"#, r#"{}"#, None).unwrap_err();
  // a bare string type against an object still fails (not treated as a key)
  validate_json_from_str(r#"m = tstr"#, r#"{"a": 1}"#, None).unwrap_err();
}

/// Regression: an object entry unmatched by any group member must be rejected
/// as an unexpected key, even when no group member consumed any key at all
#[test]
fn validate_json_unexpected_entries_rejected() {
  validate_json_from_str(r#"m = { ? k: uint }"#, r#"{}"#, None).unwrap();
  validate_json_from_str(r#"m = { ? k: uint }"#, r#"{"k": 1}"#, None).unwrap();
  validate_json_from_str(r#"m = { ? k: uint }"#, r#"{"a": 1}"#, None).unwrap_err();
  validate_json_from_str(r#"m = { ? k: uint }"#, r#"{"k": 1, "a": 2}"#, None).unwrap_err();
}

/// Regression: `* any => any` (the RFC 8610 extension-point idiom) must
/// permit unknown extra entries instead of rejecting them as unexpected keys
#[test]
fn validate_json_any_key_permits_extra_entries() {
  // the openness idiom, with and without other members, in both member orders
  validate_json_from_str(r#"m = { k: uint, * any => any }"#, r#"{"k": 1}"#, None).unwrap();
  validate_json_from_str(
    r#"m = { k: uint, * any => any }"#,
    r#"{"k": 1, "z": 9}"#,
    None,
  )
  .unwrap();
  validate_json_from_str(
    r#"m = { * any => any, k: uint }"#,
    r#"{"k": 1, "z": 9}"#,
    None,
  )
  .unwrap();
  // colon shortcut form
  validate_json_from_str(
    r#"m = { k: uint, * any: any }"#,
    r#"{"k": 1, "z": 9}"#,
    None,
  )
  .unwrap();
  validate_json_from_str(r#"m = { * any => any }"#, r#"{}"#, None).unwrap();
  validate_json_from_str(r#"m = { * any => any }"#, r#"{"z": 9}"#, None).unwrap();
  // `+` still requires at least one entry
  validate_json_from_str(r#"m = { + any => any }"#, r#"{}"#, None).unwrap_err();
  validate_json_from_str(r#"m = { + any => any }"#, r#"{"z": 9}"#, None).unwrap();
  // an occurrence-less `any` key consumes an entry
  validate_json_from_str(r#"m = { any => any }"#, r#"{"z": 9}"#, None).unwrap();
  // an `any` key does not excuse a value-type mismatch
  validate_json_from_str(
    r#"m = { k: uint, * any => uint }"#,
    r#"{"k": 1, "z": "s"}"#,
    None,
  )
  .unwrap_err();
  // a map without the extension member stays closed
  validate_json_from_str(r#"m = { k: uint }"#, r#"{"k": 1, "z": 9}"#, None).unwrap_err();
}

/// A wrong value type under a cut-carrying key (`k: uint` — the colon
/// shortcut implies a cut, RFC 8610 §3.5.4) must NOT be rescued by a
/// `* any => any` extension member, and the error must name the value-type
/// mismatch rather than anything about the `any` branch
#[test]
fn validate_json_any_key_does_not_rescue_cut_value_mismatch() {
  for schema in [
    r#"m = { k: uint, * any => any }"#,
    r#"m = { k: uint, any => any }"#,
  ] {
    let err = validate_json_from_str(schema, r#"{"k": "s", "z": 9}"#, None)
      .unwrap_err()
      .to_string();
    assert!(
      err.contains(r#"/k: expected type uint, got "s""#),
      "schema {}: expected a value-type error for k, got: {}",
      schema,
      err
    );
  }
}
