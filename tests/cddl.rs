#![cfg(feature = "std")]
#![cfg(feature = "additional-controls")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{parser, validate_json_from_str, validator::json};
use std::fs;

#[test]
fn verify_cddl_compiles() -> Result<(), parser::Error> {
  for file in fs::read_dir("tests/fixtures/cddl/").unwrap() {
    let file = file.unwrap();

    if file.path().extension().unwrap() != "cddl" {
      continue;
    }

    let file_content = fs::read_to_string(file.path()).unwrap();
    match parser::cddl_from_str(&file_content, true) {
      Ok(_) => println!("file: {:#?} ... success", file.path()),
      Err(e) => {
        return Err(parser::Error::CDDL(format!(
          "Failed to parse {}: {}",
          file.path().display(),
          e
        )));
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
