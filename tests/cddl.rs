#![cfg(feature = "std")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{lexer_from_str, parser, validate_json_from_str, validator::json};
use std::fs;

#[test]
fn verify_cddl_compiles() -> Result<(), parser::Error> {
  for file in fs::read_dir("tests/fixtures/cddl/").unwrap() {
    let file = file.unwrap();

    if file.path().extension().unwrap() != "cddl" {
      continue;
    }

    let file_content = fs::read_to_string(file.path()).unwrap();
    match parser::cddl_from_str(&mut lexer_from_str(&file_content), &file_content, true) {
      Ok(_) => println!("file: {:#?} ... success", file.path()),
      Err(_) => {
        return Err(parser::Error::PARSER);
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
