#![cfg(feature = "std")]

extern crate cddl;

#[cfg(feature = "std")]
use cddl::{
  parser::{self, ParserError},
  validator::{validate_json_from_str, ValidationError},
};
use std::fs;

#[test]
fn verify_cddl_compiles() -> Result<(), ParserError> {
  for file in fs::read_dir("tests/cddl/").unwrap() {
    let file = file.unwrap();

    if file.path().extension().unwrap() != "cddl" {
      continue;
    }

    println!("file: {:#?}", file.path());
    println!(
      "{:#?}",
      parser::cddl_from_str(&fs::read_to_string(file.path()).unwrap())?
    );
  }

  Ok(())
}

#[test]
fn verify_json_validation() -> Result<(), ValidationError> {
  validate_json_from_str(
    &fs::read_to_string("tests/cddl/reputon.cddl").unwrap(),
    &fs::read_to_string("tests/json/reputon.json").unwrap(),
  )
}
