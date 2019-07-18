#![cfg(feature = "std")]

extern crate cddl;

#[cfg(feature = "std")]
use cddl::{
  parser::ParserError,
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

    let cddl_contents = fs::read_to_string(file.path()).unwrap();
    println!("file: {:#?}", file.path());
    println!("{:#?}", cddl::cddl_from_str(&cddl_contents)?);
  }

  Ok(())
}

#[test]
fn verify_json_validation() -> Result<(), ValidationError> {
  let reputon_cddl = fs::read_to_string("tests/cddl/reputon.cddl").unwrap();
  let reputon_json = fs::read_to_string("tests/json/reputon.json").unwrap();

  validate_json_from_str(&reputon_cddl, &reputon_json)
}
