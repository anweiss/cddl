#![cfg(feature = "std")]

mod data;

use cddl::{
  parser,
  validation::{json::validate_json_from_str, Error},
};
use pretty_assertions::assert_eq;
use std::fs;

#[test]
fn verify_cddl_compiles() -> Result<(), String> {
  for file in fs::read_dir("tests/data/cddl/").unwrap() {
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
fn verify_json_validation() -> Result<(), Error> {
  validate_json_from_str(
    &fs::read_to_string("tests/data/cddl/reputon.cddl").unwrap(),
    &fs::read_to_string("tests/data/json/reputon.json").unwrap(),
  )
}

#[test]
fn verify_ast_correctness() -> Result<(), Box<dyn std::error::Error>> {
  assert_eq!(
    parser::cddl_from_str(std::str::from_utf8(include_bytes!(
      "data/cddl/reputon.cddl"
    ))?)?,
    data::reputon()
  );

  Ok(())
}
