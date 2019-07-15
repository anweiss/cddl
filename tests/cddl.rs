#![cfg(feature = "std")]

extern crate cddl;

#[cfg(feature = "std")]
use cddl::{
  lexer::Lexer,
  parser::Parser,
  validator::{validate_json_from_str, ValidationError},
};
use std::fs;

#[test]
fn verify_cddl_compiles() {
  for file in fs::read_dir("tests/cddl/").unwrap() {
    let file = file.unwrap();
    let cddl_contents = fs::read_to_string(file.path()).unwrap();
    let mut l = Lexer::new(&cddl_contents);
    let mut p = Parser::new(&mut l).unwrap();
    println!("{:#?}", p.parse_cddl().unwrap());
  }
}

#[test]
fn verify_json_validation() -> Result<(), ValidationError> {
  let reputon_cddl = fs::read_to_string("tests/cddl/reputon.cddl").unwrap();
  let reputon_json = fs::read_to_string("tests/json/reputon.json").unwrap();

  validate_json_from_str(&reputon_cddl, &reputon_json)
}
