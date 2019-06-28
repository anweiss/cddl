extern crate cddl;

use cddl::{lexer::Lexer, parser::Parser, validator::validate_json};
use serde_json;
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
fn verify_json_validation() {
  let reputon_cddl = fs::read_to_string("tests/cddl/reputon.cddl").unwrap();
  let reputon_json = fs::read_to_string("tests/json/reputon.json").unwrap();

  let mut l = Lexer::new(&reputon_cddl);
  let mut p = Parser::new(&mut l).unwrap();

  let json: serde_json::Value = serde_json::from_str(&reputon_json).unwrap();

  validate_json(&p.parse_cddl().unwrap(), &json).unwrap();
}
