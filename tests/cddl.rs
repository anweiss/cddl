#![cfg(feature = "std")]

mod data;

use cddl::{
  parser,
  validation::{self, json::validate_json_from_str},
};
use std::fs;

#[test]
fn verify_cddl_compiles() -> Result<(), parser::Error> {
  for file in fs::read_dir("tests/data/cddl/").unwrap() {
    let file = file.unwrap();

    if file.path().extension().unwrap() != "cddl" {
      continue;
    }

    match parser::cddl_from_str(&fs::read_to_string(file.path()).unwrap()) {
      Ok(_) => println!("file: {:#?} ... success", file.path()),
      Err(e) => {
        println!("{}", e);

        return Err(parser::Error::PARSER);
      }
    }
  }

  Ok(())
}

#[test]
fn verify_json_validation() -> Result<(), validation::Error> {
  validate_json_from_str(
    &fs::read_to_string("tests/data/cddl/reputon.cddl").unwrap(),
    &fs::read_to_string("tests/data/json/reputon.json").unwrap(),
  )
}

// #[test]
// fn verify_ast_correctness() -> Result<(), Box<dyn std::error::Error>> {
//   let c = parser::cddl_from_str(std::str::from_utf8(include_bytes!(
//     "data/cddl/reputon.cddl"
//   ))?)?;

//   assert_eq!(c, data::reputon());

//   Ok(())
// }
