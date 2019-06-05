extern crate cddl;

use cddl::{lexer::Lexer, parser::Parser};
use std::fs;

#[test]
fn verify_cddl_compiles() {
  let cddl_contents = fs::read_to_string("tests/cddl/test.cddl").unwrap();
  let mut l = Lexer::new(&cddl_contents);
  let mut p = Parser::new(&mut l).unwrap();
  println!("{:#?}", p.parse_cddl().unwrap());
}
