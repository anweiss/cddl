extern crate cddl;

use cddl::{lexer::Lexer, parser::Parser};
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
