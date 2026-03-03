#![cfg(feature = "codegen")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::codegen::generate_rust_code;
use std::fs;

#[test]
fn codegen_from_fixture_file() {
  let cddl_input =
    fs::read_to_string("tests/fixtures/cddl/codegen-structs.cddl").expect("fixture not found");
  let result = generate_rust_code(&cddl_input).expect("codegen failed");

  // person struct
  assert!(
    result.contains("pub struct Person"),
    "expected Person struct in:\n{}",
    result
  );
  assert!(result.contains("pub name: String,"));
  assert!(result.contains("pub age: u64,"));
  assert!(result.contains("pub email: Option<String>,"));

  // address struct with serde renames for hyphenated keys
  assert!(result.contains("pub struct Address"));
  assert!(result.contains("#[serde(rename = \"zip-code\")]"));
  assert!(result.contains("pub zip_code: String,"));
  assert!(result.contains("pub state: Option<String>,"));

  // contact struct referencing other types
  assert!(result.contains("pub struct Contact"));
  assert!(result.contains("pub person: Person,"));
  assert!(result.contains("#[serde(rename = \"home-address\")]"));
  assert!(result.contains("pub home_address: Address,"));

  // status enum from text choices
  assert!(result.contains("pub enum Status"));
  assert!(result.contains("Active"));
  assert!(result.contains("Inactive"));
  assert!(result.contains("Pending"));

  // measurement struct with float types
  assert!(result.contains("pub struct Measurement"));
  assert!(result.contains("pub value: f64,"));
  assert!(result.contains("pub precision: Option<f64>,"));

  // type alias
  assert!(result.contains("pub type Name = String;"));

  // array type alias
  assert!(result.contains("pub type Ids = Vec<u64>;"));

  // payload struct with Rust keyword field rename
  assert!(result.contains("pub struct Payload"));
  assert!(result.contains("#[serde(rename = \"type\")]"));
  assert!(result.contains("pub type_: String,"));
  assert!(result.contains("pub data: Vec<u8>,"));

  // response struct with nullable field
  assert!(result.contains("pub struct Response"));
  assert!(result.contains("pub body: Option<String>,"));
}

#[test]
fn codegen_reputon_fixture() {
  let cddl_input =
    fs::read_to_string("tests/fixtures/cddl/reputon.cddl").expect("fixture not found");
  let result = generate_rust_code(&cddl_input).expect("codegen failed");

  // Should generate structs for the reputon types
  assert!(
    result.contains("pub struct ReputationObject"),
    "expected ReputationObject struct in:\n{}",
    result
  );
  assert!(result.contains("pub struct Reputon"));
}

#[test]
fn codegen_empty_cddl() {
  // An empty CDDL should produce just the header
  let result = generate_rust_code("").expect("codegen failed");
  assert!(result.contains("Auto-generated from CDDL"));
  assert!(result.contains("use serde::{Deserialize, Serialize};"));
}

#[test]
fn codegen_complex_enum_with_types() {
  let cddl_input = r#"
    value = int / tstr / bool / null
  "#;
  let result = generate_rust_code(cddl_input).expect("codegen failed");
  assert!(result.contains("pub enum Value"));
  assert!(result.contains("Int(i64)"));
  assert!(result.contains("Tstr(String)"));
  assert!(result.contains("Bool(bool)"));
}

#[test]
fn codegen_nested_arrays() {
  let cddl_input = r#"
    matrix = [* [* int]]
  "#;
  let result = generate_rust_code(cddl_input).expect("codegen failed");
  assert!(
    result.contains("Matrix"),
    "expected Matrix type in:\n{}",
    result
  );
}

#[test]
fn codegen_map_table_type() {
  let cddl_input = r#"
    headers = { * tstr => tstr }
  "#;
  let result = generate_rust_code(cddl_input).expect("codegen failed");
  assert!(
    result.contains("HashMap<String, String>"),
    "expected HashMap in:\n{}",
    result
  );
}

#[test]
fn codegen_multiple_rules() {
  let cddl_input = r#"
    point = {
      x: float,
      y: float,
    }
    line = {
      start: point,
      end: point,
    }
    shape = point / line
  "#;
  let result = generate_rust_code(cddl_input).expect("codegen failed");
  assert!(result.contains("pub struct Point"));
  assert!(result.contains("pub struct Line"));
  assert!(result.contains("pub enum Shape"));
  assert!(result.contains("pub start: Point,"));
  assert!(result.contains("pub end: Point,"));
}

#[test]
fn codegen_serde_derives_present() {
  let cddl_input = r#"
    item = { name: tstr }
  "#;
  let result = generate_rust_code(cddl_input).expect("codegen failed");
  assert!(result.contains("#[derive(Clone, Debug, Deserialize, Serialize)]"));
  assert!(result.contains("use serde::{Deserialize, Serialize};"));
}

#[test]
fn codegen_invalid_cddl_returns_error() {
  let result = generate_rust_code("this is not valid cddl !!!");
  assert!(result.is_err());
  let err = result.unwrap_err();
  assert!(
    err.to_string().contains("parse error"),
    "expected parse error, got: {}",
    err
  );
}
