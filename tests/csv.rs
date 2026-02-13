#![cfg(feature = "std")]
#![cfg(feature = "csv-validate")]
#![cfg(feature = "additional-controls")]
#![cfg(not(target_arch = "wasm32"))]

//! Integration tests for CSV validation (draft-bormann-cbor-cddl-csv-07)

use cddl::validate_csv_from_str;
use std::fs;

#[test]
fn validate_simple_csv() {
  let cddl = fs::read_to_string("tests/fixtures/cddl/csv-simple.cddl").unwrap();
  let csv = fs::read_to_string("tests/fixtures/csv/simple.csv").unwrap();
  let result = validate_csv_from_str(&cddl, &csv, None, None);
  assert!(result.is_ok(), "Validation failed: {:?}", result.err());
}

#[test]
fn validate_csv_with_header() {
  let cddl = fs::read_to_string("tests/fixtures/cddl/csv-with-header.cddl").unwrap();
  let csv = fs::read_to_string("tests/fixtures/csv/with-header.csv").unwrap();
  let result = validate_csv_from_str(&cddl, &csv, Some(true), None);
  assert!(result.is_ok(), "Validation failed: {:?}", result.err());
}

#[test]
fn validate_sid_file_csv() {
  let cddl = fs::read_to_string("tests/fixtures/cddl/csv-sid-file.cddl").unwrap();
  let csv = fs::read_to_string("tests/fixtures/csv/sid-file.csv").unwrap();
  let result = validate_csv_from_str(&cddl, &csv, None, None);
  assert!(result.is_ok(), "Validation failed: {:?}", result.err());
}

#[test]
fn validate_csv_type_mismatch() {
  // CDDL expects uint for age, but CSV has text "abc"
  let cddl = r#"
    records = [*record]
    record = [name: text, age: uint]
  "#;
  let csv = "Alice,abc\n";
  let result = validate_csv_from_str(cddl, csv, None, None);
  assert!(
    result.is_err(),
    "Expected validation error for type mismatch"
  );
}

#[test]
fn validate_csv_empty() {
  let cddl = r#"
    records = [*record]
    record = [text, uint]
  "#;
  let csv = "";
  let result = validate_csv_from_str(cddl, csv, None, None);
  assert!(result.is_ok(), "Empty CSV should validate against *record");
}

#[test]
fn validate_csv_with_empty_fields() {
  let cddl = r#"
    records = [*record]
    record = [key: text, value: text]
  "#;
  let csv = "hello,\nworld,foo\n";
  let result = validate_csv_from_str(cddl, csv, None, None);
  assert!(
    result.is_ok(),
    "CSV with empty fields should validate: {:?}",
    result.err()
  );
}

#[test]
fn validate_csv_wrong_field_count() {
  let cddl = r#"
    records = [*record]
    record = [text, text, text]
  "#;
  let csv = "a,b\n";
  let result = validate_csv_from_str(cddl, csv, None, None);
  assert!(
    result.is_err(),
    "Expected validation error for wrong number of fields"
  );
}

#[test]
fn validate_csv_inline_cddl() {
  let cddl = r#"
    data = [*[text, uint, text]]
  "#;
  let csv = "foo,42,bar\nbaz,10,qux\n";
  let result = validate_csv_from_str(cddl, csv, None, None);
  assert!(result.is_ok(), "Validation failed: {:?}", result.err());
}

#[test]
fn validate_csv_mixed_record_types() {
  // Test with choice types (different record formats)
  let cddl = r#"
    file = [*record]
    record = meta-record / data-record
    meta-record = ["meta", text]
    data-record = [uint, text]
  "#;
  let csv = "meta,info\n42,hello\n";
  let result = validate_csv_from_str(cddl, csv, None, None);
  assert!(result.is_ok(), "Validation failed: {:?}", result.err());
}

#[test]
fn validate_csv_negative_numbers() {
  let cddl = r#"
    data = [*[text, int]]
  "#;
  let csv = "temp,-5\npressure,-10\n";
  let result = validate_csv_from_str(cddl, csv, None, None);
  assert!(result.is_ok(), "Validation failed: {:?}", result.err());
}

#[test]
fn validate_csv_float_values() {
  let cddl = r#"
    data = [*[text, float]]
  "#;
  let csv = "pi,3.14\ne,2.718\n";
  let result = validate_csv_from_str(cddl, csv, None, None);
  assert!(result.is_ok(), "Validation failed: {:?}", result.err());
}
