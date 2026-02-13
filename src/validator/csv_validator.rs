#![cfg(feature = "std")]
#![cfg(feature = "csv-validate")]
#![cfg(not(feature = "lsp"))]

//! CSV validation implementation based on draft-bormann-cbor-cddl-csv-07.
//!
//! This module validates CSV data against CDDL definitions. The generic data
//! model for CSV is:
//!
//! ```cddl
//! csv = [?header, *record]
//! header = [+header-field]
//! record = [+field]
//! header-field = text
//! field = text
//! ```
//!
//! CSV fields are text strings, but the CDDL model may specify application-level
//! types such as `uint`, `int`, or `float`. When such types are specified, the
//! validator coerces text fields to their JSON representation as described in
//! the spec: "As a preferred choice, the JSON representation of the data model
//! item, if it exists, MAY be chosen by that instruction."

use std::fmt::{self, Write};

use super::json;

#[cfg(not(target_arch = "wasm32"))]
use crate::cddl_from_str;

#[cfg(target_arch = "wasm32")]
use crate::{error::ErrorMsg, lexer::Position, parser, pest_bridge};
#[cfg(target_arch = "wasm32")]
use serde::Serialize;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
#[derive(Serialize)]
struct ParserError {
  #[cfg(feature = "ast-span")]
  position: Position,
  msg: ErrorMsg,
}

/// CSV validation Result
pub type Result = std::result::Result<(), Error>;

/// CSV validation error
#[derive(Debug)]
pub enum Error {
  /// Zero or more validation errors (from JSON validator)
  Validation(Vec<json::ValidationError>),
  /// CSV parsing error
  CSVParsing(csv::Error),
  /// JSON serialization error (internal conversion)
  JSONSerialization(serde_json::Error),
  /// CDDL parsing error
  CDDLParsing(String),
  /// JSON validation error (delegated)
  JSONValidation(json::Error),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Error::Validation(errors) => {
        let mut error_str = String::new();
        for e in errors.iter() {
          let _ = writeln!(error_str, "{}", e);
        }
        write!(f, "{}", error_str)
      }
      Error::CSVParsing(error) => write!(f, "error parsing CSV: {}", error),
      Error::JSONSerialization(error) => {
        write!(f, "error converting CSV to JSON representation: {}", error)
      }
      Error::CDDLParsing(error) => write!(f, "error parsing CDDL: {}", error),
      Error::JSONValidation(error) => write!(f, "{}", error),
    }
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Error::CSVParsing(error) => Some(error),
      Error::JSONSerialization(error) => Some(error),
      _ => None,
    }
  }
}

/// Parse CSV data into the generic CDDL data model.
///
/// Returns a `serde_json::Value::Array` where each element is a
/// `serde_json::Value::Array` representing a record (row). The header row, if
/// present, is included as the first element. Each field is represented as
/// a JSON value: integers become `Number`, floats become `Number`, empty
/// strings become `String("")`, and everything else stays as `String`.
///
/// The `has_header` parameter controls whether the first row should be treated
/// as a header. If `true`, the first row remains as text strings (no numeric
/// coercion). If `false` or `None`, all rows are coerced.
pub fn parse_csv_to_json(
  csv_data: &str,
  has_header: Option<bool>,
) -> std::result::Result<serde_json::Value, csv::Error> {
  let has_header = has_header.unwrap_or(false);
  let mut reader = csv::ReaderBuilder::new()
    .has_headers(false)
    .flexible(true)
    .from_reader(csv_data.as_bytes());

  let mut rows: Vec<serde_json::Value> = Vec::new();

  for (row_idx, result) in reader.records().enumerate() {
    let record = result?;
    let fields: Vec<serde_json::Value> = record
      .iter()
      .map(|field| {
        // Header row: keep as text
        if has_header && row_idx == 0 {
          return serde_json::Value::String(field.to_string());
        }
        coerce_field(field)
      })
      .collect();

    rows.push(serde_json::Value::Array(fields));
  }

  Ok(serde_json::Value::Array(rows))
}

/// Coerce a CSV field text value to an appropriate JSON value.
///
/// Per draft-bormann-cbor-cddl-csv-07: "As a preferred choice, the JSON
/// representation of the data model item, if it exists, MAY be chosen."
///
/// - Empty string `""` stays as `String("")`
/// - Unsigned integers (all digits) become `Number` (u64)
/// - Negative integers (`-` followed by digits) become `Number` (i64)
/// - Floating-point numbers become `Number` (f64)
/// - Everything else stays as `String`
fn coerce_field(field: &str) -> serde_json::Value {
  if field.is_empty() {
    return serde_json::Value::String(String::new());
  }

  // Try unsigned integer first
  if let Ok(n) = field.parse::<u64>() {
    return serde_json::json!(n);
  }

  // Try signed integer
  if let Ok(n) = field.parse::<i64>() {
    return serde_json::json!(n);
  }

  // Try float
  if let Ok(n) = field.parse::<f64>() {
    if n.is_finite() {
      return serde_json::json!(n);
    }
  }

  serde_json::Value::String(field.to_string())
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "additional-controls")]
/// Validate CSV string against a given CDDL document string.
///
/// The CSV data is parsed according to RFC 4180 and mapped to the CDDL generic
/// data model as described in draft-bormann-cbor-cddl-csv-07. The first rule in
/// the CDDL document is used as the root validation type.
///
/// `has_header` indicates whether the first row of the CSV is a header row.
/// If `None`, defaults to `false`.
pub fn validate_csv_from_str(
  cddl: &str,
  csv_data: &str,
  has_header: Option<bool>,
  enabled_features: Option<&[&str]>,
) -> Result {
  let cddl_ast = cddl_from_str(cddl, true).map_err(Error::CDDLParsing)?;
  let json_value = parse_csv_to_json(csv_data, has_header).map_err(Error::CSVParsing)?;

  let mut jv = json::JSONValidator::new(&cddl_ast, json_value, enabled_features);

  use super::Validator;
  jv.validate().map_err(Error::JSONValidation)
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(not(feature = "additional-controls"))]
/// Validate CSV string against a given CDDL document string.
///
/// The CSV data is parsed according to RFC 4180 and mapped to the CDDL generic
/// data model as described in draft-bormann-cbor-cddl-csv-07. The first rule in
/// the CDDL document is used as the root validation type.
///
/// `has_header` indicates whether the first row of the CSV is a header row.
/// If `None`, defaults to `false`.
pub fn validate_csv_from_str(cddl: &str, csv_data: &str, has_header: Option<bool>) -> Result {
  let cddl_ast = cddl_from_str(cddl, true).map_err(Error::CDDLParsing)?;
  let json_value = parse_csv_to_json(csv_data, has_header).map_err(Error::CSVParsing)?;

  let mut jv = json::JSONValidator::new(&cddl_ast, json_value);

  use super::Validator;
  jv.validate().map_err(Error::JSONValidation)
}

#[cfg(target_arch = "wasm32")]
#[cfg(feature = "additional-controls")]
#[wasm_bindgen]
/// Validate CSV string from a given CDDL document string
pub fn validate_csv_from_str(
  cddl: &str,
  csv_data: &str,
  has_header: Option<bool>,
  enabled_features: Option<Box<[JsValue]>>,
) -> std::result::Result<JsValue, JsValue> {
  let c = pest_bridge::cddl_from_pest_str(cddl).map_err(|e| {
    if let parser::Error::PARSER {
      #[cfg(feature = "ast-span")]
      position,
      msg,
    } = &e
    {
      let errors = vec![ParserError {
        #[cfg(feature = "ast-span")]
        position: *position,
        msg: msg.clone(),
      }];
      serde_wasm_bindgen::to_value(&errors).unwrap_or_else(|e| JsValue::from(e.to_string()))
    } else {
      JsValue::from(e.to_string())
    }
  })?;

  let json_value =
    parse_csv_to_json(csv_data, has_header).map_err(|e| JsValue::from(e.to_string()))?;

  let mut jv = json::JSONValidator::new(&c, json_value, enabled_features);

  use super::Validator;
  jv.validate()
    .map_err(|e| JsValue::from(e.to_string()))
    .map(|_| JsValue::default())
}

#[cfg(target_arch = "wasm32")]
#[cfg(not(feature = "additional-controls"))]
#[wasm_bindgen]
/// Validate CSV string from a given CDDL document string
pub fn validate_csv_from_str(
  cddl: &str,
  csv_data: &str,
  has_header: Option<bool>,
) -> std::result::Result<JsValue, JsValue> {
  let c = pest_bridge::cddl_from_pest_str(cddl).map_err(|e| {
    if let parser::Error::PARSER {
      #[cfg(feature = "ast-span")]
      position,
      msg,
    } = &e
    {
      let errors = vec![ParserError {
        #[cfg(feature = "ast-span")]
        position: *position,
        msg: msg.clone(),
      }];
      serde_wasm_bindgen::to_value(&errors).unwrap_or_else(|e| JsValue::from(e.to_string()))
    } else {
      JsValue::from(e.to_string())
    }
  })?;

  let json_value =
    parse_csv_to_json(csv_data, has_header).map_err(|e| JsValue::from(e.to_string()))?;

  let mut jv = json::JSONValidator::new(&c, json_value);

  use super::Validator;
  jv.validate()
    .map_err(|e| JsValue::from(e.to_string()))
    .map(|_| JsValue::default())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_coerce_field_empty() {
    assert_eq!(coerce_field(""), serde_json::Value::String(String::new()));
  }

  #[test]
  fn test_coerce_field_uint() {
    assert_eq!(coerce_field("42"), serde_json::json!(42));
    assert_eq!(coerce_field("0"), serde_json::json!(0));
    assert_eq!(coerce_field("1700"), serde_json::json!(1700));
  }

  #[test]
  fn test_coerce_field_negative_int() {
    assert_eq!(coerce_field("-1"), serde_json::json!(-1));
    assert_eq!(coerce_field("-100"), serde_json::json!(-100));
  }

  #[test]
  fn test_coerce_field_float() {
    assert_eq!(coerce_field("3.14"), serde_json::json!(3.14));
    assert_eq!(coerce_field("-2.5"), serde_json::json!(-2.5));
  }

  #[test]
  fn test_coerce_field_text() {
    assert_eq!(
      coerce_field("hello"),
      serde_json::Value::String("hello".to_string())
    );
    assert_eq!(
      coerce_field("ietf-system"),
      serde_json::Value::String("ietf-system".to_string())
    );
  }

  #[test]
  fn test_parse_csv_no_header() {
    let csv = "a,b,c\n1,2,3\n";
    let result = parse_csv_to_json(csv, None).unwrap();
    let expected = serde_json::json!([["a", "b", "c"], [1, 2, 3]]);
    assert_eq!(result, expected);
  }

  #[test]
  fn test_parse_csv_with_header() {
    let csv = "name,value\nhello,42\n";
    let result = parse_csv_to_json(csv, Some(true)).unwrap();
    let expected = serde_json::json!([["name", "value"], ["hello", 42]]);
    assert_eq!(result, expected);
  }

  #[test]
  fn test_parse_csv_empty_fields() {
    let csv = "a,,c\n";
    let result = parse_csv_to_json(csv, None).unwrap();
    let expected = serde_json::json!([["a", "", "c"]]);
    assert_eq!(result, expected);
  }
}
