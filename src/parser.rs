//! CDDL Parser - Pest-based implementation
//!
//! This module provides the public API for parsing CDDL documents.
//! The implementation uses the Pest parser through the pest_bridge module.

use super::{ast::*, error::ErrorMsg, lexer::Position};

use crate::pest_bridge::cddl_from_pest_str;

use std::result;

use displaydoc::Display;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
use serde::Serialize;

/// Alias for `Result` with an error of type `cddl::ParserError`
pub type Result<T> = result::Result<T, Error>;

/// Parsing error types
#[derive(Debug, Display)]
pub enum Error {
  /// Parsing errors
  #[displaydoc("{0}")]
  CDDL(String),
  #[cfg_attr(
    feature = "ast-span",
    displaydoc("parsing error: position {position:?}, msg: {msg}")
  )]
  #[cfg_attr(not(feature = "ast-span"), displaydoc("parsing error: msg: {msg}"))]
  /// Parsing error occurred
  PARSER {
    /// Error position
    #[cfg(feature = "ast-span")]
    position: Position,
    /// Error message
    msg: ErrorMsg,
  },
  /// Regex error
  #[displaydoc("regex parsing error: {0}")]
  REGEX(regex::Error),
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

/// Returns a `ast::CDDL` from a `&str`
///
/// # Arguments
///
/// * `input` - A string slice with the CDDL text input
/// * `print_stderr` - Whether to print errors to stderr (used for compatibility)
///
/// # Example
///
/// ```
/// use cddl::cddl_from_str;
///
/// let input = r#"myrule = int"#;
/// let _ = cddl_from_str(input, true);
/// ```
#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "std")]
pub fn cddl_from_str(input: &str, print_stderr: bool) -> std::result::Result<CDDL<'_>, String> {
  match cddl_from_pest_str(input) {
    Ok(cddl) => Ok(cddl),
    Err(e) => {
      if print_stderr {
        eprintln!("{}", e);
      }
      Err(e.to_string())
    }
  }
}

/// Returns a `ast::CDDL` from a `&str`
///
/// # Arguments
///
/// * `input` - A string slice with the CDDL text input
///
/// # Example
///
/// ```
/// use cddl::cddl_from_str;
///
/// let input = r#"myrule = int"#;
///
/// let _ = cddl_from_str(input);
/// ```
#[cfg(not(target_arch = "wasm32"))]
#[cfg(not(feature = "std"))]
pub fn cddl_from_str(input: &str) -> std::result::Result<CDDL<'_>, String> {
  cddl_from_pest_str(input).map_err(|e| e.to_string())
}

/// Returns a `ast::CDDL` wrapped in `JsValue` from a `&str`
///
/// # Arguments
///
/// * `input` - A string slice with the CDDL text input
///
/// # Example
///
/// ```typescript
/// import * as wasm from 'cddl';
///
/// let cddl: any;
/// try {
///   cddl = wasm.cddl_from_str(text);
/// } catch (e) {
///   console.error(e);
/// }
/// ```
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn cddl_from_str(input: &str) -> result::Result<JsValue, JsValue> {
  #[derive(Serialize)]
  struct ParserError {
    position: Position,
    msg: ErrorMsg,
  }

  match cddl_from_pest_str(input) {
    Ok(c) => serde_wasm_bindgen::to_value(&c).map_err(|e| JsValue::from(e.to_string())),
    Err(e) => {
      if let Error::PARSER { position, msg } = e {
        serde_wasm_bindgen::to_value(&ParserError { position, msg })
          .map(JsValue::from)
          .map(Err)?
      } else {
        Err(JsValue::from(e.to_string()))
      }
    }
  }
}

/// Identify root type name from CDDL input string
#[cfg(feature = "std")]
#[cfg(not(target_arch = "wasm32"))]
pub fn root_type_name_from_cddl_str(input: &str) -> std::result::Result<String, String> {
  let cddl = cddl_from_str(input, false)?;

  for r in cddl.rules.iter() {
    // First type rule is root
    if let Rule::Type { rule, .. } = r {
      if rule.generic_params.is_none() {
        return Ok(rule.name.to_string());
      }
    }
  }

  Err("cddl spec contains no root type".to_string())
}

impl CDDL<'_> {
  /// Parses CDDL from a byte slice
  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(feature = "std")]
  pub fn from_slice(input: &[u8]) -> std::result::Result<CDDL<'_>, String> {
    let str_input = std::str::from_utf8(input).map_err(|e| e.to_string())?;
    cddl_from_str(str_input, false)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_cddl_from_str_basic() {
    let input = "myrule = int\n";
    let result = cddl_from_str(input, false);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_cddl_from_str_struct() {
    let input = "person = { name: tstr, age: uint }\n";
    let result = cddl_from_str(input, false);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_cddl_from_str_multiple_rules() {
    let input = r#"
person = { name: tstr }
address = { street: tstr }
"#;
    let result = cddl_from_str(input, false);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    let cddl = result.unwrap();
    assert_eq!(cddl.rules.len(), 2);
  }

  #[test]
  fn test_root_type_name() {
    let input = r#"
person = { name: tstr }
address = { street: tstr }
"#;
    let result = root_type_name_from_cddl_str(input);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "person");
  }

  #[test]
  fn test_error_handling() {
    let input = "invalid syntax @#$";
    let result = cddl_from_str(input, false);
    assert!(result.is_err());
  }
}
