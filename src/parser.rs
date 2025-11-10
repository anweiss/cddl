//! Parser for CDDL using nom combinator framework
//!
//! This module provides the main parsing interface for CDDL, built on top of
//! the nom parser combinator library. It maintains API compatibility with the
//! previous handwritten parser.

use crate::{
  ast::CDDL,
  error::{ErrorMsg, Position},
  nom_bridge::cddl_from_nom_str,
};

use std::result;

#[cfg(target_arch = "wasm32")]
use serde::Serialize;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

use displaydoc::Display;

/// Alias for `Result` with an error of type `cddl::parser::Error`
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
  #[displaydoc("incremental parsing error")]
  /// Incremental parsing error
  INCREMENTAL,
  #[displaydoc("defer parsing error")]
  /// Group parsing error
  GROUP,
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

/// Parse CDDL from a string with optional error printing to stderr
///
/// # Arguments
///
/// * `input` - A string slice containing the CDDL text
/// * `print_stderr` - Whether to print errors to stderr (used for compatibility, currently ignored)
///
/// # Example
///
/// ```
/// use cddl::parser::cddl_from_str;
///
/// let input = r#"myrule = int"#;
/// let result = cddl_from_str(input, true);
/// assert!(result.is_ok());
/// ```
#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "std")]
pub fn cddl_from_str(input: &str, _print_stderr: bool) -> std::result::Result<CDDL<'_>, String> {
  cddl_from_nom_str(input).map_err(|e| e.to_string())
}

/// Parse CDDL from a string (no_std version)
///
/// # Arguments
///
/// * `input` - A string slice containing the CDDL text
///
/// # Example
///
/// ```
/// use cddl::parser::cddl_from_str;
///
/// let input = r#"myrule = int"#;
/// let result = cddl_from_str(input);
/// assert!(result.is_ok());
/// ```
#[cfg(not(target_arch = "wasm32"))]
#[cfg(not(feature = "std"))]
pub fn cddl_from_str(input: &str) -> std::result::Result<CDDL<'_>, String> {
  cddl_from_nom_str(input).map_err(|e| e.to_string())
}

/// Returns a `ast::CDDL` wrapped in `JsValue` from a `&str` (WASM version)
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

  match cddl_from_nom_str(input) {
    Ok(c) => serde_wasm_bindgen::to_value(&c).map_err(|e| JsValue::from(e.to_string())),
    Err(Error::PARSER { position, msg }) => {
      let error = ParserError { position, msg };
      Err(serde_wasm_bindgen::to_value(&error).map_err(|e| JsValue::from(e.to_string()))?)
    }
    Err(e) => Err(JsValue::from(e.to_string())),
  }
}

/// Identify root type name from CDDL input string
#[cfg(feature = "std")]
#[cfg(not(target_arch = "wasm32"))]
pub fn root_type_name_from_cddl_str(input: &str) -> std::result::Result<String, String> {
  let cddl = cddl_from_str(input, false)?;

  if let Some(rule) = cddl.rules.first() {
    match rule {
      crate::ast::Rule::Type { rule, .. } => Ok(rule.name.to_string()),
      crate::ast::Rule::Group { rule, .. } => Ok(rule.name.to_string()),
    }
  } else {
    Err("no rules defined".into())
  }
}
