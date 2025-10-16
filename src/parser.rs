//! CDDL parser using Pest
//!
//! This module provides the main parsing interface for CDDL using the Pest parsing library.
//! The actual parsing is implemented in the `pest_bridge` module, which converts Pest's
//! parse tree into our AST.

use super::{
  ast::*,
  error::ErrorMsg,
  lexer::{self, Position},
};

use std::{fmt, result};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

/// Alias for `Result` with an error of type `cddl::ParserError`
pub type Result<T> = result::Result<T, Error>;

/// Parsing error types
#[derive(Debug)]
pub enum Error {
  /// Parsing errors
  CDDL(String),
  /// Parsing error occurred
  PARSER {
    /// Error position
    #[cfg(feature = "ast-span")]
    position: Position,
    /// Error message
    msg: ErrorMsg,
  },
  /// Lexing error
  LEXER(lexer::Error),
  /// Regex error
  #[cfg(feature = "std")]
  REGEX(regex::Error),
  /// Incremental parsing error
  INCREMENTAL,
  /// Incremental parsing error
  GROUP,
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl From<lexer::Error> for Error {
  fn from(e: lexer::Error) -> Self {
    Error::LEXER(e)
  }
}

#[cfg(feature = "std")]
impl From<regex::Error> for Error {
  fn from(e: regex::Error) -> Self {
    Error::REGEX(e)
  }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Error::CDDL(s) => write!(f, "{}", s),
      Error::PARSER { msg, .. } => write!(f, "parsing error: {}", msg.short),
      Error::LEXER(e) => write!(f, "{}", e),
      #[cfg(feature = "std")]
      Error::REGEX(e) => write!(f, "regex parsing error: {}", e),
      Error::INCREMENTAL => write!(f, "incremental parsing error"),
      Error::GROUP => write!(f, "defer parsing error"),
    }
  }
}

/// Returns a `ast::CDDL` from a `&str`
///
/// # Arguments
///
/// * `input` - A string slice with the CDDL text input
/// * `print_stderr` - When true, print any errors to stderr
///
/// # Example
///
/// ```
/// use cddl::parser::cddl_from_str;
///
/// let input = r#"myrule = int"#;
/// let _ = cddl_from_str(input, true);
/// ```
#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "std")]
pub fn cddl_from_str(input: &str, print_stderr: bool) -> std::result::Result<CDDL<'_>, String> {
  #[cfg(feature = "std")]
  use crate::pest_bridge::cddl_from_pest_str;
  
  match cddl_from_pest_str(input) {
    Ok(c) => Ok(c),
    Err(e) => {
      if print_stderr {
        eprintln!("{}", e);
      }
      Err(e.to_string())
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

  /// Parses CDDL from a byte slice
  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(not(feature = "std"))]
  pub fn from_slice(input: &[u8]) -> std::result::Result<CDDL<'_>, String> {
    let str_input = std::str::from_utf8(input).map_err(|e| e.to_string())?;
    cddl_from_str(str_input)
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
  #[cfg(feature = "std")]
  use crate::pest_bridge::cddl_from_pest_str;
  
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
  #[cfg(feature = "std")]
  use crate::pest_bridge::cddl_from_pest_str;
  
  match cddl_from_pest_str(input) {
    Ok(c) => serde_wasm_bindgen::to_value(&c).map_err(|e| JsValue::from(e.to_string())),
    Err(e) => Err(JsValue::from(e.to_string())),
  }
}

/// Format CDDL from string
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn format_cddl_from_str(input: &str) -> result::Result<String, JsValue> {
  #[cfg(feature = "std")]
  use crate::pest_bridge::cddl_from_pest_str;
  
  match cddl_from_pest_str(input) {
    Ok(c) => Ok(format!("{}", c)),
    Err(e) => Err(JsValue::from(e.to_string())),
  }
}

