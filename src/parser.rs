use super::{ast::*, error::ErrorMsg, lexer::Position, pest_bridge};

use std::result;

use codespan_reporting::{
  diagnostic::{Diagnostic, Label},
  files::SimpleFiles,
  term,
  term::termcolor::{ColorChoice, StandardStream},
};
use displaydoc::Display;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
use serde::Serialize;

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
}

impl std::error::Error for Error {}

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
#[cfg(not(target_arch = "wasm32"))]
pub fn cddl_from_str(input: &str, print_stderr: bool) -> std::result::Result<CDDL<'_>, String> {
  pest_bridge::cddl_from_pest_str(input).map_err(|e| {
    if print_stderr {
      report_pest_error(&e, input);
    }
    e.to_string()
  })
}

/// Print a pest parser error to stderr with codespan diagnostics
fn report_pest_error(error: &Error, input: &str) {
  if let Error::PARSER {
    #[cfg(feature = "ast-span")]
    position,
    msg,
  } = error
  {
    let mut files = SimpleFiles::new();
    let file_id = files.add("input", input);

    let label_message = msg.to_string();

    let label = {
      #[cfg(feature = "ast-span")]
      {
        Label::primary(file_id, position.range.0..position.range.1).with_message(label_message)
      }
      #[cfg(not(feature = "ast-span"))]
      {
        Label::primary(file_id, 0..0).with_message(label_message)
      }
    };

    let mut diagnostic = Diagnostic::error()
      .with_message("parser errors")
      .with_labels(vec![label]);

    if let Some(ref extended) = msg.extended {
      diagnostic = diagnostic.with_notes(vec![extended.clone()]);
    }

    let config = term::Config::default();
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let _ = term::emit(&mut writer.lock(), &config, &files, &diagnostic);
  }
}

impl CDDL<'_> {
  /// Parses CDDL from a byte slice
  #[cfg(not(target_arch = "wasm32"))]
  pub fn from_slice(input: &[u8]) -> std::result::Result<CDDL<'_>, String> {
    let str_input = std::str::from_utf8(input).map_err(|e| e.to_string())?;
    cddl_from_str(str_input, false)
  }
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

  match pest_bridge::cddl_from_pest_str(input) {
    Ok(c) => serde_wasm_bindgen::to_value(&c).map_err(|e| JsValue::from(e.to_string())),
    Err(Error::PARSER {
      #[cfg(feature = "ast-span")]
      position,
      msg,
    }) => {
      let errors = vec![ParserError {
        #[cfg(feature = "ast-span")]
        position,
        msg,
      }];
      Err(serde_wasm_bindgen::to_value(&errors).map_err(|e| JsValue::from(e.to_string()))?)
    }
    Err(e) => Err(JsValue::from(e.to_string())),
  }
}

/// Formats CDDL from input string
#[cfg(feature = "lsp")]
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn format_cddl_from_str(input: &str) -> result::Result<String, JsValue> {
  #[derive(Serialize)]
  struct ParserError {
    position: Position,
    msg: ErrorMsg,
  }

  match pest_bridge::cddl_from_pest_str(input) {
    Ok(c) => Ok(c.to_string()),
    Err(Error::PARSER {
      #[cfg(feature = "ast-span")]
      position,
      msg,
    }) => {
      let errors = vec![ParserError {
        #[cfg(feature = "ast-span")]
        position,
        msg,
      }];
      Err(serde_wasm_bindgen::to_value(&errors).map_err(|e| JsValue::from(e.to_string()))?)
    }
    Err(e) => Err(JsValue::from(e.to_string())),
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

#[cfg(test)]
#[cfg(feature = "std")]
#[cfg(not(target_arch = "wasm32"))]
mod tests {
  use super::*;

  #[test]
  fn test_cddl_from_str_basic() {
    let input = "myrule = int\n";
    let result = cddl_from_str(input, false);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_multiple_rules_with_reference_to_parenthesized_type() {
    let input = "basic = int\nouter = (basic)\n";
    let result = cddl_from_str(input, false);
    assert!(result.is_ok(), "Parser errors: {:?}", result.err());

    let cddl = result.unwrap();
    assert_eq!(cddl.rules.len(), 2);

    let rule_names: Vec<_> = cddl.rules.iter().map(|r| r.name()).collect();
    assert!(rule_names.contains(&"basic".to_string()));
    assert!(rule_names.contains(&"outer".to_string()));
  }
}
