//! CDDL lexer types
//!
//! This module provides position and error types used by the parser.
//! The actual lexing is now performed by the Pest parser.

use super::error::MsgType;

use std::fmt;

#[cfg(feature = "std")]
use std::string;

#[cfg(not(feature = "std"))]
use alloc::string::{self, String};

#[cfg(target_arch = "wasm32")]
use serde::Serialize;

/// Lexer position
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Copy, Clone)]
pub struct Position {
  /// Line number
  pub line: usize,
  /// Column number
  pub column: usize,
  /// Token begin and end index range
  pub range: (usize, usize),
  /// Lexer index
  pub index: usize,
}

impl Default for Position {
  fn default() -> Self {
    Position {
      line: 1,
      column: 1,
      range: (0, 0),
      index: 0,
    }
  }
}

/// Lexer error
#[derive(Debug)]
pub struct Error {
  /// Error type
  pub error_type: LexerErrorType,
  pub(crate) input: String,
  /// Error position
  pub position: Position,
}

/// Various error types emitted by the lexer
#[derive(Debug)]
pub enum LexerErrorType {
  /// CDDL lexing syntax error
  LEXER(MsgType),
  /// UTF-8 parsing error
  UTF8(string::FromUtf8Error),
  /// Byte string not properly encoded as base 16
  BASE16(String),
  /// Byte string not properly encoded as base 64
  BASE64(String),
  /// Error parsing integer
  PARSEINT(std::num::ParseIntError),
  /// Error parsing float
  PARSEFLOAT(lexical_core::Error),
  /// Error parsing hexfloat
  PARSEHEXF(hexf_parse::ParseHexfError),
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Lexer error at line {}, column {}: {:?}", 
      self.position.line, self.position.column, self.error_type)
  }
}
