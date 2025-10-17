//! Lexer types for CDDL
//!
//! This module provides the Position type used for error reporting.
//! The actual lexing is handled by the Pest parser.

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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_position_default() {
    let pos = Position::default();
    assert_eq!(pos.line, 1);
    assert_eq!(pos.column, 1);
    assert_eq!(pos.range, (0, 0));
    assert_eq!(pos.index, 0);
  }
}
