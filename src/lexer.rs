/// Lexer types preserved for AST compatibility

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
