//! Lexer compatibility module
//!
//! This module provides backward compatibility exports.
//! The actual lexing is now handled by the nom parser.

// Re-export Position from error module for backward compatibility
pub use crate::error::Position;
