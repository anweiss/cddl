//! # CDDL Data Generator
//!
//! This module provides functionality to generate sample data from CDDL schemas.
//! It supports generating both JSON and CBOR data that conforms to a given CDDL definition.
//!
//! ## Example
//!
//! ```rust
//! use cddl::generator::{generate_json_from_cddl, GeneratorConfig};
//!
//! let cddl = r#"
//! person = {
//!   name: tstr,
//!   age: uint,
//!   email: tstr,
//! }
//! "#;
//!
//! let config = GeneratorConfig::default();
//! let json = generate_json_from_cddl(cddl, &config).unwrap();
//! println!("{}", json);
//! ```

#[cfg(feature = "json")]
pub mod json;

#[cfg(feature = "json")]
pub use json::*;

use std::collections::HashMap;

/// Configuration options for data generation
#[derive(Debug, Clone)]
pub struct GeneratorConfig {
  /// Seed for random number generation (for reproducible output)
  pub seed: Option<u64>,
  /// Maximum depth for recursive types (to prevent infinite loops)
  pub max_depth: usize,
  /// Maximum number of items to generate for arrays with `*` occurrence
  pub max_array_items: usize,
  /// Minimum number of items to generate for arrays with `*` occurrence
  pub min_array_items: usize,
  /// Maximum length for generated strings
  pub max_string_length: usize,
  /// Minimum length for generated strings
  pub min_string_length: usize,
  /// Whether to generate optional fields
  pub include_optional: bool,
  /// Custom values for specific rule names
  pub custom_values: HashMap<String, serde_json::Value>,
  /// Whether to use realistic-looking data (names, emails, etc.)
  pub realistic_data: bool,
}

impl Default for GeneratorConfig {
  fn default() -> Self {
    Self {
      seed: None,
      max_depth: 10,
      max_array_items: 5,
      min_array_items: 1,
      max_string_length: 20,
      min_string_length: 3,
      include_optional: true,
      custom_values: HashMap::new(),
      realistic_data: true,
    }
  }
}

impl GeneratorConfig {
  /// Create a new configuration with a specific seed for reproducible output
  pub fn with_seed(seed: u64) -> Self {
    Self {
      seed: Some(seed),
      ..Default::default()
    }
  }

  /// Create a minimal configuration that generates the simplest valid data
  pub fn minimal() -> Self {
    Self {
      max_array_items: 1,
      min_array_items: 0,
      max_string_length: 5,
      min_string_length: 1,
      include_optional: false,
      realistic_data: false,
      ..Default::default()
    }
  }
}

/// Errors that can occur during data generation
#[derive(Debug)]
pub enum GeneratorError {
  /// Error parsing the CDDL schema
  ParseError(String),
  /// The schema contains unsupported constructs
  UnsupportedConstruct(String),
  /// Maximum recursion depth exceeded
  MaxDepthExceeded,
  /// No valid choice could be selected
  NoValidChoice(String),
  /// Custom error
  Custom(String),
}

impl std::fmt::Display for GeneratorError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      GeneratorError::ParseError(msg) => write!(f, "CDDL parse error: {}", msg),
      GeneratorError::UnsupportedConstruct(msg) => {
        write!(f, "Unsupported CDDL construct: {}", msg)
      }
      GeneratorError::MaxDepthExceeded => write!(f, "Maximum recursion depth exceeded"),
      GeneratorError::NoValidChoice(msg) => write!(f, "No valid choice available: {}", msg),
      GeneratorError::Custom(msg) => write!(f, "{}", msg),
    }
  }
}

impl std::error::Error for GeneratorError {}
