/// CBOR validation implementation
pub mod cbor;
/// JSON validation implementation
pub mod json;

use crate::{ast::*, parser::ParserError};
use std::{fmt, result};

/// Alias for `Result` with an error of type `validator::ValidationError`
pub type Result = result::Result<(), Error>;

/// Validation error types
#[derive(Debug)]
pub enum Error {
  /// CDDL syntax error, specific to the target data structure being validated
  Syntax(String),
  /// Error validating specific target data structure
  Target(Box<std::error::Error>),
  /// Error compiling CDDL and/or target data structure
  Compilation(CompilationError),
  /// Occurrence error
  Occurrence(String),
  /// Aggregate errors
  MultiError(Vec<Error>),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Error::Syntax(se) => write!(f, "CDDL syntax error: {}", se),
      Error::Target(te) => write!(f, "{}", te),
      Error::Compilation(ce) => write!(f, "error on compilation: {}", ce),
      Error::Occurrence(oe) => write!(f, "occurrence error: {}", oe),
      Error::MultiError(me) => {
        let mut errors = String::new();

        for e in me.iter() {
          match e {
            // Temporary work around for nested MultiError's
            Error::MultiError(_) => errors.push_str(&format!("{}", e)),
            _ => errors.push_str(&format!("{}\n\n", e)),
          }
        }

        write!(f, "{}", errors)
      }
    }
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Error::Compilation(ce) => Some(ce),
      Error::Target(te) => Some(te.as_ref()),
      _ => None,
    }
  }
}

#[derive(Debug)]
pub enum CompilationError {
  CDDL(ParserError),
  Target(Box<std::error::Error>),
}

impl fmt::Display for CompilationError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      CompilationError::CDDL(ce) => write!(f, "{}", ce),
      CompilationError::Target(te) => write!(f, "{}", te),
    }
  }
}

impl std::error::Error for CompilationError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      CompilationError::CDDL(ce) => Some(ce),
      CompilationError::Target(te) => Some(te.as_ref()),
    }
  }
}

pub trait Validator<T> {
  fn validate(&self, value: &T) -> Result;

  fn validate_rule_for_ident(
    &self,
    ident: &Identifier,
    is_enumeration: bool,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    occur: Option<&Occur>,
    value: &T,
  ) -> Result;

  fn validate_type_rule(
    &self,
    tr: &TypeRule,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    occur: Option<&Occur>,
    value: &T,
  ) -> Result;

  fn validate_group_rule(
    &self,
    gr: &GroupRule,
    is_enumeration: bool,
    occur: Option<&Occur>,
    value: &T,
  ) -> Result;

  fn validate_type(
    &self,
    t: &Type,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    occur: Option<&Occur>,
    value: &T,
  ) -> Result;

  fn validate_type1(
    &self,
    t1: &Type1,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    occur: Option<&Occur>,
    value: &T,
  ) -> Result;

  fn validate_type2(
    &self,
    t2: &Type2,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    occur: Option<&Occur>,
    value: &T,
  ) -> Result;

  fn validate_group(&self, g: &Group, occur: Option<&Occur>, value: &T) -> Result;

  fn validate_group_to_choice_enum(&self, g: &Group, occur: Option<&Occur>, value: &T) -> Result;

  fn validate_group_choice(&self, gc: &GroupChoice, occur: Option<&Occur>, value: &T) -> Result;

  fn validate_group_entry(
    &self,
    ge: &GroupEntry,
    is_enumeration: bool,
    occur: Option<&Occur>,
    value: &T,
  ) -> Result;

  fn validate_array_occurrence(&self, occur: &Occur, group: &str, values: &[T]) -> Result;

  fn expect_bool(&self, ident: &str, value: &T) -> Result;

  fn validate_numeric_data_type(
    &self,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    ident: &str,
    value: &T,
  ) -> Result;
}
