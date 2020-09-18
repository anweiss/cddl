use std::fmt;

use serde_cbor::Value;

use crate::{ast::*, token};

pub type Result = std::result::Result<(), Error>;

#[derive(Debug)]
pub enum Error {
  Validation(Vec<ValidationError>),
  JSONParsing(serde_json::Error),
  CDDLParsing(String),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Error::Validation(errors) => write!(f, "errors: {:?}", errors),
      Error::JSONParsing(error) => write!(f, "error parsing JSON: {}", error),
      Error::CDDLParsing(error) => write!(f, "error parsing CDDL: {}", error),
    }
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Error::JSONParsing(error) => Some(error),
      _ => None,
    }
  }
}

#[derive(Clone, Debug)]
pub struct ValidationError {
  pub reason: String,
  pub cddl_location: String,
  pub json_location: String,
  pub is_multi_type_choice: bool,
  pub is_group_to_choice_enum: bool,
}

impl fmt::Display for ValidationError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut error_str = String::from("error validating");
    if self.is_multi_type_choice {
      error_str.push_str(" type choice");
    }
    if self.is_group_to_choice_enum {
      error_str.push_str(" type choice in group to choice enumeration");
    }

    write!(
      f,
      "{} at cddl location {} and JSON location {}: {}",
      error_str, self.cddl_location, self.json_location, self.reason
    )
  }
}

impl std::error::Error for ValidationError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

impl ValidationError {
  fn from_validator(cv: &CBORValidator, reason: String) -> Self {
    ValidationError {
      cddl_location: cv.cddl_location.clone(),
      json_location: cv.json_location.clone(),
      reason,
      is_multi_type_choice: cv.is_multi_type_choice,
      is_group_to_choice_enum: cv.is_group_to_choice_enum,
    }
  }
}

pub struct CBORValidator<'a> {
  cddl: &'a CDDL<'a>,
  json: Value,
  errors: Vec<ValidationError>,
  cddl_location: String,
  json_location: String,
  // Occurrence indicator detected in current state of AST evaluation
  occurence: Option<Occur>,
  // Current group entry index detected in current state of AST evaluation
  group_entry_idx: Option<usize>,
  // JSON object value hoisted from previous state of AST evaluation
  object_value: Option<Value>,
  // Is member key detected in current state of AST evaluation
  is_member_key: bool,
  // Is a cut detected in current state of AST evaluation
  is_cut_present: bool,
  // Str value of cut detected in current state of AST evaluation
  cut_value: Option<&'a str>,
  // Validate the generic rule given by str ident in current state of AST
  // evaluation
  eval_generic_rule: Option<&'a str>,
  // Aggregation of generic rules
  generic_rules: Vec<GenericRule<'a>>,
  // Control operator token detected in current state of AST evaluation
  ctrl: Option<token::Token<'a>>,
  // Is a group to choice enumeration detected in current state of AST
  // evaluation
  is_group_to_choice_enum: bool,
  // Are 2 or more type choices detected in current state of AST evaluation
  is_multi_type_choice: bool,
}

#[derive(Clone, Debug)]
struct GenericRule<'a> {
  name: &'a str,
  params: Vec<&'a str>,
  args: Vec<Type1<'a>>,
}
