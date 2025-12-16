#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(feature = "lsp"))]

use super::*;
use crate::{
  ast::*,
  token,
  visitor::{self, *},
};

use core::convert::TryInto;
use std::{
  borrow::Cow,
  collections::HashMap,
  convert::TryFrom,
  fmt::{self, Write},
};

use chrono::{TimeZone, Utc};
use ciborium::value::Value;
use serde_json;

#[cfg(feature = "additional-controls")]
use crate::validator::control::{
  abnf_from_complex_controller, cat_operation, plus_operation, validate_abnf,
};

/// cbor validation Result
pub type Result<T> = std::result::Result<(), Error<T>>;

/// cbor validation error
#[derive(Debug)]
pub enum Error<T: std::fmt::Debug> {
  /// Zero or more validation errors
  Validation(Vec<ValidationError>),
  /// cbor parsing error
  CBORParsing(ciborium::de::Error<T>),
  /// json parsing error. Used only for parsing regex controller strings
  JSONParsing(serde_json::Error),
  /// CDDL parsing error
  CDDLParsing(String),
  /// UTF8 parsing error,
  UTF8Parsing(std::str::Utf8Error),
  /// Base16 decoding error
  Base16Decoding(base16::DecodeError),
  /// Base64 decoding error
  Base64Decoding(data_encoding::DecodeError),
}

impl<T: std::fmt::Debug> fmt::Display for Error<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Error::Validation(errors) => {
        let mut error_str = String::new();
        for e in errors.iter() {
          let _ = writeln!(error_str, "{}", e);
        }
        write!(f, "{}", error_str)
      }
      Error::CBORParsing(error) => write!(f, "error parsing cbor: {}", error),
      Error::JSONParsing(error) => write!(f, "error parsing json string: {}", error),
      Error::CDDLParsing(error) => write!(f, "error parsing CDDL: {}", error),
      Error::UTF8Parsing(error) => write!(f, "error parsing utf8: {}", error),
      Error::Base16Decoding(error) => write!(f, "error decoding base16: {}", error),
      Error::Base64Decoding(error) => write!(f, "error decoding base64: {}", error),
    }
  }
}

impl<T: std::fmt::Debug + 'static> std::error::Error for Error<T> {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Error::CBORParsing(error) => Some(error),
      _ => None,
    }
  }
}

/// cbor validation error
#[derive(Clone, Debug)]
pub struct ValidationError {
  /// Error message
  pub reason: String,
  /// Location in CDDL where error occurred
  pub cddl_location: String,
  /// Location in CBOR where error occurred
  pub cbor_location: String,
  /// Whether or not the error is associated with multiple type choices
  pub is_multi_type_choice: bool,
  /// Whether or not the error is associated with multiple group choices
  pub is_multi_group_choice: bool,
  /// Whether or not the error is associated with a group to choice enumeration
  pub is_group_to_choice_enum: bool,
  /// Error is associated with a type/group name group entry
  pub type_group_name_entry: Option<String>,
}

impl fmt::Display for ValidationError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut error_str = String::from("error validating");
    if self.is_multi_group_choice {
      error_str.push_str(" group choice");
    }
    if self.is_multi_type_choice {
      error_str.push_str(" type choice");
    }
    if self.is_group_to_choice_enum {
      error_str.push_str(" type choice in group to choice enumeration");
    }
    if let Some(entry) = &self.type_group_name_entry {
      let _ = write!(error_str, " group entry associated with rule \"{}\"", entry);
    }

    write!(
      f,
      "{} at cbor location {}: {}",
      error_str, self.cbor_location, self.reason
    )
  }
}

impl std::error::Error for ValidationError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

impl<T: std::fmt::Debug> Error<T> {
  fn from_validator(cv: &CBORValidator, reason: String) -> Self {
    Error::Validation(vec![ValidationError {
      cddl_location: cv.cddl_location.clone(),
      cbor_location: cv.cbor_location.clone(),
      reason,
      is_multi_type_choice: cv.is_multi_type_choice,
      is_group_to_choice_enum: cv.is_group_to_choice_enum,
      type_group_name_entry: cv.type_group_name_entry.map(|e| e.to_string()),
      is_multi_group_choice: cv.is_multi_group_choice,
    }])
  }
}

/// cbor validator type
#[derive(Clone)]
pub struct CBORValidator<'a> {
  cddl: &'a CDDL<'a>,
  cbor: Value,
  errors: Vec<ValidationError>,
  cddl_location: String,
  cbor_location: String,
  // Occurrence indicator detected in current state of AST evaluation
  occurrence: Option<Occur>,
  // Current group entry index detected in current state of AST evaluation
  group_entry_idx: Option<usize>,
  // cbor object value hoisted from previous state of AST evaluation
  object_value: Option<Value>,
  // Is member key detected in current state of AST evaluation
  is_member_key: bool,
  // Is a cut detected in current state of AST evaluation
  is_cut_present: bool,
  // Str value of cut detected in current state of AST evaluation
  cut_value: Option<Type1<'a>>,
  // Validate the generic rule given by str ident in current state of AST
  // evaluation
  eval_generic_rule: Option<&'a str>,
  // Aggregation of generic rules
  generic_rules: Vec<GenericRule<'a>>,
  // Control operator token detected in current state of AST evaluation
  ctrl: Option<token::ControlOperator>,
  // Is a group to choice enumeration detected in current state of AST
  // evaluation
  is_group_to_choice_enum: bool,
  // Are 2 or more type choices detected in current state of AST evaluation
  is_multi_type_choice: bool,
  // Are 2 or more group choices detected in current state of AST evaluation
  is_multi_group_choice: bool,
  // Type/group name entry detected in current state of AST evaluation. Used
  // only for providing more verbose error messages
  type_group_name_entry: Option<&'a str>,
  // Whether or not to advance to the next group entry if member key validation
  // fails as detected during the current state of AST evaluation
  advance_to_next_entry: bool,
  // Is validation checking for map quality
  is_ctrl_map_equality: bool,
  entry_counts: Option<Vec<EntryCount>>,
  // Collect map entry keys that have already been validated
  validated_keys: Option<Vec<Value>>,
  // Collect map entry values that have yet to be validated
  values_to_validate: Option<Vec<Value>>,
  // Whether or not the validator is validating a map entry value
  validating_value: bool,
  // Collect valid array indices when entries are type choices
  valid_array_items: Option<Vec<usize>>,
  // Collect invalid array item errors where the key is the index of the invalid
  // array item
  array_errors: Option<HashMap<usize, Vec<ValidationError>>>,
  is_colon_shortcut_present: bool,
  is_root: bool,
  is_multi_type_choice_type_rule_validating_array: bool,
  // Track visited rules to prevent infinite recursion
  visited_rules: std::collections::HashSet<String>,
  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(feature = "additional-controls")]
  enabled_features: Option<&'a [&'a str]>,
  #[cfg(target_arch = "wasm32")]
  #[cfg(feature = "additional-controls")]
  enabled_features: Option<Box<[JsValue]>>,
  #[cfg(feature = "additional-controls")]
  has_feature_errors: bool,
  #[cfg(feature = "additional-controls")]
  disabled_features: Option<Vec<String>>,
  range_upper: Option<usize>,
}

#[derive(Clone, Debug)]
struct GenericRule<'a> {
  name: &'a str,
  params: Vec<&'a str>,
  args: Vec<Type1<'a>>,
}

impl<'a> CBORValidator<'a> {
  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(feature = "additional-controls")]
  /// New cborValidation from CDDL AST and cbor value
  pub fn new(cddl: &'a CDDL<'a>, cbor: Value, enabled_features: Option<&'a [&'a str]>) -> Self {
    CBORValidator {
      cddl,
      cbor,
      errors: Vec::default(),
      cddl_location: String::new(),
      cbor_location: String::new(),
      occurrence: None,
      group_entry_idx: None,
      object_value: None,
      is_member_key: false,
      is_cut_present: false,
      cut_value: None,
      eval_generic_rule: None,
      generic_rules: Vec::new(),
      ctrl: None,
      is_group_to_choice_enum: false,
      is_multi_type_choice: false,
      is_multi_group_choice: false,
      type_group_name_entry: None,
      advance_to_next_entry: false,
      is_ctrl_map_equality: false,
      entry_counts: None,
      validated_keys: None,
      values_to_validate: None,
      validating_value: false,
      valid_array_items: None,
      array_errors: None,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      visited_rules: std::collections::HashSet::new(),
      enabled_features,
      has_feature_errors: false,
      disabled_features: None,
      range_upper: None,
    }
  }

  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(not(feature = "additional-controls"))]
  /// New cborValidation from CDDL AST and cbor value
  pub fn new(cddl: &'a CDDL<'a>, cbor: Value) -> Self {
    CBORValidator {
      cddl,
      cbor,
      errors: Vec::default(),
      cddl_location: String::new(),
      cbor_location: String::new(),
      occurrence: None,
      group_entry_idx: None,
      object_value: None,
      is_member_key: false,
      is_cut_present: false,
      cut_value: None,
      eval_generic_rule: None,
      generic_rules: Vec::new(),
      ctrl: None,
      is_group_to_choice_enum: false,
      is_multi_type_choice: false,
      is_multi_group_choice: false,
      type_group_name_entry: None,
      advance_to_next_entry: false,
      is_ctrl_map_equality: false,
      entry_counts: None,
      validated_keys: None,
      values_to_validate: None,
      validating_value: false,
      valid_array_items: None,
      array_errors: None,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      visited_rules: std::collections::HashSet::new(),
      range_upper: None,
    }
  }

  #[cfg(target_arch = "wasm32")]
  #[cfg(feature = "additional-controls")]
  /// New cborValidation from CDDL AST and cbor value
  pub fn new(cddl: &'a CDDL<'a>, cbor: Value, enabled_features: Option<Box<[JsValue]>>) -> Self {
    CBORValidator {
      cddl,
      cbor,
      errors: Vec::default(),
      cddl_location: String::new(),
      cbor_location: String::new(),
      occurrence: None,
      group_entry_idx: None,
      object_value: None,
      is_member_key: false,
      is_cut_present: false,
      cut_value: None,
      eval_generic_rule: None,
      generic_rules: Vec::new(),
      ctrl: None,
      is_group_to_choice_enum: false,
      is_multi_type_choice: false,
      is_multi_group_choice: false,
      type_group_name_entry: None,
      advance_to_next_entry: false,
      is_ctrl_map_equality: false,
      entry_counts: None,
      validated_keys: None,
      values_to_validate: None,
      validating_value: false,
      valid_array_items: None,
      array_errors: None,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      visited_rules: std::collections::HashSet::new(),
      enabled_features,
      has_feature_errors: false,
      disabled_features: None,
      range_upper: None,
    }
  }

  #[cfg(target_arch = "wasm32")]
  #[cfg(not(feature = "additional-controls"))]
  /// New cborValidation from CDDL AST and cbor value
  pub fn new(cddl: &'a CDDL<'a>, cbor: Value) -> Self {
    CBORValidator {
      cddl,
      cbor,
      errors: Vec::default(),
      cddl_location: String::new(),
      cbor_location: String::new(),
      occurrence: None,
      group_entry_idx: None,
      object_value: None,
      is_member_key: false,
      is_cut_present: false,
      cut_value: None,
      eval_generic_rule: None,
      generic_rules: Vec::new(),
      ctrl: None,
      is_group_to_choice_enum: false,
      is_multi_type_choice: false,
      is_multi_group_choice: false,
      type_group_name_entry: None,
      advance_to_next_entry: false,
      is_ctrl_map_equality: false,
      entry_counts: None,
      validated_keys: None,
      values_to_validate: None,
      validating_value: false,
      valid_array_items: None,
      array_errors: None,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      visited_rules: std::collections::HashSet::new(),
      range_upper: None,
    }
  }

  /// Extract the underlying CBOR Value.
  pub fn extract_cbor(self) -> Value {
    self.cbor
  }

  /// Create a new CBORValidator with inherited recursion state
  fn new_with_recursion_state(&self, cbor: Value) -> CBORValidator<'a> {
    #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
    let mut cv = CBORValidator::new(self.cddl, cbor, self.enabled_features.clone());
    #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
    let mut cv = CBORValidator::new(self.cddl, cbor, self.enabled_features);
    #[cfg(not(feature = "additional-controls"))]
    let mut cv = CBORValidator::new(self.cddl, cbor);

    cv.generic_rules = self.generic_rules.clone();
    cv.eval_generic_rule = self.eval_generic_rule;
    cv.visited_rules = self.visited_rules.clone();
    cv
  }

  fn validate_array_items<T: std::fmt::Debug + 'static>(
    &mut self,
    token: &ArrayItemToken,
  ) -> visitor::Result<Error<T>>
  where
    cbor::Error<T>: From<cbor::Error<std::io::Error>>,
  {
    if let Value::Array(a) = &self.cbor {
      // Member keys are annotation only in an array context
      if self.is_member_key {
        return Ok(());
      }

      match validate_array_occurrence(
        self.occurrence.as_ref(),
        self.entry_counts.as_ref().map(|ec| &ec[..]),
        a,
      ) {
        Ok((iter_items, allow_empty_array)) => {
          if iter_items {
            for (idx, v) in a.iter().enumerate() {
              if let Some(indices) = &self.valid_array_items {
                if self.is_multi_type_choice && indices.contains(&idx) {
                  continue;
                }
              }

              #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
              let mut cv = self.new_with_recursion_state(v.clone());
              #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
              let mut cv = self.new_with_recursion_state(v.clone());
              #[cfg(not(feature = "additional-controls"))]
              let mut cv = self.new_with_recursion_state(v.clone());

              cv.generic_rules = self.generic_rules.clone();
              cv.eval_generic_rule = self.eval_generic_rule;
              cv.ctrl = self.ctrl;
              cv.is_multi_type_choice = self.is_multi_type_choice;
              let _ = write!(cv.cbor_location, "{}/{}", self.cbor_location, idx);

              match token {
                ArrayItemToken::Value(value) => cv.visit_value(value)?,
                ArrayItemToken::Range(lower, upper, is_inclusive) => {
                  cv.visit_range(lower, upper, *is_inclusive)?
                }
                // Special handling for nested arrays when using the Group variant
                ArrayItemToken::Group(group) if v.is_array() => {
                  // Special handling for nested arrays
                  cv.visit_group(group)?;
                }
                ArrayItemToken::Group(group) => cv.visit_group(group)?,
                ArrayItemToken::Identifier(ident) => cv.visit_identifier(ident)?,
                ArrayItemToken::TaggedData(tagged_data) => cv.visit_type2(tagged_data)?,
              }

              if self.is_multi_type_choice && cv.errors.is_empty() {
                if let Some(indices) = &mut self.valid_array_items {
                  indices.push(idx);
                } else {
                  self.valid_array_items = Some(vec![idx]);
                }
                continue;
              }

              if let Some(errors) = &mut self.array_errors {
                if let Some(error) = errors.get_mut(&idx) {
                  error.append(&mut cv.errors);
                } else {
                  errors.insert(idx, cv.errors);
                }
              } else {
                let mut errors = HashMap::new();
                errors.insert(idx, cv.errors);
                self.array_errors = Some(errors)
              }
            }
          } else {
            let idx = if !self.is_multi_type_choice {
              self.group_entry_idx.take()
            } else {
              self.group_entry_idx
            };

            if let Some(idx) = idx {
              if let Some(v) = a.get(idx) {
                #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
                let mut cv = self.new_with_recursion_state(v.clone());
                #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
                let mut cv = self.new_with_recursion_state(v.clone());
                #[cfg(not(feature = "additional-controls"))]
                let mut cv = self.new_with_recursion_state(v.clone());

                cv.ctrl = self.ctrl;
                cv.is_multi_type_choice = self.is_multi_type_choice;
                let _ = write!(cv.cbor_location, "{}/{}", self.cbor_location, idx);

                match token {
                  ArrayItemToken::Value(value) => cv.visit_value(value)?,
                  ArrayItemToken::Range(lower, upper, is_inclusive) => {
                    cv.visit_range(lower, upper, *is_inclusive)?
                  }
                  // Special nested array handling when using the Group variant
                  ArrayItemToken::Group(group) if v.is_array() => {
                    // Special handling for nested arrays
                    cv.visit_group(group)?;
                  }
                  ArrayItemToken::Group(group) => cv.visit_group(group)?,
                  ArrayItemToken::Identifier(ident) => cv.visit_identifier(ident)?,
                  ArrayItemToken::TaggedData(tagged_data) => cv.visit_type2(tagged_data)?,
                }

                self.errors.append(&mut cv.errors);
              } else if !allow_empty_array {
                self.add_error(token.error_msg(Some(idx)));
              }
            } else if !self.is_multi_type_choice {
              self.add_error(format!("{}, got {:?}", token.error_msg(None), self.cbor));
            }
          }
        }
        Err(errors) => {
          for e in errors.into_iter() {
            self.add_error(e);
          }
        }
      }
    }

    Ok(())
  }

  // Helper function to resolve a Type2 bound to a usize value
  fn resolve_bound_to_uint(&self, bound: &Type2<'a>) -> std::result::Result<usize, String> {
    match bound {
      Type2::UintValue { value, .. } => Ok(*value),
      Type2::Typename { ident, .. } => {
        // Look up the identifier in the CDDL rules
        for rule in self.cddl.rules.iter() {
          if let Rule::Type { rule, .. } = rule {
            if rule.name.ident == ident.ident {
              // Found the rule, now check if it's a simple uint value
              if rule.value.type_choices.len() == 1 {
                let type_choice = &rule.value.type_choices[0];
                if let Type2::UintValue { value, .. } = &type_choice.type1.type2 {
                  return Ok(*value);
                }
              }
              return Err(format!("Type name '{}' does not resolve to a simple uint value", ident.ident));
            }
          }
        }
        Err(format!("Type name '{}' not found in CDDL rules", ident.ident))
      },
      _ => Err(format!("Expected uint value or type name, got {}", bound)),
    }
  }
}

impl<'a, T: std::fmt::Debug + 'static> Validator<'a, '_, cbor::Error<T>> for CBORValidator<'a>
where
  cbor::Error<T>: From<cbor::Error<std::io::Error>>,
{
  fn validate(&mut self) -> std::result::Result<(), cbor::Error<T>> {
    for r in self.cddl.rules.iter() {
      // First type rule is root
      if let Rule::Type { rule, .. } = r {
        if rule.generic_params.is_none() {
          self.is_root = true;
          self.visit_type_rule(rule)?;
          self.is_root = false;
          break;
        }
      }
    }

    if !self.errors.is_empty() {
      return Err(Error::Validation(self.errors.clone()));
    }

    Ok(())
  }

  fn add_error(&mut self, reason: String) {
    self.errors.push(ValidationError {
      reason,
      cddl_location: self.cddl_location.clone(),
      cbor_location: self.cbor_location.clone(),
      is_multi_type_choice: self.is_multi_type_choice,
      is_multi_group_choice: self.is_multi_group_choice,
      is_group_to_choice_enum: self.is_group_to_choice_enum,
      type_group_name_entry: self.type_group_name_entry.map(|e| e.to_string()),
    });
  }
}

impl<'a, T: std::fmt::Debug + 'static> Visitor<'a, '_, Error<T>> for CBORValidator<'a>
where
  cbor::Error<T>: From<cbor::Error<std::io::Error>>,
{
  fn visit_type_rule(&mut self, tr: &TypeRule<'a>) -> visitor::Result<Error<T>> {
    if let Some(gp) = &tr.generic_params {
      if let Some(gr) = self
        .generic_rules
        .iter_mut()
        .find(|r| r.name == tr.name.ident)
      {
        gr.params = gp.params.iter().map(|p| p.param.ident).collect();
      } else {
        self.generic_rules.push(GenericRule {
          name: tr.name.ident,
          params: gp.params.iter().map(|p| p.param.ident).collect(),
          args: Vec::new(),
        });
      }
    }

    let type_choice_alternates = type_choice_alternates_from_ident(self.cddl, &tr.name);
    if !type_choice_alternates.is_empty() {
      self.is_multi_type_choice = true;

      if self.cbor.is_array() {
        self.is_multi_type_choice_type_rule_validating_array = true;
      }

      // When there are type choice alternates, we need to treat the main rule
      // and all alternates as equal choices. According to RFC 8610 Section 2.2.2,
      // "/=" extends a type by creating additional choices that should be
      // combined with the main rule definition.
      let error_count = self.errors.len();

      // First try the main rule
      let cur_errors = self.errors.len();
      self.visit_type(&tr.value)?;
      if self.errors.len() == cur_errors {
        for _ in 0..self.errors.len() - error_count {
          self.errors.pop();
        }
        return Ok(());
      }

      // Then try each alternate
      for t in type_choice_alternates {
        let cur_errors = self.errors.len();
        self.visit_type(t)?;
        if self.errors.len() == cur_errors {
          for _ in 0..self.errors.len() - error_count {
            self.errors.pop();
          }
          return Ok(());
        }
      }

      // If we get here, none of the choices matched
      return Ok(());
    }

    if tr.value.type_choices.len() > 1 && self.cbor.is_array() {
      self.is_multi_type_choice_type_rule_validating_array = true;
    }

    self.visit_type(&tr.value)
  }

  fn visit_group_rule(&mut self, gr: &GroupRule<'a>) -> visitor::Result<Error<T>> {
    if let Some(gp) = &gr.generic_params {
      if let Some(gr) = self
        .generic_rules
        .iter_mut()
        .find(|r| r.name == gr.name.ident)
      {
        gr.params = gp.params.iter().map(|p| p.param.ident).collect();
      } else {
        self.generic_rules.push(GenericRule {
          name: gr.name.ident,
          params: gp.params.iter().map(|p| p.param.ident).collect(),
          args: Vec::new(),
        });
      }
    }

    let group_choice_alternates = group_choice_alternates_from_ident(self.cddl, &gr.name);
    if !group_choice_alternates.is_empty() {
      self.is_multi_group_choice = true;
    }

    let error_count = self.errors.len();
    for ge in group_choice_alternates {
      let cur_errors = self.errors.len();
      self.visit_group_entry(ge)?;
      if self.errors.len() == cur_errors {
        for _ in 0..self.errors.len() - error_count {
          self.errors.pop();
        }

        return Ok(());
      }
    }

    self.visit_group_entry(&gr.entry)
  }

  fn visit_type(&mut self, t: &Type<'a>) -> visitor::Result<Error<T>> {
    // Special case for nested array in literal position
    if let Value::Array(outer_array) = &self.cbor {
      if let Some(idx) = self.group_entry_idx {
        // We're processing a specific array item
        if let Some(item) = outer_array.get(idx) {
          if item.is_array() {
            // This is a nested array, check if we're supposed to validate against an array type
            for tc in t.type_choices.iter() {
              if let Type2::Array { .. } = &tc.type1.type2 {
                #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
                let mut cv =
                  CBORValidator::new(self.cddl, item.clone(), self.enabled_features.clone());
                #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
                let mut cv = CBORValidator::new(self.cddl, item.clone(), self.enabled_features);
                #[cfg(not(feature = "additional-controls"))]
                let mut cv = CBORValidator::new(self.cddl, item.clone());

                cv.generic_rules = self.generic_rules.clone();
                cv.eval_generic_rule = self.eval_generic_rule;
                cv.is_multi_type_choice = self.is_multi_type_choice;

                let _ = write!(cv.cbor_location, "{}/{}", self.cbor_location, idx);

                // Visit the type choice with the inner array value
                cv.visit_type_choice(tc)?;

                self.errors.append(&mut cv.errors);
                return Ok(());
              }
            }
          }
        }
      }
    }

    // Regular type processing
    if t.type_choices.len() > 1 {
      self.is_multi_type_choice = true;
    }

    let initial_error_count = self.errors.len();
    let mut choice_validation_succeeded = false;

    for type_choice in t.type_choices.iter() {
      if matches!(self.cbor, Value::Array(_))
        && !self.is_multi_type_choice_type_rule_validating_array
      {
        let error_count = self.errors.len();

        self.visit_type_choice(type_choice)?;

        #[cfg(feature = "additional-controls")]
        if self.errors.len() == error_count
          && !self.has_feature_errors
          && self.disabled_features.is_none()
        {
          // Disregard invalid type choice validation errors if one of the
          // choices validates successfully
          let type_choice_error_count = self.errors.len() - initial_error_count;
          if type_choice_error_count > 0 {
            for _ in 0..type_choice_error_count {
              self.errors.pop();
            }
          }
          choice_validation_succeeded = true;
        }

        #[cfg(not(feature = "additional-controls"))]
        if self.errors.len() == error_count {
          // Disregard invalid type choice validation errors if one of the
          // choices validates successfully
          let type_choice_error_count = self.errors.len() - initial_error_count;
          if type_choice_error_count > 0 {
            for _ in 0..type_choice_error_count {
              self.errors.pop();
            }
          }
          choice_validation_succeeded = true;
        }

        continue;
      }

      // Create a copy of the validator to test this choice in isolation
      let mut choice_validator = self.clone();
      choice_validator.errors.clear();

      choice_validator.visit_type_choice(type_choice)?;

      // If this choice validates successfully (no errors), use it
      if choice_validator.errors.is_empty() {
        #[cfg(feature = "additional-controls")]
        if !choice_validator.has_feature_errors || choice_validator.disabled_features.is_some() {
          // Clear any accumulated errors and return success
          let type_choice_error_count = self.errors.len() - initial_error_count;
          if type_choice_error_count > 0 {
            for _ in 0..type_choice_error_count {
              self.errors.pop();
            }
          }
          return Ok(());
        }

        #[cfg(not(feature = "additional-controls"))]
        {
          // Clear any accumulated errors and return success
          let type_choice_error_count = self.errors.len() - initial_error_count;
          if type_choice_error_count > 0 {
            for _ in 0..type_choice_error_count {
              self.errors.pop();
            }
          }
          return Ok(());
        }
      } else {
        // This choice failed, accumulate its errors
        self.errors.extend(choice_validator.errors);
      }
    }

    // If we got here and choice_validation_succeeded is true (for array case),
    // then validation succeeded
    if choice_validation_succeeded {
      return Ok(());
    }
    Ok(())
  }

  fn visit_group(&mut self, g: &Group<'a>) -> visitor::Result<Error<T>> {
    if g.group_choices.len() > 1 {
      self.is_multi_group_choice = true;
    }

    // Map equality/inequality validation
    if self.is_ctrl_map_equality {
      if let Some(t) = &self.ctrl {
        if let Value::Map(m) = &self.cbor {
          let entry_counts = entry_counts_from_group(self.cddl, g);
          let len = m.len();
          if let ControlOperator::EQ | ControlOperator::NE = t {
            if !validate_entry_count(&entry_counts, len) {
              for ec in entry_counts.iter() {
                if let Some(occur) = &ec.entry_occurrence {
                  self.add_error(format!(
                    "expected array with length per occurrence {}",
                    occur,
                  ));
                } else {
                  self.add_error(format!(
                    "expected array with length {}, got {}",
                    ec.count, len
                  ));
                }
              }
              return Ok(());
            }
          }
        }
      }
    }

    self.is_ctrl_map_equality = false;

    let initial_error_count = self.errors.len();
    for group_choice in g.group_choices.iter() {
      let error_count = self.errors.len();
      self.visit_group_choice(group_choice)?;
      if self.errors.len() == error_count {
        // Disregard invalid group choice validation errors if one of the
        // choices validates successfully
        let group_choice_error_count = self.errors.len() - initial_error_count;
        if group_choice_error_count > 0 {
          for _ in 0..group_choice_error_count {
            self.errors.pop();
          }
        }

        return Ok(());
      }
    }

    Ok(())
  }

  fn visit_group_choice(&mut self, gc: &GroupChoice<'a>) -> visitor::Result<Error<T>> {
    if self.is_group_to_choice_enum {
      let initial_error_count = self.errors.len();
      for tc in type_choices_from_group_choice(self.cddl, gc).iter() {
        let error_count = self.errors.len();
        self.visit_type_choice(tc)?;
        if self.errors.len() == error_count {
          let type_choice_error_count = self.errors.len() - initial_error_count;
          if type_choice_error_count > 0 {
            for _ in 0..type_choice_error_count {
              self.errors.pop();
            }
          }
          return Ok(());
        }
      }

      return Ok(());
    }

    for (idx, ge) in gc.group_entries.iter().enumerate() {
      self.group_entry_idx = Some(idx);

      self.visit_group_entry(&ge.0)?;
    }

    Ok(())
  }

  fn visit_range(
    &mut self,
    lower: &Type2,
    upper: &Type2,
    is_inclusive: bool,
  ) -> visitor::Result<Error<T>> {
    if let Value::Array(_) = &self.cbor {
      return self.validate_array_items(&ArrayItemToken::Range(lower, upper, is_inclusive));
    }

    // Resolve the bounds to uint values
    let l_result = self.resolve_bound_to_uint(lower);
    let u_result = self.resolve_bound_to_uint(upper);

    match (l_result, u_result) {
      (Ok(l), Ok(u)) => {
        match &self.cbor {
          Value::Bytes(b) => {
            let len = b.len();
            if is_inclusive {
              if len < l || len > u {
                self.add_error(format!(
                  "expected uint to be in range {} <= value <= {}, got Bytes({:?})",
                  l, u, b
                ));
              }
            } else if len < l || len >= u {
              self.add_error(format!(
                "expected uint to be in range {} <= value < {}, got Bytes({:?})",
                l, u, b
              ));
            }
          }
          Value::Text(s) => match self.ctrl {
            Some(ControlOperator::SIZE) => {
              let len = s.len();
              let s = s.clone();
              if is_inclusive {
                if s.len() < l || s.len() > u {
                  self.add_error(format!(
                    "expected \"{}\" string length to be in the range {} <= value <= {}, got {}",
                    s, l, u, len
                  ));
                }

                return Ok(());
              } else if s.len() < l || s.len() >= u {
                self.add_error(format!(
                  "expected \"{}\" string length to be in the range {} <= value < {}, got {}",
                  s, l, u, len
                ));
                return Ok(());
              }
            }
            _ => {
              self.add_error("string value cannot be validated against a range without the .size control operator".to_string());
              return Ok(());
            }
          },
          Value::Integer(i) => {
            if is_inclusive {
              if i128::from(*i) < l as i128 || i128::from(*i) > u as i128 {
                self.add_error(format!(
                  "expected integer to be in range {} <= value <= {}, got {:?}",
                  l, u, i
                ));
              }
            } else if i128::from(*i) < l as i128 || i128::from(*i) >= u as i128 {
              self.add_error(format!(
                "expected integer to be in range {} <= value < {}, got {:?}",
                l, u, i
              ));
            }
          }
          _ => {
            self.add_error(format!(
              "expected value to be in range {} {} value {} {}, got {:?}",
              l,
              if is_inclusive { "<=" } else { "<" },
              if is_inclusive { "<=" } else { "<" },
              u,
              self.cbor
            ));
          }
        }
      }
       (Ok(_), Err(u_err)) => {
        self.add_error(format!(
           "invalid cddl range. upper value must be a uint type. got {}. Error: {}",
          upper, u_err
        ));
      }
      (Err(l_err), Ok(_)) => {
        self.add_error(format!(
          "invalid cddl range. lower value must be a uint type. got {}. Error: {}",
          lower, l_err
        ));
      }
      (Err(l_err), Err(u_err)) => {
        let err_msg = format!("{} and {}", l_err, u_err);
        self.add_error(format!(
          "invalid cddl range. upper and lower values must be uint types. got {} and {}. Error: {}",
          lower, upper, err_msg
        ));
      }
    }

    Ok(())
  }

  fn visit_control_operator(
    &mut self,
    target: &Type2<'a>,
    ctrl: ControlOperator,
    controller: &Type2<'a>,
  ) -> visitor::Result<Error<T>> {
    if let Type2::Typename {
      ident: target_ident,
      ..
    } = target
    {
      if let Type2::Typename {
        ident: controller_ident,
        ..
      } = controller
      {
        if let Some(name) = self.eval_generic_rule {
          if let Some(gr) = self
            .generic_rules
            .iter()
            .find(|&gr| gr.name == name)
            .cloned()
          {
            for (idx, gp) in gr.params.iter().enumerate() {
              if let Some(arg) = gr.args.get(idx) {
                if *gp == target_ident.ident {
                  let t2 = Type2::from(arg.clone());

                  if *gp == controller_ident.ident {
                    return self.visit_control_operator(&t2, ctrl, &t2);
                  }

                  return self.visit_control_operator(&arg.type2, ctrl, controller);
                }
              }
            }
          }
        }
      }

      if let Some(name) = self.eval_generic_rule {
        if let Some(gr) = self
          .generic_rules
          .iter()
          .find(|&gr| gr.name == name)
          .cloned()
        {
          for (idx, gp) in gr.params.iter().enumerate() {
            if let Some(arg) = gr.args.get(idx) {
              if *gp == target_ident.ident {
                let t2 = Type2::from(arg.clone());
                return self.visit_control_operator(&t2, ctrl, controller);
              }
            }
          }
        }
      }
    }

    match ctrl {
      ControlOperator::EQ => {
        match target {
          Type2::Typename { ident, .. } => {
            if is_ident_string_data_type(self.cddl, ident)
              || is_ident_numeric_data_type(self.cddl, ident)
            {
              return self.visit_type2(controller);
            }
          }
          Type2::Array { group, .. } => {
            if let Value::Array(_) = &self.cbor {
              self.entry_counts = Some(entry_counts_from_group(self.cddl, group));
              self.visit_type2(controller)?;
              self.entry_counts = None;
              return Ok(());
            }
          }
          Type2::Map { .. } => {
            if let Value::Map(_) = &self.cbor {
              self.ctrl = Some(ctrl);
              self.is_ctrl_map_equality = true;
              self.visit_type2(controller)?;
              self.ctrl = None;
              self.is_ctrl_map_equality = false;
              return Ok(());
            }
          }
          _ => self.add_error(format!(
            "target for .eq operator must be a string, numerical, array or map data type, got {}",
            target
          )),
        }
        Ok(())
      }
      ControlOperator::NE => {
        match target {
          Type2::Typename { ident, .. } => {
            if is_ident_string_data_type(self.cddl, ident)
              || is_ident_numeric_data_type(self.cddl, ident)
            {
              self.ctrl = Some(ctrl);
              self.visit_type2(controller)?;
              self.ctrl = None;
              return Ok(());
            }
          }
          Type2::Array { .. } => {
            if let Value::Array(_) = &self.cbor {
              self.ctrl = Some(ctrl);
              self.visit_type2(controller)?;
              self.ctrl = None;
              return Ok(());
            }
          }
          Type2::Map { .. } => {
            if let Value::Map(_) = &self.cbor {
              self.ctrl = Some(ctrl);
              self.is_ctrl_map_equality = true;
              self.visit_type2(controller)?;
              self.ctrl = None;
              self.is_ctrl_map_equality = false;
              return Ok(());
            }
          }
          _ => self.add_error(format!(
            "target for .ne operator must be a string, numerical, array or map data type, got {}",
            target
          )),
        }
        Ok(())
      }
      ControlOperator::LT | ControlOperator::GT | ControlOperator::GE | ControlOperator::LE => {
        match target {
          Type2::Typename { ident, .. } if is_ident_numeric_data_type(self.cddl, ident) => {
            self.ctrl = Some(ctrl);
            self.visit_type2(controller)?;
            self.ctrl = None;
            Ok(())
          }
          _ => {
            self.add_error(format!(
              "target for .lt, .gt, .ge or .le operator must be a numerical data type, got {}",
              target
            ));
            Ok(())
          }
        }
      }
      ControlOperator::SIZE => match target {
        Type2::Typename { ident, .. }
          if is_ident_string_data_type(self.cddl, ident)
            || is_ident_uint_data_type(self.cddl, ident)
            || is_ident_byte_string_data_type(self.cddl, ident) =>
        {
          self.ctrl = Some(ctrl);
          self.visit_type2(controller)?;
          self.ctrl = None;
          Ok(())
        }
        _ => {
          self.add_error(format!(
            "target for .size must a string or uint data type, got {}",
            target
          ));
          Ok(())
        }
      },
      ControlOperator::AND => {
        self.ctrl = Some(ctrl);
        self.visit_type2(target)?;
        self.visit_type2(controller)?;
        self.ctrl = None;
        Ok(())
      }
      ControlOperator::WITHIN => {
        self.ctrl = Some(ctrl);
        let error_count = self.errors.len();
        self.visit_type2(target)?;
        let no_errors = self.errors.len() == error_count;
        self.visit_type2(controller)?;
        if no_errors && self.errors.len() > error_count {
          for _ in 0..self.errors.len() - error_count {
            self.errors.pop();
          }

          self.add_error(format!(
            "expected type {} .within type {}, got {:?}",
            target, controller, self.cbor,
          ));
        }

        self.ctrl = None;

        Ok(())
      }
      ControlOperator::DEFAULT => {
        self.ctrl = Some(ctrl);
        let error_count = self.errors.len();
        self.visit_type2(target)?;
        if self.errors.len() != error_count {
          #[cfg(feature = "ast-span")]
          if let Some(Occur::Optional { .. }) = self.occurrence.take() {
            self.add_error(format!(
              "expected default value {}, got {:?}",
              controller, self.cbor
            ));
          }
          #[cfg(not(feature = "ast-span"))]
          if let Some(Occur::Optional {}) = self.occurrence.take() {
            self.add_error(format!(
              "expected default value {}, got {:?}",
              controller, self.cbor
            ));
          }
        }
        self.ctrl = None;
        Ok(())
      }
      ControlOperator::REGEXP | ControlOperator::PCRE => {
        self.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match self.cbor {
              Value::Text(_) | Value::Array(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".regexp/.pcre control can only be matched against CBOR string, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".regexp/.pcre control can only be matched against string data type, got {}",
            target
          )),
        }
        self.ctrl = None;

        Ok(())
      }
      ControlOperator::CBOR | ControlOperator::CBORSEQ => {
        self.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_byte_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Bytes(b) => {
                // Handle direct byte string case
                let inner_value = ciborium::de::from_reader(&b[..]);
                match inner_value {
                  Ok(value) => {
                    #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
                    let mut cv =
                      CBORValidator::new(self.cddl, value, self.enabled_features.clone());
                    #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
                    let mut cv = CBORValidator::new(self.cddl, value, self.enabled_features);
                    #[cfg(not(feature = "additional-controls"))]
                    let mut cv = CBORValidator::new(self.cddl, value);

                    cv.generic_rules = self.generic_rules.clone();
                    cv.eval_generic_rule = self.eval_generic_rule;
                    cv.cbor_location.push_str(&self.cbor_location);

                    cv.visit_type2(controller)?;

                    if !cv.errors.is_empty() {
                      self.errors.append(&mut cv.errors);
                    }
                  }
                  Err(e) => {
                    self.add_error(format!("error decoding embedded CBOR: {}", e));
                  }
                }
              }
              Value::Array(arr) => {
                // Handle array of byte strings case
                for (idx, item) in arr.iter().enumerate() {
                  if let Value::Bytes(b) = item {
                    let inner_value = ciborium::de::from_reader(&b[..]);
                    match inner_value {
                      Ok(value) => {
                        let current_location = self.cbor_location.clone();
                        #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
                        let mut cv =
                          CBORValidator::new(self.cddl, value, self.enabled_features.clone());
                        #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
                        let mut cv = CBORValidator::new(self.cddl, value, self.enabled_features);
                        #[cfg(not(feature = "additional-controls"))]
                        let mut cv = CBORValidator::new(self.cddl, value);

                        cv.generic_rules = self.generic_rules.clone();
                        cv.eval_generic_rule = self.eval_generic_rule;
                        let _ = write!(cv.cbor_location, "{}/{}", self.cbor_location, idx);

                        cv.visit_type2(controller)?;

                        if !cv.errors.is_empty() {
                          self.errors.append(&mut cv.errors);
                        }
                        self.cbor_location = current_location;
                      }
                      Err(e) => {
                        let error_msg =
                          format!("error decoding embedded CBOR at index {}: {}", idx, e);
                        self.errors.push(ValidationError {
                          reason: error_msg,
                          cddl_location: self.cddl_location.clone(),
                          cbor_location: self.cbor_location.clone(),
                          is_multi_type_choice: self.is_multi_type_choice,
                          is_multi_group_choice: self.is_multi_group_choice,
                          is_group_to_choice_enum: self.is_group_to_choice_enum,
                          type_group_name_entry: self.type_group_name_entry.map(|e| e.to_string()),
                        });
                      }
                    }
                  } else {
                    let error_msg = format!(
                      "array item at index {} must be a byte string for .cbor control, got {:?}",
                      idx, item
                    );
                    self.errors.push(ValidationError {
                      reason: error_msg,
                      cddl_location: self.cddl_location.clone(),
                      cbor_location: self.cbor_location.clone(),
                      is_multi_type_choice: self.is_multi_type_choice,
                      is_multi_group_choice: self.is_multi_group_choice,
                      is_group_to_choice_enum: self.is_group_to_choice_enum,
                      type_group_name_entry: self.type_group_name_entry.map(|e| e.to_string()),
                    });
                  }
                }
              }
              _ => {
                self.add_error(format!(
                  ".cbor control can only be matched against a CBOR byte string or array of byte strings, got {:?}",
                  self.cbor
                ));
              }
            }
          }
          _ => self.add_error(format!(
            ".cbor control can only be matched against a byte string data type, got {}",
            target
          )),
        }
        self.ctrl = None;

        Ok(())
      }
      ControlOperator::BITS => {
        self.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. }
            if is_ident_byte_string_data_type(self.cddl, ident)
              || is_ident_uint_data_type(self.cddl, ident) =>
          {
            match &self.cbor {
              Value::Bytes(_) | Value::Array(_) => self.visit_type2(controller)?,
              Value::Integer(i) if i128::from(*i) >= 0i128 => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                "{} control can only be matched against a CBOR byte string or uint, got {:?}",
                ctrl, self.cbor,
              )),
            }
          }
          _ => self.add_error(format!(
            ".bits control can only be matched against a byte string data type, got {}",
            target
          )),
        }
        self.ctrl = None;

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::CAT => {
        self.ctrl = Some(ctrl);

        match cat_operation(self.cddl, target, controller, false) {
          Ok(values) => {
            let error_count = self.errors.len();
            for v in values.iter() {
              let cur_errors = self.errors.len();

              self.visit_type2(v)?;

              if self.errors.len() == cur_errors {
                for _ in 0..self.errors.len() - error_count {
                  self.errors.pop();
                }

                break;
              }
            }
          }
          Err(e) => self.add_error(e),
        }

        self.ctrl = None;

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::DET => {
        self.ctrl = Some(ctrl);

        match cat_operation(self.cddl, target, controller, true) {
          Ok(values) => {
            let error_count = self.errors.len();

            for v in values.iter() {
              let cur_errors = self.errors.len();
              self.visit_type2(v)?;

              if self.errors.len() == cur_errors {
                for _ in 0..self.errors.len() - error_count {
                  self.errors.pop();
                }

                break;
              }
            }
          }
          Err(e) => self.add_error(e),
        }

        self.ctrl = None;

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::PLUS => {
        self.ctrl = Some(ctrl);

        match plus_operation(self.cddl, target, controller) {
          Ok(values) => {
            let error_count = self.errors.len();
            for v in values.iter() {
              let cur_errors = self.errors.len();

              self.visit_type2(v)?;

              self.visit_type2(v)?;
              if self.errors.len() == cur_errors {
                for _ in 0..self.errors.len() - error_count {
                  self.errors.pop();
                }

                break;
              }
            }
          }

          Err(e) => self.add_error(e),
        }

        self.ctrl = None;

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::ABNF => {
        self.ctrl = Some(ctrl);

        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match self.cbor {
              Value::Text(_) | Value::Array(_) => {
                if let Type2::ParenthesizedType { pt, .. } = controller {
                  match abnf_from_complex_controller(self.cddl, pt) {
                    Ok(values) => {
                      let error_count = self.errors.len();
                      for v in values.iter() {
                        let cur_errors = self.errors.len();

                        self.visit_type2(v)?;

                        if self.errors.len() == cur_errors {
                          for _ in 0..self.errors.len() - error_count {
                            self.errors.pop();
                          }

                          break;
                        }
                      }
                    }
                    Err(e) => self.add_error(e),
                  }
                } else {
                  self.visit_type2(controller)?
                }
              }
              _ => self.add_error(format!(
                ".abnf control can only be matched against a cbor string, got {:?}",
                self.cbor,
              )),
            }
          }
          _ => self.add_error(format!(
            ".abnf can only be matched against string data type, got {}",
            target,
          )),
        }

        self.ctrl = None;

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::ABNFB => {
        self.ctrl = Some(ctrl);

        match target {
          Type2::Typename { ident, .. } if is_ident_byte_string_data_type(self.cddl, ident) => {
            match self.cbor {
              Value::Bytes(_) | Value::Array(_) => {
                if let Type2::ParenthesizedType { pt, .. } = controller {
                  match abnf_from_complex_controller(self.cddl, pt) {
                    Ok(values) => {
                      let error_count = self.errors.len();
                      for v in values.iter() {
                        let cur_errors = self.errors.len();

                        self.visit_type2(v)?;

                        if self.errors.len() == cur_errors {
                          for _ in 0..self.errors.len() - error_count {
                            self.errors.pop();
                          }

                          break;
                        }
                      }
                    }
                    Err(e) => self.add_error(e),
                  }
                } else {
                  self.visit_type2(controller)?
                }
              }
              _ => self.add_error(format!(
                ".abnfb control can only be matched against cbor bytes, got {:?}",
                self.cbor,
              )),
            }
          }
          _ => self.add_error(format!(
            ".abnfb can only be matched against byte string target data type, got {}",
            target,
          )),
        }

        self.ctrl = None;

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      #[cfg(not(target_arch = "wasm32"))]
      ControlOperator::FEATURE => {
        self.ctrl = Some(ctrl);

        if let Some(ef) = self.enabled_features {
          let tv = text_value_from_type2(self.cddl, controller);
          if let Some(Type2::TextValue { value, .. }) = tv {
            if ef.contains(&&**value) {
              let err_count = self.errors.len();
              self.visit_type2(target)?;
              if self.errors.len() > err_count {
                self.has_feature_errors = true;
              }
              self.ctrl = None;
            } else {
              self
                .disabled_features
                .get_or_insert(vec![value.to_string()])
                .push(value.to_string());
            }
          } else if let Some(Type2::UTF8ByteString { value, .. }) = tv {
            let value = std::str::from_utf8(value).map_err(Error::UTF8Parsing)?;
            if ef.contains(&value) {
              let err_count = self.errors.len();
              self.visit_type2(target)?;
              if self.errors.len() > err_count {
                self.has_feature_errors = true;
              }
              self.ctrl = None;
            } else {
              self
                .disabled_features
                .get_or_insert(vec![value.to_string()])
                .push(value.to_string());
            }
          }
        }

        self.ctrl = None;

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      #[cfg(target_arch = "wasm32")]
      ControlOperator::FEATURE => {
        self.ctrl = Some(ctrl);

        if let Some(ef) = &self.enabled_features {
          let tv = text_value_from_type2(self.cddl, controller);
          if let Some(Type2::TextValue { value, .. }) = tv {
            if ef.contains(&JsValue::from(value.as_ref())) {
              let err_count = self.errors.len();
              self.visit_type2(target)?;
              if self.errors.len() > err_count {
                self.has_feature_errors = true;
              }
              self.ctrl = None;
            } else {
              self
                .disabled_features
                .get_or_insert(vec![value.to_string()])
                .push(value.to_string());
            }
          } else if let Some(Type2::UTF8ByteString { value, .. }) = tv {
            let value = std::str::from_utf8(value).map_err(Error::UTF8Parsing)?;
            if ef.contains(&JsValue::from(value)) {
              let err_count = self.errors.len();
              self.visit_type2(target)?;
              if self.errors.len() > err_count {
                self.has_feature_errors = true;
              }
              self.ctrl = None;
            } else {
              self
                .disabled_features
                .get_or_insert(vec![value.to_string()])
                .push(value.to_string());
            }
          }
        }

        self.ctrl = None;

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64U => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_b64u_text(target, controller, s, false) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .b64u encoded bytes",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".b64u can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64u can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64C => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_b64c_text(target, controller, s, false) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .b64c encoded bytes",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".b64c can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64c can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64USLOPPY => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_b64u_text(target, controller, s, true) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .b64u-sloppy encoded bytes",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".b64u-sloppy can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64u-sloppy can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64CSLOPPY => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_b64c_text(target, controller, s, true) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .b64c-sloppy encoded bytes",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".b64c-sloppy can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64c-sloppy can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::HEX => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_hex_text(
                  target,
                  controller,
                  s,
                  crate::validator::control::HexCase::Any,
                ) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .hex encoded bytes",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".hex can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".hex can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::HEXLC => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_hex_text(
                  target,
                  controller,
                  s,
                  crate::validator::control::HexCase::Lower,
                ) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .hexlc encoded bytes",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".hexlc can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".hexlc can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::HEXUC => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_hex_text(
                  target,
                  controller,
                  s,
                  crate::validator::control::HexCase::Upper,
                ) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .hexuc encoded bytes",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".hexuc can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".hexuc can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B32 => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_b32_text(target, controller, s, false) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .b32 encoded bytes",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".b32 can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".b32 can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::H32 => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_b32_text(target, controller, s, true) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .h32 encoded bytes",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".h32 can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".h32 can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B45 => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_b45_text(target, controller, s) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .b45 encoded bytes",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".b45 can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".b45 can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::BASE10 => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_base10_text(target, controller, s) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .base10 integer format",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".base10 can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".base10 can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::PRINTF => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_printf_text(target, controller, s) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!(
                        "text string \"{}\" does not match .printf format",
                        s
                      ));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".printf can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".printf can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::JSON => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_json_text(target, controller, s) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!("text string \"{}\" does not contain valid JSON", s));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".json can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".json can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::JOIN => {
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Text(s) => {
                match crate::validator::control::validate_join_text(target, controller, s) {
                  Ok(is_valid) => {
                    if !is_valid {
                      self.add_error(format!("text string \"{}\" does not match .join result", s));
                    }
                  }
                  Err(e) => self.add_error(e),
                }
              }
              _ => self.add_error(format!(
                ".join can only be matched against CBOR text, got {:?}",
                self.cbor
              )),
            }
          }
          _ => self.add_error(format!(
            ".join can only be matched against string data type, got {}",
            target
          )),
        }

        Ok(())
      }
    }
  }

  fn visit_type2(&mut self, t2: &Type2<'a>) -> visitor::Result<Error<T>> {
    if matches!(self.ctrl, Some(ControlOperator::CBOR)) {
      if let Value::Bytes(b) = &self.cbor {
        let value = ciborium::de::from_reader(&b[..]);
        match value {
          Ok(value) => {
            let current_location = self.cbor_location.clone();

            #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
            let mut cv = CBORValidator::new(self.cddl, value, self.enabled_features.clone());
            #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
            let mut cv = CBORValidator::new(self.cddl, value, self.enabled_features);
            #[cfg(not(feature = "additional-controls"))]
            let mut cv = CBORValidator::new(self.cddl, value);

            cv.generic_rules = self.generic_rules.clone();
            cv.eval_generic_rule = self.eval_generic_rule;
            cv.is_multi_type_choice = self.is_multi_type_choice;
            cv.is_multi_group_choice = self.is_multi_group_choice;
            cv.cbor_location.push_str(&self.cbor_location);
            cv.type_group_name_entry = self.type_group_name_entry;
            cv.visit_type2(t2)?;

            if cv.errors.is_empty() {
              self.cbor_location = current_location;
              return Ok(());
            }

            self.errors.append(&mut cv.errors);
          }
          Err(e) => {
            self.add_error(format!("error decoding embedded CBOR, {}", e));
          }
        }
      }

      return Ok(());
    } else if matches!(self.ctrl, Some(ControlOperator::CBORSEQ)) {
      if let Value::Bytes(b) = &self.cbor {
        let value = ciborium::de::from_reader(&b[..]);
        match value {
          Ok(Value::Array(_)) => {
            let current_location = self.cbor_location.clone();

            #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
            let mut cv = CBORValidator::new(
              self.cddl,
              value.unwrap_or(Value::Null),
              self.enabled_features.clone(),
            );
            #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
            let mut cv = CBORValidator::new(
              self.cddl,
              value.unwrap_or(Value::Null),
              self.enabled_features,
            );

            #[cfg(not(feature = "additional-controls"))]
            let mut cv = CBORValidator::new(self.cddl, value.unwrap_or(Value::Null));

            cv.generic_rules = self.generic_rules.clone();
            cv.eval_generic_rule = self.eval_generic_rule;
            cv.is_multi_type_choice = self.is_multi_type_choice;
            cv.is_multi_group_choice = self.is_multi_group_choice;
            cv.cbor_location.push_str(&self.cbor_location);
            cv.type_group_name_entry = self.type_group_name_entry;
            cv.visit_type2(t2)?;

            if cv.errors.is_empty() {
              self.cbor_location = current_location;
              return Ok(());
            }

            self.errors.append(&mut cv.errors);
          }
          Err(e) => {
            self.add_error(format!("error decoding embedded CBOR, {}", e));
          }
          Ok(v) => self.add_error(format!(
            "embedded CBOR must be a CBOR sequence, got {:?}",
            v
          )),
        }
      }

      return Ok(());
    }

    match t2 {
      Type2::TextValue { value, .. } => self.visit_value(&token::Value::TEXT(value.clone())),
      Type2::Map { group, .. } => match &self.cbor {
        Value::Map(m) => {
          if self.is_member_key {
            let current_location = self.cbor_location.clone();

            for (k, v) in m.iter() {
              #[cfg(feature = "additional-controls")]
              #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
              let mut cv = CBORValidator::new(self.cddl, k.clone(), self.enabled_features.clone());
              #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
              let mut cv = CBORValidator::new(self.cddl, k.clone(), self.enabled_features);
              #[cfg(not(feature = "additional-controls"))]
              let mut cv = CBORValidator::new(self.cddl, k.clone());

              cv.generic_rules = self.generic_rules.clone();
              cv.eval_generic_rule = self.eval_generic_rule;
              cv.is_multi_type_choice = self.is_multi_type_choice;
              cv.is_multi_group_choice = self.is_multi_group_choice;
              cv.cbor_location.push_str(&self.cbor_location);
              cv.type_group_name_entry = self.type_group_name_entry;
              cv.visit_type2(t2)?;

              if cv.errors.is_empty() {
                self.object_value = Some(v.clone());
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.cbor_location = current_location;
                return Ok(());
              }

              self.errors.append(&mut cv.errors);
            }

            return Ok(());
          }

          // Check if this is an empty map schema with non-empty CBOR map
          if group.group_choices.len() == 1
            && group.group_choices[0].group_entries.is_empty()
            && !m.is_empty()
            && !matches!(
              self.ctrl,
              Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT)
            )
          {
            self.add_error(format!("expected empty map, got {:?}", self.cbor));
            return Ok(());
          }

          #[allow(clippy::needless_collect)]
          let m = m.iter().map(|entry| entry.0.clone()).collect::<Vec<_>>();

          self.visit_group(group)?;

          // If extra map entries are detected, return validation error
          if self.values_to_validate.is_none() {
            for k in m.into_iter() {
              if let Some(keys) = &self.validated_keys {
                if !keys.contains(&k) {
                  self.add_error(format!("unexpected key {:?}", k));
                }
              }
            }
          }

          self.is_cut_present = false;
          self.cut_value = None;
          Ok(())
        }
        Value::Array(_) => self.validate_array_items(&ArrayItemToken::Group(group)),
        _ => {
          self.add_error(format!("expected map object {}, got {:?}", t2, self.cbor));
          Ok(())
        }
      },
      Type2::Array { group, .. } => match &self.cbor {
        Value::Array(a) => {
          if group.group_choices.len() == 1
            && group.group_choices[0].group_entries.is_empty()
            && !a.is_empty()
            && !matches!(
              self.ctrl,
              Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT)
            )
          {
            self.add_error(format!("expected empty array, got {:?}", self.cbor));
            return Ok(());
          }

          self.entry_counts = Some(entry_counts_from_group(self.cddl, group));
          self.visit_group(group)?;
          self.entry_counts = None;

          if let Some(errors) = &mut self.array_errors {
            if let Some(indices) = &self.valid_array_items {
              for idx in indices.iter() {
                errors.remove(idx);
              }
            }

            for error in errors.values_mut() {
              self.errors.append(error);
            }
          }

          self.valid_array_items = None;
          self.array_errors = None;

          Ok(())
        }
        Value::Map(m) if self.is_member_key => {
          let current_location = self.cbor_location.clone();

          self.entry_counts = Some(entry_counts_from_group(self.cddl, group));

          for (k, v) in m.iter() {
            #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
            let mut cv = CBORValidator::new(self.cddl, k.clone(), self.enabled_features.clone());
            #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
            let mut cv = CBORValidator::new(self.cddl, k.clone(), self.enabled_features);
            #[cfg(not(feature = "additional-controls"))]
            let mut cv = CBORValidator::new(self.cddl, k.clone());

            cv.generic_rules = self.generic_rules.clone();
            cv.entry_counts = self.entry_counts.clone();
            cv.eval_generic_rule = self.eval_generic_rule;
            cv.is_multi_type_choice = self.is_multi_type_choice;
            cv.is_multi_group_choice = self.is_multi_group_choice;
            cv.cbor_location.push_str(&self.cbor_location);
            cv.type_group_name_entry = self.type_group_name_entry;
            cv.visit_type2(t2)?;

            if cv.errors.is_empty() {
              self.object_value = Some(v.clone());
              self
                .validated_keys
                .get_or_insert(vec![k.clone()])
                .push(k.clone());
              self.cbor_location = current_location;
              return Ok(());
            }

            self.errors.append(&mut cv.errors);
          }

          self.entry_counts = None;

          Ok(())
        }
        _ => {
          self.add_error(format!("expected array type, got {:?}", self.cbor));
          Ok(())
        }
      },
      Type2::ChoiceFromGroup {
        ident,
        generic_args,
        ..
      } => {
        if let Some(ga) = generic_args {
          if let Some(rule) = rule_from_ident(self.cddl, ident) {
            if let Some(gr) = self
              .generic_rules
              .iter_mut()
              .find(|gr| gr.name == ident.ident)
            {
              for arg in ga.args.iter() {
                gr.args.push((*arg.arg).clone());
              }
            } else if let Some(params) = generic_params_from_rule(rule) {
              self.generic_rules.push(GenericRule {
                name: ident.ident,
                params,
                args: ga.args.iter().cloned().map(|arg| *arg.arg).collect(),
              });
            }

            #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
            let mut cv =
              CBORValidator::new(self.cddl, self.cbor.clone(), self.enabled_features.clone());
            #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
            let mut cv = CBORValidator::new(self.cddl, self.cbor.clone(), self.enabled_features);
            #[cfg(not(feature = "additional-controls"))]
            let mut cv = CBORValidator::new(self.cddl, self.cbor.clone());

            cv.generic_rules = self.generic_rules.clone();
            cv.eval_generic_rule = Some(ident.ident);
            cv.is_group_to_choice_enum = true;
            cv.is_multi_type_choice = self.is_multi_type_choice;
            cv.visit_rule(rule)?;

            self.errors.append(&mut cv.errors);

            return Ok(());
          }
        }

        if group_rule_from_ident(self.cddl, ident).is_none() {
          self.add_error(format!(
            "rule {} must be a group rule to turn it into a choice",
            ident
          ));
          return Ok(());
        }

        self.is_group_to_choice_enum = true;
        self.visit_identifier(ident)?;
        self.is_group_to_choice_enum = false;

        Ok(())
      }
      Type2::ChoiceFromInlineGroup { group, .. } => {
        self.is_group_to_choice_enum = true;
        self.visit_group(group)?;
        self.is_group_to_choice_enum = false;
        Ok(())
      }
      Type2::Typename {
        ident,
        generic_args,
        ..
      } => {
        if let Some(ga) = generic_args {
          if let Some(rule) = rule_from_ident(self.cddl, ident) {
            if let Some(gr) = self
              .generic_rules
              .iter_mut()
              .find(|gr| gr.name == ident.ident)
            {
              for arg in ga.args.iter() {
                gr.args.push((*arg.arg).clone());
              }
            } else if let Some(params) = generic_params_from_rule(rule) {
              self.generic_rules.push(GenericRule {
                name: ident.ident,
                params,
                args: ga.args.iter().cloned().map(|arg| *arg.arg).collect(),
              });
            }

            #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
            let mut cv =
              CBORValidator::new(self.cddl, self.cbor.clone(), self.enabled_features.clone());
            #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
            let mut cv = CBORValidator::new(self.cddl, self.cbor.clone(), self.enabled_features);
            #[cfg(not(feature = "additional-controls"))]
            let mut cv = CBORValidator::new(self.cddl, self.cbor.clone());

            cv.generic_rules = self.generic_rules.clone();
            cv.eval_generic_rule = Some(ident.ident);
            cv.is_multi_type_choice = self.is_multi_type_choice;
            cv.visit_rule(rule)?;

            self.errors.append(&mut cv.errors);

            return Ok(());
          }
        }

        let type_choice_alternates = type_choice_alternates_from_ident(self.cddl, ident);
        if !type_choice_alternates.is_empty() {
          self.is_multi_type_choice = true;
        }

        let error_count = self.errors.len();
        for t in type_choice_alternates {
          let cur_errors = self.errors.len();
          self.visit_type(t)?;
          if self.errors.len() == cur_errors {
            for _ in 0..self.errors.len() - error_count {
              self.errors.pop();
            }

            return Ok(());
          }
        }

        self.visit_identifier(ident)
      }
      Type2::IntValue { value, .. } => self.visit_value(&token::Value::INT(*value)),
      Type2::UintValue { value, .. } => self.visit_value(&token::Value::UINT(*value)),
      Type2::FloatValue { value, .. } => self.visit_value(&token::Value::FLOAT(*value)),
      Type2::UTF8ByteString { value, .. } => {
        self.visit_value(&token::Value::BYTE(ByteValue::UTF8(value.clone())))
      }
      Type2::B16ByteString { value, .. } => {
        self.visit_value(&token::Value::BYTE(ByteValue::B16(value.clone())))
      }
      Type2::ParenthesizedType { pt, .. } => self.visit_type(pt),
      Type2::Unwrap {
        ident,
        generic_args,
        ..
      } => {
        // Per
        // https://github.com/w3c/did-spec-registries/pull/138#issuecomment-719739215,
        // strip tag and validate underlying type
        if let Some(Type2::TaggedData { t, .. }) = tag_from_token(&lookup_ident(ident.ident)) {
          return self.visit_type(&t);
        }

        if let Some(ga) = generic_args {
          if let Some(rule) = unwrap_rule_from_ident(self.cddl, ident) {
            if let Some(gr) = self
              .generic_rules
              .iter_mut()
              .find(|gr| gr.name == ident.ident)
            {
              for arg in ga.args.iter() {
                gr.args.push((*arg.arg).clone());
              }
            } else if let Some(params) = generic_params_from_rule(rule) {
              self.generic_rules.push(GenericRule {
                name: ident.ident,
                params,
                args: ga.args.iter().cloned().map(|arg| *arg.arg).collect(),
              });
            }

            #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
            let mut cv =
              CBORValidator::new(self.cddl, self.cbor.clone(), self.enabled_features.clone());
            #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
            let mut cv = CBORValidator::new(self.cddl, self.cbor.clone(), self.enabled_features);
            #[cfg(not(feature = "additional-controls"))]
            let mut cv = CBORValidator::new(self.cddl, self.cbor.clone());

            cv.generic_rules = self.generic_rules.clone();
            cv.eval_generic_rule = Some(ident.ident);
            cv.is_multi_type_choice = self.is_multi_type_choice;
            cv.visit_rule(rule)?;

            self.errors.append(&mut cv.errors);

            return Ok(());
          }
        }

        if let Some(rule) = unwrap_rule_from_ident(self.cddl, ident) {
          return self.visit_rule(rule);
        }

        self.add_error(format!(
          "cannot unwrap identifier {}, rule not found",
          ident
        ));

        Ok(())
      }
      Type2::TaggedData { tag, t, .. } => match &self.cbor {
        Value::Tag(actual_tag, value) => {
          if let Some(tag_constraint) = tag {
            // For literal tag constraints, check the value matches
            if let Some(expected_tag) = tag_constraint.as_literal() {
              if expected_tag != *actual_tag {
                self.add_error(format!(
                  "expected tagged data #6.{}({}), got {:?}",
                  expected_tag, t, self.cbor
                ));
                return Ok(());
              }
            } else {
              // For type expression constraints, we would need to evaluate the type
              // For now, accept any tag value (this could be enhanced later)
            }
          } else if *actual_tag > 0 {
            self.add_error(format!(
              "expected tagged data #6({}), got {:?}",
              t, self.cbor
            ));
            return Ok(());
          }

          #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
          let mut cv = CBORValidator::new(
            self.cddl,
            value.as_ref().clone(),
            self.enabled_features.clone(),
          );
          #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
          let mut cv = CBORValidator::new(self.cddl, value.as_ref().clone(), self.enabled_features);
          #[cfg(not(feature = "additional-controls"))]
          let mut cv = CBORValidator::new(self.cddl, value.as_ref().clone());

          cv.generic_rules = self.generic_rules.clone();
          cv.eval_generic_rule = self.eval_generic_rule;
          cv.is_multi_type_choice = self.is_multi_type_choice;
          cv.is_multi_group_choice = self.is_multi_group_choice;
          cv.cbor_location.push_str(&self.cbor_location);
          cv.type_group_name_entry = self.type_group_name_entry;
          cv.visit_type(t)?;

          self.errors.append(&mut cv.errors);
          Ok(())
        }
        Value::Array(_) => self.validate_array_items(&ArrayItemToken::TaggedData(t2)),
        _ => {
          if let Some(tag) = tag {
            self.add_error(format!(
              "expected tagged data #6.{}({}), got {:?}",
              tag, t, self.cbor
            ));
          } else {
            self.add_error(format!(
              "expected tagged data #6({}), got {:?}",
              t, self.cbor
            ));
          }

          Ok(())
        }
      },
      Type2::DataMajorType { mt, constraint, .. } => match &self.cbor {
        Value::Integer(i) => {
          match mt {
            0u8 => match constraint {
              Some(c) => {
                if let Some(literal_val) = c.as_literal() {
                  if i128::from(*i) == literal_val as i128 && i128::from(*i) >= 0i128 {
                    return Ok(());
                  }
                }
                self.add_error(format!(
                  "expected uint data type with constraint {} (#{}.{}), got {:?}",
                  c, mt, c, self.cbor
                ));
                return Ok(());
              }
              _ => {
                if i128::from(*i).is_negative() {
                  self.add_error(format!(
                    "expected uint data type (#{}), got {:?}",
                    mt, self.cbor
                  ));
                  return Ok(());
                }
              }
            },
            1u8 => match constraint {
              Some(c) => {
                if let Some(literal_val) = c.as_literal() {
                  if i128::from(*i) == 0i128 - literal_val as i128 {
                    return Ok(());
                  }
                }
                self.add_error(format!(
                  "expected nint type with constraint {} (#{}.{}), got {:?}",
                  c, mt, c, self.cbor
                ));
                return Ok(());
              }
              _ => {
                if i128::from(*i) >= 0i128 {
                  self.add_error(format!(
                    "expected nint data type (#{}), got {:?}",
                    mt, self.cbor
                  ));
                  return Ok(());
                }
              }
            },
            _ => self.add_error(format!(
              "expected major type {} with constraint {:?}, got {:?}",
              mt, constraint, self.cbor
            )),
          }

          Ok(())
        }
        Value::Bytes(b) => {
          match mt {
            2u8 => match constraint {
              Some(c) if c.is_literal(b.len() as u64) => return Ok(()),
              Some(c) => self.add_error(format!(
                "expected byte string type with constraint {} (#{}.{}), got {:?}",
                c, mt, c, self.cbor
              )),
              _ => return Ok(()),
            },
            _ => self.add_error(format!(
              "expected major type {} with constraint {:?}, got {:?}",
              mt, constraint, self.cbor
            )),
          }

          Ok(())
        }
        Value::Text(t) => {
          match mt {
            3u8 => match constraint {
              Some(c) if c.is_literal(t.len() as u64) => return Ok(()),
              Some(c) => self.add_error(format!(
                "expected text string type with constraint {} (#{}.{}), got {:?}",
                c, mt, c, self.cbor
              )),
              _ => return Ok(()),
            },
            _ => self.add_error(format!(
              "expected major type {} with constraint {:?}, got {:?}",
              mt, constraint, self.cbor
            )),
          }

          Ok(())
        }
        Value::Array(a) => {
          match mt {
            4u8 => match constraint {
              Some(c) if c.is_literal(a.len() as u64) => return Ok(()),
              Some(c) => self.add_error(format!(
                "expected array type with constraint {} (#{}.{}), got {:?}",
                c, mt, c, self.cbor
              )),
              _ => return Ok(()),
            },
            _ => self.add_error(format!(
              "expected major type {} with constraint {:?}, got {:?}",
              mt, constraint, self.cbor
            )),
          }

          Ok(())
        }
        Value::Map(m) => {
          match mt {
            5u8 => match constraint {
              Some(c) if c.is_literal(m.len() as u64) => return Ok(()),
              Some(c) => self.add_error(format!(
                "expected map type with constraint {} (#{}.{}), got {:?}",
                c, mt, c, self.cbor
              )),
              _ => return Ok(()),
            },
            _ => self.add_error(format!(
              "expected major type {} with constraint {:?}, got {:?}",
              mt, constraint, self.cbor
            )),
          }

          Ok(())
        }
        Value::Float(_f) => {
          match mt {
            7u8 => match constraint {
              Some(_c) => unimplemented!(),
              _ => return Ok(()),
            },
            _ => self.add_error(format!(
              "expected major type {} with constraint {:?}, got {:?}",
              mt, constraint, self.cbor
            )),
          }

          Ok(())
        }
        _ => {
          if let Some(constraint) = constraint {
            self.add_error(format!(
              "expected major type #{}.{}, got {:?}",
              mt, constraint, self.cbor
            ));
          } else {
            self.add_error(format!("expected major type #{}, got {:?}", mt, self.cbor));
          }

          Ok(())
        }
      },
      #[cfg(feature = "ast-span")]
      Type2::Any { .. } => Ok(()),
      #[cfg(not(feature = "ast-span"))]
      Type2::Any {} => Ok(()),
      _ => {
        self.add_error(format!(
          "unsupported data type for validating cbor, got {}",
          t2
        ));
        Ok(())
      }
    }
  }

  fn visit_identifier(&mut self, ident: &Identifier<'a>) -> visitor::Result<Error<T>> {
    if let Some(name) = self.eval_generic_rule {
      if let Some(gr) = self
        .generic_rules
        .iter()
        .find(|&gr| gr.name == name)
        .cloned()
      {
        for (idx, gp) in gr.params.iter().enumerate() {
          if *gp == ident.ident {
            if let Some(arg) = gr.args.get(idx) {
              return self.visit_type1(arg);
            }
          }
        }
      }
    }

    // self.is_colon_shortcut_present is only true when the ident is part of a
    // member key
    if !self.is_colon_shortcut_present {
      if let Some(r) = rule_from_ident(self.cddl, ident) {
        // Check for recursion to prevent stack overflow
        let rule_key = ident.ident.to_string();
        if self.visited_rules.contains(&rule_key) {
          // We've already validated this rule in the current validation path
          // This is a recursive reference, so we allow it and assume it's valid
          return Ok(());
        }

        // Mark this rule as visited before recursing
        self.visited_rules.insert(rule_key.clone());
        let result = self.visit_rule(r);
        // Remove the rule from visited set after processing
        self.visited_rules.remove(&rule_key);

        return result;
      }
    }

    if is_ident_any_type(self.cddl, ident) {
      return Ok(());
    }

    // Special case for array values - check if we're in an array context and this
    // is a reference to another array type
    if let Value::Array(_) = &self.cbor {
      if let Some(Rule::Type { rule, .. }) = rule_from_ident(self.cddl, ident) {
        for tc in rule.value.type_choices.iter() {
          if let Type2::Array { .. } = &tc.type1.type2 {
            return self.visit_type_choice(tc);
          }
        }
      }
    }

    match &self.cbor {
      Value::Null if is_ident_null_data_type(self.cddl, ident) => Ok(()),
      Value::Bytes(_) if is_ident_byte_string_data_type(self.cddl, ident) => Ok(()),
      Value::Bool(b) => {
        if is_ident_bool_data_type(self.cddl, ident) {
          return Ok(());
        }

        if ident_matches_bool_value(self.cddl, ident, *b) {
          return Ok(());
        }

        self.add_error(format!("expected type {}, got {:?}", ident, self.cbor));
        Ok(())
      }
      Value::Integer(i) => {
        if is_ident_uint_data_type(self.cddl, ident) {
          if i128::from(*i).is_negative() {
            self.add_error(format!("expected type {}, got {:?}", ident, self.cbor));
          }

          Ok(())
        } else if is_ident_integer_data_type(self.cddl, ident) {
          Ok(())
        } else if is_ident_time_data_type(self.cddl, ident) {
          if let chrono::LocalResult::None =
            Utc.timestamp_millis_opt((i128::from(*i) * 1000) as i64)
          {
            let i = *i;
            self.add_error(format!(
              "expected time data type, invalid UNIX timestamp {:?}",
              i,
            ));
          }

          Ok(())
        } else {
          self.add_error(format!("expected type {}, got {:?}", ident, self.cbor));
          Ok(())
        }
      }
      Value::Float(f) => {
        if is_ident_float_data_type(self.cddl, ident) {
          Ok(())
        } else if is_ident_time_data_type(self.cddl, ident) {
          if let chrono::LocalResult::None = Utc.timestamp_millis_opt((*f * 1000f64) as i64) {
            let f = *f;
            self.add_error(format!(
              "expected time data type, invalid UNIX timestamp {:?}",
              f,
            ));
          }

          Ok(())
        } else {
          self.add_error(format!("expected type {}, got {:?}", ident, self.cbor));
          Ok(())
        }
      }
      Value::Text(s) => {
        if is_ident_uri_data_type(self.cddl, ident) {
          if let Err(e) = uriparse::URI::try_from(&**s) {
            self.add_error(format!("expected URI data type, decoding error: {}", e));
          }
        } else if is_ident_b64url_data_type(self.cddl, ident) {
          if let Err(e) = base64_url::decode(s) {
            self.add_error(format!(
              "expected base64 URL data type, decoding error: {}",
              e
            ));
          }
        } else if is_ident_tdate_data_type(self.cddl, ident) {
          if let Err(e) = chrono::DateTime::parse_from_rfc3339(s) {
            self.add_error(format!("expected tdate data type, decoding error: {}", e));
          }
        } else if is_ident_string_data_type(self.cddl, ident) {
          return Ok(());
        } else {
          self.add_error(format!("expected type {}, got {:?}", ident, self.cbor));
        }

        Ok(())
      }
      Value::Tag(tag, value) => {
        match *tag {
          0 => {
            if is_ident_tdate_data_type(self.cddl, ident) {
              if let Value::Text(value) = value.as_ref() {
                if let Err(e) = chrono::DateTime::parse_from_rfc3339(value) {
                  self.add_error(format!("expected tdate data type, decoding error: {}", e));
                }
              } else {
                self.add_error(format!("expected type {}, got {:?}", ident, self.cbor));
              }
            } else {
              self.add_error(format!("expected type {}, got {:?}", ident, self.cbor));
            }
          }
          1 => {
            if is_ident_time_data_type(self.cddl, ident) {
              if let Value::Integer(value) = *value.as_ref() {
                let dt = Utc.timestamp_opt(value.try_into().unwrap(), 0);
                if let chrono::LocalResult::None = dt {
                  self.add_error(format!(
                    "expected time data type, invalid UNIX timestamp {:?}",
                    self.cbor
                  ));
                }
              } else if let Value::Float(value) = value.as_ref() {
                let seconds = value.trunc() as i64;
                let nanoseconds = (value.fract() * 1e9) as u32;
                let dt = Utc.timestamp_opt(seconds, nanoseconds);
                if let chrono::LocalResult::None = dt {
                  self.add_error(format!(
                    "expected time data type, invalid UNIX timestamp {:?}",
                    self.cbor
                  ));
                }
              } else {
                self.add_error(format!("expected type {}, got {:?}", ident, self.cbor));
              }
            } else {
              self.add_error(format!("expected type {}, got {:?}", ident, self.cbor));
            }
          }
          _ => (),
        }

        Ok(())
      }
      Value::Array(_) => self.validate_array_items(&ArrayItemToken::Identifier(ident)),
      Value::Map(m) => {
        match &self.occurrence {
          #[cfg(feature = "ast-span")]
          Some(Occur::Optional { .. }) | None => {
            if is_ident_string_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Text(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_integer_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Integer(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_bool_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Bool(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_null_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Null)) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_byte_string_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Bytes(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_float_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Null)) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if token::lookup_ident(ident.ident)
              .in_standard_prelude()
              .is_some()
            {
              self.add_error(format!(
                "expected object value of type {}, got object",
                ident.ident
              ));
              return Ok(());
            }

            self.visit_value(&token::Value::TEXT(ident.ident.into()))
          }
          #[cfg(not(feature = "ast-span"))]
          Some(Occur::Optional {}) | None => {
            if is_ident_string_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Text(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                self.cbor_location.push_str(&format!("/{}", value));
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }

              return Ok(());
            }

            if is_ident_integer_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Integer(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                self.cbor_location.push_str(&format!("/{}", value));
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_bool_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Bool(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                self.cbor_location.push_str(&format!("/{}", value));
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_null_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Null)) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                self.cbor_location.push_str(&format!("/{}", value));
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_byte_string_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Bytes(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                self.cbor_location.push_str(&format!("/{}", value));
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_float_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Null)) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                self.cbor_location.push_str(&format!("/{}", value));
              } else {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if token::lookup_ident(ident.ident)
              .in_standard_prelude()
              .is_some()
            {
              self.add_error(format!(
                "expected object value of type {}, got object",
                ident.ident
              ));
              return Ok(());
            }

            self.visit_value(&token::Value::TEXT(ident.ident.into()))
          }
          Some(occur) => {
            let mut errors = Vec::new();

            if is_ident_string_data_type(self.cddl, ident) {
              let values_to_validate = m
                .iter()
                .filter_map(|(k, v)| {
                  if let Some(keys) = &self.validated_keys {
                    if !keys.contains(k) {
                      if matches!(k, Value::Text(_)) {
                        Some(v.clone())
                      } else {
                        errors.push(format!("key of type {} required, got {:?}", ident, k));
                        None
                      }
                    } else {
                      None
                    }
                  } else if matches!(k, Value::Text(_)) {
                    Some(v.clone())
                  } else {
                    errors.push(format!("key of type {} required, got {:?}", ident, k));
                    None
                  }
                })
                .collect::<Vec<_>>();

              self.values_to_validate = Some(values_to_validate);
            }

            if is_ident_integer_data_type(self.cddl, ident) {
              let mut errors = Vec::new();
              let values_to_validate = m
                .iter()
                .filter_map(|(k, v)| {
                  if let Some(keys) = &self.validated_keys {
                    if !keys.contains(k) {
                      if matches!(k, Value::Integer(_)) {
                        Some(v.clone())
                      } else {
                        errors.push(format!("key of type {} required, got {:?}", ident, k));
                        None
                      }
                    } else {
                      None
                    }
                  } else if matches!(k, Value::Integer(_)) {
                    Some(v.clone())
                  } else {
                    errors.push(format!("key of type {} required, got {:?}", ident, k));
                    None
                  }
                })
                .collect::<Vec<_>>();

              self.values_to_validate = Some(values_to_validate);
            }

            if is_ident_bool_data_type(self.cddl, ident) {
              let mut errors = Vec::new();
              let values_to_validate = m
                .iter()
                .filter_map(|(k, v)| {
                  if let Some(keys) = &self.validated_keys {
                    if !keys.contains(k) {
                      if matches!(k, Value::Bool(_)) {
                        Some(v.clone())
                      } else {
                        errors.push(format!("key of type {} required, got {:?}", ident, k));
                        None
                      }
                    } else {
                      None
                    }
                  } else if matches!(k, Value::Bool(_)) {
                    Some(v.clone())
                  } else {
                    errors.push(format!("key of type {} required, got {:?}", ident, k));
                    None
                  }
                })
                .collect::<Vec<_>>();

              self.values_to_validate = Some(values_to_validate);
            }

            if is_ident_byte_string_data_type(self.cddl, ident) {
              let mut errors = Vec::new();
              let values_to_validate = m
                .iter()
                .filter_map(|(k, v)| {
                  if let Some(keys) = &self.validated_keys {
                    if !keys.contains(k) {
                      if matches!(k, Value::Bytes(_)) {
                        Some(v.clone())
                      } else {
                        errors.push(format!("key of type {} required, got {:?}", ident, k));
                        None
                      }
                    } else {
                      None
                    }
                  } else if matches!(k, Value::Bytes(_)) {
                    Some(v.clone())
                  } else {
                    errors.push(format!("key of type {} required, got {:?}", ident, k));
                    None
                  }
                })
                .collect::<Vec<_>>();

              self.values_to_validate = Some(values_to_validate);
            }

            if is_ident_null_data_type(self.cddl, ident) {
              let mut errors = Vec::new();
              let values_to_validate = m
                .iter()
                .filter_map(|(k, v)| {
                  if let Some(keys) = &self.validated_keys {
                    if !keys.contains(k) {
                      if matches!(k, Value::Null) {
                        Some(v.clone())
                      } else {
                        errors.push(format!("key of type {} required, got {:?}", ident, k));
                        None
                      }
                    } else {
                      None
                    }
                  } else if matches!(k, Value::Null) {
                    Some(v.clone())
                  } else {
                    errors.push(format!("key of type {} required, got {:?}", ident, k));
                    None
                  }
                })
                .collect::<Vec<_>>();

              self.values_to_validate = Some(values_to_validate);
            }

            if is_ident_float_data_type(self.cddl, ident) {
              let mut errors = Vec::new();
              let values_to_validate = m
                .iter()
                .filter_map(|(k, v)| {
                  if let Some(keys) = &self.validated_keys {
                    if !keys.contains(k) {
                      if matches!(k, Value::Float(_)) {
                        Some(v.clone())
                      } else {
                        errors.push(format!("key of type {} required, got {:?}", ident, k));
                        None
                      }
                    } else {
                      None
                    }
                  } else if matches!(k, Value::Float(_)) {
                    Some(v.clone())
                  } else {
                    errors.push(format!("key of type {} required, got {:?}", ident, k));
                    None
                  }
                })
                .collect::<Vec<_>>();

              self.values_to_validate = Some(values_to_validate);
            }

            // If key validation error occurs, return early before checking occurrences
            if !errors.is_empty() {
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
            }

            #[cfg(feature = "ast-span")]
            if let Occur::ZeroOrMore { .. } | Occur::OneOrMore { .. } = occur {
              if let Occur::OneOrMore { .. } = occur {
                if m.is_empty() {
                  self.add_error(format!(
                    "map cannot be empty, one or more entries with key type {} required",
                    ident
                  ));
                  return Ok(());
                }
              }
            } else if let Occur::Exact { lower, upper, .. } = occur {
              if let Some(values_to_validate) = &self.values_to_validate {
                if let Some(lower) = lower {
                  if let Some(upper) = upper {
                    if values_to_validate.len() < *lower || values_to_validate.len() > *upper {
                      if lower == upper {
                        self.add_error(format!(
                          "object must contain exactly {} entries of key of type {}",
                          lower, ident,
                        ));
                      } else {
                        self.add_error(format!(
                          "object must contain between {} and {} entries of key of type {}",
                          lower, upper, ident,
                        ));
                      }

                      return Ok(());
                    }
                  }

                  if values_to_validate.len() < *lower {
                    self.add_error(format!(
                      "object must contain at least {} entries of key of type {}",
                      lower, ident,
                    ));

                    return Ok(());
                  }
                }

                if let Some(upper) = upper {
                  if values_to_validate.len() > *upper {
                    self.add_error(format!(
                      "object must contain no more than {} entries of key of type {}",
                      upper, ident,
                    ));

                    return Ok(());
                  }
                }

                return Ok(());
              }
            }

            #[cfg(not(feature = "ast-span"))]
            if let Occur::ZeroOrMore {} | Occur::OneOrMore {} = occur {
              if let Occur::OneOrMore {} = occur {
                if m.is_empty() {
                  self.add_error(format!(
                    "object cannot be empty, one or more entries with key type {} required",
                    ident
                  ));
                  return Ok(());
                }
              }
            } else if let Occur::Exact { lower, upper } = occur {
              if let Some(values_to_validate) = &self.values_to_validate {
                if let Some(lower) = lower {
                  if let Some(upper) = upper {
                    if values_to_validate.len() < *lower || values_to_validate.len() > *upper {
                      if lower == upper {
                        self.add_error(format!(
                          "object must contain exactly {} entries of key of type {}",
                          lower, ident,
                        ));
                      } else {
                        self.add_error(format!(
                          "object must contain between {} and {} entries of key of type {}",
                          lower, upper, ident,
                        ));
                      }

                      return Ok(());
                    }
                  }

                  if values_to_validate.len() < *lower {
                    self.add_error(format!(
                      "object must contain at least {} entries of key of type {}",
                      lower, ident,
                    ));

                    return Ok(());
                  }
                }

                if let Some(upper) = upper {
                  if values_to_validate.len() > *upper {
                    self.add_error(format!(
                      "object must contain no more than {} entries of key of type {}",
                      upper, ident,
                    ));

                    return Ok(());
                  }
                }

                return Ok(());
              }
            }

            if is_ident_string_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Text(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else if (!matches!(occur, Occur::ZeroOrMore { .. }) && m.is_empty())
                || (matches!(occur, Occur::ZeroOrMore { .. }) && !m.is_empty())
              {
                self.add_error(format!("map requires entry key of type {}", ident));
              }

              return Ok(());
            }

            if is_ident_integer_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Integer(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else if (!matches!(occur, Occur::ZeroOrMore { .. }) && m.is_empty())
                || (matches!(occur, Occur::ZeroOrMore { .. }) && !m.is_empty())
              {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_bool_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Bool(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else if (!matches!(occur, Occur::ZeroOrMore { .. }) && m.is_empty())
                || (matches!(occur, Occur::ZeroOrMore { .. }) && !m.is_empty())
              {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_null_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Null)) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else if (!matches!(occur, Occur::ZeroOrMore { .. }) && m.is_empty())
                || (matches!(occur, Occur::ZeroOrMore { .. }) && !m.is_empty())
              {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_byte_string_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Bytes(_))) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else if (!matches!(occur, Occur::ZeroOrMore { .. }) && m.is_empty())
                || (matches!(occur, Occur::ZeroOrMore { .. }) && !m.is_empty())
              {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if is_ident_float_data_type(self.cddl, ident) && !self.validating_value {
              if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Null)) {
                self
                  .validated_keys
                  .get_or_insert(vec![k.clone()])
                  .push(k.clone());
                self.object_value = Some(v.clone());
                let _ = write!(self.cbor_location, "/{:?}", v);
              } else if (!matches!(occur, Occur::ZeroOrMore { .. }) && m.is_empty())
                || (matches!(occur, Occur::ZeroOrMore { .. }) && !m.is_empty())
              {
                self.add_error(format!("map requires entry key of type {}", ident));
              }
              return Ok(());
            }

            if token::lookup_ident(ident.ident)
              .in_standard_prelude()
              .is_some()
            {
              self.add_error(format!(
                "expected object value of type {}, got object",
                ident.ident
              ));
              return Ok(());
            }

            self.visit_value(&token::Value::TEXT(ident.ident.into()))
          }
        }
      }
      _ => {
        if let Some(cut_value) = self.cut_value.take() {
          self.add_error(format!(
            "cut present for member key {}. expected type {}, got {:?}",
            cut_value, ident, self.cbor
          ));
        } else {
          self.add_error(format!("expected type {}, got {:?}", ident, self.cbor));
        }
        Ok(())
      }
    }
  }

  fn visit_value_member_key_entry(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
  ) -> visitor::Result<Error<T>> {
    if let Some(occur) = &entry.occur {
      self.visit_occurrence(occur)?;
    }

    let current_location = self.cbor_location.clone();

    if let Some(mk) = &entry.member_key {
      let error_count = self.errors.len();
      self.is_member_key = true;
      self.visit_memberkey(mk)?;
      self.is_member_key = false;

      // Move to next entry if member key validation fails
      if self.errors.len() != error_count {
        self.advance_to_next_entry = true;
        return Ok(());
      }
    }

    if let Some(values) = &self.values_to_validate {
      for v in values.iter() {
        #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
        let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
        #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
        let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features);
        #[cfg(not(feature = "additional-controls"))]
        let mut cv = CBORValidator::new(self.cddl, v.clone());

        cv.generic_rules = self.generic_rules.clone();
        cv.eval_generic_rule = self.eval_generic_rule;
        cv.is_multi_type_choice = self.is_multi_type_choice;
        cv.is_multi_group_choice = self.is_multi_group_choice;
        cv.cbor_location.push_str(&self.cbor_location);
        cv.type_group_name_entry = self.type_group_name_entry;
        cv.validating_value = true;
        cv.visit_type(&entry.entry_type)?;

        self.cbor_location = current_location.clone();

        self.errors.append(&mut cv.errors);
        if entry.occur.is_some() {
          self.occurrence = None;
        }
      }

      return Ok(());
    }

    if let Some(v) = self.object_value.take() {
      #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
      let mut cv = CBORValidator::new(self.cddl, v, self.enabled_features.clone());
      #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
      let mut cv = CBORValidator::new(self.cddl, v, self.enabled_features);
      #[cfg(not(feature = "additional-controls"))]
      let mut cv = CBORValidator::new(self.cddl, v);

      cv.generic_rules = self.generic_rules.clone();
      cv.eval_generic_rule = self.eval_generic_rule;
      cv.is_multi_type_choice = self.is_multi_type_choice;
      cv.is_multi_group_choice = self.is_multi_group_choice;
      cv.cbor_location.push_str(&self.cbor_location);
      cv.type_group_name_entry = self.type_group_name_entry;
      cv.visit_type(&entry.entry_type)?;

      self.cbor_location = current_location;

      self.errors.append(&mut cv.errors);
      if entry.occur.is_some() {
        self.occurrence = None;
      }

      Ok(())
    } else if !self.advance_to_next_entry {
      self.visit_type(&entry.entry_type)
    } else {
      Ok(())
    }
  }

  fn visit_type_groupname_entry(
    &mut self,
    entry: &TypeGroupnameEntry<'a>,
  ) -> visitor::Result<Error<T>> {
    self.type_group_name_entry = Some(entry.name.ident);

    if let Some(ga) = &entry.generic_args {
      if let Some(rule) = rule_from_ident(self.cddl, &entry.name) {
        if let Some(gr) = self
          .generic_rules
          .iter_mut()
          .find(|gr| gr.name == entry.name.ident)
        {
          for arg in ga.args.iter() {
            gr.args.push((*arg.arg).clone());
          }
        } else if let Some(params) = generic_params_from_rule(rule) {
          self.generic_rules.push(GenericRule {
            name: entry.name.ident,
            params,
            args: ga.args.iter().cloned().map(|arg| *arg.arg).collect(),
          });
        }

        #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
        let mut cv =
          CBORValidator::new(self.cddl, self.cbor.clone(), self.enabled_features.clone());
        #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
        let mut cv = CBORValidator::new(self.cddl, self.cbor.clone(), self.enabled_features);
        #[cfg(not(feature = "additional-controls"))]
        let mut cv = CBORValidator::new(self.cddl, self.cbor.clone());

        cv.generic_rules = self.generic_rules.clone();
        cv.eval_generic_rule = Some(entry.name.ident);
        cv.is_multi_type_choice = self.is_multi_type_choice;
        cv.visit_rule(rule)?;

        self.errors.append(&mut cv.errors);

        return Ok(());
      }
    }

    let type_choice_alternates = type_choice_alternates_from_ident(self.cddl, &entry.name);
    if !type_choice_alternates.is_empty() {
      self.is_multi_type_choice = true;
    }

    let error_count = self.errors.len();
    for t in type_choice_alternates {
      let cur_errors = self.errors.len();
      self.visit_type(t)?;
      if self.errors.len() == cur_errors {
        for _ in 0..self.errors.len() - error_count {
          self.errors.pop();
        }

        return Ok(());
      }
    }

    let error_count = self.errors.len();
    let group_choice_alternates = group_choice_alternates_from_ident(self.cddl, &entry.name);
    if !group_choice_alternates.is_empty() {
      self.is_multi_group_choice = true;
    }

    for ge in group_choice_alternates {
      let cur_errors = self.errors.len();
      self.visit_group_entry(ge)?;
      if self.errors.len() == cur_errors {
        for _ in 0..self.errors.len() - error_count {
          self.errors.pop();
        }

        return Ok(());
      }
    }

    walk_type_groupname_entry(self, entry)?;
    self.type_group_name_entry = None;

    Ok(())
  }

  fn visit_memberkey(&mut self, mk: &MemberKey<'a>) -> visitor::Result<Error<T>> {
    match mk {
      MemberKey::Type1 { is_cut, .. } => {
        self.is_cut_present = *is_cut;
        walk_memberkey(self, mk)?;
        self.is_cut_present = false;
      }
      MemberKey::Bareword { .. } => {
        self.is_colon_shortcut_present = true;
        walk_memberkey(self, mk)?;
        self.is_colon_shortcut_present = false;
      }
      _ => return walk_memberkey(self, mk),
    }

    Ok(())
  }

  fn visit_value(&mut self, value: &token::Value<'a>) -> visitor::Result<Error<T>> {
    let error: Option<String> = match &self.cbor {
      Value::Integer(i) => match value {
        token::Value::INT(v) => match &self.ctrl {
          Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT)
            if i128::from(*i) != *v as i128 =>
          {
            None
          }
          Some(ControlOperator::LT) if i128::from(*i) < *v as i128 => None,
          Some(ControlOperator::LE) if i128::from(*i) <= *v as i128 => None,
          Some(ControlOperator::GT) if i128::from(*i) > *v as i128 => None,
          Some(ControlOperator::GE) if i128::from(*i) >= *v as i128 => None,
          #[cfg(feature = "additional-controls")]
          Some(ControlOperator::PLUS) => {
            if i128::from(*i) == *v as i128 {
              None
            } else {
              Some(format!("expected computed .plus value {}, got {:?}", v, i))
            }
          }
          #[cfg(feature = "additional-controls")]
          None | Some(ControlOperator::FEATURE) => {
            if i128::from(*i) == *v as i128 {
              None
            } else {
              Some(format!("expected value {}, got {:?}", v, i))
            }
          }
          #[cfg(not(feature = "additional-controls"))]
          None => {
            if i128::from(*i) == *v as i128 {
              None
            } else {
              Some(format!("expected value {}, got {:?}", v, i))
            }
          }
          _ => Some(format!(
            "expected value {} {}, got {:?}",
            self.ctrl.unwrap(),
            v,
            i
          )),
        },
        token::Value::UINT(v) => match &self.ctrl {
          Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT)
            if i128::from(*i) != *v as i128 =>
          {
            None
          }
          Some(ControlOperator::LT) if i128::from(*i) < *v as i128 => None,
          Some(ControlOperator::LE) if i128::from(*i) <= *v as i128 => None,
          Some(ControlOperator::GT) if i128::from(*i) > *v as i128 => None,
          Some(ControlOperator::GE) if i128::from(*i) >= *v as i128 => None,
          Some(ControlOperator::SIZE) => match 256i128.checked_pow(*v as u32) {
            Some(n) if i128::from(*i) < n => None,
            _ => Some(format!("expected value .size {}, got {:?}", v, i)),
          },
          Some(ControlOperator::BITS) => {
            if let Some(sv) = 1u32.checked_shl(*v as u32) {
              if (i128::from(*i) & sv as i128) != 0 {
                None
              } else {
                Some(format!("expected uint .bits {}, got {:?}", v, i))
              }
            } else {
              Some(format!("expected uint .bits {}, got {:?}", v, i))
            }
          }
          #[cfg(feature = "additional-controls")]
          Some(ControlOperator::PLUS) => {
            if i128::from(*i) == *v as i128 {
              None
            } else {
              Some(format!("expected computed .plus value {}, got {:?}", v, i))
            }
          }
          #[cfg(feature = "additional-controls")]
          None | Some(ControlOperator::FEATURE) => {
            if i128::from(*i) == *v as i128 {
              None
            } else {
              Some(format!("expected value {}, got {:?}", v, i))
            }
          }
          #[cfg(not(feature = "additional-controls"))]
          None => {
            if i128::from(*i) == *v as i128 {
              None
            } else {
              Some(format!("expected value {}, got {:?}", v, i))
            }
          }
          _ => Some(format!(
            "expected value {} {}, got {:?}",
            self.ctrl.unwrap(),
            v,
            i
          )),
        },

        _ => Some(format!("expected {}, got {:?}", value, i)),
      },
      Value::Float(f) => match value {
        token::Value::FLOAT(v) => match &self.ctrl {
          Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT)
            if (*f - *v).abs() > f64::EPSILON =>
          {
            None
          }
          Some(ControlOperator::LT) if *f < *v => None,
          Some(ControlOperator::LE) if *f <= *v => None,
          Some(ControlOperator::GT) if *f > *v => None,
          Some(ControlOperator::GE) if *f >= *v => None,
          #[cfg(feature = "additional-controls")]
          Some(ControlOperator::PLUS) => {
            if (*f - *v).abs() < f64::EPSILON {
              None
            } else {
              Some(format!("expected computed .plus value {}, got {:?}", v, f))
            }
          }
          #[cfg(feature = "additional-controls")]
          None | Some(ControlOperator::FEATURE) => {
            if (*f - *v).abs() < f64::EPSILON {
              None
            } else {
              Some(format!("expected value {}, got {:?}", v, f))
            }
          }
          #[cfg(not(feature = "additional-controls"))]
          None => {
            if (*f - *v).abs() < f64::EPSILON {
              None
            } else {
              Some(format!("expected value {}, got {:?}", v, f))
            }
          }
          _ => Some(format!(
            "expected value {} {}, got {:?}",
            self.ctrl.unwrap(),
            v,
            f
          )),
        },
        _ => Some(format!("expected {}, got {:?}", value, f)),
      },
      Value::Text(s) => match value {
        token::Value::TEXT(t) => match &self.ctrl {
          Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT) => {
            if s != t {
              None
            } else {
              Some(format!("expected {} .ne to \"{}\"", value, s))
            }
          }
          Some(ControlOperator::REGEXP) | Some(ControlOperator::PCRE) => {
            let re = regex::Regex::new(
              &format_regex(t)
                .ok_or_else(|| Error::from_validator(self, "malformed regex".to_string()))?,
            )
            .map_err(|e| Error::from_validator(self, e.to_string()))?;

            if re.is_match(s) {
              None
            } else {
              Some(format!("expected \"{}\" to match regex \"{}\"", s, t))
            }
          }
          #[cfg(feature = "additional-controls")]
          Some(ControlOperator::ABNF) => validate_abnf(t, s)
            .err()
            .map(|e| format!("\"{}\" is not valid against abnf: {}", s, e)),
          _ => {
            #[cfg(feature = "additional-controls")]
            if s == t {
              None
            } else if let Some(ControlOperator::CAT) | Some(ControlOperator::DET) = &self.ctrl {
              Some(format!(
                "expected value to match concatenated string {}, got \"{}\"",
                value, s
              ))
            } else if let Some(ctrl) = &self.ctrl {
              Some(format!("expected value {} {}, got \"{}\"", ctrl, value, s))
            } else {
              Some(format!("expected value {} got \"{}\"", value, s))
            }

            #[cfg(not(feature = "additional-controls"))]
            if s == t {
              None
            } else if let Some(ctrl) = &self.ctrl {
              Some(format!("expected value {} {}, got \"{}\"", ctrl, value, s))
            } else {
              Some(format!("expected value {} got \"{}\"", value, s))
            }
          }
        },
        token::Value::UINT(u) => match &self.ctrl {
          Some(ControlOperator::SIZE) => {
            if s.len() == *u {
              None
            } else {
              Some(format!("expected \"{}\" .size {}, got {}", s, u, s.len()))
            }
          }
          _ => Some(format!("expected {}, got {}", u, s)),
        },
        token::Value::BYTE(token::ByteValue::UTF8(b)) if s.as_bytes() == b.as_ref() => None,
        token::Value::BYTE(token::ByteValue::B16(b)) if s.as_bytes() == b.as_ref() => None,
        token::Value::BYTE(token::ByteValue::B64(b)) if s.as_bytes() == b.as_ref() => None,
        _ => Some(format!("expected {}, got \"{}\"", value, s)),
      },
      Value::Bytes(b) => match value {
        token::Value::UINT(v) => match &self.ctrl {
          Some(ControlOperator::SIZE) => {
            if let Some(range_upper) = self.range_upper.as_ref() {
              let len = b.len();
              if len < *v || len > *range_upper {
                Some(format!(
                  "expected bytes .size to be in range {} <= value <= {}, got {}",
                  v, range_upper, len
                ))
              } else {
                None
              }
            } else if b.len() == *v {
              None
            } else {
              Some(format!("expected \"{:?}\" .size {}, got {}", b, v, b.len()))
            }
          }
          Some(ControlOperator::BITS) => {
            if let Some(rsv) = v.checked_shr(3) {
              if let Some(s) = b.get(rsv) {
                if let Some(lsv) = 1u32.checked_shl(*v as u32 & 7) {
                  if (*s as u32 & lsv) != 0 {
                    None
                  } else {
                    Some(format!(
                      "expected value {} {}, got {:?}",
                      self.ctrl.unwrap(),
                      v,
                      b
                    ))
                  }
                } else {
                  Some(format!(
                    "expected value {} {}, got {:?}",
                    self.ctrl.unwrap(),
                    v,
                    b
                  ))
                }
              } else {
                Some(format!(
                  "expected value {} {}, got {:?}",
                  self.ctrl.unwrap(),
                  v,
                  b
                ))
              }
            } else {
              Some(format!(
                "expected value {} {}, got {:?}",
                self.ctrl.unwrap(),
                v,
                b
              ))
            }
          }
          _ => {
            if let Some(ctrl) = self.ctrl {
              Some(format!("expected value {} {}, got {:?}", ctrl, v, b))
            } else {
              Some(format!("expected value {}, got {:?}", v, b))
            }
          }
        },
        #[cfg(feature = "additional-controls")]
        token::Value::TEXT(t) => match &self.ctrl {
          Some(ControlOperator::ABNFB) => {
            validate_abnf(t, std::str::from_utf8(b).map_err(Error::UTF8Parsing)?)
              .err()
              .map(|e| {
                format!(
                  "cbor bytes \"{:?}\" are not valid against abnf {}: {}",
                  b, t, e
                )
              })
          }
          _ => Some(format!(
            "expected value {} {}, got {:?}",
            self.ctrl.unwrap(),
            t,
            b
          )),
        },
        #[cfg(feature = "additional-controls")]
        token::Value::BYTE(bv) => match &self.ctrl {
          Some(ControlOperator::ABNFB) => match bv {
            ByteValue::UTF8(utf8bv) => validate_abnf(
              std::str::from_utf8(utf8bv).map_err(Error::UTF8Parsing)?,
              std::str::from_utf8(b).map_err(Error::UTF8Parsing)?,
            )
            .err()
            .map(|e| {
              format!(
                "cbor bytes \"{:?}\" are not valid against abnf {}: {}",
                b, bv, e
              )
            }),
            ByteValue::B16(b16bv) => validate_abnf(
              std::str::from_utf8(&base16::decode(b16bv).map_err(Error::Base16Decoding)?)
                .map_err(Error::UTF8Parsing)?,
              std::str::from_utf8(b).map_err(Error::UTF8Parsing)?,
            )
            .err()
            .map(|e| {
              format!(
                "cbor bytes \"{:?}\" are not valid against abnf {}: {}",
                b, bv, e
              )
            }),
            ByteValue::B64(b64bv) => validate_abnf(
              std::str::from_utf8(
                &data_encoding::BASE64URL
                  .decode(b64bv)
                  .map_err(Error::Base64Decoding)?,
              )
              .map_err(Error::UTF8Parsing)?,
              std::str::from_utf8(b).map_err(Error::UTF8Parsing)?,
            )
            .err()
            .map(|e| {
              format!(
                "cbor bytes \"{:?}\" are not valid against abnf {}: {}",
                b, bv, e
              )
            }),
          },
          _ => Some(format!(
            "expected value {} {}, got {:?}",
            self.ctrl.unwrap(),
            bv,
            b
          )),
        },
        _ => Some(format!("expected {}, got {:?}", value, b)),
      },
      Value::Array(_) => {
        self.validate_array_items(&ArrayItemToken::Value(value))?;

        None
      }
      Value::Map(o) => {
        if self.is_cut_present {
          self.cut_value = Some(Type1::from(value.clone()));
        }

        if let token::Value::TEXT(Cow::Borrowed("any")) = value {
          return Ok(());
        }

        // Retrieve the value from key unless optional/zero or more, in which
        // case advance to next group entry
        let k = token_value_into_cbor_value(value.clone());

        #[cfg(feature = "ast-span")]
        if let Some(v) = o
          .iter()
          .find_map(|entry| if entry.0 == k { Some(&entry.1) } else { None })
        {
          self.validated_keys.get_or_insert(vec![k.clone()]).push(k);
          self.object_value = Some(v.clone());
          let _ = write!(self.cbor_location, "/{}", value);

          None
        } else if let Some(Occur::Optional { .. }) | Some(Occur::ZeroOrMore { .. }) =
          &self.occurrence.take()
        {
          self.advance_to_next_entry = true;
          None
        } else if let Some(Occur::Exact {
          lower: None,
          upper: None,
          ..
        }) = &self.occurrence.take()
        {
          // Handle Exact { lower: None, upper: None } as zero-or-more (for backward compatibility)
          self.advance_to_next_entry = true;
          None
        } else if let Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT) = &self.ctrl {
          None
        } else {
          Some(format!("object missing key: {}", value))
        }

        #[cfg(not(feature = "ast-span"))]
        if let Some(v) = o
          .iter()
          .find_map(|entry| if entry.0 == k { Some(&entry.1) } else { None })
        {
          self.validated_keys.get_or_insert(vec![k.clone()]).push(k);
          self.object_value = Some(v.clone());
          self.cbor_location.push_str(&format!("/{}", value));

          None
        } else if let Some(Occur::Optional {}) | Some(Occur::ZeroOrMore {}) =
          &self.occurrence.take()
        {
          self.advance_to_next_entry = true;
          None
        } else if let Some(Token::NE) | Some(Token::DEFAULT) = &self.ctrl {
          None
        } else {
          Some(format!("object missing key: {}", value))
        }
      }
      _ => Some(format!("expected {}, got {:?}", value, self.cbor)),
    };

    if let Some(e) = error {
      self.add_error(e);
    }

    Ok(())
  }

  fn visit_occurrence(&mut self, o: &Occurrence<'a>) -> visitor::Result<Error<T>> {
    self.occurrence = Some(o.occur);

    Ok(())
  }
}

/// Converts a CDDL value type to ciborium::value::Value
pub fn token_value_into_cbor_value(value: token::Value) -> ciborium::value::Value {
  match value {
    token::Value::UINT(i) => ciborium::value::Value::Integer(i.into()),
    token::Value::INT(i) => ciborium::value::Value::Integer(i.into()),
    token::Value::FLOAT(f) => ciborium::value::Value::Float(f),
    token::Value::TEXT(t) => ciborium::value::Value::Text(t.to_string()),
    token::Value::BYTE(b) => match b {
      ByteValue::UTF8(b) | ByteValue::B16(b) | ByteValue::B64(b) => {
        ciborium::value::Value::Bytes(b.into_owned())
      }
    },
  }
}

#[cfg(test)]
#[cfg(not(target_arch = "wasm32"))]
mod tests {
  use super::*;
  use ciborium::cbor;
  use indoc::indoc;

  #[cfg(not(feature = "additional-controls"))]
  #[test]
  fn validate() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        tcpflagbytes = bstr .bits flags
        flags = &(
          fin: 8,
          syn: 9,
          rst: 10,
          psh: 11,
          ack: 12,
          urg: 13,
          ece: 14,
          cwr: 15,
          ns: 0,
        ) / (4..7) ; data offset bits
      "#
    );

    let cbor = ciborium::value::Value::Bytes(vec![0x90, 0x6d]);

    let cddl = crate::cddl_from_str(cddl, true)?;

    let mut cv = CBORValidator::new(&cddl, cbor);
    cv.validate()?;

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn validate_abnfb_1() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        oid = bytes .abnfb ("oid" .det cbor-tags-oid)
        roid = bytes .abnfb ("roid" .det cbor-tags-oid)
 
        cbor-tags-oid = '
          oid = 1*arc
          roid = *arc
          arc = [nlsb] %x00-7f
          nlsb = %x81-ff *%x80-ff
        '
      "#
    );

    let sha256_oid = "2.16.840.1.101.3.4.2.1";

    let cbor = ciborium::value::Value::Bytes(sha256_oid.as_bytes().to_vec());

    let cddl = cddl_from_str(cddl, true)?;

    let mut cv = CBORValidator::new(&cddl, cbor, None);
    cv.validate()?;

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn validate_feature() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        v = JC<"v", 2>
        JC<J, C> = J .feature "json" / C .feature "cbor"
      "#
    );

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let cbor = ciborium::value::Value::Integer(2.into());

    let cddl = cddl.unwrap();

    let mut cv = CBORValidator::new(&cddl, cbor, Some(&["cbor"]));
    cv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_type_choice_alternate() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        tester = [ $vals ]
        $vals /= 12
        $vals /= 13
      "#
    );

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let cbor = ciborium::cbor!([13]).unwrap();

    let cddl = cddl.unwrap();

    let mut cv = CBORValidator::new(&cddl, cbor, None);
    cv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_group_choice_alternate_in_array(
  ) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        tester = [$$val]
        $$val //= (
          type: 10,
          data: uint
        )
        $$val //= (
          type: 11,
          data: tstr
        )
      "#
    );

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let cbor = ciborium::cbor!([11, "test"]).unwrap();

    let cddl = cddl.unwrap();

    let mut cv = CBORValidator::new(&cddl, cbor, None);
    cv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_tdate_tag() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        root = time
      "#
    );

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let cbor = ciborium::value::Value::Tag(
      1,
      Box::from(ciborium::value::Value::Float(1680965875.01_f64)),
    );

    let cddl = cddl.unwrap();

    let mut cv = CBORValidator::new(&cddl, cbor, None);
    cv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_abnfb_2() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        ; Binary ABNF Test Schema
        test_cbor = {
          61285: sub_map
        }
        
        sub_map = {
          1: signature_abnf
        }
        
        signature = bytes .size 64
        
        signature_abnf = signature .abnfb '     
        ANYDATA
        ANYDATA = *OCTET

        OCTET =  %x00-FF
        '
      "#
    );

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let cbor = ciborium::value::Value::Map(vec![(
      ciborium::value::Value::Integer(61285.into()),
      ciborium::value::Value::Map(vec![(
        ciborium::value::Value::Integer(1.into()),
        ciborium::value::Value::Bytes(b"test".to_vec()),
      )]),
    )]);

    let cddl = cddl.unwrap();

    let mut cv = CBORValidator::new(&cddl, cbor, None);
    cv.validate()?;

    Ok(())
  }

  #[test]
  fn multi_type_choice_type_rule_array_validation(
  ) -> std::result::Result<(), Box<dyn std::error::Error>> {
    use ciborium::value::Value;

    let cddl = indoc!(
      r#"
        Ref = nil / refShort / refFull

        blobSize = uint
        hashID = uint .lt 23
        hashName = text
        hashDigest = bytes
        
        refShort = [ blobSize, hashID, hashDigest ]
        refFull = { 1: blobSize, 2: hashName, 3: hashDigest }
      "#
    );

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let cbor = Value::Array(vec![
      Value::Integer(3.into()),
      Value::Integer(2.into()),
      Value::Bytes(
        base16::decode("BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD").unwrap(),
      ),
    ]);

    let cddl = cddl.unwrap();

    let mut cv = CBORValidator::new(&cddl, cbor, None);
    cv.validate()?;

    Ok(())
  }

  #[test]
  fn tagged_data_in_array_validation() -> std::result::Result<(), Box<dyn std::error::Error>> {
    use ciborium::value::Value;

    let cddl = indoc!(
      r#"
        start = [ * help ]

        help = #6.123(bstr)
      "#
    );

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let cbor = Value::Array(vec![Value::Tag(
      123,
      Box::from(Value::Bytes(base16::decode("00").unwrap())),
    )]);

    let cddl = cddl.unwrap();

    let mut cv = CBORValidator::new(&cddl, cbor, None);
    cv.validate()?;

    Ok(())
  }

  #[test]
  fn test_conditional_array_validation() {
    let cddl_str = r#"
        NestedPart = [
          disposition: 0,
          language: tstr,
          partIndex: uint,
          ( NullPart // SinglePart )
        ]

        NullPart = ( cardinality: 0 )
        SinglePart = (
            cardinality: 1,
            contentType: tstr,
            content: bstr
        )
    "#;

    let cddl = cddl_from_str(cddl_str, true).unwrap();

    // Test data: A SinglePart with 6 elements (disposition, language, partIndex, cardinality, contentType, content)
    let cbor_data = Value::Array(vec![
      Value::Integer(0.into()),              // disposition
      Value::Text("en".to_string()),         // language: tstr
      Value::Integer(1.into()),              // partIndex: uint
      Value::Integer(1.into()),              // cardinality: single (1)
      Value::Text("text/plain".to_string()), // contentType: tstr
      Value::Bytes(b"hello world".to_vec()), // content: bstr
    ]);

    #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
    let mut validator = CBORValidator::new(&cddl, cbor_data, None);
    #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
    let mut validator = CBORValidator::new(&cddl, cbor_data, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut validator = CBORValidator::new(&cddl, cbor_data);
    let result = validator.validate();

    // This should pass but currently fails
    assert!(
      result.is_ok(),
      "Validation should succeed for SinglePart structure: {:?}",
      result
    );
  }

  #[test]
  fn extract_cbor() {
    use ciborium::value::Value;

    let cbor = Value::Float(1.23);
    let cddl = cddl_from_str("start = any", true).unwrap();
    let cv = CBORValidator::new(&cddl, cbor, None);
    assert_eq!(cv.extract_cbor(), Value::Float(1.23));
  }

  #[test]
  fn validate_bstr_size_range() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        m = { field: bstr .size (16..1000) }
        "#
    );

    let cddl = cddl_from_str(cddl, true)?;

    // Test valid byte string length
    let valid_bytes = vec![0u8; 100];
    let valid_cbor = ciborium::value::Value::Map(vec![(
      ciborium::value::Value::Text("field".to_string()),
      ciborium::value::Value::Bytes(valid_bytes),
    )]);

    #[cfg(feature = "additional-controls")]
    let mut cv = CBORValidator::new(&cddl, valid_cbor, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut cv = CBORValidator::new(&cddl, valid_cbor);
    assert!(cv.validate().is_ok());

    // Test byte string that's too short
    let short_bytes = vec![0u8; 10];
    let short_cbor = ciborium::value::Value::Map(vec![(
      ciborium::value::Value::Text("field".to_string()),
      ciborium::value::Value::Bytes(short_bytes),
    )]);

    #[cfg(feature = "additional-controls")]
    let mut cv = CBORValidator::new(&cddl, short_cbor, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut cv = CBORValidator::new(&cddl, short_cbor);
    assert!(cv.validate().is_err());

    // Test byte string that's too long
    let long_bytes = vec![0u8; 1500];
    let long_cbor = ciborium::value::Value::Map(vec![(
      ciborium::value::Value::Text("field".to_string()),
      ciborium::value::Value::Bytes(long_bytes),
    )]);

    #[cfg(feature = "additional-controls")]
    let mut cv = CBORValidator::new(&cddl, long_cbor, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut cv = CBORValidator::new(&cddl, long_cbor);
    assert!(cv.validate().is_err());

    Ok(())
  }

  #[test]
  fn validate_bstr_size_exclusive_range() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        m = { field: bstr .size (16...1000) }
      "#
    );

    let cddl = cddl_from_str(cddl, true)?;

    // Test valid byte string length (17 bytes - should pass)
    let valid_bytes = vec![0u8; 17];
    let valid_cbor = ciborium::value::Value::Map(vec![(
      ciborium::value::Value::Text("field".to_string()),
      ciborium::value::Value::Bytes(valid_bytes),
    )]);

    #[cfg(feature = "additional-controls")]
    let mut cv = CBORValidator::new(&cddl, valid_cbor, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut cv = CBORValidator::new(&cddl, valid_cbor);
    assert!(cv.validate().is_ok());

    // Test boundary case (16 bytes - should pass)
    let boundary_bytes = vec![0u8; 16];
    let boundary_cbor = ciborium::value::Value::Map(vec![(
      ciborium::value::Value::Text("field".to_string()),
      ciborium::value::Value::Bytes(boundary_bytes),
    )]);

    #[cfg(feature = "additional-controls")]
    let mut cv = CBORValidator::new(&cddl, boundary_cbor, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut cv = CBORValidator::new(&cddl, boundary_cbor);
    assert!(cv.validate().is_ok());

    Ok(())
  }

  #[test]
  fn validate_nested_cbor() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        root = {
            foo: bstr .cbor bar
        }

        bar = {
            a: text,
            b: int,
            c: bstr
        }
        "#
    );

    let cddl = cddl_from_str(cddl, true)?;

    // Create valid inner CBOR map
    let inner_cbor = ciborium::cbor!({
        "a" => "test",
        "b" => -42,
        "c" => Value::Bytes(b"bytes".to_vec())
    })?;

    // Serialize inner CBOR to bytes
    let mut inner_bytes = Vec::new();
    ciborium::ser::into_writer(&inner_cbor, &mut inner_bytes)?;

    // Create outer CBOR map containing the bytes
    let outer_cbor = Value::Map(vec![(
      Value::Text("foo".to_string()),
      Value::Bytes(inner_bytes),
    )]);

    // Test valid case
    #[cfg(feature = "additional-controls")]
    let mut cv = CBORValidator::new(&cddl, outer_cbor.clone(), None);
    #[cfg(not(feature = "additional-controls"))]
    let mut cv = CBORValidator::new(&cddl, outer_cbor.clone());
    assert!(cv.validate().is_ok());

    // Test invalid inner CBOR (missing required field)
    let invalid_inner = ciborium::cbor!({
        "a" => "test",
        "b" => -42
        // missing "c" field
    })?;

    let mut invalid_bytes = Vec::new();
    ciborium::ser::into_writer(&invalid_inner, &mut invalid_bytes)?;

    let invalid_outer = Value::Map(vec![(
      Value::Text("foo".to_string()),
      Value::Bytes(invalid_bytes),
    )]);

    #[cfg(feature = "additional-controls")]
    let mut cv = CBORValidator::new(&cddl, invalid_outer, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut cv = CBORValidator::new(&cddl, invalid_outer);
    assert!(cv.validate().is_err());

    Ok(())
  }

  #[test]
  fn validate_nested_cbor_in_array() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        root = [
            foo: bstr .cbor bar
        ]

        bar = {
            a: text,
            b: int,
            c: bstr
        }
        "#
    );

    let cddl = cddl_from_str(cddl, true)?;

    // Create valid inner CBOR map
    let inner_cbor = ciborium::cbor!({
        "a" => "test",
        "b" => -42,
        "c" => Value::Bytes(b"bytes".to_vec())
    })?;

    // Serialize inner CBOR to bytes
    let mut inner_bytes = Vec::new();
    ciborium::ser::into_writer(&inner_cbor, &mut inner_bytes)?;

    // Create outer CBOR array containing the bytes
    let outer_cbor = Value::Array(vec![Value::Bytes(inner_bytes)]);

    // Test valid case
    #[cfg(feature = "additional-controls")]
    let mut cv = CBORValidator::new(&cddl, outer_cbor.clone(), None);
    #[cfg(not(feature = "additional-controls"))]
    let mut cv = CBORValidator::new(&cddl, outer_cbor.clone());
    cv.validate()?;
    assert!(
      cv.validate().is_ok(),
      // "Nested array with wildcard should validate successfully"
    );

    // Test invalid inner CBOR (missing required field)
    let invalid_inner = ciborium::cbor!({
        "a" => "test",
        "b" => -42
        // missing "c" field
    })?;

    let mut invalid_bytes = Vec::new();
    ciborium::ser::into_writer(&invalid_inner, &mut invalid_bytes)?;

    let invalid_outer = Value::Array(vec![Value::Bytes(invalid_bytes)]);

    #[cfg(feature = "additional-controls")]
    let mut cv = CBORValidator::new(&cddl, invalid_outer, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut cv = CBORValidator::new(&cddl, invalid_outer);
    assert!(cv.validate().is_err());

    Ok(())
  }

  #[test]
  fn validate_nested_arrays() -> std::result::Result<(), Box<dyn std::error::Error>> {
    // Test with explicit array type
    let cddl = indoc!(
      r#"
        array = [0, [* int]]
      "#
    );

    let cbor = ciborium::cbor!([0, [1, 2]]).unwrap();
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;

    let mut cv = CBORValidator::new(&cddl, cbor, None);
    cv.validate()?;

    // Test with named arrays
    let cddl = indoc!(
      r#"
        root = [0, inner]
        inner = [* int]
      "#
    );

    let cbor = ciborium::cbor!([0, [1, 2]]).unwrap();
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;

    let mut cv = CBORValidator::new(&cddl, cbor, None);
    cv.validate()?;

    // Test with explicit array literals
    let cddl = indoc!(
      r#"
        direct = [1, [2, 3]]
      "#
    );

    let cbor = ciborium::cbor!([1, [2, 3]]).unwrap();
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;

    let mut cv = CBORValidator::new(&cddl, cbor, None);
    cv.validate()?; // If this passes, our fix works

    Ok(())
  }

  #[test]
  fn validate_direct_nested_array() -> std::result::Result<(), Box<dyn std::error::Error>> {
    // Simplest possible case with explicit literal array
    let cddl = indoc!(
      r#"
        direct = [1, [2, 3]]
        "#
    );

    let cbor = ciborium::cbor!([1, [2, 3]]).unwrap();
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;

    let mut cv = CBORValidator::new(&cddl, cbor, None);

    // Print detailed information for debugging
    match cv.validate() {
      Ok(_) => println!("Validation successful!"),
      Err(e) => {
        eprintln!("Validation error: {}", e);
        if let Error::Validation(errors) = &e {
          for (i, err) in errors.iter().enumerate() {
            eprintln!("Error {}: {} at {}", i, err.reason, err.cbor_location);
          }
        }
        return Err(e.into());
      }
    }

    Ok(())
  }

  #[test]
  fn validate_recursive_structures() -> std::result::Result<(), Box<dyn std::error::Error>> {
    // Test case for issue #248: stack overflow with recursive structures
    let cddl = indoc!(
      r#"
        Tree = {
          root: Node
        }

        Node = [
          value: text,
          children: [* Node]
        ]
      "#
    );

    let cddl = cddl_from_str(cddl, true)?;

    // Create a nested tree structure
    let cbor = ciborium::cbor!({
      "root" => ["value", [["child1", []], ["child2", []]]]
    })?;

    // This should not cause a stack overflow
    #[cfg(feature = "additional-controls")]
    let mut cv = CBORValidator::new(&cddl, cbor, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut cv = CBORValidator::new(&cddl, cbor);

    // The validation should complete without stack overflow
    let result = cv.validate();

    // The structure should be valid
    assert!(
      result.is_ok(),
      "Recursive structure validation should not cause stack overflow"
    );

    Ok(())
  }

  #[test]
  fn test_issue_221_empty_map_with_extra_keys_cbor() {
    // This test reproduces issue #221 for CBOR
    // CDDL schema defines an empty map: root = {}
    // CBOR has extra keys
    // This should FAIL validation but currently passes

    let cddl_str = "root = {}";
    let cddl = cddl_from_str(cddl_str, true).unwrap();

    // Create a CBOR map with extra keys
    use ciborium::Value;
    let cbor_data = Value::Map(vec![(
      Value::Text("x".to_string()),
      Value::Text("y".to_string()),
    )]);

    #[cfg(feature = "additional-controls")]
    let mut validator = CBORValidator::new(&cddl, cbor_data, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut validator = CBORValidator::new(&cddl, cbor_data);

    // This should fail but currently passes (the bug)
    let result = validator.validate();
    assert!(
      result.is_err(),
      "CBOR validation should fail for extra keys in empty map schema"
    );

    // Verify the error message is what we expect
    if let Err(Error::Validation(errors)) = result {
      assert_eq!(errors.len(), 1);
      assert!(errors[0].reason.contains("expected empty map"));
    } else {
      panic!("Expected validation error but got something else");
    }
  }

  #[test]
  fn test_empty_map_schema_with_empty_cbor() -> std::result::Result<(), Box<dyn std::error::Error>>
  {
    // This should pass - empty map schema with empty CBOR map
    let cddl_str = "root = {}";
    let cddl = cddl_from_str(cddl_str, true)?;

    // Create an empty CBOR map
    use ciborium::Value;
    let cbor_data = Value::Map(vec![]);

    #[cfg(feature = "additional-controls")]
    let mut validator = CBORValidator::new(&cddl, cbor_data, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut validator = CBORValidator::new(&cddl, cbor_data);

    let result = validator.validate();
    assert!(
      result.is_ok(),
      "CBOR validation should pass for empty map with empty map schema"
    );

    Ok(())
  }

  #[test]
  fn test_issue_221_reproduce_exact_scenario_cbor(
  ) -> std::result::Result<(), Box<dyn std::error::Error>> {
    // This test reproduces the exact scenario from issue #221 for CBOR
    let cddl_str = "root = {}";
    let cddl = cddl_from_str(cddl_str, true)?;

    // Create CBOR equivalent of {"x": "y"}
    use ciborium::Value;
    let cbor_data = Value::Map(vec![(
      Value::Text("x".to_string()),
      Value::Text("y".to_string()),
    )]);

    #[cfg(feature = "additional-controls")]
    let mut validator = CBORValidator::new(&cddl, cbor_data, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut validator = CBORValidator::new(&cddl, cbor_data);

    let result = validator.validate();

    // Before the fix, this would incorrectly pass. After the fix, it should fail.
    match result {
      Err(Error::Validation(errors)) => {
        assert!(!errors.is_empty(), "Should have validation errors");
        let error_message = &errors[0].reason;
        assert!(
          error_message.contains("expected empty map"),
          "Error message should indicate expected empty map, got: {}",
          error_message
        );
      }
      Ok(_) => panic!(
        "Issue #221 bug detected: CBOR validation incorrectly passed for extra keys in empty map"
      ),
      Err(other) => panic!("Unexpected error type: {:?}", other),
    }

    Ok(())
  }
}
