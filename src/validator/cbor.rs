#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(feature = "lsp"))]

use super::*;
use crate::{
  ast::*,
  token::{self, Token},
  visitor::{self, *},
};

use std::{borrow::Cow, collections::HashMap, convert::TryFrom, fmt};

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
}

impl<T: std::fmt::Debug> fmt::Display for Error<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Error::Validation(errors) => {
        let mut error_str = String::new();
        for e in errors.iter() {
          error_str.push_str(&format!("{}\n", e));
        }
        write!(f, "{}", error_str)
      }
      Error::CBORParsing(error) => write!(f, "error parsing cbor: {}", error),
      Error::JSONParsing(error) => write!(f, "error parsing json string: {}", error),
      Error::CDDLParsing(error) => write!(f, "error parsing CDDL: {}", error),
      Error::UTF8Parsing(error) => write!(f, "error pasing utf8: {}", error),
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
      error_str.push_str(&format!(" group entry associated with rule \"{}\"", entry));
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
  ctrl: Option<token::Token<'a>>,
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
      enabled_features,
      has_feature_errors: false,
      disabled_features: None,
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
      enabled_features,
      has_feature_errors: false,
      disabled_features: None,
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
    }
  }

  /// Validate
  pub fn validate<T: std::fmt::Debug + 'static>(&mut self) -> std::result::Result<(), Error<T>>
  where
    cbor::Error<T>: From<cbor::Error<std::io::Error>>,
  {
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

impl<'a, T: std::fmt::Debug + 'static> Visitor<'a, Error<T>> for CBORValidator<'a>
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
          args: vec![],
        });
      }
    }

    let error_count = self.errors.len();

    for t in type_choice_alternates_from_ident(self.cddl, &tr.name) {
      let cur_errors = self.errors.len();
      self.visit_type(t)?;
      if self.errors.len() == cur_errors {
        for _ in 0..self.errors.len() - error_count {
          self.errors.pop();
        }

        return Ok(());
      }
    }

    Ok(())
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
          args: vec![],
        });
      }
    }

    let error_count = self.errors.len();

    for ge in group_choice_alternates_from_ident(self.cddl, &gr.name) {
      let cur_errors = self.errors.len();
      self.visit_group_entry(ge)?;
      if self.errors.len() == cur_errors {
        for _ in 0..self.errors.len() - error_count {
          self.errors.pop();
        }

        return Ok(());
      }
    }

    Ok(())
  }

  fn visit_type(&mut self, t: &Type<'a>) -> visitor::Result<Error<T>> {
    if t.type_choices.len() > 1 {
      self.is_multi_type_choice = true;
    }

    let initial_error_count = self.errors.len();
    for type_choice in t.type_choices.iter() {
      // If validating an array whose elements are type choices (i.e. [ 1* tstr
      // / integer ]), collect all errors and filter after the fact
      if matches!(self.cbor, Value::Array(_)) {
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
        }

        continue;
      }

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

        return Ok(());
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

        return Ok(());
      }
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
          let mut entry_counts = Vec::new();
          for gc in g.group_choices.iter() {
            let count = entry_counts_from_group_choice(self.cddl, gc);
            entry_counts.push(count);
          }
          let len = m.len();
          if let Token::EQ | Token::NE = t {
            if !validate_entry_count(&entry_counts, len) {
              for ec in entry_counts.iter() {
                if let Some(occur) = &ec.entry_occurrence {
                  self.add_error(format!(
                    "expecting array with length per occurrence {}",
                    occur,
                  ));
                } else {
                  self.add_error(format!(
                    "expecting array with length {}, got {}",
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
    if let Value::Array(a) = &self.cbor {
      match validate_array_occurrence(
        self.occurrence.as_ref().take(),
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
              let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
              #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
              let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features);
              #[cfg(not(feature = "additional-controls"))]
              let mut cv = CBORValidator::new(self.cddl, v.clone());

              cv.generic_rules = self.generic_rules.clone();
              cv.eval_generic_rule = self.eval_generic_rule;
              cv.ctrl = self.ctrl.clone();
              cv.is_multi_type_choice = self.is_multi_type_choice;
              cv.cbor_location
                .push_str(&format!("{}/{}", self.cbor_location, idx));

              cv.visit_range(lower, upper, is_inclusive)?;

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
          } else if let Some(idx) = self.group_entry_idx.take() {
            if let Some(v) = a.get(idx) {
              #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
              let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
              #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
              let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features);
              #[cfg(not(feature = "additional-controls"))]
              let mut cv = CBORValidator::new(self.cddl, v.clone());

              cv.generic_rules = self.generic_rules.clone();
              cv.eval_generic_rule = self.eval_generic_rule;
              cv.is_multi_type_choice = self.is_multi_type_choice;
              cv.ctrl = self.ctrl.clone();
              cv.cbor_location
                .push_str(&format!("{}/{}", self.cbor_location, idx));

              cv.visit_range(lower, upper, is_inclusive)?;

              self.errors.append(&mut cv.errors);
            } else if !allow_empty_array {
              self.add_error(format!("expected array item at index {}", idx));
            }
          } else {
            self.add_error(format!(
              "expected range lower {} upper {} inclusive {}, got {:?}",
              lower, upper, is_inclusive, self.cbor
            ));
          }
        }
        Err(errors) => {
          for e in errors.into_iter() {
            self.add_error(e);
          }
        }
      }

      return Ok(());
    }

    match lower {
      Type2::IntValue { value: l, .. } => match upper {
        Type2::IntValue { value: u, .. } => {
          let error_str = if is_inclusive {
            format!(
              "expected integer to be in range {} <= value <= {}, got {:?}",
              l, u, self.cbor
            )
          } else {
            format!(
              "expected integer to be in range {} < value < {}, got {:?}",
              l, u, self.cbor
            )
          };

          match &self.cbor {
            Value::Integer(i) => {
              if is_inclusive {
                if i128::from(*i) < *l as i128 || i128::from(*i) > *u as i128 {
                  self.add_error(error_str);
                } else {
                  return Ok(());
                }
              } else if i128::from(*i) <= *l as i128 || i128::from(*i) >= *u as i128 {
                self.add_error(error_str);
                return Ok(());
              } else {
                return Ok(());
              }
            }
            _ => {
              self.add_error(error_str);
              return Ok(());
            }
          }
        }
        Type2::UintValue { value: u, .. } => {
          let error_str = if is_inclusive {
            format!(
              "expected integer to be in range {} <= value <= {}, got {:?}",
              l, u, self.cbor
            )
          } else {
            format!(
              "expected integer to be in range {} < value < {}, got {:?}",
              l, u, self.cbor
            )
          };

          match &self.cbor {
            Value::Integer(i) => {
              if is_inclusive {
                if i128::from(*i) < *l as i128 || i128::from(*i) > *u as i128 {
                  self.add_error(error_str);
                } else {
                  return Ok(());
                }
              } else if i128::from(*i) <= *l as i128 || i128::from(*i) >= *u as i128 {
                self.add_error(error_str);
                return Ok(());
              } else {
                return Ok(());
              }
            }
            _ => {
              self.add_error(error_str);
              return Ok(());
            }
          }
        }
        _ => {
          self.add_error(format!(
            "invalid cddl range. upper value must be an integer type. got {}",
            upper
          ));
          return Ok(());
        }
      },
      Type2::UintValue { value: l, .. } => match upper {
        Type2::UintValue { value: u, .. } => {
          let error_str = if is_inclusive {
            format!(
              "expected uint to be in range {} <= value <= {}, got {:?}",
              l, u, self.cbor
            )
          } else {
            format!(
              "expected uint to be in range {} < value < {}, got {:?}",
              l, u, self.cbor
            )
          };

          match &self.cbor {
            Value::Integer(i) => {
              if is_inclusive {
                if i128::from(*i) < *l as i128 || i128::from(*i) > *u as i128 {
                  self.add_error(error_str);
                } else {
                  return Ok(());
                }
              } else if i128::from(*i) <= *l as i128 || i128::from(*i) >= *u as i128 {
                self.add_error(error_str);
                return Ok(());
              } else {
                return Ok(());
              }
            }
            Value::Text(s) => match self.ctrl {
              Some(Token::SIZE) => {
                let len = s.len();
                let s = s.clone();
                if is_inclusive {
                  if s.len() < *l || s.len() > *u {
                    self.add_error(format!(
                      "expected \"{}\" string length to be in the range {} <= value <= {}, got {}",
                      s, l, u, len
                    ));
                  }

                  return Ok(());
                } else if s.len() <= *l || s.len() >= *u {
                  self.add_error(format!(
                    "expected \"{}\" string length to be in the range {} < value < {}, got {}",
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
            _ => {
              self.add_error(error_str);
              return Ok(());
            }
          }
        }
        _ => {
          self.add_error(format!(
            "invalid cddl range. upper value must be a uint type. got {}",
            upper
          ));
          return Ok(());
        }
      },
      Type2::FloatValue { value: l, .. } => match upper {
        Type2::FloatValue { value: u, .. } => {
          let error_str = if is_inclusive {
            format!(
              "expected float to be in range {} <= value <= {}, got {:?}",
              l, u, self.cbor
            )
          } else {
            format!(
              "expected float to be in range {} < value < {}, got {:?}",
              l, u, self.cbor
            )
          };

          match &self.cbor {
            Value::Float(f) => {
              if is_inclusive {
                if *f < *l || *f > *u {
                  self.add_error(error_str);
                } else {
                  return Ok(());
                }
              } else if *f <= *l || *f >= *u {
                self.add_error(error_str);
                return Ok(());
              } else {
                return Ok(());
              }
            }
            _ => {
              self.add_error(error_str);
              return Ok(());
            }
          }
        }
        _ => {
          self.add_error(format!(
            "invalid cddl range. upper value must be a float type. got {}",
            upper
          ));
          return Ok(());
        }
      },
      _ => {
        self.add_error(
          "invalid cddl range. upper and lower values must be either integers or floats"
            .to_string(),
        );

        return Ok(());
      }
    }

    Ok(())
  }

  fn visit_control_operator(
    &mut self,
    target: &Type2<'a>,
    ctrl: &str,
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
            .cloned()
            .find(|gr| gr.name == name)
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
          .cloned()
          .find(|gr| gr.name == name)
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

    match lookup_control_from_str(ctrl) {
      t @ Some(Token::EQ) => {
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
              let mut entry_counts = Vec::new();
              for gc in group.group_choices.iter() {
                let count = entry_counts_from_group_choice(self.cddl, gc);
                entry_counts.push(count);
              }
              self.entry_counts = Some(entry_counts);
              self.visit_type2(controller)?;
              self.entry_counts = None;
              return Ok(());
            }
          }
          Type2::Map { .. } => {
            if let Value::Map(_) = &self.cbor {
              self.ctrl = t;
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
      t @ Some(Token::NE) => {
        match target {
          Type2::Typename { ident, .. } => {
            if is_ident_string_data_type(self.cddl, ident)
              || is_ident_numeric_data_type(self.cddl, ident)
            {
              self.ctrl = t;
              self.visit_type2(controller)?;
              self.ctrl = None;
              return Ok(());
            }
          }
          Type2::Array { .. } => {
            if let Value::Array(_) = &self.cbor {
              self.ctrl = t;
              self.visit_type2(controller)?;
              self.ctrl = None;
              return Ok(());
            }
          }
          Type2::Map { .. } => {
            if let Value::Map(_) = &self.cbor {
              self.ctrl = t;
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
      t @ Some(Token::LT) | t @ Some(Token::GT) | t @ Some(Token::GE) | t @ Some(Token::LE) => {
        match target {
          Type2::Typename { ident, .. } if is_ident_numeric_data_type(self.cddl, ident) => {
            self.ctrl = t;
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
      t @ Some(Token::SIZE) => match target {
        Type2::Typename { ident, .. }
          if is_ident_string_data_type(self.cddl, ident)
            || is_ident_uint_data_type(self.cddl, ident)
            || is_ident_byte_string_data_type(self.cddl, ident) =>
        {
          self.ctrl = t;
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
      t @ Some(Token::AND) => {
        self.ctrl = t;
        self.visit_type2(target)?;
        self.visit_type2(controller)?;
        self.ctrl = None;
        Ok(())
      }
      t @ Some(Token::WITHIN) => {
        self.ctrl = t;
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
      t @ Some(Token::DEFAULT) => {
        self.ctrl = t;
        let error_count = self.errors.len();
        self.visit_type2(target)?;
        if self.errors.len() != error_count {
          #[cfg(feature = "ast-span")]
          if let Some(Occur::Optional(_)) = self.occurrence.take() {
            self.add_error(format!(
              "expected default value {}, got {:?}",
              controller, self.cbor
            ));
          }
          #[cfg(not(feature = "ast-span"))]
          if let Some(Occur::Optional) = self.occurrence.take() {
            self.add_error(format!(
              "expected default value {}, got {:?}",
              controller, self.cbor
            ));
          }
        }
        self.ctrl = None;
        Ok(())
      }
      t @ Some(Token::REGEXP) | t @ Some(Token::PCRE) => {
        self.ctrl = t;
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
      t @ Some(Token::CBOR) | t @ Some(Token::CBORSEQ) => {
        self.ctrl = t;
        match target {
          Type2::Typename { ident, .. } if is_ident_byte_string_data_type(self.cddl, ident) => {
            match &self.cbor {
              Value::Bytes(_) | Value::Array(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                "{} control can only be matched against a CBOR byte string, got {:?}",
                ctrl, self.cbor
              )),
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
      t @ Some(Token::BITS) => {
        self.ctrl = t;
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
      t @ Some(Token::CAT) => {
        self.ctrl = t;

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
      t @ Some(Token::DET) => {
        self.ctrl = t;

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
      t @ Some(Token::PLUS) => {
        self.ctrl = t;

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
      t @ Some(Token::ABNF) => {
        self.ctrl = t;

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
      t @ Some(Token::ABNFB) => {
        self.ctrl = t;

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
      t @ Some(Token::FEATURE) => {
        self.ctrl = t;

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
      t @ Some(Token::FEATURE) => {
        self.ctrl = t;

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
      _ => {
        self.add_error(format!("unsupported control operator {}", ctrl));
        Ok(())
      }
    }
  }

  fn visit_type2(&mut self, t2: &Type2<'a>) -> visitor::Result<Error<T>> {
    if matches!(self.ctrl, Some(Token::CBOR)) {
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
    } else if matches!(self.ctrl, Some(Token::CBORSEQ)) {
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
        Value::Array(a) => {
          // Member keys are annotation only in an array context
          if self.is_member_key {
            return Ok(());
          }

          match validate_array_occurrence(
            self.occurrence.as_ref().take(),
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
                  let mut cv =
                    CBORValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
                  #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
                  let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features);
                  #[cfg(not(feature = "additional-controls"))]
                  let mut cv = CBORValidator::new(self.cddl, v.clone());

                  cv.generic_rules = self.generic_rules.clone();
                  cv.eval_generic_rule = self.eval_generic_rule;
                  cv.ctrl = self.ctrl.clone();
                  cv.is_multi_type_choice = self.is_multi_type_choice;
                  cv.cbor_location
                    .push_str(&format!("{}/{}", self.cbor_location, idx));

                  cv.visit_group(group)?;

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
              } else if let Some(idx) = self.group_entry_idx.take() {
                if let Some(v) = a.get(idx) {
                  #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
                  let mut cv =
                    CBORValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
                  #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
                  let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features);
                  #[cfg(not(feature = "additional-controls"))]
                  let mut cv = CBORValidator::new(self.cddl, v.clone());

                  cv.generic_rules = self.generic_rules.clone();
                  cv.eval_generic_rule = self.eval_generic_rule;
                  cv.ctrl = self.ctrl.clone();
                  cv.is_multi_type_choice = self.is_multi_type_choice;
                  cv.cbor_location
                    .push_str(&format!("{}/{}", self.cbor_location, idx));

                  cv.visit_group(group)?;

                  self.errors.append(&mut cv.errors);
                } else if !allow_empty_array {
                  self.add_error(format!("expected map object {} at index {}", group, idx));
                }
              } else {
                self.add_error(format!(
                  "expected map object {}, got {:?}",
                  group, self.cbor
                ));
              }
            }
            Err(errors) => {
              for e in errors.into_iter() {
                self.add_error(e);
              }
            }
          }

          Ok(())
        }
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
            && !matches!(self.ctrl, Some(Token::NE) | Some(Token::DEFAULT))
          {
            self.add_error(format!("expected empty array, got {:?}", self.cbor));
            return Ok(());
          }

          let mut entry_counts = Vec::new();
          for gc in group.group_choices.iter() {
            let count = entry_counts_from_group_choice(self.cddl, gc);
            entry_counts.push(count);
          }

          self.entry_counts = Some(entry_counts);
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

          let mut entry_counts = Vec::new();
          for gc in group.group_choices.iter() {
            let count = entry_counts_from_group_choice(self.cddl, gc);
            entry_counts.push(count);
          }

          self.entry_counts = Some(entry_counts);

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

        self.visit_identifier(ident)
      }
      Type2::IntValue { value, .. } => self.visit_value(&token::Value::INT(*value)),
      Type2::UintValue { value, .. } => self.visit_value(&token::Value::UINT(*value)),
      Type2::FloatValue { value, .. } => self.visit_value(&token::Value::FLOAT(*value)),
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
          if let Some(tag) = tag {
            if *tag as u64 != *actual_tag {
              self.add_error(format!(
                "expected tagged data #6.{}({}), got {:?}",
                tag, t, self.cbor
              ));
              return Ok(());
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
              Some(c) if i128::from(*i) == *c as i128 && i128::from(*i) >= 0i128 => return Ok(()),
              Some(c) => {
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
              Some(c) if i128::from(*i) == 0i128 - *c as i128 => return Ok(()),
              Some(c) => {
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
              Some(c) if *c == b.len() => return Ok(()),
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
              Some(c) if *c == t.len() => return Ok(()),
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
              Some(c) if *c == a.len() => return Ok(()),
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
              Some(c) if *c == m.len() => return Ok(()),
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
      Type2::Any(_) => Ok(()),
      #[cfg(not(feature = "ast-span"))]
      Type2::Any => Ok(()),
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
        .cloned()
        .find(|gr| gr.name == name)
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
        return self.visit_rule(r);
      }
    }

    if is_ident_any_type(self.cddl, ident) {
      return Ok(());
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
      Value::Array(a) => {
        // Member keys are annotation only in an array context
        if self.is_member_key {
          return Ok(());
        }

        match validate_array_occurrence(
          self.occurrence.as_ref().take(),
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
                let mut cv =
                  CBORValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
                #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
                let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features);
                #[cfg(not(feature = "additional-controls"))]
                let mut cv = CBORValidator::new(self.cddl, v.clone());

                cv.generic_rules = self.generic_rules.clone();
                cv.ctrl = self.ctrl.clone();
                cv.eval_generic_rule = self.eval_generic_rule;
                cv.is_multi_type_choice = self.is_multi_type_choice;
                cv.cbor_location
                  .push_str(&format!("{}/{}", self.cbor_location, idx));

                cv.visit_identifier(ident)?;

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
            } else if let Some(idx) = self.group_entry_idx.take() {
              if let Some(v) = a.get(idx) {
                #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
                let mut cv =
                  CBORValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
                #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
                let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features);
                #[cfg(not(feature = "additional-controls"))]
                let mut cv = CBORValidator::new(self.cddl, v.clone());

                cv.generic_rules = self.generic_rules.clone();
                cv.eval_generic_rule = self.eval_generic_rule;
                cv.is_multi_type_choice = self.is_multi_type_choice;
                cv.ctrl = self.ctrl.clone();
                cv.cbor_location
                  .push_str(&format!("{}/{}", self.cbor_location, idx));

                cv.visit_identifier(ident)?;

                self.errors.append(&mut cv.errors);
              } else if !allow_empty_array {
                self.add_error(format!("expected type {} at index {}", ident, idx));
              }
            } else {
              self.add_error(format!("expected type {}, got {:?}", ident, self.cbor));
            }
          }
          Err(errors) => {
            for e in errors.into_iter() {
              self.add_error(e);
            }
          }
        }

        Ok(())
      }
      Value::Map(m) => {
        if let Some(occur) = &self.occurrence {
          #[cfg(feature = "ast-span")]
          if let Occur::ZeroOrMore(_) | Occur::OneOrMore(_) = occur {
            if let Occur::OneOrMore(_) = occur {
              if m.is_empty() {
                self.add_error(format!(
                  "map cannot be empty, one or more entries with key type {} required",
                  ident
                ));
                return Ok(());
              }
            }

            if is_ident_string_data_type(self.cddl, ident) {
              let mut errors = Vec::new();
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
            }
          }

          #[cfg(not(feature = "ast-span"))]
          if let Occur::ZeroOrMore | Occur::OneOrMore = occur {
            if let Occur::OneOrMore = occur {
              if m.is_empty() {
                self.add_error(format!(
                  "map cannot be empty, one or more entries with key type {} required",
                  ident
                ));
                return Ok(());
              }
            }

            if is_ident_string_data_type(self.cddl, ident) {
              let mut errors = Vec::new();
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
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
              for e in errors.into_iter() {
                self.add_error(e);
              }

              return Ok(());
            }
          }
        }

        if is_ident_string_data_type(self.cddl, ident) && !self.validating_value {
          if let Some((k, v)) = m.iter().find(|(k, _)| matches!(k, Value::Text(_))) {
            self
              .validated_keys
              .get_or_insert(vec![k.clone()])
              .push(k.clone());
            self.object_value = Some(v.clone());
            self.cbor_location.push_str(&format!("/{:?}", v));
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
            self.cbor_location.push_str(&format!("/{:?}", v));
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
            self.cbor_location.push_str(&format!("/{:?}", v));
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
            self.cbor_location.push_str(&format!("/{:?}", v));
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
            self.cbor_location.push_str(&format!("/{:?}", v));
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
            self.cbor_location.push_str(&format!("/{:?}", v));
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
            "expecting object value of type {}, got object",
            ident.ident
          ));
          return Ok(());
        }

        self.visit_value(&token::Value::TEXT(ident.ident.into()))
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
          Some(Token::NE) | Some(Token::DEFAULT) if i128::from(*i) != *v as i128 => None,
          Some(Token::LT) if i128::from(*i) < *v as i128 => None,
          Some(Token::LE) if i128::from(*i) <= *v as i128 => None,
          Some(Token::GT) if i128::from(*i) > *v as i128 => None,
          Some(Token::GE) if i128::from(*i) >= *v as i128 => None,
          #[cfg(feature = "additional-controls")]
          Some(Token::PLUS) => {
            if i128::from(*i) == *v as i128 {
              None
            } else {
              Some(format!("expected computed .plus value {}, got {:?}", v, i))
            }
          }
          #[cfg(feature = "additional-controls")]
          None | Some(Token::FEATURE) => {
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
            self.ctrl.clone().unwrap(),
            v,
            i
          )),
        },
        token::Value::UINT(v) => match &self.ctrl {
          Some(Token::NE) | Some(Token::DEFAULT) if i128::from(*i) != *v as i128 => None,
          Some(Token::LT) if i128::from(*i) < *v as i128 => None,
          Some(Token::LE) if i128::from(*i) <= *v as i128 => None,
          Some(Token::GT) if i128::from(*i) > *v as i128 => None,
          Some(Token::GE) if i128::from(*i) >= *v as i128 => None,
          Some(Token::SIZE) if i128::from(*i) < 256i128.pow(*v as u32) => None,
          Some(Token::BITS) => {
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
          Some(Token::PLUS) => {
            if i128::from(*i) == *v as i128 {
              None
            } else {
              Some(format!("expected computed .plus value {}, got {:?}", v, i))
            }
          }
          #[cfg(feature = "additional-controls")]
          None | Some(Token::FEATURE) => {
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
            self.ctrl.clone().unwrap(),
            v,
            i
          )),
        },

        _ => Some(format!("expected {}, got {:?}", value, i)),
      },
      Value::Float(f) => match value {
        token::Value::FLOAT(v) => match &self.ctrl {
          Some(Token::NE) | Some(Token::DEFAULT) if (*f - *v).abs() > std::f64::EPSILON => None,
          Some(Token::LT) if *f < *v as f64 => None,
          Some(Token::LE) if *f <= *v as f64 => None,
          Some(Token::GT) if *f > *v as f64 => None,
          Some(Token::GE) if *f >= *v as f64 => None,
          #[cfg(feature = "additional-controls")]
          Some(Token::PLUS) => {
            if (*f - *v).abs() < std::f64::EPSILON {
              None
            } else {
              Some(format!("expected computed .plus value {}, got {:?}", v, f))
            }
          }
          #[cfg(feature = "additional-controls")]
          None | Some(Token::FEATURE) => {
            if (*f - *v).abs() < std::f64::EPSILON {
              None
            } else {
              Some(format!("expected value {}, got {:?}", v, f))
            }
          }
          #[cfg(not(feature = "additional-controls"))]
          None => {
            if (*f - *v).abs() < std::f64::EPSILON {
              None
            } else {
              Some(format!("expected value {}, got {:?}", v, f))
            }
          }
          _ => Some(format!(
            "expected value {} {}, got {:?}",
            self.ctrl.clone().unwrap(),
            v,
            f
          )),
        },
        _ => Some(format!("expected {}, got {:?}", value, f)),
      },
      Value::Text(s) => match value {
        token::Value::TEXT(t) => match &self.ctrl {
          Some(Token::NE) | Some(Token::DEFAULT) => {
            if s != t {
              None
            } else {
              Some(format!("expected {} .ne to \"{}\"", value, s))
            }
          }
          Some(Token::REGEXP) | Some(Token::PCRE) => {
            let re = regex::Regex::new(
              &format_regex(
                // Text strings must be JSON escaped per
                // https://datatracker.ietf.org/doc/html/rfc8610#section-3.1
                serde_json::from_str::<serde_json::Value>(&format!("\"{}\"", t))
                  .map_err(Error::JSONParsing)?
                  .as_str()
                  .ok_or_else(|| Error::from_validator(self, "malformed regex".to_string()))?,
              )
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
          Some(Token::ABNF) => validate_abnf(t, s)
            .err()
            .map(|e| format!("\"{}\" is not valid against abnf: {}", s, e)),
          _ => {
            #[cfg(feature = "additional-controls")]
            if s == t {
              None
            } else if let Some(Token::CAT) | Some(Token::DET) = &self.ctrl {
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
          Some(Token::SIZE) => {
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
          Some(Token::SIZE) => {
            if b.len() == *v {
              None
            } else {
              Some(format!("expected \"{:?}\" .size {}, got {}", b, v, b.len()))
            }
          }
          Some(Token::BITS) => {
            if let Some(rsv) = v.checked_shr(3) {
              if let Some(s) = b.get(rsv) {
                if let Some(lsv) = 1u32.checked_shl(*v as u32 & 7) {
                  if (*s as u32 & lsv) != 0 {
                    None
                  } else {
                    Some(format!(
                      "expected value {} {}, got {:?}",
                      self.ctrl.clone().unwrap(),
                      v,
                      b
                    ))
                  }
                } else {
                  Some(format!(
                    "expected value {} {}, got {:?}",
                    self.ctrl.clone().unwrap(),
                    v,
                    b
                  ))
                }
              } else {
                Some(format!(
                  "expected value {} {}, got {:?}",
                  self.ctrl.clone().unwrap(),
                  v,
                  b
                ))
              }
            } else {
              Some(format!(
                "expected value {} {}, got {:?}",
                self.ctrl.clone().unwrap(),
                v,
                b
              ))
            }
          }
          _ => Some(format!(
            "expected value {} {}, got {:?}",
            self.ctrl.clone().unwrap(),
            v,
            b
          )),
        },
        #[cfg(feature = "additional-controls")]
        token::Value::TEXT(t) => match &self.ctrl {
          Some(Token::ABNFB) => {
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
            self.ctrl.clone().unwrap(),
            t,
            b
          )),
        },
        _ => Some(format!("expected {}, got {:?}", value, b)),
      },
      Value::Array(a) => {
        // Member keys are annotation only in an array context
        if self.is_member_key {
          return Ok(());
        }

        match validate_array_occurrence(
          self.occurrence.as_ref().take(),
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
                let mut cv =
                  CBORValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
                #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
                let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features);
                #[cfg(not(feature = "additional-controls"))]
                let mut cv = CBORValidator::new(self.cddl, v.clone());

                cv.generic_rules = self.generic_rules.clone();
                cv.eval_generic_rule = self.eval_generic_rule;
                cv.is_multi_type_choice = self.is_multi_type_choice;
                cv.ctrl = self.ctrl.clone();
                cv.cbor_location
                  .push_str(&format!("{}/{}", self.cbor_location, idx));

                cv.visit_value(value)?;

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
            } else if let Some(idx) = self.group_entry_idx.take() {
              if let Some(v) = a.get(idx) {
                #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
                let mut cv =
                  CBORValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
                #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
                let mut cv = CBORValidator::new(self.cddl, v.clone(), self.enabled_features);
                #[cfg(not(feature = "additional-controls"))]
                let mut cv = CBORValidator::new(self.cddl, v.clone());

                cv.generic_rules = self.generic_rules.clone();
                cv.eval_generic_rule = self.eval_generic_rule;
                cv.ctrl = self.ctrl.clone();
                cv.is_multi_type_choice = self.is_multi_type_choice;
                cv.cbor_location
                  .push_str(&format!("{}/{}", self.cbor_location, idx));

                cv.visit_value(value)?;

                self.errors.append(&mut cv.errors);
              } else if !allow_empty_array {
                self.add_error(format!("expected value {} at index {}", value, idx));
              }
            } else {
              self.add_error(format!("expected value {}, got {:?}", value, self.cbor));
            }
          }
          Err(errors) => {
            for e in errors.into_iter() {
              self.add_error(e);
            }
          }
        }

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
          self.cbor_location.push_str(&format!("/{}", value));

          None
        } else if let Some(Occur::Optional(_)) | Some(Occur::ZeroOrMore(_)) =
          &self.occurrence.take()
        {
          self.advance_to_next_entry = true;
          None
        } else if let Some(Token::NE) | Some(Token::DEFAULT) = &self.ctrl {
          None
        } else {
          Some(format!("object missing key: \"{}\"", value))
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
        } else if let Some(Occur::Optional) | Some(Occur::ZeroOrMore) = &self.occurrence.take() {
          self.advance_to_next_entry = true;
          None
        } else if let Some(Token::NE) | Some(Token::DEFAULT) = &self.ctrl {
          None
        } else {
          Some(format!("object missing key: \"{}\"", value))
        }
      }
      _ => Some(format!("expected {}, got {:?}", value, self.cbor)),
    };

    if let Some(e) = error {
      self.add_error(e);
    }

    Ok(())
  }

  fn visit_occurrence(&mut self, o: &Occurrence) -> visitor::Result<Error<T>> {
    self.occurrence = Some(o.occur.clone());

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

    let mut lexer = lexer_from_str(cddl);
    let cddl = cddl_from_str(&mut lexer, cddl, true)?;

    let mut cv = CBORValidator::new(&cddl, cbor);
    cv.validate()?;

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn validate_abnfb() -> std::result::Result<(), Box<dyn std::error::Error>> {
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

    let mut jv = CBORValidator::new(&cddl, cbor, Some(&["cbor"]));
    jv.validate()?;

    Ok(())
  }
}
