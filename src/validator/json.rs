#![cfg(feature = "std")]
#![cfg(feature = "json")]
#![cfg(not(feature = "lsp"))]

use super::*;
use crate::{
  ast::*,
  token,
  visitor::{self, *},
};

use std::{
  borrow::Cow,
  collections::{HashMap, HashSet},
  convert::TryFrom,
  fmt::{self, Write},
};

use chrono::{TimeZone, Utc};
use serde_json::Value;

#[cfg(feature = "additional-controls")]
use control::{abnf_from_complex_controller, cat_operation, plus_operation, validate_abnf};

/// JSON validation Result
pub type Result = std::result::Result<(), Error>;

/// JSON validation error
#[derive(Debug)]
pub enum Error {
  /// Zero or more validation errors
  Validation(Vec<ValidationError>),
  /// JSON parsing error
  JSONParsing(serde_json::Error),
  /// CDDL parsing error
  CDDLParsing(String),
  /// UTF8 parsing error,
  UTF8Parsing(std::str::Utf8Error),
  /// Disabled feature
  DisabledFeature(String),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Error::Validation(errors) => {
        let mut error_str = String::new();
        for e in errors.iter() {
          let _ = writeln!(error_str, "{}", e);
        }
        write!(f, "{}", error_str)
      }
      Error::JSONParsing(error) => write!(f, "error parsing JSON: {}", error),
      Error::CDDLParsing(error) => write!(f, "error parsing CDDL: {}", error),
      Error::UTF8Parsing(error) => write!(f, "error pasing utf8: {}", error),
      Error::DisabledFeature(feature) => write!(f, "feature {} is not enabled", feature),
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

impl Error {
  fn from_validator(jv: &JSONValidator, reason: String) -> Self {
    Error::Validation(vec![ValidationError {
      cddl_location: jv.cddl_location.clone(),
      json_location: jv.json_location.clone(),
      reason,
      is_multi_type_choice: jv.is_multi_type_choice,
      is_group_to_choice_enum: jv.is_group_to_choice_enum,
      type_group_name_entry: jv.type_group_name_entry.map(|e| e.to_string()),
      is_multi_group_choice: jv.is_multi_group_choice,
    }])
  }
}

/// JSON validation error
#[derive(Clone, Debug)]
pub struct ValidationError {
  /// Error message
  pub reason: String,
  /// Location in CDDL where error occurred
  pub cddl_location: String,
  /// Location in JSON (in JSONPointer notation) where error occurred
  pub json_location: String,
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

    if self.json_location.is_empty() {
      return write!(
        f,
        "{} at the root of the JSON document: {}",
        error_str, self.reason
      );
    }

    write!(
      f,
      "{} at JSON location {}: {}",
      error_str, self.json_location, self.reason
    )
  }
}

impl std::error::Error for ValidationError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

impl ValidationError {
  fn from_validator(jv: &JSONValidator, reason: String) -> Self {
    ValidationError {
      cddl_location: jv.cddl_location.clone(),
      json_location: jv.json_location.clone(),
      reason,
      is_multi_type_choice: jv.is_multi_type_choice,
      is_group_to_choice_enum: jv.is_group_to_choice_enum,
      type_group_name_entry: jv.type_group_name_entry.map(|e| e.to_string()),
      is_multi_group_choice: jv.is_multi_group_choice,
    }
  }
}

/// JSON validator type
#[derive(Clone)]
pub struct JSONValidator<'a> {
  cddl: &'a CDDL<'a>,
  json: Value,
  errors: Vec<ValidationError>,
  cddl_location: String,
  json_location: String,
  // Occurrence indicator detected in current state of AST evaluation
  occurrence: Option<Occur>,
  // Current group entry index detected in current state of AST evaluation
  group_entry_idx: Option<usize>,
  // JSON object value hoisted from previous state of AST evaluation
  object_value: Option<Value>,
  // Is member key detected in current state of AST evaluation
  is_member_key: bool,
  // Is a cut detected in current state of AST evaluation
  is_cut_present: bool,
  // Str value of cut detected in current state of AST evaluation
  cut_value: Option<Cow<'a, str>>,
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
  is_ctrl_map_equality: bool,
  entry_counts: Option<Vec<EntryCount>>,
  // Collect map entry keys that have already been validated
  validated_keys: Option<Vec<String>>,
  // Collect map entry values that have yet to be validated
  values_to_validate: Option<Vec<Value>>,
  // Collect valid array indices when entries are type choices
  valid_array_items: Option<Vec<usize>>,
  // Collect invalid array item errors where the key is the index of the invalid
  // array item
  array_errors: Option<HashMap<usize, Vec<ValidationError>>>,
  is_colon_shortcut_present: bool,
  is_root: bool,
  is_multi_type_choice_type_rule_validating_array: bool,
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
  // Track visited rules to prevent infinite recursion during validation
  // This prevents stack overflow when validating recursive CDDL structures
  visited_rules: HashSet<String>,
}

#[derive(Clone, Debug)]
struct GenericRule<'a> {
  name: &'a str,
  params: Vec<&'a str>,
  args: Vec<Type1<'a>>,
}

impl<'a> JSONValidator<'a> {
  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(feature = "additional-controls")]
  /// New JSONValidation from CDDL AST and JSON value
  pub fn new(cddl: &'a CDDL<'a>, json: Value, enabled_features: Option<&'a [&'a str]>) -> Self {
    JSONValidator {
      cddl,
      json,
      errors: Vec::default(),
      cddl_location: String::new(),
      json_location: String::new(),
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
      valid_array_items: None,
      array_errors: None,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      enabled_features,
      has_feature_errors: false,
      disabled_features: None,
      visited_rules: HashSet::new(),
    }
  }

  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(not(feature = "additional-controls"))]
  /// New JSONValidation from CDDL AST and JSON value
  pub fn new(cddl: &'a CDDL<'a>, json: Value) -> Self {
    JSONValidator {
      cddl,
      json,
      errors: Vec::default(),
      cddl_location: String::new(),
      json_location: String::new(),
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
      valid_array_items: None,
      array_errors: None,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      visited_rules: HashSet::new(),
    }
  }

  #[cfg(target_arch = "wasm32")]
  #[cfg(feature = "additional-controls")]
  /// New JSONValidation from CDDL AST and JSON value
  pub fn new(cddl: &'a CDDL<'a>, json: Value, enabled_features: Option<Box<[JsValue]>>) -> Self {
    JSONValidator {
      cddl,
      json,
      errors: Vec::default(),
      cddl_location: String::new(),
      json_location: String::new(),
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
      valid_array_items: None,
      array_errors: None,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      enabled_features,
      has_feature_errors: false,
      disabled_features: None,
      visited_rules: HashSet::new(),
    }
  }

  #[cfg(target_arch = "wasm32")]
  #[cfg(not(feature = "additional-controls"))]
  /// New JSONValidation from CDDL AST and JSON value
  pub fn new(cddl: &'a CDDL<'a>, json: Value) -> Self {
    JSONValidator {
      cddl,
      json,
      errors: Vec::default(),
      cddl_location: String::new(),
      json_location: String::new(),
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
      valid_array_items: None,
      array_errors: None,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      visited_rules: HashSet::new(),
    }
  }

  fn validate_array_items(&mut self, token: &ArrayItemToken) -> visitor::Result<Error> {
    if let Value::Array(a) = &self.json {
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
              let mut jv = JSONValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
              #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
              let mut jv = JSONValidator::new(self.cddl, v.clone(), self.enabled_features);
              #[cfg(not(feature = "additional-controls"))]
              let mut jv = JSONValidator::new(self.cddl, v.clone());

              jv.generic_rules = self.generic_rules.clone();
              jv.eval_generic_rule = self.eval_generic_rule;
              jv.is_multi_type_choice = self.is_multi_type_choice;
              jv.ctrl = self.ctrl;
              let _ = write!(jv.json_location, "{}/{}", self.json_location, idx);

              match token {
                ArrayItemToken::Value(value) => jv.visit_value(value)?,
                ArrayItemToken::Range(lower, upper, is_inclusive) => {
                  jv.visit_range(lower, upper, *is_inclusive)?
                }
                ArrayItemToken::Group(group) => jv.visit_group(group)?,
                ArrayItemToken::Identifier(ident) => jv.visit_identifier(ident)?,
                ArrayItemToken::TaggedData(tag_data) => {
                  // Handle tagged data
                  jv.add_error(format!(
                    "Tagged data not supported in arrays: {:?}",
                    tag_data
                  ))
                }
              }

              if self.is_multi_type_choice && jv.errors.is_empty() {
                if let Some(indices) = &mut self.valid_array_items {
                  indices.push(idx);
                } else {
                  self.valid_array_items = Some(vec![idx]);
                }
                continue;
              }

              if let Some(errors) = &mut self.array_errors {
                if let Some(error) = errors.get_mut(&idx) {
                  error.append(&mut jv.errors);
                } else {
                  errors.insert(idx, jv.errors);
                }
              } else {
                let mut errors = HashMap::new();
                errors.insert(idx, jv.errors);
                self.array_errors = Some(errors)
              }
            }
          } else if let Some(idx) = self.group_entry_idx {
            if let Some(v) = a.get(idx) {
              #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
              let mut jv = JSONValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
              #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
              let mut jv = JSONValidator::new(self.cddl, v.clone(), self.enabled_features);
              #[cfg(not(feature = "additional-controls"))]
              let mut jv = JSONValidator::new(self.cddl, v.clone());

              jv.generic_rules = self.generic_rules.clone();
              jv.eval_generic_rule = self.eval_generic_rule;
              jv.is_multi_type_choice = self.is_multi_type_choice;
              jv.ctrl = self.ctrl;
              let _ = write!(jv.json_location, "{}/{}", self.json_location, idx);

              match token {
                ArrayItemToken::Value(value) => jv.visit_value(value)?,
                ArrayItemToken::Range(lower, upper, is_inclusive) => {
                  jv.visit_range(lower, upper, *is_inclusive)?
                }

                ArrayItemToken::Identifier(ident) => jv.visit_identifier(ident)?,
                ArrayItemToken::TaggedData(tag_data) => {
                  // Handle tagged data
                  jv.add_error(format!(
                    "Tagged data not supported in arrays: {:?}",
                    tag_data
                  ))
                }
                // Special nested array handling - using Group variant instead
                // as Array variant doesn't exist
                ArrayItemToken::Group(group) if v.is_array() => {
                  // Special handling for nested arrays
                  jv.visit_group(group)?;
                }
                ArrayItemToken::Group(group) => jv.visit_group(group)?,
              }

              self.errors.append(&mut jv.errors);
            } else if !allow_empty_array {
              self.add_error(token.error_msg(Some(idx)));
            }
          } else if !self.is_multi_type_choice {
            self.add_error(format!("{}, got {}", token.error_msg(None), self.json));
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

  fn validate_object_value(&mut self, value: &token::Value<'a>) -> visitor::Result<Error> {
    if let Value::Object(o) = &self.json {
      // Bareword member keys are converted to text string values
      if let token::Value::TEXT(t) = value {
        if self.is_cut_present {
          self.cut_value = Some(t.clone());
        }

        if *t == "any" {
          return Ok(());
        }

        // Retrieve the value from key unless optional/zero or more, in which
        // case advance to next group entry
        #[cfg(feature = "ast-span")]
        if let Some(v) = o.get(t.as_ref()) {
          self
            .validated_keys
            .get_or_insert(vec![t.to_string()])
            .push(t.to_string());
          self.object_value = Some(v.clone());
          let _ = write!(self.json_location, "/{}", t);

          return Ok(());
        } else if let Some(Occur::Optional { .. }) | Some(Occur::ZeroOrMore { .. }) =
          &self.occurrence.take()
        {
          self.advance_to_next_entry = true;
          return Ok(());
        } else if let Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT) = &self.ctrl {
          return Ok(());
        } else {
          self.add_error(format!("object missing key: \"{}\"", t))
        }

        // Retrieve the value from key unless optional/zero or more, in which
        // case advance to next group entry
        #[cfg(not(feature = "ast-span"))]
        if let Some(v) = o.get(t.as_ref()) {
          self
            .validated_keys
            .get_or_insert(vec![t.to_string()])
            .push(t.to_string());
          self.object_value = Some(v.clone());
          self.json_location.push_str(&format!("/{}", t));

          return Ok(());
        } else if let Some(Occur::Optional {}) | Some(Occur::ZeroOrMore {}) =
          &self.occurrence.take()
        {
          self.advance_to_next_entry = true;
          return Ok(());
        } else if let Some(Token::NE) | Some(Token::DEFAULT) = &self.ctrl {
          return Ok(());
        } else {
          self.add_error(format!("object missing key: \"{}\"", t))
        }
      } else {
        self.add_error(format!(
          "CDDL member key must be string data type. got {}",
          value
        ))
      }
    }

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  fn validate_b64_control(
    &mut self,
    bytes: &[u8],
    is_classic: bool,
    is_sloppy: bool,
  ) -> visitor::Result<Error> {
    if let Value::String(s) = &self.json {
      let decoded_result = if is_classic {
        if is_sloppy {
          data_encoding::BASE64.decode(s.as_bytes())
        } else {
          data_encoding::BASE64.decode(s.as_bytes())
        }
      } else {
        if is_sloppy {
          data_encoding::BASE64URL_NOPAD.decode(s.as_bytes())
        } else {
          data_encoding::BASE64URL_NOPAD.decode(s.as_bytes())
        }
      };

      match decoded_result {
        Ok(decoded_bytes) => {
          if decoded_bytes == bytes {
            // Validation succeeded
          } else {
            let expected_encoding = if is_classic {
              data_encoding::BASE64.encode(bytes)
            } else {
              data_encoding::BASE64URL_NOPAD.encode(bytes)
            };
            self.add_error(format!(
              "string \"{}\" does not match expected base64 encoding \"{}\"",
              s, expected_encoding
            ));
          }
        }
        Err(e) => {
          self.add_error(format!("invalid base64 encoding: {}", e));
        }
      }
    } else {
      self.add_error(format!(
        "expected string for base64 validation, got {}",
        self.json
      ));
    }
    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  fn validate_hex_control(&mut self, bytes: &[u8], is_lowercase: bool) -> visitor::Result<Error> {
    if let Value::String(s) = &self.json {
      match hex::decode(s) {
        Ok(decoded_bytes) => {
          if decoded_bytes == bytes {
            // Validation succeeded
          } else {
            let expected_encoding = if is_lowercase {
              hex::encode(bytes)
            } else {
              hex::encode_upper(bytes)
            };
            self.add_error(format!(
              "string \"{}\" does not match expected hex encoding \"{}\"",
              s, expected_encoding
            ));
          }
        }
        Err(e) => {
          self.add_error(format!("invalid hex encoding: {}", e));
        }
      }
    } else {
      self.add_error(format!(
        "expected string for hex validation, got {}",
        self.json
      ));
    }
    Ok(())
  }
}

impl<'a> Validator<'a, '_, Error> for JSONValidator<'a> {
  /// Validate
  fn validate(&mut self) -> std::result::Result<(), Error> {
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
      json_location: self.json_location.clone(),
      is_multi_type_choice: self.is_multi_type_choice,
      is_multi_group_choice: self.is_multi_group_choice,
      is_group_to_choice_enum: self.is_group_to_choice_enum,
      type_group_name_entry: self.type_group_name_entry.map(|e| e.to_string()),
    });
  }
}

impl<'a> Visitor<'a, '_, Error> for JSONValidator<'a> {
  fn visit_type_rule(&mut self, tr: &TypeRule<'a>) -> visitor::Result<Error> {
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

    let type_choice_alternates = type_choice_alternates_from_ident(self.cddl, &tr.name);
    if !type_choice_alternates.is_empty() {
      self.is_multi_type_choice = true;

      if self.json.is_array() {
        self.is_multi_type_choice_type_rule_validating_array = true;
      }
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

    if tr.value.type_choices.len() > 1 && self.json.is_array() {
      self.is_multi_type_choice_type_rule_validating_array = true;
    }

    self.visit_type(&tr.value)
  }

  fn visit_group_rule(&mut self, gr: &GroupRule<'a>) -> visitor::Result<Error> {
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

  fn visit_type(&mut self, t: &Type<'a>) -> visitor::Result<Error> {
    // Special case for nested array in literal position
    if let Value::Array(outer_array) = &self.json {
      if let Some(idx) = self.group_entry_idx {
        // We're processing a specific array item
        if let Some(item) = outer_array.get(idx) {
          if item.is_array() {
            // This is a nested array, check if we're supposed to validate against an array type
            for tc in t.type_choices.iter() {
              if let Type2::Array { .. } = &tc.type1.type2 {
                #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
                let mut jv =
                  JSONValidator::new(self.cddl, item.clone(), self.enabled_features.clone());
                #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
                let mut jv = JSONValidator::new(self.cddl, item.clone(), self.enabled_features);
                #[cfg(not(feature = "additional-controls"))]
                let mut jv = JSONValidator::new(self.cddl, item.clone());

                jv.generic_rules = self.generic_rules.clone();
                jv.eval_generic_rule = self.eval_generic_rule;
                jv.is_multi_type_choice = self.is_multi_type_choice;

                let _ = write!(jv.json_location, "{}/{}", self.json_location, idx);

                // Visit the type choice with the inner array value
                jv.visit_type_choice(tc)?;

                self.errors.append(&mut jv.errors);
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
      // If validating an array whose elements are type choices (i.e. [ 1* tstr
      // / integer ]), collect all errors and filter after the fact
      if matches!(self.json, Value::Array(_))
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

  fn visit_group(&mut self, g: &Group<'a>) -> visitor::Result<Error> {
    if g.group_choices.len() > 1 {
      self.is_multi_group_choice = true;
    }

    // Map equality/inequality validation
    if self.is_ctrl_map_equality {
      if let Some(t) = &self.ctrl {
        if let Value::Object(o) = &self.json {
          let entry_counts = entry_counts_from_group(self.cddl, g);

          let len = o.len();
          if let ControlOperator::EQ = t {
            if !validate_entry_count(&entry_counts, len) {
              for ec in entry_counts.iter() {
                if let Some(occur) = &ec.entry_occurrence {
                  self.add_error(format!(
                    "map equality error. expected object with number of entries per occurrence {}",
                    occur,
                  ));
                } else {
                  self.add_error(format!(
                    "map equality error, expected object with length {}, got {}",
                    ec.count, len
                  ));
                }
              }
              return Ok(());
            }
          } else if let ControlOperator::NE | ControlOperator::DEFAULT = t {
            if !validate_entry_count(&entry_counts, len) {
              for ec in entry_counts.iter() {
                if let Some(occur) = &ec.entry_occurrence {
                  self.add_error(format!(
                    "map inequality error. expected object with number of entries not per occurrence {}",
                    occur,
                  ));
                } else {
                  self.add_error(format!(
                    "map inequality error, expected object not with length {}, got {}",
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

  fn visit_group_choice(&mut self, gc: &GroupChoice<'a>) -> visitor::Result<Error> {
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
      if let Some(current_index) = self.group_entry_idx.as_mut() {
        if idx != 0 {
          *current_index += 1;
        }
      } else {
        self.group_entry_idx = Some(idx);
      }

      self.visit_group_entry(&ge.0)?;
    }

    Ok(())
  }

  fn visit_range(
    &mut self,
    lower: &Type2,
    upper: &Type2,
    is_inclusive: bool,
  ) -> visitor::Result<Error> {
    if matches!(&self.json, Value::Array(_)) {
      return self.validate_array_items(&ArrayItemToken::Range(lower, upper, is_inclusive));
    }

    match (lower, upper) {
      (Type2::IntValue { value: l, .. }, Type2::IntValue { value: u, .. }) => {
        let error_str = if is_inclusive {
          format!(
            "expected integer to be in range {} <= value <= {}, got {}",
            l, u, self.json
          )
        } else {
          // Changed: For ... ranges, make it inclusive on lower bound, exclusive on upper bound
          format!(
            "expected integer to be in range {} <= value < {}, got {}",
            l, u, self.json
          )
        };

        match &self.json {
          Value::Number(n) => {
            if let Some(i) = n.as_i64() {
              if is_inclusive {
                if i < *l as i64 || i > *u as i64 {
                  self.add_error(error_str);
                }
              } else {
                // Changed: For ... ranges, make it inclusive on lower bound, exclusive on upper bound
                if i < *l as i64 || i >= *u as i64 {
                  self.add_error(error_str);
                }
              }
            } else {
              self.add_error(error_str);
              return Ok(());
            }
          }
          _ => {
            self.add_error(format!(
              "invalid cddl range. value must be an integer type. got {}",
              self.json
            ));
            return Ok(());
          }
        }
      }
      (Type2::UintValue { value: l, .. }, Type2::UintValue { value: u, .. }) => {
        let error_str = if is_inclusive {
          format!(
            "expected uint to be in range {} <= value <= {}, got {}",
            l, u, self.json
          )
        } else {
          // Changed: For ... ranges, make it inclusive on lower bound, exclusive on upper bound
          format!(
            "expected uint to be in range {} <= value < {}, got {}",
            l, u, self.json
          )
        };

        match &self.json {
          Value::Number(n) => {
            if let Some(i) = n.as_u64() {
              // Fix: Cast all usize values to u64
              let lower = *l as u64;
              let upper = *u as u64;
              if is_inclusive {
                if i < lower || i > upper {
                  self.add_error(error_str);
                }
              } else if i < lower || i >= upper {
                self.add_error(error_str);
              }
            } else {
              self.add_error(error_str);
              return Ok(());
            }
          }
          Value::String(s) => match self.ctrl {
            Some(ControlOperator::SIZE) => {
              let len = s.len() as u64; // Fix: Convert len to u64
                                        // Fix: Cast all usize values to u64
              let lower = *l as u64;
              let upper = *u as u64;
              if is_inclusive {
                if len < lower || len > upper {
                  self.add_error(error_str);
                }
              } else if len < lower || len >= upper {
                self.add_error(error_str);
              }
            }
            _ => {
              self.add_error("string value cannot be validated against a range without the .size control operator".to_string());
              return Ok(());
            }
          },
          _ => {
            self.add_error(format!(
              "invalid cddl range. value must be a uint type. got {}",
              self.json
            ));
            return Ok(());
          }
        }
      }
      (Type2::FloatValue { value: l, .. }, Type2::FloatValue { value: u, .. }) => {
        let error_str = if is_inclusive {
          format!(
            "expected float to be in range {} <= value <= {}, got {}",
            l, u, self.json
          )
        } else {
          // Changed: For ... ranges, make it inclusive on lower bound, exclusive on upper bound
          format!(
            "expected float to be in range {} <= value < {}, got {}",
            l, u, self.json
          )
        };

        match &self.json {
          Value::Number(n) => {
            if let Some(f) = n.as_f64() {
              if is_inclusive {
                if f < *l || f > *u {
                  self.add_error(error_str);
                }
              } else {
                // Changed: For ... ranges, make it inclusive on lower bound, exclusive on upper bound
                if f < *l || f >= *u {
                  self.add_error(error_str);
                }
              }
            } else {
              self.add_error(error_str);
              return Ok(());
            }
          }
          _ => {
            self.add_error(format!(
              "invalid cddl range. value must be a float type. got {}",
              self.json
            ));
            return Ok(());
          }
        }
      }
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
    ctrl: ControlOperator,
    controller: &Type2<'a>,
  ) -> visitor::Result<Error> {
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
          if let Some(gr) = self.generic_rules.iter().find(|r| r.name == name).cloned() {
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
        if let Some(gr) = self.generic_rules.iter().find(|r| r.name == name).cloned() {
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
      ControlOperator::EQ => match target {
        Type2::Typename { ident, .. } => {
          if is_ident_string_data_type(self.cddl, ident)
            || is_ident_numeric_data_type(self.cddl, ident)
          {
            return self.visit_type2(controller);
          }
        }
        Type2::Array { group, .. } => {
          if let Value::Array(_) = &self.json {
            self.entry_counts = Some(entry_counts_from_group(self.cddl, group));
            self.visit_type2(controller)?;
            self.entry_counts = None;
            return Ok(());
          }
        }
        Type2::Map { .. } => {
          if let Value::Object(_) = &self.json {
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
      },
      ControlOperator::NE => match target {
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
          if let Value::Array(_) = &self.json {
            self.ctrl = Some(ctrl);
            self.visit_type2(controller)?;
            self.ctrl = None;
            return Ok(());
          }
        }
        Type2::Map { .. } => {
          if let Value::Object(_) = &self.json {
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
      },
      ControlOperator::LT | ControlOperator::GT | ControlOperator::GE | ControlOperator::LE => {
        match target {
          Type2::Typename { ident, .. } if is_ident_numeric_data_type(self.cddl, ident) => {
            self.ctrl = Some(ctrl);
            self.visit_type2(controller)?;
            self.ctrl = None;
          }
          _ => {
            self.add_error(format!(
              "target for .lt, .gt, .ge or .le operator must be a numerical data type, got {}",
              target
            ));
          }
        }
      }
      ControlOperator::SIZE => match target {
        Type2::Typename { ident, .. }
          if is_ident_string_data_type(self.cddl, ident)
            || is_ident_uint_data_type(self.cddl, ident) =>
        {
          self.ctrl = Some(ctrl);
          self.visit_type2(controller)?;
          self.ctrl = None;
        }
        _ => {
          self.add_error(format!(
            "target for .size must a string or uint data type, got {}",
            target
          ));
        }
      },
      ControlOperator::AND => {
        self.ctrl = Some(ctrl);
        self.visit_type2(target)?;
        self.visit_type2(controller)?;
        self.ctrl = None;
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
            "expected type {} .within type {}, got {}",
            target, controller, self.json,
          ));
        }

        self.ctrl = None;
      }
      ControlOperator::DEFAULT => {
        self.ctrl = Some(ctrl);
        let error_count = self.errors.len();
        self.visit_type2(target)?;
        if self.errors.len() != error_count {
          if let Some(Occur::Optional { .. }) = self.occurrence.as_ref() {
            self.add_error(format!(
              "expected default value {}, got {}",
              controller, self.json
            ));
          }
        }
        self.ctrl = None;
      }
      ControlOperator::REGEXP | ControlOperator::PCRE => {
        self.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match self.json {
              Value::String(_) | Value::Array(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".regexp/.pcre control can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".regexp/.pcre control can only be matched against string data type, got {}",
            target
          )),
        }
        self.ctrl = None;
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
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::ABNF => {
        self.ctrl = Some(ctrl);

        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match self.json {
              Value::String(_) | Value::Array(_) => {
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
                ".abnf control can only be matched against a JSON string, got {}",
                self.json,
              )),
            }
          }
          _ => self.add_error(format!(
            ".abnf can only be matched against string data type, got {}",
            target,
          )),
        }

        self.ctrl = None;
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
      }
      #[cfg(feature = "additional-controls")]
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64U => {
        self.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.json {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".b64u can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64u can only be matched against string data type, got {}",
            target
          )),
        }
        self.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64C => {
        self.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.json {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".b64c can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64c can only be matched against string data type, got {}",
            target
          )),
        }
        self.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64USLOPPY => {
        self.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.json {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".b64u-sloppy can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64u-sloppy can only be matched against string data type, got {}",
            target
          )),
        }
        self.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64CSLOPPY => {
        self.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.json {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".b64c-sloppy can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64c-sloppy can only be matched against string data type, got {}",
            target
          )),
        }
        self.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::HEX => {
        self.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.json {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".hex can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".hex can only be matched against string data type, got {}",
            target
          )),
        }
        self.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::HEXLC => {
        self.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.json {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".hexlc can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".hexlc can only be matched against string data type, got {}",
            target
          )),
        }
        self.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::HEXUC => {
        self.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match &self.json {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".hexuc can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".hexuc can only be matched against string data type, got {}",
            target
          )),
        }
        self.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B32 => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
          match &self.json {
            Value::String(s) => {
              match crate::validator::control::validate_b32_text(target, controller, s, false) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!(
                      "string \"{}\" does not match .b32 encoded bytes",
                      s
                    ));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".b32 can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".b32 can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::H32 => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
          match &self.json {
            Value::String(s) => {
              match crate::validator::control::validate_b32_text(target, controller, s, true) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!(
                      "string \"{}\" does not match .h32 encoded bytes",
                      s
                    ));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".h32 can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".h32 can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::B45 => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
          match &self.json {
            Value::String(s) => {
              match crate::validator::control::validate_b45_text(target, controller, s) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!(
                      "string \"{}\" does not match .b45 encoded bytes",
                      s
                    ));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".b45 can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".b45 can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::BASE10 => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
          match &self.json {
            Value::String(s) => {
              match crate::validator::control::validate_base10_text(target, controller, s) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!(
                      "string \"{}\" does not match .base10 integer format",
                      s
                    ));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".base10 can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".base10 can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::PRINTF => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
          match &self.json {
            Value::String(s) => {
              match crate::validator::control::validate_printf_text(target, controller, s) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!("string \"{}\" does not match .printf format", s));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".printf can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".printf can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::JSON => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
          match &self.json {
            Value::String(s) => {
              match crate::validator::control::validate_json_text(target, controller, s) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!("string \"{}\" does not contain valid JSON", s));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".json can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".json can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::JOIN => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
          match &self.json {
            Value::String(s) => {
              match crate::validator::control::validate_join_text(target, controller, s) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!("string \"{}\" does not match .join result", s));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".join can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".join can only be matched against string data type, got {}",
          target
        )),
      },
      _ => {
        self.add_error(format!("unsupported control operator {}", ctrl));
      }
    }

    Ok(())
  }

  fn visit_type2(&mut self, t2: &Type2<'a>) -> visitor::Result<Error> {
    match t2 {
      Type2::TextValue { value, .. } => self.visit_value(&token::Value::TEXT(value.clone())),
      Type2::Map { group, .. } => match &self.json {
        Value::Object(o) => {
          // Check if this is an empty map schema with non-empty JSON object
          if group.group_choices.len() == 1
            && group.group_choices[0].group_entries.is_empty()
            && !o.is_empty()
            && !matches!(
              self.ctrl,
              Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT)
            )
          {
            self.add_error(format!("expected empty map, got {}", self.json));
            return Ok(());
          }

          #[allow(clippy::needless_collect)]
          let o = o.keys().cloned().collect::<Vec<_>>();

          self.visit_group(group)?;

          if self.values_to_validate.is_none() {
            for k in o.into_iter() {
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
          self.add_error(format!("expected map object {}, got {}", t2, self.json));
          Ok(())
        }
      },
      Type2::Array { group, .. } => match &self.json {
        Value::Array(a) => {
          if group.group_choices.len() == 1
            && group.group_choices[0].group_entries.is_empty()
            && !a.is_empty()
            && !matches!(
              self.ctrl,
              Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT)
            )
          {
            self.add_error(format!("expected empty array, got {}", self.json));
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
        // Special case for nested array validation
        _ if matches!(self.json, Value::Array(_)) => {
          self.validate_array_items(&ArrayItemToken::Group(group))?;
          Ok(())
        }
        _ => {
          self.add_error(format!("expected array type, got {}", self.json));
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
              .find(|r| r.name == ident.ident)
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
            let mut jv =
              JSONValidator::new(self.cddl, self.json.clone(), self.enabled_features.clone());
            #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
            let mut jv = JSONValidator::new(self.cddl, self.json.clone(), self.enabled_features);
            #[cfg(not(feature = "additional-controls"))]
            let mut jv = JSONValidator::new(self.cddl, self.json.clone());

            jv.generic_rules = self.generic_rules.clone();
            jv.eval_generic_rule = Some(ident.ident);
            jv.is_group_to_choice_enum = true;
            jv.is_multi_type_choice = self.is_multi_type_choice;
            jv.visit_rule(rule)?;

            self.errors.append(&mut jv.errors);

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
              .find(|r| r.name == ident.ident)
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
            let mut jv =
              JSONValidator::new(self.cddl, self.json.clone(), self.enabled_features.clone());
            #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
            let mut jv = JSONValidator::new(self.cddl, self.json.clone(), self.enabled_features);
            #[cfg(not(feature = "additional-controls"))]
            let mut jv = JSONValidator::new(self.cddl, self.json.clone());

            jv.generic_rules = self.generic_rules.clone();
            jv.eval_generic_rule = Some(ident.ident);
            jv.is_multi_type_choice = self.is_multi_type_choice;
            jv.visit_rule(rule)?;

            self.errors.append(&mut jv.errors);

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
            let mut jv =
              JSONValidator::new(self.cddl, self.json.clone(), self.enabled_features.clone());
            #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
            let mut jv = JSONValidator::new(self.cddl, self.json.clone(), self.enabled_features);
            #[cfg(not(feature = "additional-controls"))]
            let mut jv = JSONValidator::new(self.cddl, self.json.clone());

            jv.generic_rules = self.generic_rules.clone();
            jv.eval_generic_rule = Some(ident.ident);
            jv.is_multi_type_choice = self.is_multi_type_choice;
            jv.visit_rule(rule)?;

            self.errors.append(&mut jv.errors);

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
      #[cfg(feature = "ast-span")]
      Type2::Any { .. } => Ok(()),
      #[cfg(not(feature = "ast-span"))]
      Type2::Any {} => Ok(()),
      #[cfg(feature = "additional-controls")]
      Type2::B16ByteString { value, .. } => {
        if let Some(ctrl) = &self.ctrl {
          // For B16ByteString, the value contains the hex string as bytes (ASCII representation)
          // We need to decode it to get the actual bytes
          let hex_str = match std::str::from_utf8(value) {
            Ok(s) => s,
            Err(_) => {
              self.add_error("invalid UTF-8 in hex string".to_string());
              return Ok(());
            }
          };

          match hex::decode(hex_str.replace(' ', "")) {
            Ok(actual_bytes) => match ctrl {
              ControlOperator::B64U => self.validate_b64_control(&actual_bytes, false, false),
              ControlOperator::B64C => self.validate_b64_control(&actual_bytes, true, false),
              ControlOperator::B64USLOPPY => self.validate_b64_control(&actual_bytes, false, true),
              ControlOperator::B64CSLOPPY => self.validate_b64_control(&actual_bytes, true, true),
              ControlOperator::HEX => self.validate_hex_control(&actual_bytes, false),
              ControlOperator::HEXLC => self.validate_hex_control(&actual_bytes, true),
              ControlOperator::HEXUC => self.validate_hex_control(&actual_bytes, true),
              _ => {
                self.add_error(format!(
                  "unsupported byte string data type for JSON validation with control {}, got {}",
                  ctrl, t2
                ));
                Ok(())
              }
            },
            Err(_) => {
              self.add_error("invalid hex encoding in byte string".to_string());
              Ok(())
            }
          }
        } else {
          self.add_error(format!(
            "byte string data type not supported for JSON validation without control, got {}",
            t2
          ));
          Ok(())
        }
      }
      #[cfg(feature = "additional-controls")]
      Type2::UTF8ByteString { value, .. } => {
        if let Some(ctrl) = &self.ctrl {
          match ctrl {
            ControlOperator::B64U => self.validate_b64_control(value, false, false),
            ControlOperator::B64C => self.validate_b64_control(value, true, false),
            ControlOperator::B64USLOPPY => self.validate_b64_control(value, false, true),
            ControlOperator::B64CSLOPPY => self.validate_b64_control(value, true, true),
            ControlOperator::HEX => self.validate_hex_control(value, false),
            ControlOperator::HEXLC => self.validate_hex_control(value, true),
            ControlOperator::HEXUC => self.validate_hex_control(value, true),
            _ => {
              self.add_error(format!(
                "unsupported byte string data type for JSON validation with control {}, got {}",
                ctrl, t2
              ));
              Ok(())
            }
          }
        } else {
          self.add_error(format!(
            "byte string data type not supported for JSON validation without control, got {}",
            t2
          ));
          Ok(())
        }
      }
      #[cfg(feature = "additional-controls")]
      Type2::B64ByteString { value, .. } => {
        if let Some(ctrl) = &self.ctrl {
          match ctrl {
            ControlOperator::B64U => self.validate_b64_control(value, false, false),
            ControlOperator::B64C => self.validate_b64_control(value, true, false),
            ControlOperator::B64USLOPPY => self.validate_b64_control(value, false, true),
            ControlOperator::B64CSLOPPY => self.validate_b64_control(value, true, true),
            ControlOperator::HEX => self.validate_hex_control(value, false),
            ControlOperator::HEXLC => self.validate_hex_control(value, true),
            ControlOperator::HEXUC => self.validate_hex_control(value, true),
            _ => {
              self.add_error(format!(
                "unsupported byte string data type for JSON validation with control {}, got {}",
                ctrl, t2
              ));
              Ok(())
            }
          }
        } else {
          self.add_error(format!(
            "byte string data type not supported for JSON validation without control, got {}",
            t2
          ));
          Ok(())
        }
      }
      _ => {
        self.add_error(format!(
          "unsupported data type for validating JSON, got {}",
          t2
        ));
        Ok(())
      }
    }
  }

  fn visit_identifier(&mut self, ident: &Identifier<'a>) -> visitor::Result<Error> {
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

    // Check for recursion before processing the identifier
    if self.visited_rules.contains(ident.ident) {
      // We've seen this rule before in the current validation path, indicating a cycle
      self.add_error(format!(
        "Recursive rule reference detected: {}. This may indicate a circular definition in the CDDL schema.",
        ident.ident
      ));
      return Ok(());
    }

    // self.is_colon_shortcut_present is only true when the ident is part of a
    // member key
    if !self.is_colon_shortcut_present {
      if let Some(r) = rule_from_ident(self.cddl, ident) {
        // Mark this rule as visited
        self.visited_rules.insert(ident.ident.to_string());
        let result = self.visit_rule(r);
        // Remove this rule from visited set when we're done
        self.visited_rules.remove(ident.ident);
        return result;
      }
    }

    if is_ident_any_type(self.cddl, ident) {
      return Ok(());
    }

    // Special case for array values - check if we're in an array context and this
    // is a reference to another array type
    if let Value::Array(_) = &self.json {
      if let Some(Rule::Type { rule, .. }) = rule_from_ident(self.cddl, ident) {
        for tc in rule.value.type_choices.iter() {
          if let Type2::Array { .. } = &tc.type1.type2 {
            // Mark this rule as visited for array type processing
            self.visited_rules.insert(ident.ident.to_string());
            let result = self.visit_type_choice(tc);
            // Remove this rule from visited set when we're done
            self.visited_rules.remove(ident.ident);
            return result;
          }
        }
      }
    }

    match &self.json {
      Value::Null if is_ident_null_data_type(self.cddl, ident) => Ok(()),
      Value::Bool(b) => {
        if is_ident_bool_data_type(self.cddl, ident) {
          return Ok(());
        }

        if ident_matches_bool_value(self.cddl, ident, *b) {
          return Ok(());
        }

        self.add_error(format!("expected type {}, got {}", ident, self.json));
        Ok(())
      }
      Value::Number(n) => {
        if is_ident_uint_data_type(self.cddl, ident) && n.is_u64() {
          return Ok(());
        } else if is_ident_nint_data_type(self.cddl, ident) {
          if let Some(n) = n.as_i64() {
            if n.is_negative() {
              return Ok(());
            }
          }
        } else if is_ident_time_data_type(self.cddl, ident) {
          if let Some(n) = n.as_i64() {
            if let chrono::LocalResult::None = Utc.timestamp_millis_opt(n * 1000) {
              self.add_error(format!(
                "expected time data type, invalid UNIX timestamp {}",
                n,
              ));
            }

            return Ok(());
          } else if let Some(n) = n.as_f64() {
            // truncates fractional milliseconds when validating
            if let chrono::LocalResult::None = Utc.timestamp_millis_opt((n * 1000f64) as i64) {
              self.add_error(format!(
                "expected time data type, invalid UNIX timestamp {}",
                n,
              ));
            }
          }
        } else if (is_ident_integer_data_type(self.cddl, ident) && n.is_i64())
          || (is_ident_float_data_type(self.cddl, ident) && n.is_f64())
        {
          return Ok(());
        }

        self.add_error(format!("expected type {}, got {}", ident, self.json));
        Ok(())
      }
      Value::String(s) => {
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
          self.add_error(format!("expected type {}, got {}", ident, self.json));
        }

        Ok(())
      }
      Value::Array(_) => {
        // For arrays, check if this identifier refers to an array type rule first
        if let Some(r) = rule_from_ident(self.cddl, ident) {
          if is_array_type_rule(self.cddl, ident) {
            // Mark this rule as visited for array type processing
            self.visited_rules.insert(ident.ident.to_string());
            let result = self.visit_rule(r);
            // Remove this rule from visited set when we're done
            self.visited_rules.remove(ident.ident);
            return result;
          }
        }

        // Then fallback to normal array validation
        self.validate_array_items(&ArrayItemToken::Identifier(ident))
      }
      Value::Object(o) => match &self.occurrence {
        #[cfg(feature = "ast-span")]
        Some(Occur::Optional { .. }) | None => {
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
          if is_ident_string_data_type(self.cddl, ident) {
            let values_to_validate = o
              .iter()
              .filter_map(|(k, v)| match &self.validated_keys {
                Some(keys) if !keys.contains(k) => Some(v.clone()),
                Some(_) => None,
                None => Some(v.clone()),
              })
              .collect::<Vec<_>>();

            self.values_to_validate = Some(values_to_validate);
          }

          #[cfg(feature = "ast-span")]
          if let Occur::ZeroOrMore { .. } | Occur::OneOrMore { .. } = occur {
            if let Occur::OneOrMore { .. } = occur {
              if o.is_empty() {
                self.add_error(format!(
                  "object cannot be empty, one or more entries with key type {} required",
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
              if o.is_empty() {
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

          Ok(())
        }
      },
      _ => {
        if let Some(cut_value) = self.cut_value.take() {
          self.add_error(format!(
            "cut present for member key {}. expected type {}, got {}",
            cut_value, ident, self.json
          ));
        } else {
          self.add_error(format!("expected type {}, got {}", ident, self.json));
        }
        Ok(())
      }
    }
  }

  fn visit_value_member_key_entry(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
  ) -> visitor::Result<Error> {
    if let Some(occur) = &entry.occur {
      self.visit_occurrence(occur)?;
    }

    let current_location = self.json_location.clone();

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
        let mut jv = JSONValidator::new(self.cddl, v.clone(), self.enabled_features.clone());
        #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
        let mut jv = JSONValidator::new(self.cddl, v.clone(), self.enabled_features);
        #[cfg(not(feature = "additional-controls"))]
        let mut jv = JSONValidator::new(self.cddl, v.clone());

        jv.generic_rules = self.generic_rules.clone();
        jv.eval_generic_rule = self.eval_generic_rule;
        jv.is_multi_type_choice = self.is_multi_type_choice;
        jv.is_multi_group_choice = self.is_multi_group_choice;
        jv.json_location.push_str(&self.json_location);
        jv.type_group_name_entry = self.type_group_name_entry;
        jv.visit_type(&entry.entry_type)?;

        self.json_location = current_location.clone();

        self.errors.append(&mut jv.errors);
        if entry.occur.is_some() {
          self.occurrence = None;
        }
      }

      return Ok(());
    }

    if let Some(v) = self.object_value.take() {
      #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
      let mut jv = JSONValidator::new(self.cddl, v, self.enabled_features.clone());
      #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
      let mut jv = JSONValidator::new(self.cddl, v, self.enabled_features);
      #[cfg(not(feature = "additional-controls"))]
      let mut jv = JSONValidator::new(self.cddl, v);

      jv.generic_rules = self.generic_rules.clone();
      jv.eval_generic_rule = self.eval_generic_rule;
      jv.is_multi_type_choice = self.is_multi_type_choice;
      jv.is_multi_group_choice = self.is_multi_group_choice;
      jv.json_location.push_str(&self.json_location);
      jv.type_group_name_entry = self.type_group_name_entry;
      jv.visit_type(&entry.entry_type)?;

      self.json_location = current_location;

      self.errors.append(&mut jv.errors);
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
  ) -> visitor::Result<Error> {
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
        let mut jv =
          JSONValidator::new(self.cddl, self.json.clone(), self.enabled_features.clone());
        #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
        let mut jv = JSONValidator::new(self.cddl, self.json.clone(), self.enabled_features);
        #[cfg(not(feature = "additional-controls"))]
        let mut jv = JSONValidator::new(self.cddl, self.json.clone());

        jv.generic_rules = self.generic_rules.clone();
        jv.eval_generic_rule = Some(entry.name.ident);
        jv.is_multi_type_choice = self.is_multi_type_choice;
        jv.visit_rule(rule)?;

        self.errors.append(&mut jv.errors);

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

  fn visit_memberkey(&mut self, mk: &MemberKey<'a>) -> visitor::Result<Error> {
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

  fn visit_value(&mut self, value: &token::Value<'a>) -> visitor::Result<Error> {
    // FIXME: If during traversal the type being validated is supposed to be a value,
    // this fails
    if let Value::Array(_) = &self.json {
      return self.validate_array_items(&ArrayItemToken::Value(value));
    }

    if let Value::Object(_) = &self.json {
      return self.validate_object_value(value);
    }

    let error: Option<String> = match value {
      token::Value::INT(v) => match &self.json {
        Value::Number(n) => match n.as_i64() {
          Some(i) => match &self.ctrl {
            Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT) if i != *v as i64 => None,
            Some(ControlOperator::LT) if i < *v as i64 => None,
            Some(ControlOperator::LE) if i <= *v as i64 => None,
            Some(ControlOperator::GT) if i > *v as i64 => None,
            Some(ControlOperator::GE) if i >= *v as i64 => None,
            #[cfg(feature = "additional-controls")]
            Some(ControlOperator::PLUS) => {
              if i == *v as i64 {
                None
              } else {
                Some(format!("expected computed .plus value {}, got {}", v, n))
              }
            }
            #[cfg(feature = "additional-controls")]
            None | Some(ControlOperator::FEATURE) => {
              if i == *v as i64 {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            #[cfg(not(feature = "additional-controls"))]
            None => {
              if i == *v as i64 {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            _ => Some(format!(
              "expected value {} {}, got {}",
              self.ctrl.unwrap(),
              v,
              n
            )),
          },
          None => Some(format!("{} cannot be represented as an i64", n)),
        },
        _ => Some(format!("expected value {}, got {}", v, self.json)),
      },
      token::Value::UINT(v) => match &self.json {
        Value::Number(n) => match n.as_u64() {
          Some(i) => match &self.ctrl {
            Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT) if i != *v as u64 => None,
            Some(ControlOperator::LT) if i < *v as u64 => None,
            Some(ControlOperator::LE) if i <= *v as u64 => None,
            Some(ControlOperator::GT) if i > *v as u64 => None,
            Some(ControlOperator::GE) if i >= *v as u64 => None,
            Some(ControlOperator::SIZE) => match 256u128.checked_pow(*v as u32) {
              Some(n) if (i as u128) < n => None,
              _ => Some(format!("expected value .size {}, got {}", v, n)),
            },
            #[cfg(feature = "additional-controls")]
            Some(ControlOperator::PLUS) => {
              if i == *v as u64 {
                None
              } else {
                Some(format!("expected computed .plus value {}, got {}", v, n))
              }
            }
            #[cfg(feature = "additional-controls")]
            None | Some(ControlOperator::FEATURE) => {
              if i == *v as u64 {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            #[cfg(not(feature = "additional-controls"))]
            None => {
              if i == *v as u64 {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            _ => Some(format!(
              "expected value {} {}, got {}",
              self.ctrl.unwrap(),
              v,
              n
            )),
          },
          None => Some(format!("{} cannot be represented as a u64", n)),
        },
        Value::String(s) => match &self.ctrl {
          Some(ControlOperator::SIZE) => {
            if s.len() == *v {
              None
            } else {
              Some(format!("expected \"{}\" .size {}, got {}", s, v, s.len()))
            }
          }
          _ => Some(format!("expected {}, got {}", v, s)),
        },
        _ => Some(format!("expected value {}, got {}", v, self.json)),
      },
      token::Value::FLOAT(v) => match &self.json {
        Value::Number(n) => match n.as_f64() {
          Some(f) => match &self.ctrl {
            Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT)
              if (f - *v).abs() > f64::EPSILON =>
            {
              None
            }
            Some(ControlOperator::LT) if f < *v => None,
            Some(ControlOperator::LE) if f <= *v => None,
            Some(ControlOperator::GT) if f > *v => None,
            Some(ControlOperator::GE) if f >= *v => None,
            #[cfg(feature = "additional-controls")]
            Some(ControlOperator::PLUS) => {
              if (f - *v).abs() < f64::EPSILON {
                None
              } else {
                Some(format!("expected computed .plus value {}, got {}", v, n))
              }
            }
            #[cfg(feature = "additional-controls")]
            None | Some(ControlOperator::FEATURE) => {
              if (f - *v).abs() < f64::EPSILON {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            #[cfg(not(feature = "additional-controls"))]
            None => {
              if (f - *v).abs() < f64::EPSILON {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            _ => Some(format!(
              "expected value {} {}, got {}",
              self.ctrl.unwrap(),
              v,
              n
            )),
          },
          None => Some(format!("{} cannot be represented as an i64", n)),
        },
        _ => Some(format!("expected value {}, got {}", v, self.json)),
      },
      token::Value::TEXT(t) => match &self.json {
        Value::String(s) => match &self.ctrl {
          Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT) => {
            if s != t {
              None
            } else {
              Some(format!("expected {} .ne to \"{}\"", value, s))
            }
          }
          Some(ControlOperator::REGEXP) | Some(ControlOperator::PCRE) => {
            let re = regex::Regex::new(
              &format_regex(
                // Text strings must be JSON escaped per
                // https://datatracker.ietf.org/doc/html/rfc8610#section-3.1
                serde_json::from_str::<Value>(&format!("\"{}\"", t))
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
        _ => Some(format!("expected value {}, got {}", t, self.json)),
      },
      token::Value::BYTE(token::ByteValue::UTF8(b)) => match &self.json {
        Value::String(s) if s.as_bytes() == b.as_ref() => None,
        _ => Some(format!("expected byte value {:?}, got {}", b, self.json)),
      },
      token::Value::BYTE(token::ByteValue::B16(b)) => match &self.json {
        Value::String(s) if s.as_bytes() == b.as_ref() => None,
        _ => Some(format!("expected byte value {:?}, got {}", b, self.json)),
      },
      token::Value::BYTE(token::ByteValue::B64(b)) => match &self.json {
        Value::String(s) if s.as_bytes() == b.as_ref() => None,
        _ => Some(format!("expected byte value {:?}, got {}", b, self.json)),
      },
    };

    if let Some(e) = error {
      self.add_error(e);
    }

    Ok(())
  }

  fn visit_occurrence(&mut self, o: &Occurrence) -> visitor::Result<Error> {
    self.occurrence = Some(o.occur);

    Ok(())
  }
}

// Helper function to check if an identifier refers to an array type rule
fn is_array_type_rule<'a>(cddl: &'a CDDL<'a>, ident: &Identifier<'a>) -> bool {
  for r in cddl.rules.iter() {
    if let Rule::Type { rule, .. } = r {
      if rule.name.ident == ident.ident {
        // Check if the type rule defines an array
        for tc in rule.value.type_choices.iter() {
          if let Type2::Array { .. } = &tc.type1.type2 {
            return true;
          }
        }
      }
    }
  }
  false
}

#[cfg(test)]
#[cfg(not(target_arch = "wasm32"))]
mod tests {
  #![allow(unused_imports)]

  use super::*;
  use indoc::indoc;

  #[cfg(feature = "additional-controls")]
  #[test]
  fn validate_plus() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        interval<BASE> = (
          "test" => BASE .plus a
        )
    
        rect = {
          interval<X>
        }
        X = 0
        a = 10
      "#
    );
    let json = r#"{ "test": 10 }"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();
    let mut jv = JSONValidator::new(&cddl, json, None);
    jv.validate()?;

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn validate_feature() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        v = JC<"v", 2>
        JC<J, C> =  C .feature "cbor" / J .feature "json"
      "#
    );

    let json = r#""v""#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(&cddl, json, Some(&["json"]));
    jv.validate()?;

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

    let json = r#"[ 13 ]"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(&cddl, json, None);
    jv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_group_choice_alternate() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        tester = $$vals
        $$vals //= 18
        $$vals //= 12
      "#
    );

    let json = r#"15"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(&cddl, json, None);
    jv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_group_choice_alternate_in_array_1(
  ) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        tester = [$$val]
        $$val //= (
          type: 10,
          data: uint,
          t: 11
        )
        $$val //= (
          type: 11,
          data: tstr
        )
      "#
    );

    let json = r#"[10, 11, 11]"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(&cddl, json, None);
    jv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_group_choice_alternate_in_array_2(
  ) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        tester = [$$val]
        $$val //= (
          type: 10,
          extra,
        )
        extra = (
          something: uint,
        )
      "#
    );

    let json = r#"[10, 1]"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(&cddl, json, None);
    jv.validate()?;

    Ok(())
  }

  #[test]
  fn size_control_validation_error() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        start = Record
        Record = {
          id: Id
        }
        Id = uint .size 8
      "#
    );

    let json = r#"{ "id": 5 }"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(&cddl, json, None);
    jv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_occurrences_in_object() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        limited = { 1* tstr => tstr }
      "#
    );

    let json = r#"{ "A": "B" }"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(&cddl, json, None);
    jv.validate().unwrap();

    Ok(())
  }

  #[test]
  fn validate_optional_occurrences_in_object() -> std::result::Result<(), Box<dyn std::error::Error>>
  {
    let cddl = indoc!(
      r#"
        argument = {
          name: text,
          ? valid: "yes" / "no",
        }
      "#
    );

    let json = r#"{
      "name": "foo",
      "valid": "no"
    }"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(&cddl, json, None);
    jv.validate().unwrap();

    Ok(())
  }

  #[test]
  fn validate_range_operators() -> std::result::Result<(), Box<dyn std::error::Error>> {
    // Test inclusive range (..)
    let cddl = indoc!(
      r#"
        thing = 5..10
        "#
    );

    let json = serde_json::json!(5); // Should pass
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(10); // Should pass
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(4); // Should fail
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_err());

    let json = serde_json::json!(11); // Should fail
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_err());

    // Test inclusive-exclusive range (...)
    let cddl = indoc!(
      r#"
        thing = 5...10
        "#
    );
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;

    let json = serde_json::json!(5); // Should pass
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(9); // Should pass
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(10); // Should fail (exclusive upper bound)
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_err());

    let json = serde_json::json!(4); // Should fail
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_err());

    // Test inclusive-exclusive range with floats
    let cddl = indoc!(
      r#"
        thing = 1.5...3.5
        "#
    );
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;

    let json = serde_json::json!(1.5); // Should pass
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(2.5); // Should pass
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(3.5); // Should fail (exclusive upper bound)
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_err());

    let json = serde_json::json!(1.0); // Should fail
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_err());

    // Test range with string length using .size control
    let cddl = indoc!(
      r#"
        thing = tstr .size (2...5)
        "#
    );
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;

    let json = serde_json::json!("ab"); // Length 2 - should pass
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_ok());

    let json = serde_json::json!("abcd"); // Length 4 - should pass
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_ok());

    let json = serde_json::json!("abcde"); // Length 5 - should fail (exclusive)
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_err());

    let json = serde_json::json!("a"); // Length 1 - should fail
    let mut jv = JSONValidator::new(&cddl, json, None);
    assert!(jv.validate().is_err());

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

    let json = r#"[0, [1, 2]]"#;
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;
    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let mut jv = JSONValidator::new(&cddl, json, None);
    jv.validate()?;

    // Test with named arrays
    let cddl = indoc!(
      r#"
        root = [0, inner]
        inner = [* int]
      "#
    );

    let json = r#"[0, [1, 2]]"#;
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;
    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let mut jv = JSONValidator::new(&cddl, json, None);
    jv.validate()?;

    // Test with explicit array literals
    let cddl = indoc!(
      r#"
        direct = [1, [2, 3]]
      "#
    );

    let json = r#"[1, [2, 3]]"#;
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;
    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let mut jv = JSONValidator::new(&cddl, json, None);
    jv.validate()?; // If this passes, our fix works

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

    let json = r#"[1, [2, 3]]"#;
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;
    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let mut jv = JSONValidator::new(&cddl, json, None);

    // Print detailed information for debugging
    match jv.validate() {
      Ok(_) => println!("Validation successful!"),
      Err(e) => {
        eprintln!("Validation error: {}", e);
        if let Error::Validation(errors) = &e {
          for (i, err) in errors.iter().enumerate() {
            eprintln!("Error {}: {} at {}", i, err.reason, err.json_location);
          }
        }
        return Err(e.into());
      }
    }

    Ok(())
  }

  #[test]
  fn test_issue_221_empty_map_with_extra_keys() {
    // This test reproduces issue #221
    // CDDL schema defines an empty map: root = {}
    // JSON has extra keys: {"x": "y"}
    // This should FAIL validation but currently passes

    let cddl_str = "root = {}";
    let json_str = r#"{"x": "y"}"#;

    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_str, json_str, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_str, json_str);

    // This should fail but currently passes (the bug)
    assert!(
      result.is_err(),
      "Validation should fail for extra keys in empty map schema"
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
  fn test_empty_map_schema_with_empty_json() {
    // This should pass - empty map schema with empty JSON map
    let cddl_str = "root = {}";
    let json_str = r#"{}"#;

    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_str, json_str, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_str, json_str);

    assert!(
      result.is_ok(),
      "Validation should pass for empty map with empty map schema"
    );
  }

  #[test]
  fn test_issue_174_choice_validation() {
    // Test for issue #174 - Choice validates in case where neither option validates
    let cddl_str = indoc!(
      r#"
        Root = Choice1 / Choice2

        Choice1 = {
           id1: text,
           ?id2: text,
           Extensible
        }

        Choice2 = {
           ?id1: text,
           id2: text,
           Extensible
        }

        Extensible = (*text => any)
      "#
    );

    // This JSON should fail because id2 is not text in either choice
    let json_str = r#"{"id1": "example", "id2": 2}"#;

    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_str, json_str, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_str, json_str);

    // This should fail validation since id2 is a number, not text
    match result {
      Err(Error::Validation(errors)) => {
        assert!(!errors.is_empty(), "Should have validation errors");
        // We expect errors related to type mismatch
      }
      Ok(_) => {
        panic!("Issue #174 bug detected: validation incorrectly passed when neither choice should validate")
      }
      Err(other) => panic!("Unexpected error type: {:?}", other),
    }

    // Test valid cases still work
    let valid_cases = [
      r#"{"id1": "example", "id2": "text"}"#, // Both fields as text
      r#"{"id1": "example"}"#,                // Only id1 (matches Choice1)
      r#"{"id2": "text"}"#,                   // Only id2 (matches Choice2)
    ];

    for valid_json in valid_cases.iter() {
      #[cfg(feature = "additional-controls")]
      let result = validate_json_from_str(cddl_str, valid_json, None);
      #[cfg(not(feature = "additional-controls"))]
      let result = validate_json_from_str(cddl_str, valid_json);

      match result {
        Ok(_) => {} // Expected
        Err(e) => panic!(
          "Valid JSON should pass validation: {}, error: {:?}",
          valid_json, e
        ),
      }
    }
  }

  #[test]
  fn test_issue_221_reproduce_exact_scenario() {
    // This test reproduces the exact scenario from issue #221
    let cddl_str = "root = {}";
    let json_str = r#"{"x": "y"}"#;

    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_str, json_str, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_str, json_str);

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
      Ok(_) => {
        panic!("Issue #221 bug detected: validation incorrectly passed for extra keys in empty map")
      }
      Err(other) => panic!("Unexpected error type: {:?}", other),
    }
  }
}
