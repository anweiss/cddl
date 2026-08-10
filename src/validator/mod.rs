#![cfg(not(feature = "lsp"))]

/// CBOR validation implementation
pub mod cbor;
/// Custom CBOR value type with simple value support
pub mod cbor_value;
/// CSV validation implementation (draft-bormann-cbor-cddl-csv-08)
pub mod csv_validator;
/// JSON validation implementation
pub mod json;

mod control;

use crate::{
  ast::{
    Group, GroupChoice, GroupEntry, GroupRule, Identifier, Occur, Rule, Type, Type1, Type2,
    TypeChoice, TypeRule, CDDL,
  },
  token::{self, *},
  visitor::Visitor,
};

use std::collections::HashSet;
use std::error::Error;

#[cfg(feature = "cbor")]
use cbor::CBORValidator;
#[cfg(feature = "cbor")]
use cbor_value::decode_cbor;
#[cfg(feature = "json")]
use json::JSONValidator;

#[cfg(target_arch = "wasm32")]
use crate::{error::ErrorMsg, lexer::Position, parser, pest_bridge};
#[cfg(target_arch = "wasm32")]
use serde::Serialize;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
#[derive(Serialize)]
struct ParserError {
  #[cfg(feature = "ast-span")]
  position: Position,
  msg: ErrorMsg,
}

#[cfg(not(target_arch = "wasm32"))]
use crate::cddl_from_str;

/// Validator trait. Implemented for JSON documents and CBOR binaries
pub trait Validator<'a, 'b, E: Error>: Visitor<'a, 'b, E> {
  /// Validate the target
  fn validate(&mut self) -> std::result::Result<(), E>;
  /// Collect validation errors
  fn add_error(&mut self, reason: String);
}

/// Generic rule representation used during validation.
///
/// Tracks a named rule along with its generic parameters and the concrete
/// type arguments it has been instantiated with during AST evaluation.
#[derive(Clone, Debug)]
pub struct GenericRule<'a> {
  /// Rule name
  pub name: &'a str,
  /// Generic parameter names
  pub params: Vec<&'a str>,
  /// Concrete type arguments for this instantiation
  pub args: Vec<Type1<'a>>,
}

/// Shared validation state used by all format-specific validators.
///
/// This struct contains the common CDDL AST tracking fields that are
/// identical across JSON, CBOR, and other validators. By composing with
/// this struct, new validators can reuse all the tracking infrastructure
/// without duplicating the ~25 fields needed for proper CDDL evaluation.
///
/// # Creating a new validator
///
/// To create a new format-specific validator:
///
/// 1. Define a struct containing `state: ValidationState<'a>` and your
///    format-specific fields (e.g., the data value, validated keys, errors).
/// 2. Implement `Deref<Target = ValidationState<'a>>` and `DerefMut` to
///    enable transparent access to the shared state fields.
/// 3. Implement the `Visitor` trait, providing format-specific logic in
///    methods like `visit_identifier` and `visit_value`.
/// 4. Implement the `Validator` trait for your entry point.
///
/// The shared state handles occurrence tracking, generic rule management,
/// control operator tracking, feature flags, and recursion detection — all
/// of which are identical across validators.
#[derive(Clone)]
pub struct ValidationState<'a> {
  /// Reference to the CDDL AST being validated against
  pub cddl: &'a CDDL<'a>,
  /// Current location in the CDDL document
  pub cddl_location: String,
  /// Current location in the data being validated (e.g., JSON Pointer or
  /// CBOR path). Uses a generic name so validators for any format can share
  /// the same field.
  pub data_location: String,
  /// Occurrence indicator detected in current state of AST evaluation
  pub occurrence: Option<Occur>,
  /// Is member key detected in current state of AST evaluation
  pub is_member_key: bool,
  /// Is a cut detected in current state of AST evaluation
  pub is_cut_present: bool,
  /// Validate the generic rule given by str ident in current state of AST
  /// evaluation
  pub eval_generic_rule: Option<&'a str>,
  /// Aggregation of generic rules
  pub generic_rules: Vec<GenericRule<'a>>,
  /// Control operator token detected in current state of AST evaluation
  pub ctrl: Option<token::ControlOperator>,
  /// Is a group to choice enumeration detected in current state of AST
  /// evaluation
  pub is_group_to_choice_enum: bool,
  /// Are 2 or more type choices detected in current state of AST evaluation
  pub is_multi_type_choice: bool,
  /// Are 2 or more group choices detected in current state of AST evaluation
  pub is_multi_group_choice: bool,
  /// Type/group name entry detected in current state of AST evaluation. Used
  /// only for providing more verbose error messages
  pub type_group_name_entry: Option<&'a str>,
  /// Whether or not to advance to the next group entry if member key
  /// validation fails as detected during the current state of AST evaluation
  pub advance_to_next_entry: bool,
  /// Is validation checking for map equality
  pub is_ctrl_map_equality: bool,
  /// Is colon shortcut present in member key
  pub is_colon_shortcut_present: bool,
  /// Is the current rule the root rule
  pub is_root: bool,
  /// Is multi type choice type rule validating an array
  pub is_multi_type_choice_type_rule_validating_array: bool,
  /// Track visited rules to prevent infinite recursion during validation
  pub visited_rules: HashSet<String>,
  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(feature = "additional-controls")]
  /// Enabled features for validation
  pub enabled_features: Option<&'a [&'a str]>,
  #[cfg(target_arch = "wasm32")]
  #[cfg(feature = "additional-controls")]
  /// Enabled features for validation (WASM)
  pub enabled_features: Option<Box<[JsValue]>>,
  #[cfg(feature = "additional-controls")]
  /// Whether feature-related errors have been detected
  pub has_feature_errors: bool,
  #[cfg(feature = "additional-controls")]
  /// Disabled features encountered during validation
  pub disabled_features: Option<Vec<String>>,
}

impl<'a> ValidationState<'a> {
  /// Create a new `ValidationState` with default values.
  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(feature = "additional-controls")]
  pub fn new(cddl: &'a CDDL<'a>, enabled_features: Option<&'a [&'a str]>) -> Self {
    ValidationState {
      cddl,
      cddl_location: String::new(),
      data_location: String::new(),
      occurrence: None,
      is_member_key: false,
      is_cut_present: false,
      eval_generic_rule: None,
      generic_rules: Vec::new(),
      ctrl: None,
      is_group_to_choice_enum: false,
      is_multi_type_choice: false,
      is_multi_group_choice: false,
      type_group_name_entry: None,
      advance_to_next_entry: false,
      is_ctrl_map_equality: false,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      visited_rules: HashSet::new(),
      enabled_features,
      has_feature_errors: false,
      disabled_features: None,
    }
  }

  /// Create a new `ValidationState` with default values.
  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(not(feature = "additional-controls"))]
  pub fn new(cddl: &'a CDDL<'a>) -> Self {
    ValidationState {
      cddl,
      cddl_location: String::new(),
      data_location: String::new(),
      occurrence: None,
      is_member_key: false,
      is_cut_present: false,
      eval_generic_rule: None,
      generic_rules: Vec::new(),
      ctrl: None,
      is_group_to_choice_enum: false,
      is_multi_type_choice: false,
      is_multi_group_choice: false,
      type_group_name_entry: None,
      advance_to_next_entry: false,
      is_ctrl_map_equality: false,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      visited_rules: HashSet::new(),
    }
  }

  /// Create a new `ValidationState` with default values.
  #[cfg(target_arch = "wasm32")]
  #[cfg(feature = "additional-controls")]
  pub fn new(cddl: &'a CDDL<'a>, enabled_features: Option<Box<[JsValue]>>) -> Self {
    ValidationState {
      cddl,
      cddl_location: String::new(),
      data_location: String::new(),
      occurrence: None,
      is_member_key: false,
      is_cut_present: false,
      eval_generic_rule: None,
      generic_rules: Vec::new(),
      ctrl: None,
      is_group_to_choice_enum: false,
      is_multi_type_choice: false,
      is_multi_group_choice: false,
      type_group_name_entry: None,
      advance_to_next_entry: false,
      is_ctrl_map_equality: false,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      visited_rules: HashSet::new(),
      enabled_features,
      has_feature_errors: false,
      disabled_features: None,
    }
  }

  /// Create a new `ValidationState` with default values.
  #[cfg(target_arch = "wasm32")]
  #[cfg(not(feature = "additional-controls"))]
  pub fn new(cddl: &'a CDDL<'a>) -> Self {
    ValidationState {
      cddl,
      cddl_location: String::new(),
      data_location: String::new(),
      occurrence: None,
      is_member_key: false,
      is_cut_present: false,
      eval_generic_rule: None,
      generic_rules: Vec::new(),
      ctrl: None,
      is_group_to_choice_enum: false,
      is_multi_type_choice: false,
      is_multi_group_choice: false,
      type_group_name_entry: None,
      advance_to_next_entry: false,
      is_ctrl_map_equality: false,
      is_colon_shortcut_present: false,
      is_root: false,
      is_multi_type_choice_type_rule_validating_array: false,
      visited_rules: HashSet::new(),
    }
  }
}

impl CDDL<'_> {
  /// Validate the given document against the CDDL definition
  fn validate_json(
    &self,
    document: &[u8],
    #[cfg(feature = "additional-controls")]
    #[cfg(not(target_arch = "wasm32"))]
    enabled_features: Option<&[&str]>,
    #[cfg(feature = "additional-controls")]
    #[cfg(target_arch = "wasm32")]
    enabled_features: Option<Box<[JsValue]>>,
  ) -> Result<(), Box<dyn Error>> {
    let json =
      serde_json::from_slice::<serde_json::Value>(document).map_err(json::Error::JSONParsing)?;

    #[cfg(feature = "additional-controls")]
    let mut jv = JSONValidator::new(self, json, enabled_features);
    #[cfg(not(feature = "additional-controls"))]
    let mut jv = JSONValidator::new(&cddl, json);

    jv.validate().map_err(|e| e.into())
  }

  fn validate_cbor(
    &self,
    document: &[u8],
    #[cfg(feature = "additional-controls")]
    #[cfg(not(target_arch = "wasm32"))]
    enabled_features: Option<&[&str]>,
    #[cfg(feature = "additional-controls")]
    #[cfg(target_arch = "wasm32")]
    enabled_features: Option<Box<[JsValue]>>,
  ) -> Result<(), Box<dyn Error>> {
    let cbor = decode_cbor(document).map_err(|e| e.to_string())?;

    let mut cv = CBORValidator::new(self, cbor, enabled_features);
    cv.validate().map_err(|e| e.into())
  }
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "json")]
/// Validate JSON string from a given CDDL document string
pub fn validate_json_from_str(
  cddl: &str,
  json: &str,
  #[cfg(feature = "additional-controls")] enabled_features: Option<&[&str]>,
) -> json::Result {
  let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;
  let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

  #[cfg(feature = "additional-controls")]
  let mut jv = JSONValidator::new(&cddl, json, enabled_features);
  #[cfg(not(feature = "additional-controls"))]
  let mut jv = JSONValidator::new(&cddl, json);

  jv.validate()
}

#[cfg(target_arch = "wasm32")]
#[cfg(feature = "additional-controls")]
#[cfg(feature = "json")]
#[wasm_bindgen]
/// Validate JSON string from a given CDDL document string
pub fn validate_json_from_str(
  cddl: &str,
  json: &str,
  enabled_features: Option<Box<[JsValue]>>,
) -> std::result::Result<JsValue, JsValue> {
  let c = pest_bridge::cddl_from_pest_str(cddl).map_err(|e| {
    if let parser::Error::PARSER {
      #[cfg(feature = "ast-span")]
      position,
      msg,
    } = &e
    {
      let errors = vec![ParserError {
        #[cfg(feature = "ast-span")]
        position: *position,
        msg: msg.clone(),
      }];
      serde_wasm_bindgen::to_value(&errors).unwrap_or_else(|e| JsValue::from(e.to_string()))
    } else {
      JsValue::from(e.to_string())
    }
  })?;

  let json =
    serde_json::from_str::<serde_json::Value>(json).map_err(|e| JsValue::from(e.to_string()))?;

  let mut jv = JSONValidator::new(&c, json, enabled_features);
  jv.validate()
    .map_err(|e| JsValue::from(e.to_string()))
    .map(|_| JsValue::default())
}

#[cfg(target_arch = "wasm32")]
#[cfg(feature = "json")]
#[cfg(not(feature = "additional-controls"))]
#[wasm_bindgen]
/// Validate JSON string from a given CDDL document string
pub fn validate_json_from_str(cddl: &str, json: &str) -> std::result::Result<JsValue, JsValue> {
  let c = pest_bridge::cddl_from_pest_str(cddl).map_err(|e| {
    if let parser::Error::PARSER {
      #[cfg(feature = "ast-span")]
      position,
      msg,
    } = &e
    {
      let errors = vec![ParserError {
        #[cfg(feature = "ast-span")]
        position: *position,
        msg: msg.clone(),
      }];
      serde_wasm_bindgen::to_value(&errors).unwrap_or_else(|e| JsValue::from(e.to_string()))
    } else {
      JsValue::from(e.to_string())
    }
  })?;

  let json =
    serde_json::from_str::<serde_json::Value>(json).map_err(|e| JsValue::from(e.to_string()))?;

  let mut jv = JSONValidator::new(&c, json);
  jv.validate()
    .map_err(|e| JsValue::from(e.to_string()))
    .map(|_| JsValue::default())
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "cbor")]
#[cfg(feature = "additional-controls")]
/// Validate CBOR slice from a given CDDL document string
pub fn validate_cbor_from_slice(
  cddl: &str,
  cbor_slice: &[u8],
  enabled_features: Option<&[&str]>,
) -> cbor::Result<std::io::Error> {
  let cddl = cddl_from_str(cddl, true).map_err(cbor::Error::CDDLParsing)?;

  let cbor = decode_cbor(cbor_slice).map_err(|e| cbor::Error::CDDLParsing(e.to_string()))?;

  let mut cv = CBORValidator::new(&cddl, cbor, enabled_features);
  cv.validate()
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "cbor")]
#[cfg(not(feature = "additional-controls"))]
/// Validate CBOR slice from a given CDDL document string
pub fn validate_cbor_from_slice(cddl: &str, cbor_slice: &[u8]) -> cbor::Result<std::io::Error> {
  let cddl = cddl_from_str(cddl, true).map_err(cbor::Error::CDDLParsing)?;
  let cbor = decode_cbor(cbor_slice).map_err(|e| cbor::Error::CDDLParsing(e.to_string()))?;

  let mut cv = CBORValidator::new(&cddl, cbor);
  cv.validate()
}

#[cfg(target_arch = "wasm32")]
#[cfg(feature = "cbor")]
#[cfg(feature = "additional-controls")]
#[wasm_bindgen]
/// Validate CBOR slice from a given CDDL document string
pub fn validate_cbor_from_slice(
  cddl: &str,
  cbor_slice: &[u8],
  enabled_features: Option<Box<[JsValue]>>,
) -> std::result::Result<JsValue, JsValue> {
  let c = pest_bridge::cddl_from_pest_str(cddl).map_err(|e| {
    if let parser::Error::PARSER {
      #[cfg(feature = "ast-span")]
      position,
      msg,
    } = &e
    {
      let errors = vec![ParserError {
        #[cfg(feature = "ast-span")]
        position: *position,
        msg: msg.clone(),
      }];
      serde_wasm_bindgen::to_value(&errors).unwrap_or_else(|e| JsValue::from(e.to_string()))
    } else {
      JsValue::from(e.to_string())
    }
  })?;

  let cbor = decode_cbor(cbor_slice).map_err(|e| JsValue::from(e.to_string()))?;

  let mut cv = CBORValidator::new(&c, cbor, enabled_features);
  cv.validate()
    .map_err(|e| JsValue::from(e.to_string()))
    .map(|_| JsValue::default())
}

#[cfg(target_arch = "wasm32")]
#[cfg(feature = "cbor")]
#[cfg(not(feature = "additional-controls"))]
#[wasm_bindgen]
/// Validate CBOR slice from a given CDDL document string
pub fn validate_cbor_from_slice(
  cddl: &str,
  cbor_slice: &[u8],
) -> std::result::Result<JsValue, JsValue> {
  let c = pest_bridge::cddl_from_pest_str(cddl).map_err(|e| {
    if let parser::Error::PARSER {
      #[cfg(feature = "ast-span")]
      position,
      msg,
    } = &e
    {
      let errors = vec![ParserError {
        #[cfg(feature = "ast-span")]
        position: *position,
        msg: msg.clone(),
      }];
      serde_wasm_bindgen::to_value(&errors).unwrap_or_else(|e| JsValue::from(e.to_string()))
    } else {
      JsValue::from(e.to_string())
    }
  })?;

  let cbor = decode_cbor(cbor_slice).map_err(|e| JsValue::from(e.to_string()))?;

  let mut cv = CBORValidator::new(&c, cbor);
  cv.validate()
    .map_err(|e| JsValue::from(e.to_string()))
    .map(|_| JsValue::default())
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "csv-validate")]
#[cfg(feature = "additional-controls")]
/// Validate CSV string from a given CDDL document string.
///
/// Implements draft-bormann-cbor-cddl-csv-08. CSV data is parsed according to
/// RFC 4180 and mapped to the CDDL generic data model. Fields are coerced to
/// their JSON representation for validation.
///
/// `has_header` indicates whether the first row is a header. Defaults to `false`.
pub fn validate_csv_from_str(
  cddl: &str,
  csv_data: &str,
  has_header: Option<bool>,
  enabled_features: Option<&[&str]>,
) -> csv_validator::Result {
  csv_validator::validate_csv_from_str(cddl, csv_data, has_header, enabled_features)
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "csv-validate")]
#[cfg(not(feature = "additional-controls"))]
/// Validate CSV string from a given CDDL document string.
///
/// Implements draft-bormann-cbor-cddl-csv-08. CSV data is parsed according to
/// RFC 4180 and mapped to the CDDL generic data model.
pub fn validate_csv_from_str(
  cddl: &str,
  csv_data: &str,
  has_header: Option<bool>,
) -> csv_validator::Result {
  csv_validator::validate_csv_from_str(cddl, csv_data, has_header)
}

/// Find non-choice alternate rule from a given identifier
pub fn rule_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Option<&'a Rule<'a>> {
  cddl.rules.iter().find(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident && !rule.is_type_choice_alternate => true,
    Rule::Group { rule, .. } if rule.name == *ident && !rule.is_group_choice_alternate => true,
    _ => false,
  })
}

/// Find text values from a given identifier
pub fn text_value_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Option<&'a Type2<'a>> {
  cddl.rules.iter().find_map(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => {
      rule.value.type_choices.iter().find_map(|tc| {
        if tc.type1.operator.is_none() {
          match &tc.type1.type2 {
            Type2::TextValue { .. } | Type2::UTF8ByteString { .. } => Some(&tc.type1.type2),
            Type2::Typename { ident, .. } => text_value_from_ident(cddl, ident),
            Type2::ParenthesizedType { pt, .. } => pt.type_choices.iter().find_map(|tc| {
              if tc.type1.operator.is_none() {
                text_value_from_type2(cddl, &tc.type1.type2)
              } else {
                None
              }
            }),
            _ => None,
          }
        } else {
          None
        }
      })
    }
    _ => None,
  })
}

/// Find text values from a given Type2
pub fn text_value_from_type2<'a>(cddl: &'a CDDL, t2: &'a Type2<'a>) -> Option<&'a Type2<'a>> {
  match t2 {
    Type2::TextValue { .. } | Type2::UTF8ByteString { .. } => Some(t2),
    Type2::Typename { ident, .. } => text_value_from_ident(cddl, ident),
    Type2::Array { group, .. } => group.group_choices.iter().find_map(|gc| {
      if gc.group_entries.len() == 2 {
        if let Some(ge) = gc.group_entries.first() {
          if let GroupEntry::ValueMemberKey { ge, .. } = &ge.0 {
            if ge.member_key.is_none() {
              ge.entry_type.type_choices.iter().find_map(|tc| {
                if tc.type1.operator.is_none() {
                  text_value_from_type2(cddl, &tc.type1.type2)
                } else {
                  None
                }
              })
            } else {
              None
            }
          } else {
            None
          }
        } else {
          None
        }
      } else {
        None
      }
    }),
    Type2::ParenthesizedType { pt, .. } => pt.type_choices.iter().find_map(|tc| {
      if tc.type1.operator.is_none() {
        text_value_from_type2(cddl, &tc.type1.type2)
      } else {
        None
      }
    }),
    _ => None,
  }
}

/// Unwrap array, map or tag type rule from ident
pub fn unwrap_rule_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Option<&'a Rule<'a>> {
  cddl.rules.iter().find_map(|r| match r {
    Rule::Type {
      rule:
        TypeRule {
          name,
          is_type_choice_alternate,
          value: Type { type_choices, .. },
          ..
        },
      ..
    } if name == ident && !is_type_choice_alternate => {
      let match_fn = |tc: &TypeChoice| {
        matches!(
          tc.type1.type2,
          Type2::Map { .. } | Type2::Array { .. } | Type2::TaggedData { .. }
        )
      };

      if type_choices.iter().any(match_fn) {
        Some(r)
      } else if let Some(ident) = type_choices.iter().find_map(|tc| {
        if let Type2::Typename {
          ident,
          generic_args: None,
          ..
        } = &tc.type1.type2
        {
          Some(ident)
        } else {
          None
        }
      }) {
        unwrap_rule_from_ident(cddl, ident)
      } else {
        None
      }
    }
    _ => None,
  })
}

/// Find non-group choice alternate rule from a given identifier
pub fn group_rule_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Option<&'a GroupRule<'a>> {
  cddl.rules.iter().find_map(|r| match r {
    Rule::Group { rule, .. } if rule.name == *ident && !rule.is_group_choice_alternate => {
      Some(rule.as_ref())
    }
    _ => None,
  })
}

/// Find non-group choice alternate rule from a given identifier
pub fn type_rule_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Option<&'a TypeRule<'a>> {
  cddl.rules.iter().find_map(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident && !rule.is_type_choice_alternate => Some(rule),
    _ => None,
  })
}

/// Retrieve the list of generic parameters for a given rule
pub fn generic_params_from_rule<'a>(rule: &Rule<'a>) -> Option<Vec<&'a str>> {
  match rule {
    Rule::Type { rule, .. } => rule
      .generic_params
      .as_ref()
      .map(|gp| gp.params.iter().map(|gp| gp.param.ident).collect()),
    Rule::Group { rule, .. } => rule
      .generic_params
      .as_ref()
      .map(|gp| gp.params.iter().map(|gp| gp.param.ident).collect()),
  }
}

/// Find all type choice alternate rules from a given identifier
pub fn type_choice_alternates_from_ident<'a>(
  cddl: &'a CDDL,
  ident: &Identifier,
) -> Vec<&'a Type<'a>> {
  cddl
    .rules
    .iter()
    .filter_map(|r| match r {
      Rule::Type { rule, .. } if &rule.name == ident && rule.is_type_choice_alternate => {
        Some(&rule.value)
      }
      _ => None,
    })
    .collect::<Vec<_>>()
}

/// Find all group choice alternate rules from a given identifier
pub fn group_choice_alternates_from_ident<'a>(
  cddl: &'a CDDL,
  ident: &Identifier,
) -> Vec<&'a GroupEntry<'a>> {
  cddl
    .rules
    .iter()
    .filter_map(|r| match r {
      Rule::Group { rule, .. } if &rule.name == ident && rule.is_group_choice_alternate => {
        Some(&rule.entry)
      }
      _ => None,
    })
    .collect::<Vec<_>>()
}

/// Convert a given group choice to a list of type choices
pub fn type_choices_from_group_choice<'a>(
  cddl: &'a CDDL,
  grpchoice: &GroupChoice<'a>,
) -> Vec<TypeChoice<'a>> {
  let mut type_choices = Vec::new();
  for ge in grpchoice.group_entries.iter() {
    match &ge.0 {
      GroupEntry::ValueMemberKey { ge, .. } => {
        type_choices.append(&mut ge.entry_type.type_choices.clone());
      }
      GroupEntry::TypeGroupname { ge, .. } => {
        // TODO: parse generic args
        if let Some(r) = rule_from_ident(cddl, &ge.name) {
          match r {
            Rule::Type { rule, .. } => type_choices.append(&mut rule.value.type_choices.clone()),
            Rule::Group { rule, .. } => type_choices.append(&mut type_choices_from_group_choice(
              cddl,
              &GroupChoice::new(vec![rule.entry.clone()]),
            )),
          }
        }
      }
      GroupEntry::InlineGroup { group, .. } => {
        for gc in group.group_choices.iter() {
          type_choices.append(&mut type_choices_from_group_choice(cddl, gc));
        }
      }
    }
  }

  type_choices
}

/// Is the given identifier associated with a null data type
pub fn is_ident_null_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::NULL | Token::NIL = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_null_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a boolean data type
pub fn is_ident_bool_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::BOOL = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_bool_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Does the given boolean identifier match the boolean value
pub fn ident_matches_bool_value(cddl: &CDDL, ident: &Identifier, value: bool) -> bool {
  if let Token::TRUE = lookup_ident(ident.ident) {
    if value {
      return true;
    }
  }

  if let Token::FALSE = lookup_ident(ident.ident) {
    if !value {
      return true;
    }
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        ident_matches_bool_value(cddl, ident, value)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a URI data type
pub fn is_ident_uri_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::URI = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_uri_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a b64url data type
pub fn is_ident_b64url_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::B64URL = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_b64url_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a tdate data type
pub fn is_ident_tdate_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::TDATE = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_tdate_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a time data type
pub fn is_ident_time_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::TIME = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_time_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a decfrac data type
pub fn is_ident_decfrac_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::DECFRAC = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_decfrac_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a bigfloat data type
pub fn is_ident_bigfloat_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::BIGFLOAT = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_bigfloat_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a numeric data type
pub fn is_ident_numeric_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::UINT
  | Token::NINT
  | Token::INTEGER
  | Token::INT
  | Token::NUMBER
  | Token::FLOAT
  | Token::FLOAT16
  | Token::FLOAT32
  | Token::FLOAT64
  | Token::FLOAT1632
  | Token::FLOAT3264
  | Token::UNSIGNED = lookup_ident(ident.ident)
  {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_numeric_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a uint data type
pub fn is_ident_uint_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::UINT = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_uint_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a nint data type
pub fn is_ident_nint_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::NINT = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_nint_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Numbers are defined as `number = int / float`
/// Therefore, this enum represents which type (or both) is allowed in a given position
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NumericKind {
  /// Only integer values (e.g. `int`, `uint`, `nint`)
  Int,
  /// Only float values (e.g. `float`, `float16` .. `float64`)
  Float,
  /// Both integer and float values (e.g. `number = int / float`)
  Both,
}

impl NumericKind {
  /// Does this kind admit integer values?
  pub fn admits_int(self) -> bool {
    matches!(self, NumericKind::Int | NumericKind::Both)
  }

  /// Does this kind admit float values?
  pub fn admits_float(self) -> bool {
    matches!(self, NumericKind::Float | NumericKind::Both)
  }
}

/// Classify the given identifier's numeric kind
/// or `None` if it is not associated with an int/float numeric data type
pub fn ident_numeric_kind(cddl: &CDDL, ident: &Identifier) -> Option<NumericKind> {
  #[allow(deprecated)]
  match (
    is_ident_integer_data_type(cddl, ident),
    is_ident_float_data_type(cddl, ident),
  ) {
    (true, true) => Some(NumericKind::Both),
    (true, false) => Some(NumericKind::Int),
    (false, true) => Some(NumericKind::Float),
    (false, false) => None,
  }
}

/// Is the given identifier associated with an integer data type
#[deprecated(
  note = "not mutually exclusive with is_ident_float_data_type (`number` matches both); use ident_numeric_kind and handle NumericKind::Both"
)]
pub fn is_ident_integer_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::INT | Token::INTEGER | Token::NINT | Token::UINT | Token::NUMBER | Token::UNSIGNED =
    lookup_ident(ident.ident)
  {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_integer_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Does the given identifier denote a bignum data type that accepts CBOR tag
/// `tag`? Per the RFC 8610 prelude: `biguint = #6.2(bstr)`,
/// `bignint = #6.3(bstr)` and `bigint = biguint / bignint`.
pub fn ident_accepts_bignum_tag(cddl: &CDDL, ident: &Identifier, tag: u64) -> bool {
  match lookup_ident(ident.ident) {
    Token::BIGUINT => return tag == 2,
    Token::BIGNINT => return tag == 3,
    Token::BIGINT => return tag == 2 || tag == 3,
    _ => (),
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        ident_accepts_bignum_tag(cddl, ident, tag)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a bignum data type
pub fn is_ident_bignum_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  ident_accepts_bignum_tag(cddl, ident, 2) || ident_accepts_bignum_tag(cddl, ident, 3)
}

/// Is the given identifier associated with a float data type
#[deprecated(
  note = "not mutually exclusive with is_ident_integer_data_type (`number` matches both); use ident_numeric_kind and handle NumericKind::Both"
)]
pub fn is_ident_float_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::FLOAT
  | Token::FLOAT16
  | Token::FLOAT1632
  | Token::FLOAT32
  | Token::FLOAT3264
  | Token::FLOAT64
  | Token::NUMBER = lookup_ident(ident.ident)
  {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_float_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a string data type
pub fn is_ident_string_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::TEXT | Token::TSTR = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_string_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with the any type
pub fn is_ident_any_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::ANY = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_any_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a byte string data type
pub fn is_ident_byte_string_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::BSTR | Token::BYTES = lookup_ident(ident.ident) {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_byte_string_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Retrieve number of group entries from a group. This is currently only used
/// for determining map equality/inequality (the `.eq`/`.ne` control
/// operators), but may be useful in other contexts. The occurrence is only
/// captured for the second entry of the group choice to avoid ambiguity in
/// non-homogenous definitions
pub fn entry_counts_from_group<'a, 'b: 'a>(
  cddl: &'a CDDL,
  group: &'b Group<'a>,
) -> Vec<EntryCount> {
  // Each EntryCount is associated with a group choice in the given group
  let mut entry_counts = Vec::new();

  for gc in group.group_choices.iter() {
    let mut count = 0;
    let mut entry_occurrence = None;
    let mut skip_final_push = false;

    for (idx, ge) in gc.group_entries.iter().enumerate() {
      match &ge.0 {
        GroupEntry::ValueMemberKey { ge, .. } => {
          if idx == 1 {
            if let Some(occur) = &ge.occur {
              entry_occurrence = Some(occur.occur)
            }
          }

          count += 1;
        }
        GroupEntry::InlineGroup { group, occur, .. } => {
          if idx == 1 {
            if let Some(occur) = occur {
              entry_occurrence = Some(occur.occur)
            }
          }

          // For inline groups with multiple choices, we need to add the current count
          // to each of the nested entry counts, not replace the entire list
          let nested_entry_counts = entry_counts_from_group(cddl, group);
          if group.group_choices.len() > 1 {
            // Add current accumulated count to each nested choice count
            for nested_ec in nested_entry_counts {
              entry_counts.push(EntryCount {
                count: count + nested_ec.count,
                entry_occurrence: nested_ec.entry_occurrence.or(entry_occurrence),
              });
            }
            // Don't add the current group choice count at the end since we've handled it here
            skip_final_push = true;
            break;
          } else {
            // Single choice case: add the nested count to current count
            count += if let Some(ec) = nested_entry_counts.first() {
              ec.count
            } else {
              0
            };
          }
        }
        GroupEntry::TypeGroupname { ge, .. } => {
          if idx == 1 {
            if let Some(occur) = &ge.occur {
              entry_occurrence = Some(occur.occur)
            }
          }

          if let Some(gr) = group_rule_from_ident(cddl, &ge.name) {
            if let GroupEntry::InlineGroup { group, .. } = &gr.entry {
              if group.group_choices.len() == 1 {
                count += if let Some(ec) = entry_counts_from_group(cddl, group).first() {
                  ec.count
                } else {
                  0
                };
              } else {
                entry_counts.append(&mut entry_counts_from_group(cddl, group));
              }
            } else {
              entry_counts.append(&mut entry_counts_from_group(cddl, &gr.entry.clone().into()));
            }
          } else if group_choice_alternates_from_ident(cddl, &ge.name).is_empty() {
            count += 1;
          } else {
            for ge in group_choice_alternates_from_ident(cddl, &ge.name).into_iter() {
              entry_counts.append(&mut entry_counts_from_group(cddl, &ge.clone().into()));
            }
          }
        }
      }
    }

    if !skip_final_push {
      entry_counts.push(EntryCount {
        count,
        entry_occurrence,
      });
    }
  }

  entry_counts
}

/// Validate the number of entries given an array of possible valid entry counts
pub fn validate_entry_count(valid_entry_counts: &[EntryCount], num_entries: usize) -> bool {
  valid_entry_counts.iter().any(|ec| {
    num_entries == ec.count as usize
      || match ec.entry_occurrence {
        #[cfg(feature = "ast-span")]
        Some(Occur::ZeroOrMore { .. }) | Some(Occur::Optional { .. }) => true,
        #[cfg(not(feature = "ast-span"))]
        Some(Occur::ZeroOrMore {}) | Some(Occur::Optional {}) => true,
        #[cfg(feature = "ast-span")]
        Some(Occur::OneOrMore { .. }) if num_entries > 0 => true,
        #[cfg(not(feature = "ast-span"))]
        Some(Occur::OneOrMore {}) if num_entries > 0 => true,
        Some(Occur::Exact { lower, upper, .. }) => {
          if let Some(lower) = lower {
            if let Some(upper) = upper {
              num_entries >= lower && num_entries <= upper
            } else {
              num_entries >= lower
            }
          } else if let Some(upper) = upper {
            num_entries <= upper
          } else {
            false
          }
        }
        _ => false,
      }
  })
}

/// Entry count
#[derive(Clone, Debug)]
pub struct EntryCount {
  /// Count
  pub count: u64,
  /// Optional occurrence
  pub entry_occurrence: Option<Occur>,
}

/// Shared bookkeeping for the array sequence matcher (RFC 8610 Appendix A
/// PEG semantics), generic over the validator's error type
pub struct ArraySeqCtx<'a, E> {
  /// Farthest element index at which a leaf validation failed, together with
  /// the child validator errors produced there. Used for error reporting once
  /// the overall match fails.
  pub best_failure: Option<(usize, Vec<E>)>,
  /// (group rule name, cursor) pairs currently being expanded; guards against
  /// recursive group rule references that would loop without consuming
  /// elements
  pub active_group_refs: Vec<(&'a str, usize)>,
}

impl<E> Default for ArraySeqCtx<'_, E> {
  fn default() -> Self {
    ArraySeqCtx {
      best_failure: None,
      active_group_refs: Vec::new(),
    }
  }
}

impl<E> ArraySeqCtx<'_, E> {
  /// Record a leaf failure at the given element index, keeping only the
  /// farthest one
  pub fn note_failure(&mut self, idx: usize, errors: Vec<E>) {
    if self
      .best_failure
      .as_ref()
      .is_none_or(|(best, _)| idx >= *best)
    {
      self.best_failure = Some((idx, errors));
    }
  }
}

/// Regex needs to be formatted in a certain way so it can be parsed. See
/// <https://github.com/anweiss/cddl/issues/67>
pub fn format_regex(input: &str) -> Option<String> {
  let mut formatted_regex = String::from(input);
  let mut unescape = Vec::new();
  for (idx, c) in formatted_regex.char_indices() {
    if c == '\\' {
      if let Some(c) = formatted_regex.chars().nth(idx + 1) {
        if !regex_syntax::is_meta_character(c) && c != 'd' {
          unescape.push(format!("\\{}", c));
        }
      }
    }
  }

  for replace in unescape.iter() {
    formatted_regex =
      formatted_regex.replace(replace, &replace.chars().nth(1).unwrap().to_string());
  }

  for find in ["?=", "?!", "?<=", "?<!"].iter() {
    if formatted_regex.contains(find) {
      return None;
    }
  }

  formatted_regex = formatted_regex.replace("?<", "?P<");

  Some(formatted_regex)
}

#[allow(missing_docs)]
#[derive(Debug)]
pub enum ArrayItemToken<'a> {
  Value(&'a Value<'a>),
  Range(&'a Type2<'a>, &'a Type2<'a>, bool),
  Group(&'a Group<'a>),
  Identifier(&'a Identifier<'a>),
  TaggedData(&'a Type2<'a>),
}

#[allow(missing_docs)]
impl ArrayItemToken<'_> {
  pub fn error_msg(&self, idx: Option<usize>) -> String {
    match self {
      ArrayItemToken::Value(value) => {
        if let Some(idx) = idx {
          format!("expected value {} at index {}", value, idx)
        } else {
          format!("expected value {}", value)
        }
      }
      ArrayItemToken::Range(lower, upper, is_inclusive) => {
        if let Some(idx) = idx {
          format!(
            "expected range lower {} upper {} inclusive {} at index {}",
            lower, upper, is_inclusive, idx
          )
        } else {
          format!(
            "expected range lower {} upper {} inclusive {}",
            lower, upper, is_inclusive
          )
        }
      }
      ArrayItemToken::Group(group) => {
        if let Some(idx) = idx {
          format!("expected map object {} at index {}", group, idx)
        } else {
          format!("expected map object {}", group)
        }
      }
      ArrayItemToken::Identifier(ident) => {
        if let Some(idx) = idx {
          format!("expected type {} at index {}", ident, idx)
        } else {
          format!("expected type {}", ident)
        }
      }
      ArrayItemToken::TaggedData(tagged_data) => {
        if let Some(idx) = idx {
          format!(
            "expected tagged data tag {:?} at index {}",
            tagged_data, idx
          )
        } else {
          format!("expected tagged data {:?}", tagged_data)
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  #![cfg(not(target_arch = "wasm32"))]

  use super::*;

  #[test]
  fn validate_json() {
    let cddl_schema = cddl_from_str(
      r#"
  foo = {
    bar: tstr
  }
  "#,
      true,
    )
    .unwrap();

    let documents = [r#"{ "bar": "foo" }"#, r#"{ "bar": "foo2" }"#];

    documents
      .iter()
      .all(|doc| cddl_schema.validate_json(doc.as_bytes(), None).is_ok());
  }

  #[test]
  fn numeric_kind_classification() {
    let cddl = cddl_from_str(
      r#"
  a = int
  b = float32
  c = number
  d = int / float
  e = a / b
  f = tstr
  "#,
      true,
    )
    .unwrap();

    let kind_of = |name: &str| {
      let rule_ident = cddl
        .rules
        .iter()
        .find_map(|r| match r {
          Rule::Type { rule, .. } if rule.name.ident == name => Some(&rule.name),
          _ => None,
        })
        .unwrap();
      ident_numeric_kind(&cddl, rule_ident)
    };

    assert_eq!(kind_of("a"), Some(NumericKind::Int));
    assert_eq!(kind_of("b"), Some(NumericKind::Float));
    assert_eq!(kind_of("c"), Some(NumericKind::Both));
    assert_eq!(kind_of("d"), Some(NumericKind::Both));
    assert_eq!(kind_of("e"), Some(NumericKind::Both));
    assert_eq!(kind_of("f"), None);
  }
}
