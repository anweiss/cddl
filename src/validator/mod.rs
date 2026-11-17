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
mod numeric_structure;
use numeric_structure::*;
mod numeric_domains;
use numeric_domains::*;
mod numeric_sets;
use numeric_sets::*;

#[cfg(feature = "ast-span")]
use crate::ast::Span;
use crate::{
  ast::{
    GenericArgs, Group, GroupChoice, GroupEntry, GroupRule, Identifier, MemberKey, Occur,
    RangeCtlOp, Rule, Type, Type1, Type2, TypeChoice, TypeRule, ValueMemberKeyEntry, CDDL,
  },
  token::{self, *},
  visitor::Visitor,
};

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::rc::Rc;

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
#[cfg(any(feature = "json", feature = "cbor", test))]
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

#[cfg(all(test, feature = "json", not(target_arch = "wasm32")))]
impl CDDL<'_> {
  #[cfg(feature = "json")]
  /// Validate the given document against the CDDL definition
  fn validate_json(
    &self,
    document: &[u8],
    #[cfg(feature = "additional-controls")] enabled_features: Option<&[&str]>,
  ) -> Result<(), Box<dyn Error>> {
    let json =
      serde_json::from_slice::<serde_json::Value>(document).map_err(json::Error::JSONParsing)?;

    #[cfg(feature = "additional-controls")]
    let mut jv = JSONValidator::new(self, json, enabled_features);
    #[cfg(not(feature = "additional-controls"))]
    let mut jv = JSONValidator::new(self, json);

    jv.validate().map_err(|e| e.into())
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

fn enumeration_rule_from_ident<'a>(cddl: &'a CDDL<'a>, ident: &Identifier) -> Option<&'a Rule<'a>> {
  rule_from_ident(cddl, ident).or_else(|| {
    cddl
      .rules
      .iter()
      .find(|rule| matches!(rule, Rule::Group { rule, .. } if rule.name == *ident))
  })
}

fn evaluation_rule_from_ident<'a>(
  state: &ValidationState<'a>,
  ident: &Identifier,
) -> Option<&'a Rule<'a>> {
  if state.is_group_to_choice_enum {
    enumeration_rule_from_ident(state.cddl, ident)
  } else {
    rule_from_ident(state.cddl, ident)
  }
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

/// Find every type assigned to an identifier, in document order.
///
/// RFC 8610 Section 2.2.2 defines each `/=` assignment as another arm of the
/// named type choice. Appendix C requires the arms to retain rule order.
pub fn type_choice_types_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Vec<&'a Type<'a>> {
  cddl
    .rules
    .iter()
    .filter_map(|r| match r {
      Rule::Type { rule, .. } if &rule.name == ident => Some(&rule.value),
      _ => None,
    })
    .collect::<Vec<_>>()
}

/// Lower and optional upper bound of an occurrence indicator; no indicator
/// means exactly once.
pub(crate) fn occurrence_bounds(occur: Option<Occur>) -> (usize, Option<usize>) {
  match occur {
    None => (1, Some(1)),
    Some(Occur::Optional { .. }) => (0, Some(1)),
    Some(Occur::ZeroOrMore { .. }) => (0, None),
    Some(Occur::OneOrMore { .. }) => (1, None),
    Some(Occur::Exact { lower, upper, .. }) => (lower.unwrap_or(0), upper),
  }
}

/// A cache/recursion key for an identifier that keeps socket-prefixed names
/// (`$s`, `$$s`) distinct from the plain name.
pub(crate) fn qualified_ident_key(ident: &Identifier) -> String {
  match ident.socket {
    Some(SocketPlug::TYPE) => format!("${}", ident.ident),
    Some(SocketPlug::GROUP) => format!("$${}", ident.ident),
    None => ident.ident.to_string(),
  }
}

/// The data location of a probe that matches a map key. It differs from the
/// map's own location and from every value location, so the recursion guard
/// never mistakes a key for the map or one of its values.
pub(crate) fn key_probe_location(map_location: &str, key: &impl std::fmt::Debug) -> String {
  format!("{map_location}#key({key:?})")
}

/// The key type, its alternatives and the cut flag of a member whose key
/// type is a choice; `None` keeps every other member on its existing path.
#[cfg(feature = "json")]
pub(crate) fn choice_member_key<'e, 'a>(
  state: &ValidationState<'a>,
  entry: &'e ValueMemberKeyEntry<'a>,
) -> Option<(&'e Type1<'a>, Vec<Type1<'a>>, bool)> {
  choice_member_key_for_depth(state, entry, 0)
}

/// `choice_member_key` for keys nested up to `key_depth` levels deep.
pub(crate) fn choice_member_key_for_depth<'e, 'a>(
  state: &ValidationState<'a>,
  entry: &'e ValueMemberKeyEntry<'a>,
  key_depth: usize,
) -> Option<(&'e Type1<'a>, Vec<Type1<'a>>, bool)> {
  let Some(MemberKey::Type1 { t1, is_cut, .. }) = &entry.member_key else {
    return None;
  };
  let (alternatives, acyclic) = member_key_type_alternatives_for_depth(state, t1, key_depth);
  // A cut recursive reference must not reach the scalar path, which follows
  // it without end, even when a single alternative remains. A control over a
  // choice (`(int / tstr) .and any`) also matches each key on its own, by
  // probing the complete constrained type.
  let choice = alternatives.len() > 1
    || !acyclic
    || alternatives
      .first()
      .is_some_and(|alternative| control_targets_choice(state, alternative));
  choice.then_some((t1.as_ref(), alternatives, *is_cut))
}

/// The occurrence and bounds of a choice-key member. An indicator inherited
/// from an enclosing group repeats the whole group: a zero-allowed group's
/// absence is handled by its own rollback, so inside a present group the
/// member is still required.
pub(crate) fn choice_member_occurrence(
  entry: &ValueMemberKeyEntry,
  inherited: Option<Occur>,
) -> (Option<Occur>, usize, Option<usize>) {
  let own = entry.occur.as_ref().map(|o| o.occur);
  let occurrence = own.or(inherited);
  let (min, max) = occurrence_bounds(occurrence);
  let min = if own.is_none() && inherited.is_some() {
    min.max(1)
  } else {
    min
  };
  (occurrence, min, max)
}

/// Whether a type carries a control operator whose target is a choice.
fn control_targets_choice(state: &ValidationState, t1: &Type1) -> bool {
  let Some(operator) = &t1.operator else {
    return false;
  };
  if matches!(operator.operator, RangeCtlOp::RangeOp { .. }) {
    return false;
  }
  let mut target = t1.clone();
  target.operator = None;
  let (alternatives, acyclic) = member_key_type_alternatives(state, &target);
  alternatives.len() > 1 || !acyclic
}

/// How often a generic alias that recurses with changing arguments
/// (`k<T> = T / k<[T]>`) is unrolled at least when expanding member-key
/// alternatives. Deeper keys raise the limit to their own nesting depth, up
/// to a fixed bound that keeps expansion finite for any input.
const MIN_GENERIC_KEY_UNROLL: usize = 8;
const MAX_GENERIC_KEY_UNROLL_DEPTH: usize = 64;

/// The total size, in bytes of instantiation keys, that one member-key
/// expansion may expand. It bounds recursion that branches
/// (`k<T> = T / k<[T]> / k<[T, T]>`) or doubles its argument however deep
/// the keys are.
const MAX_GENERIC_KEY_EXPANSION_SIZE: usize = 1 << 20;

/// State shared by one member-key expansion: the per-name cache, the unroll
/// limit for recursion with changing arguments and the remaining size budget
/// of generic instantiations.
struct KeyExpansion<'a> {
  memo: HashMap<String, Rc<Vec<Type1<'a>>>>,
  unroll_limit: usize,
  budget: usize,
}

/// A recursion and cache key for one generic instantiation. Arguments are
/// keyed by their structure rather than their display form, which prints
/// `1` and `1.0` alike.
pub(crate) fn generic_instantiation_key<'b, 'a: 'b>(
  name: &str,
  args: impl IntoIterator<Item = &'b Type1<'a>>,
) -> String {
  let args = args
    .into_iter()
    .map(|arg| format!("{arg:?}"))
    .collect::<Vec<_>>()
    .join(", ");
  format!("{name}<{args}>")
}

/// The argument bound to a parameter reference. Socket-prefixed names such
/// as `$K` are distinct rules, never parameters.
fn param_binding<'m, 'a>(
  bindings: &'m HashMap<&'a str, Type1<'a>>,
  ident: &Identifier,
) -> Option<&'m Type1<'a>> {
  if ident.socket.is_some() {
    return None;
  }
  bindings.get(ident.ident)
}

/// The alternatives one map key may match for a member key type, following
/// parentheses, aliases, `/=` extensions and generic parameters. A type with
/// a control operator or generic arguments stays whole because it constrains
/// the complete type rather than one alternative. Expansion tracks the active
/// name chain so recursive paths stop, and caches acyclic expansions by name:
/// an alias subgraph shared by many paths is expanded once, so DAG-shaped
/// alias hierarchies stay linear instead of exponential.
pub(crate) fn member_key_type_alternatives<'a>(
  state: &ValidationState<'a>,
  key_type: &Type1<'a>,
) -> (Vec<Type1<'a>>, bool) {
  member_key_type_alternatives_for_depth(state, key_type, 0)
}

/// Member-key alternatives for keys nested up to `key_depth` levels deep.
/// Recursion that wraps its argument (`k<T> = T / k<[T]>`) adds a level per
/// unrolling, so unrolling past the deepest key keeps keys up to the depth
/// limit reachable without unbounded expansion.
pub(crate) fn member_key_type_alternatives_for_depth<'a>(
  state: &ValidationState<'a>,
  key_type: &Type1<'a>,
  key_depth: usize,
) -> (Vec<Type1<'a>>, bool) {
  let mut expansion = KeyExpansion {
    memo: HashMap::new(),
    unroll_limit: key_depth
      .saturating_add(2)
      .clamp(MIN_GENERIC_KEY_UNROLL, MAX_GENERIC_KEY_UNROLL_DEPTH),
    budget: MAX_GENERIC_KEY_EXPANSION_SIZE,
  };
  expand_key_type_alternatives(state, key_type, &mut Vec::new(), &mut expansion)
}

/// Expand one candidate type into leaf alternatives. The returned flag is
/// false when a recursive path was cut underneath, making the result specific
/// to the active chain and unfit for caching by name.
fn expand_key_type_alternatives<'a>(
  state: &ValidationState<'a>,
  candidate: &Type1<'a>,
  active: &mut Vec<String>,
  expansion: &mut KeyExpansion<'a>,
) -> (Vec<Type1<'a>>, bool) {
  match &candidate.type2 {
    _ if candidate.operator.is_some() => {
      expand_controlled_key_alternatives(state, candidate, active, expansion)
    }
    Type2::ParenthesizedType { pt, .. } => {
      let mut alternatives = Vec::new();
      let mut acyclic = true;
      for choice in &pt.type_choices {
        let (mut nested, nested_acyclic) =
          expand_key_type_alternatives(state, &choice.type1, active, expansion);
        alternatives.append(&mut nested);
        acyclic &= nested_acyclic;
      }
      (alternatives, acyclic)
    }
    Type2::Typename {
      ident,
      generic_args: None,
      ..
    } => expand_key_ident_alternatives(state, candidate, ident, active, expansion),
    Type2::Typename {
      ident,
      generic_args: Some(args),
      ..
    } => expand_key_generic_alternatives(state, candidate, ident, args, active, expansion),
    _ => (vec![candidate.clone()], true),
  }
}

/// A predicate control constrains each value its target admits, so
/// `(A / B) .ctl C` matches what `A .ctl C / B .ctl C` matches. Distributing
/// it over the target's alternatives lets a key's domain check and probe
/// agree on one alternative, and keeps a recursive target under the
/// expansion guards. Ranges and controls that compute a value from their
/// target (`.plus`, `.cat`, `.det`) stay whole.
fn expand_controlled_key_alternatives<'a>(
  state: &ValidationState<'a>,
  candidate: &Type1<'a>,
  active: &mut Vec<String>,
  expansion: &mut KeyExpansion<'a>,
) -> (Vec<Type1<'a>>, bool) {
  let Some(operator) = &candidate.operator else {
    return (vec![candidate.clone()], true);
  };
  if !matches!(
    &operator.operator,
    RangeCtlOp::CtlOp { ctrl, .. } if !is_computed_control(ctrl)
  ) {
    return (vec![candidate.clone()], true);
  }
  let mut target = candidate.clone();
  target.operator = None;
  let (targets, acyclic) = expand_key_type_alternatives(state, &target, active, expansion);
  let alternatives = targets
    .iter()
    .map(|target| {
      let mut constrained = target.clone();
      constrained.type2 = bound_type2(target);
      constrained.operator = Some(operator.clone());
      constrained
    })
    .collect();
  (alternatives, acyclic)
}

/// Whether a control computes a new value from its target (`.plus`, `.cat`,
/// `.det`) rather than constraining the target's values.
pub(crate) fn is_computed_control(ctrl: &ControlOperator) -> bool {
  #[cfg(feature = "additional-controls")]
  {
    matches!(
      ctrl,
      ControlOperator::PLUS | ControlOperator::CAT | ControlOperator::DET
    )
  }
  #[cfg(not(feature = "additional-controls"))]
  {
    let _ = ctrl;
    false
  }
}

/// Expand a generic instantiation by substituting its arguments, resolved in
/// the caller's environment, for the rule's parameters. Instantiations are
/// not cached: the expansion depends on the arguments.
fn expand_key_generic_alternatives<'a>(
  state: &ValidationState<'a>,
  candidate: &Type1<'a>,
  ident: &Identifier<'a>,
  args: &GenericArgs<'a>,
  active: &mut Vec<String>,
  expansion: &mut KeyExpansion<'a>,
) -> (Vec<Type1<'a>>, bool) {
  let Some(rule) = rule_from_ident(state.cddl, ident) else {
    return (vec![candidate.clone()], true);
  };
  let Some(params) = generic_params_from_rule(rule).filter(|_| matches!(rule, Rule::Type { .. }))
  else {
    return (vec![candidate.clone()], true);
  };
  let key = qualified_ident_key(ident);
  let Ok(resolved) = resolved_generic_args(state, args) else {
    return (Vec::new(), false);
  };
  // Instantiations are cached by name and resolved arguments, so a DAG of
  // generic aliases expands each distinct instantiation once.
  let memo_key = generic_instantiation_key(&key, &resolved);
  if active.contains(&memo_key) {
    // The same instantiation again (`k<T> = T / k<T>`) adds no keys of its
    // own, and probing it would recurse without end.
    return (Vec::new(), false);
  }
  if active.iter().filter(|name| **name == key).count() >= expansion.unroll_limit
    || memo_key.len() > expansion.budget
  {
    // Recursion with changing arguments is unrolled to a bounded depth and
    // deeper levels are dropped: probing them would never consume input.
    return (Vec::new(), false);
  }
  if let Some(cached) = expansion.memo.get(&memo_key) {
    return ((**cached).clone(), true);
  }
  expansion.budget -= memo_key.len();
  let bindings: HashMap<&'a str, Type1<'a>> = params.iter().copied().zip(resolved).collect();
  // Every definition contributes, including `/=` extensions.
  let types = type_choice_types_from_ident(state.cddl, ident);
  active.push(key);
  active.push(memo_key.clone());
  let mut alternatives = Vec::new();
  let mut acyclic = true;
  for rule_type in types {
    for choice in &rule_type.type_choices {
      let substituted = substitute_generic_params(&choice.type1, &bindings);
      let (mut nested, nested_acyclic) =
        expand_key_type_alternatives(state, &substituted, active, expansion);
      alternatives.append(&mut nested);
      acyclic &= nested_acyclic;
    }
  }
  active.truncate(active.len() - 2);
  if acyclic {
    expansion
      .memo
      .insert(memo_key, Rc::new(deduplicate_alternatives(&alternatives)));
  }
  (alternatives, acyclic)
}

/// Replace parameter references in a member-key type with their bound
/// arguments. A bare parameter takes the whole argument; parameters nested
/// in generic arguments or parenthesized choices are substituted in place.
fn substitute_generic_params<'a>(
  t1: &Type1<'a>,
  bindings: &HashMap<&'a str, Type1<'a>>,
) -> Type1<'a> {
  if let Type2::Typename {
    ident,
    generic_args: None,
    ..
  } = &t1.type2
  {
    if let Some(arg) = param_binding(bindings, ident) {
      let Some(operator) = &t1.operator else {
        return arg.clone();
      };
      let mut op = operator.clone();
      op.type2 = substitute_type2_params(&operator.type2, bindings);
      let mut substituted = t1.clone();
      // An argument that carries its own operator is parenthesized so both
      // constraints apply.
      substituted.type2 = bound_type2(arg);
      substituted.operator = Some(op);
      return substituted;
    }
  }
  let mut substituted = t1.clone();
  substituted.type2 = substitute_type2_params(&t1.type2, bindings);
  if let Some(operator) = &t1.operator {
    let mut op = operator.clone();
    op.type2 = substitute_type2_params(&operator.type2, bindings);
    substituted.operator = Some(op);
  }
  substituted
}

/// Substitute generic parameters inside a type2 node, reaching through
/// arrays, maps and tagged payloads so expanded alternatives never probe
/// with an unbound parameter.
fn substitute_type2_params<'a>(
  t2: &Type2<'a>,
  bindings: &HashMap<&'a str, Type1<'a>>,
) -> Type2<'a> {
  if let Type2::Typename {
    ident,
    generic_args: None,
    ..
  } = t2
  {
    if let Some(arg) = param_binding(bindings, ident) {
      return bound_type2(arg);
    }
  }
  let mut substituted = t2.clone();
  match &mut substituted {
    Type2::Typename {
      generic_args: Some(args),
      ..
    }
    | Type2::ChoiceFromGroup {
      generic_args: Some(args),
      ..
    }
    | Type2::Unwrap {
      generic_args: Some(args),
      ..
    } => {
      for arg in args.args.iter_mut() {
        *arg.arg = substitute_generic_params(&arg.arg, bindings);
      }
    }
    Type2::ParenthesizedType { pt, .. } => {
      substitute_type_params(pt, bindings);
    }
    Type2::Array { group, .. }
    | Type2::Map { group, .. }
    | Type2::ChoiceFromInlineGroup { group, .. } => {
      substitute_group_params(group, bindings);
    }
    Type2::TaggedData { t, .. } => {
      substitute_type_params(t, bindings);
    }
    _ => {}
  }
  substituted
}

/// A bound argument as a type2 node: an unconstrained argument is used
/// directly, a constrained one is parenthesized to keep its operator.
fn bound_type2<'a>(arg: &Type1<'a>) -> Type2<'a> {
  if arg.operator.is_none() {
    return arg.type2.clone();
  }
  Type2::ParenthesizedType {
    pt: Type {
      type_choices: vec![TypeChoice {
        type1: arg.clone(),
        #[cfg(feature = "ast-comments")]
        comments_before_type: None,
        #[cfg(feature = "ast-comments")]
        comments_after_type: None,
      }],
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    },
    #[cfg(feature = "ast-span")]
    span: Span::default(),
    #[cfg(feature = "ast-comments")]
    comments_before_type: None,
    #[cfg(feature = "ast-comments")]
    comments_after_type: None,
  }
}

/// A deduplicated copy of expanded alternatives for the expansion cache; the
/// caller's list keeps duplicates so a syntactic choice still reads as one.
fn deduplicate_alternatives<'a>(alternatives: &[Type1<'a>]) -> Vec<Type1<'a>> {
  let mut deduplicated: Vec<Type1<'a>> = Vec::new();
  for alternative in alternatives {
    if !deduplicated.contains(alternative) {
      deduplicated.push(alternative.clone());
    }
  }
  deduplicated
}

/// Substitute generic parameters in every choice of a type.
fn substitute_type_params<'a>(t: &mut Type<'a>, bindings: &HashMap<&'a str, Type1<'a>>) {
  for choice in t.type_choices.iter_mut() {
    choice.type1 = substitute_generic_params(&choice.type1, bindings);
  }
}

/// Substitute generic parameters through the entries of a group.
fn substitute_group_params<'a>(group: &mut Group<'a>, bindings: &HashMap<&'a str, Type1<'a>>) {
  for choice in group.group_choices.iter_mut() {
    for (entry, _) in choice.group_entries.iter_mut() {
      match entry {
        GroupEntry::ValueMemberKey { ge, .. } => {
          if let Some(MemberKey::Type1 { t1, .. }) = &mut ge.member_key {
            **t1 = substitute_generic_params(t1, bindings);
          }
          substitute_type_params(&mut ge.entry_type, bindings);
        }
        GroupEntry::TypeGroupname { ge, .. } => {
          // An entry that names a parameter directly, as `[K]` does, takes
          // the bound argument: a bare type name replaces the name, anything
          // else becomes a value entry for the bound type.
          let substitution = if ge.generic_args.is_none() && ge.name.socket.is_none() {
            bindings
              .get(ge.name.ident)
              .map(|arg| match (&arg.operator, &arg.type2) {
                (
                  None,
                  Type2::Typename {
                    ident,
                    generic_args,
                    ..
                  },
                ) => Ok((ident.clone(), generic_args.clone())),
                _ => Err(Box::new(arg.clone())),
              })
          } else {
            None
          };
          match substitution {
            Some(Ok((ident, generic_args))) => {
              ge.name = ident;
              ge.generic_args = generic_args;
            }
            Some(Err(arg)) => {
              let occur = ge.occur.clone();
              *entry = GroupEntry::ValueMemberKey {
                ge: Box::new(ValueMemberKeyEntry {
                  occur,
                  member_key: None,
                  entry_type: Type {
                    type_choices: vec![TypeChoice {
                      type1: *arg,
                      #[cfg(feature = "ast-comments")]
                      comments_before_type: None,
                      #[cfg(feature = "ast-comments")]
                      comments_after_type: None,
                    }],
                    #[cfg(feature = "ast-span")]
                    span: Span::default(),
                  },
                }),
                #[cfg(feature = "ast-span")]
                span: Span::default(),
                #[cfg(feature = "ast-comments")]
                leading_comments: None,
                #[cfg(feature = "ast-comments")]
                trailing_comments: None,
              };
            }
            None => {
              if let Some(args) = &mut ge.generic_args {
                for arg in args.args.iter_mut() {
                  *arg.arg = substitute_generic_params(&arg.arg, bindings);
                }
              }
            }
          }
        }
        GroupEntry::InlineGroup { group, .. } => {
          substitute_group_params(group, bindings);
        }
      }
    }
  }
}

/// Expand one alias occurrence. Results are cached by name only when no
/// recursive path was cut underneath them; a cut makes the result depend on
/// the active chain.
fn expand_key_ident_alternatives<'a>(
  state: &ValidationState<'a>,
  candidate: &Type1<'a>,
  ident: &Identifier<'a>,
  active: &mut Vec<String>,
  expansion: &mut KeyExpansion<'a>,
) -> (Vec<Type1<'a>>, bool) {
  if let Some(arg) = ident
    .socket
    .is_none()
    .then(|| generic_argument_for_param(state, ident.ident))
    .flatten()
  {
    return expand_key_type_alternatives(state, &arg, active, expansion);
  }
  let key = qualified_ident_key(ident);
  if let Some(cached) = expansion.memo.get(&key) {
    return ((**cached).clone(), true);
  }
  if active.contains(&key) {
    // A union back-reference adds no keys of its own (the least fixed
    // point), and probing it would recurse without end.
    return (Vec::new(), false);
  }
  let types = type_choice_types_from_ident(state.cddl, ident);
  if types.is_empty() {
    return (vec![candidate.clone()], true);
  }
  active.push(key.clone());
  let mut alternatives = Vec::new();
  let mut acyclic = true;
  for rule_type in types {
    for choice in &rule_type.type_choices {
      let (mut nested, nested_acyclic) =
        expand_key_type_alternatives(state, &choice.type1, active, expansion);
      alternatives.append(&mut nested);
      acyclic &= nested_acyclic;
    }
  }
  active.pop();
  if acyclic {
    // Cache a deduplicated copy so alias subgraphs shared by many paths are
    // expanded once and stay small; the returned list keeps duplicates so a
    // syntactic choice still reads as a choice to its caller.
    expansion
      .memo
      .insert(key, Rc::new(deduplicate_alternatives(&alternatives)));
  }
  (alternatives, acyclic)
}

/// The argument bound to `param` in the generic rule currently being
/// evaluated. Arguments accumulate per instantiation, so the current ones
/// are the trailing `params.len()` entries.
fn generic_argument_for_param<'a>(state: &ValidationState<'a>, param: &str) -> Option<Type1<'a>> {
  let rule = state
    .generic_rules
    .iter()
    .find(|rule| Some(rule.name) == state.eval_generic_rule)?;
  let index = rule.params.iter().position(|p| *p == param)?;
  let start = rule.args.len().checked_sub(rule.params.len())?;
  let arg = rule.args.get(start + index)?;
  // A parameter bound only to itself was never given an argument; treating
  // it as one would recurse without end.
  if arg.operator.is_none()
    && matches!(
      &arg.type2,
      Type2::Typename {
        ident,
        generic_args: None,
        ..
      } if ident.ident == param && ident.socket.is_none()
    )
  {
    return None;
  }
  Some(arg.clone())
}

fn generic_argument_for_ident<'a>(
  state: &ValidationState<'a>,
  ident: &Identifier,
) -> Option<Type1<'a>> {
  ident
    .socket
    .is_none()
    .then(|| generic_argument_for_param(state, ident.ident))
    .flatten()
}

/// Resolve a generic argument against the rule currently being evaluated, so
/// a parameter passed through unchanged (`outer<T> = inner<T>`) binds to the
/// caller's argument instead of to itself. Arguments that are not plain
/// parameter references stay as written.
pub(crate) fn resolve_generic_argument<'a>(
  state: &ValidationState<'a>,
  arg: &Type1<'a>,
) -> Type1<'a> {
  let mut resolved = arg.clone();
  let mut seen = HashSet::new();
  loop {
    if resolved.operator.is_some() {
      break;
    }
    let Type2::Typename {
      ident,
      generic_args: None,
      ..
    } = &resolved.type2
    else {
      break;
    };
    if ident.socket.is_some() || !seen.insert(ident.ident) {
      break;
    }
    match generic_argument_for_param(state, ident.ident) {
      Some(next) => resolved = next,
      None => break,
    }
  }
  resolved
}

/// Instantiation must resolve nested parameter references before replacing
/// the caller's bindings; lexical analysis of other expressions stays separate.
fn resolve_argument_in_caller<'a>(state: &ValidationState<'a>, arg: &Type1<'a>) -> Type1<'a> {
  let Some(rule) = state
    .generic_rules
    .iter()
    .find(|rule| Some(rule.name) == state.eval_generic_rule)
  else {
    return normalize_generic_argument(arg.clone());
  };
  let bindings: HashMap<_, _> = rule
    .params
    .iter()
    .filter_map(|param| generic_argument_for_param(state, param).map(|arg| (*param, arg)))
    .collect();
  normalize_generic_argument(substitute_generic_params(arg, &bindings))
}

fn normalize_generic_argument<'a>(mut arg: Type1<'a>) -> Type1<'a> {
  while let Type2::ParenthesizedType { pt, .. } = &arg.type2 {
    let [choice] = pt.type_choices.as_slice() else {
      break;
    };
    if arg.operator.is_none() {
      arg = choice.type1.clone();
    } else if choice.type1.operator.is_none() {
      arg.type2 = choice.type1.type2.clone();
    } else {
      break;
    }
  }
  arg
}

fn active_generic_context<'s, 'a>(
  state: &'s ValidationState<'a>,
) -> Option<(&'a str, &'s [Type1<'a>])> {
  let rule = state
    .generic_rules
    .iter()
    .find(|rule| Some(rule.name) == state.eval_generic_rule)?;
  let start = rule.args.len().saturating_sub(rule.params.len());
  Some((rule.name, &rule.args[start..]))
}

fn current_generic_bindings<'a>(state: &ValidationState<'a>) -> HashMap<&'a str, Type1<'a>> {
  state
    .generic_rules
    .iter()
    .find(|rule| Some(rule.name) == state.eval_generic_rule)
    .into_iter()
    .flat_map(|rule| &rule.params)
    .filter_map(|param| generic_argument_for_param(state, param).map(|arg| (*param, arg)))
    .collect()
}

fn instantiated_type_choices<'a>(
  state: &ValidationState<'a>,
  ident: &Identifier<'a>,
) -> std::result::Result<Vec<std::borrow::Cow<'a, Type<'a>>>, ()> {
  let bindings = if state.eval_generic_rule == Some(ident.ident)
    && rule_from_ident(state.cddl, ident)
      .and_then(generic_params_from_rule)
      .is_some()
  {
    current_generic_bindings(state)
  } else {
    HashMap::new()
  };
  type_choice_types_from_ident(state.cddl, ident)
    .into_iter()
    .map(|value| {
      if bindings.is_empty() {
        return Ok(std::borrow::Cow::Borrowed(value));
      }
      if !value
        .type_choices
        .iter()
        .all(|choice| numeric_substitution_fits(&choice.type1, &bindings))
      {
        return Err(());
      }
      let mut expanded = value.clone();
      substitute_type_params(&mut expanded, &bindings);
      Ok(std::borrow::Cow::Owned(expanded))
    })
    .collect()
}

fn constrained_controller<'a>(
  target: &Type2<'a>,
  ctrl: ControlOperator,
  controller: &Type2<'a>,
) -> Type2<'a> {
  let mut value = Type1::from(token::Value::UINT(0));
  value.type2 = target.clone();
  value.operator = Some(crate::ast::Operator {
    operator: RangeCtlOp::CtlOp {
      ctrl,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    },
    type2: controller.clone(),
    #[cfg(feature = "ast-comments")]
    comments_before_operator: None,
    #[cfg(feature = "ast-comments")]
    comments_after_operator: None,
  });
  bound_type2(&value)
}

fn instantiated_group_entry<'a>(
  state: &ValidationState<'a>,
  entry: &GroupEntry<'a>,
) -> std::result::Result<Option<GroupEntry<'a>>, ()> {
  if state.eval_generic_rule.is_none() {
    return Ok(None);
  }
  let bindings = current_generic_bindings(state);
  if !group_entry_substitution_fits(entry, &bindings) {
    return Err(());
  }
  let mut group = Group::from(entry.clone());
  substitute_group_params(&mut group, &bindings);
  Ok(Some(
    group.group_choices.remove(0).group_entries.remove(0).0,
  ))
}

fn numeric_comparison(ctrl: ControlOperator) -> bool {
  matches!(
    ctrl,
    ControlOperator::EQ
      | ControlOperator::NE
      | ControlOperator::LT
      | ControlOperator::LE
      | ControlOperator::GT
      | ControlOperator::GE
  )
}

fn complete_numeric_candidate(operator: &RangeCtlOp) -> bool {
  match operator {
    RangeCtlOp::RangeOp { .. } => true,
    RangeCtlOp::CtlOp {
      ctrl:
        ControlOperator::AND | ControlOperator::WITHIN | ControlOperator::EQ | ControlOperator::NE,
      ..
    } => true,
    #[cfg(feature = "additional-controls")]
    RangeCtlOp::CtlOp {
      ctrl: ControlOperator::PLUS,
      ..
    } => true,
    _ => false,
  }
}

#[cfg(feature = "cbor")]
fn reset_numeric_context(context: &mut NumericContext, state: &ValidationState) {
  if context.work.is_none() && state.visited_rules.is_empty() {
    *context = Default::default();
  }
}

fn numeric_recursion_marker(
  state: &ValidationState,
  context: &NumericContext,
  category: &str,
  subject: &impl std::fmt::Debug,
) -> Option<String> {
  let location = format!("\u{0}{}", state.data_location);
  if !numeric_recursion_depth_available(state, category) {
    return None;
  }
  let identity = compact_numeric_key(
    context,
    format_args!("{:?}\u{0}{subject:?}", active_generic_context(state)),
  );
  Some(format!("{category}\u{0}{identity}{location}"))
}

fn numeric_recursion_depth_available(state: &ValidationState, category: &str) -> bool {
  let depth = state
    .visited_rules
    .iter()
    .filter(|key| {
      key
        .strip_prefix(category)
        .and_then(|tail| tail.strip_prefix('\0'))
        .and_then(|tail| tail.split_once('\0'))
        .is_some_and(|(_, location)| location == state.data_location)
    })
    .count();
  depth < MAX_NUMERIC_RESOLUTION_DEPTH
}

const MAX_NUMERIC_RESOLUTION_DEPTH: usize = 48;
const MAX_NUMERIC_RESOLUTION_WORK: usize = 4096;

fn charge_numeric_probe(context: &NumericContext) -> std::result::Result<(), ()> {
  if let Some(work) = &context.work {
    use std::sync::atomic::Ordering;
    work
      .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |remaining| {
        remaining.checked_sub(1)
      })
      .map_err(|_| ())?;
  }
  Ok(())
}

#[derive(Clone, Debug, PartialEq)]
enum NumericDomain<'a> {
  Empty,
  Singleton(token::Value<'a>),
  IntegerRange(IntegerBounds),
  IntegerSet(Vec<IntegerBounds>),
  Literals(Vec<token::Value<'a>>),
  Unknown,
}

fn merge_numeric_domains<'a>(
  left: NumericDomain<'a>,
  right: NumericDomain<'a>,
) -> NumericDomain<'a> {
  match (left, right) {
    (NumericDomain::Empty, domain) | (domain, NumericDomain::Empty) => domain,
    (NumericDomain::Singleton(a), NumericDomain::Singleton(b))
      if numeric_literals_equal(&a, &b) =>
    {
      NumericDomain::Singleton(a)
    }
    (left, right) => merge_integer_domains(left, right),
  }
}

fn merge_integer_domains<'a>(
  left: NumericDomain<'a>,
  right: NumericDomain<'a>,
) -> NumericDomain<'a> {
  union_numeric_domains(left, right)
}

fn numeric_literals_equal(a: &token::Value, b: &token::Value) -> bool {
  match (a, b) {
    (token::Value::INT(a), token::Value::UINT(b))
    | (token::Value::UINT(b), token::Value::INT(a)) => *a >= 0 && *a as usize == *b,
    _ => a == b,
  }
}

fn numeric_literal_integer(value: &token::Value) -> Option<i128> {
  match value {
    token::Value::INT(value) => Some(*value as i128),
    token::Value::UINT(value) => Some(*value as i128),
    _ => None,
  }
}

fn numeric_literal_order(a: &token::Value, b: &token::Value) -> Option<std::cmp::Ordering> {
  match (a, b) {
    (token::Value::FLOAT(a), token::Value::FLOAT(b)) => a.partial_cmp(b),
    _ => Some(numeric_literal_integer(a)?.cmp(&numeric_literal_integer(b)?)),
  }
}

fn numeric_instantiation_key(
  identities: &NumericContext,
  context: &str,
  ident: &Identifier,
  args: &[Type1],
) -> String {
  compact_numeric_key(identities, format_args!("{context}\u{0}{ident}<{args:?}>"))
}

enum NumericControllerError<E> {
  Validation(E),
  ResourceLimit,
  Proof(NumericProofFailure),
}

enum NumericValue {
  Integer(i128),
  Float(f64),
}

impl NumericValue {
  fn compare(self, ctrl: ControlOperator, controller: &token::Value) -> bool {
    match (self, controller) {
      (Self::Integer(value), token::Value::INT(bound)) => {
        compare_numeric_values(value, *bound as i128, ctrl)
      }
      (Self::Integer(value), token::Value::UINT(bound)) => {
        compare_numeric_values(value, *bound as i128, ctrl)
      }
      (Self::Float(value), token::Value::FLOAT(bound)) => {
        compare_numeric_values(value, *bound, ctrl)
      }
      (Self::Integer(value), token::Value::FLOAT(bound)) => {
        compare_mixed_numeric(value, *bound, ctrl, false)
      }
      (Self::Float(value), token::Value::INT(bound)) => {
        compare_mixed_numeric(*bound as i128, value, ctrl, true)
      }
      (Self::Float(value), token::Value::UINT(bound)) => {
        compare_mixed_numeric(*bound as i128, value, ctrl, true)
      }
      _ => false,
    }
  }
}

fn compare_numeric_values<T: PartialOrd>(value: T, bound: T, ctrl: ControlOperator) -> bool {
  match ctrl {
    ControlOperator::EQ => value == bound,
    ControlOperator::NE => value != bound,
    ControlOperator::GT => value > bound,
    ControlOperator::GE => value >= bound,
    ControlOperator::LT => value < bound,
    ControlOperator::LE => value <= bound,
    _ => false,
  }
}

fn integer_float_order(integer: i128, float: f64) -> Option<std::cmp::Ordering> {
  use std::cmp::Ordering;
  if float.is_nan() {
    return None;
  }
  if float >= i128::MAX as f64 {
    return Some(Ordering::Less);
  }
  if float < i128::MIN as f64 {
    return Some(Ordering::Greater);
  }
  let order = integer.cmp(&(float as i128));
  if order != Ordering::Equal {
    return Some(order);
  }
  // Compare the fractional remainder only after comparing integral parts
  // exactly; converting the integer to f64 would round 64-bit boundaries.
  0.0_f64.partial_cmp(&float.fract())
}

fn compare_mixed_numeric(integer: i128, float: f64, ctrl: ControlOperator, reverse: bool) -> bool {
  use std::cmp::Ordering;
  if matches!(ctrl, ControlOperator::EQ | ControlOperator::NE) {
    return ctrl == ControlOperator::NE;
  }
  let Some(mut order) = integer_float_order(integer, float) else {
    return false;
  };
  if reverse {
    order = order.reverse();
  }
  match ctrl {
    ControlOperator::GT => order == Ordering::Greater,
    ControlOperator::GE => order != Ordering::Less,
    ControlOperator::LT => order == Ordering::Less,
    ControlOperator::LE => order != Ordering::Greater,
    _ => false,
  }
}

/// Prove a controller singleton only after checking each candidate against
/// its originating alternative. All proof state belongs to this invocation.
#[cfg(test)]
fn numeric_controller_value<'a, E>(
  state: &ValidationState<'a>,
  controller: &Type2<'a>,
  validate: impl FnMut(&Type1<'a>, &token::Value<'a>) -> std::result::Result<bool, E>,
) -> std::result::Result<Option<token::Value<'a>>, NumericControllerError<E>> {
  numeric_controller_result(state, controller, false, validate)
    .map(|values| values.and_then(|mut values| values.pop()))
}

fn numeric_controller_result<'a, E>(
  state: &ValidationState<'a>,
  controller: &Type2<'a>,
  all: bool,
  validate: impl FnMut(&Type1<'a>, &token::Value<'a>) -> std::result::Result<bool, E>,
) -> std::result::Result<Option<Vec<token::Value<'a>>>, NumericControllerError<E>> {
  let bindings = current_generic_bindings(state);
  if !numeric_type2_substitution_fits(controller, &bindings) {
    return Err(NumericControllerError::ResourceLimit);
  }
  let controller = substitute_type2_params(controller, &bindings);
  let Some(mut resolver) = NumericControllerResolver::new(state, validate) else {
    return Err(NumericControllerError::ResourceLimit);
  };
  let domain = resolver
    .type2_value(&controller)
    .map_err(NumericControllerError::Validation)?;
  if let Some(failure) = resolver.failure {
    return Err(NumericControllerError::Proof(failure));
  }
  if resolver.exhausted {
    return Err(NumericControllerError::ResourceLimit);
  }
  let values = if all {
    finite_literals(&domain, resolver.remaining)
  } else {
    match domain {
      NumericDomain::Singleton(value) => Some(vec![value]),
      _ => None,
    }
  };
  Ok(values.map(|values| {
    values
      .into_iter()
      .map(|value| match value {
        token::Value::INT(value) if value >= 0 => token::Value::UINT(value as usize),
        value => value,
      })
      .collect()
  }))
}

struct NumericControllerResolver<'s, 'a, F> {
  state: &'s ValidationState<'a>,
  numeric_context: NumericContext,
  context: String,
  validate: F,
  active: HashSet<String>,
  negative: HashSet<String>,
  memo: HashMap<String, NumericDomain<'a>>,
  cycle_cuts: usize,
  candidate_target: bool,
  depth: usize,
  remaining: usize,
  exhausted: bool,
  failure: Option<NumericProofFailure>,
}

impl<'s, 'a, E, F> NumericControllerResolver<'s, 'a, F>
where
  F: FnMut(&Type1<'a>, &token::Value<'a>) -> std::result::Result<bool, E>,
{
  fn new(state: &'s ValidationState<'a>, validate: F) -> Option<Self> {
    let numeric_context = NumericContext::default();
    let context = compact_numeric_key(
      &numeric_context,
      format_args!("{:?}", active_generic_context(state)),
    );
    Some(NumericControllerResolver {
      state,
      numeric_context,
      context,
      validate,
      active: HashSet::new(),
      negative: HashSet::new(),
      memo: HashMap::new(),
      cycle_cuts: 0,
      candidate_target: false,
      depth: 0,
      remaining: MAX_NUMERIC_RESOLUTION_WORK,
      exhausted: false,
      failure: None,
    })
  }

  fn type2_value(&mut self, value: &Type2<'a>) -> std::result::Result<NumericDomain<'a>, E> {
    self.bounded_resolution(|resolver| resolver.resolve_type2(value))
  }

  fn bounded_resolution(
    &mut self,
    resolve: impl FnOnce(&mut Self) -> std::result::Result<NumericDomain<'a>, E>,
  ) -> std::result::Result<NumericDomain<'a>, E> {
    if self.failure.is_some() {
      return Ok(NumericDomain::Unknown);
    }
    // Check before resolving arguments, formatting keys or cloning/substituting
    // bodies: changing-argument cycles otherwise evade exact-key detection.
    if self.depth >= MAX_NUMERIC_RESOLUTION_DEPTH || self.remaining == 0 {
      self.exhausted = true;
      return Ok(NumericDomain::Unknown);
    }
    self.remaining -= 1;
    self.depth += 1;
    let result = resolve(self);
    self.depth -= 1;
    result
  }

  fn resolve_type2(&mut self, value: &Type2<'a>) -> std::result::Result<NumericDomain<'a>, E> {
    match value {
      Type2::IntValue { value, .. } => Ok(NumericDomain::Singleton(token::Value::INT(*value))),
      Type2::UintValue { value, .. } => Ok(NumericDomain::Singleton(token::Value::UINT(*value))),
      Type2::FloatValue { value, .. } => Ok(NumericDomain::Singleton(token::Value::FLOAT(*value))),
      Type2::ParenthesizedType { pt, .. } => self.choices_value(
        pt.type_choices.iter().map(|choice| &choice.type1),
        &HashMap::new(),
      ),
      Type2::Typename {
        ident,
        generic_args,
        ..
      } => self.ident_value(ident, generic_args.as_ref()),
      Type2::ChoiceFromInlineGroup { group, .. } => self.group_value(group, &HashMap::new()),
      Type2::ChoiceFromGroup {
        ident,
        generic_args,
        ..
      } => self.ident_value_mode(ident, generic_args.as_ref(), true),
      _ => Ok(NumericDomain::Unknown),
    }
  }

  fn type1_value(&mut self, value: &Type1<'a>) -> std::result::Result<NumericDomain<'a>, E> {
    if let Some(domain) = self.ordered_integer_value(value)? {
      return Ok(domain);
    }
    let candidate = self.type1_candidate(value)?;
    if self.exhausted || self.failure.is_some() {
      return Ok(NumericDomain::Unknown);
    }
    // Ranges and computed sums are already proved by their resolved operands.
    // Replaying their unresolved aliases would lose the operands' constraints.
    if value
      .operator
      .as_ref()
      .is_some_and(|operator| complete_numeric_candidate(&operator.operator))
    {
      return Ok(candidate);
    }
    if value.operator.is_some()
      && matches!(
        candidate,
        NumericDomain::IntegerRange(_) | NumericDomain::IntegerSet(_) | NumericDomain::Literals(_)
      )
    {
      return Ok(NumericDomain::Unknown);
    }
    if let (Some(_), NumericDomain::Singleton(literal)) = (&value.operator, &candidate) {
      let mut alternative = value.clone();
      let target = self.type2_value(&value.type2)?;
      if self.exhausted {
        return Ok(NumericDomain::Unknown);
      }
      if target == NumericDomain::Empty {
        return Ok(NumericDomain::Empty);
      }
      if target == candidate
        && matches!(
          value.operator.as_ref().map(|operator| &operator.operator),
          Some(RangeCtlOp::CtlOp { ctrl, .. }) if numeric_comparison(*ctrl)
        )
      {
        // The entire target domain has already been proved, including its
        // constraints. Do not replay an alias DAG in the ordinary evaluator.
        alternative.type2 = Type1::from(literal.clone()).type2;
      }
      if !(self.validate)(&alternative, literal)? {
        return Ok(NumericDomain::Empty);
      }
    }
    Ok(candidate)
  }

  fn type1_candidate(&mut self, value: &Type1<'a>) -> std::result::Result<NumericDomain<'a>, E> {
    match value.operator.as_ref().map(|operator| &operator.operator) {
      Some(RangeCtlOp::RangeOp { is_inclusive, .. }) => self.range_value(
        &value.type2,
        &value.operator.as_ref().unwrap().type2,
        *is_inclusive,
      ),
      #[cfg(feature = "additional-controls")]
      Some(RangeCtlOp::CtlOp {
        ctrl: ControlOperator::PLUS,
        ..
      }) => self.plus_value(&value.type2, &value.operator.as_ref().unwrap().type2),
      Some(RangeCtlOp::CtlOp {
        ctrl: ControlOperator::AND | ControlOperator::WITHIN,
        ..
      }) => self.intersection_value(&value.type2, &value.operator.as_ref().unwrap().type2),
      Some(RangeCtlOp::CtlOp {
        ctrl: ControlOperator::EQ | ControlOperator::NE,
        ..
      }) => self.equality_value(
        &value.type2,
        &value.operator.as_ref().unwrap().type2,
        matches!(
          &value.operator.as_ref().unwrap().operator,
          RangeCtlOp::CtlOp {
            ctrl: ControlOperator::NE,
            ..
          }
        ),
      ),
      _ => self.type2_value(&value.type2),
    }
  }

  fn range_value(
    &mut self,
    lower: &Type2<'a>,
    upper: &Type2<'a>,
    inclusive: bool,
  ) -> std::result::Result<NumericDomain<'a>, E> {
    let (lower, upper) = (self.type2_value(lower)?, self.type2_value(upper)?);
    if lower == NumericDomain::Empty || upper == NumericDomain::Empty {
      return Ok(NumericDomain::Empty);
    }
    let (NumericDomain::Singleton(lower), NumericDomain::Singleton(upper)) = (lower, upper) else {
      return Ok(NumericDomain::Unknown);
    };
    if let (Some(lower), Some(upper)) = (
      numeric_literal_integer(&lower),
      numeric_literal_integer(&upper),
    ) {
      return Ok(self.proved_integer(lower, upper - i128::from(!inclusive)));
    }
    use std::cmp::Ordering;
    Ok(match numeric_literal_order(&lower, &upper) {
      Some(Ordering::Greater) => NumericDomain::Empty,
      Some(Ordering::Equal) if !inclusive => NumericDomain::Empty,
      Some(Ordering::Equal) => NumericDomain::Singleton(lower),
      Some(Ordering::Less)
        if !inclusive
          && numeric_literal_integer(&lower).is_some_and(|lower| {
            numeric_literal_integer(&upper).and_then(|upper| upper.checked_sub(lower)) == Some(1)
          }) =>
      {
        NumericDomain::Singleton(lower)
      }
      _ => NumericDomain::Unknown,
    })
  }

  #[cfg(feature = "additional-controls")]
  fn plus_value(
    &mut self,
    target: &Type2<'a>,
    controller: &Type2<'a>,
  ) -> std::result::Result<NumericDomain<'a>, E> {
    let cuts = self.cycle_cuts;
    let (target, controller) = (self.type2_value(target)?, self.type2_value(controller)?);
    if self.cycle_cuts != cuts {
      return Ok(self.failed_proof(NumericProofFailure::CyclicComputation));
    }
    if target == NumericDomain::Empty || controller == NumericDomain::Empty {
      return Ok(NumericDomain::Empty);
    }
    let Some(targets) = finite_literals(&target, self.remaining) else {
      return Ok(NumericDomain::Unknown);
    };
    let Some(controllers) = finite_literals(&controller, self.remaining) else {
      return Ok(NumericDomain::Unknown);
    };
    let mut result = NumericDomain::Empty;
    for target in targets {
      for controller in &controllers {
        if !self.proof_step() {
          return Ok(NumericDomain::Unknown);
        }
        match checked_numeric_addition(&target, controller) {
          Ok(value) => result = merge_numeric_domains(result, NumericDomain::Singleton(value)),
          Err(failure) => return Ok(self.failed_proof(failure)),
        }
      }
    }
    Ok(result)
  }

  fn choices_value<'b>(
    &mut self,
    choices: impl IntoIterator<Item = &'b Type1<'a>>,
    bindings: &HashMap<&'a str, Type1<'a>>,
  ) -> std::result::Result<NumericDomain<'a>, E>
  where
    'a: 'b,
  {
    let mut value = NumericDomain::Empty;
    for choice in choices {
      if !numeric_substitution_fits(choice, bindings) {
        self.exhausted = true;
        return Ok(NumericDomain::Unknown);
      }
      let expanded = substitute_generic_params(choice, bindings);
      value = merge_numeric_domains(value, self.type1_value(&expanded)?);
      if value == NumericDomain::Unknown {
        return Ok(value);
      }
    }
    Ok(value)
  }

  fn resolve_args(&mut self, args: Option<&GenericArgs<'a>>) -> Option<Vec<Type1<'a>>> {
    let Some(args) = args else {
      return Some(Vec::new());
    };
    if !generic_arguments_fit(args, &HashMap::new()) {
      return None;
    }
    Some(
      args
        .args
        .iter()
        .map(|arg| normalize_generic_argument((*arg.arg).clone()))
        .collect(),
    )
  }

  fn ident_value(
    &mut self,
    ident: &Identifier<'a>,
    args: Option<&GenericArgs<'a>>,
  ) -> std::result::Result<NumericDomain<'a>, E> {
    self.ident_value_mode(ident, args, false)
  }

  fn ident_value_mode(
    &mut self,
    ident: &Identifier<'a>,
    args: Option<&GenericArgs<'a>>,
    group: bool,
  ) -> std::result::Result<NumericDomain<'a>, E> {
    let context = format!("{}{}", if group { "&" } else { "" }, self.context);
    let Some((resolved, key)) = self.resolve_args(args).map(|resolved| {
      let key = numeric_instantiation_key(&self.numeric_context, &context, ident, &resolved);
      (resolved, key)
    }) else {
      self.exhausted = true;
      return Ok(NumericDomain::Unknown);
    };
    if let Some(cached) = self.memo.get(&key) {
      return Ok(cached.clone());
    }
    if !self.active.insert(key.clone()) {
      return Ok(self.cut_domain(&key));
    }
    let cuts = self.cycle_cuts;
    let result = if group {
      self.group_rule_value(ident, resolved)
    } else {
      self.rule_value(ident, resolved)
    };
    self.active.remove(&key);
    // A cut is a least-fixed-point assumption about the active chain, not a
    // completed proof. Never reuse that result from an unguarded context.
    if cuts == self.cycle_cuts && !self.exhausted && self.failure.is_none() {
      if let Ok(value) = &result {
        self.memo.insert(key, value.clone());
      }
    }
    result
  }

  fn group_rule_value(
    &mut self,
    ident: &Identifier<'a>,
    args: Vec<Type1<'a>>,
  ) -> std::result::Result<NumericDomain<'a>, E> {
    let params = enumeration_rule_from_ident(self.state.cddl, ident)
      .and_then(generic_params_from_rule)
      .unwrap_or_default();
    let bindings = params.into_iter().zip(args).collect();
    let mut found = false;
    let mut result = NumericDomain::Empty;
    for rule in &self.state.cddl.rules {
      if let Rule::Group { rule, .. } = rule {
        if rule.name == *ident {
          found = true;
          result = merge_numeric_domains(result, self.group_entry_value(&rule.entry, &bindings)?);
        }
      }
    }
    Ok(if found {
      result
    } else {
      NumericDomain::Unknown
    })
  }

  fn group_value(
    &mut self,
    group: &Group<'a>,
    bindings: &HashMap<&'a str, Type1<'a>>,
  ) -> std::result::Result<NumericDomain<'a>, E> {
    let mut result = NumericDomain::Empty;
    for choice in &group.group_choices {
      for (entry, _) in &choice.group_entries {
        result = merge_numeric_domains(result, self.group_entry_value(entry, bindings)?);
        if result == NumericDomain::Unknown || self.exhausted {
          return Ok(result);
        }
      }
    }
    Ok(result)
  }

  fn group_entry_value(
    &mut self,
    entry: &GroupEntry<'a>,
    bindings: &HashMap<&'a str, Type1<'a>>,
  ) -> std::result::Result<NumericDomain<'a>, E> {
    self.bounded_resolution(|resolver| {
      if !group_entry_substitution_fits(entry, bindings) {
        resolver.exhausted = true;
        return Ok(NumericDomain::Unknown);
      }
      let mut group = Group::from(entry.clone());
      substitute_group_params(&mut group, bindings);
      resolver.resolve_group_entry(&group.group_choices[0].group_entries[0].0)
    })
  }

  fn resolve_group_entry(
    &mut self,
    entry: &GroupEntry<'a>,
  ) -> std::result::Result<NumericDomain<'a>, E> {
    match entry {
      GroupEntry::ValueMemberKey { ge, .. } => self.choices_value(
        ge.entry_type
          .type_choices
          .iter()
          .map(|choice| &choice.type1),
        &HashMap::new(),
      ),
      GroupEntry::InlineGroup { group, .. } => self.group_value(group, &HashMap::new()),
      GroupEntry::TypeGroupname { ge, .. } => self.ident_value_mode(
        &ge.name,
        ge.generic_args.as_ref(),
        enumeration_rule_from_ident(self.state.cddl, &ge.name)
          .is_some_and(|rule| matches!(rule, Rule::Group { .. })),
      ),
    }
  }

  fn rule_value(
    &mut self,
    ident: &Identifier<'a>,
    args: Vec<Type1<'a>>,
  ) -> std::result::Result<NumericDomain<'a>, E> {
    let types = type_choice_types_from_ident(self.state.cddl, ident);
    if types.is_empty() {
      return Ok(primitive_integer_domain(ident));
    }
    let params = rule_from_ident(self.state.cddl, ident)
      .and_then(generic_params_from_rule)
      .unwrap_or_default();
    let bindings = params.into_iter().zip(args).collect();
    self.choices_value(
      types
        .iter()
        .flat_map(|ty| ty.type_choices.iter().map(|choice| &choice.type1)),
      &bindings,
    )
  }
}

/// The arguments of a generic instantiation resolved against the caller's
/// evaluation environment.
pub(crate) fn resolved_generic_args<'a>(
  state: &ValidationState<'a>,
  args: &GenericArgs<'a>,
) -> std::result::Result<Vec<Type1<'a>>, ()> {
  let bindings = current_generic_bindings(state);
  if !generic_arguments_fit(args, &bindings)
    || !numeric_recursion_depth_available(state, "#generic")
  {
    return Err(());
  }
  Ok(
    args
      .args
      .iter()
      .map(|arg| resolve_argument_in_caller(state, &arg.arg))
      .collect(),
  )
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

/// Float prelude names constrain IEEE 754 representability, not CBOR encoding
/// width. NaNs and infinities belong to every precision's value domain.
pub(crate) fn primitive_float_ident_matches_value(ident: &Identifier, value: f64) -> Option<bool> {
  match lookup_ident(ident.ident) {
    Token::FLOAT16 => Some(value.is_nan() || half::f16::from_f64(value).to_f64() == value),
    Token::FLOAT32 | Token::FLOAT1632 => Some(value.is_nan() || f64::from(value as f32) == value),
    Token::FLOAT | Token::FLOAT64 | Token::FLOAT3264 => Some(true),
    _ => None,
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
            count += nested_entry_counts.first().map_or(0, |ec| ec.count);
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
                count += entry_counts_from_group(cddl, group)
                  .first()
                  .map_or(0, |ec| ec.count);
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

  fn numeric_test_state<'a>(cddl: &'a CDDL<'a>) -> ValidationState<'a> {
    ValidationState::new(
      cddl,
      #[cfg(feature = "additional-controls")]
      None,
    )
  }

  fn numeric_rule_target<'a>(cddl: &'a CDDL<'a>, name: &str) -> &'a Type2<'a> {
    cddl
      .rules
      .iter()
      .find_map(|rule| match rule {
        Rule::Type { rule, .. } if rule.name.ident == name => {
          Some(&rule.value.type_choices[0].type1.type2)
        }
        _ => None,
      })
      .unwrap()
  }

  #[test]
  fn numeric_controller_cycle_cuts_are_not_cached_as_completed_proofs() {
    let cddl = cddl_from_str("root = a\na = b\nb = a / 0.0", true).unwrap();
    let state = numeric_test_state(&cddl);
    let mut resolver = NumericControllerResolver::new(&state, |_, _| Ok::<_, ()>(true)).unwrap();
    let target = numeric_rule_target(&cddl, "root");
    let Type2::Typename { ident, .. } = numeric_rule_target(&cddl, "a") else {
      unreachable!()
    };
    resolver.active.insert(numeric_instantiation_key(
      &resolver.numeric_context,
      &resolver.context,
      ident,
      &[],
    ));
    assert_eq!(resolver.type2_value(target).unwrap(), NumericDomain::Empty);
    assert!(resolver.memo.is_empty());
    resolver.active.clear();
    assert_eq!(
      resolver.type2_value(target).unwrap(),
      NumericDomain::Singleton(token::Value::FLOAT(0.0)),
    );
    assert!(resolver.memo.is_empty());
    assert_eq!(resolver.cycle_cuts, 2);
  }

  #[test]
  fn numeric_controller_callback_failures_are_not_cached_or_left_active() {
    let cddl = cddl_from_str("root = a\na = float64 .eq 0.0", true).unwrap();
    let state = numeric_test_state(&cddl);
    let mut fail = true;
    let mut resolver = NumericControllerResolver::new(&state, |_, _| {
      if std::mem::take(&mut fail) {
        Err(())
      } else {
        Ok(true)
      }
    })
    .unwrap();
    let target = numeric_rule_target(&cddl, "root");
    let Type2::Typename { ident, .. } = target else {
      unreachable!()
    };
    let key = numeric_instantiation_key(&resolver.numeric_context, &resolver.context, ident, &[]);
    assert_eq!(resolver.type2_value(target), Err(()));
    assert!(resolver.active.is_empty());
    assert!(!resolver.memo.contains_key(&key));
    assert_eq!(resolver.depth, 0);
    assert_eq!(
      resolver.type2_value(target).unwrap(),
      NumericDomain::Singleton(token::Value::FLOAT(0.0))
    );
  }

  #[test]
  fn numeric_controller_completed_proofs_have_linear_diamond_work() {
    for base in ["0.0", "(float16 .eq 1.1)", "tstr"] {
      let mut schema = format!("root = c30\nc0 = {base}\n");
      for depth in 1..=30 {
        schema.push_str(&format!("c{depth} = c{} / c{}\n", depth - 1, depth - 1));
      }
      let cddl = cddl_from_str(&schema, true).unwrap();
      let state = numeric_test_state(&cddl);
      let mut resolver = NumericControllerResolver::new(&state, |_, candidate| {
        Ok::<_, ()>(candidate != &token::Value::FLOAT(1.1))
      })
      .unwrap();
      let expected = match base {
        "0.0" => NumericDomain::Singleton(token::Value::FLOAT(0.0)),
        "tstr" => NumericDomain::Unknown,
        _ => NumericDomain::Empty,
      };
      assert_eq!(
        resolver
          .type2_value(numeric_rule_target(&cddl, "root"))
          .unwrap(),
        expected
      );
      assert!(!resolver.exhausted);
      assert!(resolver.remaining >= MAX_NUMERIC_RESOLUTION_WORK - 128);
      assert!(resolver.memo.len() <= 33);
    }
  }

  #[test]
  fn numeric_controller_cache_keys_keep_literal_and_binding_structure() {
    let cddl = cddl_from_str(
      "integer = id<0>\nfloat = id<0.0>\nbound = id<(float64 .lt 0.0)>\nid<t> = t",
      true,
    )
    .unwrap();
    let state = numeric_test_state(&cddl);
    let mut resolver = NumericControllerResolver::new(&state, |_, _| Ok::<_, ()>(true)).unwrap();
    assert_eq!(
      resolver
        .type2_value(numeric_rule_target(&cddl, "integer"))
        .unwrap(),
      NumericDomain::Singleton(token::Value::UINT(0))
    );
    assert_eq!(
      resolver
        .type2_value(numeric_rule_target(&cddl, "float"))
        .unwrap(),
      NumericDomain::Singleton(token::Value::FLOAT(0.0))
    );
    assert_eq!(
      resolver
        .type2_value(numeric_rule_target(&cddl, "bound"))
        .unwrap(),
      NumericDomain::Unknown
    );
    assert_eq!(resolver.memo.len(), 4);
    let remaining = resolver.remaining;
    resolver
      .type2_value(numeric_rule_target(&cddl, "integer"))
      .unwrap();
    assert_eq!(resolver.remaining, remaining - 1);
  }

  #[test]
  fn numeric_controller_growth_has_explicit_per_invocation_limits() {
    for argument in ["[t]", "[t, t]"] {
      let schema = format!("root = zero<float64>\nzero<t> = 0.0 / zero<{argument}>");
      let cddl = cddl_from_str(&schema, true).unwrap();
      let state = numeric_test_state(&cddl);
      let mut resolver = NumericControllerResolver::new(&state, |_, _| Ok::<_, ()>(true)).unwrap();
      let result = resolver
        .type2_value(numeric_rule_target(&cddl, "root"))
        .unwrap();
      assert_eq!(result, NumericDomain::Unknown);
      assert!(resolver.exhausted);
      assert!(resolver.remaining > 0);
      assert_eq!(resolver.depth, 0);
      assert!(resolver.memo.is_empty());
    }
    let cddl = cddl_from_str("root = 0.0", true).unwrap();
    let state = numeric_test_state(&cddl);
    let result = numeric_controller_value(&state, numeric_rule_target(&cddl, "root"), |_, _| {
      Ok::<_, ()>(true)
    });
    assert!(matches!(result, Ok(Some(token::Value::FLOAT(0.0)))));
  }

  #[cfg(feature = "json")]
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

    documents.iter().all(|doc| {
      cddl_schema
        .validate_json(
          doc.as_bytes(),
          #[cfg(feature = "additional-controls")]
          None,
        )
        .is_ok()
    });
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
