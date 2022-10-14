#![cfg(not(feature = "lsp"))]

/// CBOR validation implementation
pub mod cbor;
/// JSON validation implementation
pub mod json;

/// parent visitor implementation
mod parent_visitor;

mod control;

use crate::{
  ast::{
    Group, GroupChoice, GroupEntry, GroupRule, Identifier, Occur, Rule, Type, Type2, TypeChoice,
    TypeRule, CDDL,
  },
  token::*,
  visitor::Visitor,
};

use std::error::Error;

#[cfg(feature = "cbor")]
use cbor::CBORValidator;
#[cfg(feature = "cbor")]
use ciborium;
#[cfg(feature = "json")]
use json::JSONValidator;
use serde::de::Deserialize;

#[cfg(target_arch = "wasm32")]
use crate::{
  error::ErrorMsg,
  lexer::Position,
  parser::{self, Parser},
};
#[cfg(target_arch = "wasm32")]
use serde::Serialize;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(not(target_arch = "wasm32"))]
use crate::cddl_from_str;

#[cfg(target_arch = "wasm32")]
#[derive(Serialize)]
struct ParserError {
  position: Position,
  msg: ErrorMsg,
}

trait Validator<'a, 'b, E: Error>: Visitor<'a, 'b, E> {
  fn validate(&mut self) -> std::result::Result<(), E>;
  fn add_error(&mut self, reason: String);
}

impl CDDL<'_> {
  /// Validate the given document against the CDDL definition
  pub fn validate(
    &self,
    document: &[u8],
    #[cfg(feature = "additional-controls")]
    #[cfg(not(target_arch = "wasm32"))]
    enabled_features: Option<&[&str]>,
    #[cfg(feature = "additional-controls")]
    #[cfg(target_arch = "wasm32")]
    enabled_features: Option<Box<[JsValue]>>,
  ) -> Result<(), Box<dyn Error>> {
    if std::str::from_utf8(document).is_ok() {
      let json =
        serde_json::from_slice::<serde_json::Value>(document).map_err(json::Error::JSONParsing)?;

      #[cfg(feature = "additional-controls")]
      let mut jv = JSONValidator::new(self, json, enabled_features);
      #[cfg(not(feature = "additional-controls"))]
      let mut jv = JSONValidator::new(&cddl, json);

      return jv.validate().map_err(|e| e.into());
    }

    let cbor: ciborium::value::Value = ciborium::de::from_reader(document)?;

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
  let mut p = Parser::new(cddl, Box::new(crate::lexer::lexer_from_str(cddl).iter()))
    .map_err(|e| JsValue::from(e.to_string()))?;
  let c = p.parse_cddl().map_err(|e| JsValue::from(e.to_string()))?;
  if !p.errors.is_empty() {
    return Err(
      serde_wasm_bindgen::to_value(
        &p.errors
          .iter()
          .filter_map(|e| {
            if let parser::Error::PARSER { position, msg } = e {
              Some(ParserError {
                position: *position,
                msg: msg.clone(),
              })
            } else {
              None
            }
          })
          .collect::<Vec<ParserError>>(),
      )
      .map_err(|e| JsValue::from(e.to_string()))?,
    );
  }

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
  let mut l = Lexer::new(cddl);
  let mut p = Parser::new((&mut l).iter(), cddl).map_err(|e| JsValue::from(e.to_string()))?;
  let c = p.parse_cddl().map_err(|e| JsValue::from(e.to_string()))?;
  if !p.errors.is_empty() {
    return Err(
      JsValue::from_serde(
        &p.errors
          .iter()
          .filter_map(|e| {
            if let parser::Error::PARSER { position, msg } = e {
              Some(ParserError {
                position: *position,
                msg: msg.clone(),
              })
            } else {
              None
            }
          })
          .collect::<Vec<ParserError>>(),
      )
      .map_err(|e| JsValue::from(e.to_string()))?,
    );
  }

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

  let cbor: ciborium::value::Value =
    ciborium::de::from_reader(cbor_slice).map_err(cbor::Error::CBORParsing)?;

  let mut cv = CBORValidator::new(&cddl, cbor, enabled_features);
  cv.validate()
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "cbor")]
#[cfg(not(feature = "additional-controls"))]
/// Validate CBOR slice from a given CDDL document string
pub fn validate_cbor_from_slice(cddl: &str, cbor_slice: &[u8]) -> cbor::Result<std::io::Error> {
  let mut lexer = lexer_from_str(cddl);
  let cddl = cddl_from_str(&mut lexer, cddl, true).map_err(cbor::Error::CDDLParsing)?;
  let cbor: ciborium::value::Value =
    ciborium::de::from_reader(cbor_slice).map_err(cbor::Error::CBORParsing)?;

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
  let mut p = Parser::new(cddl, Box::new(crate::lexer::lexer_from_str(cddl).iter()))
    .map_err(|e| JsValue::from(e.to_string()))?;
  let c = p.parse_cddl().map_err(|e| JsValue::from(e.to_string()))?;
  if !p.errors.is_empty() {
    return Err(
      serde_wasm_bindgen::to_value(
        &p.errors
          .iter()
          .filter_map(|e| {
            if let parser::Error::PARSER { position, msg } = e {
              Some(ParserError {
                position: *position,
                msg: msg.clone(),
              })
            } else {
              None
            }
          })
          .collect::<Vec<ParserError>>(),
      )
      .map_err(|e| JsValue::from(e.to_string()))?,
    );
  }

  let cbor: ciborium::value::Value =
    ciborium::de::from_reader(cbor_slice).map_err(|e| JsValue::from(e.to_string()))?;

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
  let mut l = Lexer::new(cddl);
  let mut p = Parser::new((&mut l).iter(), cddl).map_err(|e| JsValue::from(e.to_string()))?;
  let c = p.parse_cddl().map_err(|e| JsValue::from(e.to_string()))?;
  if !p.errors.is_empty() {
    return Err(
      JsValue::from_serde(
        &p.errors
          .iter()
          .filter_map(|e| {
            if let parser::Error::PARSER { position, msg } = e {
              Some(ParserError {
                position: *position,
                msg: msg.clone(),
              })
            } else {
              None
            }
          })
          .collect::<Vec<ParserError>>(),
      )
      .map_err(|e| JsValue::from(e.to_string()))?,
    );
  }

  let cbor: ciborium::value::Value =
    ciborium::de::from_reader(cbor_slice).map_err(|e| JsValue::from(e.to_string()))?;

  let mut cv = CBORValidator::new(&c, cbor);
  cv.validate()
    .map_err(|e| JsValue::from(e.to_string()))
    .map(|_| JsValue::default())
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

/// Is the given identifier associated with an integer data type
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

/// Is the given identifier associated with a float data type
pub fn is_ident_float_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let Token::FLOAT
  | Token::FLOAT16
  | Token::FLOAT1632
  | Token::FLOAT32
  | Token::FLOAT3264
  | Token::FLOAT64 = lookup_ident(ident.ident)
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

/// Validate array length and \[non\]homogeneity based on a given optional
/// occurrence indicator. The first bool in the returned tuple indicates whether
/// or not a subsequent validation of the array's elements shouch be homogenous.
/// The second bool in the returned tuple indicates whether or not an empty
/// array is allowed during a subsequent validation of the array's elements.
pub fn validate_array_occurrence<'de, T: Deserialize<'de>>(
  occurrence: Option<&Occur>,
  entry_counts: Option<&[EntryCount]>,
  values: &[T],
) -> std::result::Result<(bool, bool), Vec<String>> {
  let mut iter_items = false;
  #[cfg(feature = "ast-span")]
  let allow_empty_array = matches!(occurrence, Some(Occur::Optional { .. }));
  #[cfg(not(feature = "ast-span"))]
  let allow_empty_array = matches!(occurrence, Some(Occur::Optional {}));

  let mut errors = Vec::new();

  match occurrence {
    #[cfg(feature = "ast-span")]
    Some(Occur::ZeroOrMore { .. }) => iter_items = true,
    #[cfg(not(feature = "ast-span"))]
    Some(Occur::ZeroOrMore {}) => iter_items = true,
    #[cfg(feature = "ast-span")]
    Some(Occur::OneOrMore { .. }) => {
      if values.is_empty() {
        errors.push("array must have at least one item".to_string());
      } else {
        iter_items = true;
      }
    }
    #[cfg(not(feature = "ast-span"))]
    Some(Occur::OneOrMore {}) => {
      if values.is_empty() {
        errors.push("array must have at least one item".to_string());
      } else {
        iter_items = true;
      }
    }
    Some(Occur::Exact { lower, upper, .. }) => {
      if let Some(lower) = lower {
        if let Some(upper) = upper {
          if lower == upper && values.len() != *lower {
            errors.push(format!("array must have exactly {} items", lower));
          }
          if values.len() < *lower || values.len() > *upper {
            errors.push(format!(
              "array must have between {} and {} items",
              lower, upper
            ));
          }
        } else if values.len() < *lower {
          errors.push(format!("array must have at least {} items", lower));
        }
      } else if let Some(upper) = upper {
        if values.len() > *upper {
          errors.push(format!("array must have not more than {} items", upper));
        }
      }

      iter_items = true;
    }
    #[cfg(feature = "ast-span")]
    Some(Occur::Optional { .. }) => {
      if values.len() > 1 {
        errors.push("array must have 0 or 1 items".to_string());
      }

      iter_items = false;
    }
    #[cfg(not(feature = "ast-span"))]
    Some(Occur::Optional {}) => {
      if values.len() > 1 {
        errors.push("array must have 0 or 1 items".to_string());
      }

      iter_items = false;
    }
    None => {
      if values.is_empty() {
        errors.push("array must have exactly one item".to_string());
      } else {
        iter_items = false;
      }
    }
  }

  if !iter_items && !allow_empty_array {
    if let Some(entry_counts) = entry_counts {
      let len = values.len();
      if !validate_entry_count(entry_counts, len) {
        for ec in entry_counts.iter() {
          if let Some(occur) = &ec.entry_occurrence {
            errors.push(format!(
              "expected array with length per occurrence {}",
              occur,
            ));
          } else {
            errors.push(format!(
              "expected array with length {}, got {}",
              ec.count, len
            ));
          }
        }
      }
    }
  }

  if !errors.is_empty() {
    return Err(errors);
  }

  Ok((iter_items, allow_empty_array))
}

/// Retrieve number of group entries from a group. This is currently only used
/// for determining map equality/inequality and for validating the number of
/// entries in arrays, but may be useful in other contexts. The occurrence is
/// only captured for the second element of the CDDL array to avoid ambiguity in
/// non-homogenous array definitions
pub fn entry_counts_from_group<'a, 'b: 'a>(
  cddl: &'a CDDL,
  group: &'b Group<'a>,
) -> Vec<EntryCount> {
  // Each EntryCount is associated with a group choice in the given group
  let mut entry_counts = Vec::new();

  for gc in group.group_choices.iter() {
    let mut count = 0;
    let mut entry_occurrence = None;

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

          entry_counts = entry_counts_from_group(cddl, group);
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

    entry_counts.push(EntryCount {
      count,
      entry_occurrence,
    });
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
    }
  }
}

#[cfg(test)]
mod tests {
  #![cfg(not(target_arch = "wasm32"))]

  use super::*;

  #[test]
  fn validate() {
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
      .all(|doc| cddl_schema.validate(doc.as_bytes(), None).is_ok());
  }
}
