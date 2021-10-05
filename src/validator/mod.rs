#![cfg(not(feature = "lsp"))]

/// CBOR validation implementation
pub mod cbor;
/// JSON validation implementation
pub mod json;

mod control;

use crate::{ast::Occur, token::*, util::EntryCount, visitor::Visitor};

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
  error::ParserError,
  lexer::Lexer,
  parser::{self, Parser},
};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(not(target_arch = "wasm32"))]
use crate::{cddl_from_str, lexer_from_str};

trait Validator<'a, E: Error>: Visitor<'a, E> {
  fn validate(&mut self) -> std::result::Result<(), E>;
  fn add_error(&mut self, reason: String);
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "json")]
/// Validate JSON string from a given CDDL document string
pub fn validate_json_from_str(
  cddl: &str,
  json: &str,
  #[cfg(feature = "additional-controls")] enabled_features: Option<&[&str]>,
) -> json::Result {
  let mut lexer = lexer_from_str(cddl);
  let cddl = cddl_from_str(&mut lexer, cddl, true).map_err(json::Error::CDDLParsing)?;
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
  let mut lexer = lexer_from_str(cddl);
  let cddl = cddl_from_str(&mut lexer, cddl, true).map_err(cbor::Error::CDDLParsing)?;

  let cbor: ciborium::value::Value = ciborium::de::from_reader(cbor_slice).unwrap();

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
  let allow_empty_array = matches!(occurrence, Some(Occur::Optional(_)));
  #[cfg(not(feature = "ast-span"))]
  let allow_empty_array = matches!(occurrence, Some(Occur::Optional));

  let mut errors = Vec::new();

  match occurrence {
    #[cfg(feature = "ast-span")]
    Some(Occur::ZeroOrMore(_)) => iter_items = true,
    #[cfg(not(feature = "ast-span"))]
    Some(Occur::ZeroOrMore) => iter_items = true,
    #[cfg(feature = "ast-span")]
    Some(Occur::OneOrMore(_)) => {
      if values.is_empty() {
        errors.push("array must have at least one item".to_string());
      } else {
        iter_items = true;
      }
    }
    #[cfg(not(feature = "ast-span"))]
    Some(Occur::OneOrMore) => {
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
    Some(Occur::Optional(_)) => {
      if values.len() > 1 {
        errors.push("array must have 0 or 1 items".to_string());
      }

      iter_items = false;
    }
    #[cfg(not(feature = "ast-span"))]
    Some(Occur::Optional) => {
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
              "expecting array with length per occurrence {}",
              occur,
            ));
          } else {
            errors.push(format!(
              "expecting array with length {}, got {}",
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

/// Validate the number of entries given an array of possible valid entry counts
pub fn validate_entry_count(valid_entry_counts: &[EntryCount], num_entries: usize) -> bool {
  valid_entry_counts.iter().any(|ec| {
    num_entries == ec.count as usize
      || match ec.entry_occurrence {
        #[cfg(feature = "ast-span")]
        Some(Occur::ZeroOrMore(_)) | Some(Occur::Optional(_)) => true,
        #[cfg(not(feature = "ast-span"))]
        Some(Occur::ZeroOrMore) | Some(Occur::Optional) => true,
        #[cfg(feature = "ast-span")]
        Some(Occur::OneOrMore(_)) if num_entries > 0 => true,
        #[cfg(not(feature = "ast-span"))]
        Some(Occur::OneOrMore) if num_entries > 0 => true,
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
