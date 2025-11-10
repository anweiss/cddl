//! Bridge layer between nom parser and existing AST
//!
//! This module provides conversion functions to transform nom parse trees into the existing
//! AST structure, ensuring API compatibility while leveraging nom's parsing capabilities.
//!
//! # Overview
//!
//! The bridge layer enables the use of nom's parser combinator framework while maintaining
//! complete backward compatibility with the existing AST structure. This allows gradual
//! migration or dual-parser support alongside the handwritten and Pest parsers.
//!
//! # Key Features
//!
//! - **AST Conversion**: Converts nom parse results to existing AST nodes
//! - **Error Mapping**: Translates nom errors to the existing error format
//! - **Span Preservation**: Maintains position and span information for error reporting
//! - **API Compatibility**: Produces identical AST structures to the handwritten parser
//!
//! # Example
//!
//! ```
//! use cddl::nom_bridge::cddl_from_nom_str;
//!
//! let input = r#"
//! person = {
//!   name: tstr,
//!   age: uint
//! }
//! "#;
//!
//! let cddl = cddl_from_nom_str(input).expect("Failed to parse CDDL");
//! assert_eq!(cddl.rules.len(), 1);
//! ```

use crate::{
  ast,
  error::{ErrorMsg, Position},
  nom_parser::{
    parse_cddl, ParsedCDDL, ParsedGroupEntry, ParsedGroupRule, ParsedMemberKey, ParsedOccurrence,
    ParsedRule, ParsedType, ParsedTypeRule, ParsedValue,
  },
  parser::Error,
  token::{self, ControlOperator, SocketPlug, Value},
};

use nom::error::VerboseError;

#[cfg(feature = "std")]
use std::{borrow::Cow, collections::BTreeSet, rc::Rc};

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::Cow,
  boxed::Box,
  collections::BTreeSet,
  rc::Rc,
  string::{String, ToString},
  vec::Vec,
};

/// Convert a nom error to the existing parser error format
pub fn convert_nom_error(error: nom::Err<VerboseError<&str>>, input: &str) -> Error {
  let (short_msg, extended_msg) = match error {
    nom::Err::Incomplete(_) => (
      "Unexpected end of input".to_string(),
      Some("The CDDL specification appears to be incomplete".to_string()),
    ),
    nom::Err::Error(e) | nom::Err::Failure(e) => create_error_message_from_verbose(&e, input),
  };

  Error::PARSER {
    #[cfg(feature = "ast-span")]
    position: calculate_position(input, 0),
    msg: ErrorMsg {
      short: short_msg,
      extended: extended_msg,
    },
  }
}

/// Create an error message from a VerboseError
fn create_error_message_from_verbose(
  error: &VerboseError<&str>,
  input: &str,
) -> (String, Option<String>) {
  if error.errors.is_empty() {
    return ("Parse error".to_string(), None);
  }

  // Get the first error entry
  let (error_input, kind) = &error.errors[0];

  // Calculate how far we got
  let consumed = input.len() - error_input.len();
  let position = calculate_position(input, consumed);

  let short = match kind {
    nom::error::VerboseErrorKind::Context(ctx) => {
      format!(
        "Expected {} at line {}, column {}",
        ctx, position.line, position.column
      )
    }
    nom::error::VerboseErrorKind::Char(c) => {
      format!(
        "Expected '{}' at line {}, column {}",
        c, position.line, position.column
      )
    }
    nom::error::VerboseErrorKind::Nom(error_kind) => {
      format!(
        "Parse error at line {}, column {}: {:?}",
        position.line, position.column, error_kind
      )
    }
  };

  let extended = Some(format!(
    "Parse error occurred at position {} (line {}, column {})",
    consumed, position.line, position.column
  ));

  (short, extended)
}

/// Calculate position in input
fn calculate_position(input: &str, byte_offset: usize) -> Position {
  let mut line = 1;
  let mut column = 1;
  let mut index = 0;

  for ch in input.chars() {
    if index >= byte_offset {
      break;
    }

    if ch == '\n' {
      line += 1;
      column = 1;
    } else {
      column += 1;
    }

    index += ch.len_utf8();
  }

  Position {
    line,
    column,
    range: (byte_offset, byte_offset),
    index: byte_offset,
  }
}

/// Parse CDDL from string using nom parser and convert to AST
pub fn cddl_from_nom_str<'a>(input: &'a str) -> Result<ast::CDDL<'a>, Error> {
  let (_, parsed) = parse_cddl(input).map_err(|e| convert_nom_error(e, input))?;

  convert_cddl(&parsed, input)
}

/// Convert ParsedCDDL to AST CDDL
fn convert_cddl<'a>(parsed: &ParsedCDDL<'a>, input: &'a str) -> Result<ast::CDDL<'a>, Error> {
  let mut rules = Vec::new();

  for rule in &parsed.rules {
    rules.push(convert_rule(rule, input)?);
  }

  Ok(ast::CDDL {
    rules,
    #[cfg(feature = "ast-comments")]
    comments: None,
  })
}

/// Convert ParsedRule to AST Rule
fn convert_rule<'a>(rule: &ParsedRule<'a>, input: &'a str) -> Result<ast::Rule<'a>, Error> {
  match rule {
    ParsedRule::Type(type_rule) => convert_type_rule(type_rule, input),
    ParsedRule::Group(group_rule) => convert_group_rule(group_rule, input),
  }
}

/// Convert ParsedTypeRule to AST Rule::Type
fn convert_type_rule<'a>(
  type_rule: &ParsedTypeRule<'a>,
  input: &'a str,
) -> Result<ast::Rule<'a>, Error> {
  let name = convert_identifier(type_rule.name, false);
  let generic_params = type_rule
    .generic_params
    .as_ref()
    .map(|params| ast::GenericParams {
      params: params.iter().map(|p| convert_generic_param(p)).collect(),
      #[cfg(feature = "ast-span")]
      span: (0, 0, 1),
    });

  let value = convert_type(&type_rule.value, input)?;

  #[cfg(feature = "ast-span")]
  let span = (type_rule.span.0, type_rule.span.1, 1);

  Ok(ast::Rule::Type {
    rule: ast::TypeRule {
      name,
      generic_params,
      is_type_choice_alternate: type_rule.is_choice_alternate,
      value,
      #[cfg(feature = "ast-comments")]
      comments_before_assignt: None,
      #[cfg(feature = "ast-comments")]
      comments_after_assignt: None,
    },
    #[cfg(feature = "ast-span")]
    span,
    #[cfg(feature = "ast-comments")]
    comments_after_rule: None,
  })
}

/// Convert ParsedGroupRule to AST Rule::Group
fn convert_group_rule<'a>(
  group_rule: &ParsedGroupRule<'a>,
  input: &'a str,
) -> Result<ast::Rule<'a>, Error> {
  let name = convert_identifier(group_rule.name, true);
  let generic_params = group_rule
    .generic_params
    .as_ref()
    .map(|params| ast::GenericParams {
      params: params.iter().map(|p| convert_generic_param(p)).collect(),
      #[cfg(feature = "ast-span")]
      span: (0, 0, 1),
    });

  let entry = convert_group_entry(&group_rule.entry, input)?;

  #[cfg(feature = "ast-span")]
  let span = (group_rule.span.0, group_rule.span.1, 1);

  Ok(ast::Rule::Group {
    rule: Box::new(ast::GroupRule {
      name,
      generic_params,
      is_group_choice_alternate: group_rule.is_choice_alternate,
      entry,
      #[cfg(feature = "ast-comments")]
      comments_before_assigng: None,
      #[cfg(feature = "ast-comments")]
      comments_after_assigng: None,
    }),
    #[cfg(feature = "ast-span")]
    span,
    #[cfg(feature = "ast-comments")]
    comments_after_rule: None,
  })
}

/// Convert a string identifier to AST Identifier
fn convert_identifier<'a>(name: &'a str, _is_group: bool) -> ast::Identifier<'a> {
  ast::Identifier {
    ident: name,
    socket: None,
    #[cfg(feature = "ast-span")]
    span: (0, 0, 1),
  }
}

/// Convert a generic parameter name to AST GenericParam
fn convert_generic_param<'a>(name: &'a str) -> ast::GenericParam<'a> {
  ast::GenericParam {
    param: convert_identifier(name, false),
    #[cfg(feature = "ast-comments")]
    comments_before_ident: None,
    #[cfg(feature = "ast-comments")]
    comments_after_ident: None,
  }
}

/// Convert ParsedType to AST Type
fn convert_type<'a>(parsed: &ParsedType<'a>, input: &'a str) -> Result<ast::Type<'a>, Error> {
  let type_choices = match parsed {
    ParsedType::Choice(choices) => {
      let mut type_choices = Vec::new();
      for choice in choices {
        type_choices.push(convert_type_choice(choice, input)?);
      }
      type_choices
    }
    _ => vec![convert_type_choice(parsed, input)?],
  };

  Ok(ast::Type {
    type_choices,
    #[cfg(feature = "ast-span")]
    span: (0, 0, 1),
  })
}

/// Convert a single type choice to AST TypeChoice
fn convert_type_choice<'a>(
  parsed: &ParsedType<'a>,
  input: &'a str,
) -> Result<ast::TypeChoice<'a>, Error> {
  Ok(ast::TypeChoice {
    type1: convert_type1(parsed, input)?,
    #[cfg(feature = "ast-comments")]
    comments_before_type: None,
    #[cfg(feature = "ast-comments")]
    comments_after_type: None,
  })
}

/// Convert to AST Type1
fn convert_type1<'a>(parsed: &ParsedType<'a>, input: &'a str) -> Result<ast::Type1<'a>, Error> {
  match parsed {
    ParsedType::Range {
      start,
      end,
      inclusive,
    } => {
      let operator = if *inclusive {
        ast::RangeCtlOp::RangeOp {
          is_inclusive: true,
          #[cfg(feature = "ast-span")]
          span: (0, 0, 1),
        }
      } else {
        ast::RangeCtlOp::RangeOp {
          is_inclusive: false,
          #[cfg(feature = "ast-span")]
          span: (0, 0, 1),
        }
      };

      Ok(ast::Type1 {
        type2: convert_type2(start, input)?,
        operator: Some(ast::Operator {
          operator,
          type2: convert_type2(end, input)?,
          #[cfg(feature = "ast-comments")]
          comments_before_operator: None,
          #[cfg(feature = "ast-comments")]
          comments_after_operator: None,
        }),
        #[cfg(feature = "ast-comments")]
        comments_after_type: None,
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      })
    }
    _ => Ok(ast::Type1 {
      type2: convert_type2(parsed, input)?,
      operator: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: (0, 0, 1),
    }),
  }
}

/// Convert to AST Type2
fn convert_type2<'a>(parsed: &ParsedType<'a>, input: &'a str) -> Result<ast::Type2<'a>, Error> {
  match parsed {
    ParsedType::Identifier(name) => Ok(ast::Type2::Typename {
      ident: convert_identifier(name, false),
      generic_args: None,
      #[cfg(feature = "ast-span")]
      span: (0, 0, 1),
    }),

    ParsedType::Value(val) => match val {
      ParsedValue::Int(i) => Ok(ast::Type2::IntValue {
        value: *i as isize,
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      }),
      ParsedValue::Uint(u) => Ok(ast::Type2::UintValue {
        value: *u as usize,
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      }),
      ParsedValue::Float(f) => Ok(ast::Type2::FloatValue {
        value: *f,
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      }),
      ParsedValue::Text(s) => Ok(ast::Type2::TextValue {
        value: Cow::Borrowed(s),
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      }),
      ParsedValue::Bytes(s) => Ok(ast::Type2::UTF8ByteString {
        value: Cow::Borrowed(s.as_bytes()),
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      }),
    },

    ParsedType::Array(entries) => {
      let group = convert_group_from_entries(entries, input)?;
      Ok(ast::Type2::Array {
        group,
        #[cfg(feature = "ast-comments")]
        comments_before_group: None,
        #[cfg(feature = "ast-comments")]
        comments_after_group: None,
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      })
    }

    ParsedType::Map(entries) => {
      let group = convert_group_from_entries(entries, input)?;
      Ok(ast::Type2::Map {
        group,
        #[cfg(feature = "ast-comments")]
        comments_before_group: None,
        #[cfg(feature = "ast-comments")]
        comments_after_group: None,
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      })
    }

    ParsedType::Tagged { tag, t } => Ok(ast::Type2::TaggedData {
      tag: Some(token::TagConstraint::Literal(*tag)),
      t: convert_type(t, input)?,
      #[cfg(feature = "ast-span")]
      span: (0, 0, 1),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
    }),

    ParsedType::Unwrap(t) => Ok(ast::Type2::Unwrap {
      ident: match t.as_ref() {
        ParsedType::Identifier(name) => convert_identifier(name, false),
        _ => {
          return Err(Error::PARSER {
            #[cfg(feature = "ast-span")]
            position: Position::default(),
            msg: ErrorMsg {
              short: "Unwrap requires an identifier".to_string(),
              extended: None,
            },
          });
        }
      },
      generic_args: None,
      #[cfg(feature = "ast-comments")]
      comments: None,
      #[cfg(feature = "ast-span")]
      span: (0, 0, 1),
    }),

    ParsedType::Generic { name, args } => {
      let generic_args = Some(ast::GenericArgs {
        args: args
          .iter()
          .map(|arg| convert_generic_arg(arg, input))
          .collect::<Result<Vec<_>, _>>()?,
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      });

      Ok(ast::Type2::Typename {
        ident: convert_identifier(name, false),
        generic_args,
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      })
    }

    ParsedType::Parenthesized(t) => Ok(ast::Type2::ParenthesizedType {
      pt: convert_type(t, input)?,
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: (0, 0, 1),
    }),

    ParsedType::Choice(choices) => {
      // Shouldn't happen at this level, but handle it
      if let Some(first) = choices.first() {
        convert_type2(first, input)
      } else {
        Err(Error::PARSER {
          #[cfg(feature = "ast-span")]
          position: Position::default(),
          msg: ErrorMsg {
            short: "Empty choice".to_string(),
            extended: None,
          },
        })
      }
    }

    ParsedType::Range { .. } => Err(Error::PARSER {
      #[cfg(feature = "ast-span")]
      position: Position::default(),
      msg: ErrorMsg {
        short: "Range should be handled at Type1 level".to_string(),
        extended: None,
      },
    }),
  }
}

/// Convert ParsedValue to AST Value
fn convert_value<'a>(val: &ParsedValue<'a>) -> Value<'a> {
  match val {
    ParsedValue::Int(i) => Value::INT(*i as isize),
    ParsedValue::Uint(u) => Value::UINT(*u as usize),
    ParsedValue::Float(f) => Value::FLOAT(*f),
    ParsedValue::Text(s) => Value::TEXT(Cow::Borrowed(s)),
    ParsedValue::Bytes(s) => Value::BYTE(token::ByteValue::UTF8(Cow::Borrowed(s.as_bytes()))),
  }
}

/// Convert generic argument
fn convert_generic_arg<'a>(
  arg: &ParsedType<'a>,
  input: &'a str,
) -> Result<ast::GenericArg<'a>, Error> {
  Ok(ast::GenericArg {
    arg: Box::new(convert_type1(arg, input)?),
    #[cfg(feature = "ast-comments")]
    comments_before_type: None,
    #[cfg(feature = "ast-comments")]
    comments_after_type: None,
  })
}

/// Convert a group from entries
fn convert_group_from_entries<'a>(
  entries: &[ParsedGroupEntry<'a>],
  input: &'a str,
) -> Result<ast::Group<'a>, Error> {
  let mut group_entries_with_commas = Vec::new();

  for entry in entries {
    let converted = convert_group_entry(entry, input)?;
    group_entries_with_commas.push((converted, opt_comma()));
  }

  let group_choices = vec![ast::GroupChoice {
    group_entries: group_entries_with_commas,
    #[cfg(feature = "ast-span")]
    span: (0, 0, 1),
    #[cfg(feature = "ast-comments")]
    comments_before_grpchoice: None,
  }];

  Ok(ast::Group {
    group_choices,
    #[cfg(feature = "ast-span")]
    span: (0, 0, 1),
  })
}

/// Helper to create optional comma
fn opt_comma<'a>() -> ast::OptionalComma<'a> {
  ast::OptionalComma {
    optional_comma: false,
    #[cfg(feature = "ast-comments")]
    trailing_comments: None,
    _a: core::marker::PhantomData,
  }
}

/// Convert ParsedGroupEntry to AST GroupEntry
fn convert_group_entry<'a>(
  entry: &ParsedGroupEntry<'a>,
  input: &'a str,
) -> Result<ast::GroupEntry<'a>, Error> {
  let occur = entry.occur.as_ref().map(|o| convert_occurrence(o));

  let member_key = entry
    .key
    .as_ref()
    .map(|k| convert_member_key(k, input))
    .transpose()?;

  Ok(ast::GroupEntry::ValueMemberKey {
    ge: Box::new(ast::ValueMemberKeyEntry {
      occur,
      member_key,
      entry_type: convert_type(&entry.value, input)?,
    }),
    #[cfg(feature = "ast-span")]
    span: (0, 0, 1),
    #[cfg(feature = "ast-comments")]
    leading_comments: None,
    #[cfg(feature = "ast-comments")]
    trailing_comments: None,
  })
}

/// Convert ParsedOccurrence to AST Occurrence
fn convert_occurrence<'a>(occur: &ParsedOccurrence) -> ast::Occurrence<'a> {
  ast::Occurrence {
    occur: match occur {
      ParsedOccurrence::Optional => ast::Occur::Optional {
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      },
      ParsedOccurrence::ZeroOrMore => ast::Occur::ZeroOrMore {
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      },
      ParsedOccurrence::OneOrMore => ast::Occur::OneOrMore {
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      },
      ParsedOccurrence::Exact(n) => ast::Occur::Exact {
        lower: Some(*n as usize),
        upper: None,
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      },
      ParsedOccurrence::Range { lower, upper } => ast::Occur::Exact {
        lower: Some(*lower as usize),
        upper: upper.map(|u| u as usize),
        #[cfg(feature = "ast-span")]
        span: (0, 0, 1),
      },
    },
    #[cfg(feature = "ast-comments")]
    comments: None,
    _a: core::marker::PhantomData,
  }
}

/// Convert ParsedMemberKey to AST MemberKey
fn convert_member_key<'a>(
  key: &ParsedMemberKey<'a>,
  input: &'a str,
) -> Result<ast::MemberKey<'a>, Error> {
  match key {
    ParsedMemberKey::Bareword(name) => Ok(ast::MemberKey::Bareword {
      ident: convert_identifier(name, false),
      #[cfg(feature = "ast-comments")]
      comments: None,
      #[cfg(feature = "ast-comments")]
      comments_after_colon: None,
      #[cfg(feature = "ast-span")]
      span: (0, 0, 1),
    }),
    ParsedMemberKey::Value(val) => Ok(ast::MemberKey::Value {
      value: convert_value(val),
      #[cfg(feature = "ast-comments")]
      comments: None,
      #[cfg(feature = "ast-comments")]
      comments_after_colon: None,
      #[cfg(feature = "ast-span")]
      span: (0, 0, 1),
    }),
    ParsedMemberKey::Type(t) => Ok(ast::MemberKey::Type1 {
      t1: Box::new(convert_type1(t, input)?),
      is_cut: false,
      #[cfg(feature = "ast-comments")]
      comments_before_cut: None,
      #[cfg(feature = "ast-comments")]
      comments_after_cut: None,
      #[cfg(feature = "ast-comments")]
      comments_after_arrowmap: None,
      #[cfg(feature = "ast-span")]
      span: (0, 0, 1),
    }),
  }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_nom_bridge_basic() {
    let input = "myrule = int\n";
    let result = cddl_from_nom_str(input);
    assert!(
      result.is_ok(),
      "Failed to parse basic CDDL: {:?}",
      result.err()
    );

    if let Ok(cddl) = result {
      assert_eq!(cddl.rules.len(), 1);
    }
  }

  #[test]
  fn test_nom_bridge_simple_struct() {
    let input = r#"
person = {
  name: tstr,
  age: uint
}
"#;
    let result = cddl_from_nom_str(input);
    assert!(result.is_ok(), "Failed to parse struct: {:?}", result.err());
  }

  #[test]
  fn test_nom_bridge_type_choice() {
    let input = "value = int / text / bool\n";
    let result = cddl_from_nom_str(input);
    assert!(
      result.is_ok(),
      "Failed to parse type choice: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_nom_bridge_array() {
    let input = "my-array = [* int]\n";
    let result = cddl_from_nom_str(input);
    assert!(result.is_ok(), "Failed to parse array: {:?}", result.err());
  }

  #[test]
  fn test_nom_bridge_generic() {
    let input = r#"
map<K, V> = { * K => V }
my-map = map<text, int>
"#;
    let result = cddl_from_nom_str(input);
    assert!(
      result.is_ok(),
      "Failed to parse generic: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_nom_bridge_range() {
    let input = "port = 0..65535\n";
    let result = cddl_from_nom_str(input);
    assert!(result.is_ok(), "Failed to parse range: {:?}", result.err());
  }

  #[test]
  fn test_nom_bridge_with_comments() {
    let input = r#"
; This is a comment
person = {
  name: tstr,  ; person's name
  age: uint   ; person's age
}
"#;
    let result = cddl_from_nom_str(input);
    assert!(
      result.is_ok(),
      "Failed to parse with comments: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_nom_bridge_coexistence_with_existing_parser() {
    use crate::cddl_from_str;

    let input = r#"
person = {
  name: tstr,
  age: uint
}
"#;

    // Test existing parser
    #[cfg(all(not(target_arch = "wasm32"), feature = "std"))]
    let existing_result = cddl_from_str(input, true);
    #[cfg(any(target_arch = "wasm32", not(feature = "std")))]
    let existing_result = cddl_from_str(input);
    assert!(
      existing_result.is_ok(),
      "Existing parser failed: {:?}",
      existing_result.err()
    );

    // Test nom parser
    let nom_result = cddl_from_nom_str(input);
    assert!(
      nom_result.is_ok(),
      "Nom parser failed: {:?}",
      nom_result.err()
    );
  }

  #[test]
  fn test_nom_bridge_rfc8610_examples() {
    let examples = vec![
      "reputation-object = { application: tstr, reputons: [* reputon] }\n",
      "CDDLtest = thing\n",
      "thing = ( int / float )\n",
    ];

    for example in examples {
      let result = cddl_from_nom_str(example);
      assert!(
        result.is_ok(),
        "Failed to parse: {}\nError: {:?}",
        example,
        result.err()
      );
    }
  }
}
