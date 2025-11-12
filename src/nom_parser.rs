//! Nom-based parser for CDDL
//!
//! This module provides an alternative parser implementation using the nom parser combinator framework.
//! It parses CDDL grammar as defined in RFC 8610 using nom combinators.
//!
//! # Overview
//!
//! This parser uses the nom parser combinator library to parse CDDL specifications.
//! It provides an alternative parsing approach that can coexist with both the existing
//! handwritten parser and the Pest-based parser.
//!
//! # Example
//!
//! ```
//! use cddl::nom_parser::parse_cddl;
//!
//! let input = r#"
//! person = {
//!   name: tstr,
//!   age: uint
//! }
//! "#;
//!
//! let result = parse_cddl(input);
//! assert!(result.is_ok());
//! ```

use nom::{
  branch::alt,
  bytes::complete::{tag, take_until, take_while, take_while1},
  character::complete::{
    alpha1, alphanumeric1, char, digit1, hex_digit1, line_ending, multispace0, multispace1,
    not_line_ending, one_of, space0, space1,
  },
  combinator::{map, not, opt, peek, recognize, value, verify},
  error::{context, ErrorKind, ParseError, VerboseError},
  multi::{many0, many1, separated_list0, separated_list1},
  sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
  IResult,
};

#[cfg(feature = "std")]
use std::borrow::Cow;

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::Cow,
  string::{String, ToString},
  vec::Vec,
};

/// Type alias for nom parsing results with verbose errors
pub type NomResult<'a, T> = IResult<&'a str, T, VerboseError<&'a str>>;

/// Represents a parsed CDDL specification
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedCDDL<'a> {
  pub rules: Vec<ParsedRule<'a>>,
}

/// Represents a parsed CDDL rule
#[derive(Debug, Clone, PartialEq)]
pub enum ParsedRule<'a> {
  Type(ParsedTypeRule<'a>),
  Group(ParsedGroupRule<'a>),
}

/// Represents a parsed type rule
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedTypeRule<'a> {
  pub name: &'a str,
  pub generic_params: Option<Vec<&'a str>>,
  pub is_choice_alternate: bool,
  pub value: ParsedType<'a>,
  pub span: (usize, usize),
}

/// Represents a parsed group rule
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedGroupRule<'a> {
  pub name: &'a str,
  pub generic_params: Option<Vec<&'a str>>,
  pub is_choice_alternate: bool,
  pub entry: ParsedGroupEntry<'a>,
  pub span: (usize, usize),
}

/// Represents a parsed type expression
#[derive(Debug, Clone, PartialEq)]
pub enum ParsedType<'a> {
  Identifier(&'a str),
  Value(ParsedValue<'a>),
  Array(Vec<ParsedGroupEntry<'a>>),
  Map(Vec<ParsedGroupEntry<'a>>),
  InlineGroup(Vec<ParsedGroupEntry<'a>>),
  GroupChoice(Vec<Vec<ParsedGroupEntry<'a>>>),  // Group choice: ( group // group // ... )
  ChoiceFromGroup {
    name: &'a str,
    generic_args: Option<Vec<ParsedType<'a>>>,
  },
  Choice(Vec<ParsedType<'a>>),
  Range {
    start: Box<ParsedType<'a>>,
    end: Box<ParsedType<'a>>,
    inclusive: bool,
  },
  ControlOp {
    target: Box<ParsedType<'a>>,
    operator: &'a str,
    controller: Box<ParsedType<'a>>,
  },
  Tagged {
    tag: u64,
    t: Box<ParsedType<'a>>,
  },
  Unwrap(Box<ParsedType<'a>>),
  Generic {
    name: &'a str,
    args: Vec<ParsedType<'a>>,
  },
  Parenthesized(Box<ParsedType<'a>>),
}

/// Represents a parsed value
#[derive(Debug, Clone, PartialEq)]
pub enum ParsedValue<'a> {
  Int(i128),
  Uint(u64),
  Float(f64),
  Text(&'a str),
  Bytes(&'a str),
}

/// Represents a parsed group entry
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedGroupEntry<'a> {
  pub occur: Option<ParsedOccurrence>,
  pub key: Option<ParsedMemberKey<'a>>,
  pub is_arrow_map: bool, // true for =>, false for :
  pub value: ParsedType<'a>,
}

/// Represents a parsed member key
#[derive(Debug, Clone, PartialEq)]
pub enum ParsedMemberKey<'a> {
  Type(ParsedType<'a>),
  Bareword(&'a str),
  Value(ParsedValue<'a>),
}

/// Represents a parsed occurrence indicator
#[derive(Debug, Clone, PartialEq)]
pub enum ParsedOccurrence {
  Optional,
  ZeroOrMore,
  OneOrMore,
  Exact(u64),
  Range { lower: u64, upper: Option<u64> },
}

// =============================================================================
// Whitespace and Comment Handling
// =============================================================================

/// Parse a CDDL comment
fn comment(input: &str) -> NomResult<&str> {
  context("comment", preceded(char(';'), not_line_ending))(input)
}

/// Parse whitespace including comments
fn ws(input: &str) -> NomResult<&str> {
  recognize(many0(alt((multispace1, comment))))(input)
}

// =============================================================================
// Identifiers and Names
// =============================================================================

/// Parse a bareword identifier
fn bareword(input: &str) -> NomResult<&str> {
  context(
    "bareword",
    recognize(pair(
      alt((alpha1, tag("_"), tag("@"), tag("$"))),
      many0(alt((
        alphanumeric1,
        tag("_"),
        tag("-"),
        tag("@"),
        tag("$"),
        tag("."),
      ))),
    )),
  )(input)
}

/// Parse a typename
fn typename(input: &str) -> NomResult<&str> {
  context("typename", bareword)(input)
}

/// Parse a groupname
fn groupname(input: &str) -> NomResult<&str> {
  context("groupname", bareword)(input)
}

// =============================================================================
// Generic Parameters
// =============================================================================

/// Parse generic parameters like <K, V>
fn generic_params(input: &str) -> NomResult<Vec<&str>> {
  context(
    "generic_params",
    delimited(
      char('<'),
      separated_list1(delimited(ws, char(','), ws), delimited(ws, bareword, ws)),
      preceded(ws, char('>')),
    ),
  )(input)
}

/// Parse generic arguments like <text, int>
fn generic_args(input: &str) -> NomResult<Vec<ParsedType>> {
  context(
    "generic_args",
    delimited(
      char('<'),
      separated_list1(delimited(ws, char(','), ws), delimited(ws, parse_type, ws)),
      preceded(ws, char('>')),
    ),
  )(input)
}

// =============================================================================
// Values
// =============================================================================

/// Parse an unsigned integer
fn uint_value(input: &str) -> NomResult<u64> {
  context("uint", map(digit1, |s: &str| s.parse().unwrap()))(input)
}

/// Parse an integer (can be negative)
fn int_value(input: &str) -> NomResult<i128> {
  context(
    "int",
    map(recognize(pair(opt(char('-')), digit1)), |s: &str| {
      s.parse().unwrap()
    }),
  )(input)
}

/// Parse a floating-point number (must have decimal point or exponent)
fn float_value(input: &str) -> NomResult<f64> {
  context(
    "float",
    map(
      recognize(alt((
        // Number with decimal point: 1.0, 1.5, -2.3
        recognize(tuple((
          opt(char('-')),
          digit1,
          char('.'),
          digit1,
          opt(recognize(tuple((one_of("eE"), opt(one_of("+-")), digit1)))),
        ))),
        // Number with exponent but no decimal: 1e5, 2E-3
        recognize(tuple((
          opt(char('-')),
          digit1,
          one_of("eE"),
          opt(one_of("+-")),
          digit1,
        ))),
      ))),
      |s: &str| s.parse().unwrap(),
    ),
  )(input)
}

/// Parse a text string value
fn text_value(input: &str) -> NomResult<&str> {
  context(
    "text_value",
    alt((
      // Double-quoted text: "..."
      delimited(char('"'), take_while(|c| c != '"'), char('"')),
      // Single-quoted text: '...' (can be multi-line)
      delimited(char('\''), take_while(|c| c != '\''), char('\'')),
    )),
  )(input)
}

/// Parse a byte string value
fn bytes_value(input: &str) -> NomResult<&str> {
  context(
    "bytes_value",
    alt((
      // h'...' format
      delimited(tag("h'"), take_while(|c| c != '\''), char('\'')),
      // b64'...' format
      delimited(tag("b64'"), take_while(|c| c != '\''), char('\'')),
      // h"..." format
      delimited(tag("h\""), take_while(|c| c != '"'), char('"')),
    )),
  )(input)
}

/// Parse any value
fn value_parser(input: &str) -> NomResult<ParsedValue> {
  context(
    "value",
    alt((
      map(text_value, ParsedValue::Text),
      map(bytes_value, ParsedValue::Bytes),
      // Try int_value before float_value so integers don't get parsed as floats
      map(int_value, |i| {
        if i >= 0 && i <= u64::MAX as i128 {
          ParsedValue::Uint(i as u64)
        } else {
          ParsedValue::Int(i)
        }
      }),
      map(float_value, ParsedValue::Float),
    )),
  )(input)
}

// =============================================================================
// Type Expressions
// =============================================================================

/// Parse a tagged type like #6.32(tstr)
fn tagged_type(input: &str) -> NomResult<ParsedType> {
  let _start = input;
  let (input, _) = char('#')(input)?;
  let (input, _major_type) = uint_value(input)?; // major type (usually 6 for tags)
  let (input, tag_num) = opt(preceded(char('.'), uint_value))(input)?;
  let (input, _) = char('(')(input)?;
  let (input, _) = ws(input)?;
  let (input, t) = parse_type(input)?;
  let (input, _) = ws(input)?;
  let (input, _) = char(')')(input)?;

  // If there's a dot-separated number, use it as the tag, otherwise use the first number
  let actual_tag = tag_num.unwrap_or(_major_type);

  Ok((
    input,
    ParsedType::Tagged {
      tag: actual_tag,
      t: Box::new(t),
    },
  ))
}

/// Parse an unwrapped type like ~identifier
fn unwrap_type(input: &str) -> NomResult<ParsedType> {
  let (input, _) = char('~')(input)?;
  let (input, t) = parse_type2(input)?;
  Ok((input, ParsedType::Unwrap(Box::new(t))))
}

/// Parse a type with optional generic arguments
fn type_with_generics(input: &str) -> NomResult<ParsedType> {
  let (input, name) = bareword(input)?;
  let (input, args) = opt(generic_args)(input)?;

  match args {
    Some(args) => Ok((input, ParsedType::Generic { name, args })),
    None => Ok((input, ParsedType::Identifier(name))),
  }
}

/// Parse an array type like [* int]
fn array_type(input: &str) -> NomResult<ParsedType> {
  context(
    "array",
    map(
      delimited(char('['), delimited(ws, group_entries, ws), char(']')),
      ParsedType::Array,
    ),
  )(input)
}

/// Parse a map type like { key: value }
fn map_type(input: &str) -> NomResult<ParsedType> {
  context(
    "map",
    map(
      delimited(char('{'), delimited(ws, group_entries, ws), char('}')),
      ParsedType::Map,
    ),
  )(input)
}

/// Parse an inline group or parenthesized type
fn parenthesized_type(input: &str) -> NomResult<ParsedType> {
  context(
    "parenthesized",
    alt((
      // Try group choice first: ( group // group // ... )
      map(
        delimited(char('('), delimited(ws, group_choice, ws), char(')')),
        |choices| {
          if choices.len() == 1 {
            // Single group, not a choice
            ParsedType::InlineGroup(choices.into_iter().next().unwrap())
          } else {
            // Multiple groups - this is a group choice
            ParsedType::GroupChoice(choices)
          }
        },
      ),
      // Otherwise try parenthesized type: ( type )
      map(
        delimited(char('('), delimited(ws, parse_type, ws), char(')')),
        |t| ParsedType::Parenthesized(Box::new(t)),
      ),
    )),
  )(input)
}

/// Parse a choice from group (&groupname or &groupname<args>)
fn choice_from_group(input: &str) -> NomResult<ParsedType> {
  let (input, _) = char('&')(input)?;
  let (input, name) = bareword(input)?;
  let (input, generic_args) = opt(generic_args)(input)?;

  Ok((input, ParsedType::ChoiceFromGroup { name, generic_args }))
}

/// Parse a base type (type2 in CDDL grammar)
fn parse_type2(input: &str) -> NomResult<ParsedType> {
  context(
    "type2",
    alt((
      tagged_type,
      unwrap_type,
      choice_from_group,
      array_type,
      map_type,
      parenthesized_type,
      type_with_generics,
      map(value_parser, ParsedType::Value),
    )),
  )(input)
}

/// Parse a control operator name
fn control_operator(input: &str) -> NomResult<&str> {
  context(
    "control_operator",
    preceded(
      char('.'),
      alt((
        tag("size"),
        tag("bits"),
        tag("regexp"),
        tag("cbor"),
        tag("cborseq"),
        tag("within"),
        tag("and"),
        tag("lt"),
        tag("le"),
        tag("gt"),
        tag("ge"),
        tag("eq"),
        tag("ne"),
        tag("default"),
        tag("pcre"),
        tag("cat"),
        tag("det"),
        tag("plus"),
        tag("abnf"),
        tag("abnfb"),
        tag("feature"),
      )),
    ),
  )(input)
}

/// Parse a type with range or control operator
fn parse_type1(input: &str) -> NomResult<ParsedType> {
  let (input, first) = parse_type2(input)?;
  let (input, _) = ws(input)?;

  // Check for range operator
  if let Ok((after_range, op)) = alt::<_, _, VerboseError<&str>, _>((tag("..."), tag("..")))(input)
  {
    let inclusive = op == "..";
    let (input, _) = ws(after_range)?;
    let (input, second) = parse_type2(input)?;
    return Ok((
      input,
      ParsedType::Range {
        start: Box::new(first),
        end: Box::new(second),
        inclusive,
      },
    ));
  }

  // Check for control operator
  if let Ok((after_ctrl, ctrl)) = control_operator(input) {
    let (input, _) = ws(after_ctrl)?;
    let (input, second) = parse_type2(input)?;
    return Ok((
      input,
      ParsedType::ControlOp {
        target: Box::new(first),
        operator: ctrl,
        controller: Box::new(second),
      },
    ));
  }

  Ok((input, first))
}

/// Parse a type choice (type separated by /)
fn parse_type(input: &str) -> NomResult<ParsedType> {
  let (input, types) = separated_list1(delimited(ws, char('/'), ws), parse_type1)(input)?;

  if types.len() == 1 {
    Ok((input, types.into_iter().next().unwrap()))
  } else {
    Ok((input, ParsedType::Choice(types)))
  }
}

// =============================================================================
// Group Entries
// =============================================================================

/// Parse an occurrence indicator
fn occurrence(input: &str) -> NomResult<ParsedOccurrence> {
  context(
    "occurrence",
    alt((
      value(ParsedOccurrence::Optional, char('?')),
      value(ParsedOccurrence::ZeroOrMore, char('*')),
      value(ParsedOccurrence::OneOrMore, char('+')),
      map(
        separated_pair(uint_value, char('*'), opt(uint_value)),
        |(lower, upper)| ParsedOccurrence::Range { lower, upper },
      ),
      map(uint_value, ParsedOccurrence::Exact),
    )),
  )(input)
}

/// Parse a member key (bareword or value)
fn member_key(input: &str) -> NomResult<ParsedMemberKey> {
  context(
    "member_key",
    alt((
      map(bareword, ParsedMemberKey::Bareword),
      map(value_parser, ParsedMemberKey::Value),
    )),
  )(input)
}

/// Helper to check if a character could start a value literal
fn could_start_value(c: char) -> bool {
  matches!(c, '"' | '\'' | 'h' | '-' | '0'..='9')
}

/// Parse group choices separated by //
fn group_choice(input: &str) -> NomResult<Vec<Vec<ParsedGroupEntry>>> {
  separated_list1(
    delimited(ws, tag("//"), ws),
    delimited(ws, group_entries, ws),
  )(input)
}

/// Parse a group entry
fn group_entry(input: &str) -> NomResult<ParsedGroupEntry> {
  // Try to parse occurrence indicator
  // Occurrences like ?, *, + don't need following whitespace
  // Numeric occurrences need whitespace to distinguish from values
  let (input, occur) = if let Ok((after_occ, occ)) = occurrence(input) {
    match occ {
      // These are unambiguous single-character occurrences
      ParsedOccurrence::Optional | ParsedOccurrence::ZeroOrMore | ParsedOccurrence::OneOrMore => {
        // Consume optional whitespace after the occurrence
        let (after_ws, _) = ws(after_occ)?;
        (after_ws, Some(occ))
      }
      // Numeric occurrences (Exact or Range) need whitespace to distinguish from values
      ParsedOccurrence::Exact(_) | ParsedOccurrence::Range { .. } => {
        // Must be followed by whitespace
        if let Ok((after_ws, _)) = multispace1::<&str, VerboseError<&str>>(after_occ) {
          (after_ws, Some(occ))
        } else {
          // No whitespace, this is a value not an occurrence
          (input, None)
        }
      }
    }
  } else {
    (input, None)
  };

  // Try to parse as key:value or key=>value first
  // More carefully avoid consuming input if not a key:value pair
  let (input, key, is_arrow_map, value) = {
    // First try to parse bareword : or bareword =>
    if let Ok((after_key, bareword_key)) = bareword(input) {
      if let Ok((after_sep, (_, sep, _))) = tuple::<_, _, VerboseError<&str>, _>((
        ws,
        alt((tag(":"), tag("=>"))),
        ws
      ))(after_key) {
        // We have bareword : or bareword =>, parse the value
        let (input, v) = parse_type(after_sep)?;
        (input, Some(ParsedMemberKey::Bareword(bareword_key)), sep == "=>", v)
      } else {
        // Bareword but not followed by : or =>, this is a type not a member key
        let (input, v) = parse_type(input)?;
        (input, None, false, v)
      }
    } else {
      // Try value as member key only if input starts with something that could be a value
      // This prevents unnecessary parsing attempts that would fail
      let first_char = input.chars().next();
      let could_be_value = first_char.map_or(false, could_start_value);
      
      if could_be_value {
        if let Ok((after_key, value_key)) = value_parser(input) {
          // Try value followed by : or =>
          if let Ok((after_sep, (_, sep, _))) = tuple::<_, _, VerboseError<&str>, _>((
            ws,
            alt((tag(":"), tag("=>"))),
            ws
          ))(after_key) {
            // We have value : or value =>, parse the value
            let (input, v) = parse_type(after_sep)?;
            (input, Some(ParsedMemberKey::Value(value_key)), sep == "=>", v)
          } else {
            // Value but not followed by : or =>, parse as type
            let (input, v) = parse_type(input)?;
            (input, None, false, v)
          }
        } else {
          // value_parser failed, parse as type
          let (input, v) = parse_type(input)?;
          (input, None, false, v)
        }
      } else {
        // Not a member key, just parse the value
        let (input, v) = parse_type(input)?;
        (input, None, false, v)
      }
    }
  };

  Ok((
    input,
    ParsedGroupEntry {
      occur,
      key,
      is_arrow_map,
      value,
    },
  ))
}

/// Parse multiple group entries (allowing trailing comma)
fn group_entries(input: &str) -> NomResult<Vec<ParsedGroupEntry>> {
  let (input, entries) =
    separated_list0(delimited(ws, char(','), ws), delimited(ws, group_entry, ws))(input)?;

  // Consume optional trailing comma
  let (input, _) = opt(delimited(ws, char(','), ws))(input)?;

  Ok((input, entries))
}

// =============================================================================
// Rules
// =============================================================================

/// Parse a type rule assignment operator
fn type_assign(input: &str) -> NomResult<bool> {
  alt((value(true, tag("/=")), value(false, char('='))))(input)
}

/// Parse a group rule assignment operator
fn group_assign(input: &str) -> NomResult<bool> {
  alt((value(true, tag("//=")), value(false, char('='))))(input)
}

/// Parse a type rule
fn type_rule(input: &str) -> NomResult<ParsedRule> {
  let start_pos = input.as_ptr() as usize;
  let (input, name) = typename(input)?;
  let (input, _) = ws(input)?;
  let (input, gen_params) = opt(generic_params)(input)?;
  let (input, _) = ws(input)?;
  let (input, is_choice) = type_assign(input)?;
  let (input, _) = ws(input)?;
  let (input, value) = parse_type(input)?;
  let end_pos = input.as_ptr() as usize;

  Ok((
    input,
    ParsedRule::Type(ParsedTypeRule {
      name,
      generic_params: gen_params,
      is_choice_alternate: is_choice,
      value,
      span: (start_pos, end_pos),
    }),
  ))
}

/// Parse a group rule
fn group_rule(input: &str) -> NomResult<ParsedRule> {
  let start_pos = input.as_ptr() as usize;
  let (input, name) = groupname(input)?;
  let (input, _) = ws(input)?;
  let (input, gen_params) = opt(generic_params)(input)?;
  let (input, _) = ws(input)?;
  let (input, is_choice) = group_assign(input)?;
  let (input, _) = ws(input)?;
  let (input, entry) = group_entry(input)?;
  let end_pos = input.as_ptr() as usize;

  Ok((
    input,
    ParsedRule::Group(ParsedGroupRule {
      name,
      generic_params: gen_params,
      is_choice_alternate: is_choice,
      entry,
      span: (start_pos, end_pos),
    }),
  ))
}

/// Parse a CDDL rule (type or group)
fn rule(input: &str) -> NomResult<ParsedRule> {
  context("rule", alt((type_rule, group_rule)))(input)
}

/// Parse a complete CDDL specification
pub fn parse_cddl(input: &str) -> NomResult<ParsedCDDL> {
  let (input, _) = ws(input)?;
  let (input, rules) = many1(delimited(ws, rule, ws))(input)?;
  let (input, _) = ws(input)?;

  Ok((input, ParsedCDDL { rules }))
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_nom_parser_basic() {
    let input = "myrule = int\n";
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse basic CDDL: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_nom_parser_simple_rule() {
    let input = "person = { name: tstr, age: uint }\n";
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse struct rule: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_nom_parser_multiple_rules() {
    let input = r#"
person = {
  name: tstr,
  age: uint
}

address = {
  street: tstr,
  city: tstr
}
"#;
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse multiple rules: {:?}",
      result.err()
    );

    if let Ok((_, cddl)) = result {
      assert_eq!(cddl.rules.len(), 2);
    }
  }

  #[test]
  fn test_nom_parser_with_comments() {
    let input = r#"
; This is a comment
person = {
  name: tstr,  ; person's name
  age: uint   ; person's age
}
"#;
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse with comments: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_nom_parser_choice() {
    let input = "value = int / text / bool\n";
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse type choice: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_nom_parser_array() {
    let input = "my-array = [* int]\n";
    let result = parse_cddl(input);
    assert!(result.is_ok(), "Failed to parse array: {:?}", result.err());
  }

  #[test]
  fn test_nom_parser_generic() {
    let input = r#"
map<K, V> = { * K => V }
my-map = map<text, int>
"#;
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse generic: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_nom_parser_range() {
    let input = "port = 0..65535\n";
    let result = parse_cddl(input);
    assert!(result.is_ok(), "Failed to parse range: {:?}", result.err());
  }

  #[test]
  fn test_nom_parser_tag() {
    let input = "tagged-value = #6.32(tstr)\n";
    let result = parse_cddl(input);
    assert!(result.is_ok(), "Failed to parse tag: {:?}", result.err());
  }

  #[test]
  fn test_nom_parser_values() {
    let input = r#"
int-rule = 42
float-rule = 3.14
text-rule = "hello"
neg-rule = -100
"#;
    let result = parse_cddl(input);
    assert!(result.is_ok(), "Failed to parse values: {:?}", result.err());
  }

  #[test]
  fn test_nom_parser_occurrence() {
    let input = r#"
optional-field = { ? key: value }
zero-or-more = { * key: value }
one-or-more = { + key: value }
"#;
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse occurrence: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_nom_parser_parenthesized() {
    let input = "thing = ( int / float )\n";
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse parenthesized: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_nom_parser_nested_array() {
    let input = "array = [0, [* int]]";
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse nested array: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_occurrence_zero_or_more() {
    let input = "* int";
    let result = occurrence(input);
    assert!(
      result.is_ok(),
      "Failed to parse * occurrence: {:?}",
      result.err()
    );
    if let Ok((remaining, occ)) = result {
      assert_eq!(remaining, " int");
      assert_eq!(occ, ParsedOccurrence::ZeroOrMore);
    }
  }

  #[test]
  fn test_group_entry_with_occurrence() {
    let input = "* int";
    let result = group_entry(input);
    assert!(
      result.is_ok(),
      "Failed to parse group entry with occurrence: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_array_with_occurrence() {
    let input = "[* int]";
    let result = array_type(input);
    assert!(
      result.is_ok(),
      "Failed to parse array with occurrence: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_nested_array_direct() {
    let input = "[0, [* int]]";
    let result = array_type(input);
    assert!(
      result.is_ok(),
      "Failed to parse nested array directly: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_group_entries_multiple() {
    let input = "0, 1, 2";
    let result = group_entries(input);
    assert!(
      result.is_ok(),
      "Failed to parse multiple group entries: {:?}",
      result.err()
    );
    if let Ok((remaining, entries)) = result {
      assert_eq!(
        entries.len(),
        3,
        "Expected 3 entries, got {}",
        entries.len()
      );
      assert_eq!(remaining, "");
    }
  }

  #[test]
  fn test_group_entry_simple_value() {
    let input = "0";
    let result = group_entry(input);
    assert!(
      result.is_ok(),
      "Failed to parse simple value as group entry: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_optional_entry_in_map() {
    let input = r#"argument = {
      name: text,
      ? valid: text,
    }"#;
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse optional entry: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_simple_map() {
    let input = "{ name: text }";
    let result = map_type(input);
    assert!(
      result.is_ok(),
      "Failed to parse simple map: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_map_with_optional() {
    let input = "{ ? valid: text }";
    let result = map_type(input);
    assert!(
      result.is_ok(),
      "Failed to parse map with optional: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_multiline_map_simple() {
    let input = "{
      name: text,
    }";
    let result = map_type(input);
    assert!(
      result.is_ok(),
      "Failed to parse multiline map: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_control_operator_plus() {
    let input = "x = y .plus z";
    let result = parse_cddl(input);
    assert!(result.is_ok(), "Failed to parse .plus: {:?}", result.err());
  }

  #[test]
  fn test_control_op_in_group() {
    let input = r#"interval<BASE> = (
      "test" => BASE .plus a
    )"#;
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse control op in group: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_simple_inline_group() {
    let input = "x = ( a: int )";
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse simple inline group: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_inline_group_with_generic() {
    let input = "interval<BASE> = ( a: BASE )";
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse inline group with generic: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_inline_group_with_arrow_map() {
    let input = r#"x = ( "test" => y )"#;
    let result = parse_cddl(input);
    assert!(
      result.is_ok(),
      "Failed to parse inline group with =>: {:?}",
      result.err()
    );
  }
}
