//! Bridge layer between Pest parser and existing AST
//!
//! This module provides conversion functions to transform Pest parse trees into the existing
//! AST structure, ensuring API compatibility while leveraging Pest's parsing capabilities.
//!
//! # Overview
//!
//! The bridge layer enables the use of Pest's powerful parsing library while maintaining
//! complete backward compatibility with the existing AST structure. This allows gradual
//! migration or dual-parser support.
//!
//! # Key Features
//!
//! - **AST Conversion**: Converts Pest `Pair` and `Pairs` to existing AST nodes
//! - **Error Mapping**: Translates Pest errors to the existing error format
//! - **Span Preservation**: Maintains position and span information for error reporting
//! - **API Compatibility**: Produces identical AST structures to the handwritten parser
//!
//! # Example
//!
//! ```
//! use cddl::pest_bridge::cddl_from_pest_str;
//!
//! let input = r#"
//! person = {
//!   name: tstr,
//!   age: uint
//! }
//! "#;
//!
//! let cddl = cddl_from_pest_str(input).expect("Failed to parse CDDL");
//! assert_eq!(cddl.rules.len(), 1);
//! ```

use crate::{
  ast,
  error::ErrorMsg,
  lexer::Position,
  parser::Error,
  pest_parser::{CddlParser, Rule},
  token::{lookup_control_from_str, ControlOperator, SocketPlug, TagConstraint, Value},
};

use pest::{
  iterators::{Pair, Pairs},
  Parser as PestParser, Span as PestSpan,
};

#[cfg(not(feature = "std"))]
use alloc::borrow::Cow;
#[cfg(feature = "std")]
use std::borrow::Cow;

#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap as HashMap;
#[cfg(feature = "std")]
use std::collections::HashMap;

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, format, string::String, string::ToString, vec, vec::Vec};

/// Convert a Pest error to the existing parser error format with enhanced messages
#[allow(unused_variables)]
pub fn convert_pest_error(error: pest::error::Error<Rule>, input: &str) -> Error {
  let (line, column, byte_pos) = match error.line_col {
    pest::error::LineColLocation::Pos((line, col)) => (line, col, error.location.clone()),
    pest::error::LineColLocation::Span((line, col), _) => (line, col, error.location.clone()),
  };

  // Extract byte position from location
  let index = match byte_pos {
    pest::error::InputLocation::Pos(pos) => pos,
    pest::error::InputLocation::Span((start, _)) => start,
  };

  // Compute a meaningful range for highlighting.
  //
  // Pest reports errors at the position *after* the last successfully consumed
  // token. When that position is at end-of-line or end-of-input (i.e. there is
  // no forward token to highlight), scan backwards to find the preceding token
  // and use its span so the error marker lands on something visible.
  #[cfg(feature = "ast-span")]
  let range = compute_error_range(index, input);

  // Adjust line/column to the start of the range when we moved backwards
  #[cfg(feature = "ast-span")]
  let (adj_line, adj_column) = if range.0 < index {
    let mut l = 1usize;
    let mut c = 1usize;
    for (i, ch) in input.char_indices() {
      if i >= range.0 {
        break;
      }
      if ch == '\n' {
        l += 1;
        c = 1;
      } else {
        c += 1;
      }
    }
    (l, c)
  } else {
    (line, column)
  };

  // Create enhanced error message with user-friendly rule names
  let (short_msg, extended_msg) = create_enhanced_error_message(&error, input);

  Error::PARSER {
    #[cfg(feature = "ast-span")]
    position: Position {
      line: adj_line,
      column: adj_column,
      range,
      index: range.0,
    },
    msg: ErrorMsg {
      short: short_msg,
      extended: extended_msg,
    },
  }
}

/// Compute a byte range for an error at `index` in `input`.
///
/// If `index` points at a visible token (identifier, number, etc.), return
/// its span.  Otherwise scan backwards to the preceding token and return
/// that span instead.  This ensures the error marker covers something the
/// user can actually see.
fn compute_error_range(index: usize, input: &str) -> (usize, usize) {
  let bytes = input.as_bytes();

  // Try forward first: if there is a token starting at `index`, use it.
  if index < bytes.len() {
    let ch = bytes[index];
    if !ch.is_ascii_whitespace() && ch != b';' {
      let end = scan_token_end(bytes, index);
      if end > index {
        return (index, end);
      }
    }
  }

  // Nothing useful at `index` — scan backwards to the previous token.
  if index > 0 {
    let mut pos = index;
    // Skip whitespace / comments backwards
    while pos > 0 {
      pos -= 1;
      let ch = bytes[pos];
      if !ch.is_ascii_whitespace() && ch != b';' {
        // Found a non-ws char. Find the start of this token.
        let token_end = pos + 1;
        let token_start = scan_token_start(bytes, pos);
        return (token_start, token_end);
      }
    }
  }

  // Fallback: zero-width at index
  (index, index)
}

/// Scan forward from `start` to find the end of a token (identifier, number,
/// operator, etc.).
fn scan_token_end(bytes: &[u8], start: usize) -> usize {
  let mut pos = start;
  if pos >= bytes.len() {
    return pos;
  }
  let first = bytes[pos];
  if first.is_ascii_alphanumeric() || first == b'_' || first == b'$' || first == b'@' {
    // Identifier or number
    while pos < bytes.len() {
      let ch = bytes[pos];
      if ch.is_ascii_alphanumeric()
        || ch == b'_'
        || ch == b'-'
        || ch == b'.'
        || ch == b'$'
        || ch == b'@'
      {
        pos += 1;
      } else {
        break;
      }
    }
  } else {
    // Single character (operator, delimiter, etc.)
    pos += 1;
  }
  pos
}

/// Scan backwards from `pos` to find the start of the token containing `pos`.
fn scan_token_start(bytes: &[u8], pos: usize) -> usize {
  let ch = bytes[pos];
  if ch.is_ascii_alphanumeric()
    || ch == b'_'
    || ch == b'-'
    || ch == b'.'
    || ch == b'$'
    || ch == b'@'
  {
    let mut start = pos;
    while start > 0 {
      let prev = bytes[start - 1];
      if prev.is_ascii_alphanumeric()
        || prev == b'_'
        || prev == b'-'
        || prev == b'.'
        || prev == b'$'
        || prev == b'@'
      {
        start -= 1;
      } else {
        break;
      }
    }
    start
  } else {
    pos
  }
}

/// Create enhanced error messages using Pest's error information
fn create_enhanced_error_message(
  error: &pest::error::Error<Rule>,
  _input: &str,
) -> (String, Option<String>) {
  use pest::error::ErrorVariant;

  match &error.variant {
    ErrorVariant::ParsingError {
      positives,
      negatives,
    } => {
      // Map internal rule names to user-friendly descriptions
      let friendly_positives: Vec<String> = positives
        .iter()
        .filter_map(get_friendly_rule_name)
        .collect();

      let friendly_negatives: Vec<String> = negatives
        .iter()
        .filter_map(get_friendly_rule_name)
        .collect();

      let short = if !friendly_positives.is_empty() {
        if friendly_positives.len() == 1 {
          format!("expected {}", friendly_positives[0])
        } else {
          format!("expected one of: {}", friendly_positives.join(", "))
        }
      } else if !friendly_negatives.is_empty() {
        format!("unexpected {}", friendly_negatives.join(", "))
      } else {
        "syntax error".to_string()
      };

      // Create extended message with context
      let extended = if !friendly_positives.is_empty() || !friendly_negatives.is_empty() {
        Some(create_error_context(
          error,
          &friendly_positives,
          &friendly_negatives,
        ))
      } else {
        None
      };

      (short, extended)
    }
    ErrorVariant::CustomError { message } => (message.clone(), None),
  }
}

/// Map Pest rule names to user-friendly descriptions
fn get_friendly_rule_name(rule_type: &Rule) -> Option<String> {
  match rule_type {
    Rule::cddl => Some("CDDL specification".to_string()),
    Rule::rule => Some("rule definition".to_string()),
    Rule::typename => Some("type name".to_string()),
    Rule::groupname => Some("group name".to_string()),
    Rule::assign => Some("assignment operator '='".to_string()),
    Rule::assign_t => Some("type assignment ('=' or '/=')".to_string()),
    Rule::assign_g => Some("group assignment ('=' or '//=')".to_string()),
    Rule::assign_t_choice => Some("type choice assignment '/='".to_string()),
    Rule::assign_g_choice => Some("group choice assignment '//='".to_string()),
    Rule::generic_params => Some("generic parameters '<...>'".to_string()),
    Rule::generic_param => Some("generic parameter".to_string()),
    Rule::generic_args => Some("generic arguments '<...>'".to_string()),
    Rule::generic_arg => Some("generic argument".to_string()),
    Rule::type_expr => Some("type expression".to_string()),
    Rule::type_choice => Some("type choice".to_string()),
    Rule::type_choice_op => Some("type choice operator '/'".to_string()),
    Rule::type1 => Some("type".to_string()),
    Rule::type2 => Some("type value".to_string()),
    Rule::range_op => Some("range operator ('..' or '...')".to_string()),
    Rule::range_op_inclusive => Some("inclusive range '..'".to_string()),
    Rule::range_op_exclusive => Some("exclusive range '...'".to_string()),
    Rule::control_op => Some("control operator".to_string()),
    Rule::control_name => Some("control operator name".to_string()),
    Rule::controller => Some("control argument".to_string()),
    Rule::group => Some("group definition".to_string()),
    Rule::group_choice => Some("group choice".to_string()),
    Rule::group_entry => Some("group entry".to_string()),
    Rule::occur => Some("occurrence indicator".to_string()),
    Rule::occur_optional => Some("optional '?'".to_string()),
    Rule::occur_zero_or_more => Some("zero or more '*'".to_string()),
    Rule::occur_one_or_more => Some("one or more '+'".to_string()),
    Rule::occur_exact => Some("exact occurrence".to_string()),
    Rule::occur_range => Some("occurrence range".to_string()),
    Rule::member_key => Some("member key".to_string()),
    Rule::bareword => Some("bareword identifier".to_string()),
    Rule::value => Some("value".to_string()),
    Rule::number => Some("number".to_string()),
    Rule::int_value => Some("integer".to_string()),
    Rule::uint_value => Some("unsigned integer".to_string()),
    Rule::float_value => Some("floating-point number".to_string()),
    Rule::hexfloat => Some("hexadecimal float".to_string()),
    Rule::text_value => Some("text string".to_string()),
    Rule::bytes_value => Some("byte string".to_string()),
    Rule::bytes_b16 => Some("base16 byte string".to_string()),
    Rule::bytes_b64 => Some("base64 byte string".to_string()),
    Rule::bytes_h_quoted => Some("hex-quoted byte string".to_string()),
    Rule::tag_expr => Some("tag expression".to_string()),
    Rule::id => Some("identifier".to_string()),
    Rule::socket_type => Some("type socket '$'".to_string()),
    Rule::socket_group => Some("group socket '$$'".to_string()),
    // Skip internal/whitespace rules
    Rule::COMMENT | Rule::S | Rule::EOI => None,
    _ => Some(format!("{:?}", rule_type)),
  }
}

/// Create contextual error message with suggestions
fn create_error_context(
  error: &pest::error::Error<Rule>,
  positives: &[String],
  negatives: &[String],
) -> String {
  let mut context = String::new();

  // Add location context
  match &error.line_col {
    pest::error::LineColLocation::Pos((line, col)) => {
      context.push_str(&format!("At line {}, column {}: ", line, col));
    }
    pest::error::LineColLocation::Span((line1, col1), (line2, col2)) => {
      context.push_str(&format!(
        "From line {}, column {} to line {}, column {}: ",
        line1, col1, line2, col2
      ));
    }
  }

  // Add helpful suggestions based on what was expected
  if !positives.is_empty() {
    context.push_str("Expected ");
    context.push_str(&positives.join(" or "));
    context.push('.');

    // Add specific suggestions
    if positives.iter().any(|p| p.contains("assignment")) {
      context.push_str("\n\nHint: Every rule needs an assignment operator ('=' for new rules, '/=' for type alternatives, or '//=' for group alternatives).");
    } else if positives.iter().any(|p| p.contains("type")) {
      context
        .push_str("\n\nHint: Make sure your type expression is complete and properly formatted.");
    } else if positives.iter().any(|p| p.contains("group")) {
      context.push_str("\n\nHint: Group definitions should contain valid group entries.");
    }
  }

  if !negatives.is_empty() {
    if !context.is_empty() {
      context.push(' ');
    }
    context.push_str("Did not expect ");
    context.push_str(&negatives.join(" or "));
    context.push('.');
  }

  context
}

/// Convert Pest span to AST span
#[cfg(feature = "ast-span")]
fn pest_span_to_ast_span(span: &PestSpan, input: &str) -> ast::Span {
  let start = span.start();
  let end = span.end();

  // Calculate line number by counting newlines up to start position
  let line = input[..start].chars().filter(|&c| c == '\n').count() + 1;

  (start, end, line)
}

/// Convert Pest span to position
fn pest_span_to_position(span: &PestSpan, input: &str) -> Position {
  let start = span.start();
  let end = span.end();

  // Calculate line and column
  let mut line = 1;
  let mut column = 1;

  for ch in input[..start].chars() {
    if ch == '\n' {
      line += 1;
      column = 1;
    } else {
      column += 1;
    }
  }

  Position {
    line,
    column,
    range: (start, end),
    index: start,
  }
}

/// Standard prelude names from RFC 8610 §D
const STANDARD_PRELUDE: &[&str] = &[
  "any",
  "uint",
  "nint",
  "int",
  "bstr",
  "bytes",
  "tstr",
  "text",
  "tdate",
  "time",
  "number",
  "biguint",
  "bignint",
  "bigint",
  "integer",
  "unsigned",
  "decfrac",
  "bigfloat",
  "eb64url",
  "eb64legacy",
  "eb16",
  "encoded-cbor",
  "uri",
  "b64url",
  "b64legacy",
  "regexp",
  "mime-message",
  "cbor-any",
  "float16",
  "float32",
  "float64",
  "float16-32",
  "float32-64",
  "float",
  "false",
  "true",
  "bool",
  "nil",
  "null",
  "undefined",
];

/// Parse CDDL from string using Pest parser and convert to AST
pub fn cddl_from_pest_str<'a>(input: &'a str) -> Result<ast::CDDL<'a>, Error> {
  let pairs = CddlParser::parse(Rule::cddl, input).map_err(|e| convert_pest_error(e, input))?;

  convert_cddl(pairs, input)
}

/// Parse CDDL from string, converting to AST and checking for undefined references.
///
/// This is the same as `cddl_from_pest_str` but additionally verifies that all
/// referenced type/group names are either defined by a rule in the document,
/// part of the standard prelude (RFC 8610 §D), a generic parameter, or a
/// socket/plug reference.
#[cfg(feature = "std")]
pub fn cddl_from_pest_str_checked<'a>(input: &'a str) -> Result<ast::CDDL<'a>, Error> {
  let pairs = CddlParser::parse(Rule::cddl, input).map_err(|e| convert_pest_error(e, input))?;

  // Clone pairs so we can check for undefined references after AST conversion
  let pairs_for_ref_check = pairs.clone();

  let cddl = convert_cddl(pairs, input)?;

  // Check for undefined references
  if let Some((name, _pos)) = find_first_undefined_reference(pairs_for_ref_check, input) {
    return Err(Error::PARSER {
      #[cfg(feature = "ast-span")]
      position: _pos,
      msg: ErrorMsg {
        short: format!("missing definition for rule {}", name),
        extended: None,
      },
    });
  }

  Ok(cddl)
}

/// Walk a successful pest parse tree to find the first undefined reference.
///
/// Returns `Some((name, position))` if an undefined reference is found.
#[cfg(feature = "std")]
fn find_first_undefined_reference(
  pairs: Pairs<'_, Rule>,
  input: &str,
) -> Option<(String, Position)> {
  use std::collections::HashSet;

  let prelude: HashSet<&str> = STANDARD_PRELUDE.iter().copied().collect();

  // Phase 1: collect all defined rule names and per-rule generic parameters
  let mut defined: HashSet<String> = HashSet::new();
  let mut rule_generic_params: HashMap<usize, HashSet<String>> = HashMap::new();

  fn collect_definitions(
    pair: &Pair<'_, Rule>,
    defined: &mut HashSet<String>,
    rule_generic_params: &mut HashMap<usize, HashSet<String>>,
  ) {
    if pair.as_rule() == Rule::rule {
      let mut generic_params_for_rule: HashSet<String> = HashSet::new();
      for inner in pair.clone().into_inner() {
        match inner.as_rule() {
          Rule::typename | Rule::groupname => {
            for id_pair in inner.into_inner() {
              if id_pair.as_rule() == Rule::id {
                defined.insert(id_pair.as_str().to_string());
              }
            }
          }
          Rule::generic_params => {
            for gp in inner.into_inner() {
              if gp.as_rule() == Rule::generic_param {
                for id_pair in gp.into_inner() {
                  if id_pair.as_rule() == Rule::id {
                    generic_params_for_rule.insert(id_pair.as_str().to_string());
                  }
                }
              }
            }
          }
          _ => {}
        }
      }
      if !generic_params_for_rule.is_empty() {
        rule_generic_params.insert(pair.as_span().start(), generic_params_for_rule);
      }
    }
    for inner in pair.clone().into_inner() {
      collect_definitions(&inner, defined, rule_generic_params);
    }
  }

  let pairs_clone = pairs.clone();
  for pair in pairs_clone {
    collect_definitions(&pair, &mut defined, &mut rule_generic_params);
  }

  // Phase 2: find the first undefined reference
  struct RefFinder<'a> {
    prelude: &'a HashSet<&'a str>,
    defined: &'a HashSet<String>,
    input: &'a str,
    current_rule_generics: Option<&'a HashSet<String>>,
    result: Option<(String, Position)>,
  }

  impl<'a> RefFinder<'a> {
    fn walk(
      &mut self,
      pair: Pair<'_, Rule>,
      rule_generic_params: &'a HashMap<usize, HashSet<String>>,
    ) {
      if self.result.is_some() {
        return;
      }

      if pair.as_rule() == Rule::rule {
        let generics = rule_generic_params.get(&pair.as_span().start());
        let prev = self.current_rule_generics;
        self.current_rule_generics = generics;
        for inner in pair.into_inner() {
          self.walk(inner, rule_generic_params);
          if self.result.is_some() {
            return;
          }
        }
        self.current_rule_generics = prev;
        return;
      }

      match pair.as_rule() {
        Rule::type2 => {
          for inner in pair.clone().into_inner() {
            match inner.as_rule() {
              Rule::typename | Rule::groupname => {
                self.check_reference(&inner);
                if self.result.is_some() {
                  return;
                }
              }
              _ => {}
            }
            self.walk(inner, rule_generic_params);
            if self.result.is_some() {
              return;
            }
          }
          return;
        }
        Rule::group_entry => {
          for inner in pair.clone().into_inner() {
            if inner.as_rule() == Rule::groupname {
              self.check_reference(&inner);
              if self.result.is_some() {
                return;
              }
            }
            self.walk(inner, rule_generic_params);
            if self.result.is_some() {
              return;
            }
          }
          return;
        }
        _ => {}
      }

      for inner in pair.into_inner() {
        self.walk(inner, rule_generic_params);
        if self.result.is_some() {
          return;
        }
      }
    }

    fn check_reference(&mut self, pair: &Pair<'_, Rule>) {
      // Skip socket/plug references ($typename, $$groupname)
      for child in pair.clone().into_inner() {
        if child.as_rule() == Rule::socket_type || child.as_rule() == Rule::socket_group {
          return;
        }
      }

      for id_pair in pair.clone().into_inner() {
        if id_pair.as_rule() == Rule::id {
          let name = id_pair.as_str();

          if self.defined.contains(name)
            || self.prelude.contains(name)
            || self
              .current_rule_generics
              .is_some_and(|gp| gp.contains(name))
          {
            return;
          }

          // Also skip socket/plug references ($name, $$name)
          if name.starts_with('$') {
            return;
          }

          let pos = pest_span_to_position(&id_pair.as_span(), self.input);
          self.result = Some((name.to_string(), pos));
        }
      }
    }
  }

  let mut finder = RefFinder {
    prelude: &prelude,
    defined: &defined,
    input,
    current_rule_generics: None,
    result: None,
  };

  for pair in pairs {
    finder.walk(pair, &rule_generic_params);
    if finder.result.is_some() {
      break;
    }
  }

  finder.result
}

/// A collected parser error with position and message, used for partial
/// compilation results.
#[cfg(target_arch = "wasm32")]
#[derive(serde::Serialize, Clone, Debug)]
pub struct CollectedError {
  /// Error position in the source
  pub position: Position,
  /// Error message
  pub msg: ErrorMsg,
  /// Severity: "error" or "warning"
  pub severity: String,
}

/// Validate CDDL input and return **all** errors found via partial compilation.
///
/// Strategy:
///   1. Attempt a full-document parse.  If it succeeds the input is valid and
///      an empty error list is returned.
///   2. When the full parse fails, collect its error(s) and additionally split
///      the source into individual top-level rule blocks.  Each block is parsed
///      independently so that errors beyond the first failure are surfaced.
///   3. All errors are deduplicated, and sorted by position.
#[cfg(target_arch = "wasm32")]
pub fn validate_cddl(input: &str, check_refs: bool) -> Vec<CollectedError> {
  // 1. Full-document parse
  match CddlParser::parse(Rule::cddl, input) {
    Ok(pairs) => {
      // Syntax is valid — optionally check for undefined references
      if check_refs {
        check_undefined_references(pairs, input)
      } else {
        Vec::new()
      }
    }
    Err(_) => {
      // Full parse failed — get structured error via our converter
      let e = match cddl_from_pest_str(input) {
        Ok(_) => return Vec::new(), // shouldn't happen, but be safe
        Err(e) => e,
      };
      let mut errors = Vec::new();
      let mut seen = std::collections::HashSet::new();

      // Collect the full-parse error
      push_error(&e, &mut errors, &mut seen);

      // 2. Partial compilation – split into rule blocks and parse each
      let blocks = split_rule_blocks(input);

      for block in &blocks {
        let trimmed = block.text.trim();
        if trimmed.is_empty() || trimmed.starts_with(';') {
          continue;
        }

        if let Err(block_err) = cddl_from_pest_str(&block.text) {
          push_error_with_offset(&block_err, block, input, &mut errors, &mut seen);
        }
      }

      // 3. Optionally check for undefined references in successfully-parsed blocks
      if check_refs {
        for block in &blocks {
          let trimmed = block.text.trim();
          if trimmed.is_empty() || trimmed.starts_with(';') {
            continue;
          }
          if let Ok(block_pairs) = CddlParser::parse(Rule::cddl, &block.text) {
            let ref_errors = check_undefined_references(block_pairs, &block.text);
            for mut re in ref_errors {
              // Adjust positions to document-relative
              re.position.line += block.start_line - 1;
              re.position.range.0 += block.byte_offset;
              re.position.range.1 += block.byte_offset;
              re.position.index += block.byte_offset;
              let key = format!(
                "{}:{}:{}",
                re.position.line, re.position.column, re.msg.short
              );
              if seen.insert(key) {
                errors.push(re);
              }
            }
          }
        }
      }

      // 4. Sort by position
      errors.sort_by(|a, b| {
        a.position
          .line
          .cmp(&b.position.line)
          .then(a.position.column.cmp(&b.position.column))
      });

      errors
    }
  }
}

/// A block of source text corresponding to a single top-level rule.
#[cfg(target_arch = "wasm32")]
struct RuleBlock {
  text: String,
  /// 1-based line number of the first line of this block in the original source
  start_line: usize,
  /// Byte offset of this block's first character in the original source
  byte_offset: usize,
}

/// Split CDDL source into top-level rule blocks.
///
/// A block starts at a line whose non-comment content matches the beginning of
/// a rule definition (`identifier` followed by `=`, `/=` or `//=`).  Everything
/// up to the next such line belongs to the current block.
#[cfg(target_arch = "wasm32")]
fn split_rule_blocks(input: &str) -> Vec<RuleBlock> {
  let mut blocks: Vec<RuleBlock> = Vec::new();
  let mut current_lines: Vec<&str> = Vec::new();
  let mut current_start_line: usize = 1;
  let mut current_byte_offset: usize = 0;

  // Regex-free: a line starts a rule if its non-comment prefix matches
  // `<identifier>  <ws>*  ( = | /= | //= )`
  fn is_rule_start(line: &str) -> bool {
    let stripped = line.trim_start();
    // Identifier: starts with [a-zA-Z_$@], followed by [a-zA-Z0-9_\-$@]*
    let mut chars = stripped.chars().peekable();
    match chars.peek() {
      Some(&c) if c.is_ascii_alphabetic() || c == '_' || c == '$' || c == '@' => {
        chars.next();
      }
      _ => return false,
    }
    while let Some(&c) = chars.peek() {
      if c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '$' || c == '@' || c == '.' {
        chars.next();
      } else {
        break;
      }
    }
    // Skip whitespace
    while let Some(&c) = chars.peek() {
      if c == ' ' || c == '\t' {
        chars.next();
      } else {
        break;
      }
    }
    // Optional generic params <...>
    if chars.peek() == Some(&'<') {
      let mut depth = 0i32;
      while let Some(&c) = chars.peek() {
        chars.next();
        if c == '<' {
          depth += 1;
        } else if c == '>' {
          depth -= 1;
          if depth == 0 {
            break;
          }
        }
      }
      // Skip whitespace after '>'
      while let Some(&c) = chars.peek() {
        if c == ' ' || c == '\t' {
          chars.next();
        } else {
          break;
        }
      }
    }
    // Must see `=`, `/=` or `//=`
    match chars.peek() {
      Some(&'=') => true,
      Some(&'/') => {
        chars.next();
        match chars.peek() {
          Some(&'=') => true, // `/=`
          Some(&'/') => {
            chars.next();
            chars.peek() == Some(&'=') // `//=`
          }
          _ => false,
        }
      }
      _ => false,
    }
  }

  let lines: Vec<&str> = input.split('\n').collect();
  let mut byte_pos: usize = 0;

  for (i, line) in lines.iter().enumerate() {
    let stripped = line.replace(';', "\x00"); // cheap comment strip
    let without_comment: &str = stripped.split('\x00').next().unwrap_or("");
    let _ = without_comment; // not used directly, we use is_rule_start on full line

    if is_rule_start(line) && !current_lines.is_empty() {
      // Flush previous block
      blocks.push(RuleBlock {
        text: current_lines.join("\n"),
        start_line: current_start_line,
        byte_offset: current_byte_offset,
      });
      current_lines = vec![line];
      current_start_line = i + 1;
      current_byte_offset = byte_pos;
    } else {
      if current_lines.is_empty() {
        current_start_line = i + 1;
        current_byte_offset = byte_pos;
      }
      current_lines.push(line);
    }

    byte_pos += line.len() + 1; // +1 for the '\n'
  }

  if !current_lines.is_empty() {
    blocks.push(RuleBlock {
      text: current_lines.join("\n"),
      start_line: current_start_line,
      byte_offset: current_byte_offset,
    });
  }

  blocks
}

/// Push an error into the collection, deduplicating by (line, column, short message).
#[cfg(target_arch = "wasm32")]
fn push_error(
  error: &Error,
  errors: &mut Vec<CollectedError>,
  seen: &mut std::collections::HashSet<String>,
) {
  if let Error::PARSER {
    #[cfg(feature = "ast-span")]
    position,
    msg,
  } = error
  {
    let key = format!("{}:{}:{}", position.line, position.column, msg.short);
    if seen.insert(key) {
      errors.push(CollectedError {
        position: *position,
        msg: msg.clone(),
        severity: "error".to_string(),
      });
    }
  } else {
    // Non-parser error (e.g. regex) — use default position
    let msg_text = error.to_string();
    let key = format!("0:0:{}", msg_text);
    if seen.insert(key) {
      errors.push(CollectedError {
        position: Position::default(),
        msg: ErrorMsg {
          short: msg_text,
          extended: None,
        },
        severity: "error".to_string(),
      });
    }
  }
}

/// Push an error from a partial-compilation block, adjusting positions to be
/// relative to the original full document.
#[cfg(target_arch = "wasm32")]
fn push_error_with_offset(
  error: &Error,
  block: &RuleBlock,
  _input: &str,
  errors: &mut Vec<CollectedError>,
  seen: &mut std::collections::HashSet<String>,
) {
  if let Error::PARSER {
    #[cfg(feature = "ast-span")]
    position,
    msg,
  } = error
  {
    let adjusted = Position {
      line: position.line + block.start_line - 1,
      column: position.column,
      range: (
        position.range.0 + block.byte_offset,
        position.range.1 + block.byte_offset,
      ),
      index: position.index + block.byte_offset,
    };

    let key = format!("{}:{}:{}", adjusted.line, adjusted.column, msg.short);
    if seen.insert(key) {
      errors.push(CollectedError {
        position: adjusted,
        msg: msg.clone(),
        severity: "error".to_string(),
      });
    }
  } else {
    push_error(error, errors, seen);
  }
}

/// Walk a successful pest parse tree to find undefined references.
///
/// Collects all rule definitions (LHS of type/group rules) and all referenced
/// typenames/groupnames in type expressions and group entries.  Any reference
/// that is neither a defined rule, a standard prelude name, nor a generic
/// parameter is reported as a warning.
#[cfg(target_arch = "wasm32")]
fn check_undefined_references(pairs: Pairs<'_, Rule>, input: &str) -> Vec<CollectedError> {
  use std::collections::{HashMap, HashSet};

  let prelude: HashSet<&str> = STANDARD_PRELUDE.iter().copied().collect();

  // Phase 1: collect all defined rule names and per-rule generic parameters
  let mut defined: HashSet<String> = HashSet::new();
  // Map from rule pair span to its generic param names
  let mut rule_generic_params: HashMap<usize, HashSet<String>> = HashMap::new();

  fn collect_definitions(
    pair: &pest::iterators::Pair<'_, Rule>,
    defined: &mut HashSet<String>,
    rule_generic_params: &mut HashMap<usize, HashSet<String>>,
  ) {
    if pair.as_rule() == Rule::rule {
      let mut generic_params_for_rule: HashSet<String> = HashSet::new();
      for inner in pair.clone().into_inner() {
        match inner.as_rule() {
          Rule::typename | Rule::groupname => {
            // Extract the id from typename/groupname
            for id_pair in inner.into_inner() {
              if id_pair.as_rule() == Rule::id {
                defined.insert(id_pair.as_str().to_string());
              }
            }
          }
          Rule::generic_params => {
            for gp in inner.into_inner() {
              if gp.as_rule() == Rule::generic_param {
                for id_pair in gp.into_inner() {
                  if id_pair.as_rule() == Rule::id {
                    generic_params_for_rule.insert(id_pair.as_str().to_string());
                  }
                }
              }
            }
          }
          _ => {}
        }
      }
      if !generic_params_for_rule.is_empty() {
        rule_generic_params.insert(pair.as_span().start(), generic_params_for_rule);
      }
    }
    for inner in pair.clone().into_inner() {
      collect_definitions(&inner, defined, rule_generic_params);
    }
  }

  let pairs_clone = pairs.clone();
  for pair in pairs_clone {
    collect_definitions(&pair, &mut defined, &mut rule_generic_params);
  }

  // Phase 2: find all referenced names and report undefined ones
  struct RefCollector<'a> {
    prelude: &'a HashSet<&'a str>,
    defined: &'a HashSet<String>,
    errors: Vec<CollectedError>,
    seen: HashSet<String>,
    input: &'a str,
    /// Stack of generic parameter sets for the current rule context
    current_rule_generics: Option<&'a HashSet<String>>,
  }

  impl<'a> RefCollector<'a> {
    fn walk(
      &mut self,
      pair: pest::iterators::Pair<'_, Rule>,
      rule_generic_params: &'a HashMap<usize, HashSet<String>>,
    ) {
      // Track the current rule's generics
      if pair.as_rule() == Rule::rule {
        let generics = rule_generic_params.get(&pair.as_span().start());
        let prev = self.current_rule_generics;
        self.current_rule_generics = generics;
        for inner in pair.into_inner() {
          self.walk(inner, rule_generic_params);
        }
        self.current_rule_generics = prev;
        return;
      }

      // Check typename/groupname references in type2 and group_entry
      // In `rule`, the first typename/groupname is the definition (LHS).
      // In `type2` and `group_entry`, typename/groupname are references (RHS).
      match pair.as_rule() {
        Rule::type2 => {
          // type2 can contain: typename ~ generic_args?, or &groupname, or ~typename
          // We need to check the typename/groupname that appear as direct children
          for inner in pair.clone().into_inner() {
            match inner.as_rule() {
              Rule::typename | Rule::groupname => {
                self.check_reference(&inner);
              }
              _ => {}
            }
            self.walk(inner, rule_generic_params);
          }
          return;
        }
        Rule::group_entry => {
          // group_entry can contain: groupname ~ generic_args?
          for inner in pair.clone().into_inner() {
            if inner.as_rule() == Rule::groupname {
              self.check_reference(&inner);
            }
            self.walk(inner, rule_generic_params);
          }
          return;
        }
        _ => {}
      }

      for inner in pair.into_inner() {
        self.walk(inner, rule_generic_params);
      }
    }

    fn check_reference(&mut self, pair: &pest::iterators::Pair<'_, Rule>) {
      // Extract the id from typename/groupname
      for id_pair in pair.clone().into_inner() {
        if id_pair.as_rule() == Rule::id {
          let name = id_pair.as_str();

          // Skip if it's defined, in prelude, or a generic param
          if self.defined.contains(name)
            || self.prelude.contains(name)
            || self
              .current_rule_generics
              .is_some_and(|gp| gp.contains(name))
          {
            return;
          }

          // Also skip socket/plug references ($name, $$name)
          if name.starts_with('$') {
            return;
          }

          let span = id_pair.as_span();
          let pos = position_from_span(span.start(), self.input);
          let end_pos = span.end();

          let msg_short = format!("Undefined reference: '{}'", name);
          let key = format!("{}:{}:{}", pos.line, pos.column, msg_short);

          if self.seen.insert(key) {
            self.errors.push(CollectedError {
              position: Position {
                line: pos.line,
                column: pos.column,
                range: (span.start(), end_pos),
                index: span.start(),
              },
              msg: ErrorMsg {
                short: msg_short,
                extended: Some(format!(
                  "The name '{}' is not defined by any rule in this document and is not a standard prelude type.",
                  name
                )),
              },
              severity: "warning".to_string(),
            });
          }
        }
      }
    }
  }

  let mut collector = RefCollector {
    prelude: &prelude,
    defined: &defined,
    errors: Vec::new(),
    seen: HashSet::new(),
    input,
    current_rule_generics: None,
  };

  for pair in pairs {
    collector.walk(pair, &rule_generic_params);
  }

  collector.errors
}

/// Compute a Position (line, column) from a byte offset in the source.
#[cfg(target_arch = "wasm32")]
fn position_from_span(byte_offset: usize, input: &str) -> Position {
  let mut line = 1usize;
  let mut col = 1usize;
  for (i, ch) in input.char_indices() {
    if i >= byte_offset {
      break;
    }
    if ch == '\n' {
      line += 1;
      col = 1;
    } else {
      col += 1;
    }
  }
  Position {
    line,
    column: col,
    range: (byte_offset, byte_offset),
    index: byte_offset,
  }
}

/// One comment-anchor slot: a tight source position plus which AST field it writes.
#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
#[derive(Debug, Clone, Copy)]
struct Anchor {
  pos: AnchorPos,
  kind: SlotKind,
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
impl SlotKind {
  fn is_leading(self) -> bool {
    matches!(
      self,
      SlotKind::RuleLeading
        | SlotKind::ChoiceLeading
        | SlotKind::GrpChoiceLeading
        | SlotKind::EntryLeading
    )
  }
  fn is_trailing(self) -> bool {
    matches!(self, SlotKind::ChoiceTrailing | SlotKind::EntryTrailing)
  }
}

/// Tight `(lo, close_hi)` extents of every bracketed container (array / map /
/// inline group), used only by the leading-branch enclosing guard in `merge`.
#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn collect_container_extents<'a>(rules: &[ast::Rule<'a>]) -> Vec<(usize, usize)> {
  let mut out = Vec::new();
  for rule in rules {
    match rule {
      ast::Rule::Type { rule, .. } => extents_in_type(&rule.value, &mut out),
      ast::Rule::Group { rule, .. } => extents_in_group_entry(&rule.entry, &mut out),
    }
  }
  out
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn extents_in_type<'a>(ty: &ast::Type<'a>, out: &mut Vec<(usize, usize)>) {
  for tc in &ty.type_choices {
    extents_in_type2(&tc.type1.type2, out);
    if let Some(op) = &tc.type1.operator {
      extents_in_type2(&op.type2, out);
    }
  }
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn extents_in_type2<'a>(t2: &ast::Type2<'a>, out: &mut Vec<(usize, usize)>) {
  match t2 {
    ast::Type2::Map { group, span, .. }
    | ast::Type2::Array { group, span, .. }
    | ast::Type2::ChoiceFromInlineGroup { group, span, .. } => {
      out.push((span.0, span.1));
      extents_in_group(group, out);
    }
    ast::Type2::ParenthesizedType { pt, .. } => extents_in_type(pt, out),
    ast::Type2::TaggedData { t, .. } => extents_in_type(t, out),
    _ => {}
  }
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn extents_in_group<'a>(group: &ast::Group<'a>, out: &mut Vec<(usize, usize)>) {
  for gc in &group.group_choices {
    for (entry, _comma) in &gc.group_entries {
      extents_in_group_entry(entry, out);
    }
  }
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn extents_in_group_entry<'a>(entry: &ast::GroupEntry<'a>, out: &mut Vec<(usize, usize)>) {
  match entry {
    ast::GroupEntry::ValueMemberKey { ge, .. } => extents_in_type(&ge.entry_type, out),
    ast::GroupEntry::InlineGroup { group, span, .. } => {
      out.push((span.0, span.1));
      extents_in_group(group, out);
    }
    ast::GroupEntry::TypeGroupname { .. } => {}
  }
}

/// Bind each comment to exactly one anchor by source position (the rustfmt /
/// prettier trivia-merge model). Returns `(assigned, orphans)`: `assigned[i]` holds
/// the comment texts for `anchors[i]`; `orphans` are comments that bind nowhere and
/// are dropped by the caller (never a panic at the public entry).
#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn merge<'a>(
  comments: &[CommentTok<'a>],
  anchors: &[Anchor],
  containers: &[(usize, usize)],
) -> (Vec<Vec<&'a str>>, Vec<CommentTok<'a>>) {
  let mut assigned: Vec<Vec<&'a str>> = vec![Vec::new(); anchors.len()];
  let mut orphans: Vec<CommentTok<'a>> = Vec::new();

  // Lines carrying a stand-alone (pure) comment — for the leading-contiguity test.
  let pure_lines: std::collections::HashSet<usize> =
    comments.iter().filter(|c| c.pure).map(|c| c.line).collect();

  for c in comments {
    // Step 1 — trailing: nearest same-line preceding tight token; on an equal-`hi`
    // tie the outermost (smallest `lo`) wins; on a *full* (hi, lo) tie — a bare
    // value/typename member whose span equals its inner type1 — the entry wins.
    if !c.pure {
      let prev = anchors
        .iter()
        .enumerate()
        .filter(|(_, a)| a.kind.is_trailing() && a.pos.hi <= c.lo && a.pos.line_hi == c.line)
        .max_by(|(_, a), (_, b)| {
          a.pos
            .hi
            .cmp(&b.pos.hi)
            .then_with(|| b.pos.lo.cmp(&a.pos.lo))
            .then_with(|| {
              (a.kind == SlotKind::EntryTrailing).cmp(&(b.kind == SlotKind::EntryTrailing))
            })
        });
      if let Some((i, _)) = prev {
        assigned[i].push(c.text);
        continue;
      }
    }

    // Step 2 — leading: first following leading anchor (emission order), subject to
    // the enclosing-container guard and the upward-contiguity rule.
    let next = anchors
      .iter()
      .enumerate()
      .find(|(_, a)| a.kind.is_leading() && a.pos.lo > c.hi);

    if let Some((i, a)) = next {
      // Innermost enclosing container = the one with the greatest `lo`. Among nested
      // containers all enclosing the comment, the innermost has the smallest `hi`,
      // so `max(hi)` would wrongly pick the outermost and let a comment leak past
      // its own bracket.
      let enclosing_close = containers
        .iter()
        .filter(|(lo, hi)| *lo < c.lo && c.lo < *hi)
        .max_by_key(|(lo, _)| *lo)
        .map(|(_, hi)| *hi);
      let escapes_container = enclosing_close.map_or(false, |close| close < a.pos.lo);

      // Contiguity: the comment must lie in the unbroken run of pure-comment lines
      // immediately above the anchor (parity with the old line-bucket `leading`);
      // otherwise it is silently dropped (a blank/code line breaks the run).
      let contiguous = (c.line + 1..a.pos.line_hi).all(|l| pure_lines.contains(&l));

      if escapes_container {
        orphans.push(*c);
      } else if contiguous {
        assigned[i].push(c.text);
      }
    } else {
      orphans.push(*c);
    }
  }

  (assigned, orphans)
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn line_of_byte(input: &str, byte: usize) -> usize {
  input[..byte].bytes().filter(|&b| b == b'\n').count() + 1
}

/// Tight `Span` of a `Type2` — its own span, tight for value/container forms (a
/// leaf ends at its token; a container ends at its closing bracket). The basis for
/// deriving a node's rightmost real token, since the enclosing `Type1` /
/// `GroupEntry` spans are *greedy*. The `match` is wildcard-free so a new `Type2`
/// variant is a compile error here rather than a silent misbind.
#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn type2_span(t2: &ast::Type2) -> ast::Span {
  match t2 {
    // These three grammar alternatives end in an optional `generic_args?`
    // (`typename`, `~typename`, `&groupname` — cddl.pest). When the
    // optional is absent, pest folds trailing whitespace+comment+newline into the
    // pair span, so its stored end is GREEDY (lands on the next line). Derive a
    // tight end from the inner identifier / generic args, whose token spans ARE
    // tight — mirroring `entry_tight_end`'s TypeGroupname handling.
    ast::Type2::Typename {
      ident,
      generic_args,
      span,
      ..
    }
    | ast::Type2::Unwrap {
      ident,
      generic_args,
      span,
      ..
    }
    | ast::Type2::ChoiceFromGroup {
      ident,
      generic_args,
      span,
      ..
    } => {
      let end = generic_args
        .as_ref()
        .map(|g| g.span.1)
        .unwrap_or(ident.span.1);
      (span.0, end, span.2)
    }
    ast::Type2::IntValue { span, .. }
    | ast::Type2::UintValue { span, .. }
    | ast::Type2::FloatValue { span, .. }
    | ast::Type2::TextValue { span, .. }
    | ast::Type2::UTF8ByteString { span, .. }
    | ast::Type2::B16ByteString { span, .. }
    | ast::Type2::B64ByteString { span, .. }
    | ast::Type2::ParenthesizedType { span, .. }
    | ast::Type2::Map { span, .. }
    | ast::Type2::Array { span, .. }
    | ast::Type2::ChoiceFromInlineGroup { span, .. }
    | ast::Type2::TaggedData { span, .. }
    // DataMajorType (`#6.5`) and Any (`#`) can also carry a greedy span but have no
    // inner token to recover a tight end from; rare as a hint target, deferred.
    | ast::Type2::DataMajorType { span, .. }
    | ast::Type2::Any { span, .. } => *span,
  }
}

/// Byte offset just past a `Type1`'s rightmost real token. With a range/control
/// operator the controller `type2` is rightmost (`uint .size 4` -> past `4`);
/// otherwise the base `type2`.
#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn type1_tight_end(t1: &ast::Type1) -> usize {
  match &t1.operator {
    Some(op) => type2_span(&op.type2).1,
    None => type2_span(&t1.type2).1,
  }
}

/// Byte offset just past a `GroupEntry`'s rightmost real token — tight, not the
/// greedy `GroupEntry` span which runs forward to the following comma/bracket.
#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn entry_tight_end(e: &ast::GroupEntry) -> usize {
  match e {
    // `entry_type` is the entry's rightmost element (occurrence + member key
    // precede it). An empty `type_choices` only arises from a defaulted `Type` when
    // `entry_type` is absent, unreachable for valid CDDL; fall back to the entry's
    // own (greedy) end so this never panics.
    ast::GroupEntry::ValueMemberKey { ge, span, .. } => ge
      .entry_type
      .type_choices
      .last()
      .map(|tc| type1_tight_end(&tc.type1))
      .unwrap_or(span.1),
    ast::GroupEntry::TypeGroupname { ge, .. } => ge
      .generic_args
      .as_ref()
      .map(|g| g.span.1)
      .unwrap_or_else(|| ge.name.span.1),
    ast::GroupEntry::InlineGroup { span, .. } => span.1,
  }
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn collect_comment_spans<'a>(pair: &Pair<'a, Rule>, out: &mut Vec<PestSpan<'a>>) {
  for inner in pair.clone().into_inner() {
    if inner.as_rule() == Rule::COMMENT {
      out.push(inner.as_span());
    } else {
      collect_comment_spans(&inner, out);
    }
  }
}

// --- Source-order trivia merge: data model + collection ---

/// A source-ordered comment token. `lo` is the `;`; `hi` is the end of the comment
/// text; `pure` means the comment stands alone on its line (leading), otherwise it
/// trails code on the line.
#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
#[derive(Debug, Clone, Copy)]
struct CommentTok<'a> {
  lo: usize,
  hi: usize,
  line: usize,
  pure: bool,
  text: &'a str,
}

/// Tight source position of an anchor slot. `hi`/`line_hi` are the slot's tight
/// rightmost byte and its line; for leading slots `hi == lo`.
#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
#[derive(Debug, Clone, Copy)]
struct AnchorPos {
  lo: usize,
  hi: usize,
  line_hi: usize,
}

/// Which AST comment field an anchor writes.
#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SlotKind {
  RuleLeading,      // Rule::*.comments_before_rule
  ChoiceLeading,    // TypeChoice.comments_before_type   (idx > 0 only)
  ChoiceTrailing,   // TypeChoice.comments_after_type
  GrpChoiceLeading, // GroupChoice.comments_before_grpchoice  (multi-choice groups only)
  EntryLeading,     // GroupEntry.leading_comments
  EntryTrailing,    // GroupEntry.trailing_comments
}

/// Document comment tokens in source order, each tagged leading (`pure`) or
/// trailing. Reuses `collect_comment_spans` (so semicolons inside text/byte strings
/// are never mistaken for comments) and the same alone-on-line test.
#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn collect_comment_toks<'a>(pair: &Pair<'a, Rule>, input: &'a str) -> Vec<CommentTok<'a>> {
  let mut spans = Vec::new();
  collect_comment_spans(pair, &mut spans);

  let toks: Vec<CommentTok> = spans
    .into_iter()
    .map(|span| {
      let lo = span.start();
      let hi = span.end();
      let line = line_of_byte(input, lo);
      let line_start = input[..lo].rfind('\n').map(|i| i + 1).unwrap_or(0);
      let pure = input[line_start..lo].trim().is_empty();
      CommentTok {
        lo,
        hi,
        line,
        pure,
        text: &input[lo + 1..hi], // strip the leading ';'
      }
    })
    .collect();

  debug_assert!(
    toks.windows(2).all(|w| w[0].lo <= w[1].lo),
    "comment tokens must be source-ordered"
  );
  toks
}

/// Walk the built AST once, invoking `cb` for every comment-anchor slot in source
/// order with the slot's tight `AnchorPos`, its `SlotKind`, and an `&mut` to the
/// slot's comment field. Collect and apply both drive this one traversal, so their
/// orderings cannot diverge.
///
/// INVARIANT: callers must not change any structural field (Vec lengths, enum
/// variants) between the two passes — only `Option<Comments>` fields may change.
#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn visit_anchor_slots<'a>(
  rules: &mut [ast::Rule<'a>],
  input: &str,
  mut cb: impl FnMut(AnchorPos, SlotKind, &mut Option<ast::Comments<'a>>),
) {
  for rule in rules.iter_mut() {
    visit_rule(rule, input, &mut cb);
  }
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn leading_pos(input: &str, lo: usize) -> AnchorPos {
  AnchorPos {
    lo,
    hi: lo,
    line_hi: line_of_byte(input, lo),
  }
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn trailing_pos(input: &str, lo: usize, hi: usize) -> AnchorPos {
  AnchorPos {
    lo,
    hi,
    line_hi: line_of_byte(input, hi),
  }
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn visit_rule<'a>(
  rule: &mut ast::Rule<'a>,
  input: &str,
  cb: &mut dyn FnMut(AnchorPos, SlotKind, &mut Option<ast::Comments<'a>>),
) {
  match rule {
    ast::Rule::Type {
      rule,
      span,
      comments_before_rule,
      ..
    } => {
      cb(
        leading_pos(input, span.0),
        SlotKind::RuleLeading,
        comments_before_rule,
      );
      visit_type(&mut rule.value, input, &mut *cb);
    }
    ast::Rule::Group {
      rule,
      span,
      comments_before_rule,
      ..
    } => {
      cb(
        leading_pos(input, span.0),
        SlotKind::RuleLeading,
        comments_before_rule,
      );
      visit_group_entry(&mut rule.entry, input, &mut *cb);
    }
  }
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn visit_type<'a>(
  ty: &mut ast::Type<'a>,
  input: &str,
  cb: &mut dyn FnMut(AnchorPos, SlotKind, &mut Option<ast::Comments<'a>>),
) {
  for (i, tc) in ty.type_choices.iter_mut().enumerate() {
    let lo = tc.type1.span.0;
    let tight = type1_tight_end(&tc.type1);
    if i > 0 {
      cb(
        leading_pos(input, lo),
        SlotKind::ChoiceLeading,
        &mut tc.comments_before_type,
      );
    }
    cb(
      trailing_pos(input, lo, tight),
      SlotKind::ChoiceTrailing,
      &mut tc.comments_after_type,
    );
    visit_type2(&mut tc.type1.type2, input, &mut *cb);
    if let Some(op) = &mut tc.type1.operator {
      visit_type2(&mut op.type2, input, &mut *cb);
    }
  }
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn visit_type2<'a>(
  t2: &mut ast::Type2<'a>,
  input: &str,
  cb: &mut dyn FnMut(AnchorPos, SlotKind, &mut Option<ast::Comments<'a>>),
) {
  match t2 {
    ast::Type2::Map { group, .. }
    | ast::Type2::Array { group, .. }
    | ast::Type2::ChoiceFromInlineGroup { group, .. } => visit_group(group, input, cb),
    ast::Type2::ParenthesizedType { pt, .. } => visit_type(pt, input, cb),
    ast::Type2::TaggedData { t, .. } => visit_type(t, input, cb),
    _ => {}
  }
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn visit_group<'a>(
  group: &mut ast::Group<'a>,
  input: &str,
  cb: &mut dyn FnMut(AnchorPos, SlotKind, &mut Option<ast::Comments<'a>>),
) {
  // GrpChoiceLeading only for true `//` alternatives; for a single-choice group a
  // before-first-entry comment belongs to the first entry's leading slot.
  let multi = group.group_choices.len() > 1;
  for gc in group.group_choices.iter_mut() {
    if multi {
      cb(
        leading_pos(input, gc.span.0),
        SlotKind::GrpChoiceLeading,
        &mut gc.comments_before_grpchoice,
      );
    }
    for (entry, _comma) in gc.group_entries.iter_mut() {
      visit_group_entry(entry, input, &mut *cb);
    }
  }
}

#[cfg(all(feature = "ast-comments", feature = "ast-span"))]
fn visit_group_entry<'a>(
  entry: &mut ast::GroupEntry<'a>,
  input: &str,
  cb: &mut dyn FnMut(AnchorPos, SlotKind, &mut Option<ast::Comments<'a>>),
) {
  let tight = entry_tight_end(entry);
  match entry {
    ast::GroupEntry::ValueMemberKey {
      ge,
      span,
      leading_comments,
      trailing_comments,
    } => {
      let lo = span.0;
      cb(
        leading_pos(input, lo),
        SlotKind::EntryLeading,
        leading_comments,
      );
      cb(
        trailing_pos(input, lo, tight),
        SlotKind::EntryTrailing,
        trailing_comments,
      );
      visit_type(&mut ge.entry_type, input, &mut *cb);
    }
    ast::GroupEntry::TypeGroupname {
      span,
      leading_comments,
      trailing_comments,
      ..
    } => {
      let lo = span.0;
      cb(
        leading_pos(input, lo),
        SlotKind::EntryLeading,
        leading_comments,
      );
      cb(
        trailing_pos(input, lo, tight),
        SlotKind::EntryTrailing,
        trailing_comments,
      );
    }
    ast::GroupEntry::InlineGroup { group, .. } => {
      visit_group(group, input, &mut *cb);
    }
  }
}

/// Convert Pest Pairs to CDDL AST
fn convert_cddl<'a>(mut pairs: Pairs<'a, Rule>, input: &'a str) -> Result<ast::CDDL<'a>, Error> {
  let pair = pairs.next().ok_or_else(|| Error::PARSER {
    #[cfg(feature = "ast-span")]
    position: Position::default(),
    msg: ErrorMsg {
      short: "No CDDL rule found".to_string(),
      extended: None,
    },
  })?;

  let mut rules = Vec::new();

  // Collect the document's comment tokens (real `COMMENT` pairs, so semicolons in
  // text/byte strings are never mistaken for comments) before the pair tree is
  // consumed; they are bound to AST nodes by a source-order merge once the rules
  // are built (see `merge`).
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  let comment_toks = collect_comment_toks(&pair, input);

  for inner_pair in pair.into_inner() {
    match inner_pair.as_rule() {
      Rule::rule => {
        rules.push(convert_rule(inner_pair, input)?);
      }
      Rule::EOI => break,
      _ => {}
    }
  }

  // Bind comments to AST nodes: collect anchors in source order, merge each
  // comment to exactly one anchor by position, then apply via the same traversal
  // (one definition of order, so the two passes cannot desync). Orphans — comments
  // in unsupported positions — are dropped, never panicked on.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  {
    let mut anchors: Vec<Anchor> = Vec::new();
    visit_anchor_slots(&mut rules, input, |pos, kind, _slot| {
      anchors.push(Anchor { pos, kind })
    });
    let containers = collect_container_extents(&rules);
    let (assigned, _orphans) = merge(&comment_toks, &anchors, &containers);
    let mut ai = 0usize;
    visit_anchor_slots(&mut rules, input, |_pos, _kind, slot| {
      if !assigned[ai].is_empty() {
        *slot = Some(ast::Comments(assigned[ai].clone()));
      }
      ai += 1;
    });
  }

  // Check for duplicate rule names (non-alternate rules)
  let mut seen_names: HashMap<String, usize> = HashMap::new();
  for (idx, rule) in rules.iter().enumerate() {
    let name = rule.name();
    let is_alternate = match rule {
      ast::Rule::Type { rule, .. } => rule.is_type_choice_alternate,
      ast::Rule::Group { rule, .. } => rule.is_group_choice_alternate,
    };
    if !is_alternate {
      if let Some(_prev_idx) = seen_names.get(&name) {
        return Err(Error::PARSER {
          #[cfg(feature = "ast-span")]
          position: Position::default(),
          msg: ErrorMsg {
            short: format!("rule \"{}\" is already defined", name),
            extended: None,
          },
        });
      }
      seen_names.insert(name, idx);
    }
  }

  Ok(ast::CDDL {
    rules,
    #[cfg(feature = "ast-comments")]
    comments: None,
  })
}

/// Convert a Pest rule Pair to AST Rule
fn convert_rule<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::Rule<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  let mut inner = pair.into_inner();
  let first = inner.next().ok_or_else(|| Error::PARSER {
    #[cfg(feature = "ast-span")]
    position: Position::default(),
    msg: ErrorMsg {
      short: "Empty rule".to_string(),
      extended: None,
    },
  })?;

  // Determine if this is a type rule or group rule based on first identifier
  match first.as_rule() {
    Rule::typename => {
      let name = convert_identifier(first, input, false)?;
      let mut generic_params = None;
      let mut is_type_choice_alternate = false;
      let mut value = None;

      for p in inner {
        match p.as_rule() {
          Rule::generic_params => {
            generic_params = Some(convert_generic_params(p, input)?);
          }
          Rule::assign_t => {
            for assign_pair in p.into_inner() {
              if assign_pair.as_rule() == Rule::assign_t_choice {
                is_type_choice_alternate = true;
              }
            }
          }
          Rule::type_expr => {
            value = Some(convert_type_expr(p, input)?);
          }
          _ => {}
        }
      }

      let value = value.ok_or_else(|| Error::PARSER {
        #[cfg(feature = "ast-span")]
        position: Position::default(),
        msg: ErrorMsg {
          short: "Missing type expression in type rule".to_string(),
          extended: None,
        },
      })?;

      Ok(ast::Rule::Type {
        rule: ast::TypeRule {
          name,
          generic_params,
          is_type_choice_alternate,
          value,
          #[cfg(feature = "ast-comments")]
          comments_before_assignt: None,
          #[cfg(feature = "ast-comments")]
          comments_after_assignt: None,
        },
        #[cfg(feature = "ast-span")]
        span,
        #[cfg(feature = "ast-comments")]
        comments_before_rule: None,
        #[cfg(feature = "ast-comments")]
        comments_after_rule: None,
      })
    }
    Rule::groupname => {
      let name = convert_identifier(first, input, true)?;
      let mut generic_params = None;
      let mut is_group_choice_alternate = false;
      let mut entry = None;

      for p in inner {
        match p.as_rule() {
          Rule::generic_params => {
            generic_params = Some(convert_generic_params(p, input)?);
          }
          Rule::assign_g => {
            for assign_pair in p.into_inner() {
              if assign_pair.as_rule() == Rule::assign_g_choice {
                is_group_choice_alternate = true;
              }
            }
          }
          Rule::group_entry => {
            entry = Some(convert_group_entry(p, input)?);
          }
          _ => {}
        }
      }

      let entry = entry.ok_or_else(|| Error::PARSER {
        #[cfg(feature = "ast-span")]
        position: Position::default(),
        msg: ErrorMsg {
          short: "Missing group entry in group rule".to_string(),
          extended: None,
        },
      })?;

      Ok(ast::Rule::Group {
        rule: Box::new(ast::GroupRule {
          name,
          generic_params,
          is_group_choice_alternate,
          entry,
          #[cfg(feature = "ast-comments")]
          comments_before_assigng: None,
          #[cfg(feature = "ast-comments")]
          comments_after_assigng: None,
        }),
        #[cfg(feature = "ast-span")]
        span,
        #[cfg(feature = "ast-comments")]
        comments_before_rule: None,
        #[cfg(feature = "ast-comments")]
        comments_after_rule: None,
      })
    }
    _ => Err(Error::PARSER {
      #[cfg(feature = "ast-span")]
      position: Position::default(),
      msg: ErrorMsg {
        short: format!("Unexpected rule type: {:?}", first.as_rule()),
        extended: None,
      },
    }),
  }
}

/// Convert identifier (typename or groupname)
#[allow(unused_variables)]
fn convert_identifier<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
  _is_group: bool,
) -> Result<ast::Identifier<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  let mut socket = None;
  let mut ident = pair.as_str();

  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::socket_type => {
        socket = Some(SocketPlug::TYPE);
        // The actual id will follow
      }
      Rule::socket_group => {
        socket = Some(SocketPlug::GROUP);
        // The actual id will follow
      }
      Rule::id => {
        ident = inner.as_str();
      }
      _ => {}
    }
  }

  Ok(ast::Identifier {
    ident,
    socket,
    #[cfg(feature = "ast-span")]
    span,
  })
}

/// Convert generic parameters
#[allow(unused_variables)]
fn convert_generic_params<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
) -> Result<ast::GenericParams<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  let mut params = Vec::new();

  for inner in pair.into_inner() {
    if inner.as_rule() == Rule::generic_param {
      for id_pair in inner.into_inner() {
        if id_pair.as_rule() == Rule::id {
          params.push(ast::GenericParam {
            param: ast::Identifier {
              ident: id_pair.as_str(),
              socket: None,
              #[cfg(feature = "ast-span")]
              span: pest_span_to_ast_span(&id_pair.as_span(), input),
            },
            #[cfg(feature = "ast-comments")]
            comments_before_ident: None,
            #[cfg(feature = "ast-comments")]
            comments_after_ident: None,
          });
        }
      }
    }
  }

  Ok(ast::GenericParams {
    params,
    #[cfg(feature = "ast-span")]
    span,
  })
}

/// Convert generic arguments
fn convert_generic_args<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
) -> Result<ast::GenericArgs<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  let mut args = Vec::new();

  for inner in pair.into_inner() {
    if inner.as_rule() == Rule::generic_arg {
      for type1_pair in inner.into_inner() {
        if type1_pair.as_rule() == Rule::type1 {
          args.push(ast::GenericArg {
            arg: Box::new(convert_type1(type1_pair, input)?),
            #[cfg(feature = "ast-comments")]
            comments_before_type: None,
            #[cfg(feature = "ast-comments")]
            comments_after_type: None,
          });
        }
      }
    }
  }

  Ok(ast::GenericArgs {
    args,
    #[cfg(feature = "ast-span")]
    span,
  })
}

/// Convert type expression
fn convert_type_expr<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::Type<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  let mut type_choices = Vec::new();

  for inner in pair.into_inner() {
    if inner.as_rule() == Rule::type_choice {
      for type1_pair in inner.into_inner() {
        if type1_pair.as_rule() == Rule::type1 {
          type_choices.push(ast::TypeChoice {
            type1: convert_type1(type1_pair, input)?,
            #[cfg(feature = "ast-comments")]
            comments_before_type: None,
            #[cfg(feature = "ast-comments")]
            comments_after_type: None,
          });
        }
      }
    }
  }

  Ok(ast::Type {
    type_choices,
    #[cfg(feature = "ast-span")]
    span,
  })
}

/// Convert type1 expression
fn convert_type1<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::Type1<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  let mut type2 = None;
  let mut operator = None;

  // Clone the pair to allow multiple iterations
  let pair_clone = pair.clone();

  for inner in pair_clone.into_inner() {
    match inner.as_rule() {
      Rule::type2 => {
        if type2.is_none() {
          type2 = Some(convert_type2(inner, input)?);
        } else {
          // This is the second type2 in a range or control operation
          if let Some(ref _op_type2) = type2 {
            // We need to determine the operator from previous context
            // For now, we'll handle this in the operator conversion
          }
        }
      }
      Rule::range_op => {
        let is_inclusive = inner
          .clone()
          .into_inner()
          .any(|p| p.as_rule() == Rule::range_op_inclusive);
        operator = Some(ast::Operator {
          operator: ast::RangeCtlOp::RangeOp {
            is_inclusive,
            #[cfg(feature = "ast-span")]
            span: pest_span_to_ast_span(&inner.as_span(), input),
          },
          type2: ast::Type2::Any {
            #[cfg(feature = "ast-span")]
            span: ast::Span::default(),
          }, // Will be filled in next iteration
          #[cfg(feature = "ast-comments")]
          comments_before_operator: None,
          #[cfg(feature = "ast-comments")]
          comments_after_operator: None,
        });
      }
      Rule::control_op => {
        let ctrl = convert_control_operator(inner.clone(), input)?;
        operator = Some(ast::Operator {
          operator: ast::RangeCtlOp::CtlOp {
            ctrl,
            #[cfg(feature = "ast-span")]
            span: pest_span_to_ast_span(&inner.as_span(), input),
          },
          type2: ast::Type2::Any {
            #[cfg(feature = "ast-span")]
            span: ast::Span::default(),
          }, // Will be filled by controller rule
          #[cfg(feature = "ast-comments")]
          comments_before_operator: None,
          #[cfg(feature = "ast-comments")]
          comments_after_operator: None,
        });
      }
      Rule::controller => {
        // This contains the type2 for the control operator
        for controller_inner in inner.into_inner() {
          if controller_inner.as_rule() == Rule::type2 {
            if let Some(ref mut op) = operator {
              op.type2 = convert_type2(controller_inner, input)?;
            }
          }
        }
      }
      _ => {}
    }
  }

  // Fill in the second type2 for range operators if we have an operator but its type2 is Any
  let pairs_vec: Vec<_> = pair.into_inner().collect();
  if let Some(ref mut op) = operator {
    if matches!(op.type2, ast::Type2::Any { .. }) {
      // Find the second type2
      let type2_pairs: Vec<_> = pairs_vec
        .iter()
        .filter(|p| p.as_rule() == Rule::type2)
        .collect();
      if type2_pairs.len() > 1 {
        op.type2 = convert_type2(type2_pairs[1].clone(), input)?;
      }
    }
  }

  let type2 = type2.ok_or_else(|| Error::PARSER {
    #[cfg(feature = "ast-span")]
    position: Position::default(),
    msg: ErrorMsg {
      short: "Missing type2 in type1".to_string(),
      extended: None,
    },
  })?;

  Ok(ast::Type1 {
    type2,
    operator,
    #[cfg(feature = "ast-span")]
    span,
    #[cfg(feature = "ast-comments")]
    comments_after_type: None,
  })
}

/// Convert control operator
#[allow(unused_variables)]
fn convert_control_operator<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
) -> Result<ControlOperator, Error> {
  for inner in pair.into_inner() {
    if inner.as_rule() == Rule::control_name {
      let ctrl_str = inner.as_str();
      // Prepend dot to match the expected format for lookup_control_from_str
      let ctrl_with_dot = format!(".{}", ctrl_str);
      return lookup_control_from_str(&ctrl_with_dot).ok_or_else(|| Error::PARSER {
        #[cfg(feature = "ast-span")]
        position: pest_span_to_position(&inner.as_span(), input),
        msg: ErrorMsg {
          short: format!("Invalid control operator: {}", ctrl_str),
          extended: None,
        },
      });
    }
  }

  Err(Error::PARSER {
    #[cfg(feature = "ast-span")]
    position: Position::default(),
    msg: ErrorMsg {
      short: "Missing control operator name".to_string(),
      extended: None,
    },
  })
}

/// Convert type2 expression
fn convert_type2<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::Type2<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  // Use the raw text prefix to determine which type2 alternative matched,
  // since PEG literals (~, &, (, {, [, #) are not captured as child pairs.
  let text = pair.as_str().trim_start();

  if text.starts_with('~') {
    // Unwrap: "~" ~ typename ~ generic_args?
    let mut ident = None;
    let mut generic_args = None;
    for inner in pair.into_inner() {
      match inner.as_rule() {
        Rule::typename => ident = Some(convert_identifier(inner, input, false)?),
        Rule::generic_args => generic_args = Some(convert_generic_args(inner, input)?),
        _ => {}
      }
    }
    let ident = ident.ok_or_else(|| Error::PARSER {
      #[cfg(feature = "ast-span")]
      position: Position::default(),
      msg: ErrorMsg {
        short: "Missing identifier in unwrap expression".to_string(),
        extended: None,
      },
    })?;
    Ok(ast::Type2::Unwrap {
      ident,
      generic_args,
      #[cfg(feature = "ast-span")]
      span,
      #[cfg(feature = "ast-comments")]
      comments: None,
    })
  } else if text.starts_with('&') {
    // Choice from group or inline group
    let mut group_child = None;
    let mut groupname_ident = None;
    let mut generic_args = None;
    for inner in pair.into_inner() {
      match inner.as_rule() {
        Rule::group => group_child = Some(convert_group(inner, input)?),
        Rule::groupname => groupname_ident = Some(convert_identifier(inner, input, true)?),
        Rule::generic_args => generic_args = Some(convert_generic_args(inner, input)?),
        _ => {}
      }
    }
    if let Some(group) = group_child {
      Ok(ast::Type2::ChoiceFromInlineGroup {
        group,
        #[cfg(feature = "ast-span")]
        span,
        #[cfg(feature = "ast-comments")]
        comments: None,
        #[cfg(feature = "ast-comments")]
        comments_before_group: None,
        #[cfg(feature = "ast-comments")]
        comments_after_group: None,
      })
    } else if let Some(ident) = groupname_ident {
      Ok(ast::Type2::ChoiceFromGroup {
        ident,
        generic_args,
        #[cfg(feature = "ast-span")]
        span,
        #[cfg(feature = "ast-comments")]
        comments: None,
      })
    } else {
      Err(Error::PARSER {
        #[cfg(feature = "ast-span")]
        position: Position::default(),
        msg: ErrorMsg {
          short: "Invalid choice-from-group expression".to_string(),
          extended: None,
        },
      })
    }
  } else if text.starts_with('(') {
    // Parenthesized type: "(" ~ type_expr ~ ")"
    for inner in pair.into_inner() {
      if inner.as_rule() == Rule::type_expr {
        return Ok(ast::Type2::ParenthesizedType {
          pt: convert_type_expr(inner, input)?,
          #[cfg(feature = "ast-span")]
          span,
          #[cfg(feature = "ast-comments")]
          comments_before_type: None,
          #[cfg(feature = "ast-comments")]
          comments_after_type: None,
        });
      }
    }
    Ok(ast::Type2::Any {
      #[cfg(feature = "ast-span")]
      span,
    })
  } else if text.starts_with('{') {
    // Map: "{" ~ group ~ "}"
    for inner in pair.into_inner() {
      if inner.as_rule() == Rule::group {
        return Ok(ast::Type2::Map {
          group: convert_group(inner, input)?,
          #[cfg(feature = "ast-span")]
          span,
          #[cfg(feature = "ast-comments")]
          comments_before_group: None,
          #[cfg(feature = "ast-comments")]
          comments_after_group: None,
        });
      }
    }
    Ok(ast::Type2::Any {
      #[cfg(feature = "ast-span")]
      span,
    })
  } else if text.starts_with('[') {
    // Array: "[" ~ group ~ "]"
    for inner in pair.into_inner() {
      if inner.as_rule() == Rule::group {
        return Ok(ast::Type2::Array {
          group: convert_group(inner, input)?,
          #[cfg(feature = "ast-span")]
          span,
          #[cfg(feature = "ast-comments")]
          comments_before_group: None,
          #[cfg(feature = "ast-comments")]
          comments_after_group: None,
        });
      }
    }
    Ok(ast::Type2::Any {
      #[cfg(feature = "ast-span")]
      span,
    })
  } else if text.starts_with('#') {
    // Tag expression
    for inner in pair.into_inner() {
      if inner.as_rule() == Rule::tag_expr {
        return convert_tag_expr(inner, input);
      }
    }
    Ok(ast::Type2::Any {
      #[cfg(feature = "ast-span")]
      span,
    })
  } else {
    // Remaining alternatives: value | typename ~ generic_args?
    let mut value_pair = None;
    let mut typename_ident = None;
    let mut generic_args = None;

    for inner in pair.into_inner() {
      match inner.as_rule() {
        Rule::value => {
          value_pair = Some(inner);
        }
        Rule::typename => {
          typename_ident = Some(convert_identifier(inner, input, false)?);
        }
        Rule::generic_args => {
          generic_args = Some(convert_generic_args(inner, input)?);
        }
        _ => {}
      }
    }

    if let Some(vp) = value_pair {
      convert_value_to_type2(
        vp,
        input,
        #[cfg(feature = "ast-span")]
        span,
      )
    } else if let Some(ident) = typename_ident {
      Ok(ast::Type2::Typename {
        ident,
        generic_args,
        #[cfg(feature = "ast-span")]
        span,
      })
    } else {
      Ok(ast::Type2::Any {
        #[cfg(feature = "ast-span")]
        span,
      })
    }
  }
}

/// Convert value to Type2
#[cfg(feature = "ast-span")]
fn convert_value_to_type2<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
  span: ast::Span,
) -> Result<ast::Type2<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::number => {
        return convert_number_to_type2(inner, input, span);
      }
      Rule::text_value => {
        let text = inner.as_str();
        // Remove quotes
        let text_content = &text[1..text.len() - 1];
        // Handle escape sequences
        let unescaped = unescape_text(text_content);
        return Ok(ast::Type2::TextValue {
          value: Cow::Owned(unescaped),
          span,
        });
      }
      Rule::bytes_value => {
        return convert_bytes_value_to_type2(inner, input, span);
      }
      _ => {}
    }
  }

  Err(Error::PARSER {
    position: Position::default(),
    msg: ErrorMsg {
      short: "Invalid value".to_string(),
      extended: None,
    },
  })
}

#[cfg(not(feature = "ast-span"))]
fn convert_value_to_type2<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
) -> Result<ast::Type2<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::number => {
        return convert_number_to_type2(inner, input);
      }
      Rule::text_value => {
        let text = inner.as_str();
        // Remove quotes
        let text_content = &text[1..text.len() - 1];
        // Handle escape sequences
        let unescaped = unescape_text(text_content);
        return Ok(ast::Type2::TextValue {
          value: Cow::Owned(unescaped),
        });
      }
      Rule::bytes_value => {
        return convert_bytes_value_to_type2(inner, input);
      }
      _ => {}
    }
  }

  Err(Error::PARSER {
    msg: ErrorMsg {
      short: "Invalid value".to_string(),
      extended: None,
    },
  })
}

/// Unescape text value (supports RFC 9682 \u{hex} escapes and surrogate pairs)
fn unescape_text(text: &str) -> String {
  let mut result = String::new();
  let mut chars = text.chars();

  while let Some(ch) = chars.next() {
    if ch == '\\' {
      if let Some(next_ch) = chars.next() {
        match next_ch {
          'n' => result.push('\n'),
          'r' => result.push('\r'),
          't' => result.push('\t'),
          '\\' => result.push('\\'),
          '"' => result.push('"'),
          '\'' => result.push('\''),
          '/' => result.push('/'),
          'b' => result.push('\u{0008}'),
          'f' => result.push('\u{000C}'),
          'u' => {
            // Check for RFC 9682 \u{hex} form
            let mut peekable = chars.clone();
            if peekable.next() == Some('{') {
              // Consume the '{'
              chars.next();
              let hex: String = chars.by_ref().take_while(|c| *c != '}').collect();
              if let Ok(code_point) = u32::from_str_radix(&hex, 16) {
                if let Some(unicode_char) = char::from_u32(code_point) {
                  result.push(unicode_char);
                }
              }
            } else {
              // Standard \uXXXX form
              let hex: String = chars.by_ref().take(4).collect();
              if let Ok(code_point) = u32::from_str_radix(&hex, 16) {
                // Check for surrogate pair: \uHHHH\uLLLL
                if (0xD800..=0xDBFF).contains(&code_point) {
                  // High surrogate - look for \uLLLL
                  let mut peekable2 = chars.clone();
                  if peekable2.next() == Some('\\') && peekable2.next() == Some('u') {
                    // Consume \u
                    chars.next();
                    chars.next();
                    let low_hex: String = chars.by_ref().take(4).collect();
                    if let Ok(low_surrogate) = u32::from_str_radix(&low_hex, 16) {
                      if (0xDC00..=0xDFFF).contains(&low_surrogate) {
                        let combined =
                          0x10000 + ((code_point - 0xD800) << 10) + (low_surrogate - 0xDC00);
                        if let Some(unicode_char) = char::from_u32(combined) {
                          result.push(unicode_char);
                        }
                      }
                    }
                  }
                } else if let Some(unicode_char) = char::from_u32(code_point) {
                  result.push(unicode_char);
                }
              }
            }
          }
          _ => {
            result.push('\\');
            result.push(next_ch);
          }
        }
      }
    } else {
      result.push(ch);
    }
  }

  result
}

/// Convert number to Type2
#[cfg(feature = "ast-span")]
fn convert_number_to_type2<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
  span: ast::Span,
) -> Result<ast::Type2<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::uint_value => {
        let val = inner.as_str().parse::<usize>().map_err(|_| Error::PARSER {
          position: pest_span_to_position(&inner.as_span(), input),
          msg: ErrorMsg {
            short: "Invalid unsigned integer".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::UintValue { value: val, span });
      }
      Rule::int_value => {
        let val = inner.as_str().parse::<isize>().map_err(|_| Error::PARSER {
          position: pest_span_to_position(&inner.as_span(), input),
          msg: ErrorMsg {
            short: "Invalid integer".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::IntValue { value: val, span });
      }
      Rule::float_value => {
        let val = inner.as_str().parse::<f64>().map_err(|_| Error::PARSER {
          position: pest_span_to_position(&inner.as_span(), input),
          msg: ErrorMsg {
            short: "Invalid float".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::FloatValue { value: val, span });
      }
      Rule::hexfloat => {
        #[cfg(feature = "std")]
        {
          let val = hexf_parse::parse_hexf64(inner.as_str(), false).map_err(|_| Error::PARSER {
            position: pest_span_to_position(&inner.as_span(), input),
            msg: ErrorMsg {
              short: "Invalid hexfloat".to_string(),
              extended: None,
            },
          })?;
          return Ok(ast::Type2::FloatValue { value: val, span });
        }
        #[cfg(not(feature = "std"))]
        {
          return Err(Error::PARSER {
            position: pest_span_to_position(&inner.as_span(), input),
            msg: crate::error::MsgType::InvalidHexFloat.into(),
          });
        }
      }
      _ => {}
    }
  }

  Err(Error::PARSER {
    position: Position::default(),
    msg: ErrorMsg {
      short: "Invalid number".to_string(),
      extended: None,
    },
  })
}

#[cfg(not(feature = "ast-span"))]
fn convert_number_to_type2<'a>(
  pair: Pair<'a, Rule>,
  _input: &'a str,
) -> Result<ast::Type2<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::uint_value => {
        let val = inner.as_str().parse::<usize>().map_err(|_| Error::PARSER {
          msg: ErrorMsg {
            short: "Invalid unsigned integer".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::UintValue { value: val });
      }
      Rule::int_value => {
        let val = inner.as_str().parse::<isize>().map_err(|_| Error::PARSER {
          msg: ErrorMsg {
            short: "Invalid integer".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::IntValue { value: val });
      }
      Rule::float_value => {
        let val = inner.as_str().parse::<f64>().map_err(|_| Error::PARSER {
          msg: ErrorMsg {
            short: "Invalid float".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::FloatValue { value: val });
      }
      Rule::hexfloat => {
        #[cfg(feature = "std")]
        {
          let val = hexf_parse::parse_hexf64(inner.as_str(), false).map_err(|_| Error::PARSER {
            msg: ErrorMsg {
              short: "Invalid hexfloat".to_string(),
              extended: None,
            },
          })?;
          return Ok(ast::Type2::FloatValue { value: val });
        }
        #[cfg(not(feature = "std"))]
        {
          return Err(Error::PARSER {
            msg: crate::error::MsgType::InvalidHexFloat.into(),
          });
        }
      }
      _ => {}
    }
  }

  Err(Error::PARSER {
    msg: ErrorMsg {
      short: "Invalid number".to_string(),
      extended: None,
    },
  })
}

/// Decode uppercase hex (base16) bytes without requiring alloc feature
fn hex_decode_upper(input: &[u8]) -> Result<Vec<u8>, ()> {
  let decode_len = data_encoding::HEXUPPER
    .decode_len(input.len())
    .map_err(|_| ())?;
  let mut output = vec![0u8; decode_len];
  let len = data_encoding::HEXUPPER
    .decode_mut(input, &mut output)
    .map_err(|_| ())?;
  output.truncate(len);
  Ok(output)
}

/// Convert bytes value to Type2
#[cfg(feature = "ast-span")]
fn convert_bytes_value_to_type2<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
  span: ast::Span,
) -> Result<ast::Type2<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::bytes_b64 => {
        let bytes_str = inner.as_str();
        // Remove quotes
        let content = &bytes_str[1..bytes_str.len() - 1];
        // Single-quoted byte strings are UTF-8 encoded text, not base64
        return Ok(ast::Type2::UTF8ByteString {
          value: Cow::Owned(content.as_bytes().to_vec()),
          span,
        });
      }
      Rule::bytes_b16 => {
        let bytes_str = inner.as_str();
        // Remove h' and '
        let content = &bytes_str[2..bytes_str.len() - 1];
        let cleaned: String = content.chars().filter(|c| !c.is_whitespace()).collect();
        let decoded = hex_decode_upper(cleaned.as_bytes()).map_err(|_| Error::PARSER {
          position: pest_span_to_position(&inner.as_span(), input),
          msg: ErrorMsg {
            short: "Invalid base16 encoding".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::B16ByteString {
          value: Cow::Owned(decoded),
          span,
        });
      }
      Rule::bytes_h_quoted => {
        let bytes_str = inner.as_str();
        // Remove h" and "
        let content = &bytes_str[2..bytes_str.len() - 1];
        return Ok(ast::Type2::UTF8ByteString {
          value: Cow::Owned(content.as_bytes().to_vec()),
          span,
        });
      }
      _ => {}
    }
  }

  Err(Error::PARSER {
    position: Position::default(),
    msg: ErrorMsg {
      short: "Invalid bytes value".to_string(),
      extended: None,
    },
  })
}

#[cfg(not(feature = "ast-span"))]
fn convert_bytes_value_to_type2<'a>(
  pair: Pair<'a, Rule>,
  _input: &'a str,
) -> Result<ast::Type2<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::bytes_b64 => {
        let bytes_str = inner.as_str();
        // Remove quotes
        let content = &bytes_str[1..bytes_str.len() - 1];
        // Single-quoted byte strings are UTF-8 encoded text, not base64
        return Ok(ast::Type2::UTF8ByteString {
          value: Cow::Owned(content.as_bytes().to_vec()),
        });
      }
      Rule::bytes_b16 => {
        let bytes_str = inner.as_str();
        // Remove h' and '
        let content = &bytes_str[2..bytes_str.len() - 1];
        let cleaned: String = content.chars().filter(|c| !c.is_whitespace()).collect();
        let decoded = hex_decode_upper(cleaned.as_bytes()).map_err(|_| Error::PARSER {
          msg: ErrorMsg {
            short: "Invalid base16 encoding".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::B16ByteString {
          value: Cow::Owned(decoded),
        });
      }
      Rule::bytes_h_quoted => {
        let bytes_str = inner.as_str();
        // Remove h" and "
        let content = &bytes_str[2..bytes_str.len() - 1];
        return Ok(ast::Type2::UTF8ByteString {
          value: Cow::Owned(content.as_bytes().to_vec()),
        });
      }
      _ => {}
    }
  }

  Err(Error::PARSER {
    msg: ErrorMsg {
      short: "Invalid bytes value".to_string(),
      extended: None,
    },
  })
}

/// Convert tag expression to Type2
fn convert_tag_expr<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::Type2<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  // Tag expressions can be:
  // #6.32(tstr) - tagged data with literal tag number
  // #6.<typename>(tstr) - tagged data with type constraint
  // #6(type) - tag 6 with type but no constraint
  // #N - data major type N
  // #N.V - data major type N with constraint V
  // # - any
  // #(type) - tagged with no specific tag

  let full_str = pair.as_str().trim();

  if full_str == "#" {
    return Ok(ast::Type2::Any {
      #[cfg(feature = "ast-span")]
      span,
    });
  }

  // Extract major type digit from the string (first char after #)
  let after_hash = &full_str[1..];
  let major_type: Option<u8> = after_hash
    .chars()
    .next()
    .and_then(|c| c.to_digit(10))
    .map(|d| d as u8);

  let mut tag_constraint: Option<TagConstraint<'a>> = None;
  let mut type_expr = None;

  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::tag_value => {
        // tag_value = { uint_value | "<" ~ type_expr ~ ">" }
        for tv in inner.into_inner() {
          match tv.as_rule() {
            Rule::uint_value => {
              let val = tv.as_str().parse::<u64>().unwrap_or(0);
              tag_constraint = Some(TagConstraint::Literal(val));
            }
            Rule::type_expr => {
              tag_constraint = Some(TagConstraint::Type(tv.as_str()));
            }
            _ => {}
          }
        }
      }
      Rule::type_expr => {
        type_expr = Some(convert_type_expr(inner, input)?);
      }
      _ => {}
    }
  }

  match major_type {
    Some(6) => {
      // CBOR tag (#6.N(type) or #6(type))
      let t = type_expr.unwrap_or_else(|| ast::Type {
        type_choices: vec![],
        #[cfg(feature = "ast-span")]
        span: ast::Span::default(),
      });
      Ok(ast::Type2::TaggedData {
        tag: tag_constraint,
        t,
        #[cfg(feature = "ast-span")]
        span,
        #[cfg(feature = "ast-comments")]
        comments_before_type: None,
        #[cfg(feature = "ast-comments")]
        comments_after_type: None,
      })
    }
    Some(mt) => {
      // Data major type (#N or #N.V)
      Ok(ast::Type2::DataMajorType {
        mt,
        constraint: tag_constraint,
        #[cfg(feature = "ast-span")]
        span,
      })
    }
    None => {
      // Just "#" with optional (type)
      if let Some(t) = type_expr {
        Ok(ast::Type2::TaggedData {
          tag: None,
          t,
          #[cfg(feature = "ast-span")]
          span,
          #[cfg(feature = "ast-comments")]
          comments_before_type: None,
          #[cfg(feature = "ast-comments")]
          comments_after_type: None,
        })
      } else {
        Ok(ast::Type2::Any {
          #[cfg(feature = "ast-span")]
          span,
        })
      }
    }
  }
}

/// Convert group
fn convert_group<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::Group<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  let mut group_choices = Vec::new();

  for inner in pair.into_inner() {
    if inner.as_rule() == Rule::group_choice {
      group_choices.push(convert_group_choice(inner, input)?);
    }
  }

  Ok(ast::Group {
    group_choices,
    #[cfg(feature = "ast-span")]
    span,
  })
}

/// Convert group choice
fn convert_group_choice<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
) -> Result<ast::GroupChoice<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  let mut group_entries = Vec::new();

  for inner in pair.into_inner() {
    if inner.as_rule() == Rule::group_entry {
      let entry = convert_group_entry(inner, input)?;
      group_entries.push((
        entry,
        ast::OptionalComma {
          optional_comma: false,
          #[cfg(feature = "ast-comments")]
          trailing_comments: None,
          _a: core::marker::PhantomData,
        },
      ));
    }
  }

  Ok(ast::GroupChoice {
    group_entries,
    #[cfg(feature = "ast-span")]
    span,
    #[cfg(feature = "ast-comments")]
    comments_before_grpchoice: None,
  })
}

/// Convert group entry
fn convert_group_entry<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
) -> Result<ast::GroupEntry<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  let mut occur = None;
  let mut member_key = None;
  let mut entry_type = None;
  let mut groupname_ident = None;
  let mut generic_args = None;
  let mut inline_group = None;
  let mut is_cut = false;
  let mut is_arrow_map = false;

  // Check the full matched text to determine the entry type
  let full_text = pair.as_str();
  let has_member_key = pair
    .clone()
    .into_inner()
    .any(|p| p.as_rule() == Rule::member_key);
  let has_cut = pair.clone().into_inner().any(|p| p.as_rule() == Rule::cut);
  // Determine colon vs arrow by checking for "=>" in the text
  // We need to be careful: "=>" could appear inside a text value
  // Use the grammar structure: arrow alternative has cut? child and comes first
  let has_arrow = has_member_key && (has_cut || full_text.contains("=>"));
  let has_colon = has_member_key && !has_arrow && full_text.contains(':');

  if has_cut {
    is_cut = true;
  }

  if has_arrow {
    is_arrow_map = true;
  }

  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::occur => {
        occur = Some(convert_occurrence(inner, input)?);
      }
      Rule::cut => {
        is_cut = true;
      }
      Rule::member_key => {
        if has_colon || has_arrow {
          // This is a real member key
          member_key = Some(convert_member_key_simple(
            inner,
            input,
            is_arrow_map,
            is_cut,
            #[cfg(feature = "ast-span")]
            span,
          )?);
        }
      }
      Rule::type_expr => {
        entry_type = Some(convert_type_expr(inner, input)?);
      }
      Rule::groupname => {
        groupname_ident = Some(convert_identifier(inner, input, true)?);
      }
      Rule::generic_args => {
        generic_args = Some(convert_generic_args(inner, input)?);
      }
      Rule::group => {
        inline_group = Some(convert_group(inner, input)?);
      }
      _ => {}
    }
  }

  // Determine the type of group entry
  if let Some(group) = inline_group {
    return Ok(ast::GroupEntry::InlineGroup {
      occur,
      group,
      #[cfg(feature = "ast-span")]
      span,
      #[cfg(feature = "ast-comments")]
      comments_before_group: None,
      #[cfg(feature = "ast-comments")]
      comments_after_group: None,
    });
  }

  if let Some(name) = groupname_ident {
    return Ok(ast::GroupEntry::TypeGroupname {
      ge: ast::TypeGroupnameEntry {
        occur,
        name,
        generic_args,
      },
      #[cfg(feature = "ast-span")]
      span,
      #[cfg(feature = "ast-comments")]
      leading_comments: None,
      #[cfg(feature = "ast-comments")]
      trailing_comments: None,
    });
  }

  // If entry_type is just a single typename with no type choices and no operator,
  // convert it to TypeGroupname for compatibility with downstream code
  if member_key.is_none() {
    if let Some(ref et) = entry_type {
      if et.type_choices.len() == 1 {
        let tc = &et.type_choices[0];
        if tc.type1.operator.is_none() {
          if let ast::Type2::Typename {
            ref ident,
            ref generic_args,
            ..
          } = tc.type1.type2
          {
            return Ok(ast::GroupEntry::TypeGroupname {
              ge: ast::TypeGroupnameEntry {
                occur,
                name: ident.clone(),
                generic_args: generic_args.clone(),
              },
              #[cfg(feature = "ast-span")]
              span,
              #[cfg(feature = "ast-comments")]
              leading_comments: None,
              #[cfg(feature = "ast-comments")]
              trailing_comments: None,
            });
          }
        }
      }
    }
  }

  // Default to ValueMemberKey
  let entry_type = entry_type.unwrap_or_else(|| ast::Type {
    type_choices: vec![],
    #[cfg(feature = "ast-span")]
    span: ast::Span::default(),
  });

  Ok(ast::GroupEntry::ValueMemberKey {
    ge: Box::new(ast::ValueMemberKeyEntry {
      occur,
      member_key,
      entry_type,
    }),
    #[cfg(feature = "ast-span")]
    span,
    #[cfg(feature = "ast-comments")]
    leading_comments: None,
    #[cfg(feature = "ast-comments")]
    trailing_comments: None,
  })
}

/// Convert occurrence indicator
#[allow(unused_variables)]
fn convert_occurrence<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
) -> Result<ast::Occurrence<'a>, Error> {
  for inner in pair.into_inner() {
    let occur = match inner.as_rule() {
      Rule::occur_optional => ast::Occur::Optional {
        #[cfg(feature = "ast-span")]
        span: pest_span_to_ast_span(&inner.as_span(), input),
      },
      Rule::occur_zero_or_more => ast::Occur::ZeroOrMore {
        #[cfg(feature = "ast-span")]
        span: pest_span_to_ast_span(&inner.as_span(), input),
      },
      Rule::occur_one_or_more => ast::Occur::OneOrMore {
        #[cfg(feature = "ast-span")]
        span: pest_span_to_ast_span(&inner.as_span(), input),
      },
      Rule::occur_exact | Rule::occur_range => {
        // Parse the occurrence range
        let occur_str = inner.as_str();

        // Check if this is a simple "n*" pattern (n or more)
        if occur_str.ends_with('*') && !occur_str.contains("**") {
          let trimmed = occur_str.trim_end_matches('*').trim();
          if !trimmed.is_empty() && trimmed.parse::<usize>().is_ok() {
            // This is "n*" meaning "n or more"
            let lower = Some(trimmed.parse::<usize>().unwrap());
            return Ok(ast::Occurrence {
              occur: ast::Occur::Exact {
                lower,
                upper: None,
                #[cfg(feature = "ast-span")]
                span: pest_span_to_ast_span(&inner.as_span(), input),
              },
              #[cfg(feature = "ast-comments")]
              comments: None,
              _a: core::marker::PhantomData,
            });
          }
        }

        // Handle range patterns "n*m" or "*m" or "n*"
        let parts: Vec<&str> = occur_str.split('*').collect();

        let lower = if !parts[0].is_empty() {
          Some(parts[0].trim().parse::<usize>().unwrap_or(0))
        } else {
          None
        };

        let upper = if parts.len() > 1 && !parts[1].trim().is_empty() {
          Some(parts[1].trim().parse::<usize>().unwrap_or(0))
        } else {
          None
        };

        ast::Occur::Exact {
          lower,
          upper,
          #[cfg(feature = "ast-span")]
          span: pest_span_to_ast_span(&inner.as_span(), input),
        }
      }
      _ => continue,
    };

    return Ok(ast::Occurrence {
      occur,
      #[cfg(feature = "ast-comments")]
      comments: None,
      _a: core::marker::PhantomData,
    });
  }

  Err(Error::PARSER {
    #[cfg(feature = "ast-span")]
    position: Position::default(),
    msg: ErrorMsg {
      short: "Invalid occurrence indicator".to_string(),
      extended: None,
    },
  })
}

/// Convert member key (simple version - operators handled in group_entry)
#[cfg(feature = "ast-span")]
fn convert_member_key_simple<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
  is_arrow_map: bool,
  is_cut: bool,
  span: ast::Span,
) -> Result<ast::MemberKey<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::type1 => {
        // type1 form (RFC 8610 §3.5.1): "type1 S [\"^\" S] \"=>\"".
        // Always an arrow-map member key; ignore is_arrow_map flag here.
        let t1 = convert_type1(inner, input)?;
        return Ok(ast::MemberKey::Type1 {
          t1: Box::new(t1),
          is_cut,
          #[cfg(feature = "ast-span")]
          span,
          #[cfg(feature = "ast-comments")]
          comments_before_cut: None,
          #[cfg(feature = "ast-comments")]
          comments_after_cut: None,
          #[cfg(feature = "ast-comments")]
          comments_after_arrowmap: None,
        });
      }
      Rule::bareword => {
        if is_arrow_map {
          // Convert bareword to Type1 for arrow map
          let type1 = ast::Type1 {
            type2: ast::Type2::Typename {
              ident: ast::Identifier {
                ident: inner.as_str(),
                socket: None,
                #[cfg(feature = "ast-span")]
                span: pest_span_to_ast_span(&inner.as_span(), input),
              },
              generic_args: None,
              #[cfg(feature = "ast-span")]
              span: pest_span_to_ast_span(&inner.as_span(), input),
            },
            operator: None,
            #[cfg(feature = "ast-span")]
            span: pest_span_to_ast_span(&inner.as_span(), input),
            #[cfg(feature = "ast-comments")]
            comments_after_type: None,
          };
          return Ok(ast::MemberKey::Type1 {
            t1: Box::new(type1),
            is_cut,
            #[cfg(feature = "ast-span")]
            span,
            #[cfg(feature = "ast-comments")]
            comments_before_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_arrowmap: None,
          });
        } else {
          return Ok(ast::MemberKey::Bareword {
            ident: ast::Identifier {
              ident: inner.as_str(),
              socket: None,
              #[cfg(feature = "ast-span")]
              span: pest_span_to_ast_span(&inner.as_span(), input),
            },
            #[cfg(feature = "ast-span")]
            span,
            #[cfg(feature = "ast-comments")]
            comments: None,
            #[cfg(feature = "ast-comments")]
            comments_after_colon: None,
          });
        }
      }
      Rule::typename => {
        let ident = convert_identifier(inner.clone(), input, false)?;

        if is_arrow_map {
          // Convert typename to Type1 for arrow map
          let type1 = ast::Type1 {
            type2: ast::Type2::Typename {
              ident: ident.clone(),
              generic_args: None,
              #[cfg(feature = "ast-span")]
              span: pest_span_to_ast_span(&inner.as_span(), input),
            },
            operator: None,
            #[cfg(feature = "ast-span")]
            span: pest_span_to_ast_span(&inner.as_span(), input),
            #[cfg(feature = "ast-comments")]
            comments_after_type: None,
          };
          return Ok(ast::MemberKey::Type1 {
            t1: Box::new(type1),
            is_cut,
            #[cfg(feature = "ast-span")]
            span,
            #[cfg(feature = "ast-comments")]
            comments_before_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_arrowmap: None,
          });
        } else {
          return Ok(ast::MemberKey::Bareword {
            ident,
            #[cfg(feature = "ast-span")]
            span,
            #[cfg(feature = "ast-comments")]
            comments: None,
            #[cfg(feature = "ast-comments")]
            comments_after_colon: None,
          });
        }
      }
      Rule::value => {
        let value_type2 = convert_value_to_type2(
          inner.clone(),
          input,
          #[cfg(feature = "ast-span")]
          span,
        )?;

        if is_arrow_map {
          // For arrow maps, wrap the value as a Type1 member key
          let type1 = ast::Type1 {
            type2: value_type2,
            operator: None,
            #[cfg(feature = "ast-span")]
            span,
            #[cfg(feature = "ast-comments")]
            comments_after_type: None,
          };
          return Ok(ast::MemberKey::Type1 {
            t1: Box::new(type1),
            is_cut,
            #[cfg(feature = "ast-span")]
            span,
            #[cfg(feature = "ast-comments")]
            comments_before_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_arrowmap: None,
          });
        }

        // Extract Value from Type2 for colon member keys
        let value = match value_type2 {
          ast::Type2::IntValue { value, .. } => Value::INT(value),
          ast::Type2::UintValue { value, .. } => Value::UINT(value),
          ast::Type2::FloatValue { value, .. } => Value::FLOAT(value),
          ast::Type2::TextValue { value, .. } => Value::TEXT(value),
          _ => {
            return Err(Error::PARSER {
              #[cfg(feature = "ast-span")]
              position: pest_span_to_position(&inner.as_span(), input),
              msg: ErrorMsg {
                short: "Invalid member key value".to_string(),
                extended: None,
              },
            });
          }
        };

        return Ok(ast::MemberKey::Value {
          value,
          #[cfg(feature = "ast-span")]
          span,
          #[cfg(feature = "ast-comments")]
          comments: None,
          #[cfg(feature = "ast-comments")]
          comments_after_colon: None,
        });
      }
      _ => {}
    }
  }

  Err(Error::PARSER {
    #[cfg(feature = "ast-span")]
    position: Position::default(),
    msg: ErrorMsg {
      short: "Invalid member key".to_string(),
      extended: None,
    },
  })
}

#[cfg(not(feature = "ast-span"))]
fn convert_member_key_simple<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
  is_arrow_map: bool,
  is_cut: bool,
) -> Result<ast::MemberKey<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::type1 => {
        // type1 form (RFC 8610 §3.5.1): always an arrow-map member key.
        let t1 = convert_type1(inner, input)?;
        return Ok(ast::MemberKey::Type1 {
          t1: Box::new(t1),
          is_cut,
          #[cfg(feature = "ast-comments")]
          comments_before_cut: None,
          #[cfg(feature = "ast-comments")]
          comments_after_cut: None,
          #[cfg(feature = "ast-comments")]
          comments_after_arrowmap: None,
        });
      }
      Rule::bareword => {
        if is_arrow_map {
          // Convert bareword to Type1 for arrow map
          let type1 = ast::Type1 {
            type2: ast::Type2::Typename {
              ident: ast::Identifier {
                ident: inner.as_str(),
                socket: None,
              },
              generic_args: None,
            },
            operator: None,
            #[cfg(feature = "ast-comments")]
            comments_after_type: None,
          };
          return Ok(ast::MemberKey::Type1 {
            t1: Box::new(type1),
            is_cut,
            #[cfg(feature = "ast-comments")]
            comments_before_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_arrowmap: None,
          });
        } else {
          return Ok(ast::MemberKey::Bareword {
            ident: ast::Identifier {
              ident: inner.as_str(),
              socket: None,
            },
            #[cfg(feature = "ast-comments")]
            comments: None,
            #[cfg(feature = "ast-comments")]
            comments_after_colon: None,
          });
        }
      }
      Rule::typename => {
        let ident = convert_identifier(inner.clone(), input, false)?;

        if is_arrow_map {
          // Convert typename to Type1 for arrow map
          let type1 = ast::Type1 {
            type2: ast::Type2::Typename {
              ident: ident.clone(),
              generic_args: None,
            },
            operator: None,
            #[cfg(feature = "ast-comments")]
            comments_after_type: None,
          };
          return Ok(ast::MemberKey::Type1 {
            t1: Box::new(type1),
            is_cut,
            #[cfg(feature = "ast-comments")]
            comments_before_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_arrowmap: None,
          });
        } else {
          return Ok(ast::MemberKey::Bareword {
            ident,
            #[cfg(feature = "ast-comments")]
            comments: None,
            #[cfg(feature = "ast-comments")]
            comments_after_colon: None,
          });
        }
      }
      Rule::value => {
        let value_type2 = convert_value_to_type2(inner.clone(), input)?;

        if is_arrow_map {
          // For arrow maps, wrap the value as a Type1 member key
          let type1 = ast::Type1 {
            type2: value_type2,
            operator: None,
            #[cfg(feature = "ast-comments")]
            comments_after_type: None,
          };
          return Ok(ast::MemberKey::Type1 {
            t1: Box::new(type1),
            is_cut,
            #[cfg(feature = "ast-comments")]
            comments_before_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_cut: None,
            #[cfg(feature = "ast-comments")]
            comments_after_arrowmap: None,
          });
        }

        // Extract Value from Type2 for colon member keys
        let value = match value_type2 {
          ast::Type2::IntValue { value, .. } => Value::INT(value),
          ast::Type2::UintValue { value, .. } => Value::UINT(value),
          ast::Type2::FloatValue { value, .. } => Value::FLOAT(value),
          ast::Type2::TextValue { value, .. } => Value::TEXT(value),
          _ => {
            return Err(Error::PARSER {
              msg: ErrorMsg {
                short: "Invalid member key value".to_string(),
                extended: None,
              },
            });
          }
        };

        return Ok(ast::MemberKey::Value {
          value,
          #[cfg(feature = "ast-comments")]
          comments: None,
          #[cfg(feature = "ast-comments")]
          comments_after_colon: None,
        });
      }
      _ => {}
    }
  }

  Err(Error::PARSER {
    msg: ErrorMsg {
      short: "Invalid member key".to_string(),
      extended: None,
    },
  })
}

/// Convert member key (original - now unused but kept for reference)
fn _convert_member_key<'a>(
  pair: Pair<'a, Rule>,
  input: &'a str,
) -> Result<ast::MemberKey<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);

  // Member keys can be:
  // - bareword :
  // - typename :
  // - value :
  // - type1 =>

  let full_str = pair.as_str();

  if full_str.contains("=>") {
    // Type1 with arrow
    for inner in pair.into_inner() {
      if inner.as_rule() == Rule::type1 {
        return Ok(ast::MemberKey::Type1 {
          t1: Box::new(convert_type1(inner, input)?),
          is_cut: false,
          #[cfg(feature = "ast-span")]
          span,
          #[cfg(feature = "ast-comments")]
          comments_before_cut: None,
          #[cfg(feature = "ast-comments")]
          comments_after_cut: None,
          #[cfg(feature = "ast-comments")]
          comments_after_arrowmap: None,
        });
      }
    }
  } else if full_str.contains(":") {
    // Bareword or value with colon
    for inner in pair.into_inner() {
      match inner.as_rule() {
        Rule::bareword => {
          return Ok(ast::MemberKey::Bareword {
            ident: ast::Identifier {
              ident: inner.as_str(),
              socket: None,
              #[cfg(feature = "ast-span")]
              span: pest_span_to_ast_span(&inner.as_span(), input),
            },
            #[cfg(feature = "ast-span")]
            span,
            #[cfg(feature = "ast-comments")]
            comments: None,
            #[cfg(feature = "ast-comments")]
            comments_after_colon: None,
          });
        }
        Rule::typename => {
          return Ok(ast::MemberKey::Bareword {
            ident: convert_identifier(inner, input, false)?,
            #[cfg(feature = "ast-span")]
            span,
            #[cfg(feature = "ast-comments")]
            comments: None,
            #[cfg(feature = "ast-comments")]
            comments_after_colon: None,
          });
        }
        Rule::value => {
          // Convert value to Value enum
          let value_type2 = convert_value_to_type2(
            inner.clone(),
            input,
            #[cfg(feature = "ast-span")]
            span,
          )?;

          // Extract Value from Type2
          let value = match value_type2 {
            ast::Type2::IntValue { value, .. } => Value::INT(value),
            ast::Type2::UintValue { value, .. } => Value::UINT(value),
            ast::Type2::FloatValue { value, .. } => Value::FLOAT(value),
            ast::Type2::TextValue { value, .. } => Value::TEXT(value),
            _ => {
              return Err(Error::PARSER {
                #[cfg(feature = "ast-span")]
                position: pest_span_to_position(&inner.as_span(), input),
                msg: ErrorMsg {
                  short: "Invalid member key value".to_string(),
                  extended: None,
                },
              });
            }
          };

          return Ok(ast::MemberKey::Value {
            value,
            #[cfg(feature = "ast-span")]
            span,
            #[cfg(feature = "ast-comments")]
            comments: None,
            #[cfg(feature = "ast-comments")]
            comments_after_colon: None,
          });
        }
        _ => {}
      }
    }
  }

  Err(Error::PARSER {
    #[cfg(feature = "ast-span")]
    position: Position::default(),
    msg: ErrorMsg {
      short: "Invalid member key".to_string(),
      extended: None,
    },
  })
}

#[cfg(test)]
mod tests {
  use super::*;
  #[cfg(not(target_arch = "wasm32"))]
  use crate::cddl_from_str;

  // Tight-extent helpers: leaf/container spans are tight, while the enclosing
  // Type1/GroupEntry spans are greedy (they run to the next token's start).
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_tight_extent_helpers() {
    fn first_type<'a>(cddl: &'a ast::CDDL<'a>) -> &'a ast::Type<'a> {
      match &cddl.rules[0] {
        ast::Rule::Type { rule, .. } => &rule.value,
        _ => panic!("first rule is not a type rule"),
      }
    }
    fn first_entry<'a>(ty: &'a ast::Type<'a>) -> &'a ast::GroupEntry<'a> {
      match &ty.type_choices[0].type1.type2 {
        ast::Type2::Array { group, .. } | ast::Type2::Map { group, .. } => {
          &group.group_choices[0].group_entries[0].0
        }
        _ => panic!("first type choice is not an array or map"),
      }
    }

    let input = "a = 0\n";
    let cddl = cddl_from_pest_str(input).unwrap();
    let end = type2_span(&first_type(&cddl).type_choices[0].type1.type2).1;
    assert!(input[..end].ends_with('0'), "leaf end {:?}", &input[..end]);

    let input = "a = [0, bytes]\n";
    let cddl = cddl_from_pest_str(input).unwrap();
    let end = type2_span(&first_type(&cddl).type_choices[0].type1.type2).1;
    assert!(input[..end].ends_with(']'), "array end {:?}", &input[..end]);

    let input = "a = uint .size 4\n";
    let cddl = cddl_from_pest_str(input).unwrap();
    let end = type1_tight_end(&first_type(&cddl).type_choices[0].type1);
    assert!(input[..end].ends_with('4'), "ctlop end {:?}", &input[..end]);

    // tight at `bytes`, stopping before the trailing space (not greedy to the comma)
    let input = "a = [bytes , int]\n";
    let cddl = cddl_from_pest_str(input).unwrap();
    let tight = entry_tight_end(first_entry(first_type(&cddl)));
    assert!(
      input[..tight].ends_with("bytes"),
      "tgn end {:?}",
      &input[..tight]
    );
    assert!(
      input[tight..].starts_with(' '),
      "tight end should stop before trailing trivia, got {:?}",
      &input[tight..]
    );

    let input = "a = {x: int, y: int}\n";
    let cddl = cddl_from_pest_str(input).unwrap();
    let tight = entry_tight_end(first_entry(first_type(&cddl)));
    assert!(
      input[..tight].ends_with("int"),
      "vmk end {:?}",
      &input[..tight]
    );
  }

  // collect_comment_toks tags stand-alone comments `pure` and trailing ones not;
  // anchor collection emits GrpChoiceLeading only for genuine multi-`//` groups.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_collect_comment_toks_and_anchors() {
    let input = r#"multi = [
  ; @name A
  x: int //
  ; @name B
  y: int
]
single = {
  z: int ; @name C
}
"#;
    let pair = CddlParser::parse(Rule::cddl, input)
      .unwrap()
      .next()
      .unwrap();
    let toks = collect_comment_toks(&pair, input);
    assert_eq!(
      toks.len(),
      3,
      "texts: {:?}",
      toks.iter().map(|t| t.text).collect::<Vec<_>>()
    );
    let pure: Vec<&str> = toks.iter().filter(|t| t.pure).map(|t| t.text).collect();
    assert_eq!(pure, vec![" @name A", " @name B"]);
    assert!(
      toks.windows(2).all(|w| w[0].lo < w[1].lo),
      "comment tokens must be source-ordered"
    );

    // Exactly two GrpChoiceLeading anchors — one per choice of the multi-`//` group;
    // the single-choice `single` map emits none.
    let mut cddl = cddl_from_pest_str(input).unwrap();
    let mut grp_count = 0usize;
    visit_anchor_slots(&mut cddl.rules, input, |_, kind, _| {
      if kind == SlotKind::GrpChoiceLeading {
        grp_count += 1;
      }
    });
    assert_eq!(grp_count, 2, "GrpChoiceLeading only for multi-`//` groups");
  }

  // ---- shared helpers for the comment-merge tests ----
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  fn find_type<'a>(cddl: &'a ast::CDDL<'a>, name: &str) -> &'a ast::Type<'a> {
    cddl
      .rules
      .iter()
      .find_map(|r| match r {
        ast::Rule::Type { rule, .. } if rule.name.ident == name => Some(&rule.value),
        _ => None,
      })
      .unwrap_or_else(|| panic!("type rule `{}` not found", name))
  }

  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  fn array_or_map_group<'a>(ty: &'a ast::Type<'a>) -> &'a ast::Group<'a> {
    match &ty.type_choices[0].type1.type2 {
      ast::Type2::Array { group, .. } | ast::Type2::Map { group, .. } => group,
      _ => panic!("first type choice is not an array or map"),
    }
  }

  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  fn comment_first<'a>(c: &'a Option<ast::Comments<'a>>) -> Option<&'a str> {
    c.as_ref().and_then(|c| c.0.first().copied())
  }

  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  fn entry_trailing<'a>(g: &'a ast::Group<'a>, ci: usize, ei: usize) -> Option<&'a str> {
    match &g.group_choices[ci].group_entries[ei].0 {
      ast::GroupEntry::ValueMemberKey {
        trailing_comments, ..
      }
      | ast::GroupEntry::TypeGroupname {
        trailing_comments, ..
      } => comment_first(trailing_comments),
      _ => panic!("unexpected group entry kind"),
    }
  }

  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  fn rule_leading<'a>(cddl: &'a ast::CDDL<'a>, name: &str) -> Option<&'a str> {
    cddl
      .rules
      .iter()
      .find_map(|r| match r {
        ast::Rule::Type {
          rule,
          comments_before_rule,
          ..
        } if rule.name.ident == name => Some(comments_before_rule),
        ast::Rule::Group {
          rule,
          comments_before_rule,
          ..
        } if rule.name.ident == name => Some(comments_before_rule),
        _ => None,
      })
      .and_then(|c| comment_first(c))
  }

  // Drive the merge directly (the public API discards orphans).
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  fn run_merge<'a>(input: &'a str) -> (Vec<Vec<&'a str>>, Vec<CommentTok<'a>>) {
    let pair = CddlParser::parse(Rule::cddl, input)
      .unwrap()
      .next()
      .unwrap();
    let toks = collect_comment_toks(&pair, input);
    let mut cddl = cddl_from_pest_str(input).unwrap();
    let mut anchors: Vec<Anchor> = Vec::new();
    visit_anchor_slots(&mut cddl.rules, input, |pos, kind, _slot| {
      anchors.push(Anchor { pos, kind })
    });
    let containers = collect_container_extents(&cddl.rules);
    merge(&toks, &anchors, &containers)
  }

  // Write only TypeChoice.comments_after_type, never Type1's (so each hint emits once).
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_no_double_emit_on_type_choice_trailing() {
    let input = r#"
my_enum = 0 ; @name ValA
        / 1 ; @name ValB
        / 2 ; @name ValC
"#;
    let cddl = cddl_from_pest_str(input).unwrap();
    let my_enum = find_type(&cddl, "my_enum");
    for (i, want) in [" @name ValA", " @name ValB", " @name ValC"]
      .iter()
      .enumerate()
    {
      let tc = &my_enum.type_choices[i];
      assert_eq!(comment_first(&tc.comments_after_type), Some(*want));
      assert!(
        tc.type1.comments_after_type.is_none(),
        "Type1.comments_after_type must stay None"
      );
    }
    let s = cddl.to_string();
    for hint in ["@name ValA", "@name ValB", "@name ValC"] {
      assert_eq!(s.matches(hint).count(), 1, "{} count in {:?}", hint, s);
    }
  }

  // Trailing comment after an operator-bearing type1 (`uint .size 4`).
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_operator_bearing_trailing() {
    let cddl = cddl_from_pest_str("m = uint .size 4 ; @name n\n").unwrap();
    assert_eq!(
      comment_first(&find_type(&cddl, "m").type_choices[0].comments_after_type),
      Some(" @name n")
    );
  }

  // A comment before the first alternative is rule-level, not choice-level.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_first_choice_leading_is_rule_level() {
    let cddl = cddl_from_pest_str(
      r#"
; doc
foo = 0 / 1
"#,
    )
    .unwrap();
    assert_eq!(rule_leading(&cddl, "foo"), Some(" doc"));
    assert!(find_type(&cddl, "foo").type_choices[0]
      .comments_before_type
      .is_none());
  }

  // a leading comment before a NON-first type-choice alternative
  // populates that choice's `comments_before_type` (ChoiceLeading, emitted for i>0).
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_leading_comment_on_nonfirst_type_choice() {
    let cddl = cddl_from_pest_str(
      r#"
t = int
  ; doc
  / text
"#,
    )
    .unwrap();
    let t = find_type(&cddl, "t");
    assert_eq!(
      comment_first(&t.type_choices[1].comments_before_type),
      Some(" doc")
    );
    assert!(t.type_choices[0].comments_before_type.is_none());
  }

  // Stacked leading comments accumulate; a blank line breaks the run (drop).
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_leading_accumulation_and_blank_line_break() {
    let cddl = cddl_from_pest_str(
      r#"
; first
; second
foo = 0
"#,
    )
    .unwrap();
    let lead = cddl.rules.iter().find_map(|r| match r {
      ast::Rule::Type {
        rule,
        comments_before_rule,
        ..
      } if rule.name.ident == "foo" => Some(comments_before_rule.as_ref()),
      _ => None,
    });
    assert_eq!(
      lead.flatten().map(|c| c.0.clone()),
      Some(vec![" first", " second"])
    );
    let cddl2 = cddl_from_pest_str(
      r#"
; orphan

bar = 0
"#,
    )
    .unwrap();
    assert_eq!(rule_leading(&cddl2, "bar"), None);
  }

  // Consume-once: a hint after `]` binds the array choice, not the inner entries.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_no_duplication_array_choice_trailing() {
    let cddl = cddl_from_pest_str("block = [0, bytes] ; @name w\n").unwrap();
    let block = find_type(&cddl, "block");
    assert_eq!(
      comment_first(&block.type_choices[0].comments_after_type),
      Some(" @name w")
    );
    let g = array_or_map_group(block);
    assert_eq!(entry_trailing(g, 0, 0), None);
    assert_eq!(entry_trailing(g, 0, 1), None);
  }

  // Round-trip preserves trailing hints (3-choice form, one alt per line).
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_round_trip_trailing_hints() {
    let input = r#"
my_enum = 0 ; @name ValA
        / 1 ; @name ValB
        / 2 ; @name ValC
"#;
    let cddl = cddl_from_pest_str(input).unwrap();
    let emitted = cddl.to_string();
    let reparsed = cddl_from_pest_str(&emitted).unwrap();
    let my_enum = find_type(&reparsed, "my_enum");
    for (i, want) in [" @name ValA", " @name ValB", " @name ValC"]
      .iter()
      .enumerate()
    {
      assert_eq!(
        comment_first(&my_enum.type_choices[i].comments_after_type),
        Some(*want),
        "lost choice {} in {:?}",
        i,
        emitted
      );
    }
  }

  // Tie-break: a control-op entry binds the ENTRY trailing slot, not the inner type-choice.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_entry_trailing_tiebreak() {
    let cddl = cddl_from_pest_str(
      r#"
m = {
  x: uint .size 4 ; @name n
}
"#,
    )
    .unwrap();
    let g = array_or_map_group(find_type(&cddl, "m"));
    assert_eq!(entry_trailing(g, 0, 0), Some(" @name n"));
    match &g.group_choices[0].group_entries[0].0 {
      ast::GroupEntry::ValueMemberKey { ge, .. } => {
        assert!(ge.entry_type.type_choices[0].comments_after_type.is_none());
      }
      _ => panic!("expected ValueMemberKey"),
    }
  }

  // Enclosing guard: a pure comment after the last entry before `]` does not
  // leak forward onto the next rule's leading slot.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_enclosing_guard_no_forward_leak() {
    let input = r#"
a = [ int
  ; dangling
]
b = 0
"#;
    let cddl = cddl_from_pest_str(input).unwrap();
    assert_eq!(rule_leading(&cddl, "b"), None);
    let (_assigned, orphans) = run_merge(input);
    assert!(orphans.iter().any(|o| o.text == " dangling"));
  }

  // entry_tight_end on an inline `( ... )` group is tight at `)`.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_inline_group_tight_end() {
    let cddl = cddl_from_pest_str("a = [ (int, text), bytes ]\n").unwrap();
    let g = array_or_map_group(find_type(&cddl, "a"));
    let entry = &g.group_choices[0].group_entries[0].0;
    assert!(
      matches!(entry, ast::GroupEntry::InlineGroup { .. }),
      "expected inline group entry"
    );
    let input = "a = [ (int, text), bytes ]\n";
    let end = entry_tight_end(entry);
    assert!(
      input[..end].ends_with(')'),
      "inline group end {:?}",
      &input[..end]
    );
  }

  // Group-choice discriminator: single-choice -> entry leading; multi-choice -> grpchoice.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_single_vs_multi_group_choice_leading() {
    let single = cddl_from_pest_str(
      r#"
g = {
  ; @name First
  a: int, b: int
}
"#,
    )
    .unwrap();
    let g = array_or_map_group(find_type(&single, "g"));
    assert!(g.group_choices[0].comments_before_grpchoice.is_none());
    match &g.group_choices[0].group_entries[0].0 {
      ast::GroupEntry::ValueMemberKey {
        leading_comments, ..
      } => {
        assert_eq!(comment_first(leading_comments), Some(" @name First"))
      }
      _ => panic!("expected ValueMemberKey"),
    }
    let multi = cddl_from_pest_str(
      r#"
h = [
  ; @name X
  tag: int //
  b: int
]
"#,
    )
    .unwrap();
    let hg = array_or_map_group(find_type(&multi, "h"));
    assert_eq!(
      comment_first(&hg.group_choices[0].comments_before_grpchoice),
      Some(" @name X")
    );
    match &hg.group_choices[0].group_entries[0].0 {
      ast::GroupEntry::ValueMemberKey {
        leading_comments, ..
      } => assert!(leading_comments.is_none()),
      _ => panic!("expected ValueMemberKey"),
    }
  }

  // Orphans never panic at the public boundary; a normal parse has none.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_orphan_non_panic() {
    let weird = r#"
x = {
  foo ; c
  : int
}
"#;
    assert!(cddl_from_pest_str(weird).is_ok(), "must not panic/error");
    let (_assigned, orphans) = run_merge(weird);
    assert!(!orphans.is_empty(), "the stray comment should orphan");
    let (_assigned2, orphans2) = run_merge("y = 0 ; @name k\n");
    assert!(orphans2.is_empty(), "unexpected orphans: {:?}", orphans2);
  }

  // a trailing hint on a BARE value/typename member binds to the
  // entry's `trailing_comments`, not the inner type-choice's `comments_after_type`.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_bare_value_member_trailing() {
    let cddl = cddl_from_pest_str(
      r#"
suits = [
  0, ; @name hearts
  1 ; @name spades
]
"#,
    )
    .unwrap();
    let g = array_or_map_group(find_type(&cddl, "suits"));
    assert_eq!(entry_trailing(g, 0, 0), Some(" @name hearts"));
    assert_eq!(entry_trailing(g, 0, 1), Some(" @name spades"));
    for ei in 0..2 {
      if let ast::GroupEntry::ValueMemberKey { ge, .. } = &g.group_choices[0].group_entries[ei].0 {
        assert!(
          ge.entry_type.type_choices[0].comments_after_type.is_none(),
          "inner choice must not claim the hint"
        );
      }
    }
  }

  // the enclosing guard uses the INNERMOST container, so a
  // comment dangling inside an inner array does not leak onto a sibling outer entry.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_enclosing_guard_innermost_container() {
    let input = r#"
x = [
  a: [ int
  ; @name X
  ], b: int
]
y = 0
"#;
    let cddl = cddl_from_pest_str(input).unwrap();
    let g = array_or_map_group(find_type(&cddl, "x"));
    for (ge, _) in &g.group_choices[0].group_entries {
      if let ast::GroupEntry::ValueMemberKey {
        leading_comments, ..
      } = ge
      {
        assert!(
          leading_comments.is_none(),
          "comment leaked onto an outer-array entry"
        );
      }
    }
    let (_assigned, orphans) = run_merge(input);
    assert!(
      orphans.iter().any(|o| o.text == " @name X"),
      "escaping comment should orphan"
    );
  }

  // Trailing hints on bare-typename type-choice alternatives (`t = foo / bar`):
  // each alternative keeps its own hint. The Type2::Typename span is greedy, so
  // without keying off the tight identifier the hints would swap between choices.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_trailing_comment_on_typename_type_choice() {
    let input = r#"
t = foo ; @name a
  / bar ; @name b
foo = int
bar = int
"#;
    let cddl = cddl_from_pest_str(input).unwrap();
    let t = find_type(&cddl, "t");
    assert_eq!(choice_trailing(t, 0), Some(" @name a"));
    assert_eq!(choice_trailing(t, 1), Some(" @name b"));
    assert_eq!(rule_leading(&cddl, "foo"), None);
    assert_eq!(rule_leading(&cddl, "bar"), None);
  }

  // A trailing hint on a single bare-typename type (a type alias) binds to that
  // type and does not leak forward onto the following rule's leading comment.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_trailing_comment_on_typename_no_forward_leak() {
    let input = r#"
device = phone ; @name Phone
phone = 0
"#;
    let cddl = cddl_from_pest_str(input).unwrap();
    assert_eq!(
      choice_trailing(find_type(&cddl, "device"), 0),
      Some(" @name Phone")
    );
    assert_eq!(rule_leading(&cddl, "phone"), None);
  }

  #[test]
  fn test_basic_type_rule() {
    let input = "myrule = int\n";
    let result = cddl_from_pest_str(input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

    let cddl = result.unwrap();
    assert_eq!(cddl.rules.len(), 1);
  }

  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_ast_comments_populated_for_rule_and_nested_entries() {
    let input = "; A person.\n; identity info.\nperson = {\n  ; full name\n  name: tstr, ; trailing name\n  address: {\n    ; street line\n    street: tstr,\n  },\n}\n";
    let cddl = cddl_from_pest_str(input).unwrap();

    let rule = &cddl.rules[0];
    let before = rule
      .comments_before_rule()
      .expect("rule should have leading comments");
    assert_eq!(before.0, vec![" A person.", " identity info."]);

    let ast::Rule::Type { rule, .. } = rule else {
      panic!("expected type rule");
    };

    let ast::Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("expected map");
    };

    let entries = &group.group_choices[0].group_entries;

    // First entry: leading + trailing comments.
    let ast::GroupEntry::ValueMemberKey {
      leading_comments,
      trailing_comments,
      ..
    } = &entries[0].0
    else {
      panic!("expected value member key");
    };
    assert_eq!(
      leading_comments.as_ref().map(|c| c.0.clone()),
      Some(vec![" full name"])
    );
    assert_eq!(
      trailing_comments.as_ref().map(|c| c.0.clone()),
      Some(vec![" trailing name"])
    );

    // Second entry is a nested map; its inner entry carries a leading comment,
    // demonstrating comments nested inside CDDL structures are preserved in the
    // AST.
    let ast::GroupEntry::ValueMemberKey { ge, .. } = &entries[1].0 else {
      panic!("expected value member key");
    };
    let ast::Type2::Map { group: inner, .. } = &ge.entry_type.type_choices[0].type1.type2 else {
      panic!("expected nested map");
    };
    let ast::GroupEntry::ValueMemberKey {
      leading_comments, ..
    } = &inner.group_choices[0].group_entries[0].0
    else {
      panic!("expected nested value member key");
    };
    assert_eq!(
      leading_comments.as_ref().map(|c| c.0.clone()),
      Some(vec![" street line"])
    );
  }

  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_ast_comments_roundtrip_reparses() {
    // A commented map must format back into valid, re-parseable CDDL.
    let input = "person = {\n  ; full name\n  name: tstr, ; trailing\n  age: uint,\n}\n";
    let cddl = cddl_from_pest_str(input).unwrap();
    let formatted = cddl.to_string();
    assert!(
      cddl_from_pest_str(&formatted).is_ok(),
      "formatted output should re-parse:\n{}",
      formatted
    );
    assert!(formatted.contains("; full name"));
    assert!(formatted.contains("; trailing"));
  }

  #[test]
  fn test_simple_struct() {
    let input = "person = { name: tstr, age: uint }\n";
    let result = cddl_from_pest_str(input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_type_choice() {
    let input = "value = int / text / bool\n";
    let result = cddl_from_pest_str(input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  // Per-construct tests that every `; @name X` hint is retrievable from the AST node
  // it documents. Each uses a minimal self-contained fixture (real Cardano patterns).

  // Trailing comment on the type-choice alternative at `idx`.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  fn choice_trailing<'a>(ty: &'a ast::Type<'a>, idx: usize) -> Option<&'a str> {
    comment_first(&ty.type_choices[idx].comments_after_type)
  }

  // Construct: trailing hints on scalar type-choice alternatives.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_trailing_comment_on_scalar_type_choice() {
    let cddl = cddl_from_pest_str(
      r#"my_enum = 0 ; @name first
        / 1 ; @name second
        / 2 ; @name third
"#,
    )
    .unwrap();
    let my_enum = find_type(&cddl, "my_enum");
    assert_eq!(choice_trailing(my_enum, 0), Some(" @name first"));
    assert_eq!(choice_trailing(my_enum, 1), Some(" @name second"));
    assert_eq!(choice_trailing(my_enum, 2), Some(" @name third"));
  }

  // Construct: trailing hints on array-valued type-choice alternatives (after `]`).
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_trailing_comment_on_array_type_choice() {
    let cddl = cddl_from_pest_str(
      r#"my_choice = [0, bytes] ; @name first
            / [1, bytes] ; @name second
"#,
    )
    .unwrap();
    let my_choice = find_type(&cddl, "my_choice");
    assert_eq!(choice_trailing(my_choice, 0), Some(" @name first"));
    assert_eq!(choice_trailing(my_choice, 1), Some(" @name second"));
  }

  // Construct: trailing hints on map entries (occurrence + member key + trailing comma).
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_trailing_comment_on_map_entry() {
    let cddl = cddl_from_pest_str(
      r#"my_map = {
  ? 0 : int, ; @name first
  ? 2 : tstr, ; @name second
}
"#,
    )
    .unwrap();
    let my_map = array_or_map_group(find_type(&cddl, "my_map"));
    assert_eq!(entry_trailing(my_map, 0, 0), Some(" @name first"));
    assert_eq!(entry_trailing(my_map, 0, 1), Some(" @name second"));
  }

  // Construct: trailing hints on bare-typename (TypeGroupname) array entries in
  // leading-comma layout — each entry's greedy span runs to the next line's comma,
  // so the trailing comment must key off the tight identifier end.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_trailing_comment_on_array_entry() {
    let cddl = cddl_from_pest_str(
      r#"my_array = [ bytes ; @name first
             , bytes ; @name second
             ]
"#,
    )
    .unwrap();
    let my_array = array_or_map_group(find_type(&cddl, "my_array"));
    assert_eq!(entry_trailing(my_array, 0, 0), Some(" @name first"));
    assert_eq!(entry_trailing(my_array, 0, 1), Some(" @name second"));
  }

  // Construct: leading hints before group choices / `//` alternatives.
  #[cfg(all(feature = "ast-comments", feature = "ast-span"))]
  #[test]
  fn test_leading_comment_on_group_choice() {
    let cddl = cddl_from_pest_str(
      r#"my_group = [
  ; @name first
  x: 0, int //
  ; @name second
  y: 1
]
"#,
    )
    .unwrap();
    let my_group = array_or_map_group(find_type(&cddl, "my_group"));
    assert_eq!(
      comment_first(&my_group.group_choices[0].comments_before_grpchoice),
      Some(" @name first")
    );
    assert_eq!(
      comment_first(&my_group.group_choices[1].comments_before_grpchoice),
      Some(" @name second")
    );
  }

  #[test]
  fn test_generic() {
    let input = r#"map<K, V> = { * K => V }
my-map = map<text, int>
"#;
    let result = cddl_from_pest_str(input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[cfg(not(target_arch = "wasm32"))]
  #[test]
  fn test_pest_vs_existing_parser() {
    // Test that Pest parser produces compatible AST
    let input = r#"
person = {
  name: tstr,
  age: uint
}
"#;

    // Parse with existing parser (now delegates to Pest)
    let existing_result = cddl_from_str(input, true);
    assert!(
      existing_result.is_ok(),
      "Existing parser failed: {:?}",
      existing_result.err()
    );

    // Parse with Pest parser directly
    let pest_result = cddl_from_pest_str(input);
    assert!(
      pest_result.is_ok(),
      "Pest parser failed: {:?}",
      pest_result.err()
    );

    // Both should produce 1 rule
    let existing_cddl = existing_result.unwrap();
    let pest_cddl = pest_result.unwrap();
    assert_eq!(existing_cddl.rules.len(), pest_cddl.rules.len());
  }

  #[test]
  fn test_range_operator() {
    let input = "port = 0..65535\n";
    let result = cddl_from_pest_str(input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_array() {
    let input = "my-array = [* int]\n";
    let result = cddl_from_pest_str(input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_occurrence_indicators() {
    let input = r#"
optional-field = { ? name: tstr }
zero-or-more = { * items: int }
one-or-more = { + values: text }
"#;
    let result = cddl_from_pest_str(input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_comments() {
    let input = r#"
; This is a comment
person = {
  name: tstr,  ; person's name
  age: uint    ; person's age
}
"#;
    let result = cddl_from_pest_str(input);
    assert!(
      result.is_ok(),
      "Failed to parse with comments: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_error_reporting() {
    // Test that errors are properly converted with enhanced messages
    let input = "invalid syntax @#$";
    let result = cddl_from_pest_str(input);
    assert!(result.is_err(), "Should fail on invalid syntax");

    if let Err(e) = result {
      // Verify error contains useful information
      let error_str = format!("{:?}", e);
      // Check that the error contains the enhanced user-friendly message
      assert!(
        error_str.contains("expected") || error_str.contains("assignment"),
        "Error should contain user-friendly message about expectations, got: {}",
        error_str
      );
    }
  }

  #[test]
  fn test_enhanced_error_messages() {
    // Test various error scenarios to ensure enhanced messages are working

    // Missing assignment operator
    let input = "myrule";
    if let Err(e) = cddl_from_pest_str(input) {
      let error_str = format!("{:?}", e);
      assert!(
        error_str.contains("assignment"),
        "Error should mention assignment, got: {}",
        error_str
      );
      assert!(
        error_str.contains("Hint"),
        "Error should include a hint, got: {}",
        error_str
      );
    }

    // Missing value after assignment
    let input = "myrule = ";
    if let Err(e) = cddl_from_pest_str(input) {
      let error_str = format!("{:?}", e);
      assert!(
        error_str.contains("type value") || error_str.contains("group entry"),
        "Error should mention expected elements, got: {}",
        error_str
      );
    }

    // Invalid syntax
    let input = "x = !!invalid!!";
    if let Err(e) = cddl_from_pest_str(input) {
      let error_str = format!("{:?}", e);
      assert!(
        error_str.contains("expected"),
        "Error should mention expectations, got: {}",
        error_str
      );
    }
  }

  #[test]
  fn test_error_position_tracking() {
    // Test that position information is correctly preserved
    let input = "myrule = \n  invalid @#$";
    if let Err(e) = cddl_from_pest_str(input) {
      let error_str = format!("{:?}", e);
      // Should have line 2 information
      assert!(
        error_str.contains("line: 2"),
        "Error should have correct line number, got: {}",
        error_str
      );
    }
  }

  #[test]
  fn test_undefined_reference_error() {
    let input = "X = {\n  a: UnknownType,\n}\n";
    let result = cddl_from_pest_str_checked(input);
    assert!(result.is_err(), "Expected error for undefined reference");
    let err = result.unwrap_err().to_string();
    assert!(
      err.contains("missing definition for rule UnknownType"),
      "Expected 'missing definition for rule UnknownType', got: {}",
      err
    );
  }

  #[test]
  fn test_prelude_types_not_flagged() {
    let input = "X = { a: int, b: tstr, c: bool }\n";
    let result = cddl_from_pest_str_checked(input);
    assert!(
      result.is_ok(),
      "Prelude types should not be flagged: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_defined_rule_not_flagged() {
    let input = "MyType = int\nX = { a: MyType }\n";
    let result = cddl_from_pest_str_checked(input);
    assert!(
      result.is_ok(),
      "Defined rules should not be flagged: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_generic_params_not_flagged() {
    let input = "container<T> = { value: T }\n";
    let result = cddl_from_pest_str_checked(input);
    assert!(
      result.is_ok(),
      "Generic params should not be flagged: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_socket_plug_not_flagged() {
    let input = "X = { a: $my-socket }\n";
    let result = cddl_from_pest_str_checked(input);
    assert!(
      result.is_ok(),
      "Socket/plug references should not be flagged: {:?}",
      result.err()
    );
  }

  #[cfg(not(target_arch = "wasm32"))]
  #[test]
  fn test_from_slice_undefined_reference() {
    let cddl_input = "X = {\n  a: UnknownType,\n}\n";
    let result = ast::CDDL::from_slice(cddl_input.as_bytes());
    assert!(
      result.is_err(),
      "CDDL::from_slice should return error for undefined reference"
    );
    let err = result.unwrap_err();
    assert!(
      err.contains("missing definition for rule UnknownType"),
      "Expected 'missing definition for rule UnknownType', got: {}",
      err
    );
  }
}

#[cfg(test)]
mod wasm_compat_tests {
  use super::*;

  #[test]
  fn test_error_msg_serialization_compat() {
    // Test that ErrorMsg structure is compatible with serialization
    let input = "invalid syntax @#$";
    let result = cddl_from_pest_str(input);

    assert!(result.is_err(), "Should fail on invalid syntax");

    if let Err(Error::PARSER { msg, .. }) = result {
      // Verify both fields are present
      assert!(!msg.short.is_empty(), "Short message should not be empty");
      assert!(
        msg.extended.is_some(),
        "Extended message should be present for enhanced errors"
      );

      // Verify the structure can be cloned (required for WASM serialization)
      let _cloned = msg.clone();

      // Verify Display trait works
      let display_str = msg.to_string();
      assert!(
        !display_str.is_empty(),
        "Display should produce non-empty string"
      );
    }
  }

  #[cfg(target_arch = "wasm32")]
  #[test]
  fn test_wasm_error_serialization() {
    use serde::Serialize;

    let msg = ErrorMsg {
      short: "test error".to_string(),
      extended: Some("extended details".to_string()),
    };

    // Verify ErrorMsg can be serialized (this is what WASM needs)
    #[derive(Serialize)]
    struct TestError {
      msg: ErrorMsg,
    }

    let test_error = TestError { msg };

    // This should not panic if serialization works
    let _serialized = serde_json::to_string(&test_error).expect("Should serialize");
  }

  // =========================================================================
  // RFC 9682 tests
  // =========================================================================

  #[test]
  fn test_rfc9682_empty_cddl() {
    // RFC 9682 Section 3.1: Empty CDDL is valid
    let input = "";
    let result = cddl_from_pest_str(input);
    assert!(result.is_ok(), "Empty CDDL should parse successfully");
    let cddl = result.unwrap();
    assert!(cddl.rules.is_empty(), "Empty CDDL should have no rules");
  }

  #[test]
  fn test_rfc9682_comment_only_cddl() {
    let input = "; just a comment\n";
    let result = cddl_from_pest_str(input);
    assert!(
      result.is_ok(),
      "Comment-only CDDL should parse successfully"
    );
  }

  #[test]
  fn test_rfc9682_unicode_brace_escape_parsing() {
    // RFC 9682 Section 2.1.1: \u{hex} escape sequences
    let input = r#"a = "D\u{6f}mino""#;
    let result = cddl_from_pest_str(input);
    assert!(
      result.is_ok(),
      "\\u{{hex}} escape should parse: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_rfc9682_surrogate_pair_parsing() {
    // RFC 9682 / RFC 8610: \uHHHH\uLLLL surrogate pair for U+1F073
    let input = r#"b = "test \uD83C\uDC73""#;
    let result = cddl_from_pest_str(input);
    assert!(
      result.is_ok(),
      "Surrogate pair escape should parse: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_unescape_rfc9682_brace_form() {
    // Test that \u{6f} produces 'o'
    let unescaped = unescape_text(r#"D\u{6f}mino"#);
    assert_eq!(unescaped, "Domino");
  }

  #[test]
  fn test_unescape_rfc9682_brace_form_large_codepoint() {
    // Test that \u{1F073} produces 🁳
    let unescaped = unescape_text(r#"\u{1F073}"#);
    assert_eq!(unescaped, "\u{1F073}");
  }

  #[test]
  fn test_unescape_rfc9682_brace_form_with_leading_zeros() {
    // Test that \u{006f} produces 'o'
    let unescaped = unescape_text(r#"\u{006f}"#);
    assert_eq!(unescaped, "o");
  }

  #[test]
  fn test_unescape_surrogate_pair() {
    // Test that \uD83C\uDC73 produces 🁳 (U+1F073)
    let unescaped = unescape_text(r#"\uD83C\uDC73"#);
    assert_eq!(unescaped, "\u{1F073}");
  }

  #[test]
  fn test_unescape_standard_4digit() {
    // Test that \u2318 produces ⌘
    let unescaped = unescape_text(r#"\u2318"#);
    assert_eq!(unescaped, "⌘");
  }

  #[test]
  fn test_rfc9682_tag_type_expression() {
    // RFC 9682 Section 3.2: #6.<type>(content)
    let input = r#"ct-tag<content> = #6.<ct-tag-number>(content)
ct-tag-number = 1668546817..1668612095"#;
    let result = cddl_from_pest_str(input);
    assert!(
      result.is_ok(),
      "Non-literal tag number should parse: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_rfc9682_simple_value_type_expression() {
    // RFC 9682 Section 3.2: #7.<head-number>
    let input = "my-simple = #7.<0..23>";
    let result = cddl_from_pest_str(input);
    assert!(result.is_ok(), "#7.<type> should parse: {:?}", result.err());
  }

  #[test]
  #[cfg(feature = "ast-span")]
  fn test_error_position_for_invalid_second_rule() {
    // When the input has a valid first rule and an invalid second "rule"
    // (just a bare identifier with no assignment), the error position should
    // point to the invalid token, not to the first rule.
    let input = "value = 0x1\n\nbreak";
    let result = cddl_from_pest_str(input);
    assert!(result.is_err(), "Should fail to parse");
    if let Err(crate::parser::Error::PARSER { position, msg, .. }) = result {
      // The error should point to "break" on line 3
      assert_eq!(
        position.line, 3,
        "Error should be on line 3 (at 'break'), not line {}. Message: {}",
        position.line, msg.short
      );
      assert_eq!(
        position.column, 1,
        "Error should start at column 1 (start of 'break'), not column {}",
        position.column
      );
      // The range should cover "break" (bytes 13..18)
      assert_eq!(
        position.range,
        (13, 18),
        "Range should cover 'break' (13..18), got {:?}",
        position.range
      );
    }
  }
}
