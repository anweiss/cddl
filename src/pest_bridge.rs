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

use std::borrow::Cow;
use std::collections::HashMap;

/// Convert a Pest error to the existing parser error format with enhanced messages
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

  // Create enhanced error message with user-friendly rule names
  let (short_msg, extended_msg) = create_enhanced_error_message(&error, input);

  Error::PARSER {
    #[cfg(feature = "ast-span")]
    position: Position {
      line,
      column,
      range: (index, index),
      index,
    },
    msg: ErrorMsg {
      short: short_msg,
      extended: extended_msg,
    },
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

/// Parse CDDL from string using Pest parser and convert to AST
pub fn cddl_from_pest_str<'a>(input: &'a str) -> Result<ast::CDDL<'a>, Error> {
  let pairs = CddlParser::parse(Rule::cddl, input).map_err(|e| convert_pest_error(e, input))?;

  convert_cddl(pairs, input)
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

  for inner_pair in pair.into_inner() {
    match inner_pair.as_rule() {
      Rule::rule => {
        rules.push(convert_rule(inner_pair, input)?);
      }
      Rule::EOI => break,
      _ => {}
    }
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
        let val = hexf_parse::parse_hexf64(inner.as_str(), false).map_err(|_| Error::PARSER {
          position: pest_span_to_position(&inner.as_span(), input),
          msg: ErrorMsg {
            short: "Invalid hexfloat".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::FloatValue { value: val, span });
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
  input: &'a str,
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
        let val = hexf_parse::parse_hexf64(inner.as_str(), false).map_err(|_| Error::PARSER {
          msg: ErrorMsg {
            short: "Invalid hexfloat".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::FloatValue { value: val });
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
  input: &'a str,
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
          _a: std::marker::PhantomData,
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
              _a: std::marker::PhantomData,
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
      _a: std::marker::PhantomData,
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

  #[test]
  fn test_basic_type_rule() {
    let input = "myrule = int\n";
    let result = cddl_from_pest_str(input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

    let cddl = result.unwrap();
    assert_eq!(cddl.rules.len(), 1);
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
}
