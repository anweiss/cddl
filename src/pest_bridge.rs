//! Bridge layer between Pest parser and existing AST
//!
//! This module provides conversion functions to transform Pest parse trees into the existing
//! AST structure, ensuring API compatibility while leveraging Pest's parsing capabilities.

use crate::{
  ast,
  error::ErrorMsg,
  lexer::Position,
  parser::Error,
  pest_parser::{CddlParser, Rule},
  token::{lookup_control_from_str, ControlOperator, SocketPlug, Value},
};

use pest::{
  iterators::{Pair, Pairs},
  Parser as PestParser, Span as PestSpan,
};

#[cfg(feature = "std")]
use std::borrow::Cow;

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::Cow,
  boxed::Box,
  string::{String, ToString},
  vec::Vec,
};

/// Convert a Pest error to the existing parser error format
pub fn convert_pest_error(error: pest::error::Error<Rule>, _input: &str) -> Error {
  let (line, column, index) = match error.line_col {
    pest::error::LineColLocation::Pos((line, col)) => (line, col, 0),
    pest::error::LineColLocation::Span((line, col), _) => (line, col, 0),
  };

  Error::PARSER {
    #[cfg(feature = "ast-span")]
    position: Position {
      line,
      column,
      range: (index, index),
      index,
    },
    msg: ErrorMsg {
      short: format!("Pest parsing error: {}", error),
      extended: None,
    },
  }
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
  
  for (_idx, ch) in input[..start].chars().enumerate() {
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
  let pairs = CddlParser::parse(Rule::cddl, input)
    .map_err(|e| convert_pest_error(e, input))?;
  
  convert_cddl(pairs, input)
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
fn convert_identifier<'a>(pair: Pair<'a, Rule>, input: &'a str, _is_group: bool) -> Result<ast::Identifier<'a>, Error> {
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
fn convert_generic_params<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::GenericParams<'a>, Error> {
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
fn convert_generic_args<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::GenericArgs<'a>, Error> {
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
        let is_inclusive = inner.clone().into_inner().any(|p| p.as_rule() == Rule::range_op_inclusive);
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
      let type2_pairs: Vec<_> = pairs_vec.iter().filter(|p| p.as_rule() == Rule::type2).collect();
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
fn convert_control_operator<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ControlOperator, Error> {
  for inner in pair.into_inner() {
    if inner.as_rule() == Rule::control_name {
      let ctrl_str = inner.as_str();
      return lookup_control_from_str(ctrl_str).ok_or_else(|| Error::PARSER {
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
  
  // Clone the pair to allow multiple iterations
  let pair_clone = pair.clone();
  
  for inner in pair_clone.into_inner() {
    match inner.as_rule() {
      Rule::value => {
        return convert_value_to_type2(inner, input, span);
      }
      Rule::typename => {
        let ident = convert_identifier(inner.clone(), input, false)?;
        let generic_args = None;
        
        // Check if there are generic args (they would be siblings in the parent)
        // We need to re-examine the parent's children
        return Ok(ast::Type2::Typename {
          ident,
          generic_args,
          #[cfg(feature = "ast-span")]
          span,
        });
      }
      Rule::generic_args => {
        // This will be handled together with typename
      }
      Rule::type_expr => {
        // Parenthesized type
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
      Rule::group => {
        // Check parent context to determine if this is a map or array
        // For now, we'll treat it as a map
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
      Rule::tag_expr => {
        return convert_tag_expr(inner, input);
      }
      _ => {}
    }
  }
  
  // Handle typename with generic_args by checking all children
  let mut typename_ident = None;
  let mut generic_args = None;
  
  for inner in pair.clone().into_inner() {
    match inner.as_rule() {
      Rule::typename => {
        typename_ident = Some(convert_identifier(inner, input, false)?);
      }
      Rule::generic_args => {
        generic_args = Some(convert_generic_args(inner, input)?);
      }
      _ => {}
    }
  }
  
  if let Some(ident) = typename_ident {
    return Ok(ast::Type2::Typename {
      ident,
      generic_args,
      #[cfg(feature = "ast-span")]
      span,
    });
  }
  
  // Default to Any if we can't determine the type
  Ok(ast::Type2::Any {
    #[cfg(feature = "ast-span")]
    span,
  })
}

/// Convert value to Type2
#[cfg(feature = "ast-span")]
fn convert_value_to_type2<'a>(pair: Pair<'a, Rule>, input: &'a str, span: ast::Span) -> Result<ast::Type2<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::number => {
        return convert_number_to_type2(inner, input, span);
      }
      Rule::text_value => {
        let text = inner.as_str();
        // Remove quotes
        let text_content = &text[1..text.len()-1];
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
fn convert_value_to_type2<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::Type2<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::number => {
        return convert_number_to_type2(inner, input);
      }
      Rule::text_value => {
        let text = inner.as_str();
        // Remove quotes
        let text_content = &text[1..text.len()-1];
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

/// Unescape text value
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
          '/' => result.push('/'),
          'b' => result.push('\u{0008}'),
          'f' => result.push('\u{000C}'),
          'u' => {
            // Unicode escape sequence
            let hex: String = chars.by_ref().take(4).collect();
            if let Ok(code_point) = u32::from_str_radix(&hex, 16) {
              if let Some(unicode_char) = char::from_u32(code_point) {
                result.push(unicode_char);
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
fn convert_number_to_type2<'a>(pair: Pair<'a, Rule>, input: &'a str, span: ast::Span) -> Result<ast::Type2<'a>, Error> {
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
fn convert_number_to_type2<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::Type2<'a>, Error> {
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

/// Convert bytes value to Type2
#[cfg(feature = "ast-span")]
fn convert_bytes_value_to_type2<'a>(pair: Pair<'a, Rule>, input: &'a str, span: ast::Span) -> Result<ast::Type2<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::bytes_b64 => {
        let bytes_str = inner.as_str();
        // Remove quotes
        let content = &bytes_str[1..bytes_str.len()-1];
        let decoded = data_encoding::BASE64.decode(content.as_bytes()).map_err(|_| Error::PARSER {
          position: pest_span_to_position(&inner.as_span(), input),
          msg: ErrorMsg {
            short: "Invalid base64 encoding".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::B64ByteString {
          value: Cow::Owned(decoded),
          span,
        });
      }
      Rule::bytes_b16 => {
        let bytes_str = inner.as_str();
        // Remove h' and '
        let content = &bytes_str[2..bytes_str.len()-1];
        let cleaned: String = content.chars().filter(|c| !c.is_whitespace()).collect();
        let decoded = data_encoding::HEXUPPER.decode(cleaned.as_bytes()).map_err(|_| Error::PARSER {
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
        let content = &bytes_str[2..bytes_str.len()-1];
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
fn convert_bytes_value_to_type2<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::Type2<'a>, Error> {
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::bytes_b64 => {
        let bytes_str = inner.as_str();
        // Remove quotes
        let content = &bytes_str[1..bytes_str.len()-1];
        let decoded = data_encoding::BASE64.decode(content.as_bytes()).map_err(|_| Error::PARSER {
          msg: ErrorMsg {
            short: "Invalid base64 encoding".to_string(),
            extended: None,
          },
        })?;
        return Ok(ast::Type2::B64ByteString {
          value: Cow::Owned(decoded),
        });
      }
      Rule::bytes_b16 => {
        let bytes_str = inner.as_str();
        // Remove h' and '
        let content = &bytes_str[2..bytes_str.len()-1];
        let cleaned: String = content.chars().filter(|c| !c.is_whitespace()).collect();
        let decoded = data_encoding::HEXUPPER.decode(cleaned.as_bytes()).map_err(|_| Error::PARSER {
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
        let content = &bytes_str[2..bytes_str.len()-1];
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
  // #6.32(tstr) - tagged data with literal tag
  // #6.<typename> - tagged data with type constraint
  // # - any
  
  let full_str = pair.as_str();
  
  if full_str == "#" {
    return Ok(ast::Type2::Any {
      #[cfg(feature = "ast-span")]
      span,
    });
  }
  
  // Parse tag expressions - this is complex, so we'll do a basic implementation
  // A full implementation would parse the internal structure
  Ok(ast::Type2::Any {
    #[cfg(feature = "ast-span")]
    span,
  })
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
fn convert_group_choice<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::GroupChoice<'a>, Error> {
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
fn convert_group_entry<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::GroupEntry<'a>, Error> {
  #[cfg(feature = "ast-span")]
  let span = pest_span_to_ast_span(&pair.as_span(), input);
  
  let mut occur = None;
  let mut member_key = None;
  let mut entry_type = None;
  let mut groupname_ident = None;
  let mut generic_args = None;
  let mut inline_group = None;
  
  for inner in pair.into_inner() {
    match inner.as_rule() {
      Rule::occur => {
        occur = Some(convert_occurrence(inner, input)?);
      }
      Rule::member_key => {
        member_key = Some(convert_member_key(inner, input)?);
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
fn convert_occurrence<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::Occurrence<'a>, Error> {
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
        let parts: Vec<&str> = occur_str.split('*').collect();
        
        let lower = if !parts[0].is_empty() {
          Some(parts[0].parse::<usize>().unwrap_or(0))
        } else {
          None
        };
        
        let upper = if parts.len() > 1 && !parts[1].is_empty() {
          Some(parts[1].parse::<usize>().unwrap_or(0))
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

/// Convert member key
fn convert_member_key<'a>(pair: Pair<'a, Rule>, input: &'a str) -> Result<ast::MemberKey<'a>, Error> {
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
}
