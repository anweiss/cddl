use super::{
  ast::*,
  lexer::{self, Lexer, LexerError, Position},
  token::{self, Token},
};
#[cfg(feature = "std")]
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{
  diagnostic::{Diagnostic, Label},
  files::SimpleFiles,
  term,
};

#[cfg(feature = "std")]
use std::borrow::Cow;

use std::{cmp::Ordering, fmt, mem, result};

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::{Cow, ToOwned},
  boxed::Box,
  string::{String, ToString},
  vec::Vec,
};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

/// Alias for `Result` with an error of type `cddl::ParserError`
pub type Result<T> = result::Result<T, Error>;

/// Parser type
pub struct Parser<'a, I>
where
  I: Iterator<Item = lexer::Item<'a>>,
{
  tokens: I,
  str_input: &'a str,
  cur_token: Token<'a>,
  peek_token: Token<'a>,
  lexer_position: Position,
  peek_lexer_position: Position,
  parser_position: Position,
  /// Vec of collected parsing errors
  pub errors: Vec<ParserError>,
}

/// Parsing error types
#[derive(Debug)]
pub enum Error {
  /// Parsing errors
  CDDL(String),
  /// Parsing error occurred. Used with the `print_errors()` method
  PARSER,
  /// Lexing error
  LEXER(LexerError),
  /// Regex error
  REGEX(regex::Error),
}

/// Parser error information and position
#[derive(Debug)]
pub struct ParserError {
  position: Position,
  message: String,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Error::CDDL(e) => write!(f, "{}", e),
      Error::PARSER => write!(f, "Parser error"),
      Error::LEXER(e) => write!(f, "{}", e),
      Error::REGEX(e) => write!(f, "{}", e),
    }
  }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Error::LEXER(le) => Some(le),
      Error::REGEX(re) => Some(re),
      _ => None,
    }
  }
}

impl<'a, I> Parser<'a, I>
where
  I: Iterator<Item = lexer::Item<'a>>,
{
  /// Create a new `Parser` from a given str input and iterator over
  /// `lexer::Item`.
  ///
  /// # Example
  ///
  /// ```
  /// use cddl::parser::Parser;
  /// use cddl::lexer::Lexer;
  ///
  /// let input = r#"mycddl = ( int / float )"#;
  /// let p = Parser::new(Lexer::new(input).iter(), input);
  /// ```
  pub fn new(tokens: I, str_input: &'a str) -> Result<Parser<I>> {
    let mut p = Parser {
      tokens,
      str_input,
      cur_token: Token::EOF,
      peek_token: Token::EOF,
      errors: Vec::default(),
      lexer_position: Position::default(),
      peek_lexer_position: Position::default(),
      parser_position: Position::default(),
    };

    p.next_token()?;
    p.next_token()?;

    Ok(p)
  }

  /// Print parser errors if there are any. Used with the `Error::PARSER`
  /// variant
  ///
  /// # Arguments
  ///
  /// * `to_stderr` - When true, outputs formatted errors to stderr
  ///
  /// # Example
  ///
  /// ```
  /// use cddl::parser::{Error, Parser};
  /// use cddl::lexer::Lexer;
  ///
  /// let input = r#"mycddl = ( int / float )"#;
  /// if let Ok(mut p) = Parser::new(Lexer::new(input).iter(), input) {
  ///   if let Err(Error::PARSER) = p.parse_cddl() {
  ///     let _ = p.report_errors(true);
  ///   }
  /// }
  /// ```
  #[cfg(feature = "std")]
  pub fn report_errors(
    &self,
    to_stderr: bool,
  ) -> std::result::Result<Option<String>, Box<dyn std::error::Error>> {
    if self.errors.is_empty() {
      return Ok(None);
    }

    let mut files = SimpleFiles::new();

    let file_id = files.add("input", self.str_input);

    let mut labels = Vec::new();
    for error in self.errors.iter() {
      labels.push(
        Label::primary(file_id, error.position.range.0..error.position.range.1)
          .with_message(error.message.to_string()),
      );
    }

    let diagnostic = Diagnostic::error()
      .with_message("parser errors")
      .with_labels(labels);

    let config = term::Config::default();

    if to_stderr {
      let writer = StandardStream::stderr(ColorChoice::Auto);
      // TODO: Use `map_or_else()` once it is determined this crate should set
      // its minimum version to 1.41
      match term::emit(&mut writer.lock(), &config, &files, &diagnostic) {
        Ok(_) => return Ok(None),
        Err(e) => return Err(Box::from(e)),
      };
    }

    let mut buffer = Vec::new();
    let mut writer = term::termcolor::NoColor::new(&mut buffer);

    term::emit(&mut writer, &config, &files, &diagnostic)?;

    Ok(Some(String::from_utf8(buffer)?))
  }

  /// Print parser errors if there are any. Used with the `Error::PARSER`
  /// variant
  ///
  /// # Example
  ///
  /// ```
  /// use cddl::parser::{Error, Parser};
  /// use cddl::lexer::Lexer;
  ///
  /// let input = r#"mycddl = ( int / float )"#;
  /// if let Ok(mut p) = Parser::new(Lexer::new(input).iter(), input) {
  ///   if let Err(Error::PARSER) = p.parse_cddl() {
  ///     let _ = p.report_errors();
  ///   }
  /// }
  /// ```
  #[cfg(not(feature = "std"))]
  pub fn report_errors(&self) -> Option<String> {
    if self.errors.is_empty() {
      return None;
    }

    let mut files = SimpleFiles::new();

    let file_id = files.add("input", self.str_input);

    let mut labels = Vec::new();
    for error in self.errors.iter() {
      labels.push(
        Label::primary(file_id, error.position.range.0..error.position.range.1)
          .with_message(error.message.to_string()),
      );
    }

    let diagnostic = Diagnostic::error()
      .with_message("parser errors")
      .with_labels(labels);

    let config = term::Config::default();

    let mut buffer = Vec::new();
    let mut writer = term::termcolor::NoColor::new(&mut buffer);

    term::emit(&mut writer, &config, &files, &diagnostic).ok()?;

    Some(String::from_utf8(buffer).ok()?)
  }

  fn next_token(&mut self) -> Result<()> {
    mem::swap(&mut self.cur_token, &mut self.peek_token);
    mem::swap(&mut self.lexer_position, &mut self.peek_lexer_position);

    if let Some(next_token) = self.tokens.next() {
      let nt = next_token.map_err(Error::LEXER)?;
      self.peek_lexer_position = nt.0;
      self.peek_token = nt.1;
    }

    Ok(())
  }

  fn advance_to_next_rule(&mut self) -> Result<()> {
    let mut is_possible_rule = false;

    while !is_possible_rule {
      self.next_token()?;
      if let Token::IDENT(_) = self.cur_token {
        match self.peek_token {
          Token::ASSIGN | Token::TCHOICEALT | Token::GCHOICEALT => is_possible_rule = true,
          _ => continue,
        }
      } else if let Token::EOF = self.cur_token {
        is_possible_rule = true;
      }
    }

    Ok(())
  }

  /// Parses into a `CDDL` AST
  pub fn parse_cddl(&mut self) -> Result<CDDL<'a>> {
    let mut c = CDDL::default();

    while self.cur_token != Token::EOF {
      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }
      match self.parse_rule() {
        Ok(r) => {
          let rule_exists = |existing_rule: &Rule| {
            r.name() == existing_rule.name() && !existing_rule.is_choice_alternate()
          };
          if c.rules.iter().any(rule_exists) {
            self.parser_position.range = (r.span().0, r.span().1);
            self.parser_position.line = r.span().2;

            self.errors.push(ParserError {
              position: self.parser_position,
              message: format!("Rule with name '{}' already defined", r.name()),
            });

            continue;
          }

          c.rules.push(r);

          while let Token::COMMENT(_) = self.cur_token {
            self.next_token()?;
          }
        }
        Err(Error::PARSER) => {
          if !self.cur_token_is(Token::EOF) {
            self.advance_to_next_rule()?;
          }
        }
        _ => continue,
      }
    }

    // TODO: implement second pass over parenthesized type rules whose contents
    // are Type2::Typename, and if the identifier refers to another group rule
    // per the match rules in Appendix C, refactor rule into a group rule:
    //
    // "A rule defines a name for a type expression (production "type") or for a
    // group expression (production "grpent"), with the intention that the
    // semantics does not change when the name is replaced by its (parenthesized
    // if needed) definition.  Note that whether the name defined by a rule
    // stands for a type or a group isn't always determined by syntax alone:
    // e.g., "a = b" can make "a" a type if "b" is a type, or a group if "b" is
    // a group.  More subtly, in "a = (b)", "a" may be used as a type if "b" is
    // a type, or as a group both when "b" is a group and when "b" is a type (a
    // good convention to make the latter case stand out to the human reader is
    // to write "a = (b,)")."
    if !self.errors.is_empty() {
      return Err(Error::PARSER);
    }

    Ok(c)
  }

  fn parse_rule(&mut self) -> Result<Rule<'a>> {
    let begin_rule_range = self.lexer_position.range.0;
    let begin_rule_line = self.lexer_position.line;

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    let ident = match &self.cur_token {
      Token::IDENT(i) => self.identifier_from_ident_token(*i),
      _ => {
        self.parser_position.range = self.lexer_position.range;
        self.parser_position.line = self.lexer_position.line;

        self.errors.push(ParserError {
          position: self.parser_position,
          message: format!("expected rule identifier. Got '{}'", self.cur_token),
        });

        return Err(Error::PARSER);
      }
    };

    let gp = if self.peek_token_is(&Token::LANGLEBRACKET) {
      self.next_token()?;

      Some(self.parse_genericparm()?)
    } else {
      None
    };

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    if !self.expect_peek(&Token::ASSIGN)?
      && !self.expect_peek(&Token::TCHOICEALT)?
      && !self.expect_peek(&Token::GCHOICEALT)?
    {
      self.parser_position.range = (begin_rule_range, self.lexer_position.range.1);
      self.parser_position.line = self.lexer_position.line;

      self.errors.push(ParserError {
        position: self.parser_position,
        message: format!(
          "Expected assignment '=', '/=' or '//=' after identifier {}",
          self.cur_token
        ),
      });

      return Err(Error::PARSER);
    }

    let mut is_type_choice_alternate = false;
    let mut is_group_choice_alternate = false;

    if self.cur_token_is(Token::TCHOICEALT) {
      is_type_choice_alternate = true;
    } else if self.cur_token_is(Token::GCHOICEALT) {
      is_group_choice_alternate = true;
    }

    self.next_token()?;

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    match self.cur_token {
      // Check for an occurrence indicator of uint followed by an asterisk '*'
      Token::VALUE(token::Value::UINT(_)) => {
        if self.peek_token_is(&Token::ASTERISK) {
          let ge = self.parse_grpent(true)?;

          let span = (
            begin_rule_range,
            self.parser_position.range.1,
            begin_rule_line,
          );

          Ok(Rule::Group {
            rule: Box::from(GroupRule {
              name: ident,
              generic_param: gp,
              is_group_choice_alternate,
              entry: ge,
            }),
            span,
          })
        } else {
          let t = self.parse_type(None)?;
          let span = (
            begin_rule_range,
            self.parser_position.range.1,
            begin_rule_line,
          );
          Ok(Rule::Type {
            rule: TypeRule {
              name: ident,
              generic_param: gp,
              is_type_choice_alternate,
              value: t,
            },
            span,
          })
        }
      }
      Token::LPAREN | Token::ASTERISK | Token::ONEORMORE | Token::OPTIONAL => {
        let begin_pt_range = self.lexer_position.range.0;

        let ge = self.parse_grpent(true)?;

        let mut end_rule_range = self.parser_position.range.1;

        // If a group entry is an inline group with no leading occurrence
        // indicator, and its group has only a single element that is not
        // preceded by an occurrence indicator nor member key, treat it as a
        // parenthesized type, subsequently parsing the remaining type and
        // returning the type rule. This is one of the few situations where
        // `clone` is required
        if let GroupEntry::InlineGroup {
          occur: None, group, ..
        } = &ge
        {
          if group.group_choices.len() == 1 {
            if let Some(gc) = group.group_choices.get(0) {
              if gc.group_entries.len() == 1 {
                if let Some(ge) = gc.group_entries.get(0) {
                  // Check that there is no trailing comma
                  if !ge.1 {
                    // TODO: Replace with box pattern destructuring once supported in stable
                    if let GroupEntry::ValueMemberKey { ge, .. } = &ge.0 {
                      if ge.occur.is_none() && ge.member_key.is_none() {
                        let value = self.parse_type(Some(Type2::ParenthesizedType {
                          pt: ge.entry_type.clone(),
                          span: (
                            begin_pt_range,
                            self.parser_position.range.1,
                            begin_rule_line,
                          ),
                        }))?;

                        end_rule_range = self.parser_position.range.1;

                        return Ok(Rule::Type {
                          rule: TypeRule {
                            name: ident,
                            generic_param: gp,
                            is_type_choice_alternate,
                            value,
                          },
                          span: (begin_rule_range, end_rule_range, begin_rule_line),
                        });
                      }
                    }
                  }
                }
              }
            }
          }
        }

        Ok(Rule::Group {
          rule: Box::from(GroupRule {
            name: ident,
            generic_param: gp,
            is_group_choice_alternate,
            entry: ge,
          }),
          span: (begin_rule_range, end_rule_range, begin_rule_line),
        })
      }
      _ => {
        let t = self.parse_type(None)?;

        let span = (
          begin_rule_range,
          self.parser_position.range.1,
          begin_rule_line,
        );

        Ok(Rule::Type {
          rule: TypeRule {
            name: ident,
            generic_param: gp,
            is_type_choice_alternate,
            value: t,
          },
          span,
        })
      }
    }
  }

  fn parse_genericparm(&mut self) -> Result<GenericParm<'a>> {
    let begin_range = self.lexer_position.range.0;

    if self.cur_token_is(Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    let mut generic_params = GenericParm::default();

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      match &self.cur_token {
        Token::IDENT(ident) => {
          generic_params
            .params
            .push(self.identifier_from_ident_token(*ident));
          self.next_token()?;

          if !self.cur_token_is(Token::COMMA) && !self.cur_token_is(Token::RANGLEBRACKET) {
            self.parser_position.range = (begin_range + 1, self.peek_lexer_position.range.0);
            self.parser_position.line = self.lexer_position.line;

            self.errors.push(ParserError {
              position: self.parser_position,
              message: "Generic parameters should be between angle brackets '<' and '>' and separated by a comma ','"
                .into(),
            });

            return Err(Error::PARSER);
          }
        }
        Token::COMMA => self.next_token()?,
        Token::COMMENT(_) => self.next_token()?,
        Token::VALUE(_) => {
          self.parser_position.range = (self.lexer_position.range.0, self.lexer_position.range.1);
          self.parser_position.line = self.lexer_position.line;

          self.errors.push(ParserError {
            position: self.parser_position,
            message: "Generic parameters must be named identifiers".into(),
          });

          return Err(Error::PARSER);
        }
        _ => {
          self.parser_position.range = (begin_range, self.lexer_position.range.0);
          self.parser_position.line = self.lexer_position.line;

          self.errors.push(ParserError {
            position: self.parser_position,
            message: "Generic parameters should be between angle brackets '<' and '>' and separated by a comma ','".into()
          });

          return Err(Error::PARSER);
        }
      }
    }

    // Since generic params are only found after the identifier of a rule, don't
    // advance beyond the closing '>' to retain the expect_peek semantics for
    // '=', '/=' and '//='

    let end_range = self.lexer_position.range.1;
    generic_params.span = (begin_range, end_range, self.lexer_position.line);

    Ok(generic_params)
  }

  fn parse_genericarg(&mut self) -> Result<GenericArg<'a>> {
    if self.peek_token_is(&Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    let begin_generic_arg_range = self.lexer_position.range.0;
    let begin_generic_arg_line = self.lexer_position.line;

    // Required for type2 mutual recursion
    if self.cur_token_is(Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    let mut generic_args = GenericArg::default();

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      generic_args.args.push(self.parse_type1(None)?);
      if let Token::COMMA | Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      if self.cur_token_is(Token::EOF) {
        self.errors.push(ParserError {
          position: self.parser_position,
          message: "Missing closing \">\"".into(),
        });

        return Err(Error::PARSER);
      }
    }

    if self.cur_token_is(Token::RANGLEBRACKET) {
      self.parser_position.range.1 = self.lexer_position.range.1;
      self.next_token()?;
    }

    generic_args.span = (
      begin_generic_arg_range,
      self.parser_position.range.1,
      begin_generic_arg_line,
    );

    Ok(generic_args)
  }

  fn parse_type(&mut self, parenthesized_type: Option<Type2<'a>>) -> Result<Type<'a>> {
    self.parser_position.range = self.lexer_position.range;
    self.parser_position.line = self.lexer_position.line;

    let begin_type_range = if let Some(Type2::ParenthesizedType { span, .. }) = parenthesized_type {
      self.parser_position.line = span.2;

      span.0
    } else {
      self.parser_position.range.0
    };

    let mut t = Type {
      type_choices: Vec::new(),
      span: (begin_type_range, 0, self.parser_position.line),
    };

    t.type_choices.push(self.parse_type1(parenthesized_type)?);

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    while self.cur_token_is(Token::TCHOICE) {
      self.next_token()?;

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      t.type_choices.push(self.parse_type1(None)?);
    }

    t.span.1 = self.parser_position.range.1;

    Ok(t)
  }

  fn parse_type1(&mut self, parenthesized_type: Option<Type2<'a>>) -> Result<Type1<'a>> {
    let mut begin_type1_line = self.lexer_position.line;
    let begin_type1_range = self.lexer_position.range.0;

    let (t2_1, begin_type1_range) =
      if let Some(Type2::ParenthesizedType { pt, span }) = parenthesized_type {
        begin_type1_line = span.2;

        (Type2::ParenthesizedType { pt, span }, span.0)
      } else {
        (self.parse_type2()?, begin_type1_range)
      };

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    let mut span = (
      begin_type1_range,
      self.lexer_position.range.1,
      begin_type1_line,
    );

    let op = match &self.cur_token {
      Token::RANGEOP(i) => {
        span.0 = self.lexer_position.range.0;

        Some(RangeCtlOp::RangeOp {
          is_inclusive: *i,
          span,
        })
      }
      _ => match token::control_str_from_token(&self.cur_token) {
        Some(ctrl) => {
          span.0 = self.lexer_position.range.0;

          Some(RangeCtlOp::CtlOp { ctrl, span })
        }
        None => None,
      },
    };

    span = (
      begin_type1_range,
      self.parser_position.range.1,
      begin_type1_line,
    );

    match op {
      Some(o) => {
        self.next_token()?;

        let t2 = self.parse_type2()?;

        span.1 = self.parser_position.range.1;

        Ok(Type1 {
          type2: t2_1,
          operator: Some((o, t2)),
          span,
        })
      }
      None => Ok(Type1 {
        type2: t2_1,
        operator: None,
        span,
      }),
    }
  }

  fn parse_type2(&mut self) -> Result<Type2<'a>> {
    let t2 = match &self.cur_token {
      // value
      Token::VALUE(value) => {
        self.parser_position.range = self.lexer_position.range;
        self.parser_position.line = self.lexer_position.line;

        let span = (
          self.parser_position.range.0,
          self.parser_position.range.1,
          self.parser_position.line,
        );

        match value {
          token::Value::TEXT(t) => Ok(Type2::TextValue { value: t, span }),
          token::Value::INT(i) => Ok(Type2::IntValue { value: *i, span }),
          token::Value::UINT(ui) => Ok(Type2::UintValue { value: *ui, span }),
          token::Value::FLOAT(f) => Ok(Type2::FloatValue { value: *f, span }),
          token::Value::BYTE(token::ByteValue::UTF8(Cow::Borrowed(utf8))) => {
            Ok(Type2::UTF8ByteString {
              value: Cow::Borrowed(utf8),
              span,
            })
          }
          token::Value::BYTE(token::ByteValue::UTF8(Cow::Owned(utf8))) => {
            Ok(Type2::UTF8ByteString {
              value: Cow::Owned(utf8.to_owned()),
              span,
            })
          }
          token::Value::BYTE(token::ByteValue::B16(Cow::Borrowed(b16))) => {
            Ok(Type2::B16ByteString {
              value: Cow::Borrowed(b16),
              span,
            })
          }
          token::Value::BYTE(token::ByteValue::B16(Cow::Owned(b16))) => Ok(Type2::B16ByteString {
            value: Cow::Owned(b16.to_owned()),
            span,
          }),
          token::Value::BYTE(token::ByteValue::B64(Cow::Borrowed(b64))) => {
            Ok(Type2::B64ByteString {
              value: Cow::Borrowed(b64),
              span,
            })
          }
          token::Value::BYTE(token::ByteValue::B64(Cow::Owned(b64))) => Ok(Type2::B64ByteString {
            value: Cow::Owned(b64.to_owned()),
            span,
          }),
        }
      }

      // typename [genericarg]
      Token::IDENT(ident) => {
        let begin_type2_range = self.lexer_position.range.0;
        let begin_type2_line = self.lexer_position.line;

        // optional genericarg detected
        if self.peek_token_is(&Token::LANGLEBRACKET) {
          let ident = self.identifier_from_ident_token(*ident);
          let ga = self.parse_genericarg()?;

          let end_type2_range = self.parser_position.range.1;

          return Ok(Type2::Typename {
            ident,
            generic_arg: Some(ga),
            span: (begin_type2_range, end_type2_range, begin_type2_line),
          });
        }

        self.parser_position.range = self.lexer_position.range;
        self.parser_position.line = self.lexer_position.line;

        Ok(Type2::Typename {
          ident: self.identifier_from_ident_token(*ident),
          generic_arg: None,
          span: (
            self.parser_position.range.0,
            self.parser_position.range.1,
            self.parser_position.line,
          ),
        })
      }

      // ( type )
      Token::LPAREN => {
        let begin_type2_range = self.lexer_position.range.0;
        let begin_type2_line = self.lexer_position.line;

        self.next_token()?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        let pt = self.parse_type(None)?;

        self.parser_position.range.0 = begin_type2_range;
        self.parser_position.range.1 = self.lexer_position.range.1;
        self.parser_position.line = begin_type2_line;

        Ok(Type2::ParenthesizedType {
          pt,
          span: (
            self.parser_position.range.0,
            self.parser_position.range.1,
            self.parser_position.line,
          ),
        })
      }

      // { group }
      Token::LBRACE => {
        let begin_type2_range = self.lexer_position.range.0;
        let begin_type2_line = self.lexer_position.line;

        let group = self.parse_group()?;

        Ok(Type2::Map {
          group,
          span: (
            begin_type2_range,
            self.lexer_position.range.1,
            begin_type2_line,
          ),
        })
      }

      // [ group ]
      Token::LBRACKET => {
        let begin_type2_range = self.lexer_position.range.0;
        let begin_type2_line = self.lexer_position.line;

        let group = self.parse_group()?;

        Ok(Type2::Array {
          group,
          span: (
            begin_type2_range,
            self.lexer_position.range.1,
            begin_type2_line,
          ),
        })
      }

      // ~ typename [genericarg]
      Token::UNWRAP => {
        self.next_token()?;

        if let Token::IDENT(ident) = &self.cur_token {
          let ident = self.identifier_from_ident_token(*ident);

          if self.peek_token_is(&Token::LANGLEBRACKET) {
            self.next_token()?;

            return Ok(Type2::Unwrap {
              ident,
              generic_arg: Some(self.parse_genericarg()?),
              span: (0, 0, 0),
            });
          }

          return Ok(Type2::Unwrap {
            ident,
            generic_arg: None,
            span: (0, 0, 0),
          });
        }

        self.errors.push(ParserError {
          position: self.parser_position,
          message: "Invalid unwrap syntax".into(),
        });

        Err(Error::PARSER)
      }

      // & ( group )
      // & groupname [genericarg]
      Token::GTOCHOICE => {
        let begin_type2_range = self.lexer_position.range.0;
        let begin_type2_line = self.lexer_position.line;

        self.next_token()?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        match &self.cur_token {
          Token::LPAREN => {
            self.next_token()?;

            let group = self.parse_group()?;

            Ok(Type2::ChoiceFromInlineGroup {
              group,
              span: (
                begin_type2_range,
                self.parser_position.range.1,
                begin_type2_line,
              ),
            })
          }
          Token::IDENT(ident) => {
            let ident = self.identifier_from_ident_token(*ident);
            if self.peek_token_is(&Token::LANGLEBRACKET) {
              self.next_token()?;

              let generic_arg = Some(self.parse_genericarg()?);

              return Ok(Type2::ChoiceFromGroup {
                ident,
                generic_arg,
                span: (
                  begin_type2_range,
                  self.parser_position.range.1,
                  begin_type2_line,
                ),
              });
            }

            self.parser_position.range.1 = self.lexer_position.range.1;

            Ok(Type2::ChoiceFromGroup {
              ident,
              generic_arg: None,
              span: (
                begin_type2_range,
                self.parser_position.range.1,
                begin_type2_line,
              ),
            })
          }
          _ => {
            self.errors.push(ParserError {
              position: self.parser_position,
              message: "Invalid group to choice enumeration syntax".into(),
            });
            Err(Error::PARSER)
          }
        }
      }

      // # 6 ["." uint] ( type )
      // # DIGIT ["." uint]   ; major/ai
      // #                    ; any
      // Token::TAG(tag) => match tag {
      //   Tag::DATA(data) => Ok(Type2::TaggedData(data.clone())),
      //   Tag::MAJORTYPE(mt) => Ok(Type2::TaggedDataMajorType(*mt)),
      //   Tag::ANY => Ok(Type2::Any),
      // },
      Token::TAG(t) => {
        let begin_type2_range = self.lexer_position.range.0;
        let begin_type2_line = self.lexer_position.line;

        match *t {
          // Tagged data item containing the given type as the tagged value
          (Some(6), tag) => {
            self.next_token()?;
            if !self.cur_token_is(Token::LPAREN) {
              self.errors.push(ParserError {
                position: self.parser_position,
                message: format!("Malformed tag. Unknown token: {:#?}", self.cur_token),
              });

              return Err(Error::PARSER);
            }

            self.next_token()?;

            let t = self.parse_type(None)?;

            if !self.cur_token_is(Token::RPAREN) {
              self.errors.push(ParserError {
                position: self.parser_position,
                message: format!("Malformed tag. Unknown token: {:#?}", self.cur_token),
              });

              return Err(Error::PARSER);
            }

            Ok(Type2::TaggedData {
              tag,
              t,
              span: (
                begin_type2_range,
                self.parser_position.range.1,
                begin_type2_line,
              ),
            })
          }
          // Tagged data of a major type
          (Some(mt), constraint) => Ok(Type2::TaggedDataMajorType {
            mt,
            constraint,
            span: (
              begin_type2_range,
              self.lexer_position.range.1,
              begin_type2_line,
            ),
          }),
          _ => Ok(Type2::Any((
            begin_type2_range,
            self.lexer_position.range.1,
            begin_type2_line,
          ))),
        }
      }
      _ => {
        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        match self.cur_token.in_standard_prelude() {
          Some(s) => {
            let ident = self.identifier_from_ident_token((s, None));
            self.parser_position.range = self.lexer_position.range;
            self.parser_position.line = self.lexer_position.line;

            Ok(Type2::Typename {
              ident,
              generic_arg: None,
              span: (
                self.parser_position.range.0,
                self.parser_position.range.1,
                self.parser_position.line,
              ),
            })
          }
          None => {
            self.parser_position.line = self.lexer_position.line;
            self.parser_position.range = self.lexer_position.range;

            self.errors.push(ParserError {
              position: self.parser_position,
              message: "Entry type syntax error".into(),
            });

            Err(Error::PARSER)
          }
        }
      }
    };

    self.parser_position.range.1 = self.lexer_position.range.1;

    self.next_token()?;

    t2
  }

  fn parse_group(&mut self) -> Result<Group<'a>> {
    let begin_group_range = if self.cur_token_is(Token::LBRACE)
      || self.cur_token_is(Token::LPAREN)
      || self.cur_token_is(Token::LBRACKET)
      || self.cur_token_is(Token::GCHOICE)
    {
      self.peek_lexer_position.range.0
    } else {
      self.lexer_position.range.0
    };

    let closing_delimiter = token::closing_delimiter(&self.cur_token);

    let mut group = Group {
      group_choices: Vec::new(),
      span: (begin_group_range, 0, self.lexer_position.line),
    };

    group.group_choices.push(self.parse_grpchoice()?);

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    while self.cur_token_is(Token::GCHOICE) {
      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      group.group_choices.push(self.parse_grpchoice()?);
    }

    group.span.1 = self.parser_position.range.1;

    if let Some(cd) = closing_delimiter.as_ref() {
      if cd != &self.cur_token {
        self.errors.push(ParserError {
          position: self.lexer_position,
          message: format!(
            "Missing closing delimiter \"{}\"",
            closing_delimiter.unwrap()
          ),
        });

        return Err(Error::PARSER);
      }
    }

    Ok(group)
  }

  fn parse_grpchoice(&mut self) -> Result<GroupChoice<'a>> {
    let mut grpchoice = GroupChoice {
      group_entries: Vec::new(),
      span: (self.lexer_position.range.0, 0, self.lexer_position.line),
    };

    if self.cur_token_is(Token::LBRACE)
      || self.cur_token_is(Token::LBRACKET)
      || self.cur_token_is(Token::GCHOICE)
    {
      self.next_token()?;

      grpchoice.span.0 = self.lexer_position.range.0;
    };

    // TODO: The logic in this while loop is quite messy. Need to figure out a
    // better way to advance the token when parsing the entries in a group
    // choice
    while !self.cur_token_is(Token::RBRACE)
      && !self.cur_token_is(Token::RPAREN)
      && !self.cur_token_is(Token::RBRACKET)
      && !self.cur_token_is(Token::EOF)
    {
      let ge = self.parse_grpent(false)?;

      if self.cur_token_is(Token::GCHOICE) {
        grpchoice.group_entries.push((ge, false));
        grpchoice.span.1 = self.parser_position.range.1;

        return Ok(grpchoice);
      }

      // Don't advance the token if it is a member key, comma or an opening or
      // closing map/group delimiter. Otherwise, advance
      if !self.cur_token_is(Token::RPAREN)
        && !self.cur_token_is(Token::RBRACE)
        && !self.cur_token_is(Token::RBRACKET)
        && !self.cur_token_is(Token::LPAREN)
        && !self.cur_token_is(Token::LBRACE)
        && !self.cur_token_is(Token::LBRACKET)
        && !self.cur_token_is(Token::COMMA)
        && !self.peek_token_is(&Token::COLON)
        && !self.peek_token_is(&Token::ARROWMAP)
        && !self.cur_token_is(Token::EOF)
      {
        self.parser_position.range.1 = self.lexer_position.range.1;
        self.next_token()?;
      }

      if self.cur_token_is(Token::COMMA) {
        grpchoice.group_entries.push((ge, true));
        self.parser_position.range.1 = self.lexer_position.range.1;
        self.next_token()?;
      } else {
        grpchoice.group_entries.push((ge, false));
      }

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }
    }

    grpchoice.span.1 = self.parser_position.range.1;

    Ok(grpchoice)
  }

  fn parse_grpent(&mut self, from_rule: bool) -> Result<GroupEntry<'a>> {
    let begin_grpent_range = self.lexer_position.range.0;
    let begin_grpent_line = self.lexer_position.line;

    let occur = self.parse_occur(true)?;

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    // If parsing group entry from a rule, set member key to none
    let member_key = if from_rule {
      None
    } else {
      self.parse_memberkey(true)?
    };

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    if self.cur_token_is(Token::LPAREN) {
      self.next_token()?;

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      let group = self.parse_group()?;

      let mut span = (
        begin_grpent_range,
        self.parser_position.range.1,
        begin_grpent_line,
      );

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      if !self.cur_token_is(Token::RPAREN) {
        self.errors.push(ParserError {
          position: self.lexer_position,
          message: "Missing closing parend \")\"".into(),
        });
        return Err(Error::PARSER);
      }

      self.parser_position.range.1 = self.lexer_position.range.1;
      span.1 = self.parser_position.range.1;

      self.next_token()?;

      return Ok(GroupEntry::InlineGroup { occur, group, span });
    }

    let mut span = (
      begin_grpent_range,
      self.parser_position.range.1,
      begin_grpent_line,
    );

    match member_key {
      Some(MemberKey::NonMemberKey(NonMemberKey::Type(entry_type))) => {
        if self.cur_token_is(Token::COMMA) {
          span.1 = self.lexer_position.range.1;
        }

        if let Some((name, generic_arg, _)) = entry_type.groupname_entry() {
          return Ok(GroupEntry::TypeGroupname {
            ge: TypeGroupnameEntry {
              occur,
              name,
              generic_arg,
            },
            span,
          });
        }

        // A parse tree that returns a type instead of a member key needs to
        // advance the token in the case of "(", "{" or "[". Otherwise, infinite
        // recursive loop occurs
        if let Token::LPAREN | Token::LBRACE | Token::LBRACKET = self.cur_token {
          self.next_token()?;
        }

        Ok(GroupEntry::ValueMemberKey {
          ge: Box::from(ValueMemberKeyEntry {
            occur,
            member_key: None,
            entry_type,
          }),
          span,
        })
      }
      Some(MemberKey::NonMemberKey(NonMemberKey::Group(group))) => {
        if self.cur_token_is(Token::COMMA) {
          span.1 = self.lexer_position.range.1;
        }

        Ok(GroupEntry::InlineGroup { occur, group, span })
      }
      member_key @ Some(_) => {
        let entry_type = self.parse_type(None)?;

        span.1 = self.parser_position.range.1;

        if self.cur_token_is(Token::COMMA) {
          span.1 = self.lexer_position.range.1;
        }

        Ok(GroupEntry::ValueMemberKey {
          ge: Box::from(ValueMemberKeyEntry {
            occur,
            member_key,
            entry_type,
          }),
          span,
        })
      }
      _ => {
        let entry_type = self.parse_type(None)?;

        span.1 = self.parser_position.range.1;

        if self.cur_token_is(Token::COMMA) {
          span.1 = self.lexer_position.range.1;
        }

        if let Some((name, generic_arg, _)) = entry_type.groupname_entry() {
          return Ok(GroupEntry::TypeGroupname {
            ge: TypeGroupnameEntry {
              occur,
              name,
              generic_arg,
            },
            span,
          });
        }

        Ok(GroupEntry::ValueMemberKey {
          ge: Box::from(ValueMemberKeyEntry {
            occur,
            member_key,
            entry_type,
          }),
          span,
        })
      }
    }
  }

  fn parse_memberkey_from_ident(
    &mut self,
    is_optional: bool,
    ident: (&'a str, Option<token::SocketPlug>),
    begin_memberkey_range: usize,
    begin_memberkey_line: usize,
  ) -> Result<Option<MemberKey<'a>>> {
    if !self.peek_token_is(&Token::COLON)
      && !self.peek_token_is(&Token::ARROWMAP)
      && !self.peek_token_is(&Token::CUT)
      && is_optional
    {
      return Ok(None);
    }

    self.parser_position.range.1 = self.peek_lexer_position.range.1;

    let end_t1_range = self.lexer_position.range.1;

    let mut ident = self.identifier_from_ident_token((&ident.0, ident.1));
    ident.span = (begin_memberkey_range, end_t1_range, begin_memberkey_line);

    self.next_token()?;

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    let mk = if self.cur_token_is(Token::CUT) {
      self.next_token()?;

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      if !self.cur_token_is(Token::ARROWMAP) {
        self.errors.push(ParserError {
          position: self.lexer_position,
          message: "Malformed memberkey. Missing \"=>\"".into(),
        });
        return Err(Error::PARSER);
      }

      let end_memberkey_range = self.lexer_position.range.1;
      let t1 = MemberKey::Type1 {
        t1: Box::from(Type1 {
          type2: Type2::Typename {
            ident,
            generic_arg: None,
            span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
          },
          operator: None,
          span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
        }),
        is_cut: true,
        span: (
          begin_memberkey_range,
          end_memberkey_range,
          begin_memberkey_line,
        ),
      };

      self.next_token()?;

      Some(t1)
    } else if self.cur_token_is(Token::ARROWMAP) {
      let end_memberkey_range = self.lexer_position.range.1;
      let t1 = MemberKey::Type1 {
        t1: Box::from(Type1 {
          type2: Type2::Typename {
            ident,
            generic_arg: None,
            span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
          },
          operator: None,
          span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
        }),
        is_cut: false,
        span: (
          begin_memberkey_range,
          end_memberkey_range,
          begin_memberkey_line,
        ),
      };

      self.next_token()?;

      Some(t1)
    } else {
      Some(MemberKey::Bareword {
        ident,
        span: (
          begin_memberkey_range,
          self.parser_position.range.1,
          begin_memberkey_line,
        ),
      })
    };

    if self.cur_token_is(Token::COLON) {
      self.next_token()?;
    }

    Ok(mk)
  }

  fn parse_memberkey(&mut self, is_optional: bool) -> Result<Option<MemberKey<'a>>> {
    let begin_memberkey_range = self.lexer_position.range.0;
    let begin_memberkey_line = self.lexer_position.line;

    if let Some(t) = self.cur_token.in_standard_prelude() {
      return self.parse_memberkey_from_ident(
        is_optional,
        (t, None),
        begin_memberkey_range,
        begin_memberkey_line,
      );
    }

    match &self.cur_token {
      Token::IDENT(ident) => {
        let ident = *ident;
        self.parse_memberkey_from_ident(
          is_optional,
          ident,
          begin_memberkey_range,
          begin_memberkey_line,
        )
      }
      Token::VALUE(value) => {
        if !self.peek_token_is(&Token::COLON)
          && !self.peek_token_is(&Token::ARROWMAP)
          && !self.peek_token_is(&Token::CUT)
          && is_optional
        {
          return Ok(None);
        }

        self.parser_position.range.1 = self.peek_lexer_position.range.1;

        let value = value.clone();

        let t1 = self.parse_type1(None)?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        let mk = if self.cur_token_is(Token::CUT) {
          self.next_token()?;

          while let Token::COMMENT(_) = self.cur_token {
            self.next_token()?;
          }

          if !self.cur_token_is(Token::ARROWMAP) {
            self.errors.push(ParserError {
              position: self.lexer_position,
              message: "Malformed memberkey. Missing \"=>\"".into(),
            });
            return Err(Error::PARSER);
          }

          let end_memberkey_range = self.lexer_position.range.1;
          let t1 = MemberKey::Type1 {
            t1: Box::from(t1),
            is_cut: true,
            span: (
              begin_memberkey_range,
              end_memberkey_range,
              begin_memberkey_line,
            ),
          };

          self.next_token()?;

          Some(t1)
        } else {
          if !self.cur_token_is(Token::ARROWMAP) && !self.cur_token_is(Token::COLON) {
            self.errors.push(ParserError {
              position: self.lexer_position,
              message: "Malformed memberkey. Missing \"=>\" or \":\"".into(),
            });
            return Err(Error::PARSER);
          }

          self.parser_position.range.1 = self.lexer_position.range.1;

          self.next_token()?;

          Some(MemberKey::Value {
            value,
            span: (
              begin_memberkey_range,
              self.parser_position.range.1,
              begin_memberkey_line,
            ),
          })
        };

        if self.cur_token_is(Token::COLON) {
          self.next_token()?;
        }

        Ok(mk)
      }
      // Indicates either an inline parenthesized type or an inline group. If
      // the latter, don't parse as memberkey
      Token::LPAREN => {
        let begin_memberkey_range = self.lexer_position.range.0;
        let begin_memberkey_line = self.lexer_position.line;

        let mut nested_parend_count = 0;

        self.next_token()?;

        let mut tokens: Vec<lexer::Item> = Vec::new();

        let mut has_group_entries = false;
        let mut closing_parend = false;
        while !closing_parend {
          if self.cur_token_is(Token::ARROWMAP) || !self.cur_token_is(Token::COLON) {
            has_group_entries = true;
          }

          if self.cur_token_is(Token::LPAREN) {
            nested_parend_count += 1;
          }

          if self.cur_token_is(Token::RPAREN) {
            match nested_parend_count.cmp(&0) {
              Ordering::Greater => nested_parend_count -= 1,
              Ordering::Equal | Ordering::Less => closing_parend = true,
            }
          }

          let t = self.cur_token.clone();
          tokens.push(Ok((self.lexer_position, t)));

          self.parser_position.range.1 = self.lexer_position.range.1;

          self.next_token()?;

          if self.cur_token_is(Token::EOF) {
            self.errors.push(ParserError {
              position: self.lexer_position,
              message: "Missing closing parend \")\"".into(),
            });

            return Err(Error::PARSER);
          }
        }

        // Parse tokens vec as group
        if has_group_entries {
          let mut p = Parser::new(tokens.into_iter(), self.str_input)?;
          let group = match p.parse_group() {
            Ok(g) => g,
            Err(Error::PARSER) => {
              for e in p.errors.into_iter() {
                self.errors.push(e);
              }

              return Err(Error::PARSER);
            }
            Err(e) => return Err(e),
          };

          return Ok(Some(MemberKey::NonMemberKey(NonMemberKey::Group(group))));
        }

        if let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;

          return self.parse_memberkey(is_optional);
        }

        // Parse tokens vec as type
        let mut p = Parser::new(tokens.into_iter(), self.str_input)?;
        let t = match p.parse_type(None) {
          Ok(t) => t,
          Err(Error::PARSER) => {
            for e in p.errors.into_iter() {
              self.errors.push(e);
            }

            return Err(Error::PARSER);
          }
          Err(e) => return Err(e),
        };

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        if self.cur_token_is(Token::CUT) {
          self.next_token()?;

          while let Token::COMMENT(_) = self.cur_token {
            self.next_token()?;
          }

          if !self.cur_token_is(Token::ARROWMAP) {
            self.errors.push(ParserError {
              position: self.lexer_position,
              message: "Malformed memberkey. Missing \"=>\"".into(),
            });
            return Err(Error::PARSER);
          }

          let end_memberkey_range = self.lexer_position.range.1;

          let t1 = Some(MemberKey::Type1 {
            t1: Box::from(t.type_choices[0].clone()),
            is_cut: true,
            span: (
              begin_memberkey_range,
              end_memberkey_range,
              begin_memberkey_line,
            ),
          });

          return Ok(t1);
        }

        let t1 = if self.cur_token_is(Token::ARROWMAP) {
          self.next_token()?;

          self.parser_position.range.1 = self.lexer_position.range.1;

          Some(MemberKey::Type1 {
            t1: Box::from(t.type_choices[0].clone()),
            is_cut: false,
            span: (
              begin_memberkey_range,
              self.lexer_position.range.0,
              begin_memberkey_line,
            ),
          })
        } else {
          Some(MemberKey::NonMemberKey(NonMemberKey::Type(Type {
            type_choices: t.type_choices,
            span: (
              begin_memberkey_range,
              self.parser_position.range.1,
              begin_memberkey_line,
            ),
          })))
        };

        Ok(t1)
      }
      _ => {
        if let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;

          return self.parse_memberkey(is_optional);
        }

        let t1 = self.parse_type1(None)?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        if self.cur_token_is(Token::CUT) {
          self.next_token()?;

          while let Token::COMMENT(_) = self.cur_token {
            self.next_token()?;
          }

          if !self.cur_token_is(Token::ARROWMAP) {
            self.errors.push(ParserError {
              position: self.lexer_position,
              message: "Malformed memberkey. Missing \"=>\"".into(),
            });
            return Err(Error::PARSER);
          }

          let end_memberkey_range = self.lexer_position.range.1;

          let t1 = Some(MemberKey::Type1 {
            t1: Box::from(t1),
            is_cut: true,
            span: (
              begin_memberkey_range,
              end_memberkey_range,
              begin_memberkey_line,
            ),
          });

          self.next_token()?;

          return Ok(t1);
        }

        let t1 = if self.cur_token_is(Token::ARROWMAP) {
          self.next_token()?;

          self.parser_position.range.1 = self.lexer_position.range.1;

          Some(MemberKey::Type1 {
            t1: Box::from(t1),
            is_cut: false,
            span: (
              begin_memberkey_range,
              self.parser_position.range.1,
              begin_memberkey_line,
            ),
          })
        } else {
          Some(MemberKey::NonMemberKey(NonMemberKey::Type(Type {
            type_choices: vec![t1],
            span: (
              begin_memberkey_range,
              self.parser_position.range.1,
              begin_memberkey_line,
            ),
          })))
        };

        Ok(t1)
      }
    }
  }

  fn parse_occur(&mut self, is_optional: bool) -> Result<Option<Occur>> {
    let begin_occur_range = self.lexer_position.range.0;
    let begin_occur_line = self.lexer_position.line;
    self.parser_position.line = self.lexer_position.line;

    match &self.cur_token {
      Token::OPTIONAL => {
        self.parser_position.range = self.lexer_position.range;

        self.next_token()?;
        Ok(Some(Occur::Optional((
          self.parser_position.range.0,
          self.parser_position.range.1,
          self.parser_position.line,
        ))))
      }
      Token::ONEORMORE => {
        self.parser_position.range = self.lexer_position.range;

        self.next_token()?;
        Ok(Some(Occur::OneOrMore((
          self.parser_position.range.0,
          self.parser_position.range.1,
          self.parser_position.line,
        ))))
      }
      Token::ASTERISK => {
        let o = if let Token::VALUE(token::Value::UINT(u)) = &self.peek_token {
          self.parser_position.range.0 = self.lexer_position.range.0;
          self.parser_position.range.1 = self.peek_lexer_position.range.1;

          Occur::Exact {
            lower: None,
            upper: Some(*u),
            span: (
              self.parser_position.range.0,
              self.parser_position.range.1,
              self.parser_position.line,
            ),
          }
        } else {
          self.parser_position.range = self.lexer_position.range;

          Occur::ZeroOrMore((
            self.parser_position.range.0,
            self.parser_position.range.1,
            self.parser_position.line,
          ))
        };

        self.next_token()?;

        if let Token::VALUE(token::Value::UINT(_)) = &self.cur_token {
          self.next_token()?;
        }
        Ok(Some(o))
      }
      Token::VALUE(_) => {
        let lower = if let Token::VALUE(value) = &self.cur_token {
          if let token::Value::UINT(li) = value {
            Some(*li)
          } else {
            None
          }
        } else {
          None
        };

        if !self.peek_token_is(&Token::ASTERISK) {
          if is_optional {
            return Ok(None);
          }

          self.errors.push(ParserError {
            position: self.lexer_position,
            message: "Malformed occurrence syntax".into(),
          });

          return Err(Error::PARSER);
        }

        self.next_token()?;

        self.parser_position.range.1 = self.lexer_position.range.1;

        self.next_token()?;

        let upper = if let Token::VALUE(value) = &self.cur_token {
          if let token::Value::UINT(ui) = value {
            self.parser_position.range.1 = self.lexer_position.range.1;
            Some(*ui)
          } else {
            None
          }
        } else {
          None
        };

        Ok(Some(Occur::Exact {
          lower,
          upper,
          span: (
            begin_occur_range,
            self.parser_position.range.1,
            begin_occur_line,
          ),
        }))
      }
      _ => Ok(None),
    }
  }

  fn cur_token_is(&self, t: Token) -> bool {
    mem::discriminant(&self.cur_token) == mem::discriminant(&t)
  }

  fn peek_token_is(&self, t: &Token) -> bool {
    mem::discriminant(&self.peek_token) == mem::discriminant(&t)
  }

  fn expect_peek(&mut self, t: &Token) -> Result<bool> {
    if self.peek_token_is(t) {
      return self.next_token().map(|_| true);
    }

    Ok(false)
  }

  /// Create `Identifier` from `Token::IDENT(ident)`
  fn identifier_from_ident_token(
    &self,
    ident: (&'a str, Option<token::SocketPlug>),
  ) -> Identifier<'a> {
    Identifier {
      ident: ident.0,
      socket: ident.1,
      span: (
        self.lexer_position.range.0,
        self.lexer_position.range.1,
        self.lexer_position.line,
      ),
    }
  }
}

/// Returns a `ast::CDDL` from a `&str`
///
/// # Arguments
///
/// * `lexer` - A mutable reference to a `lexer::Lexer`. Can be created from
///   `cddl::lexer_from_str()`
/// * `input` - A string slice with the CDDL text input
/// * `print_stderr` - When true, print any errors to stderr
///
/// # Example
///
/// ```
/// use cddl::{lexer_from_str, parser::cddl_from_str};
///
/// let input = r#"myrule = int"#;
/// let _ = cddl_from_str(&mut lexer_from_str(input), input, true);
#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "std")]
pub fn cddl_from_str<'a>(
  lexer: &'a mut Lexer<'a>,
  input: &'a str,
  print_stderr: bool,
) -> std::result::Result<CDDL<'a>, String> {
  match Parser::new(lexer.iter(), input).map_err(|e| e.to_string()) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(c) => Ok(c),
      Err(Error::PARSER) => {
        let e = if print_stderr {
          p.report_errors(true)
        } else {
          p.report_errors(false)
        };

        if let Ok(Some(e)) = e {
          return Err(e);
        }

        Err(Error::PARSER.to_string())
      }
      Err(e) => Err(e.to_string()),
    },
    Err(e) => Err(e),
  }
}

/// Returns a `ast::CDDL` from a `&str`
///
/// # Arguments
///
/// * `lexer` - A mutable reference to a `lexer::Lexer`. Can be created from
///   `cddl::lexer_from_str()`
/// * `input` - A string slice with the CDDL text input
///
/// # Example
///
/// ```
/// use cddl::{parser::cddl_from_str, lexer_from_str};
///
/// let input = r#"myrule = int"#;
///
/// let _ = cddl_from_str(&mut lexer_from_str(input), input);
/// ```
#[cfg(not(target_arch = "wasm32"))]
#[cfg(not(feature = "std"))]
pub fn cddl_from_str<'a>(
  lexer: &'a mut Lexer<'a>,
  input: &'a str,
) -> std::result::Result<CDDL<'a>, String> {
  match Parser::new(lexer.iter(), input).map_err(|e| e.to_string()) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(c) => Ok(c),
      Err(Error::PARSER) => {
        if let Some(e) = p.report_errors() {
          return Err(e);
        }

        Err(Error::PARSER.to_string())
      }
      Err(e) => Err(e.to_string()),
    },
    Err(e) => Err(e),
  }
}

/// Returns a `ast::CDDL` wrapped in `JsValue` from a `&str`
///
/// # Arguments
///
/// * `input` - A string slice with the CDDL text input
///
/// # Example
///
/// ```
/// use cddl::parser::cddl_from_str;
///
/// let input = r#"myrule = int"#;
/// let c = cddl_from_str(input)?;
/// ```
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn cddl_from_str(input: &str) -> result::Result<JsValue, JsValue> {
  match Parser::new(Lexer::new(input).iter(), input) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(c) => JsValue::from_serde(&c)
        .map_err(|e| JsValue::from(e.to_string()))
        .map(|c| c),
      Err(Error::PARSER) => {
        if let Ok(Some(e)) = p.report_errors(false) {
          return Err(JsValue::from(e));
        }

        Err(JsValue::from(Error::PARSER.to_string()))
      }
      Err(e) => Err(JsValue::from(e.to_string())),
    },
    Err(e) => Err(JsValue::from(e.to_string())),
  }
}

/// Validates CDDL input against RFC 8610
///
/// # Arguments
///
/// * `input` - A string slice with the CDDL text input
/// * `print_stderr` - When true, prints any errors to stderr
///
/// # Example
///
/// ```
/// use cddl::parser::compile_cddl_from_str;
///
/// let input = r#"myrule = int"#;
/// let _ = compile_cddl_from_str(input, true);
/// ```
#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "std")]
pub fn compile_cddl_from_str(input: &str, print_stderr: bool) -> Result<()> {
  match Parser::new(Lexer::new(input).iter(), input).map_err(|e| e.to_string()) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(_) => Ok(()),
      Err(Error::PARSER) => {
        let e = if print_stderr {
          p.report_errors(true)
        } else {
          p.report_errors(false)
        };

        if let Ok(Some(e)) = e {
          return Err(Error::CDDL(e));
        }

        Err(Error::CDDL(Error::PARSER.to_string()))
      }
      Err(e) => Err(e),
    },
    Err(e) => Err(Error::CDDL(e)),
  }
}

/// Validates CDDL input against RFC 8610
///
/// # Arguments
///
/// * `input` - A string slice with the CDDL text input
///
/// # Example
///
/// ```
/// use cddl::parser::compile_cddl_from_str;
///
/// let input = r#"myrule = int"#;
/// let _ = compile_cddl_from_str(input);
/// ```
#[cfg(not(target_arch = "wasm32"))]
#[cfg(not(feature = "std"))]
pub fn compile_cddl_from_str(input: &str) -> Result<()> {
  match Parser::new(Lexer::new(input).iter(), input).map_err(|e| e.to_string()) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(_) => Ok(()),
      Err(Error::PARSER) => {
        if let Some(e) = p.report_errors() {
          return Err(Error::CDDL(e));
        }

        Err(Error::CDDL(Error::PARSER.to_string()))
      }
      Err(e) => Err(e),
    },
    Err(e) => Err(Error::CDDL(e)),
  }
}

/// Validates CDDL input against RFC 8610
///
/// # Arguments
///
/// * `input` - A string slice with the CDDL text input
///
/// # Example
///
/// ```
/// use cddl::parser::compile_cddl_from_str;
///
/// let input = r#"myrule = int"#;
/// let _ = compile_cddl_from_str(input);
/// ```
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn compile_cddl_from_str(input: &str) -> result::Result<(), JsValue> {
  match Parser::new(Lexer::new(input).iter(), input) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(_) => Ok(()),
      Err(Error::PARSER) => {
        if let Ok(Some(e)) = p.report_errors(false) {
          return Err(JsValue::from(e));
        }

        Err(JsValue::from(Error::PARSER.to_string()))
      }
      Err(e) => Err(JsValue::from(e.to_string())),
    },
    Err(e) => Err(JsValue::from(e.to_string())),
  }
}

#[cfg(test)]
#[allow(unused_imports)]
mod tests {
  use super::{
    super::{ast, lexer::Lexer, token::SocketPlug},
    *,
  };
  use indoc::indoc;
  use pretty_assertions::assert_eq;

  #[test]
  #[allow(unused_variables)]
  fn verify_cddl() -> Result<()> {
    let input = indoc!(
      r#"
        myrule = secondrule
        myrange = 10..upper
        upper = 500 / 600
        gr = 2* ( test )
        messages = message<"reboot", "now">
        message<t, v> = {type: 2, value: v}
        color = &colors
        colors = ( red: "red" )
        thing = ( int / float )
      "#
    );

    match Parser::new(Lexer::new(input).iter(), input) {
      Ok(mut p) => match p.parse_cddl() {
        Ok(cddl) => {
          let expected_output = CDDL {
            rules: vec![
              Rule::Type {
                rule: TypeRule {
                  name: Identifier {
                    ident: "myrule".into(),
                    socket: None,
                    span: (0, 6, 1),
                  },
                  generic_param: None,
                  is_type_choice_alternate: false,
                  value: Type {
                    type_choices: vec![Type1 {
                      type2: Type2::Typename {
                        ident: Identifier {
                          ident: "secondrule".into(),
                          socket: None,
                          span: (9, 19, 1),
                        },
                        generic_arg: None,
                        span: (9, 19, 1),
                      },
                      operator: None,
                      span: (9, 19, 1),
                    }],
                    span: (9, 19, 1),
                  },
                },
                span: (0, 19, 1),
              },
              Rule::Type {
                rule: TypeRule {
                  name: Identifier {
                    ident: "myrange".into(),
                    socket: None,
                    span: (20, 27, 2),
                  },
                  generic_param: None,
                  is_type_choice_alternate: false,
                  value: Type {
                    type_choices: vec![Type1 {
                      type2: Type2::UintValue {
                        value: 10,
                        span: (30, 32, 2),
                      },
                      operator: Some((
                        RangeCtlOp::RangeOp {
                          is_inclusive: true,
                          span: (32, 34, 2),
                        },
                        Type2::Typename {
                          ident: Identifier {
                            ident: "upper".into(),
                            socket: None,
                            span: (34, 39, 2),
                          },
                          generic_arg: None,
                          span: (34, 39, 2),
                        },
                      )),
                      span: (30, 39, 2),
                    }],
                    span: (30, 39, 2),
                  },
                },
                span: (20, 39, 2),
              },
              Rule::Type {
                rule: TypeRule {
                  name: Identifier {
                    ident: "upper".into(),
                    socket: None,
                    span: (40, 45, 3),
                  },
                  generic_param: None,
                  is_type_choice_alternate: false,
                  value: Type {
                    type_choices: vec![
                      Type1 {
                        type2: Type2::UintValue {
                          value: 500,
                          span: (48, 51, 3),
                        },
                        operator: None,
                        span: (48, 51, 3),
                      },
                      Type1 {
                        type2: Type2::UintValue {
                          value: 600,
                          span: (54, 57, 3),
                        },
                        operator: None,
                        span: (54, 57, 3),
                      },
                    ],
                    span: (48, 57, 3),
                  },
                },
                span: (40, 57, 3),
              },
              Rule::Group {
                rule: Box::from(GroupRule {
                  name: Identifier {
                    ident: "gr".into(),
                    socket: None,
                    span: (58, 60, 4),
                  },
                  generic_param: None,
                  is_group_choice_alternate: false,
                  entry: GroupEntry::InlineGroup {
                    occur: Some(Occur::Exact {
                      lower: Some(2),
                      upper: None,
                      span: (63, 65, 4),
                    }),
                    group: Group {
                      group_choices: vec![GroupChoice {
                        group_entries: vec![(
                          GroupEntry::TypeGroupname {
                            ge: TypeGroupnameEntry {
                              occur: None,
                              name: Identifier {
                                ident: "test".into(),
                                socket: None,
                                span: (68, 72, 4),
                              },
                              generic_arg: None,
                            },
                            span: (68, 72, 4),
                          },
                          false,
                        )],
                        span: (68, 72, 4),
                      }],
                      span: (68, 72, 4),
                    },
                    span: (63, 74, 4),
                  },
                }),
                span: (58, 74, 4),
              },
              Rule::Type {
                rule: TypeRule {
                  name: Identifier {
                    ident: "messages".into(),
                    socket: None,
                    span: (75, 83, 5),
                  },
                  generic_param: None,
                  is_type_choice_alternate: false,
                  value: Type {
                    type_choices: vec![Type1 {
                      type2: Type2::Typename {
                        ident: Identifier {
                          ident: "message".into(),
                          socket: None,
                          span: (86, 93, 5),
                        },
                        generic_arg: Some(GenericArg {
                          args: vec![
                            Type1 {
                              type2: Type2::TextValue {
                                value: "reboot".into(),
                                span: (94, 102, 5),
                              },
                              operator: None,
                              span: (94, 102, 5),
                            },
                            Type1 {
                              type2: Type2::TextValue {
                                value: "now".into(),
                                span: (104, 109, 5),
                              },
                              operator: None,
                              span: (104, 109, 5),
                            },
                          ],
                          span: (93, 110, 5),
                        }),
                        span: (86, 110, 5),
                      },
                      operator: None,
                      span: (86, 110, 5),
                    }],
                    span: (86, 110, 5),
                  },
                },
                span: (75, 110, 5),
              },
              Rule::Type {
                rule: TypeRule {
                  name: Identifier {
                    ident: "message".into(),
                    socket: None,
                    span: (111, 118, 6),
                  },
                  generic_param: Some(GenericParm {
                    params: vec![
                      Identifier {
                        ident: "t".into(),
                        socket: None,
                        span: (119, 120, 6),
                      },
                      Identifier {
                        ident: "v".into(),
                        socket: None,
                        span: (122, 123, 6),
                      },
                    ],
                    span: (118, 124, 6),
                  }),
                  is_type_choice_alternate: false,
                  value: Type {
                    type_choices: vec![Type1 {
                      type2: Type2::Map {
                        group: Group {
                          group_choices: vec![GroupChoice {
                            group_entries: vec![
                              (
                                GroupEntry::ValueMemberKey {
                                  ge: Box::from(ValueMemberKeyEntry {
                                    occur: None,
                                    member_key: Some(MemberKey::Bareword {
                                      ident: Identifier {
                                        ident: "type".into(),
                                        socket: None,
                                        span: (128, 132, 6),
                                      },
                                      span: (128, 133, 6),
                                    }),
                                    entry_type: Type {
                                      type_choices: vec![Type1 {
                                        type2: Type2::UintValue {
                                          value: 2,
                                          span: (134, 135, 6),
                                        },
                                        operator: None,
                                        span: (134, 135, 6),
                                      }],
                                      span: (134, 135, 6),
                                    },
                                  }),
                                  span: (128, 136, 6),
                                },
                                true,
                              ),
                              (
                                GroupEntry::ValueMemberKey {
                                  ge: Box::from(ValueMemberKeyEntry {
                                    occur: None,
                                    member_key: Some(MemberKey::Bareword {
                                      ident: Identifier {
                                        ident: "value".into(),
                                        socket: None,
                                        span: (137, 142, 6),
                                      },
                                      span: (137, 143, 6),
                                    }),
                                    entry_type: Type {
                                      type_choices: vec![Type1 {
                                        type2: Type2::Typename {
                                          ident: Identifier {
                                            ident: "v".into(),
                                            socket: None,
                                            span: (144, 145, 6),
                                          },
                                          generic_arg: None,
                                          span: (144, 145, 6),
                                        },
                                        operator: None,
                                        span: (144, 145, 6),
                                      }],
                                      span: (144, 145, 6),
                                    },
                                  }),
                                  span: (137, 145, 6),
                                },
                                false,
                              ),
                            ],
                            span: (128, 145, 6),
                          }],
                          span: (128, 145, 6),
                        },
                        span: (127, 146, 6),
                      },
                      operator: None,
                      span: (127, 146, 6),
                    }],
                    span: (127, 146, 6),
                  },
                },
                span: (111, 146, 6),
              },
              Rule::Type {
                rule: TypeRule {
                  name: Identifier {
                    ident: "color".into(),
                    socket: None,
                    span: (147, 152, 7),
                  },
                  generic_param: None,
                  is_type_choice_alternate: false,
                  value: Type {
                    type_choices: vec![Type1 {
                      type2: Type2::ChoiceFromGroup {
                        ident: Identifier {
                          ident: "colors".into(),
                          socket: None,
                          span: (156, 162, 7),
                        },
                        generic_arg: None,
                        span: (155, 162, 7),
                      },
                      operator: None,
                      span: (155, 162, 7),
                    }],
                    span: (155, 162, 7),
                  },
                },
                span: (147, 162, 7),
              },
              Rule::Group {
                rule: Box::from(GroupRule {
                  name: Identifier {
                    ident: "colors".into(),
                    socket: None,
                    span: (163, 169, 8),
                  },
                  generic_param: None,
                  is_group_choice_alternate: false,
                  entry: GroupEntry::InlineGroup {
                    occur: None,
                    group: Group {
                      group_choices: vec![GroupChoice {
                        group_entries: vec![(
                          GroupEntry::ValueMemberKey {
                            ge: Box::from(ValueMemberKeyEntry {
                              occur: None,
                              member_key: Some(MemberKey::Bareword {
                                ident: Identifier {
                                  ident: "red".into(),
                                  socket: None,
                                  span: (174, 177, 8),
                                },
                                span: (174, 178, 8),
                              }),
                              entry_type: Type {
                                type_choices: vec![Type1 {
                                  type2: Type2::TextValue {
                                    value: "red".into(),
                                    span: (179, 184, 8),
                                  },
                                  operator: None,
                                  span: (179, 184, 8),
                                }],
                                span: (179, 184, 8),
                              },
                            }),
                            span: (174, 184, 8),
                          },
                          false,
                        )],
                        span: (174, 184, 8),
                      }],
                      span: (174, 184, 8),
                    },
                    span: (172, 186, 8),
                  },
                }),
                span: (163, 186, 8),
              },
              Rule::Type {
                rule: TypeRule {
                  name: Identifier {
                    ident: "thing".into(),
                    socket: None,
                    span: (187, 192, 9),
                  },
                  generic_param: None,
                  is_type_choice_alternate: false,
                  value: Type {
                    type_choices: vec![Type1 {
                      type2: Type2::ParenthesizedType {
                        pt: Type {
                          type_choices: vec![
                            Type1 {
                              type2: Type2::Typename {
                                ident: Identifier {
                                  ident: "int".into(),
                                  socket: None,
                                  span: (197, 200, 9),
                                },
                                generic_arg: None,
                                span: (197, 200, 9),
                              },
                              operator: None,
                              span: (197, 200, 9),
                            },
                            Type1 {
                              type2: Type2::Typename {
                                ident: Identifier {
                                  ident: "float".into(),
                                  socket: None,
                                  span: (203, 208, 9),
                                },
                                generic_arg: None,
                                span: (203, 208, 9),
                              },
                              operator: None,
                              span: (203, 208, 9),
                            },
                          ],
                          span: (197, 208, 9),
                        },
                        span: (195, 210, 9),
                      },
                      operator: None,
                      span: (195, 210, 9),
                    }],

                    span: (195, 210, 9),
                  },
                },
                span: (187, 210, 9),
              },
            ],
          };

          assert_eq!(cddl, expected_output);
          assert_eq!(cddl.to_string(), expected_output.to_string());

          Ok(())
        }

        #[cfg(feature = "std")]
        Err(Error::PARSER) if !p.errors.is_empty() => {
          let _ = p.report_errors(true);

          Err(Error::CDDL(p.report_errors(false).unwrap().unwrap()))
        }
        #[cfg(not(feature = "std"))]
        Err(Error::PARSER) if !p.errors.is_empty() => {
          let _ = p.report_errors();

          Err(Error::CDDL(p.report_errors().unwrap()))
        }
        Err(e) => Err(e),
      },
      Err(e) => Err(e),
    }
  }

  #[test]
  fn verify_rule_diagnostic() -> Result<()> {
    let input = indoc!(
      r#"
        a = 1234
        a = b
      "#
    );

    match Parser::new(Lexer::new(input).iter(), input) {
      Ok(mut p) => match p.parse_cddl() {
        Ok(_) => Ok(()),
        #[cfg(feature = "std")]
        Err(Error::PARSER) if !p.errors.is_empty() => {
          let e = p.report_errors(false).unwrap().unwrap();

          #[cfg(feature = "std")]
          println!("{}", e);

          assert_eq!(
            e,
            indoc!(
              r#"
                error: parser errors

                   ┌── input:2:1 ───
                   │
                 2 │ a = b
                   │ ^^^^^ Rule with name 'a' already defined
                   │

              "#
            )
          );
          Ok(())
        }
        #[cfg(not(feature = "std"))]
        Err(Error::PARSER) if !p.errors.is_empty() => {
          assert_eq!(
            p.report_errors().unwrap(),
            indoc!(
              r#"
                error: parser errors

                   ┌── input:2:1 ───
                   │
                 2 │ a = b
                   │ ^^^^^ Rule with name 'a' already defined
                   │

              "#
            )
          );
          Ok(())
        }
        Err(e) => Err(e),
      },
      Err(e) => Err(e),
    }
  }

  #[test]
  fn verify_genericparm() -> Result<()> {
    let input = r#"<t, v>"#;

    let mut l = Lexer::new(input);
    let gps = Parser::new(&mut l.iter(), input)?.parse_genericparm()?;

    let expected_output = GenericParm {
      params: vec![
        Identifier {
          ident: "t".into(),
          socket: None,
          span: (1, 2, 1),
        },
        Identifier {
          ident: "v".into(),
          socket: None,
          span: (4, 5, 1),
        },
      ],
      span: (0, 6, 1),
    };

    assert_eq!(gps, expected_output);
    assert_eq!(gps.to_string(), expected_output.to_string());

    Ok(())
  }

  #[test]
  fn verify_genericparm_diagnostic() -> Result<()> {
    let input = r#"<1, 2>"#;

    match Parser::new(Lexer::new(input).iter(), input) {
      Ok(mut p) => match p.parse_genericparm() {
        Ok(_) => Ok(()),
        #[cfg(feature = "std")]
        Err(Error::PARSER) if !p.errors.is_empty() => {
          let e = p.report_errors(false).unwrap().unwrap();

          #[cfg(feature = "std")]
          println!("{}", e);

          assert_eq!(
            e,
            indoc!(
              r#"
                error: parser errors

                   ┌── input:1:2 ───
                   │
                 1 │ <1, 2>
                   │  ^ Generic parameters must be named identifiers
                   │

              "#
            )
          );
          Ok(())
        }
        #[cfg(not(feature = "std"))]
        Err(Error::PARSER) if !p.errors.is_empty() => {
          assert_eq!(
            p.report_errors().unwrap(),
            indoc!(
              r#"
                error: parser errors

                   ┌── input:1:2 ───
                   │
                 1 │ <1, 2>
                   │  ^ Generic parameters must be named identifiers
                   │

              "#
            )
          );
          Ok(())
        }
        Err(e) => Err(e),
      },
      Err(e) => Err(e),
    }
  }

  #[test]
  fn verify_genericparm_rule_diagnostic() -> Result<()> {
    let input = indoc!(
      r#"
        rule<paramA paramB> = test
        ruleb = rulec
        ruleb = ruled
        rulec = rulee
        rulec = rulee2
      "#
    );

    match Parser::new(Lexer::new(input).iter(), input) {
      Ok(mut p) => match p.parse_cddl() {
        Ok(_) => Ok(()),
        #[cfg(feature = "std")]
        Err(Error::PARSER) if !p.errors.is_empty() => {
          let e = p.report_errors(false).unwrap().unwrap();

          println!("{}", e);

          assert_eq!(
            e,
            indoc!(
              r#"
                error: parser errors

                   ┌── input:1:6 ───
                   │
                 1 │ rule<paramA paramB> = test
                   │      ^^^^^^^^^^^^^ Generic parameters should be between angle brackets '<' and '>' and separated by a comma ','
                 2 │ ruleb = rulec
                 3 │ ruleb = ruled
                   │ ^^^^^^^^^^^^^ Rule with name 'ruleb' already defined
                 4 │ rulec = rulee
                 5 │ rulec = rulee2
                   │ ^^^^^^^^^^^^^^ Rule with name 'rulec' already defined
                   │

              "#
            )
          );
          Ok(())
        }
        #[cfg(not(feature = "std"))]
        Err(Error::PARSER) if !p.errors.is_empty() => {
          assert_eq!(
            p.report_errors().unwrap(),
            indoc!(
              r#"
                error: parser errors

                   ┌── input:1:6 ───
                   │
                 1 │ rule<paramA paramB> = test
                   │      ^^^^^^^^^^^^^ Generic parameters should be between angle brackets '<' and '>' and separated by a comma ','
                 2 │ ruleb = rulec
                 3 │ ruleb = ruled
                   │ ^^^^^^^^^^^^^ Rule with name 'ruleb' already defined
                 4 │ rulec = rulee
                 5 │ rulec = rulee2
                   │ ^^^^^^^^^^^^^^ Rule with name 'rulec' already defined
                   │

              "#
            )
          );
          Ok(())
        }
        Err(e) => Err(e),
      },
      Err(e) => Err(e),
    }
  }

  #[test]
  fn verify_genericarg() -> Result<()> {
    let input = r#"<"reboot", "now">"#;

    let mut l = Lexer::new(input);

    let generic_args = Parser::new(l.iter(), input)?.parse_genericarg()?;

    let expected_output = GenericArg {
      args: vec![
        Type1 {
          type2: Type2::TextValue {
            value: "reboot".into(),
            span: (1, 9, 1),
          },
          operator: None,
          span: (1, 9, 1),
        },
        Type1 {
          type2: Type2::TextValue {
            value: "now".into(),
            span: (11, 16, 1),
          },
          operator: None,
          span: (11, 16, 1),
        },
      ],
      span: (0, 17, 1),
    };

    assert_eq!(generic_args, expected_output);
    assert_eq!(generic_args.to_string(), expected_output.to_string());

    Ok(())
  }

  #[test]
  fn verify_type() -> Result<()> {
    let input = r#"( tchoice1 / tchoice2 )"#;

    let mut l = Lexer::new(input);

    let t = Parser::new(l.iter(), input)?.parse_type(None)?;

    let expected_output = Type {
      type_choices: vec![Type1 {
        type2: Type2::ParenthesizedType {
          pt: Type {
            type_choices: vec![
              Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "tchoice1".into(),
                    socket: None,
                    span: (2, 10, 1),
                  },
                  generic_arg: None,
                  span: (2, 10, 1),
                },
                operator: None,
                span: (2, 10, 1),
              },
              Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "tchoice2".into(),
                    socket: None,
                    span: (13, 21, 1),
                  },
                  generic_arg: None,
                  span: (13, 21, 1),
                },
                operator: None,
                span: (13, 21, 1),
              },
            ],
            span: (2, 21, 1),
          },
          span: (0, 23, 1),
        },
        operator: None,
        span: (0, 23, 1),
      }],
      span: (0, 23, 1),
    };

    assert_eq!(t, expected_output);
    assert_eq!(t.to_string(), expected_output.to_string());

    Ok(())
  }

  #[test]
  fn verify_type1() -> Result<()> {
    let inputs = [
      r#"5..10"#,
      r#"-10.5...10.1"#,
      r#"1.5..4.5"#,
      r#"my..lower ... upper"#,
      r#"target .lt controller"#,
      r#"( text / tstr ) .eq "hello""#,
    ];

    let expected_outputs = [
      Type1 {
        type2: Type2::UintValue {
          value: 5,
          span: (0, 1, 1),
        },
        operator: Some((
          RangeCtlOp::RangeOp {
            is_inclusive: true,
            span: (1, 3, 1),
          },
          Type2::UintValue {
            value: 10,
            span: (3, 5, 1),
          },
        )),
        span: (0, 5, 1),
      },
      Type1 {
        type2: Type2::FloatValue {
          value: -10.5,
          span: (0, 5, 1),
        },
        operator: Some((
          RangeCtlOp::RangeOp {
            is_inclusive: false,
            span: (5, 8, 1),
          },
          Type2::FloatValue {
            value: 10.1,
            span: (8, 12, 1),
          },
        )),
        span: (0, 12, 1),
      },
      Type1 {
        type2: Type2::FloatValue {
          value: 1.5,
          span: (0, 3, 1),
        },
        operator: Some((
          RangeCtlOp::RangeOp {
            is_inclusive: true,
            span: (3, 5, 1),
          },
          Type2::FloatValue {
            value: 4.5,
            span: (5, 8, 1),
          },
        )),
        span: (0, 8, 1),
      },
      Type1 {
        type2: Type2::Typename {
          ident: Identifier {
            ident: "my..lower".into(),
            socket: None,
            span: (0, 9, 1),
          },
          generic_arg: None,
          span: (0, 9, 1),
        },
        operator: Some((
          RangeCtlOp::RangeOp {
            is_inclusive: false,
            span: (10, 13, 1),
          },
          Type2::Typename {
            ident: Identifier {
              ident: "upper".into(),
              socket: None,
              span: (14, 19, 1),
            },
            generic_arg: None,
            span: (14, 19, 1),
          },
        )),
        span: (0, 19, 1),
      },
      Type1 {
        type2: Type2::Typename {
          ident: Identifier {
            ident: "target".into(),
            socket: None,
            span: (0, 6, 1),
          },
          generic_arg: None,
          span: (0, 6, 1),
        },
        operator: Some((
          RangeCtlOp::CtlOp {
            ctrl: ".lt",
            span: (7, 10, 1),
          },
          Type2::Typename {
            ident: Identifier {
              ident: "controller".into(),
              socket: None,
              span: (11, 21, 1),
            },
            generic_arg: None,
            span: (11, 21, 1),
          },
        )),
        span: (0, 21, 1),
      },
      Type1 {
        type2: Type2::ParenthesizedType {
          pt: Type {
            type_choices: vec![
              Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "text".into(),
                    socket: None,
                    span: (2, 6, 1),
                  },
                  generic_arg: None,
                  span: (2, 6, 1),
                },
                operator: None,
                span: (2, 6, 1),
              },
              Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "tstr".into(),
                    socket: None,
                    span: (9, 13, 1),
                  },
                  generic_arg: None,
                  span: (9, 13, 1),
                },
                operator: None,
                span: (9, 13, 1),
              },
            ],
            span: (2, 13, 1),
          },
          span: (0, 15, 1),
        },
        operator: Some((
          RangeCtlOp::CtlOp {
            ctrl: ".eq",
            span: (16, 19, 1),
          },
          Type2::TextValue {
            value: "hello".into(),
            span: (20, 27, 1),
          },
        )),
        span: (0, 27, 1),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let mut l = Lexer::new(inputs[idx]);
      let t1 = Parser::new(l.iter(), inputs[idx])?.parse_type1(None)?;

      assert_eq!(&t1, expected_output);
      assert_eq!(t1.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_type2() -> Result<()> {
    let inputs = [
      r#""myvalue""#,
      r#"message<"reboot", "now">"#,
      r#"$$tcp-option"#,
      r#"~group1"#,
      r#"#6.997(tstr)"#,
      r#"9.9"#,
      r#"#"#,
      r#"[*3 reputon]"#,
      r#"[+ reputon]"#,
      r#"&groupname"#,
      r#"&( inlinegroup )"#,
      r#"{ ? "optional-key" ^ => int, }"#,
      r#"[ ( a: int, b: tstr ) ]"#,
    ];

    let expected_outputs = [
      Type2::TextValue {
        value: "myvalue".into(),
        span: (0, 9, 1),
      },
      Type2::Typename {
        ident: Identifier {
          ident: "message".into(),
          socket: None,
          span: (0, 7, 1),
        },
        generic_arg: Some(GenericArg {
          args: vec![
            Type1 {
              type2: Type2::TextValue {
                value: "reboot".into(),
                span: (8, 16, 1),
              },
              operator: None,
              span: (8, 16, 1),
            },
            Type1 {
              type2: Type2::TextValue {
                value: "now".into(),
                span: (18, 23, 1),
              },
              operator: None,
              span: (18, 23, 1),
            },
          ],
          span: (7, 24, 1),
        }),
        span: (0, 24, 1),
      },
      Type2::Typename {
        ident: Identifier {
          ident: "tcp-option".into(),
          socket: Some(SocketPlug::GROUP),
          span: (0, 12, 1),
        },
        generic_arg: None,
        span: (0, 12, 1),
      },
      Type2::Unwrap {
        ident: Identifier {
          ident: "group1".into(),
          socket: None,
          span: (1, 7, 1),
        },
        generic_arg: None,
        span: (0, 0, 0),
      },
      Type2::TaggedData {
        tag: Some(997),
        t: Type {
          type_choices: vec![Type1 {
            type2: Type2::Typename {
              ident: Identifier {
                ident: "tstr".into(),
                socket: None,
                span: (7, 11, 1),
              },
              generic_arg: None,
              span: (7, 11, 1),
            },
            operator: None,
            span: (7, 11, 1),
          }],
          span: (7, 11, 1),
        },
        span: (0, 11, 1),
      },
      Type2::FloatValue {
        value: 9.9,
        span: (0, 3, 1),
      },
      Type2::Any((0, 1, 1)),
      Type2::Array {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::TypeGroupname {
                ge: TypeGroupnameEntry {
                  occur: Some(Occur::Exact {
                    lower: None,
                    upper: Some(3),
                    span: (1, 3, 1),
                  }),
                  name: Identifier {
                    ident: "reputon".into(),
                    socket: None,
                    span: (4, 11, 1),
                  },
                  generic_arg: None,
                },
                span: (1, 11, 1),
              },
              false,
            )],
            span: (1, 11, 1),
          }],
          span: (1, 11, 1),
        },
        span: (0, 12, 1),
      },
      Type2::Array {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::TypeGroupname {
                ge: TypeGroupnameEntry {
                  occur: Some(Occur::OneOrMore((1, 2, 1))),
                  name: Identifier {
                    ident: "reputon".into(),
                    socket: None,
                    span: (3, 10, 1),
                  },
                  generic_arg: None,
                },
                span: (1, 10, 1),
              },
              false,
            )],
            span: (1, 10, 1),
          }],
          span: (1, 10, 1),
        },
        span: (0, 11, 1),
      },
      Type2::ChoiceFromGroup {
        ident: Identifier {
          ident: "groupname".into(),
          socket: None,
          span: (1, 10, 1),
        },
        generic_arg: None,
        span: (0, 10, 1),
      },
      Type2::ChoiceFromInlineGroup {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::TypeGroupname {
                ge: TypeGroupnameEntry {
                  occur: None,
                  name: Identifier {
                    ident: "inlinegroup".into(),
                    socket: None,
                    span: (3, 14, 1),
                  },
                  generic_arg: None,
                },
                span: (3, 14, 1),
              },
              false,
            )],
            span: (3, 14, 1),
          }],
          span: (3, 14, 1),
        },
        span: (0, 14, 1),
      },
      Type2::Map {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::ValueMemberKey {
                ge: Box::from(ValueMemberKeyEntry {
                  occur: Some(Occur::Optional((2, 3, 1))),
                  member_key: Some(MemberKey::Type1 {
                    t1: Box::from(Type1 {
                      type2: Type2::TextValue {
                        value: "optional-key".into(),
                        span: (4, 18, 1),
                      },
                      operator: None,
                      span: (4, 18, 1),
                    }),
                    is_cut: true,
                    span: (4, 23, 1),
                  }),
                  entry_type: Type {
                    type_choices: vec![Type1 {
                      type2: Type2::Typename {
                        ident: Identifier {
                          ident: "int".into(),
                          socket: None,
                          span: (24, 27, 1),
                        },
                        generic_arg: None,
                        span: (24, 27, 1),
                      },
                      operator: None,
                      span: (24, 27, 1),
                    }],
                    span: (24, 27, 1),
                  },
                }),
                span: (2, 28, 1),
              },
              true,
            )],
            span: (2, 28, 1),
          }],
          span: (2, 28, 1),
        },
        span: (0, 30, 1),
      },
      Type2::Array {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::InlineGroup {
                group: Group {
                  group_choices: vec![GroupChoice {
                    group_entries: vec![
                      (
                        GroupEntry::ValueMemberKey {
                          ge: Box::from(ValueMemberKeyEntry {
                            occur: None,
                            member_key: Some(MemberKey::Bareword {
                              ident: Identifier {
                                ident: "a".into(),
                                socket: None,
                                span: (4, 5, 1),
                              },
                              span: (4, 6, 1),
                            }),
                            entry_type: Type {
                              type_choices: vec![Type1 {
                                type2: Type2::Typename {
                                  ident: Identifier {
                                    ident: "int".into(),
                                    socket: None,
                                    span: (7, 10, 1),
                                  },
                                  generic_arg: None,
                                  span: (7, 10, 1),
                                },
                                operator: None,
                                span: (7, 10, 1),
                              }],
                              span: (7, 10, 1),
                            },
                          }),
                          span: (4, 11, 1),
                        },
                        true,
                      ),
                      (
                        GroupEntry::ValueMemberKey {
                          ge: Box::from(ValueMemberKeyEntry {
                            occur: None,
                            member_key: Some(MemberKey::Bareword {
                              ident: Identifier {
                                ident: "b".into(),
                                socket: None,
                                span: (12, 13, 1),
                              },
                              span: (12, 14, 1),
                            }),
                            entry_type: Type {
                              type_choices: vec![Type1 {
                                type2: Type2::Typename {
                                  ident: Identifier {
                                    ident: "tstr".into(),
                                    socket: None,
                                    span: (15, 19, 1),
                                  },
                                  generic_arg: None,
                                  span: (15, 19, 1),
                                },
                                operator: None,
                                span: (15, 19, 1),
                              }],
                              span: (15, 19, 1),
                            },
                          }),
                          span: (12, 19, 1),
                        },
                        false,
                      ),
                    ],
                    span: (4, 19, 1),
                  }],
                  span: (4, 19, 1),
                },
                occur: None,
                span: (2, 21, 1),
              },
              false,
            )],
            span: (2, 21, 1),
          }],
          span: (2, 21, 1),
        },
        span: (0, 23, 1),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let mut l = Lexer::new(inputs[idx]);
      let t2 = Parser::new(l.iter(), inputs[idx])?.parse_type2()?;

      assert_eq!(&t2, expected_output);
      assert_eq!(t2.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_type2_complex() -> Result<()> {
    let inputs = [
      r#"[ [* file-entry], [* directory-entry] ]"#,
      r#"{ int, int // int, tstr }"#,
      r#"{ int, int, int, tstr }"#,
    ];

    let expected_ouputs = [
      Type2::Array {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![
              (
                GroupEntry::ValueMemberKey {
                  ge: Box::from(ValueMemberKeyEntry {
                    occur: None,
                    member_key: None,
                    entry_type: Type {
                      type_choices: vec![Type1 {
                        type2: Type2::Array {
                          group: Group {
                            group_choices: vec![GroupChoice {
                              group_entries: vec![(
                                GroupEntry::TypeGroupname {
                                  ge: TypeGroupnameEntry {
                                    occur: Some(Occur::ZeroOrMore((3, 4, 1))),
                                    name: Identifier {
                                      ident: "file-entry".into(),
                                      socket: None,
                                      span: (5, 15, 1),
                                    },
                                    generic_arg: None,
                                  },
                                  span: (3, 15, 1),
                                },
                                false,
                              )],
                              span: (3, 15, 1),
                            }],
                            span: (3, 15, 1),
                          },
                          span: (2, 16, 1),
                        },
                        operator: None,
                        span: (2, 16, 1),
                      }],
                      span: (2, 16, 1),
                    },
                  }),
                  span: (2, 17, 1),
                },
                true,
              ),
              (
                GroupEntry::ValueMemberKey {
                  ge: Box::from(ValueMemberKeyEntry {
                    occur: None,
                    member_key: None,
                    entry_type: Type {
                      type_choices: vec![Type1 {
                        type2: Type2::Array {
                          group: Group {
                            group_choices: vec![GroupChoice {
                              group_entries: vec![(
                                GroupEntry::TypeGroupname {
                                  ge: TypeGroupnameEntry {
                                    occur: Some(Occur::ZeroOrMore((19, 20, 1))),
                                    name: Identifier {
                                      ident: "directory-entry".into(),
                                      socket: None,
                                      span: (21, 36, 1),
                                    },
                                    generic_arg: None,
                                  },
                                  span: (19, 36, 1),
                                },
                                false,
                              )],
                              span: (19, 36, 1),
                            }],
                            span: (19, 36, 1),
                          },
                          span: (18, 37, 1),
                        },
                        operator: None,
                        span: (18, 37, 1),
                      }],
                      span: (18, 37, 1),
                    },
                  }),
                  span: (18, 37, 1),
                },
                false,
              ),
            ],
            span: (2, 37, 1),
          }],
          span: (2, 37, 1),
        },
        span: (0, 39, 1),
      },
      Type2::Map {
        group: Group {
          group_choices: vec![
            GroupChoice {
              group_entries: vec![
                (
                  GroupEntry::TypeGroupname {
                    ge: TypeGroupnameEntry {
                      occur: None,
                      name: Identifier {
                        ident: "int".into(),
                        socket: None,
                        span: (2, 5, 1),
                      },
                      generic_arg: None,
                    },
                    span: (2, 6, 1),
                  },
                  true,
                ),
                (
                  GroupEntry::TypeGroupname {
                    ge: TypeGroupnameEntry {
                      occur: None,
                      name: Identifier {
                        ident: "int".into(),
                        socket: None,
                        span: (7, 10, 1),
                      },
                      generic_arg: None,
                    },
                    span: (7, 10, 1),
                  },
                  false,
                ),
              ],
              span: (2, 10, 1),
            },
            GroupChoice {
              group_entries: vec![
                (
                  GroupEntry::TypeGroupname {
                    ge: TypeGroupnameEntry {
                      occur: None,
                      name: Identifier {
                        ident: "int".into(),
                        socket: None,
                        span: (14, 17, 1),
                      },
                      generic_arg: None,
                    },
                    span: (14, 18, 1),
                  },
                  true,
                ),
                (
                  GroupEntry::TypeGroupname {
                    ge: TypeGroupnameEntry {
                      occur: None,
                      name: Identifier {
                        ident: "tstr".into(),
                        socket: None,
                        span: (19, 23, 1),
                      },
                      generic_arg: None,
                    },
                    span: (19, 23, 1),
                  },
                  false,
                ),
              ],
              span: (14, 23, 1),
            },
          ],
          span: (2, 23, 1),
        },
        span: (0, 25, 1),
      },
      Type2::Map {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![
              (
                GroupEntry::TypeGroupname {
                  ge: TypeGroupnameEntry {
                    occur: None,
                    name: Identifier {
                      ident: "int".into(),
                      socket: None,
                      span: (2, 5, 1),
                    },
                    generic_arg: None,
                  },
                  span: (2, 6, 1),
                },
                true,
              ),
              (
                GroupEntry::TypeGroupname {
                  ge: TypeGroupnameEntry {
                    occur: None,
                    name: Identifier {
                      ident: "int".into(),
                      socket: None,
                      span: (7, 10, 1),
                    },
                    generic_arg: None,
                  },
                  span: (7, 11, 1),
                },
                true,
              ),
              (
                GroupEntry::TypeGroupname {
                  ge: TypeGroupnameEntry {
                    occur: None,
                    name: Identifier {
                      ident: "int".into(),
                      socket: None,
                      span: (12, 15, 1),
                    },
                    generic_arg: None,
                  },
                  span: (12, 16, 1),
                },
                true,
              ),
              (
                GroupEntry::TypeGroupname {
                  ge: TypeGroupnameEntry {
                    occur: None,
                    name: Identifier {
                      ident: "tstr".into(),
                      socket: None,
                      span: (17, 21, 1),
                    },
                    generic_arg: None,
                  },
                  span: (17, 21, 1),
                },
                false,
              ),
            ],
            span: (2, 21, 1),
          }],
          span: (2, 21, 1),
        },
        span: (0, 23, 1),
      },
    ];

    for (idx, expected_output) in expected_ouputs.iter().enumerate() {
      let mut l = Lexer::new(inputs[idx]);
      let t2 = Parser::new(l.iter(), inputs[idx])?.parse_type2()?;

      assert_eq!(&t2, expected_output);
      assert_eq!(t2.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_grpent() -> Result<()> {
    let inputs = [
      r#"* type1 ^ => "value""#,
      r#"type1: type2"#,
      r#"typename"#,
      r#"? 0: addrdistr"#,
      r#"0: finite_set<transaction_input>"#,
      r#"* [credential] => coin"#,
    ];

    let expected_outputs = [
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: Some(Occur::ZeroOrMore((0, 1, 1))),
          member_key: Some(MemberKey::Type1 {
            t1: Box::from(Type1 {
              type2: Type2::Typename {
                ident: Identifier {
                  ident: "type1".into(),
                  socket: None,
                  span: (2, 7, 1),
                },
                generic_arg: None,
                span: (2, 7, 1),
              },
              operator: None,
              span: (2, 7, 1),
            }),
            is_cut: true,
            span: (2, 12, 1),
          }),
          entry_type: Type {
            type_choices: vec![Type1 {
              type2: Type2::TextValue {
                value: "value".into(),
                span: (13, 20, 1),
              },
              operator: None,
              span: (13, 20, 1),
            }],
            span: (13, 20, 1),
          },
        }),
        span: (0, 20, 1),
      },
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: None,
          member_key: Some(MemberKey::Bareword {
            ident: Identifier {
              ident: "type1".into(),
              socket: None,
              span: (0, 5, 1),
            },
            span: (0, 6, 1),
          }),
          entry_type: Type {
            type_choices: vec![Type1 {
              type2: Type2::Typename {
                ident: Identifier {
                  ident: "type2".into(),
                  socket: None,
                  span: (7, 12, 1),
                },
                generic_arg: None,
                span: (7, 12, 1),
              },
              operator: None,
              span: (7, 12, 1),
            }],
            span: (7, 12, 1),
          },
        }),
        span: (0, 12, 1),
      },
      GroupEntry::TypeGroupname {
        ge: TypeGroupnameEntry {
          occur: None,
          name: Identifier {
            ident: "typename".into(),
            socket: None,
            span: (0, 8, 1),
          },
          generic_arg: None,
        },
        span: (0, 8, 1),
      },
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: Some(Occur::Optional((0, 1, 1))),
          member_key: Some(MemberKey::Value {
            value: token::Value::UINT(0),
            span: (2, 4, 1),
          }),
          entry_type: Type {
            type_choices: vec![Type1 {
              type2: Type2::Typename {
                ident: Identifier {
                  ident: "addrdistr".into(),
                  socket: None,
                  span: (5, 14, 1),
                },
                generic_arg: None,
                span: (5, 14, 1),
              },
              operator: None,
              span: (5, 14, 1),
            }],
            span: (5, 14, 1),
          },
        }),
        span: (0, 14, 1),
      },
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: None,
          member_key: Some(MemberKey::Value {
            value: token::Value::UINT(0),
            span: (0, 2, 1),
          }),
          entry_type: Type {
            type_choices: vec![Type1 {
              type2: Type2::Typename {
                ident: Identifier {
                  ident: "finite_set".into(),
                  socket: None,
                  span: (3, 13, 1),
                },
                generic_arg: Some(GenericArg {
                  args: vec![Type1 {
                    type2: Type2::Typename {
                      ident: Identifier {
                        ident: "transaction_input".into(),
                        socket: None,
                        span: (14, 31, 1),
                      },
                      generic_arg: None,
                      span: (14, 31, 1),
                    },
                    operator: None,
                    span: (14, 31, 1),
                  }],
                  span: (13, 32, 1),
                }),
                span: (3, 32, 1),
              },
              operator: None,
              span: (3, 32, 1),
            }],
            span: (3, 32, 1),
          },
        }),
        span: (0, 32, 1),
      },
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: Some(Occur::ZeroOrMore((0, 1, 1))),
          member_key: Some(MemberKey::Type1 {
            t1: Box::from(Type1 {
              type2: Type2::Array {
                group: Group {
                  group_choices: vec![GroupChoice {
                    group_entries: vec![(
                      GroupEntry::TypeGroupname {
                        ge: TypeGroupnameEntry {
                          occur: None,
                          name: Identifier {
                            ident: "credential".into(),
                            socket: None,
                            span: (3, 13, 1),
                          },
                          generic_arg: None,
                        },
                        span: (3, 13, 1),
                      },
                      false,
                    )],
                    span: (3, 13, 1),
                  }],
                  span: (3, 13, 1),
                },
                span: (2, 14, 1),
              },
              operator: None,
              span: (2, 14, 1),
            }),
            is_cut: false,
            span: (2, 22, 1),
          }),
          entry_type: Type {
            type_choices: vec![Type1 {
              type2: Type2::Typename {
                ident: Identifier {
                  ident: "coin".into(),
                  socket: None,
                  span: (18, 22, 1),
                },
                generic_arg: None,
                span: (18, 22, 1),
              },
              operator: None,
              span: (18, 22, 1),
            }],
            span: (18, 22, 1),
          },
        }),
        span: (0, 22, 1),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let mut l = Lexer::new(inputs[idx]);
      let grpent = Parser::new(l.iter(), inputs[idx])?.parse_grpent(false)?;

      assert_eq!(&grpent, expected_output);
      assert_eq!(grpent.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_memberkey() -> Result<()> {
    let inputs = [
      r#"type1 =>"#,
      r#""mytype1" ^ =>"#,
      r#"mybareword:"#,
      r#"my..bareword:"#,
      r#""myvalue": "#,
      r#"0:"#,
    ];

    let expected_outputs = [
      MemberKey::Type1 {
        t1: Box::from(Type1 {
          type2: Type2::Typename {
            ident: Identifier {
              ident: "type1".into(),
              socket: None,
              span: (0, 5, 1),
            },
            generic_arg: None,
            span: (0, 5, 1),
          },
          operator: None,
          span: (0, 5, 1),
        }),
        is_cut: false,
        span: (0, 8, 1),
      },
      MemberKey::Type1 {
        t1: Box::from(Type1 {
          type2: Type2::TextValue {
            value: "mytype1".into(),
            span: (0, 9, 1),
          },
          operator: None,
          span: (0, 9, 1),
        }),
        is_cut: true,
        span: (0, 14, 1),
      },
      MemberKey::Bareword {
        ident: Identifier {
          ident: "mybareword".into(),
          socket: None,
          span: (0, 10, 1),
        },
        span: (0, 11, 1),
      },
      MemberKey::Bareword {
        ident: Identifier {
          ident: "my..bareword".into(),
          socket: None,
          span: (0, 12, 1),
        },
        span: (0, 13, 1),
      },
      MemberKey::Value {
        value: token::Value::TEXT("myvalue".into()),
        span: (0, 10, 1),
      },
      MemberKey::Value {
        value: token::Value::UINT(0),
        span: (0, 2, 1),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let mut l = Lexer::new(inputs[idx]);
      let mk = Parser::new(l.iter(), inputs[idx])?.parse_memberkey(false)?;

      if let Some(mk) = mk {
        assert_eq!(&mk, expected_output);
        assert_eq!(mk.to_string(), expected_output.to_string());
      }
    }

    Ok(())
  }

  #[test]
  fn verify_occur() -> Result<()> {
    let inputs = [r#"1*3"#, r#"*"#, r#"+"#, r#"5*"#, r#"*3"#, r#"?"#];

    let expected_outputs = [
      Occur::Exact {
        lower: Some(1),
        upper: Some(3),
        span: (0, 3, 1),
      },
      Occur::ZeroOrMore((0, 1, 1)),
      Occur::OneOrMore((0, 1, 1)),
      Occur::Exact {
        lower: Some(5),
        upper: None,
        span: (0, 2, 1),
      },
      Occur::Exact {
        lower: None,
        upper: Some(3),
        span: (0, 2, 1),
      },
      Occur::Optional((0, 1, 1)),
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let o = Parser::new(Lexer::new(inputs[idx]).iter(), inputs[idx])?.parse_occur(false)?;

      if let Some(o) = o {
        assert_eq!(&o, expected_output);
        assert_eq!(o.to_string(), expected_output.to_string());
      }
    }

    Ok(())
  }
}
