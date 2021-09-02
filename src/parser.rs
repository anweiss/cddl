use super::{
  ast::*,
  error::{
    ErrorMsg,
    MsgType::{self, *},
  },
  lexer::{self, Lexer, Position},
  token::{self, SocketPlug, Token},
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

use std::{cmp::Ordering, mem, result};

use displaydoc::Display;

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::{Cow, ToOwned},
  boxed::Box,
  string::{String, ToString},
  vec::Vec,
};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
use serde::Serialize;

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
  #[cfg(feature = "ast-span")]
  parser_position: Position,
  /// Vec of collected parsing errors
  pub errors: Vec<Error>,
}

/// Parsing error types
#[derive(Debug, Display)]
pub enum Error {
  /// Parsing errors
  #[displaydoc("{0}")]
  CDDL(String),
  #[cfg_attr(
    feature = "ast-span",
    displaydoc("parsing error: position {position:?}, msg: {msg}")
  )]
  #[cfg_attr(not(feature = "ast-span"), displaydoc("parsing error: msg: {msg}"))]
  /// Parsing error occurred
  PARSER {
    /// Error position
    #[cfg(feature = "ast-span")]
    position: Position,
    /// Error message
    msg: ErrorMsg,
  },
  #[displaydoc("{0}")]
  /// Lexing error
  LEXER(lexer::Error),
  /// Regex error
  #[displaydoc("regex parsing error: {0}")]
  REGEX(regex::Error),
  #[displaydoc("incremental parsing error")]
  /// Incremental parsing error
  INCREMENTAL,
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

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
      #[cfg(feature = "ast-span")]
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
  ///   if let Err(Error::INCREMENTAL) = p.parse_cddl() {
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
      if let Error::PARSER {
        #[cfg(feature = "ast-span")]
        position,
        msg,
      } = error
      {
        labels.push(
          #[cfg(feature = "ast-span")]
          Label::primary(file_id, position.range.0..position.range.1).with_message(msg.to_string()),
          #[cfg(not(feature = "ast-span"))]
          Label::primary(file_id, 0..0).with_message(msg.to_string()),
        );
      }
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
      if let Error::PARSER { position, msg } = error {
        labels.push(
          Label::primary(file_id, position.range.0..position.range.1).with_message(msg.to_string()),
        );
      }
    }

    let diagnostic = Diagnostic::error()
      .with_message("parser errors")
      .with_labels(labels);

    let config = term::Config::default();

    let mut buffer = Vec::new();
    let mut writer = term::termcolor::NoColor::new(&mut buffer);

    term::emit(&mut writer, &config, &files, &diagnostic).ok()?;

    String::from_utf8(buffer).ok()
  }

  fn next_token(&mut self) -> Result<()> {
    mem::swap(&mut self.cur_token, &mut self.peek_token);
    mem::swap(&mut self.lexer_position, &mut self.peek_lexer_position);

    if let Some(next_token) = self.tokens.next() {
      let nt = next_token.map_err(Error::LEXER)?;
      self.peek_token = nt.1;
      self.peek_lexer_position = nt.0;
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

  fn collect_comments(&mut self) -> Result<Option<Comments<'a>>> {
    #[cfg_attr(not(feature = "lsp"), allow(unused_mut))]
    let mut comments: Option<Comments> = None;

    while let Token::COMMENT(_comment) = self.cur_token {
      #[cfg(feature = "lsp")]
      comments.get_or_insert(Comments::default()).0.push(_comment);

      self.next_token()?;
    }

    while let Token::NEWLINE = self.cur_token {
      #[cfg(feature = "lsp")]
      comments.get_or_insert(Comments::default()).0.push("\n");

      self.next_token()?;
    }

    if let Token::COMMENT(_) = self.cur_token {
      if let Some(c) = self.collect_comments()? {
        #[cfg_attr(not(feature = "lsp"), allow(unused_mut))]
        for comment in c.0.iter() {
          comments.get_or_insert(Comments::default()).0.push(comment);
        }
      }
    }

    Ok(comments)
  }

  /// Parses into a `CDDL` AST
  pub fn parse_cddl(&mut self) -> Result<CDDL<'a>> {
    let mut c = CDDL {
      comments: self.collect_comments()?,
      ..Default::default()
    };

    while self.cur_token != Token::EOF {
      match self.parse_rule() {
        Ok(r) => {
          let rule_exists =
            |existing_rule: &Rule| r.name() == existing_rule.name() && !r.is_choice_alternate();
          if c.rules.iter().any(rule_exists) {
            #[cfg(feature = "ast-span")]
            {
              self.parser_position.range = (r.span().0, r.span().1);
              self.parser_position.line = r.span().2;
            }

            self.errors.push(Error::PARSER {
              #[cfg(feature = "ast-span")]
              position: self.parser_position,
              msg: DuplicateRuleIdentifier.into(),
            });

            continue;
          }

          c.rules.push(r);
        }
        Err(Error::INCREMENTAL) => {
          if !self.cur_token_is(Token::EOF) {
            self.advance_to_next_rule()?;
          }
        }
        Err(e) => return Err(e),
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
      return Err(Error::INCREMENTAL);
    }

    if c.rules.is_empty() {
      self.errors.push(Error::PARSER {
        #[cfg(feature = "ast-span")]
        position: self.parser_position,
        msg: NoRulesDefined.into(),
      });

      return Err(Error::INCREMENTAL);
    }

    Ok(c)
  }

  fn parse_rule(&mut self) -> Result<Rule<'a>> {
    #[cfg(feature = "ast-span")]
    let begin_rule_range = self.lexer_position.range.0;
    #[cfg(feature = "ast-span")]
    let begin_rule_line = self.lexer_position.line;
    #[cfg(feature = "ast-span")]
    let begin_rule_col = self.lexer_position.column;

    let ident = match &self.cur_token {
      Token::IDENT(i) => self.identifier_from_ident_token(*i),
      _ => {
        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range = self.lexer_position.range;
          self.parser_position.line = self.lexer_position.line;
        }

        self.errors.push(Error::PARSER {
          #[cfg(feature = "ast-span")]
          position: self.parser_position,
          msg: InvalidRuleIdentifier.into(),
        });

        return Err(Error::INCREMENTAL);
      }
    };

    let gp = if self.peek_token_is(&Token::LANGLEBRACKET) {
      self.next_token()?;

      Some(self.parse_genericparm()?)
    } else {
      None
    };

    let comments_before_assign = self.collect_comments()?;

    if !self.expect_peek(&Token::ASSIGN)?
      && !self.expect_peek(&Token::TCHOICEALT)?
      && !self.expect_peek(&Token::GCHOICEALT)?
    {
      #[cfg(feature = "ast-span")]
      {
        self.parser_position.range = (begin_rule_range, self.lexer_position.range.1);
        self.parser_position.line = self.lexer_position.line;
      }

      self.errors.push(Error::PARSER {
        #[cfg(feature = "ast-span")]
        position: self.parser_position,
        msg: MsgType::MissingAssignmentToken.into(),
      });

      return Err(Error::INCREMENTAL);
    }

    let mut is_type_choice_alternate = false;
    let mut is_group_choice_alternate = false;

    if self.cur_token_is(Token::TCHOICEALT) {
      is_type_choice_alternate = true;
    } else if self.cur_token_is(Token::GCHOICEALT) {
      is_group_choice_alternate = true;
    }

    if let Some(socket) = &ident.socket {
      match socket {
        SocketPlug::TYPE if !is_type_choice_alternate => {
          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range = (begin_rule_range, self.lexer_position.range.1);
            self.parser_position.line = self.lexer_position.line;
          }

          self.errors.push(Error::PARSER {
            #[cfg(feature = "ast-span")]
            position: self.parser_position,
            msg: MsgType::TypeSocketNamesMustBeTypeAugmentations.into(),
          });

          return Err(Error::INCREMENTAL);
        }
        SocketPlug::GROUP if !is_group_choice_alternate => {
          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range = (begin_rule_range, self.lexer_position.range.1);
            self.parser_position.line = self.lexer_position.line;
          }

          self.errors.push(Error::PARSER {
            #[cfg(feature = "ast-span")]
            position: self.parser_position,
            msg: MsgType::GroupSocketNamesMustBeGroupAugmentations.into(),
          });

          return Err(Error::INCREMENTAL);
        }
        _ => (),
      }
    }

    self.next_token()?;

    let comments_after_assign = self.collect_comments()?;

    match self.cur_token {
      // Check for an occurrence indicator of uint followed by an asterisk '*'
      Token::VALUE(token::Value::UINT(_)) => {
        if self.peek_token_is(&Token::ASTERISK) {
          let ge = self.parse_grpent(true)?;

          let comments_after_rule = self.collect_comments()?;

          #[cfg(feature = "ast-span")]
          let span = (
            begin_rule_range,
            self.parser_position.range.1,
            begin_rule_line,
          );

          Ok(Rule::Group {
            rule: Box::from(GroupRule {
              name: ident,
              generic_params: gp,
              is_group_choice_alternate,
              entry: ge,
              comments_before_assigng: comments_before_assign,
              comments_after_assigng: comments_after_assign,
            }),
            comments_after_rule,
            #[cfg(feature = "ast-span")]
            span,
          })
        } else {
          let mut t = self.parse_type(None)?;

          #[cfg(feature = "ast-span")]
          let span = (
            begin_rule_range,
            self.parser_position.range.1,
            begin_rule_line,
          );

          let comments_after_rule = if let Some(comments) = t.comments_after_type() {
            Some(comments)
          } else {
            self.collect_comments()?
          };

          Ok(Rule::Type {
            rule: TypeRule {
              name: ident,
              generic_params: gp,
              is_type_choice_alternate,
              value: t,
              comments_before_assignt: comments_before_assign,
              comments_after_assignt: comments_after_assign,
            },
            comments_after_rule,
            #[cfg(feature = "ast-span")]
            span,
          })
        }
      }
      Token::LPAREN | Token::ASTERISK | Token::ONEORMORE | Token::OPTIONAL => {
        #[cfg(feature = "ast-span")]
        let begin_pt_range = self.lexer_position.range.0;

        let ge = self.parse_grpent(true)?;

        #[cfg(feature = "ast-span")]
        let mut end_rule_range = self.parser_position.range.1;

        let comments_after_rule = self.collect_comments()?;

        // If a group entry is an inline group with no leading occurrence
        // indicator, and its group has only a single element that is not
        // preceded by an occurrence indicator nor member key, treat it as a
        // parenthesized type, subsequently parsing the remaining type and
        // returning the type rule. This is one of the few situations where
        // `clone` is required
        if let GroupEntry::InlineGroup {
          occur: None,
          group,
          comments_before_group,
          comments_after_group,
          ..
        } = &ge
        {
          if group.group_choices.len() == 1 {
            if let Some(gc) = group.group_choices.get(0) {
              if gc.group_entries.len() == 1 {
                if let Some(group_entry) = gc.group_entries.get(0) {
                  // Check that there is no trailing comma
                  if !group_entry.1.optional_comma {
                    // TODO: Replace with box pattern destructuring once supported in stable
                    if let GroupEntry::ValueMemberKey { ge, .. } = &group_entry.0 {
                      if ge.occur.is_none() && ge.member_key.is_none() {
                        let value = self.parse_type(Some(Type2::ParenthesizedType {
                          comments_before_type: comments_before_group.clone(),
                          pt: ge.entry_type.clone(),
                          comments_after_type: comments_after_group.clone(),
                          #[cfg(feature = "ast-span")]
                          span: (
                            begin_pt_range,
                            self.parser_position.range.1,
                            begin_rule_line,
                          ),
                        }))?;

                        #[cfg(feature = "ast-span")]
                        {
                          end_rule_range = self.parser_position.range.1;
                        }

                        return Ok(Rule::Type {
                          rule: TypeRule {
                            name: ident,
                            generic_params: gp,
                            is_type_choice_alternate,
                            value,
                            comments_before_assignt: comments_before_assign,
                            comments_after_assignt: comments_after_assign,
                          },
                          comments_after_rule,
                          #[cfg(feature = "ast-span")]
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
            generic_params: gp,
            is_group_choice_alternate,
            entry: ge,
            comments_before_assigng: comments_before_assign,
            comments_after_assigng: comments_after_assign,
          }),
          comments_after_rule,
          #[cfg(feature = "ast-span")]
          span: (begin_rule_range, end_rule_range, begin_rule_line),
        })
      }
      _ => {
        // If type rule is an unwrap type, advance token after parsing type
        let advance_token = matches!(self.cur_token, Token::UNWRAP);
        let mut t = self.parse_type(None)?;

        if advance_token {
          self.next_token()?;
        }

        let comments_after_rule = if let Some(comments) = t.comments_after_type() {
          Some(comments)
        } else {
          self.collect_comments()?
        };

        if self.cur_token_is(Token::ASSIGN)
          || self.cur_token_is(Token::TCHOICEALT)
          || self.cur_token_is(Token::GCHOICEALT)
        {
          self.errors.push(Error::PARSER {
            #[cfg(feature = "ast-span")]
            position: Position {
              line: begin_rule_line,
              column: begin_rule_col,
              range: (ident.span.0, ident.span.1),
              index: self.parser_position.range.0,
            },
            msg: IncompleteRuleEntry.into(),
          });

          return Err(Error::INCREMENTAL);
        }

        #[cfg(feature = "ast-span")]
        let span = (
          begin_rule_range,
          self.parser_position.range.1,
          begin_rule_line,
        );

        Ok(Rule::Type {
          rule: TypeRule {
            name: ident,
            generic_params: gp,
            is_type_choice_alternate,
            value: t,
            comments_before_assignt: comments_before_assign,
            comments_after_assignt: comments_after_assign,
          },
          comments_after_rule,
          #[cfg(feature = "ast-span")]
          span,
        })
      }
    }
  }

  fn parse_genericparm(&mut self) -> Result<GenericParams<'a>> {
    #[cfg(feature = "ast-span")]
    let begin_range = self.lexer_position.range.0;

    if self.cur_token_is(Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    let mut generic_params = GenericParams::default();

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      let comments_before_ident = self.collect_comments()?;

      match &self.cur_token {
        Token::IDENT(ident) => {
          let param = self.identifier_from_ident_token(*ident);

          self.next_token()?;

          let comments_after_ident = self.collect_comments()?;

          generic_params.params.push(GenericParam {
            param,
            comments_before_ident,
            comments_after_ident,
          });

          if !self.cur_token_is(Token::COMMA) && !self.cur_token_is(Token::RANGLEBRACKET) {
            #[cfg(feature = "ast-span")]
            {
              self.parser_position.range = (begin_range + 1, self.peek_lexer_position.range.0);
              self.parser_position.line = self.lexer_position.line;
            }

            self.errors.push(Error::PARSER {
              #[cfg(feature = "ast-span")]
              position: self.parser_position,
              msg: InvalidGenericSyntax.into(),
            });

            return Err(Error::INCREMENTAL);
          }
        }
        Token::COMMA => self.next_token()?,
        Token::VALUE(_) => {
          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range = (self.lexer_position.range.0, self.lexer_position.range.1);
            self.parser_position.line = self.lexer_position.line;
          }

          self.errors.push(Error::PARSER {
            #[cfg(feature = "ast-span")]
            position: self.parser_position,
            msg: InvalidGenericIdentifier.into(),
          });

          return Err(Error::INCREMENTAL);
        }
        _ => {
          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range = (begin_range, self.lexer_position.range.0);
            self.parser_position.line = self.lexer_position.line;
          }

          self.errors.push(Error::PARSER {
            #[cfg(feature = "ast-span")]
            position: self.parser_position,
            msg: InvalidGenericSyntax.into(),
          });

          return Err(Error::INCREMENTAL);
        }
      }
    }

    // Since generic params are only found after the identifier of a rule, don't
    // advance beyond the closing '>' to retain the expect_peek semantics for
    // '=', '/=' and '//='

    #[cfg(feature = "ast-span")]
    {
      let end_range = self.lexer_position.range.1;
      generic_params.span = (begin_range, end_range, self.lexer_position.line);
    }

    Ok(generic_params)
  }

  fn parse_genericargs(&mut self) -> Result<GenericArgs<'a>> {
    if self.peek_token_is(&Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    #[cfg(feature = "ast-span")]
    let begin_generic_arg_range = self.lexer_position.range.0;
    #[cfg(feature = "ast-span")]
    let begin_generic_arg_line = self.lexer_position.line;

    // Required for type2 mutual recursion
    if self.cur_token_is(Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    let mut generic_args = GenericArgs::default();

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      let leading_comments = self.collect_comments()?;

      let t1 = self.parse_type1(None)?;

      let trailing_comments = self.collect_comments()?;

      generic_args.args.push(GenericArg {
        comments_before_type: leading_comments,
        arg: Box::from(t1),
        comments_after_type: trailing_comments,
      });

      if let Token::COMMA = self.cur_token {
        self.next_token()?;
      }

      if self.cur_token_is(Token::EOF) {
        self.errors.push(Error::PARSER {
          #[cfg(feature = "ast-span")]
          position: self.parser_position,
          msg: MissingGenericClosingDelimiter.into(),
        });

        return Err(Error::INCREMENTAL);
      }
    }

    if self.cur_token_is(Token::RANGLEBRACKET) {
      #[cfg(feature = "ast-span")]
      {
        self.parser_position.range.1 = self.lexer_position.range.1;
      }
      self.next_token()?;
    }

    #[cfg(feature = "ast-span")]
    {
      generic_args.span = (
        begin_generic_arg_range,
        self.parser_position.range.1,
        begin_generic_arg_line,
      );
    }

    Ok(generic_args)
  }

  fn parse_type(&mut self, parenthesized_type: Option<Type2<'a>>) -> Result<Type<'a>> {
    #[cfg(feature = "ast-span")]
    {
      self.parser_position.range = self.lexer_position.range;
      self.parser_position.line = self.lexer_position.line;

      
    }

    #[cfg(feature = "ast-span")]
    let begin_type_range = if let Some(Type2::ParenthesizedType { span, .. }) = parenthesized_type
      {
        self.parser_position.line = span.2;

        span.0
      } else {
        self.parser_position.range.0
      };

    let mut t = Type {
      type_choices: Vec::new(),
      #[cfg(feature = "ast-span")]
      span: (begin_type_range, 0, self.parser_position.line),
    };

    let mut tc = TypeChoice {
      type1: self.parse_type1(parenthesized_type)?,
      comments_before_type: None,
      comments_after_type: None,
    };

    tc.comments_after_type = self.collect_comments()?;

    t.type_choices.push(tc);

    while self.cur_token_is(Token::TCHOICE) {
      self.next_token()?;

      let comments_before_type = self.collect_comments()?;

      let mut tc = TypeChoice {
        comments_before_type,
        comments_after_type: None,
        type1: self.parse_type1(None)?,
      };

      tc.comments_after_type = self.collect_comments()?;

      t.type_choices.push(tc);
    }

    #[cfg(feature = "ast-span")]
    {
      t.span.1 = self.parser_position.range.1;
    }

    Ok(t)
  }

  fn parse_type1(&mut self, parenthesized_type: Option<Type2<'a>>) -> Result<Type1<'a>> {
    #[cfg(feature = "ast-span")]
    let mut begin_type1_line = self.lexer_position.line;
    #[cfg(feature = "ast-span")]
    let mut begin_type1_range = self.lexer_position.range.0;

    let t2_1 = if let Some(Type2::ParenthesizedType {
      comments_before_type,
      pt,
      comments_after_type,
      #[cfg(feature = "ast-span")]
      span,
    }) = parenthesized_type
    {
      #[cfg(feature = "ast-span")]
      {
        begin_type1_line = span.2;
        begin_type1_range = span.0;
      }

      Type2::ParenthesizedType {
        comments_before_type,
        pt,
        comments_after_type,
        #[cfg(feature = "ast-span")]
        span,
      }
    } else {
      self.parse_type2()?
    };

    #[cfg(feature = "ast-span")]
    let mut span = (
      begin_type1_range,
      self.lexer_position.range.1,
      begin_type1_line,
    );

    let comments_after_type = self.collect_comments()?;

    let op = match &self.cur_token {
      Token::RANGEOP(i) => {
        #[cfg(feature = "ast-span")]
        {
          span.0 = self.lexer_position.range.0;
        }

        Some(RangeCtlOp::RangeOp {
          is_inclusive: *i,
          #[cfg(feature = "ast-span")]
          span,
        })
      }
      _ => match token::control_str_from_token(&self.cur_token) {
        Some(ctrl) => {
          #[cfg(feature = "ast-span")]
          {
            span.0 = self.lexer_position.range.0;
          }

          Some(RangeCtlOp::CtlOp {
            ctrl,
            #[cfg(feature = "ast-span")]
            span,
          })
        }
        None => None,
      },
    };

    #[cfg(feature = "ast-span")]
    {
      span = (
        begin_type1_range,
        self.parser_position.range.1,
        begin_type1_line,
      );
    }

    match op {
      Some(operator) => {
        self.next_token()?;

        let comments_after_operator = self.collect_comments()?;

        let t2 = self.parse_type2()?;

        #[cfg(feature = "ast-span")]
        {
          span.1 = self.parser_position.range.1;
        }

        Ok(Type1 {
          type2: t2_1,
          operator: Some(Operator {
            comments_before_operator: comments_after_type,
            operator,
            comments_after_operator,
            type2: t2,
          }),
          comments_after_type: None,
          #[cfg(feature = "ast-span")]
          span,
        })
      }
      None => Ok(Type1 {
        type2: t2_1,
        operator: None,
        comments_after_type,
        #[cfg(feature = "ast-span")]
        span,
      }),
    }
  }

  fn parse_type2(&mut self) -> Result<Type2<'a>> {
    let t2 = match &self.cur_token {
      // value
      Token::VALUE(value) => {
        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range = self.lexer_position.range;
          self.parser_position.line = self.lexer_position.line;
        }

        #[cfg(feature = "ast-span")]
        let span = (
          self.parser_position.range.0,
          self.parser_position.range.1,
          self.parser_position.line,
        );

        match value {
          token::Value::TEXT(t) => Ok(Type2::TextValue {
            value: t.clone(),
            #[cfg(feature = "ast-span")]
            span,
          }),
          token::Value::INT(i) => Ok(Type2::IntValue {
            value: *i,
            #[cfg(feature = "ast-span")]
            span,
          }),
          token::Value::UINT(ui) => Ok(Type2::UintValue {
            value: *ui,
            #[cfg(feature = "ast-span")]
            span,
          }),
          token::Value::FLOAT(f) => Ok(Type2::FloatValue {
            value: *f,
            #[cfg(feature = "ast-span")]
            span,
          }),
          token::Value::BYTE(token::ByteValue::UTF8(Cow::Borrowed(utf8))) => {
            Ok(Type2::UTF8ByteString {
              value: Cow::Borrowed(utf8),
              #[cfg(feature = "ast-span")]
              span,
            })
          }
          token::Value::BYTE(token::ByteValue::UTF8(Cow::Owned(utf8))) => {
            Ok(Type2::UTF8ByteString {
              value: Cow::Owned(utf8.to_owned()),
              #[cfg(feature = "ast-span")]
              span,
            })
          }
          token::Value::BYTE(token::ByteValue::B16(Cow::Borrowed(b16))) => {
            Ok(Type2::B16ByteString {
              value: Cow::Borrowed(b16),
              #[cfg(feature = "ast-span")]
              span,
            })
          }
          token::Value::BYTE(token::ByteValue::B16(Cow::Owned(b16))) => Ok(Type2::B16ByteString {
            value: Cow::Owned(b16.to_owned()),
            #[cfg(feature = "ast-span")]
            span,
          }),
          token::Value::BYTE(token::ByteValue::B64(Cow::Borrowed(b64))) => {
            Ok(Type2::B64ByteString {
              value: Cow::Borrowed(b64),
              #[cfg(feature = "ast-span")]
              span,
            })
          }
          token::Value::BYTE(token::ByteValue::B64(Cow::Owned(b64))) => Ok(Type2::B64ByteString {
            value: Cow::Owned(b64.to_owned()),
            #[cfg(feature = "ast-span")]
            span,
          }),
        }
      }

      // typename [genericarg]
      Token::IDENT(ident) => {
        #[cfg(feature = "ast-span")]
        let begin_type2_range = self.lexer_position.range.0;
        #[cfg(feature = "ast-span")]
        let begin_type2_line = self.lexer_position.line;

        // optional genericarg detected
        if self.peek_token_is(&Token::LANGLEBRACKET) {
          let ident = self.identifier_from_ident_token(*ident);
          let ga = self.parse_genericargs()?;

          #[cfg(feature = "ast-span")]
          let end_type2_range = self.parser_position.range.1;

          return Ok(Type2::Typename {
            ident,
            generic_args: Some(ga),
            #[cfg(feature = "ast-span")]
            span: (begin_type2_range, end_type2_range, begin_type2_line),
          });
        }

        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range = self.lexer_position.range;
          self.parser_position.line = self.lexer_position.line;
        }

        Ok(Type2::Typename {
          ident: self.identifier_from_ident_token(*ident),
          generic_args: None,
          #[cfg(feature = "ast-span")]
          span: (
            self.parser_position.range.0,
            self.parser_position.range.1,
            self.parser_position.line,
          ),
        })
      }

      // ( type )
      Token::LPAREN => {
        #[cfg(feature = "ast-span")]
        let begin_type2_range = self.lexer_position.range.0;
        #[cfg(feature = "ast-span")]
        let begin_type2_line = self.lexer_position.line;

        self.next_token()?;

        let comments_before_type = self.collect_comments()?;

        let pt = self.parse_type(None)?;

        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range.0 = begin_type2_range;
          self.parser_position.range.1 = self.lexer_position.range.1;
          self.parser_position.line = begin_type2_line;
        }

        let comments_after_type = self.collect_comments()?;

        Ok(Type2::ParenthesizedType {
          comments_before_type,
          comments_after_type,
          pt,
          #[cfg(feature = "ast-span")]
          span: (
            self.parser_position.range.0,
            self.parser_position.range.1,
            self.parser_position.line,
          ),
        })
      }

      // { group }
      Token::LBRACE => {
        #[cfg(feature = "ast-span")]
        let begin_type2_range = self.lexer_position.range.0;
        #[cfg(feature = "ast-span")]
        let begin_type2_line = self.lexer_position.line;

        let mut group = self.parse_group()?;

        let comments_before_group = if let Some(GroupChoice {
          comments_before_grpchoice,
          ..
        }) = group.group_choices.first_mut()
        {
          comments_before_grpchoice.take()
        } else {
          None
        };

        #[cfg(feature = "ast-span")]
        let span = (
          begin_type2_range,
          self.lexer_position.range.1,
          begin_type2_line,
        );

        let comments_after_group = self.collect_comments()?;

        Ok(Type2::Map {
          comments_before_group,
          group,
          #[cfg(feature = "ast-span")]
          span,
          comments_after_group,
        })
      }

      // [ group ]
      Token::LBRACKET => {
        #[cfg(feature = "ast-span")]
        let begin_type2_range = self.lexer_position.range.0;
        #[cfg(feature = "ast-span")]
        let begin_type2_line = self.lexer_position.line;

        let mut group = self.parse_group()?;

        let comments_before_group = if let Some(GroupChoice {
          comments_before_grpchoice,
          ..
        }) = group.group_choices.first_mut()
        {
          if comments_before_grpchoice.is_some() {
            comments_before_grpchoice.take()
          } else {
            None
          }
        } else {
          None
        };

        #[cfg(feature = "ast-span")]
        let span = (
          begin_type2_range,
          self.lexer_position.range.1,
          begin_type2_line,
        );

        let comments_after_group = self.collect_comments()?;

        Ok(Type2::Array {
          comments_before_group,
          group,
          comments_after_group,
          #[cfg(feature = "ast-span")]
          span,
        })
      }

      // ~ typename [genericarg]
      Token::UNWRAP => {
        self.next_token()?;

        let comments = self.collect_comments()?;

        let ident = if let Some(ident) = self.cur_token.in_standard_prelude() {
          Some(self.identifier_from_ident_token((ident, None)))
        } else if let Token::IDENT(ident) = &self.cur_token {
          Some(self.identifier_from_ident_token(*ident))
        } else {
          None
        };

        if let Some(ident) = ident {
          if self.peek_token_is(&Token::LANGLEBRACKET) {
            self.next_token()?;

            return Ok(Type2::Unwrap {
              comments,
              ident,
              generic_args: Some(self.parse_genericargs()?),
              #[cfg(feature = "ast-span")]
              span: (0, 0, 0),
            });
          }

          return Ok(Type2::Unwrap {
            comments,
            ident,
            generic_args: None,
            #[cfg(feature = "ast-span")]
            span: (0, 0, 0),
          });
        }

        self.errors.push(Error::PARSER {
          #[cfg(feature = "ast-span")]
          position: self.parser_position,
          msg: InvalidUnwrapSyntax.into(),
        });

        Err(Error::INCREMENTAL)
      }

      // & ( group )
      // & groupname [genericarg]
      Token::GTOCHOICE => {
        #[cfg(feature = "ast-span")]
        let begin_type2_range = self.lexer_position.range.0;
        #[cfg(feature = "ast-span")]
        let begin_type2_line = self.lexer_position.line;

        self.next_token()?;

        let comments = self.collect_comments()?;

        match &self.cur_token {
          Token::LPAREN => {
            self.next_token()?;

            let comments_before_group = self.collect_comments()?;

            let group = self.parse_group()?;

            let comments_after_group = self.collect_comments()?;

            Ok(Type2::ChoiceFromInlineGroup {
              comments,
              comments_before_group,
              group,
              comments_after_group,
              #[cfg(feature = "ast-span")]
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

              let generic_args = Some(self.parse_genericargs()?);

              return Ok(Type2::ChoiceFromGroup {
                comments,
                ident,
                generic_args,
                #[cfg(feature = "ast-span")]
                span: (
                  begin_type2_range,
                  self.parser_position.range.1,
                  begin_type2_line,
                ),
              });
            }

            #[cfg(feature = "ast-span")]
            {
              self.parser_position.range.1 = self.lexer_position.range.1;
            }

            Ok(Type2::ChoiceFromGroup {
              comments,
              ident,
              generic_args: None,
              #[cfg(feature = "ast-span")]
              span: (
                begin_type2_range,
                self.parser_position.range.1,
                begin_type2_line,
              ),
            })
          }
          _ => {
            self.errors.push(Error::PARSER {
              #[cfg(feature = "ast-span")]
              position: self.parser_position,
              msg: InvalidGroupToChoiceEnumSyntax.into(),
            });
            Err(Error::INCREMENTAL)
          }
        }
      }

      // # 6 ["." uint] ( type )
      // # DIGIT ["." uint]   ; major/ai
      // #                    ; any
      // Token::TAG(tag) => match tag {
      //   Tag::DATA(data) => Ok(Type2::TaggedData(data.clone())),
      //   Tag::MAJORTYPE(mt) => Ok(Type2::DataMajorType(*mt)),
      //   Tag::ANY => Ok(Type2::Any),
      // },
      Token::TAG(t) => {
        #[cfg(feature = "ast-span")]
        let begin_type2_range = self.lexer_position.range.0;
        #[cfg(feature = "ast-span")]
        let begin_type2_line = self.lexer_position.line;

        match *t {
          // Tagged data item containing the given type as the tagged value
          (Some(6), tag) => {
            self.next_token()?;
            if !self.cur_token_is(Token::LPAREN) {
              self.errors.push(Error::PARSER {
                #[cfg(feature = "ast-span")]
                position: self.parser_position,
                msg: InvalidTagSyntax.into(),
              });

              return Err(Error::INCREMENTAL);
            }

            self.next_token()?;

            let comments_before_type = self.collect_comments()?;

            let t = self.parse_type(None)?;

            let comments_after_type = self.collect_comments()?;

            if !self.cur_token_is(Token::RPAREN) {
              self.errors.push(Error::PARSER {
                #[cfg(feature = "ast-span")]
                position: self.parser_position,
                msg: InvalidTagSyntax.into(),
              });

              return Err(Error::INCREMENTAL);
            }

            Ok(Type2::TaggedData {
              tag,
              comments_before_type,
              t,
              comments_after_type,
              #[cfg(feature = "ast-span")]
              span: (
                begin_type2_range,
                self.parser_position.range.1,
                begin_type2_line,
              ),
            })
          }
          // Tagged data of a major type
          (Some(mt), constraint) => Ok(Type2::DataMajorType {
            mt,
            constraint,
            #[cfg(feature = "ast-span")]
            span: (
              begin_type2_range,
              self.lexer_position.range.1,
              begin_type2_line,
            ),
          }),
          #[cfg(feature = "ast-span")]
          _ => Ok(Type2::Any((
            begin_type2_range,
            self.lexer_position.range.1,
            begin_type2_line,
          ))),
          #[cfg(not(feature = "ast-span"))]
          _ => Ok(Type2::Any),
        }
      }
      _ => {
        self.collect_comments()?;

        match self.cur_token.in_standard_prelude() {
          Some(s) => {
            let ident = self.identifier_from_ident_token((s, None));
            #[cfg(feature = "ast-span")]
            {
              self.parser_position.range = self.lexer_position.range;
              self.parser_position.line = self.lexer_position.line;
            }

            Ok(Type2::Typename {
              ident,
              generic_args: None,
              #[cfg(feature = "ast-span")]
              span: (
                self.parser_position.range.0,
                self.parser_position.range.1,
                self.parser_position.line,
              ),
            })
          }
          None => {
            #[cfg(feature = "ast-span")]
            {
              self.parser_position.line = self.lexer_position.line;
              self.parser_position.range = self.lexer_position.range;
            }

            if self.cur_token_is(Token::COLON) || self.cur_token_is(Token::ARROWMAP) {
              self.errors.push(Error::PARSER {
                #[cfg(feature = "ast-span")]
                position: self.parser_position,
                msg: MissingGroupEntryMemberKey.into(),
              });

              return Err(Error::INCREMENTAL);
            }

            if self.cur_token_is(Token::RBRACE)
              || self.cur_token_is(Token::RBRACKET)
              || self.cur_token_is(Token::RPAREN)
            {
              self.errors.push(Error::PARSER {
                #[cfg(feature = "ast-span")]
                position: self.parser_position,
                msg: MissingGroupEntry.into(),
              });

              return Err(Error::INCREMENTAL);
            }

            self.errors.push(Error::PARSER {
              #[cfg(feature = "ast-span")]
              position: self.parser_position,
              msg: InvalidGroupEntrySyntax.into(),
            });

            Err(Error::INCREMENTAL)
          }
        }
      }
    };

    #[cfg(feature = "ast-span")]
    {
      self.parser_position.range.1 = self.lexer_position.range.1;
    }

    self.next_token()?;

    t2
  }

  fn parse_group(&mut self) -> Result<Group<'a>> {
    #[cfg(feature = "ast-span")]
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
      #[cfg(feature = "ast-span")]
      span: (begin_group_range, 0, self.lexer_position.line),
    };

    group.group_choices.push(self.parse_grpchoice()?);

    while self.cur_token_is(Token::GCHOICE) {
      group.group_choices.push(self.parse_grpchoice()?);
    }

    #[cfg(feature = "ast-span")]
    {
      group.span.1 = self.parser_position.range.1;
    }

    if let Some(cd) = closing_delimiter.as_ref() {
      if cd != &self.cur_token {
        self.errors.push(Error::PARSER {
          #[cfg(feature = "ast-span")]
          position: self.lexer_position,
          msg: MissingClosingDelimiter.into(),
        });

        return Err(Error::INCREMENTAL);
      }
    }

    Ok(group)
  }

  fn parse_grpchoice(&mut self) -> Result<GroupChoice<'a>> {
    let mut grpchoice = GroupChoice {
      group_entries: Vec::new(),
      comments_before_grpchoice: None,
      #[cfg(feature = "ast-span")]
      span: (self.lexer_position.range.0, 0, self.lexer_position.line),
    };

    if self.cur_token_is(Token::GCHOICE) {
      self.next_token()?;

      grpchoice.comments_before_grpchoice = self.collect_comments()?;

      #[cfg(feature = "ast-span")]
      {
        grpchoice.span.0 = self.lexer_position.range.0;
      }
    } else if self.cur_token_is(Token::LBRACE) || self.cur_token_is(Token::LBRACKET) {
      self.next_token()?;

      #[cfg(feature = "ast-span")]
      {
        grpchoice.span.0 = self.lexer_position.range.0;
      }
    };

    grpchoice.comments_before_grpchoice = self.collect_comments()?;

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
        grpchoice.group_entries.push((
          ge,
          OptionalComma {
            optional_comma: false,
            trailing_comments: None,
          },
        ));

        #[cfg(feature = "ast-span")]
        {
          grpchoice.span.1 = self.parser_position.range.1;
        }

        return Ok(grpchoice);
      }

      // Don't advance the token if it is part of a member key, comma or an
      // opening or closing map/group delimiter. Otherwise, advance
      if !self.cur_token_is(Token::RPAREN)
        && !self.cur_token_is(Token::RBRACE)
        && !self.cur_token_is(Token::RBRACKET)
        && !self.cur_token_is(Token::LPAREN)
        && !self.cur_token_is(Token::LBRACE)
        && !self.cur_token_is(Token::LBRACKET)
        && !self.cur_token_is(Token::COMMA)
        && !self.cur_token_is(Token::OPTIONAL)
        && !self.cur_token_is(Token::ONEORMORE)
        && !self.cur_token_is(Token::ASTERISK)
        && !self.peek_token_is(&Token::COLON)
        && !self.peek_token_is(&Token::ARROWMAP)
        && !self.cur_token_is(Token::EOF)
      {
        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range.1 = self.lexer_position.range.1;
        }
        self.next_token()?;
      }

      let mut optional_comma = false;

      if self.cur_token_is(Token::COMMA) {
        optional_comma = true;

        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range.1 = self.lexer_position.range.1;
        }
        self.next_token()?;
      }

      let trailing_comments = self.collect_comments()?;

      grpchoice.group_entries.push((
        ge,
        OptionalComma {
          optional_comma,
          trailing_comments,
        },
      ));
    }

    #[cfg(feature = "ast-span")]
    {
      grpchoice.span.1 = self.parser_position.range.1;
    }

    Ok(grpchoice)
  }

  fn parse_grpent(&mut self, from_rule: bool) -> Result<GroupEntry<'a>> {
    #[cfg(feature = "ast-span")]
    let begin_grpent_range = self.lexer_position.range.0;
    #[cfg(feature = "ast-span")]
    let begin_grpent_line = self.lexer_position.line;

    let occur = self.parse_occur(true)?;

    // If parsing group entry from a rule, set member key to none
    let member_key = if from_rule {
      None
    } else {
      self.parse_memberkey(true)?
    };

    if self.cur_token_is(Token::LPAREN) && member_key.is_none() {
      self.next_token()?;

      let comments_before_group = self.collect_comments()?;

      let group = self.parse_group()?;

      #[cfg(feature = "ast-span")]
      let mut span = (
        begin_grpent_range,
        self.parser_position.range.1,
        begin_grpent_line,
      );

      #[cfg(feature = "ast-span")]
      {
        self.parser_position.range.1 = self.lexer_position.range.1;
      }

      let comments_after_group = self.collect_comments()?;

      if !self.cur_token_is(Token::RPAREN) {
        self.errors.push(Error::PARSER {
          #[cfg(feature = "ast-span")]
          position: self.lexer_position,
          msg: MissingClosingParend.into(),
        });
        return Err(Error::INCREMENTAL);
      }

      #[cfg(feature = "ast-span")]
      {
        span.1 = self.parser_position.range.1;
      }

      self.next_token()?;

      return Ok(GroupEntry::InlineGroup {
        occur,
        group,
        comments_before_group,
        comments_after_group,
        #[cfg(feature = "ast-span")]
        span,
      });
    }

    #[cfg(feature = "ast-span")]
    let mut span = (
      begin_grpent_range,
      self.parser_position.range.1,
      begin_grpent_line,
    );

    match member_key {
      Some(MemberKey::NonMemberKey {
        non_member_key: NonMemberKey::Type(mut entry_type),
        comments_before_type_or_group,
        comments_after_type_or_group,
      }) => {
        #[cfg(feature = "ast-span")]
        if self.cur_token_is(Token::COMMA) {
          span.1 = self.lexer_position.range.1;
        }

        let trailing_comments = entry_type.comments_after_type();

        #[cfg(feature = "ast-span")]
        if let Some((name, generic_args, _)) = entry_type.groupname_entry() {
          return Ok(GroupEntry::TypeGroupname {
            ge: TypeGroupnameEntry {
              occur,
              name,
              generic_args,
            },
            leading_comments: comments_before_type_or_group,
            trailing_comments,
            span,
          });
        }

        #[cfg(not(feature = "ast-span"))]
        if let Some((name, generic_args)) = entry_type.groupname_entry() {
          return Ok(GroupEntry::TypeGroupname {
            ge: TypeGroupnameEntry {
              occur,
              name,
              generic_args,
            },
            leading_comments: comments_before_type_or_group,
            trailing_comments,
          });
        }

        // A parse tree that returns a type instead of a member key needs to
        // advance the token in the case of "(", "{" or "[". Otherwise, infinite
        // recursive loop occurs
        if let Token::LPAREN | Token::LBRACE | Token::LBRACKET = self.cur_token {
          self.next_token()?;
        }

        let trailing_comments = if let Some(comments) = entry_type.comments_after_type() {
          Some(comments)
        } else {
          comments_after_type_or_group
        };

        Ok(GroupEntry::ValueMemberKey {
          ge: Box::from(ValueMemberKeyEntry {
            occur,
            member_key: None,
            entry_type,
          }),
          leading_comments: comments_before_type_or_group,
          trailing_comments,
          #[cfg(feature = "ast-span")]
          span,
        })
      }
      Some(MemberKey::NonMemberKey {
        non_member_key: NonMemberKey::Group(group),
        comments_before_type_or_group,
        comments_after_type_or_group,
      }) => {
        #[cfg(feature = "ast-span")]
        if self.cur_token_is(Token::COMMA) {
          span.1 = self.lexer_position.range.1;
        }

        Ok(GroupEntry::InlineGroup {
          occur,
          group,
          #[cfg(feature = "ast-span")]
          span,
          comments_before_group: comments_before_type_or_group,
          comments_after_group: comments_after_type_or_group,
        })
      }
      member_key @ Some(_) => {
        let mut entry_type = self.parse_type(None)?;

        let trailing_comments = entry_type.comments_after_type();

        #[cfg(feature = "ast-span")]
        {
          span.1 = self.parser_position.range.1;
        }

        #[cfg(feature = "ast-span")]
        if self.cur_token_is(Token::COMMA) {
          span.1 = self.lexer_position.range.1;
        }

        Ok(GroupEntry::ValueMemberKey {
          ge: Box::from(ValueMemberKeyEntry {
            occur,
            member_key,
            entry_type,
          }),
          leading_comments: None,
          trailing_comments,
          #[cfg(feature = "ast-span")]
          span,
        })
      }
      None => {
        let mut entry_type = self.parse_type(None)?;

        #[cfg(feature = "ast-span")]
        {
          span.1 = self.parser_position.range.1;
        }

        let trailing_comments = if let Some(comments) = entry_type.comments_after_type() {
          Some(comments)
        } else {
          self.collect_comments()?
        };

        #[cfg(feature = "ast-span")]
        if self.cur_token_is(Token::COMMA) {
          span.1 = self.lexer_position.range.1;
        }

        #[cfg(feature = "ast-span")]
        if let Some((name, generic_args, _)) = entry_type.groupname_entry() {
          if generic_args.is_some() && self.peek_token_is(&Token::LANGLEBRACKET) {
            while !self.peek_token_is(&Token::RANGLEBRACKET) {
              self.next_token()?;
            }

            self.next_token()?;
          }

          return Ok(GroupEntry::TypeGroupname {
            ge: TypeGroupnameEntry {
              occur,
              name,
              generic_args,
            },
            leading_comments: None,
            trailing_comments,
            span,
          });
        }

        #[cfg(not(feature = "ast-span"))]
        if let Some((name, generic_args)) = entry_type.groupname_entry() {
          if generic_args.is_some() && self.peek_token_is(&Token::LANGLEBRACKET) {
            while !self.peek_token_is(&Token::RANGLEBRACKET) {
              self.next_token()?;
            }

            self.next_token()?;
          }

          return Ok(GroupEntry::TypeGroupname {
            ge: TypeGroupnameEntry {
              occur,
              name,
              generic_args,
            },
            leading_comments: None,
            trailing_comments,
          });
        }

        Ok(GroupEntry::ValueMemberKey {
          ge: Box::from(ValueMemberKeyEntry {
            occur,
            member_key: None,
            entry_type,
          }),
          leading_comments: None,
          trailing_comments,
          #[cfg(feature = "ast-span")]
          span,
        })
      }
    }
  }

  // An ident memberkey could one of the following:
  //    type1 S ["^" S] "=>"
  //  / bareword S ":
  fn parse_memberkey_from_ident(
    &mut self,
    is_optional: bool,
    ident: (&'a str, Option<token::SocketPlug>),
    #[cfg(feature = "ast-span")] begin_memberkey_range: usize,
    #[cfg(feature = "ast-span")] begin_memberkey_line: usize,
  ) -> Result<Option<MemberKey<'a>>> {
    if !self.peek_token_is(&Token::COLON)
      && !self.peek_token_is(&Token::ARROWMAP)
      && !self.peek_token_is(&Token::CUT)
      && is_optional
    {
      return Ok(None);
    }

    #[cfg(feature = "ast-span")]
    {
      self.parser_position.range.1 = self.peek_lexer_position.range.1;
    }

    #[cfg(feature = "ast-span")]
    let end_t1_range = self.lexer_position.range.1;

    #[cfg(feature = "ast-span")]
    let mut ident = self.identifier_from_ident_token((ident.0, ident.1));
    #[cfg(not(feature = "ast-span"))]
    let ident = self.identifier_from_ident_token((ident.0, ident.1));
    #[cfg(feature = "ast-span")]
    {
      ident.span = (begin_memberkey_range, end_t1_range, begin_memberkey_line);
    }

    self.next_token()?;

    let comments_before_cut = self.collect_comments()?;

    let mk = if self.cur_token_is(Token::CUT) {
      self.next_token()?;

      let comments_after_cut = self.collect_comments()?;

      if !self.cur_token_is(Token::ARROWMAP) {
        self.errors.push(Error::PARSER {
          #[cfg(feature = "ast-span")]
          position: self.lexer_position,
          msg: InvalidMemberKeyArrowMapSyntax.into(),
        });
        return Err(Error::INCREMENTAL);
      }

      #[cfg(feature = "ast-span")]
      let end_memberkey_range = self.lexer_position.range.1;

      let comments_after_arrowmap = if let Token::COMMENT(_) = self.peek_token {
        self.next_token()?;

        self.collect_comments()?
      } else {
        None
      };

      let t1 = MemberKey::Type1 {
        t1: Box::from(Type1 {
          type2: Type2::Typename {
            ident,
            generic_args: None,
            #[cfg(feature = "ast-span")]
            span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
          },
          operator: None,
          comments_after_type: None,
          #[cfg(feature = "ast-span")]
          span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
        }),
        comments_before_cut,
        is_cut: true,
        comments_after_cut,
        comments_after_arrowmap,
        #[cfg(feature = "ast-span")]
        span: (
          begin_memberkey_range,
          end_memberkey_range,
          begin_memberkey_line,
        ),
      };

      self.next_token()?;

      Some(t1)
    } else if self.cur_token_is(Token::ARROWMAP) {
      #[cfg(feature = "ast-span")]
      let end_memberkey_range = self.lexer_position.range.1;

      let comments_after_arrowmap = if let Token::COMMENT(_) = self.peek_token {
        self.next_token()?;

        self.collect_comments()?
      } else {
        None
      };

      let t1 = MemberKey::Type1 {
        t1: Box::from(Type1 {
          type2: Type2::Typename {
            ident,
            generic_args: None,
            #[cfg(feature = "ast-span")]
            span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
          },
          operator: None,
          comments_after_type: None,
          #[cfg(feature = "ast-span")]
          span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
        }),
        comments_before_cut,
        is_cut: false,
        comments_after_cut: None,
        comments_after_arrowmap,
        #[cfg(feature = "ast-span")]
        span: (
          begin_memberkey_range,
          end_memberkey_range,
          begin_memberkey_line,
        ),
      };

      self.next_token()?;

      Some(t1)
    } else {
      if self.cur_token_is(Token::COLON) {
        self.next_token()?;
      }

      let comments_after_colon = self.collect_comments()?;

      Some(MemberKey::Bareword {
        ident,
        comments: comments_before_cut,
        comments_after_colon,
        #[cfg(feature = "ast-span")]
        span: (
          begin_memberkey_range,
          self.parser_position.range.1,
          begin_memberkey_line,
        ),
      })
    };

    Ok(mk)
  }

  fn parse_memberkey(&mut self, is_optional: bool) -> Result<Option<MemberKey<'a>>> {
    #[cfg(feature = "ast-span")]
    let begin_memberkey_range = self.lexer_position.range.0;
    #[cfg(feature = "ast-span")]
    let begin_memberkey_line = self.lexer_position.line;

    if let Some(t) = self.cur_token.in_standard_prelude() {
      return self.parse_memberkey_from_ident(
        is_optional,
        (t, None),
        #[cfg(feature = "ast-span")]
        begin_memberkey_range,
        #[cfg(feature = "ast-span")]
        begin_memberkey_line,
      );
    }

    match &self.cur_token {
      Token::IDENT(ident) => {
        let ident = *ident;

        self.parse_memberkey_from_ident(
          is_optional,
          ident,
          #[cfg(feature = "ast-span")]
          begin_memberkey_range,
          #[cfg(feature = "ast-span")]
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

        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range.1 = self.peek_lexer_position.range.1;
        }

        let value = value.clone();

        let t1 = self.parse_type1(None)?;

        let comments_before_cut = self.collect_comments()?;

        let mk = if self.cur_token_is(Token::CUT) {
          self.next_token()?;

          let comments_after_cut = self.collect_comments()?;

          if !self.cur_token_is(Token::ARROWMAP) {
            self.errors.push(Error::PARSER {
              #[cfg(feature = "ast-span")]
              position: self.lexer_position,
              msg: InvalidMemberKeyArrowMapSyntax.into(),
            });
            return Err(Error::INCREMENTAL);
          }

          #[cfg(feature = "ast-span")]
          let end_memberkey_range = self.lexer_position.range.1;

          self.next_token()?;

          let memberkey_comments = self.collect_comments()?;

          Some(MemberKey::Type1 {
            t1: Box::from(t1),
            comments_before_cut,
            is_cut: true,
            comments_after_cut,
            comments_after_arrowmap: memberkey_comments,
            #[cfg(feature = "ast-span")]
            span: (
              begin_memberkey_range,
              end_memberkey_range,
              begin_memberkey_line,
            ),
          })
        } else {
          let comments = self.collect_comments()?;

          if !self.cur_token_is(Token::ARROWMAP) && !self.cur_token_is(Token::COLON) {
            self.errors.push(Error::PARSER {
              #[cfg(feature = "ast-span")]
              position: self.lexer_position,
              msg: InvalidMemberKeySyntax.into(),
            });
            return Err(Error::INCREMENTAL);
          }

          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range.1 = self.lexer_position.range.1;
          }

          self.next_token()?;

          let memberkey_comments = self.collect_comments()?;

          Some(MemberKey::Value {
            value,
            comments,
            comments_after_colon: memberkey_comments,
            #[cfg(feature = "ast-span")]
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
        #[cfg(feature = "ast-span")]
        let begin_memberkey_range = self.lexer_position.range.0;
        #[cfg(feature = "ast-span")]
        let begin_memberkey_line = self.lexer_position.line;

        let mut nested_parend_count = 0;

        self.next_token()?;

        let comments_before_type_or_group = self.collect_comments()?;

        let mut tokens: Vec<lexer::Item> = Vec::new();

        let mut comments_after_type_or_group = None;

        let mut has_group_entries = false;
        let mut closing_parend = false;
        #[cfg(feature = "ast-span")]
        let mut closing_parend_index = 0;
        while !closing_parend {
          if self.cur_token_is(Token::ARROWMAP) || self.cur_token_is(Token::COLON) {
            has_group_entries = true;
          }

          // TODO: parse nested comments
          if self.cur_token_is(Token::LPAREN) {
            nested_parend_count += 1;
          }

          if self.cur_token_is(Token::RPAREN) {
            match nested_parend_count.cmp(&0) {
              Ordering::Greater => nested_parend_count -= 1,
              Ordering::Equal | Ordering::Less => {
                closing_parend = true;
                #[cfg(feature = "ast-span")]
                {
                  closing_parend_index = self.lexer_position.range.1;
                }
              }
            }
          }

          let t = self.cur_token.clone();
          tokens.push(Ok((self.lexer_position, t)));

          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range.1 = self.lexer_position.range.1;
          }

          self.next_token()?;

          comments_after_type_or_group = self.collect_comments()?;

          if self.cur_token_is(Token::EOF) {
            self.errors.push(Error::PARSER {
              #[cfg(feature = "ast-span")]
              position: self.lexer_position,
              msg: MissingClosingParend.into(),
            });

            return Err(Error::INCREMENTAL);
          }
        }

        // Parse tokens vec as group
        if has_group_entries {
          let mut p = Parser::new(tokens.into_iter(), self.str_input)?;
          let group = match p.parse_group() {
            Ok(g) => g,
            Err(Error::INCREMENTAL) => {
              for e in p.errors.into_iter() {
                self.errors.push(e);
              }

              return Err(Error::INCREMENTAL);
            }
            Err(e) => return Err(e),
          };

          return Ok(Some(MemberKey::NonMemberKey {
            non_member_key: NonMemberKey::Group(group),
            comments_before_type_or_group,
            comments_after_type_or_group,
          }));
        }

        // Parse tokens vec as type
        let mut p = Parser::new(tokens.into_iter(), self.str_input)?;
        let t = match p.parse_type(None) {
          Ok(t) => t,
          Err(Error::INCREMENTAL) => {
            for e in p.errors.into_iter() {
              self.errors.push(e);
            }

            return Err(Error::INCREMENTAL);
          }
          Err(e) => return Err(e),
        };

        let comments_before_cut = self.collect_comments()?;

        if self.cur_token_is(Token::CUT) {
          self.next_token()?;

          let comments_after_cut = self.collect_comments()?;

          if !self.cur_token_is(Token::ARROWMAP) {
            self.errors.push(Error::PARSER {
              #[cfg(feature = "ast-span")]
              position: self.lexer_position,
              msg: InvalidMemberKeyArrowMapSyntax.into(),
            });
            return Err(Error::INCREMENTAL);
          }

          #[cfg(feature = "ast-span")]
          let end_memberkey_range = self.lexer_position.range.1;

          let t1 = Some(MemberKey::Type1 {
            t1: Box::from(Type1 {
              type2: Type2::ParenthesizedType {
                pt: t,
                comments_before_type: comments_before_type_or_group,
                comments_after_type: comments_after_type_or_group,
                #[cfg(feature = "ast-span")]
                span: (
                  begin_memberkey_range,
                  closing_parend_index,
                  begin_memberkey_line,
                ),
              },
              comments_after_type: comments_before_cut.clone(),
              operator: None,
              #[cfg(feature = "ast-span")]
              span: (
                begin_memberkey_range,
                closing_parend_index,
                begin_memberkey_line,
              ),
            }),
            comments_before_cut,
            is_cut: true,
            comments_after_cut,
            comments_after_arrowmap: None,
            #[cfg(feature = "ast-span")]
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

          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range.1 = self.lexer_position.range.1;
          }

          let memberkey_comments = self.collect_comments()?;

          Some(MemberKey::Type1 {
            t1: Box::from(Type1 {
              type2: Type2::ParenthesizedType {
                pt: t,
                comments_before_type: comments_before_type_or_group,
                comments_after_type: comments_after_type_or_group,
                #[cfg(feature = "ast-span")]
                span: (
                  begin_memberkey_range,
                  closing_parend_index,
                  begin_memberkey_line,
                ),
              },
              comments_after_type: comments_before_cut.clone(),
              operator: None,
              #[cfg(feature = "ast-span")]
              span: (
                begin_memberkey_range,
                closing_parend_index,
                begin_memberkey_line,
              ),
            }),
            comments_before_cut,
            is_cut: false,
            comments_after_cut: None,
            comments_after_arrowmap: memberkey_comments,
            #[cfg(feature = "ast-span")]
            span: (
              begin_memberkey_range,
              self.lexer_position.range.0,
              begin_memberkey_line,
            ),
          })
        } else {
          Some(MemberKey::NonMemberKey {
            non_member_key: NonMemberKey::Type(Type {
              type_choices: t.type_choices,
              #[cfg(feature = "ast-span")]
              span: (
                begin_memberkey_range,
                self.parser_position.range.1,
                begin_memberkey_line,
              ),
            }),
            comments_before_type_or_group,
            comments_after_type_or_group,
          })
        };

        Ok(t1)
      }
      _ => {
        let t1 = self.parse_type1(None)?;

        let comments_before_cut = self.collect_comments()?;

        if self.cur_token_is(Token::CUT) {
          self.next_token()?;

          let comments_after_cut = self.collect_comments()?;

          if !self.cur_token_is(Token::ARROWMAP) {
            self.errors.push(Error::PARSER {
              #[cfg(feature = "ast-span")]
              position: self.lexer_position,
              msg: InvalidMemberKeyArrowMapSyntax.into(),
            });
            return Err(Error::INCREMENTAL);
          }

          #[cfg(feature = "ast-span")]
          let end_memberkey_range = self.lexer_position.range.1;

          self.next_token()?;

          let memberkey_comments = self.collect_comments()?;

          return Ok(Some(MemberKey::Type1 {
            t1: Box::from(t1),
            comments_before_cut,
            is_cut: true,
            comments_after_cut,
            comments_after_arrowmap: memberkey_comments,
            #[cfg(feature = "ast-span")]
            span: (
              begin_memberkey_range,
              end_memberkey_range,
              begin_memberkey_line,
            ),
          }));
        }

        let t1 = if self.cur_token_is(Token::ARROWMAP) {
          self.next_token()?;

          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range.1 = self.lexer_position.range.1;
          }

          let memberkey_comments = self.collect_comments()?;

          Some(MemberKey::Type1 {
            t1: Box::from(t1),
            comments_before_cut,
            is_cut: false,
            comments_after_cut: None,
            comments_after_arrowmap: memberkey_comments,
            #[cfg(feature = "ast-span")]
            span: (
              begin_memberkey_range,
              self.parser_position.range.1,
              begin_memberkey_line,
            ),
          })
        } else {
          Some(MemberKey::NonMemberKey {
            non_member_key: NonMemberKey::Type(Type {
              type_choices: vec![TypeChoice {
                comments_before_type: None,
                comments_after_type: None,
                type1: t1,
              }],
              #[cfg(feature = "ast-span")]
              span: (
                begin_memberkey_range,
                self.parser_position.range.1,
                begin_memberkey_line,
              ),
            }),
            comments_before_type_or_group: None,
            comments_after_type_or_group: comments_before_cut,
          })
        };

        Ok(t1)
      }
    }
  }

  fn parse_occur(&mut self, is_optional: bool) -> Result<Option<Occurrence<'a>>> {
    #[cfg(feature = "ast-span")]
    let begin_occur_range = self.lexer_position.range.0;
    #[cfg(feature = "ast-span")]
    let begin_occur_line = self.lexer_position.line;
    #[cfg(feature = "ast-span")]
    {
      self.parser_position.line = self.lexer_position.line;
    }

    match &self.cur_token {
      Token::OPTIONAL => {
        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range = self.lexer_position.range;
        }

        self.next_token()?;

        let comments = self.collect_comments()?;

        Ok(Some(Occurrence {
          #[cfg(feature = "ast-span")]
          occur: Occur::Optional((
            self.parser_position.range.0,
            self.parser_position.range.1,
            self.parser_position.line,
          )),
          #[cfg(not(feature = "ast-span"))]
          occur: Occur::Optional,
          comments,
        }))
      }
      Token::ONEORMORE => {
        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range = self.lexer_position.range;
        }

        self.next_token()?;

        let comments = self.collect_comments()?;

        Ok(Some(Occurrence {
          #[cfg(feature = "ast-span")]
          occur: Occur::OneOrMore((
            self.parser_position.range.0,
            self.parser_position.range.1,
            self.parser_position.line,
          )),
          #[cfg(not(feature = "ast-span"))]
          occur: Occur::OneOrMore,
          comments,
        }))
      }
      Token::ASTERISK => {
        let occur = if let Token::VALUE(token::Value::UINT(u)) = &self.peek_token {
          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range.0 = self.lexer_position.range.0;
            self.parser_position.range.1 = self.peek_lexer_position.range.1;
          }

          Occur::Exact {
            lower: None,
            upper: Some(*u),
            #[cfg(feature = "ast-span")]
            span: (
              self.parser_position.range.0,
              self.parser_position.range.1,
              self.parser_position.line,
            ),
          }
        } else {
          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range = self.lexer_position.range;
            Occur::ZeroOrMore((
              self.parser_position.range.0,
              self.parser_position.range.1,
              self.parser_position.line,
            ))
          }

          #[cfg(not(feature = "ast-span"))]
          Occur::ZeroOrMore
        };

        self.next_token()?;

        if let Token::VALUE(token::Value::UINT(_)) = &self.cur_token {
          self.next_token()?;
        }

        let comments = self.collect_comments()?;

        Ok(Some(Occurrence { occur, comments }))
      }
      Token::VALUE(_) => {
        let lower = if let Token::VALUE(token::Value::UINT(li)) = &self.cur_token {
          Some(*li)
        } else {
          None
        };

        if !self.peek_token_is(&Token::ASTERISK) {
          if is_optional {
            return Ok(None);
          }

          self.errors.push(Error::PARSER {
            #[cfg(feature = "ast-span")]
            position: self.lexer_position,
            msg: InvalidOccurrenceSyntax.into(),
          });

          return Err(Error::INCREMENTAL);
        }

        self.next_token()?;

        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range.1 = self.lexer_position.range.1;
        }

        self.next_token()?;

        let upper = if let Token::VALUE(token::Value::UINT(ui)) = &self.cur_token {
          let ui = *ui;

          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range.1 = self.lexer_position.range.1;
          }

          self.next_token()?;

          Some(ui)
        } else {
          None
        };

        let comments = self.collect_comments()?;

        Ok(Some(Occurrence {
          occur: Occur::Exact {
            lower,
            upper,
            #[cfg(feature = "ast-span")]
            span: (
              begin_occur_range,
              self.parser_position.range.1,
              begin_occur_line,
            ),
          },
          comments,
        }))
      }
      _ => Ok(None),
    }
  }

  fn cur_token_is(&self, t: Token) -> bool {
    mem::discriminant(&self.cur_token) == mem::discriminant(&t)
  }

  fn peek_token_is(&self, t: &Token) -> bool {
    mem::discriminant(&self.peek_token) == mem::discriminant(t)
  }

  fn expect_peek(&mut self, t: &Token) -> Result<bool> {
    if self.peek_token_is(t) {
      return self.next_token().map(|_| true);
    }

    Ok(false)
  }

  /// Create `ast::Identifier` from `Token::IDENT(ident)`
  fn identifier_from_ident_token(
    &self,
    ident: (&'a str, Option<token::SocketPlug>),
  ) -> Identifier<'a> {
    Identifier {
      ident: ident.0,
      socket: ident.1,
      #[cfg(feature = "ast-span")]
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
      Err(Error::INCREMENTAL) => {
        let e = if print_stderr {
          p.report_errors(true)
        } else {
          p.report_errors(false)
        };

        if let Ok(Some(e)) = e {
          return Err(e);
        }

        Err(Error::INCREMENTAL.to_string())
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
      Err(Error::INCREMENTAL) => {
        if let Some(e) = p.report_errors() {
          return Err(e);
        }

        Err(Error::INCREMENTAL.to_string())
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
/// ```typescript
/// import * as wasm from 'cddl';
///
/// let cddl: any;
/// try {
///   cddl = wasm.cddl_from_str(text);
/// } catch (e) {
///   console.error(e);
/// }
/// ```
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn cddl_from_str(input: &str) -> result::Result<JsValue, JsValue> {
  #[derive(Serialize)]
  struct ParserError {
    position: Position,
    msg: ErrorMsg,
  }

  match Parser::new(Lexer::new(input).iter(), input) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(c) => JsValue::from_serde(&c).map_err(|e| JsValue::from(e.to_string())),
      Err(Error::INCREMENTAL) => {
        if !p.errors.is_empty() {
          return Err(
            JsValue::from_serde(
              &p.errors
                .iter()
                .filter_map(|e| {
                  if let Error::PARSER { position, msg } = e {
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

        Err(JsValue::from(Error::INCREMENTAL.to_string()))
      }
      Err(e) => Err(JsValue::from(e.to_string())),
    },
    Err(e) => Err(JsValue::from(e.to_string())),
  }
}

#[cfg(feature = "lsp")]
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
/// Formats cddl from input string
pub fn format_cddl_from_str(input: &str) -> result::Result<String, JsValue> {
  #[derive(Serialize)]
  struct ParserError {
    position: Position,
    msg: ErrorMsg,
  }

  match Parser::new(Lexer::new(input).iter(), input) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(c) => Ok(c.to_string()),
      Err(Error::INCREMENTAL) => {
        if !p.errors.is_empty() {
          return Err(
            JsValue::from_serde(
              &p.errors
                .iter()
                .filter_map(|e| {
                  if let Error::PARSER { position, msg } = e {
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

        Err(JsValue::from(Error::INCREMENTAL.to_string()))
      }
      Err(e) => Err(JsValue::from(e.to_string())),
    },
    Err(e) => Err(JsValue::from(e.to_string())),
  }
}

#[cfg(test)]
#[allow(unused_imports)]
#[cfg(feature = "ast-span")]
mod tests {
  use super::{
    super::{ast, lexer::Lexer, token::SocketPlug},
    *,
  };
  use indoc::indoc;
  use pretty_assertions::assert_eq;

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
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
          let e = p.report_errors(false).unwrap().unwrap();

          #[cfg(feature = "std")]
          println!("{}", e);

          assert_eq!(
            e,
            indoc!(
              r#"
                error: parser errors
                  ┌─ input:2:1
                  │
                2 │ a = b
                  │ ^^^^^ rule with the same identifier is already defined

              "#
            )
          );
          Ok(())
        }
        #[cfg(not(feature = "std"))]
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
          assert_eq!(
            p.report_errors().unwrap(),
            indoc!(
              r#"
                error: parser errors

                   ┌── input:2:1 ───
                   │
                 2 │ a = b
                   │ ^^^^^ rule with the same identifier is already defined

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
  fn verify_genericparams() -> Result<()> {
    let input = r#"<t, v>"#;

    let mut l = Lexer::new(input);
    let gps = Parser::new(&mut l.iter(), input)?.parse_genericparm()?;

    let expected_output = GenericParams {
      params: vec![
        GenericParam {
          param: Identifier {
            ident: "t".into(),
            socket: None,
            span: (1, 2, 1),
          },
          comments_before_ident: None,
          comments_after_ident: None,
        },
        GenericParam {
          param: Identifier {
            ident: "v".into(),
            socket: None,
            span: (4, 5, 1),
          },
          comments_before_ident: None,
          comments_after_ident: None,
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
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
          let e = p.report_errors(false).unwrap().unwrap();

          #[cfg(feature = "std")]
          println!("{}", e);

          assert_eq!(
            e,
            indoc!(
              r#"
              error: parser errors
                ┌─ input:1:2
                │
              1 │ <1, 2>
                │  ^ generic parameters must be named identifiers

              "#
            )
          );
          Ok(())
        }
        #[cfg(not(feature = "std"))]
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
          assert_eq!(
            p.report_errors().unwrap(),
            indoc!(
              r#"
                error: parser errors

                   ┌── input:1:2 ───
                   │
                 1 │ <1, 2>
                   │  ^ generic parameters must be named identifiers

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
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
          let e = p.report_errors(false).unwrap().unwrap();

          println!("{}", e);

          assert_eq!(
            e,
            indoc!(
              r#"
                error: parser errors
                  ┌─ input:1:6
                  │
                1 │ rule<paramA paramB> = test
                  │      ^^^^^^^^^^^^^ generic parameters should be between angle brackets '<' and '>' and separated by a comma ','
                2 │ ruleb = rulec
                3 │ ruleb = ruled
                  │ ^^^^^^^^^^^^^ rule with the same identifier is already defined
                4 │ rulec = rulee
                5 │ rulec = rulee2
                  │ ^^^^^^^^^^^^^^ rule with the same identifier is already defined

              "#
            )
          );
          Ok(())
        }
        #[cfg(not(feature = "std"))]
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
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
                   │ ^^^^^^^^^^^^^ rule with the same identifier is already defined
                 4 │ rulec = rulee
                 5 │ rulec = rulee2
                   │ ^^^^^^^^^^^^^^ rule with the same identifier is already defined

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
  fn verify_genericargs() -> Result<()> {
    let input = r#"<"reboot", "now">"#;

    let mut l = Lexer::new(input);

    let generic_args = Parser::new(l.iter(), input)?.parse_genericargs()?;

    let expected_output = GenericArgs {
      args: vec![
        GenericArg {
          arg: Box::from(Type1 {
            type2: Type2::TextValue {
              value: "reboot".into(),
              span: (1, 9, 1),
            },
            operator: None,
            comments_after_type: None,
            span: (1, 9, 1),
          }),
          comments_before_type: None,
          comments_after_type: None,
        },
        GenericArg {
          arg: Box::from(Type1 {
            type2: Type2::TextValue {
              value: "now".into(),
              span: (11, 16, 1),
            },
            operator: None,
            comments_after_type: None,
            span: (11, 16, 1),
          }),
          comments_before_type: None,
          comments_after_type: None,
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
      type_choices: vec![TypeChoice {
        type1: Type1 {
          type2: Type2::ParenthesizedType {
            pt: Type {
              type_choices: vec![
                TypeChoice {
                  type1: Type1 {
                    type2: Type2::Typename {
                      ident: Identifier {
                        ident: "tchoice1".into(),
                        socket: None,
                        span: (2, 10, 1),
                      },
                      generic_args: None,
                      span: (2, 10, 1),
                    },
                    operator: None,
                    comments_after_type: None,
                    span: (2, 10, 1),
                  },
                  comments_before_type: None,
                  comments_after_type: None,
                },
                TypeChoice {
                  type1: Type1 {
                    type2: Type2::Typename {
                      ident: Identifier {
                        ident: "tchoice2".into(),
                        socket: None,
                        span: (13, 21, 1),
                      },
                      generic_args: None,
                      span: (13, 21, 1),
                    },
                    operator: None,
                    comments_after_type: None,
                    span: (13, 21, 1),
                  },
                  comments_before_type: None,
                  comments_after_type: None,
                },
              ],
              span: (2, 21, 1),
            },
            comments_before_type: None,
            comments_after_type: None,
            span: (0, 23, 1),
          },
          operator: None,
          comments_after_type: None,
          span: (0, 23, 1),
        },
        comments_before_type: None,
        comments_after_type: None,
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
        operator: Some(Operator {
          operator: RangeCtlOp::RangeOp {
            is_inclusive: true,
            span: (1, 3, 1),
          },
          type2: Type2::UintValue {
            value: 10,
            span: (3, 5, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
        span: (0, 5, 1),
      },
      Type1 {
        type2: Type2::FloatValue {
          value: -10.5,
          span: (0, 5, 1),
        },
        operator: Some(Operator {
          operator: RangeCtlOp::RangeOp {
            is_inclusive: false,
            span: (5, 8, 1),
          },
          type2: Type2::FloatValue {
            value: 10.1,
            span: (8, 12, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
        span: (0, 12, 1),
      },
      Type1 {
        type2: Type2::FloatValue {
          value: 1.5,
          span: (0, 3, 1),
        },
        operator: Some(Operator {
          operator: RangeCtlOp::RangeOp {
            is_inclusive: true,
            span: (3, 5, 1),
          },
          type2: Type2::FloatValue {
            value: 4.5,
            span: (5, 8, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
        span: (0, 8, 1),
      },
      Type1 {
        type2: Type2::Typename {
          ident: Identifier {
            ident: "my..lower".into(),
            socket: None,
            span: (0, 9, 1),
          },
          generic_args: None,
          span: (0, 9, 1),
        },
        operator: Some(Operator {
          operator: RangeCtlOp::RangeOp {
            is_inclusive: false,
            span: (10, 13, 1),
          },
          type2: Type2::Typename {
            ident: Identifier {
              ident: "upper".into(),
              socket: None,
              span: (14, 19, 1),
            },
            generic_args: None,
            span: (14, 19, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
        span: (0, 19, 1),
      },
      Type1 {
        type2: Type2::Typename {
          ident: Identifier {
            ident: "target".into(),
            socket: None,
            span: (0, 6, 1),
          },
          generic_args: None,
          span: (0, 6, 1),
        },
        operator: Some(Operator {
          operator: RangeCtlOp::CtlOp {
            ctrl: ".lt",
            span: (7, 10, 1),
          },
          type2: Type2::Typename {
            ident: Identifier {
              ident: "controller".into(),
              socket: None,
              span: (11, 21, 1),
            },
            generic_args: None,
            span: (11, 21, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
        span: (0, 21, 1),
      },
      Type1 {
        type2: Type2::ParenthesizedType {
          pt: Type {
            type_choices: vec![
              TypeChoice {
                type1: Type1 {
                  type2: Type2::Typename {
                    ident: Identifier {
                      ident: "text".into(),
                      socket: None,
                      span: (2, 6, 1),
                    },
                    generic_args: None,
                    span: (2, 6, 1),
                  },
                  operator: None,
                  comments_after_type: None,
                  span: (2, 6, 1),
                },
                comments_before_type: None,
                comments_after_type: None,
              },
              TypeChoice {
                type1: Type1 {
                  type2: Type2::Typename {
                    ident: Identifier {
                      ident: "tstr".into(),
                      socket: None,
                      span: (9, 13, 1),
                    },
                    generic_args: None,
                    span: (9, 13, 1),
                  },
                  operator: None,
                  comments_after_type: None,
                  span: (9, 13, 1),
                },
                comments_before_type: None,
                comments_after_type: None,
              },
            ],

            span: (2, 13, 1),
          },
          comments_before_type: None,
          comments_after_type: None,
          span: (0, 15, 1),
        },
        operator: Some(Operator {
          operator: RangeCtlOp::CtlOp {
            ctrl: ".eq",
            span: (16, 19, 1),
          },
          type2: Type2::TextValue {
            value: "hello".into(),
            span: (20, 27, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
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
        generic_args: Some(GenericArgs {
          args: vec![
            GenericArg {
              arg: Box::from(Type1 {
                type2: Type2::TextValue {
                  value: "reboot".into(),
                  span: (8, 16, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (8, 16, 1),
              }),
              comments_before_type: None,
              comments_after_type: None,
            },
            GenericArg {
              arg: Box::from(Type1 {
                type2: Type2::TextValue {
                  value: "now".into(),
                  span: (18, 23, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (18, 23, 1),
              }),
              comments_before_type: None,
              comments_after_type: None,
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
        generic_args: None,
        span: (0, 12, 1),
      },
      Type2::Unwrap {
        ident: Identifier {
          ident: "group1".into(),
          socket: None,
          span: (1, 7, 1),
        },
        generic_args: None,
        comments: None,
        span: (0, 0, 0),
      },
      Type2::TaggedData {
        tag: Some(997),
        t: Type {
          type_choices: vec![TypeChoice {
            type1: Type1 {
              type2: Type2::Typename {
                ident: Identifier {
                  ident: "tstr".into(),
                  socket: None,
                  span: (7, 11, 1),
                },
                generic_args: None,
                span: (7, 11, 1),
              },
              operator: None,
              comments_after_type: None,
              span: (7, 11, 1),
            },
            comments_before_type: None,
            comments_after_type: None,
          }],
          span: (7, 11, 1),
        },
        comments_before_type: None,
        comments_after_type: None,
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
                  occur: Some(Occurrence {
                    occur: Occur::Exact {
                      lower: None,
                      upper: Some(3),
                      span: (1, 3, 1),
                    },
                    comments: None,
                  }),
                  name: Identifier {
                    ident: "reputon".into(),
                    socket: None,
                    span: (4, 11, 1),
                  },
                  generic_args: None,
                },
                leading_comments: None,
                trailing_comments: None,
                span: (1, 11, 1),
              },
              OptionalComma {
                optional_comma: false,
                trailing_comments: None,
              },
            )],
            comments_before_grpchoice: None,
            span: (1, 11, 1),
          }],
          span: (1, 11, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
        span: (0, 12, 1),
      },
      Type2::Array {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::TypeGroupname {
                ge: TypeGroupnameEntry {
                  occur: Some(Occurrence {
                    occur: Occur::OneOrMore((1, 2, 1)),
                    comments: None,
                  }),
                  name: Identifier {
                    ident: "reputon".into(),
                    socket: None,
                    span: (3, 10, 1),
                  },
                  generic_args: None,
                },
                leading_comments: None,
                trailing_comments: None,
                span: (1, 10, 1),
              },
              OptionalComma {
                optional_comma: false,
                trailing_comments: None,
              },
            )],
            comments_before_grpchoice: None,
            span: (1, 10, 1),
          }],
          span: (1, 10, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
        span: (0, 11, 1),
      },
      Type2::ChoiceFromGroup {
        ident: Identifier {
          ident: "groupname".into(),
          socket: None,
          span: (1, 10, 1),
        },
        generic_args: None,
        comments: None,
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
                  generic_args: None,
                },
                leading_comments: None,
                trailing_comments: None,
                span: (3, 14, 1),
              },
              OptionalComma {
                optional_comma: false,
                trailing_comments: None,
              },
            )],
            comments_before_grpchoice: None,
            span: (3, 14, 1),
          }],
          span: (3, 14, 1),
        },
        comments: None,
        comments_before_group: None,
        comments_after_group: None,
        span: (0, 14, 1),
      },
      Type2::Map {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::ValueMemberKey {
                ge: Box::from(ValueMemberKeyEntry {
                  occur: Some(Occurrence {
                    occur: Occur::Optional((2, 3, 1)),
                    comments: None,
                  }),
                  member_key: Some(MemberKey::Type1 {
                    t1: Box::from(Type1 {
                      type2: Type2::TextValue {
                        value: "optional-key".into(),
                        span: (4, 18, 1),
                      },
                      operator: None,
                      comments_after_type: None,
                      span: (4, 18, 1),
                    }),
                    is_cut: true,
                    comments_before_cut: None,
                    comments_after_cut: None,
                    comments_after_arrowmap: None,
                    span: (4, 23, 1),
                  }),
                  entry_type: Type {
                    type_choices: vec![TypeChoice {
                      type1: Type1 {
                        type2: Type2::Typename {
                          ident: Identifier {
                            ident: "int".into(),
                            socket: None,
                            span: (24, 27, 1),
                          },
                          generic_args: None,
                          span: (24, 27, 1),
                        },
                        operator: None,
                        comments_after_type: None,
                        span: (24, 27, 1),
                      },
                      comments_before_type: None,
                      comments_after_type: None,
                    }],
                    span: (24, 27, 1),
                  },
                }),
                leading_comments: None,
                trailing_comments: None,
                span: (2, 28, 1),
              },
              OptionalComma {
                optional_comma: true,
                trailing_comments: None,
              },
            )],
            comments_before_grpchoice: None,
            span: (2, 28, 1),
          }],
          span: (2, 28, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
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
                              comments: None,
                              comments_after_colon: None,
                              span: (4, 6, 1),
                            }),
                            entry_type: Type {
                              type_choices: vec![TypeChoice {
                                type1: Type1 {
                                  type2: Type2::Typename {
                                    ident: Identifier {
                                      ident: "int".into(),
                                      socket: None,
                                      span: (7, 10, 1),
                                    },
                                    generic_args: None,
                                    span: (7, 10, 1),
                                  },
                                  operator: None,
                                  comments_after_type: None,
                                  span: (7, 10, 1),
                                },
                                comments_before_type: None,
                                comments_after_type: None,
                              }],
                              span: (7, 10, 1),
                            },
                          }),
                          leading_comments: None,
                          trailing_comments: None,
                          span: (4, 11, 1),
                        },
                        OptionalComma {
                          optional_comma: true,
                          trailing_comments: None,
                        },
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
                              comments: None,
                              comments_after_colon: None,
                              span: (12, 14, 1),
                            }),
                            entry_type: Type {
                              type_choices: vec![TypeChoice {
                                type1: Type1 {
                                  type2: Type2::Typename {
                                    ident: Identifier {
                                      ident: "tstr".into(),
                                      socket: None,
                                      span: (15, 19, 1),
                                    },
                                    generic_args: None,
                                    span: (15, 19, 1),
                                  },
                                  operator: None,
                                  comments_after_type: None,
                                  span: (15, 19, 1),
                                },
                                comments_before_type: None,
                                comments_after_type: None,
                              }],
                              span: (15, 19, 1),
                            },
                          }),
                          leading_comments: None,
                          trailing_comments: None,
                          span: (12, 19, 1),
                        },
                        OptionalComma {
                          optional_comma: false,
                          trailing_comments: None,
                        },
                      ),
                    ],
                    comments_before_grpchoice: None,
                    span: (4, 19, 1),
                  }],
                  span: (4, 19, 1),
                },
                occur: None,
                comments_before_group: None,
                comments_after_group: None,
                span: (2, 21, 1),
              },
              OptionalComma {
                optional_comma: false,
                trailing_comments: None,
              },
            )],
            comments_before_grpchoice: None,
            span: (2, 21, 1),
          }],
          span: (2, 21, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
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
                      type_choices: vec![TypeChoice {
                        type1: Type1 {
                          type2: Type2::Array {
                            group: Group {
                              group_choices: vec![GroupChoice {
                                group_entries: vec![(
                                  GroupEntry::TypeGroupname {
                                    ge: TypeGroupnameEntry {
                                      occur: Some(Occurrence {
                                        occur: Occur::ZeroOrMore((3, 4, 1)),
                                        comments: None,
                                      }),
                                      name: Identifier {
                                        ident: "file-entry".into(),
                                        socket: None,
                                        span: (5, 15, 1),
                                      },
                                      generic_args: None,
                                    },
                                    leading_comments: None,
                                    trailing_comments: None,
                                    span: (3, 15, 1),
                                  },
                                  OptionalComma {
                                    optional_comma: false,
                                    trailing_comments: None,
                                  },
                                )],
                                comments_before_grpchoice: None,
                                span: (3, 15, 1),
                              }],
                              span: (3, 15, 1),
                            },
                            comments_before_group: None,
                            comments_after_group: None,
                            span: (2, 16, 1),
                          },
                          operator: None,
                          comments_after_type: None,
                          span: (2, 16, 1),
                        },
                        comments_before_type: None,
                        comments_after_type: None,
                      }],
                      span: (2, 16, 1),
                    },
                  }),
                  leading_comments: None,
                  trailing_comments: None,
                  span: (2, 17, 1),
                },
                OptionalComma {
                  optional_comma: true,
                  trailing_comments: None,
                },
              ),
              (
                GroupEntry::ValueMemberKey {
                  ge: Box::from(ValueMemberKeyEntry {
                    occur: None,
                    member_key: None,
                    entry_type: Type {
                      type_choices: vec![TypeChoice {
                        type1: Type1 {
                          type2: Type2::Array {
                            group: Group {
                              group_choices: vec![GroupChoice {
                                group_entries: vec![(
                                  GroupEntry::TypeGroupname {
                                    ge: TypeGroupnameEntry {
                                      occur: Some(Occurrence {
                                        occur: Occur::ZeroOrMore((19, 20, 1)),
                                        comments: None,
                                      }),
                                      name: Identifier {
                                        ident: "directory-entry".into(),
                                        socket: None,
                                        span: (21, 36, 1),
                                      },
                                      generic_args: None,
                                    },
                                    leading_comments: None,
                                    trailing_comments: None,
                                    span: (19, 36, 1),
                                  },
                                  OptionalComma {
                                    optional_comma: false,
                                    trailing_comments: None,
                                  },
                                )],
                                comments_before_grpchoice: None,
                                span: (19, 36, 1),
                              }],
                              span: (19, 36, 1),
                            },
                            comments_before_group: None,
                            comments_after_group: None,
                            span: (18, 37, 1),
                          },
                          operator: None,
                          comments_after_type: None,
                          span: (18, 37, 1),
                        },
                        comments_before_type: None,
                        comments_after_type: None,
                      }],
                      span: (18, 37, 1),
                    },
                  }),
                  leading_comments: None,
                  trailing_comments: None,
                  span: (18, 37, 1),
                },
                OptionalComma {
                  optional_comma: false,
                  trailing_comments: None,
                },
              ),
            ],
            comments_before_grpchoice: None,
            span: (2, 37, 1),
          }],
          span: (2, 37, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
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
                      generic_args: None,
                    },
                    leading_comments: None,
                    trailing_comments: None,
                    span: (2, 6, 1),
                  },
                  OptionalComma {
                    optional_comma: true,
                    trailing_comments: None,
                  },
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
                      generic_args: None,
                    },
                    leading_comments: None,
                    trailing_comments: None,
                    span: (7, 10, 1),
                  },
                  OptionalComma {
                    optional_comma: false,
                    trailing_comments: None,
                  },
                ),
              ],
              comments_before_grpchoice: None,
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
                      generic_args: None,
                    },
                    leading_comments: None,
                    trailing_comments: None,
                    span: (14, 18, 1),
                  },
                  OptionalComma {
                    optional_comma: true,
                    trailing_comments: None,
                  },
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
                      generic_args: None,
                    },
                    leading_comments: None,
                    trailing_comments: None,
                    span: (19, 23, 1),
                  },
                  OptionalComma {
                    optional_comma: false,
                    trailing_comments: None,
                  },
                ),
              ],
              comments_before_grpchoice: None,
              span: (14, 23, 1),
            },
          ],
          span: (2, 23, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
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
                    generic_args: None,
                  },
                  leading_comments: None,
                  trailing_comments: None,
                  span: (2, 6, 1),
                },
                OptionalComma {
                  optional_comma: true,
                  trailing_comments: None,
                },
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
                    generic_args: None,
                  },
                  leading_comments: None,
                  trailing_comments: None,
                  span: (7, 11, 1),
                },
                OptionalComma {
                  optional_comma: true,
                  trailing_comments: None,
                },
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
                    generic_args: None,
                  },
                  leading_comments: None,
                  trailing_comments: None,
                  span: (12, 16, 1),
                },
                OptionalComma {
                  optional_comma: true,
                  trailing_comments: None,
                },
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
                    generic_args: None,
                  },
                  leading_comments: None,
                  trailing_comments: None,
                  span: (17, 21, 1),
                },
                OptionalComma {
                  optional_comma: false,
                  trailing_comments: None,
                },
              ),
            ],
            comments_before_grpchoice: None,
            span: (2, 21, 1),
          }],
          span: (2, 21, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
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
          occur: Some(Occurrence {
            occur: Occur::ZeroOrMore((0, 1, 1)),
            comments: None,
          }),
          member_key: Some(MemberKey::Type1 {
            t1: Box::from(Type1 {
              type2: Type2::Typename {
                ident: Identifier {
                  ident: "type1".into(),
                  socket: None,
                  span: (2, 7, 1),
                },
                generic_args: None,
                span: (2, 7, 1),
              },
              operator: None,
              comments_after_type: None,
              span: (2, 7, 1),
            }),
            is_cut: true,
            comments_before_cut: None,
            comments_after_cut: None,
            comments_after_arrowmap: None,
            span: (2, 12, 1),
          }),
          entry_type: Type {
            type_choices: vec![TypeChoice {
              type1: Type1 {
                type2: Type2::TextValue {
                  value: "value".into(),
                  span: (13, 20, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (13, 20, 1),
              },
              comments_before_type: None,
              comments_after_type: None,
            }],
            span: (13, 20, 1),
          },
        }),
        leading_comments: None,
        trailing_comments: None,
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
            comments: None,
            comments_after_colon: None,
            span: (0, 6, 1),
          }),
          entry_type: Type {
            type_choices: vec![TypeChoice {
              type1: Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "type2".into(),
                    socket: None,
                    span: (7, 12, 1),
                  },
                  generic_args: None,
                  span: (7, 12, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (7, 12, 1),
              },
              comments_before_type: None,
              comments_after_type: None,
            }],
            span: (7, 12, 1),
          },
        }),
        leading_comments: None,
        trailing_comments: None,
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
          generic_args: None,
        },
        leading_comments: None,
        trailing_comments: None,
        span: (0, 8, 1),
      },
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: Some(Occurrence {
            occur: Occur::Optional((0, 1, 1)),
            comments: None,
          }),
          member_key: Some(MemberKey::Value {
            value: token::Value::UINT(0),
            comments: None,
            comments_after_colon: None,
            span: (2, 4, 1),
          }),
          entry_type: Type {
            type_choices: vec![TypeChoice {
              type1: Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "addrdistr".into(),
                    socket: None,
                    span: (5, 14, 1),
                  },
                  generic_args: None,
                  span: (5, 14, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (5, 14, 1),
              },
              comments_before_type: None,
              comments_after_type: None,
            }],
            span: (5, 14, 1),
          },
        }),
        leading_comments: None,
        trailing_comments: None,
        span: (0, 14, 1),
      },
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: None,
          member_key: Some(MemberKey::Value {
            value: token::Value::UINT(0),
            comments: None,
            comments_after_colon: None,
            span: (0, 2, 1),
          }),
          entry_type: Type {
            type_choices: vec![TypeChoice {
              type1: Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "finite_set".into(),
                    socket: None,
                    span: (3, 13, 1),
                  },
                  generic_args: Some(GenericArgs {
                    args: vec![GenericArg {
                      arg: Box::from(Type1 {
                        type2: Type2::Typename {
                          ident: Identifier {
                            ident: "transaction_input".into(),
                            socket: None,
                            span: (14, 31, 1),
                          },
                          generic_args: None,
                          span: (14, 31, 1),
                        },
                        operator: None,
                        comments_after_type: None,
                        span: (14, 31, 1),
                      }),
                      comments_before_type: None,
                      comments_after_type: None,
                    }],
                    span: (13, 32, 1),
                  }),

                  span: (3, 32, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (3, 32, 1),
              },
              comments_before_type: None,
              comments_after_type: None,
            }],
            span: (3, 32, 1),
          },
        }),
        leading_comments: None,
        trailing_comments: None,
        span: (0, 32, 1),
      },
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: Some(Occurrence {
            occur: Occur::ZeroOrMore((0, 1, 1)),
            comments: None,
          }),
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
                          generic_args: None,
                        },
                        leading_comments: None,
                        trailing_comments: None,
                        span: (3, 13, 1),
                      },
                      OptionalComma {
                        optional_comma: false,
                        trailing_comments: None,
                      },
                    )],
                    comments_before_grpchoice: None,
                    span: (3, 13, 1),
                  }],
                  span: (3, 13, 1),
                },
                comments_before_group: None,
                comments_after_group: None,
                span: (2, 14, 1),
              },
              operator: None,
              comments_after_type: None,
              span: (2, 14, 1),
            }),
            is_cut: false,
            comments_before_cut: None,
            comments_after_cut: None,
            comments_after_arrowmap: None,
            span: (2, 22, 1),
          }),
          entry_type: Type {
            type_choices: vec![TypeChoice {
              type1: Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "coin".into(),
                    socket: None,
                    span: (18, 22, 1),
                  },
                  generic_args: None,
                  span: (18, 22, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (18, 22, 1),
              },
              comments_before_type: None,
              comments_after_type: None,
            }],
            span: (18, 22, 1),
          },
        }),
        leading_comments: None,
        trailing_comments: None,
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
      r#"( "mytype1" / int ) ^ =>"#,
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
            generic_args: None,
            span: (0, 5, 1),
          },
          operator: None,
          comments_after_type: None,
          span: (0, 5, 1),
        }),
        is_cut: false,
        comments_before_cut: None,
        comments_after_cut: None,
        comments_after_arrowmap: None,
        span: (0, 8, 1),
      },
      MemberKey::Type1 {
        t1: Box::from(Type1 {
          type2: Type2::ParenthesizedType {
            pt: Type {
              type_choices: vec![
                TypeChoice {
                  type1: Type1 {
                    type2: Type2::TextValue {
                      value: "mytype1".into(),
                      span: (2, 11, 1),
                    },
                    operator: None,
                    span: (2, 11, 1),
                    comments_after_type: None,
                  },
                  comments_after_type: None,
                  comments_before_type: None,
                },
                TypeChoice {
                  type1: Type1 {
                    type2: Type2::Typename {
                      ident: Identifier {
                        ident: "int",
                        span: (14, 17, 1),
                        socket: None,
                      },
                      span: (14, 17, 1),
                      generic_args: None,
                    },
                    span: (14, 17, 1),
                    comments_after_type: None,
                    operator: None,
                  },
                  comments_before_type: None,
                  comments_after_type: None,
                },
              ],
              span: (2, 17, 1),
            },
            span: (0, 19, 1),
            comments_before_type: None,
            comments_after_type: None,
          },
          operator: None,
          comments_after_type: None,
          span: (0, 19, 1),
        }),
        is_cut: true,
        comments_before_cut: None,
        comments_after_cut: None,
        comments_after_arrowmap: None,
        span: (0, 24, 1),
      },
      MemberKey::Bareword {
        ident: Identifier {
          ident: "mybareword".into(),
          socket: None,
          span: (0, 10, 1),
        },
        comments: None,
        comments_after_colon: None,
        span: (0, 11, 1),
      },
      MemberKey::Bareword {
        ident: Identifier {
          ident: "my..bareword".into(),
          socket: None,
          span: (0, 12, 1),
        },
        comments: None,
        comments_after_colon: None,
        span: (0, 13, 1),
      },
      MemberKey::Value {
        value: token::Value::TEXT("myvalue".into()),
        comments: None,
        comments_after_colon: None,
        span: (0, 10, 1),
      },
      MemberKey::Value {
        value: token::Value::UINT(0),
        comments: None,
        comments_after_colon: None,
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
      Occurrence {
        occur: Occur::Exact {
          lower: Some(1),
          upper: Some(3),
          span: (0, 3, 1),
        },
        comments: None,
      },
      Occurrence {
        occur: Occur::ZeroOrMore((0, 1, 1)),
        comments: None,
      },
      Occurrence {
        occur: Occur::OneOrMore((0, 1, 1)),
        comments: None,
      },
      Occurrence {
        occur: Occur::Exact {
          lower: Some(5),
          upper: None,
          span: (0, 2, 1),
        },
        comments: None,
      },
      Occurrence {
        occur: Occur::Exact {
          lower: None,
          upper: Some(3),
          span: (0, 2, 1),
        },
        comments: None,
      },
      Occurrence {
        occur: Occur::Optional((0, 1, 1)),
        comments: None,
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let mut l = Lexer::new(inputs[idx]);
      let o = Parser::new(l.iter(), inputs[idx])?.parse_occur(false)?;

      if let Some(o) = o {
        assert_eq!(&o, expected_output);
        assert_eq!(o.to_string(), expected_output.to_string());
      }
    }

    Ok(())
  }
}
