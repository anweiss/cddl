use super::{
  ast::*,
  error::{
    ErrorMsg,
    MsgType::{self, *},
  },
  lexer::{self, Position},
  token::{self, SocketPlug, Token},
};

use std::{cmp::Ordering, marker::PhantomData, mem, result};

use codespan_reporting::{
  diagnostic::{Diagnostic, Label},
  files::SimpleFiles,
  term,
};
use displaydoc::Display;

#[cfg(feature = "std")]
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
#[cfg(feature = "std")]
use std::{borrow::Cow, collections::BTreeSet, rc::Rc};

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::{Cow, ToOwned},
  boxed::Box,
  collections::BTreeSet,
  rc::Rc,
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
pub struct Parser<'a> {
  tokens: Box<dyn Iterator<Item = lexer::Item<'a>> + 'a>,
  str_input: &'a str,
  cur_token: Token<'a>,
  peek_token: Token<'a>,
  lexer_position: Position,
  peek_lexer_position: Position,
  #[cfg(feature = "ast-span")]
  parser_position: Position,
  /// Vec of collected parsing errors
  pub errors: Vec<Error>,
  current_rule_generic_param_idents: Option<Vec<&'a str>>,
  typenames: Rc<BTreeSet<&'a str>>,
  groupnames: Rc<BTreeSet<&'a str>>,
  #[cfg(feature = "ast-span")]
  unknown_rule_idents: Vec<(&'a str, Span)>,
  #[cfg(not(feature = "ast-span"))]
  unknown_rule_idents: Vec<&'a str>,
  is_guaranteed: bool,
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
  #[displaydoc("defer parsing error")]
  /// Incremental parsing error
  GROUP,
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl<'a> Parser<'a> {
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
  /// let p = Parser::new(input, Box::new(Lexer::new(input).iter()));
  /// ```
  pub fn new(
    str_input: &'a str,
    tokens: Box<dyn Iterator<Item = lexer::Item<'a>> + 'a>,
  ) -> Result<Parser<'a>> {
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
      current_rule_generic_param_idents: None,
      typenames: Rc::new(BTreeSet::from([
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
      ])),
      groupnames: Rc::new(BTreeSet::default()),
      unknown_rule_idents: Vec::default(),
      is_guaranteed: false,
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
  /// if let Ok(mut p) = Parser::new(input, Box::new(Lexer::new(input).iter())) {
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
      if let Token::IDENT(..) = self.cur_token {
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

  #[cfg(feature = "ast-comments")]
  fn collect_comments(&mut self) -> Result<Option<Comments<'a>>> {
    #[cfg_attr(not(feature = "lsp"), allow(unused_mut))]
    let mut comments: Option<Comments> = None;

    while let Token::COMMENT(_comment) = self.cur_token {
      #[cfg(not(feature = "lsp"))]
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

  #[cfg(not(feature = "ast-comments"))]
  fn advance_newline(&mut self) -> Result<()> {
    while let Token::NEWLINE = self.cur_token {
      #[cfg(feature = "lsp")]
      comments.get_or_insert(Comments::default()).0.push("\n");

      self.next_token()?;
    }

    Ok(())
  }

  fn register_rule(&mut self, rule: &Rule<'a>) {
    match &rule {
      Rule::Type { rule, .. } => Rc::make_mut(&mut self.typenames).insert(rule.name.ident),
      Rule::Group { rule, .. } => Rc::make_mut(&mut self.groupnames).insert(rule.name.ident),
    };
  }

  /// Parses into a `CDDL` AST
  pub fn parse_cddl(&mut self) -> Result<CDDL<'a>> {
    #[cfg(not(feature = "ast-comments"))]
    self.advance_newline()?;

    let mut c = CDDL {
      #[cfg(feature = "ast-comments")]
      comments: self.collect_comments()?,
      ..Default::default()
    };

    struct UnknownRule<'a> {
      rule: Rule<'a>,
      index: usize,
      range: (usize, usize),
    }

    // First pass: Parse all rules and register their names without checking for unknown identifiers
    let mut all_rules = Vec::default();
    // let mut rule_ranges = Vec::default();

    while self.cur_token != Token::EOF {
      let begin_rule_range = self.lexer_position.range.0;

      match self.parse_rule(false) {
        Ok(r) => {
          let rule_exists =
            |existing_rule: &Rule| r.name() == existing_rule.name() && !r.is_choice_alternate();

          if c.rules.iter().any(rule_exists) || all_rules.iter().any(|(rule, _)| rule_exists(rule))
          {
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

          // Register the rule name immediately
          self.register_rule(&r);

          all_rules.push((r, begin_rule_range));
          self.is_guaranteed = false;
        }
        Err(Error::INCREMENTAL) => {
          if !self.cur_token_is(Token::EOF) {
            self.advance_to_next_rule()?;
          }
        }
        Err(e) => return Err(e),
      }
    }

    // Second pass: Add all rules to the CDDL
    let mut unknown_rules = Vec::default();

    for (rule, begin_rule_range) in all_rules {
      // Check if the rule still has unknown identifiers
      if !self.unknown_rule_idents.is_empty() {
        unknown_rules.push(UnknownRule {
          rule,
          index: c.rules.len(),
          range: (begin_rule_range, self.lexer_position.range.1),
        });
        self.unknown_rule_idents = Vec::default();
      } else {
        c.rules.push(rule);
      }
    }

    // In practice unknown rules usually are declared backwards, so we reverse
    // it here.
    unknown_rules.reverse();

    // Try to specialize unknown rules until the set of them stabilizes.
    {
      let mut errors;
      let mut known_rules = Vec::default();
      loop {
        let mut resolved_rules = Vec::default();
        let mut unresolved_rules = Vec::default();

        errors = Vec::default();
        for unknown_rule in unknown_rules {
          match self.resolve_rule(unknown_rule.range, false) {
            Ok(rule) => resolved_rules.push((unknown_rule.index, rule)),
            Err(_) => match self.resolve_rule(unknown_rule.range, true) {
              Ok(rule) => resolved_rules.push((unknown_rule.index, rule)),
              Err(mut error) => {
                errors.append(&mut error);
                unresolved_rules.push(unknown_rule);
              }
            },
          }
        }
        if resolved_rules.is_empty() {
          break;
        }
        for (_, rule) in &resolved_rules {
          self.register_rule(rule);
        }
        known_rules.append(&mut resolved_rules);
        unknown_rules = unresolved_rules;
      }
      self.errors.append(&mut errors);
      known_rules.sort_by(|(a, _), (b, _)| b.partial_cmp(a).unwrap());
      for (index, rule) in known_rules {
        c.rules.insert(index, rule);
      }
    }

    if !self.errors.is_empty() {
      return Err(Error::INCREMENTAL);
    }

    // RFC 9682 Section 3.1: Empty data models are now allowed
    // The requirement for at least one rule is now a semantic constraint
    // to be fulfilled after processing of all directives.

    Ok(c)
  }

  fn resolve_rule(
    &mut self,
    range: (usize, usize),
    parse_group_rule: bool,
  ) -> result::Result<Rule<'a>, Vec<Error>> {
    let tokens = Box::new(lexer::Lexer::new(&self.str_input[range.0..range.1]).iter());
    let mut parser = Parser::new(self.str_input, tokens).map_err(|err| vec![err])?;
    parser.groupnames = self.groupnames.clone();
    parser.typenames = self.typenames.clone();
    let rule = parser
      .parse_rule(parse_group_rule)
      .map_err(|err| vec![err])?;
    if !parser.unknown_rule_idents.is_empty() {
      Err(
        #[cfg(feature = "ast-span")]
        parser
          .unknown_rule_idents
          .into_iter()
          .map(|(ident, span)| Error::PARSER {
            position: Position {
              column: 0,
              index: span.0,
              line: span.2,
              range: (span.0 + range.0, span.1 + range.0),
            },
            msg: ErrorMsg {
              short: format!("missing definition for rule {}", ident),
              extended: None,
            },
          })
          .collect(),
        #[cfg(not(feature = "ast-span"))]
        parser
          .unknown_rule_idents
          .into_iter()
          .map(|ident| Error::PARSER {
            msg: ErrorMsg {
              short: format!("missing definition for rule {}", ident),
              extended: None,
            },
          })
          .collect(),
      )
    } else {
      Ok(rule)
    }
  }

  #[allow(missing_docs)]
  pub fn parse_rule(&mut self, parse_group_rule: bool) -> Result<Rule<'a>> {
    #[cfg(feature = "ast-span")]
    let begin_rule_range = self.lexer_position.range.0;
    #[cfg(feature = "ast-span")]
    let begin_rule_line = self.lexer_position.line;
    #[cfg(feature = "ast-span")]
    let begin_rule_col = self.lexer_position.column;

    let ident = match &self.cur_token {
      Token::IDENT(i, s) => self.identifier_from_ident_token(i, *s),
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

      let params = self.parse_genericparm()?;
      let mut param_list = Vec::default();

      for param in params.params.iter() {
        param_list.push(param.param.ident);
      }

      self.current_rule_generic_param_idents = Some(param_list);

      Some(params)
    } else {
      None
    };

    #[cfg(feature = "ast-comments")]
    let comments_before_assign = self.collect_comments()?;
    #[cfg(not(feature = "ast-comments"))]
    self.advance_newline()?;

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

    if let Token::TCHOICEALT = &self.cur_token {
      is_type_choice_alternate = true;
    } else if let Token::GCHOICEALT = &self.cur_token {
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

    #[cfg(feature = "ast-comments")]
    let comments_after_assign = self.collect_comments()?;
    #[cfg(not(feature = "ast-comments"))]
    self.advance_newline()?;

    // If token is group socket or rule is a group plug alternative, parse
    // as group rule
    if matches!(self.cur_token, Token::IDENT(_, Some(SocketPlug::GROUP)))
      || is_group_choice_alternate
      || parse_group_rule
    {
      let ge = self.parse_grpent(true)?;

      #[cfg(feature = "ast-comments")]
      let comments_after_rule = self.collect_comments()?;
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      #[cfg(feature = "ast-span")]
      let span = (
        begin_rule_range,
        self.parser_position.range.1,
        begin_rule_line,
      );

      self.current_rule_generic_param_idents = None;
      self.is_guaranteed = true;

      return Ok(Rule::Group {
        rule: Box::from(GroupRule {
          name: ident,
          generic_params: gp,
          is_group_choice_alternate,
          entry: ge,
          #[cfg(feature = "ast-comments")]
          comments_before_assigng: comments_before_assign,
          #[cfg(feature = "ast-comments")]
          comments_after_assigng: comments_after_assign,
        }),
        #[cfg(feature = "ast-comments")]
        comments_after_rule,
        #[cfg(feature = "ast-span")]
        span,
      });
    }

    match self.cur_token {
      Token::LPAREN | Token::ASTERISK | Token::ONEORMORE | Token::OPTIONAL => {
        #[cfg(feature = "ast-span")]
        let begin_pt_range = self.lexer_position.range.0;

        let ge = self.parse_grpent(true)?;

        #[cfg(feature = "ast-span")]
        let mut end_rule_range = self.parser_position.range.1;

        #[cfg(feature = "ast-comments")]
        let comments_after_rule = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        // If a group entry is an inline group with no leading occurrence
        // indicator, and its group has only a single element that is not
        // preceded by an occurrence indicator nor member key, then there are
        // two valid interpretations: either it's a parenthesized inline group
        // with a type or a parenthesized type. Both cases are interpreted in
        // the same way, but according to the BNF, the parenthesized type takes
        // priority.
        //
        // A priori, we coerce this group into a parenthesized type. This is one
        // of the few situations where `clone` is required
        if let GroupEntry::InlineGroup {
          occur: None,
          group,
          #[cfg(feature = "ast-comments")]
          comments_before_group,
          #[cfg(feature = "ast-comments")]
          comments_after_group,
          ..
        } = &ge
        {
          if group.group_choices.len() == 1 {
            if let Some(gc) = group.group_choices.first() {
              if gc.group_entries.len() == 1 {
                if let Some(group_entry) = gc.group_entries.first() {
                  // Check that there is no trailing comma
                  if !group_entry.1.optional_comma {
                    // EXAMPLE: non-empty<M> = (M) .and ({ + any => any })
                    if let GroupEntry::TypeGroupname {
                      ge,
                      #[cfg(feature = "ast-comments")]
                      leading_comments,
                      #[cfg(feature = "ast-comments")]
                      trailing_comments,
                      ..
                    } = &group_entry.0
                    {
                      if ge.occur.is_none() && matches!(self.cur_token, Token::ControlOperator(_)) {
                        let value = self.parse_type(Some(Type2::ParenthesizedType {
                          #[cfg(feature = "ast-comments")]
                          comments_before_type: comments_before_group.clone(),
                          pt: Type {
                            type_choices: vec![TypeChoice {
                              #[cfg(feature = "ast-comments")]
                              comments_before_type: leading_comments.clone(),
                              #[cfg(feature = "ast-comments")]
                              comments_after_type: trailing_comments.clone(),
                              type1: Type1 {
                                type2: Type2::Typename {
                                  ident: ge.name.clone(),
                                  generic_args: ge.generic_args.clone(),
                                  #[cfg(feature = "ast-span")]
                                  span: ge.name.span,
                                },
                                operator: None,
                                #[cfg(feature = "ast-span")]
                                span: ge.name.span,
                                #[cfg(feature = "ast-comments")]
                                comments_after_type: None,
                              },
                            }],
                            #[cfg(feature = "ast-span")]
                            span: ge.name.span,
                          },
                          #[cfg(feature = "ast-comments")]
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

                        self.current_rule_generic_param_idents = None;

                        return Ok(Rule::Type {
                          rule: TypeRule {
                            name: ident,
                            generic_params: gp,
                            is_type_choice_alternate,
                            value,
                            #[cfg(feature = "ast-comments")]
                            comments_before_assignt: comments_before_assign,
                            #[cfg(feature = "ast-comments")]
                            comments_after_assignt: comments_after_assign,
                          },
                          #[cfg(feature = "ast-comments")]
                          comments_after_rule,
                          #[cfg(feature = "ast-span")]
                          span: (begin_rule_range, end_rule_range, begin_rule_line),
                        });
                      }
                    }

                    // TODO: Replace with box pattern destructuring once supported in stable
                    if let GroupEntry::ValueMemberKey { ge, .. } = &group_entry.0 {
                      if ge.occur.is_none() && ge.member_key.is_none() {
                        let value = self.parse_type(Some(Type2::ParenthesizedType {
                          #[cfg(feature = "ast-comments")]
                          comments_before_type: comments_before_group.clone(),
                          pt: ge.entry_type.clone(),
                          #[cfg(feature = "ast-comments")]
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

                        self.current_rule_generic_param_idents = None;

                        return Ok(Rule::Type {
                          rule: TypeRule {
                            name: ident,
                            generic_params: gp,
                            is_type_choice_alternate,
                            value,
                            #[cfg(feature = "ast-comments")]
                            comments_before_assignt: comments_before_assign,
                            #[cfg(feature = "ast-comments")]
                            comments_after_assignt: comments_after_assign,
                          },
                          #[cfg(feature = "ast-comments")]
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

        self.current_rule_generic_param_idents = None;

        Ok(Rule::Group {
          rule: Box::from(GroupRule {
            name: ident,
            generic_params: gp,
            is_group_choice_alternate,
            entry: ge,
            #[cfg(feature = "ast-comments")]
            comments_before_assigng: comments_before_assign,
            #[cfg(feature = "ast-comments")]
            comments_after_assigng: comments_after_assign,
          }),
          #[cfg(feature = "ast-comments")]
          comments_after_rule,
          #[cfg(feature = "ast-span")]
          span: (begin_rule_range, end_rule_range, begin_rule_line),
        })
      }
      _ => {
        // If type rule is an unwrap type, advance token after parsing type
        let advance_token = matches!(self.cur_token, Token::UNWRAP);

        #[cfg(feature = "ast-comments")]
        let mut t = self.parse_type(None)?;
        #[cfg(not(feature = "ast-comments"))]
        let t = self.parse_type(None)?;

        if advance_token {
          self.next_token()?;
        }

        #[cfg(feature = "ast-comments")]
        let comments_after_rule = if let Some(comments) = t.split_comments_after_type() {
          Some(comments)
        } else {
          self.collect_comments()?
        };

        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        if let Token::ASSIGN | Token::TCHOICEALT | Token::GCHOICEALT = &self.cur_token {
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

        self.current_rule_generic_param_idents = None;

        if t.type_choices.len() > 1
          || !matches!(
            t.type_choices[0].type1.type2,
            Type2::ParenthesizedType { .. } | Type2::Typename { .. }
          )
        {
          self.is_guaranteed = true;
        }

        Ok(Rule::Type {
          rule: TypeRule {
            name: ident,
            generic_params: gp,
            is_type_choice_alternate,
            value: t,
            #[cfg(feature = "ast-comments")]
            comments_before_assignt: comments_before_assign,
            #[cfg(feature = "ast-comments")]
            comments_after_assignt: comments_after_assign,
          },
          #[cfg(feature = "ast-comments")]
          comments_after_rule,
          #[cfg(feature = "ast-span")]
          span,
        })
      }
    }
  }

  #[allow(missing_docs)]
  pub fn parse_genericparm(&mut self) -> Result<GenericParams<'a>> {
    #[cfg(feature = "ast-span")]
    let begin_range = self.lexer_position.range.0;

    if let Token::LANGLEBRACKET = &self.cur_token {
      self.next_token()?;
    }

    let mut generic_params = GenericParams::default();

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      #[cfg(feature = "ast-comments")]
      let comments_before_ident = self.collect_comments()?;
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      match &self.cur_token {
        Token::IDENT(ident, socket) => {
          let param = self.identifier_from_ident_token(ident, *socket);

          self.next_token()?;

          #[cfg(feature = "ast-comments")]
          let comments_after_ident = self.collect_comments()?;
          #[cfg(not(feature = "ast-comments"))]
          self.advance_newline()?;

          generic_params.params.push(GenericParam {
            param,
            #[cfg(feature = "ast-comments")]
            comments_before_ident,
            #[cfg(feature = "ast-comments")]
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

  #[allow(missing_docs)]
  pub fn parse_genericargs(&mut self) -> Result<GenericArgs<'a>> {
    if self.peek_token_is(&Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    #[cfg(feature = "ast-span")]
    let begin_generic_arg_range = self.lexer_position.range.0;
    #[cfg(feature = "ast-span")]
    let begin_generic_arg_line = self.lexer_position.line;

    // Required for type2 mutual recursion
    if let Token::LANGLEBRACKET = &self.cur_token {
      self.next_token()?;
    }

    let mut generic_args = GenericArgs::default();

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      #[cfg(feature = "ast-comments")]
      let leading_comments = self.collect_comments()?;
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      let t1 = self.parse_type1(None)?;

      #[cfg(feature = "ast-comments")]
      let trailing_comments = self.collect_comments()?;
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      generic_args.args.push(GenericArg {
        #[cfg(feature = "ast-comments")]
        comments_before_type: leading_comments,
        arg: Box::from(t1),
        #[cfg(feature = "ast-comments")]
        comments_after_type: trailing_comments,
      });

      if let Token::COMMA = self.cur_token {
        self.next_token()?;
      }

      if let Token::EOF = &self.cur_token {
        self.errors.push(Error::PARSER {
          #[cfg(feature = "ast-span")]
          position: self.parser_position,
          msg: MissingGenericClosingDelimiter.into(),
        });

        return Err(Error::INCREMENTAL);
      }
    }

    if let Token::RANGLEBRACKET = &self.cur_token {
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

  // parenthesized_type can be provided as an argument to retrieve its span and
  // comments if it has been previously parsed
  #[allow(missing_docs)]
  pub fn parse_type(&mut self, parenthesized_type: Option<Type2<'a>>) -> Result<Type<'a>> {
    #[cfg(feature = "ast-span")]
    {
      self.parser_position.range = self.lexer_position.range;
      self.parser_position.line = self.lexer_position.line;
    }

    #[cfg(feature = "ast-span")]
    let begin_type_range = if let Some(Type2::ParenthesizedType { span, .. }) = parenthesized_type {
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

    #[cfg(feature = "ast-comments")]
    let mut tc = TypeChoice {
      type1: self.parse_type1(parenthesized_type)?,
      comments_before_type: None,
      comments_after_type: None,
    };

    #[cfg(not(feature = "ast-comments"))]
    let tc = TypeChoice {
      type1: self.parse_type1(parenthesized_type)?,
    };

    #[cfg(feature = "ast-comments")]
    {
      tc.comments_after_type = self.collect_comments()?;
    }
    #[cfg(not(feature = "ast-comments"))]
    self.advance_newline()?;

    t.type_choices.push(tc);

    while let Token::TCHOICE = &self.cur_token {
      self.next_token()?;

      #[cfg(feature = "ast-comments")]
      let comments_before_type = self.collect_comments()?;
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      #[cfg(feature = "ast-comments")]
      let mut tc = TypeChoice {
        comments_before_type,
        comments_after_type: None,
        type1: self.parse_type1(None)?,
      };

      #[cfg(not(feature = "ast-comments"))]
      let tc = TypeChoice {
        type1: self.parse_type1(None)?,
      };

      #[cfg(feature = "ast-comments")]
      {
        tc.comments_after_type = self.collect_comments()?;
      }
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      t.type_choices.push(tc);
    }

    #[cfg(feature = "ast-span")]
    {
      t.span.1 = self.parser_position.range.1;
    }

    Ok(t)
  }

  // parenthesized_type can be provided as an argument to retrieve its span and
  // comments if it has been previously parsed
  #[allow(missing_docs)]
  pub fn parse_type1(&mut self, parenthesized_type: Option<Type2<'a>>) -> Result<Type1<'a>> {
    #[cfg(feature = "ast-span")]
    let mut begin_type1_line = self.lexer_position.line;
    #[cfg(feature = "ast-span")]
    let mut begin_type1_range = self.lexer_position.range.0;

    let t2_1 = if let Some(Type2::ParenthesizedType {
      #[cfg(feature = "ast-comments")]
      comments_before_type,
      pt,
      #[cfg(feature = "ast-comments")]
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
        #[cfg(feature = "ast-comments")]
        comments_before_type,
        pt,
        #[cfg(feature = "ast-comments")]
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

    #[cfg(feature = "ast-comments")]
    let comments_after_type = self.collect_comments()?;
    #[cfg(not(feature = "ast-comments"))]
    self.advance_newline()?;

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
      Token::ControlOperator(ctrl) => {
        #[cfg(feature = "ast-span")]
        {
          span.0 = self.lexer_position.range.0;
        }

        Some(RangeCtlOp::CtlOp {
          ctrl: *ctrl,
          #[cfg(feature = "ast-span")]
          span,
        })
      }
      _ => None,
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

        #[cfg(feature = "ast-comments")]
        let comments_after_operator = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        let t2 = self.parse_type2()?;

        #[cfg(feature = "ast-span")]
        {
          span.1 = self.parser_position.range.1;
        }

        Ok(Type1 {
          type2: t2_1,
          operator: Some(Operator {
            #[cfg(feature = "ast-comments")]
            comments_before_operator: comments_after_type,
            operator,
            #[cfg(feature = "ast-comments")]
            comments_after_operator,
            type2: t2,
          }),
          #[cfg(feature = "ast-comments")]
          comments_after_type: None,
          #[cfg(feature = "ast-span")]
          span,
        })
      }
      None => Ok(Type1 {
        type2: t2_1,
        operator: None,
        #[cfg(feature = "ast-comments")]
        comments_after_type,
        #[cfg(feature = "ast-span")]
        span,
      }),
    }
  }

  #[allow(missing_docs)]
  pub fn parse_type2(&mut self) -> Result<Type2<'a>> {
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
      Token::IDENT(ident, socket) => {
        #[cfg(feature = "ast-span")]
        let begin_type2_range = self.lexer_position.range.0;
        #[cfg(feature = "ast-span")]
        let begin_type2_line = self.lexer_position.line;

        // optional genericarg detected
        if self.peek_token_is(&Token::LANGLEBRACKET) {
          let ident = self.identifier_from_ident_token(ident, *socket);
          let ga = self.parse_genericargs()?;

          #[cfg(feature = "ast-span")]
          let end_type2_range = self.parser_position.range.1;

          if ident.socket.is_none() {
            let mut is_generic_param = false;
            if let Some(idents) = &self.current_rule_generic_param_idents {
              is_generic_param = idents.contains(&ident.ident);
            }

            #[cfg(feature = "ast-span")]
            if !is_generic_param && !self.typenames.contains(ident.ident) {
              self.unknown_rule_idents.push((ident.ident, ident.span));
            }

            #[cfg(not(feature = "ast-span"))]
            if !is_generic_param && !self.typenames.contains(ident.ident) {
              self.unknown_rule_idents.push(ident.ident);
            }
          }

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

        let ident = self.identifier_from_ident_token(ident, *socket);

        if ident.socket.is_none() {
          let mut is_generic_param = false;
          if let Some(idents) = &self.current_rule_generic_param_idents {
            is_generic_param = idents.contains(&ident.ident);
          }

          #[cfg(feature = "ast-span")]
          if !is_generic_param && !self.typenames.contains(ident.ident) {
            self.unknown_rule_idents.push((ident.ident, ident.span));
          }

          #[cfg(not(feature = "ast-span"))]
          if !is_generic_param && !self.typenames.contains(ident.ident) {
            self.unknown_rule_idents.push(ident.ident);
          }
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

      // ( type )
      Token::LPAREN => {
        #[cfg(feature = "ast-span")]
        let begin_type2_range = self.lexer_position.range.0;
        #[cfg(feature = "ast-span")]
        let begin_type2_line = self.lexer_position.line;

        self.next_token()?;

        #[cfg(feature = "ast-comments")]
        let comments_before_type = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        let pt = self.parse_type(None)?;

        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range.0 = begin_type2_range;
          self.parser_position.range.1 = self.lexer_position.range.1;
          self.parser_position.line = begin_type2_line;
        }

        #[cfg(feature = "ast-comments")]
        let comments_after_type = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        Ok(Type2::ParenthesizedType {
          #[cfg(feature = "ast-comments")]
          comments_before_type,
          #[cfg(feature = "ast-comments")]
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

        #[cfg(feature = "ast-comments")]
        let mut group = self.parse_group()?;
        #[cfg(not(feature = "ast-comments"))]
        let group = self.parse_group()?;

        // if the group starts with a multi-line comment,
        // we take the first comment inside the 1st group to be comments_before_group
        #[cfg(feature = "ast-comments")]
        let comments_before_group = if let Some(GroupChoice {
          comments_before_grpchoice,
          ..
        }) = group.group_choices.first_mut()
        {
          comments_before_grpchoice
            .as_mut()
            .and_then(|comments| {
              if comments.0.len() > 1 {
                Some(comments.0.remove(0))
              } else {
                None
              }
            })
            .map(|comment| Comments(vec![comment]))
        } else {
          None
        };

        #[cfg(feature = "ast-span")]
        let span = (
          begin_type2_range,
          self.lexer_position.range.1,
          begin_type2_line,
        );

        #[cfg(feature = "ast-comments")]
        let comments_after_group = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        Ok(Type2::Map {
          #[cfg(feature = "ast-comments")]
          comments_before_group,
          group,
          #[cfg(feature = "ast-span")]
          span,
          #[cfg(feature = "ast-comments")]
          comments_after_group,
        })
      }

      // [ group ]
      Token::LBRACKET => {
        #[cfg(feature = "ast-span")]
        let begin_type2_range = self.lexer_position.range.0;
        #[cfg(feature = "ast-span")]
        let begin_type2_line = self.lexer_position.line;

        #[cfg(feature = "ast-comments")]
        let mut group = self.parse_group()?;
        #[cfg(not(feature = "ast-comments"))]
        let group = self.parse_group()?;

        // if the group starts with a multi-line comment,
        // we take the first comment inside the 1st group to be comments_before_group
        #[cfg(feature = "ast-comments")]
        let comments_before_group = if let Some(GroupChoice {
          comments_before_grpchoice,
          ..
        }) = group.group_choices.first_mut()
        {
          comments_before_grpchoice
            .as_mut()
            .and_then(|comments| {
              if comments.0.len() > 1 {
                Some(comments.0.remove(0))
              } else {
                None
              }
            })
            .map(|comment| Comments(vec![comment]))
        } else {
          None
        };

        #[cfg(feature = "ast-span")]
        let span = (
          begin_type2_range,
          self.lexer_position.range.1,
          begin_type2_line,
        );

        #[cfg(feature = "ast-comments")]
        let comments_after_group = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        Ok(Type2::Array {
          #[cfg(feature = "ast-comments")]
          comments_before_group,
          group,
          #[cfg(feature = "ast-comments")]
          comments_after_group,
          #[cfg(feature = "ast-span")]
          span,
        })
      }

      // ~ typename [genericarg]
      Token::UNWRAP => {
        self.next_token()?;

        #[cfg(feature = "ast-comments")]
        let comments = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        let ident = if let Some(ident) = self.cur_token.in_standard_prelude() {
          Some(self.identifier_from_ident_token(ident, None))
        } else if let Token::IDENT(ident, socket) = &self.cur_token {
          Some(self.identifier_from_ident_token(ident, *socket))
        } else {
          None
        };

        if let Some(ident) = ident {
          if self.peek_token_is(&Token::LANGLEBRACKET) {
            self.next_token()?;

            return Ok(Type2::Unwrap {
              #[cfg(feature = "ast-comments")]
              comments,
              ident,
              generic_args: Some(self.parse_genericargs()?),
              #[cfg(feature = "ast-span")]
              span: (0, 0, 0),
            });
          }

          return Ok(Type2::Unwrap {
            #[cfg(feature = "ast-comments")]
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

        #[cfg(feature = "ast-comments")]
        let comments = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        match &self.cur_token {
          Token::LPAREN => {
            self.next_token()?;

            #[cfg(feature = "ast-comments")]
            let comments_before_group = self.collect_comments()?;
            #[cfg(not(feature = "ast-comments"))]
            self.advance_newline()?;

            let group = self.parse_group()?;

            #[cfg(feature = "ast-comments")]
            let comments_after_group = self.collect_comments()?;
            #[cfg(not(feature = "ast-comments"))]
            self.advance_newline()?;

            Ok(Type2::ChoiceFromInlineGroup {
              #[cfg(feature = "ast-comments")]
              comments,
              #[cfg(feature = "ast-comments")]
              comments_before_group,
              group,
              #[cfg(feature = "ast-comments")]
              comments_after_group,
              #[cfg(feature = "ast-span")]
              span: (
                begin_type2_range,
                self.parser_position.range.1,
                begin_type2_line,
              ),
            })
          }
          Token::IDENT(ident, socket) => {
            let ident = self.identifier_from_ident_token(ident, *socket);
            if self.peek_token_is(&Token::LANGLEBRACKET) {
              self.next_token()?;

              let generic_args = Some(self.parse_genericargs()?);

              return Ok(Type2::ChoiceFromGroup {
                #[cfg(feature = "ast-comments")]
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
              #[cfg(feature = "ast-comments")]
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
      Token::TAG(mt, constraint) => {
        #[cfg(feature = "ast-span")]
        let begin_type2_range = self.lexer_position.range.0;
        #[cfg(feature = "ast-span")]
        let begin_type2_line = self.lexer_position.line;

        // Extract values to avoid borrow checker issues
        let mt_val = *mt;
        let constraint_val = *constraint;

        match (mt_val, constraint_val) {
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

            #[cfg(feature = "ast-comments")]
            let comments_before_type = self.collect_comments()?;
            #[cfg(not(feature = "ast-comments"))]
            self.advance_newline()?;

            let t = self.parse_type(None)?;

            #[cfg(feature = "ast-comments")]
            let comments_after_type = self.collect_comments()?;
            #[cfg(not(feature = "ast-comments"))]
            self.advance_newline()?;

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
              #[cfg(feature = "ast-comments")]
              comments_before_type,
              t,
              #[cfg(feature = "ast-comments")]
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
          _ => Ok(Type2::Any {
            span: (
              begin_type2_range,
              self.lexer_position.range.1,
              begin_type2_line,
            ),
          }),
          #[cfg(not(feature = "ast-span"))]
          _ => Ok(Type2::Any {}),
        }
      }
      _ => {
        #[cfg(feature = "ast-comments")]
        self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        match self.cur_token.in_standard_prelude() {
          Some(s) => {
            let ident = self.identifier_from_ident_token(s, None);
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

            if let Token::COLON | Token::ARROWMAP = &self.cur_token {
              self.errors.push(Error::PARSER {
                #[cfg(feature = "ast-span")]
                position: self.parser_position,
                msg: MissingGroupEntryMemberKey.into(),
              });

              return Err(Error::INCREMENTAL);
            }

            if let Token::RBRACE | Token::RBRACKET | Token::RPAREN = &self.cur_token {
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

  #[allow(missing_docs)]
  pub fn parse_group(&mut self) -> Result<Group<'a>> {
    #[cfg(feature = "ast-span")]
    let begin_group_range =
      if let Token::LBRACE | Token::LPAREN | Token::LBRACKET | Token::GCHOICE = &self.cur_token {
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

    while let Token::GCHOICE = &self.cur_token {
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

  #[allow(missing_docs)]
  pub fn parse_grpchoice(&mut self) -> Result<GroupChoice<'a>> {
    let mut grpchoice = GroupChoice {
      group_entries: Vec::new(),
      #[cfg(feature = "ast-comments")]
      comments_before_grpchoice: None,
      #[cfg(feature = "ast-span")]
      span: (self.lexer_position.range.0, 0, self.lexer_position.line),
    };

    if let Token::GCHOICE = &self.cur_token {
      self.next_token()?;

      #[cfg(feature = "ast-comments")]
      {
        grpchoice.comments_before_grpchoice = self.collect_comments()?;
      }
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      #[cfg(feature = "ast-span")]
      {
        grpchoice.span.0 = self.lexer_position.range.0;
      }
    } else if let Token::LBRACE | Token::LBRACKET = &self.cur_token {
      self.next_token()?;

      #[cfg(feature = "ast-span")]
      {
        grpchoice.span.0 = self.lexer_position.range.0;
      }

      #[cfg(feature = "ast-comments")]
      {
        grpchoice.comments_before_grpchoice = self.collect_comments()?;
      }
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;
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

      if let Token::GCHOICE = &self.cur_token {
        grpchoice.group_entries.push((
          ge,
          OptionalComma {
            optional_comma: false,
            #[cfg(feature = "ast-comments")]
            trailing_comments: None,
            _a: PhantomData,
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
        && !matches!(self.cur_token, Token::IDENT(..))
      {
        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range.1 = self.lexer_position.range.1;
        }
        self.next_token()?;
      }

      let mut optional_comma = false;

      if let Token::COMMA = &self.cur_token {
        optional_comma = true;

        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range.1 = self.lexer_position.range.1;
        }
        self.next_token()?;
      }

      #[cfg(feature = "ast-comments")]
      let trailing_comments = self.collect_comments()?;
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      grpchoice.group_entries.push((
        ge,
        OptionalComma {
          optional_comma,
          #[cfg(feature = "ast-comments")]
          trailing_comments,
          _a: PhantomData,
        },
      ));
    }

    #[cfg(feature = "ast-span")]
    {
      grpchoice.span.1 = self.parser_position.range.1;
    }

    Ok(grpchoice)
  }

  #[allow(missing_docs)]
  pub fn parse_grpent(&mut self, from_rule: bool) -> Result<GroupEntry<'a>> {
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

      #[cfg(feature = "ast-comments")]
      let comments_before_group = self.collect_comments()?;
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

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

      #[cfg(feature = "ast-comments")]
      let comments_after_group = self.collect_comments()?;
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

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
        #[cfg(feature = "ast-comments")]
        comments_before_group,
        #[cfg(feature = "ast-comments")]
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
        #[cfg(feature = "ast-comments")]
          non_member_key: NonMemberKey::Type(mut entry_type),
        #[cfg(not(feature = "ast-comments"))]
          non_member_key: NonMemberKey::Type(entry_type),
        #[cfg(feature = "ast-comments")]
        comments_before_type_or_group,
        #[cfg(feature = "ast-comments")]
        comments_after_type_or_group,
      }) => {
        #[cfg(feature = "ast-span")]
        if let Token::COMMA = &self.cur_token {
          span.1 = self.lexer_position.range.1;
        }

        #[cfg(feature = "ast-comments")]
        let trailing_comments = entry_type.take_comments_after_type();

        #[cfg(feature = "ast-span")]
        if let Some((name, generic_args, _)) = entry_type.groupname_entry() {
          if self.groupnames.contains(name.ident) || matches!(name.socket, Some(SocketPlug::GROUP))
          {
            if name.socket.is_none() {
              self.unknown_rule_idents = self
                .unknown_rule_idents
                .clone()
                .into_iter()
                .filter(|(ident, _)| ident != &name.ident)
                .collect();
            }
            return Ok(GroupEntry::TypeGroupname {
              ge: TypeGroupnameEntry {
                occur,
                name,
                generic_args,
              },
              #[cfg(feature = "ast-comments")]
              leading_comments: comments_before_type_or_group,
              #[cfg(feature = "ast-comments")]
              trailing_comments,
              span,
            });
          }
        }

        #[cfg(not(feature = "ast-span"))]
        if let Some((name, generic_args)) = entry_type.groupname_entry() {
          if self.groupnames.contains(name.ident) || matches!(name.socket, Some(SocketPlug::GROUP))
          {
            if name.socket.is_none() {
              self.unknown_rule_idents = self
                .unknown_rule_idents
                .clone()
                .into_iter()
                .filter(|ident| ident != &name.ident)
                .collect();
            }
            return Ok(GroupEntry::TypeGroupname {
              ge: TypeGroupnameEntry {
                occur,
                name,
                generic_args,
              },
              #[cfg(feature = "ast-comments")]
              leading_comments: comments_before_type_or_group,
              #[cfg(feature = "ast-comments")]
              trailing_comments,
            });
          }
        }

        // A parse tree that returns a type instead of a member key needs to
        // advance the token in the case of "(", "{" or "[". Otherwise, infinite
        // recursive loop occurs
        if let Token::LPAREN | Token::LBRACE | Token::LBRACKET = self.cur_token {
          self.next_token()?;
        }

        #[cfg(feature = "ast-comments")]
        let trailing_comments = if let Some(comments) = entry_type.split_comments_after_type() {
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
          #[cfg(feature = "ast-comments")]
          leading_comments: comments_before_type_or_group,
          #[cfg(feature = "ast-comments")]
          trailing_comments,
          #[cfg(feature = "ast-span")]
          span,
        })
      }
      Some(MemberKey::NonMemberKey {
        non_member_key: NonMemberKey::Group(group),
        #[cfg(feature = "ast-comments")]
        comments_before_type_or_group,
        #[cfg(feature = "ast-comments")]
        comments_after_type_or_group,
      }) => {
        #[cfg(feature = "ast-span")]
        if let Token::COMMA = &self.cur_token {
          span.1 = self.lexer_position.range.1;
        }

        Ok(GroupEntry::InlineGroup {
          occur,
          group,
          #[cfg(feature = "ast-span")]
          span,
          #[cfg(feature = "ast-comments")]
          comments_before_group: comments_before_type_or_group,
          #[cfg(feature = "ast-comments")]
          comments_after_group: comments_after_type_or_group,
        })
      }
      member_key @ Some(_) => {
        #[cfg(feature = "ast-comments")]
        let mut entry_type = self.parse_type(None)?;
        #[cfg(not(feature = "ast-comments"))]
        let entry_type = self.parse_type(None)?;

        #[cfg(feature = "ast-comments")]
        let trailing_comments = entry_type.split_comments_after_type();

        #[cfg(feature = "ast-span")]
        {
          span.1 = self.parser_position.range.1;
        }

        #[cfg(feature = "ast-span")]
        if let Token::COMMA = &self.cur_token {
          span.1 = self.lexer_position.range.1;
        }

        Ok(GroupEntry::ValueMemberKey {
          ge: Box::from(ValueMemberKeyEntry {
            occur,
            member_key,
            entry_type,
          }),
          #[cfg(feature = "ast-comments")]
          leading_comments: None,
          #[cfg(feature = "ast-comments")]
          trailing_comments,
          #[cfg(feature = "ast-span")]
          span,
        })
      }
      None => {
        #[cfg(feature = "ast-comments")]
        let mut entry_type = self.parse_type(None)?;
        #[cfg(not(feature = "ast-comments"))]
        let entry_type = self.parse_type(None)?;

        #[cfg(feature = "ast-span")]
        {
          span.1 = self.parser_position.range.1;
        }

        #[cfg(feature = "ast-comments")]
        let trailing_comments = if let Some(comments) = entry_type.take_comments_after_type() {
          Some(comments)
        } else {
          self.collect_comments()?
        };
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        #[cfg(feature = "ast-span")]
        if let Token::COMMA = &self.cur_token {
          span.1 = self.lexer_position.range.1;
        }

        #[cfg(feature = "ast-span")]
        if let Some((name, generic_args, _)) = entry_type.groupname_entry() {
          // Check if it's a known groupname OR if it could be a forward reference to a group
          if self.groupnames.contains(name.ident) || matches!(name.socket, Some(SocketPlug::GROUP))
          {
            if generic_args.is_some() && self.peek_token_is(&Token::LANGLEBRACKET) {
              while !self.peek_token_is(&Token::RANGLEBRACKET) {
                self.next_token()?;
              }

              self.next_token()?;
            }

            if name.socket.is_none() {
              self.unknown_rule_idents = self
                .unknown_rule_idents
                .clone()
                .into_iter()
                .filter(|(ident, _)| ident != &name.ident)
                .collect();
            }
            return Ok(GroupEntry::TypeGroupname {
              ge: TypeGroupnameEntry {
                occur,
                name,
                generic_args,
              },
              #[cfg(feature = "ast-comments")]
              leading_comments: None,
              #[cfg(feature = "ast-comments")]
              trailing_comments,
              span,
            });
          }
        }

        #[cfg(not(feature = "ast-span"))]
        if let Some((name, generic_args)) = entry_type.groupname_entry() {
          if self.groupnames.contains(name.ident) || matches!(name.socket, Some(SocketPlug::GROUP))
          {
            if generic_args.is_some() && self.peek_token_is(&Token::LANGLEBRACKET) {
              while !self.peek_token_is(&Token::RANGLEBRACKET) {
                self.next_token()?;
              }

              self.next_token()?;
            }

            if name.socket.is_none() {
              self.unknown_rule_idents = self
                .unknown_rule_idents
                .clone()
                .into_iter()
                .filter(|ident| ident != &name.ident)
                .collect();
            }
            return Ok(GroupEntry::TypeGroupname {
              ge: TypeGroupnameEntry {
                occur,
                name,
                generic_args,
              },
              #[cfg(feature = "ast-comments")]
              leading_comments: None,
              #[cfg(feature = "ast-comments")]
              trailing_comments,
            });
          }
        }

        // If we have a simple identifier that could be a group reference (even if not yet defined),
        // create a TypeGroupname entry instead of a ValueMemberKey with no member_key
        #[cfg(feature = "ast-span")]
        if let Some((name, generic_args, _)) = entry_type.groupname_entry() {
          return Ok(GroupEntry::TypeGroupname {
            ge: TypeGroupnameEntry {
              occur,
              name,
              generic_args,
            },
            #[cfg(feature = "ast-comments")]
            leading_comments: None,
            #[cfg(feature = "ast-comments")]
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
            #[cfg(feature = "ast-comments")]
            leading_comments: None,
            #[cfg(feature = "ast-comments")]
            trailing_comments,
          });
        }

        Ok(GroupEntry::ValueMemberKey {
          ge: Box::from(ValueMemberKeyEntry {
            occur,
            member_key: None,
            entry_type,
          }),
          #[cfg(feature = "ast-comments")]
          leading_comments: None,
          #[cfg(feature = "ast-comments")]
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
    ident: &'a str,
    socket: Option<token::SocketPlug>,
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
    let mut ident = self.identifier_from_ident_token(ident, socket);
    #[cfg(not(feature = "ast-span"))]
    let ident = self.identifier_from_ident_token(ident, socket);
    #[cfg(feature = "ast-span")]
    {
      ident.span = (begin_memberkey_range, end_t1_range, begin_memberkey_line);
    }

    self.next_token()?;

    #[cfg(feature = "ast-comments")]
    let comments_before_cut = self.collect_comments()?;
    #[cfg(not(feature = "ast-comments"))]
    self.advance_newline()?;

    let mk = if let Token::CUT = &self.cur_token {
      self.next_token()?;

      #[cfg(feature = "ast-comments")]
      let comments_after_cut = self.collect_comments()?;
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

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

      #[cfg(feature = "ast-comments")]
      let comments_after_arrowmap = if let Token::COMMENT(_) = self.peek_token {
        self.next_token()?;

        self.collect_comments()?
      } else {
        None
      };

      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      let t1 = MemberKey::Type1 {
        t1: Box::from(Type1 {
          type2: Type2::Typename {
            ident,
            generic_args: None,
            #[cfg(feature = "ast-span")]
            span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
          },
          operator: None,
          #[cfg(feature = "ast-comments")]
          comments_after_type: None,
          #[cfg(feature = "ast-span")]
          span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
        }),
        #[cfg(feature = "ast-comments")]
        comments_before_cut,
        is_cut: true,
        #[cfg(feature = "ast-comments")]
        comments_after_cut,
        #[cfg(feature = "ast-comments")]
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
    } else if let Token::ARROWMAP = &self.cur_token {
      #[cfg(feature = "ast-span")]
      let end_memberkey_range = self.lexer_position.range.1;

      #[cfg(feature = "ast-comments")]
      let comments_after_arrowmap = if let Token::COMMENT(_) = &self.peek_token {
        self.next_token()?;

        self.collect_comments()?
      } else {
        None
      };

      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      let t1 = MemberKey::Type1 {
        t1: Box::from(Type1 {
          type2: Type2::Typename {
            ident,
            generic_args: None,
            #[cfg(feature = "ast-span")]
            span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
          },
          operator: None,
          #[cfg(feature = "ast-comments")]
          comments_after_type: None,
          #[cfg(feature = "ast-span")]
          span: (begin_memberkey_range, end_t1_range, begin_memberkey_line),
        }),
        #[cfg(feature = "ast-comments")]
        comments_before_cut,
        is_cut: false,
        #[cfg(feature = "ast-comments")]
        comments_after_cut: None,
        #[cfg(feature = "ast-comments")]
        comments_after_arrowmap,
        #[cfg(feature = "ast-span")]
        span: (
          begin_memberkey_range,
          end_memberkey_range,
          begin_memberkey_line,
        ),
      };

      self.next_token()?;

      #[cfg(feature = "ast-comments")]
      let _ = self.collect_comments()?;
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      Some(t1)
    } else {
      if let Token::COLON = &self.cur_token {
        self.next_token()?;
      }

      #[cfg(feature = "ast-comments")]
      let comments_after_colon = self.collect_comments()?;
      #[cfg(not(feature = "ast-comments"))]
      self.advance_newline()?;

      Some(MemberKey::Bareword {
        ident,
        #[cfg(feature = "ast-comments")]
        comments: comments_before_cut,
        #[cfg(feature = "ast-comments")]
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

  #[allow(missing_docs)]
  pub fn parse_memberkey(&mut self, is_optional: bool) -> Result<Option<MemberKey<'a>>> {
    #[cfg(feature = "ast-span")]
    let begin_memberkey_range = self.lexer_position.range.0;
    #[cfg(feature = "ast-span")]
    let begin_memberkey_line = self.lexer_position.line;

    if let Some(t) = self.cur_token.in_standard_prelude() {
      return self.parse_memberkey_from_ident(
        is_optional,
        t,
        None,
        #[cfg(feature = "ast-span")]
        begin_memberkey_range,
        #[cfg(feature = "ast-span")]
        begin_memberkey_line,
      );
    }

    match &self.cur_token {
      Token::IDENT(ident, socket) => {
        let ident = *ident;
        let socket = *socket;

        self.parse_memberkey_from_ident(
          is_optional,
          ident,
          socket,
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

        #[cfg(feature = "ast-comments")]
        let comments_before_cut = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        let mk = if let Token::CUT = &self.cur_token {
          self.next_token()?;

          #[cfg(feature = "ast-comments")]
          let comments_after_cut = self.collect_comments()?;
          #[cfg(not(feature = "ast-comments"))]
          self.advance_newline()?;

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

          #[cfg(feature = "ast-comments")]
          let memberkey_comments = self.collect_comments()?;
          #[cfg(not(feature = "ast-comments"))]
          self.advance_newline()?;

          Some(MemberKey::Type1 {
            t1: Box::from(t1),
            #[cfg(feature = "ast-comments")]
            comments_before_cut,
            is_cut: true,
            #[cfg(feature = "ast-comments")]
            comments_after_cut,
            #[cfg(feature = "ast-comments")]
            comments_after_arrowmap: memberkey_comments,
            #[cfg(feature = "ast-span")]
            span: (
              begin_memberkey_range,
              end_memberkey_range,
              begin_memberkey_line,
            ),
          })
        } else {
          #[cfg(feature = "ast-comments")]
          let comments = self.collect_comments()?;
          #[cfg(not(feature = "ast-comments"))]
          self.advance_newline()?;

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

          #[cfg(feature = "ast-comments")]
          let memberkey_comments = self.collect_comments()?;
          #[cfg(not(feature = "ast-comments"))]
          self.advance_newline()?;

          Some(MemberKey::Value {
            value,
            #[cfg(feature = "ast-comments")]
            comments,
            #[cfg(feature = "ast-comments")]
            comments_after_colon: memberkey_comments,
            #[cfg(feature = "ast-span")]
            span: (
              begin_memberkey_range,
              self.parser_position.range.1,
              begin_memberkey_line,
            ),
          })
        };

        if let Token::COLON = &self.cur_token {
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

        #[cfg(feature = "ast-comments")]
        let comments_before_type_or_group = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        let mut tokens: Vec<lexer::Item> = Vec::new();

        #[cfg(feature = "ast-comments")]
        let mut comments_after_type_or_group = None;

        let mut has_group_entries = false;
        let mut closing_parend = false;
        #[cfg(feature = "ast-span")]
        let mut closing_parend_index = 0;
        while !closing_parend {
          if let Token::ARROWMAP
          | Token::COLON
          | Token::OPTIONAL
          | Token::ASTERISK
          | Token::GCHOICE = &self.cur_token
          {
            has_group_entries = true;
          }

          // TODO: parse nested comments
          if let Token::LPAREN = &self.cur_token {
            nested_parend_count += 1;
          }

          if let Token::RPAREN = &self.cur_token {
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

          tokens.push(Ok((self.lexer_position, self.cur_token.clone())));

          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range.1 = self.lexer_position.range.1;
          }

          self.next_token()?;

          #[cfg(feature = "ast-comments")]
          {
            comments_after_type_or_group = self.collect_comments()?;
          }
          #[cfg(not(feature = "ast-comments"))]
          self.advance_newline()?;

          if let Token::EOF = &self.cur_token {
            self.errors.push(Error::PARSER {
              #[cfg(feature = "ast-span")]
              position: self.lexer_position,
              msg: MissingClosingParend.into(),
            });

            return Err(Error::INCREMENTAL);
          }
        }

        // Create a new parser for the previously-lexed tokens.
        let mut parser = Parser::new(self.str_input, Box::new(tokens.into_iter()))?;
        parser.groupnames = self.groupnames.clone();
        parser.typenames = self.typenames.clone();

        // Parse tokens vec as group
        if has_group_entries {
          let group = match parser.parse_group() {
            Ok(g) => g,
            Err(Error::INCREMENTAL) => {
              for e in parser.errors.into_iter() {
                self.errors.push(e);
              }

              return Err(Error::INCREMENTAL);
            }
            Err(e) => return Err(e),
          };
          self
            .unknown_rule_idents
            .append(&mut parser.unknown_rule_idents);

          return Ok(Some(MemberKey::NonMemberKey {
            non_member_key: NonMemberKey::Group(group),
            #[cfg(feature = "ast-comments")]
            comments_before_type_or_group,
            #[cfg(feature = "ast-comments")]
            comments_after_type_or_group,
          }));
        }

        // Parse tokens vec as type
        let t = match parser.parse_type(None) {
          Ok(t) => t,
          Err(Error::INCREMENTAL) => {
            for e in parser.errors.into_iter() {
              self.errors.push(e);
            }

            return Err(Error::INCREMENTAL);
          }
          Err(e) => return Err(e),
        };
        self
          .unknown_rule_idents
          .append(&mut parser.unknown_rule_idents);

        #[cfg(feature = "ast-comments")]
        let comments_before_cut = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        if let Token::CUT = &self.cur_token {
          self.next_token()?;

          #[cfg(feature = "ast-comments")]
          let comments_after_cut = self.collect_comments()?;
          #[cfg(not(feature = "ast-comments"))]
          self.advance_newline()?;

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
                #[cfg(feature = "ast-comments")]
                comments_before_type: comments_before_type_or_group,
                #[cfg(feature = "ast-comments")]
                comments_after_type: comments_after_type_or_group,
                #[cfg(feature = "ast-span")]
                span: (
                  begin_memberkey_range,
                  closing_parend_index,
                  begin_memberkey_line,
                ),
              },
              #[cfg(feature = "ast-comments")]
              comments_after_type: comments_before_cut.clone(),
              operator: None,
              #[cfg(feature = "ast-span")]
              span: (
                begin_memberkey_range,
                closing_parend_index,
                begin_memberkey_line,
              ),
            }),
            #[cfg(feature = "ast-comments")]
            comments_before_cut,
            is_cut: true,
            #[cfg(feature = "ast-comments")]
            comments_after_cut,
            #[cfg(feature = "ast-comments")]
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

        let t1 = if let Token::ARROWMAP = &self.cur_token {
          self.next_token()?;

          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range.1 = self.lexer_position.range.1;
          }

          #[cfg(feature = "ast-comments")]
          let memberkey_comments = self.collect_comments()?;
          #[cfg(not(feature = "ast-comments"))]
          self.advance_newline()?;

          Some(MemberKey::Type1 {
            t1: Box::from(Type1 {
              type2: Type2::ParenthesizedType {
                pt: t,
                #[cfg(feature = "ast-comments")]
                comments_before_type: comments_before_type_or_group,
                #[cfg(feature = "ast-comments")]
                comments_after_type: comments_after_type_or_group,
                #[cfg(feature = "ast-span")]
                span: (
                  begin_memberkey_range,
                  closing_parend_index,
                  begin_memberkey_line,
                ),
              },
              #[cfg(feature = "ast-comments")]
              comments_after_type: comments_before_cut.clone(),
              operator: None,
              #[cfg(feature = "ast-span")]
              span: (
                begin_memberkey_range,
                closing_parend_index,
                begin_memberkey_line,
              ),
            }),
            #[cfg(feature = "ast-comments")]
            comments_before_cut,
            is_cut: false,
            #[cfg(feature = "ast-comments")]
            comments_after_cut: None,
            #[cfg(feature = "ast-comments")]
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
            #[cfg(feature = "ast-comments")]
            comments_before_type_or_group,
            #[cfg(feature = "ast-comments")]
            comments_after_type_or_group,
          })
        };

        Ok(t1)
      }
      _ => {
        let t1 = self.parse_type1(None)?;

        #[cfg(feature = "ast-comments")]
        let comments_before_cut = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        if let Token::CUT = &self.cur_token {
          self.next_token()?;

          #[cfg(feature = "ast-comments")]
          let comments_after_cut = self.collect_comments()?;
          #[cfg(not(feature = "ast-comments"))]
          self.advance_newline()?;

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

          #[cfg(feature = "ast-comments")]
          let memberkey_comments = self.collect_comments()?;
          #[cfg(not(feature = "ast-comments"))]
          self.advance_newline()?;

          return Ok(Some(MemberKey::Type1 {
            t1: Box::from(t1),
            #[cfg(feature = "ast-comments")]
            comments_before_cut,
            is_cut: true,
            #[cfg(feature = "ast-comments")]
            comments_after_cut,
            #[cfg(feature = "ast-comments")]
            comments_after_arrowmap: memberkey_comments,
            #[cfg(feature = "ast-span")]
            span: (
              begin_memberkey_range,
              end_memberkey_range,
              begin_memberkey_line,
            ),
          }));
        }

        let t1 = if let Token::ARROWMAP = &self.cur_token {
          self.next_token()?;

          #[cfg(feature = "ast-span")]
          {
            self.parser_position.range.1 = self.lexer_position.range.1;
          }

          #[cfg(feature = "ast-comments")]
          let memberkey_comments = self.collect_comments()?;
          #[cfg(not(feature = "ast-comments"))]
          self.advance_newline()?;

          Some(MemberKey::Type1 {
            t1: Box::from(t1),
            #[cfg(feature = "ast-comments")]
            comments_before_cut,
            is_cut: false,
            #[cfg(feature = "ast-comments")]
            comments_after_cut: None,
            #[cfg(feature = "ast-comments")]
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
                #[cfg(feature = "ast-comments")]
                comments_before_type: None,
                #[cfg(feature = "ast-comments")]
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
            #[cfg(feature = "ast-comments")]
            comments_before_type_or_group: None,
            #[cfg(feature = "ast-comments")]
            comments_after_type_or_group: comments_before_cut,
          })
        };

        Ok(t1)
      }
    }
  }

  #[allow(missing_docs)]
  pub fn parse_occur(&mut self, is_optional: bool) -> Result<Option<Occurrence<'a>>> {
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

        #[cfg(feature = "ast-comments")]
        let comments = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        Ok(Some(Occurrence {
          #[cfg(feature = "ast-span")]
          occur: Occur::Optional {
            span: (
              self.parser_position.range.0,
              self.parser_position.range.1,
              self.parser_position.line,
            ),
          },
          #[cfg(not(feature = "ast-span"))]
          occur: Occur::Optional {},
          #[cfg(feature = "ast-comments")]
          comments,
          _a: PhantomData,
        }))
      }
      Token::ONEORMORE => {
        #[cfg(feature = "ast-span")]
        {
          self.parser_position.range = self.lexer_position.range;
        }

        self.next_token()?;

        #[cfg(feature = "ast-comments")]
        let comments = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        Ok(Some(Occurrence {
          #[cfg(feature = "ast-span")]
          occur: Occur::OneOrMore {
            span: (
              self.parser_position.range.0,
              self.parser_position.range.1,
              self.parser_position.line,
            ),
          },
          #[cfg(not(feature = "ast-span"))]
          occur: Occur::OneOrMore {},
          #[cfg(feature = "ast-comments")]
          comments,
          _a: PhantomData,
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
            Occur::ZeroOrMore {
              span: (
                self.parser_position.range.0,
                self.parser_position.range.1,
                self.parser_position.line,
              ),
            }
          }

          #[cfg(not(feature = "ast-span"))]
          Occur::ZeroOrMore {}
        };

        self.next_token()?;

        if let Token::VALUE(token::Value::UINT(_)) = &self.cur_token {
          self.next_token()?;
        }

        #[cfg(feature = "ast-comments")]
        let comments = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

        Ok(Some(Occurrence {
          occur,
          #[cfg(feature = "ast-comments")]
          comments,
          _a: PhantomData,
        }))
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

        #[cfg(feature = "ast-comments")]
        let comments = self.collect_comments()?;
        #[cfg(not(feature = "ast-comments"))]
        self.advance_newline()?;

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
          #[cfg(feature = "ast-comments")]
          comments,
          _a: PhantomData,
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
    ident: &'a str,
    socket: Option<token::SocketPlug>,
  ) -> Identifier<'a> {
    Identifier {
      ident,
      socket,
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
/// * `input` - A string slice with the CDDL text input
/// * `print_stderr` - When true, print any errors to stderr
///
/// # Example
///
/// ```
/// use cddl::parser::cddl_from_str;
///
/// let input = r#"myrule = int"#;
/// let _ = cddl_from_str(input, true);
#[cfg(not(target_arch = "wasm32"))]
#[cfg(feature = "std")]
pub fn cddl_from_str(input: &str, print_stderr: bool) -> std::result::Result<CDDL<'_>, String> {
  match Parser::new(input, Box::new(lexer::lexer_from_str(input).iter())).map_err(|e| e.to_string())
  {
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

/// Identify root type name from CDDL input string
#[cfg(feature = "std")]
#[cfg(not(target_arch = "wasm32"))]
pub fn root_type_name_from_cddl_str(input: &str) -> std::result::Result<String, String> {
  let cddl = cddl_from_str(input, false)?;

  for r in cddl.rules.iter() {
    // First type rule is root
    if let Rule::Type { rule, .. } = r {
      if rule.generic_params.is_none() {
        return Ok(rule.name.to_string());
      }
    }
  }

  Err("cddl spec contains no root type".to_string())
}

impl CDDL<'_> {
  /// Parses CDDL from a byte slice
  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(feature = "std")]
  pub fn from_slice(input: &[u8]) -> std::result::Result<CDDL<'_>, String> {
    let str_input = std::str::from_utf8(input).map_err(|e| e.to_string())?;

    match Parser::new(str_input, Box::new(lexer::Lexer::from_slice(input).iter()))
      .map_err(|e| e.to_string())
    {
      Ok(mut p) => match p.parse_cddl() {
        Ok(c) => Ok(c),
        Err(Error::INCREMENTAL) => {
          if let Ok(Some(e)) = p.report_errors(false) {
            return Err(e);
          }

          Err(Error::INCREMENTAL.to_string())
        }
        Err(e) => Err(e.to_string()),
      },
      Err(e) => Err(e),
    }
  }

  /// Parses CDDL from a byte slice
  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(not(feature = "std"))]
  pub fn from_slice(input: &[u8]) -> std::result::Result<CDDL<'_>, String> {
    let str_input = std::str::from_utf8(input).map_err(|e| e.to_string())?;

    match Parser::new(str_input, Box::new(lexer::Lexer::from_slice(input).iter()))
      .map_err(|e| e.to_string())
    {
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
/// use cddl::cddl_from_str;
///
/// let input = r#"myrule = int"#;
///
/// let _ = cddl_from_str(input);
/// ```
#[cfg(not(target_arch = "wasm32"))]
#[cfg(not(feature = "std"))]
pub fn cddl_from_str(input: &str) -> std::result::Result<CDDL<'_>, String> {
  match Parser::new(input, Box::new(lexer::lexer_from_str(input).iter())).map_err(|e| e.to_string())
  {
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

  match Parser::new(input, Box::new(lexer::Lexer::new(input).iter())) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(c) => serde_wasm_bindgen::to_value(&c).map_err(|e| JsValue::from(e.to_string())),
      Err(Error::INCREMENTAL) => {
        if !p.errors.is_empty() {
          return Err(
            serde_wasm_bindgen::to_value(
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

  match Parser::new(input, Box::new(lexer::Lexer::new(input).iter())) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(c) => Ok(c.to_string()),
      Err(Error::INCREMENTAL) => {
        if !p.errors.is_empty() {
          return Err(
            serde_wasm_bindgen::to_value(
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
mod tests {
  use super::*;
  use crate::lexer;

  #[test]
  fn test_multiple_rules_with_reference_to_parenthesized_type() {
    let input = r#"basic = (d: #6.23(uint), e: bytes)
        outer = [a: uint, b: basic, c: "some text"]"#;

    // Use the parser directly for better error diagnostics
    let mut parser = Parser::new(input, Box::new(lexer::lexer_from_str(input).iter())).unwrap();
    let result = parser.parse_cddl();

    // Ensure there are no errors
    assert!(result.is_ok(), "Parser errors: {:?}", parser.errors);

    // Check that the CDDL contains two rules
    let cddl = result.unwrap();
    assert_eq!(cddl.rules.len(), 2);

    // Verify rule names
    let rule_names: Vec<_> = cddl.rules.iter().map(|r| r.name()).collect();
    assert!(rule_names.contains(&"basic".to_string()));
    assert!(rule_names.contains(&"outer".to_string()));
  }
}
