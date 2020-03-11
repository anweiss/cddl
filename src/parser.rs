use super::{
  ast::*,
  lexer::{Lexer, LexerError, Position},
  token::{self, Token, Value},
};
use annotate_snippets::{
  display_list::DisplayList,
  formatter::DisplayListFormatter,
  snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use regex;
use std::{fmt, mem, result};

#[cfg(not(feature = "std"))]
use alloc::{
  boxed::Box,
  string::{String, ToString},
  vec::Vec,
};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

/// Alias for `Result` with an error of type `cddl::ParserError`
pub type Result<T> = result::Result<T, Error>;

/// Parser type
#[derive(Debug)]
pub struct Parser<'a> {
  l: Lexer<'a>,
  cur_token: Token,
  peek_token: Token,
  lexer_position: Position,
  peek_lexer_position: Position,
  parser_position: Position,
  errors: Vec<ParserError>,
}

/// Parsing error types
#[derive(Debug)]
pub enum Error {
  /// Parsing error
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

impl<'a> Parser<'a> {
  /// Create a new `Parser` from a given `Lexer`
  pub fn new(l: Lexer<'a>) -> Result<Parser> {
    let mut p = Parser {
      l,
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

  /// Print parser errors if there are any
  pub fn print_errors(self) -> Option<String> {
    let dlf = DisplayListFormatter::new(false, false);
    if self.errors.is_empty() {
      return None;
    }

    let mut errors = String::new();

    for error in self.errors.into_iter() {
      errors.push_str(&format! {
        "{}\n\n",
        dlf
          .format(&DisplayList::from(Snippet {
            title: Some(Annotation {
              label: Some(error.message.to_string()),
              id: None,
              annotation_type: AnnotationType::Error,
            }),
            footer: vec![],
            slices: vec![Slice {
              source: String::from_utf8(self.l.str_input.clone()).ok()?,
              line_start: error.position.line,
              origin: Some("input".to_string()),
              fold: false,
              annotations: vec![SourceAnnotation {
                range: error.position.range,
                label: error.message,
                annotation_type: AnnotationType::Error,
              }],
            }],
          }))
          .to_string(),
      })
    }

    Some(errors)
  }

  fn next_token(&mut self) -> Result<()> {
    mem::swap(&mut self.cur_token, &mut self.peek_token);
    mem::swap(&mut self.lexer_position, &mut self.peek_lexer_position);
    let nt = self.l.next_token().map_err(Error::LEXER)?;
    self.peek_lexer_position = nt.0;
    self.peek_token = nt.1;

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
      }
    }

    Ok(())
  }

  /// Parses into a `CDDL` AST
  pub fn parse_cddl(&mut self) -> Result<CDDL> {
    let mut c = CDDL::default();

    while self.cur_token != Token::EOF {
      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }
      match self.parse_rule() {
        Ok(r) => {
          let rule_exists = |existing_rule: &&Rule| {
            r.name() == existing_rule.name() && !existing_rule.is_choice_alternate()
          };
          if let Some(r) = c.rules.iter().find(rule_exists) {
            self.parser_position.range = (r.span().0, r.span().1);
            self.parser_position.line = r.span().2;

            self.errors.push(ParserError {
              position: self.parser_position,
              message: format!("Rule with name '{}' already defined", r.name()),
            });

            if !self.cur_token_is(Token::EOF) {
              self.advance_to_next_rule()?;
            }
          }
          c.rules.push(r);
          while let Token::COMMENT(_) = self.cur_token {
            self.next_token()?;
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

    Ok(c)
  }

  fn parse_rule(&mut self) -> Result<Rule> {
    let begin_rule_range = self.lexer_position.range.0;
    let begin_rule_line = self.lexer_position.line;

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    let ident = match &self.cur_token {
      Token::IDENT(i) => self.identifier_from_ident_token(i.clone()),
      _ => {
        self.parser_position.range = self.lexer_position.range;
        self.parser_position.line = self.lexer_position.line;

        self.errors.push(ParserError {
          position: self.parser_position,
          message: format!("expected rule identifier. Got '{}'", self.cur_token),
        });

        self.advance_to_next_rule()?;

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
      // Check for an occurrence indicator if uint followed by an asterisk '*'
      Token::VALUE(Value::UINT(_)) => {
        if self.peek_token_is(&Token::ASTERISK) {
          let ge = self.parse_grpent()?;

          let span = (
            begin_rule_range,
            self.parser_position.range.1,
            begin_rule_line,
          );

          Ok(Rule::Group(Box::from(GroupRule {
            name: ident,
            generic_param: gp,
            is_group_choice_alternate,
            entry: ge,
            span,
          })))
        } else {
          let t = self.parse_type(None)?;
          let span = (
            begin_rule_range,
            self.parser_position.range.1,
            begin_rule_line,
          );
          Ok(Rule::Type(TypeRule {
            name: ident,
            generic_param: gp,
            is_type_choice_alternate,
            value: t,
            span,
          }))
        }
      }
      Token::LPAREN | Token::ASTERISK | Token::ONEORMORE | Token::OPTIONAL => {
        let ge = self.parse_grpent()?;

        let mut end_rule_range = self.parser_position.range.1;

        // If a group entry is an inline group with no leading occurrence
        // indicator, and its group has only a single element that is not
        // preceded by an occurrence indicator nor member key, treat it as a
        // parenthesized type, subsequently parsing the remaining type and
        // returning the type rule. This is the only situation where `clone` is
        // required
        if let GroupEntry::InlineGroup((None, g)) = &ge {
          if g.0.len() == 1 {
            if let Some(gc) = g.0.get(0) {
              if gc.0.len() == 1 {
                if let Some(ge) = gc.0.get(0) {
                  // Check that there is no trailing comma
                  if !ge.1 {
                    // TODO: Replace with box pattern destructuring once supported in stable
                    if let GroupEntry::ValueMemberKey(vmke) = &ge.0 {
                      if vmke.occur.is_none() && vmke.member_key.is_none() {
                        let value = self
                          .parse_type(Some(Type2::ParenthesizedType(vmke.entry_type.clone())))?;

                        end_rule_range = self.parser_position.range.1;

                        return Ok(Rule::Type(TypeRule {
                          name: ident,
                          generic_param: gp,
                          is_type_choice_alternate,
                          value,
                          span: (begin_rule_range, end_rule_range, begin_rule_line),
                        }));
                      }
                    }
                  }
                }
              }
            }
          }
        }

        Ok(Rule::Group(Box::from(GroupRule {
          name: ident,
          generic_param: gp,
          is_group_choice_alternate,
          entry: ge,
          span: (begin_rule_range, end_rule_range, begin_rule_line),
        })))
      }
      _ => {
        let t = self.parse_type(None)?;

        let span = (
          begin_rule_range,
          self.parser_position.range.1,
          begin_rule_line,
        );

        Ok(Rule::Type(TypeRule {
          name: ident,
          generic_param: gp,
          is_type_choice_alternate,
          value: t,
          span,
        }))
      }
    }
  }

  fn parse_genericparm(&mut self) -> Result<GenericParm> {
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
            .push(self.identifier_from_ident_token(ident.clone()));
          self.next_token()?;

          if !self.cur_token_is(Token::COMMA) && !self.cur_token_is(Token::RANGLEBRACKET) {
            self.parser_position.range = (begin_range, self.lexer_position.range.0);
            self.parser_position.line = self.lexer_position.line;

            self.errors.push(ParserError {
              position: self.parser_position,
              message: "Expecting comma between generic parameters or closing right angle bracket"
                .into(),
            });

            return Err(Error::PARSER);
          }
        }
        Token::COMMA => self.next_token()?,
        Token::COMMENT(_) => self.next_token()?,
        _ => {
          self.parser_position.range = (self.lexer_position.range.0, self.lexer_position.range.1);
          self.parser_position.line = self.lexer_position.line;

          self.errors.push(ParserError {
            position: self.parser_position,
            message: format!("Illegal token {}", self.cur_token),
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

  fn parse_genericarg(&mut self) -> Result<GenericArg> {
    if self.peek_token_is(&Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    let begin_generic_arg_range = self.lexer_position.range.0;
    let begin_generic_arg_line = self.lexer_position.line;

    // Required for type2 mutual recursion
    if self.cur_token_is(Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    let mut generic_args = GenericArg::new();

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      generic_args.args.push(self.parse_type1(None)?);
      if let Token::COMMA | Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
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

  fn parse_type(&mut self, parenthesized_type: Option<Type2>) -> Result<Type> {
    let mut t = Type(Vec::new());

    t.0.push(self.parse_type1(parenthesized_type)?);

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    while self.cur_token_is(Token::TCHOICE) {
      self.next_token()?;

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      t.0.push(self.parse_type1(None)?);
    }

    Ok(t)
  }

  fn parse_type1(&mut self, parenthesized_type: Option<Type2>) -> Result<Type1> {
    let t2_1 = if let Some(t2) = parenthesized_type {
      t2
    } else {
      self.parse_type2()?
    };

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    let op = match &self.cur_token {
      Token::RANGEOP(i) => Some(RangeCtlOp::RangeOp(*i)),
      _ => match token::control_str_from_token(&self.cur_token) {
        Some(co) => Some(RangeCtlOp::CtlOp(co)),
        None => None,
      },
    };

    match op {
      Some(o) => {
        self.next_token()?;

        Ok(Type1 {
          type2: t2_1,
          operator: Some((o, self.parse_type2()?)),
        })
      }
      None => Ok(Type1 {
        type2: t2_1,
        operator: None,
      }),
    }
  }

  fn parse_type2(&mut self) -> Result<Type2> {
    let t2 = match &self.cur_token {
      // value
      Token::VALUE(value) => Ok(value.into()),

      // typename [genericarg]
      Token::IDENT(ident) => {
        // optional genericarg detected
        if self.peek_token_is(&Token::LANGLEBRACKET) {
          return Ok(Type2::Typename((
            self.identifier_from_ident_token(ident.clone()),
            Some(self.parse_genericarg()?),
          )));
        }

        Ok(Type2::Typename((
          self.identifier_from_ident_token(ident.clone()),
          None,
        )))
      }

      // ( type )
      // TODO: Develop additional test cases
      Token::LPAREN => {
        self.next_token()?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        Ok(Type2::ParenthesizedType(self.parse_type(None)?))
      }

      // { group }
      Token::LBRACE => {
        // self.next_token()?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        Ok(Type2::Map(self.parse_group()?))
      }

      // [ group ]
      Token::LBRACKET => {
        // self.next_token()?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        Ok(Type2::Array(self.parse_group()?))
      }

      // ~ typename [genericarg]
      Token::UNWRAP => {
        self.next_token()?;

        if let Token::IDENT(ident) = &self.cur_token {
          let ident = self.identifier_from_ident_token(ident.clone());

          if self.peek_token_is(&Token::LANGLEBRACKET) {
            self.next_token()?;

            return Ok(Type2::Unwrap((ident, Some(self.parse_genericarg()?))));
          }

          return Ok(Type2::Unwrap((ident, None)));
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
        self.next_token()?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        match &self.cur_token {
          Token::LPAREN => {
            self.next_token()?;

            Ok(Type2::ChoiceFromInlineGroup(self.parse_group()?))
          }
          Token::IDENT(ident) => {
            let ident = self.identifier_from_ident_token(ident.clone());
            if self.peek_token_is(&Token::LANGLEBRACKET) {
              self.next_token()?;

              return Ok(Type2::ChoiceFromGroup((
                ident,
                Some(self.parse_genericarg()?),
              )));
            }

            Ok(Type2::ChoiceFromGroup((ident, None)))
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

            Ok(Type2::TaggedData((tag, t)))
          }
          // Tagged data of a major type
          (Some(mt), tag) => Ok(Type2::TaggedDataMajorType((mt, tag))),
          _ => Ok(Type2::Any),
        }
      }
      _ => {
        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        match self.cur_token.in_standard_prelude() {
          Some(s) => Ok(Type2::Typename((
            self.identifier_from_ident_token((s.into(), None)),
            None,
          ))),
          None => {
            self.errors.push(ParserError {
              position: self.parser_position,
              message: format!(
                "Unknown type2 alternative. Unknown token: {:#?}",
                self.cur_token
              ),
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

  fn parse_group(&mut self) -> Result<Group> {
    let mut group = Group(Vec::new());

    group.0.push(self.parse_grpchoice()?);

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    while self.cur_token_is(Token::GCHOICE) {
      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      group.0.push(self.parse_grpchoice()?);
    }

    Ok(group)
  }

  fn parse_grpchoice(&mut self) -> Result<GroupChoice> {
    let mut grpchoice = GroupChoice(Vec::new());

    if self.cur_token_is(Token::LBRACE)
      || self.cur_token_is(Token::LPAREN)
      || self.cur_token_is(Token::LBRACKET)
      || self.cur_token_is(Token::GCHOICE)
    {
      self.next_token()?;
    }

    // TODO: The logic in this while loop is quite messy. Need to figure out a
    // better way to advance the token when parsing the entries in a group
    // choice
    while !self.cur_token_is(Token::RBRACE)
      && !self.cur_token_is(Token::RPAREN)
      && !self.cur_token_is(Token::RBRACKET)
      && !self.cur_token_is(Token::EOF)
    {
      let ge = self.parse_grpent()?;

      if self.cur_token_is(Token::GCHOICE) {
        grpchoice.0.push((ge, false));
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
      {
        self.next_token()?;
        self.parser_position.range.1 = self.lexer_position.range.1;
      }

      if self.cur_token_is(Token::COMMA) {
        grpchoice.0.push((ge, true));
        self.next_token()?;
      } else {
        grpchoice.0.push((ge, false));
      }

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }
    }

    Ok(grpchoice)
  }

  fn parse_grpent(&mut self) -> Result<GroupEntry> {
    let occur = self.parse_occur(true)?;

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    let member_key = self.parse_memberkey(true)?;

    if member_key.is_some() {
      // Two member keys in a row indicates a malformed entry
      if let Some(mk) = self.parse_memberkey(true)? {
        self.errors.push(ParserError {
          position: self.parser_position,
          message: format!(
            "Incomplete group entry for memberkey {}. Missing entry type",
            mk
          ),
        });

        return Err(Error::PARSER);
      }
    }

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    if self.cur_token_is(Token::LPAREN) {
      self.next_token()?;

      // TODO: Keep track of parenthesis count
      while self.cur_token_is(Token::LPAREN) {
        self.next_token()?;
      }

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      let ge = GroupEntry::InlineGroup((occur, self.parse_group()?));

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      while self.cur_token_is(Token::RPAREN) {
        self.parser_position.range.1 = self.lexer_position.range.1;
        self.next_token()?;
      }

      return Ok(ge);
    }

    let entry_type = self.parse_type(None)?;

    if member_key.is_some() {
      return Ok(GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
        occur,
        member_key,
        entry_type,
      })));
    }

    if let Some((name, generic_arg)) = entry_type.groupname_entry() {
      return Ok(GroupEntry::TypeGroupname(TypeGroupnameEntry {
        occur,
        name,
        generic_arg,
      }));
    }

    Ok(GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      occur,
      member_key,
      entry_type,
    })))
  }

  fn parse_memberkey(&mut self, is_optional: bool) -> Result<Option<MemberKey>> {
    match &self.peek_token {
      Token::ARROWMAP | Token::CUT => {
        let t1 = self.parse_type1(None)?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        if self.cur_token_is(Token::CUT) {
          self.next_token()?;

          while let Token::COMMENT(_) = self.cur_token {
            self.next_token()?;
          }

          let t1 = Some(MemberKey::Type1(Box::from((t1, true))));

          self.next_token()?;

          return Ok(t1);
        }

        let t1 = Some(MemberKey::Type1(Box::from((t1, false))));

        self.next_token()?;

        Ok(t1)
      }
      Token::COLON => {
        let mk = match &self.cur_token {
          Token::IDENT(ident) => Some(MemberKey::Bareword(
            self.identifier_from_ident_token(ident.clone()),
          )),
          Token::VALUE(value) => Some(MemberKey::Value(value.clone())),
          _ => {
            self.errors.push(ParserError {
              position: self.parser_position,
              message: "Malformed memberkey".into(),
            });
            return Err(Error::PARSER);
          }
        };

        self.next_token()?;
        self.next_token()?;

        Ok(mk)
      }
      _ => {
        if let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;

          return self.parse_memberkey(is_optional);
        }

        if !is_optional {
          self.errors.push(ParserError {
            position: self.lexer_position,
            message: "Malformed memberkey. Missing \":\" or \"=>\"".into(),
          });

          return Err(Error::PARSER);
        }

        Ok(None)
      }
    }
  }

  fn parse_occur(&mut self, is_optional: bool) -> Result<Option<Occur>> {
    match &self.cur_token {
      Token::OPTIONAL => {
        self.next_token()?;
        Ok(Some(Occur::Optional))
      }
      Token::ONEORMORE => {
        self.next_token()?;
        Ok(Some(Occur::OneOrMore))
      }
      Token::ASTERISK => {
        let o = if let Token::VALUE(Value::UINT(u)) = &self.peek_token {
          Some(Occur::Exact((None, Some(*u))))
        } else {
          Some(Occur::ZeroOrMore)
        };

        self.next_token()?;

        if let Token::VALUE(Value::UINT(_)) = &self.cur_token {
          self.next_token()?;
        }
        Ok(o)
      }
      Token::VALUE(_) => {
        let lower = if let Token::VALUE(value) = &self.cur_token {
          if let Value::UINT(li) = value {
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
        self.next_token()?;

        let upper = if let Token::VALUE(value) = &self.cur_token {
          if let Value::UINT(ui) = value {
            Some(*ui)
          } else {
            None
          }
        } else {
          None
        };

        Ok(Some(Occur::Exact((lower, upper))))
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

    self.peek_error(t);

    Ok(false)
  }

  fn peek_error(&mut self, t: &Token) {
    self.errors.push(ParserError {
      position: self.lexer_position,
      message: format!(
        "expected next token to be {:?}, got {:?} instead",
        t, self.peek_token
      ),
    })
  }

  /// Create `Identifier` from `Token::IDENT(ident)`
  pub fn identifier_from_ident_token(
    &self,
    ident: (String, Option<token::SocketPlug>),
  ) -> Identifier {
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
#[cfg(not(target_arch = "wasm32"))]
pub fn cddl_from_str(input: &str) -> std::result::Result<CDDL, String> {
  match Parser::new(Lexer::new(input)).map_err(|e| e.to_string()) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(c) => Ok(c),
      Err(Error::PARSER) => Err(p.print_errors().unwrap()),
      Err(e) => Err(e.to_string()),
    },
    Err(e) => Err(e),
  }
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn cddl_from_str(input: &str) -> result::Result<JsValue, JsValue> {
  let c = Parser::new(Lexer::new(input))
    .map_err(|e| JsValue::from(e.to_string()))?
    .parse_cddl()
    .map_err(|e| JsValue::from(e.to_string()))?;

  JsValue::from_serde(&c)
    .map_err(|e| JsValue::from(e.to_string()))
    .map(|c| c)
}

/// Validates CDDL input against RFC 8610
#[cfg(not(target_arch = "wasm32"))]
pub fn compile_cddl_from_str(input: &str) -> Result<()> {
  Parser::new(Lexer::new(input))?.parse_cddl().map(|_| ())
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn compile_cddl_from_str(input: &str) -> result::Result<(), JsValue> {
  Parser::new(Lexer::new(input))
    .map_err(|e| JsValue::from(e.to_string()))?
    .parse_cddl()
    .map_err(|e| JsValue::from(e.to_string()))
    .map(|_| ())
}

#[cfg(test)]
#[allow(unused_imports)]
mod tests {
  use super::{
    super::{ast, lexer::Lexer, token::SocketPlug},
    *,
  };
  use pretty_assertions::assert_eq;

  #[test]
  fn verify_cddl() -> Result<()> {
    let input = r#"myrule = secondrule
myrange = 10..upper
upper = 500 / 600
gr = 2* ( test )
messages = message<"reboot", "now">
message<t, v> = {type: 2, value: v}"#;

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    let cddl = p.parse_cddl()?;

    if let Some(e) = p.print_errors() {
      #[cfg(std)]
      println!("{}", e);

      return Err(Error::PARSER);
    }

    let expected_output = CDDL {
      rules: vec![
        Rule::Type(TypeRule {
          name: Identifier {
            ident: "myrule".into(),
            socket: None,
            span: (0, 6, 1),
          },
          generic_param: None,
          is_type_choice_alternate: false,
          value: Type(vec![Type1 {
            type2: Type2::Typename((
              Identifier {
                ident: "secondrule".into(),
                socket: None,
                span: (9, 19, 1),
              },
              None,
            )),
            operator: None,
          }]),
          span: (0, 19, 1),
        }),
        Rule::Type(TypeRule {
          name: Identifier {
            ident: "myrange".into(),
            socket: None,
            span: (20, 27, 2),
          },
          generic_param: None,
          is_type_choice_alternate: false,
          value: Type(vec![Type1 {
            type2: Type2::UintValue(10),
            operator: Some((
              RangeCtlOp::RangeOp(true),
              Type2::Typename((
                Identifier {
                  ident: "upper".into(),
                  socket: None,
                  span: (34, 39, 2),
                },
                None,
              )),
            )),
          }]),
          span: (20, 39, 2),
        }),
        Rule::Type(TypeRule {
          name: Identifier {
            ident: "upper".into(),
            socket: None,
            span: (40, 45, 3),
          },
          generic_param: None,
          is_type_choice_alternate: false,
          value: Type(vec![
            Type1 {
              type2: Type2::UintValue(500),
              operator: None,
            },
            Type1 {
              type2: Type2::UintValue(600),
              operator: None,
            },
          ]),
          span: (40, 57, 3),
        }),
        Rule::Group(Box::from(GroupRule {
          name: Identifier {
            ident: "gr".into(),
            socket: None,
            span: (58, 60, 4),
          },
          generic_param: None,
          is_group_choice_alternate: false,
          entry: GroupEntry::InlineGroup((
            Some(Occur::Exact((Some(2), None))),
            Group(vec![GroupChoice(vec![(
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: None,
                name: Identifier {
                  ident: "test".into(),
                  socket: None,
                  span: (68, 72, 4),
                },
                generic_arg: None,
              }),
              false,
            )])]),
          )),
          span: (58, 74, 4),
        })),
        Rule::Type(TypeRule {
          name: Identifier {
            ident: "messages".into(),
            socket: None,
            span: (75, 83, 5),
          },
          generic_param: None,
          is_type_choice_alternate: false,
          value: Type(vec![Type1 {
            type2: Type2::Typename((
              Identifier {
                ident: "message".into(),
                socket: None,
                span: (86, 93, 5),
              },
              Some(GenericArg {
                args: vec![
                  Type1 {
                    type2: Type2::TextValue("reboot".into()),
                    operator: None,
                  },
                  Type1 {
                    type2: Type2::TextValue("now".into()),
                    operator: None,
                  },
                ],
                span: (93, 110, 5),
              }),
            )),
            operator: None,
          }]),
          span: (75, 110, 5),
        }),
        Rule::Type(TypeRule {
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
          value: Type(vec![Type1 {
            type2: Type2::Map(Group(vec![GroupChoice(vec![
              (
                GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
                  occur: None,
                  member_key: Some(MemberKey::Bareword(Identifier {
                    ident: "type".into(),
                    socket: None,
                    span: (128, 132, 6),
                  })),
                  entry_type: Type(vec![Type1 {
                    type2: Type2::UintValue(2),
                    operator: None,
                  }]),
                })),
                true,
              ),
              (
                GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
                  occur: None,
                  member_key: Some(MemberKey::Bareword(Identifier {
                    ident: "value".into(),
                    socket: None,
                    span: (137, 142, 6),
                  })),
                  entry_type: Type(vec![Type1 {
                    type2: Type2::Typename((
                      Identifier {
                        ident: "v".into(),
                        socket: None,
                        span: (144, 145, 6),
                      },
                      None,
                    )),
                    operator: None,
                  }]),
                })),
                false,
              ),
            ])])),
            operator: None,
          }]),
          span: (111, 146, 6),
        }),
      ],
    };

    assert_eq!(cddl, expected_output);
    assert_eq!(cddl.to_string(), expected_output.to_string());

    Ok(())
  }

  #[test]
  fn verify_rule_diagnostic() -> Result<()> {
    let input = r#"a = 1234

    a = b"#;

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    let _ = p.parse_cddl()?;

    //   match p.parse_cddl() {
    //     Ok(_) => Ok(()),
    //     Err(Error::PARSER) => {
    //       assert_eq!(
    //         p.print_errors().unwrap(),
    //         r#"error: Rule with name 'a' already defined
    //  --> input:1:1
    //   |
    // 1 | a = 1234
    //   | ^^^^^^^^ Rule with name 'a' already defined
    // 2 |
    // 3 |   a = b
    //   |"#
    //       );

    //       return Ok(());
    //     }
    //     Err(e) => Err(e),
    //   }

    Ok(())
  }

  #[test]
  fn verify_genericparm() -> Result<()> {
    let input = r#"<t, v>"#;

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    let gps = p.parse_genericparm()?;

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

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    match p.parse_genericparm() {
      Ok(_) => Ok(()),
      Err(Error::PARSER) => {
        assert_eq!(
          p.print_errors().unwrap(),
          r#"error: Illegal token 1
 --> input:1:1
  |
1 | <1, 2>
  |  ^ Illegal token 1
  |

"#
        );

        Ok(())
      }
      Err(e) => Err(e),
    }
  }

  #[test]
  fn verify_genericarg() -> Result<()> {
    let input = r#"<"reboot", "now">"#;

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    let generic_args = p.parse_genericarg()?;

    let expected_output = GenericArg {
      args: vec![
        Type1 {
          type2: Type2::TextValue("reboot".into()),
          operator: None,
        },
        Type1 {
          type2: Type2::TextValue("now".into()),
          operator: None,
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
    let input = r#"tchoice1 / tchoice2"#;

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    let t = p.parse_type(None)?;

    let expected_output = Type(vec![
      Type1 {
        type2: Type2::Typename((
          Identifier {
            ident: "tchoice1".into(),
            socket: None,
            span: (0, 8, 1),
          },
          None,
        )),
        operator: None,
      },
      Type1 {
        type2: Type2::Typename((
          Identifier {
            ident: "tchoice2".into(),
            socket: None,
            span: (11, 19, 1),
          },
          None,
        )),
        operator: None,
      },
    ]);

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
        type2: Type2::UintValue(5),
        operator: Some((RangeCtlOp::RangeOp(true), Type2::UintValue(10))),
      },
      Type1 {
        type2: Type2::FloatValue(-10.5),
        operator: Some((RangeCtlOp::RangeOp(false), Type2::FloatValue(10.1))),
      },
      Type1 {
        type2: Type2::FloatValue(1.5),
        operator: Some((RangeCtlOp::RangeOp(true), Type2::FloatValue(4.5))),
      },
      Type1 {
        type2: Type2::Typename((
          Identifier {
            ident: "my..lower".into(),
            socket: None,
            span: (0, 9, 1),
          },
          None,
        )),
        operator: Some((
          RangeCtlOp::RangeOp(false),
          Type2::Typename((
            Identifier {
              ident: "upper".into(),
              socket: None,
              span: (14, 19, 1),
            },
            None,
          )),
        )),
      },
      Type1 {
        type2: Type2::Typename((
          Identifier {
            ident: "target".into(),
            socket: None,
            span: (0, 6, 1),
          },
          None,
        )),
        operator: Some((
          RangeCtlOp::CtlOp(".lt"),
          Type2::Typename((
            Identifier {
              ident: "controller".into(),
              socket: None,
              span: (11, 21, 1),
            },
            None,
          )),
        )),
      },
      Type1 {
        type2: Type2::ParenthesizedType(Type(vec![
          Type1 {
            type2: Type2::Typename((
              Identifier {
                ident: "text".into(),
                socket: None,
                span: (2, 6, 1),
              },
              None,
            )),
            operator: None,
          },
          Type1 {
            type2: Type2::Typename((
              Identifier {
                ident: "tstr".into(),
                socket: None,
                span: (9, 13, 1),
              },
              None,
            )),
            operator: None,
          },
        ])),
        operator: Some((RangeCtlOp::CtlOp(".eq"), Type2::TextValue("hello".into()))),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(l)?;

      let t1 = p.parse_type1(None)?;

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
    ];

    let expected_outputs = [
      Type2::TextValue("myvalue".into()),
      Type2::Typename((
        Identifier {
          ident: "message".into(),
          socket: None,
          span: (0, 7, 1),
        },
        Some(GenericArg {
          args: vec![
            Type1 {
              type2: Type2::TextValue("reboot".into()),
              operator: None,
            },
            Type1 {
              type2: Type2::TextValue("now".into()),
              operator: None,
            },
          ],
          span: (7, 24, 1),
        }),
      )),
      Type2::Typename((
        Identifier {
          ident: "tcp-option".into(),
          socket: Some(SocketPlug::GROUP),
          span: (0, 12, 1),
        },
        None,
      )),
      Type2::Unwrap((
        Identifier {
          ident: "group1".into(),
          socket: None,
          span: (1, 7, 1),
        },
        None,
      )),
      Type2::TaggedData((
        Some(997),
        Type(vec![Type1 {
          type2: Type2::Typename((
            Identifier {
              ident: "tstr".into(),
              socket: None,
              span: (7, 11, 1),
            },
            None,
          )),
          operator: None,
        }]),
      )),
      Type2::FloatValue(9.9),
      Type2::Any,
      Type2::Array(Group(vec![GroupChoice(vec![(
        GroupEntry::TypeGroupname(TypeGroupnameEntry {
          occur: Some(Occur::Exact((None, Some(3)))),
          name: Identifier {
            ident: "reputon".into(),
            socket: None,
            span: (4, 11, 1),
          },
          generic_arg: None,
        }),
        false,
      )])])),
      Type2::Array(Group(vec![GroupChoice(vec![(
        GroupEntry::TypeGroupname(TypeGroupnameEntry {
          occur: Some(Occur::OneOrMore),
          name: Identifier {
            ident: "reputon".into(),
            socket: None,
            span: (3, 10, 1),
          },
          generic_arg: None,
        }),
        false,
      )])])),
      Type2::ChoiceFromGroup((
        Identifier {
          ident: "groupname".into(),
          socket: None,
          span: (1, 10, 1),
        },
        None,
      )),
      Type2::ChoiceFromInlineGroup(Group(vec![GroupChoice(vec![(
        GroupEntry::TypeGroupname(TypeGroupnameEntry {
          occur: None,
          name: Identifier {
            ident: "inlinegroup".into(),
            socket: None,
            span: (3, 14, 1),
          },
          generic_arg: None,
        }),
        false,
      )])])),
      Type2::Map(Group(vec![GroupChoice(vec![(
        GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
          occur: Some(Occur::Optional),
          member_key: Some(MemberKey::Type1(Box::from((
            Type1 {
              type2: Type2::TextValue("optional-key".into()),
              operator: None,
            },
            true,
          )))),
          entry_type: Type(vec![Type1 {
            type2: Type2::Typename((
              Identifier {
                ident: "int".into(),
                socket: None,
                span: (24, 27, 1),
              },
              None,
            )),
            operator: None,
          }]),
        })),
        true,
      )])])),
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(l)?;

      let t2 = p.parse_type2()?;

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
      Type2::Array(Group(vec![GroupChoice(vec![
        (
          GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
            occur: None,
            member_key: None,
            entry_type: Type(vec![Type1 {
              type2: Type2::Array(Group(vec![GroupChoice(vec![(
                GroupEntry::TypeGroupname(TypeGroupnameEntry {
                  occur: Some(Occur::ZeroOrMore),
                  name: Identifier {
                    ident: "file-entry".into(),
                    socket: None,
                    span: (5, 15, 1),
                  },
                  generic_arg: None,
                }),
                false,
              )])])),
              operator: None,
            }]),
          })),
          true,
        ),
        (
          GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
            occur: None,
            member_key: None,
            entry_type: Type(vec![Type1 {
              type2: Type2::Array(Group(vec![GroupChoice(vec![(
                GroupEntry::TypeGroupname(TypeGroupnameEntry {
                  occur: Some(Occur::ZeroOrMore),
                  name: Identifier {
                    ident: "directory-entry".into(),
                    socket: None,
                    span: (21, 36, 1),
                  },
                  generic_arg: None,
                }),
                false,
              )])])),
              operator: None,
            }]),
          })),
          false,
        ),
      ])])),
      Type2::Map(Group(vec![
        GroupChoice(vec![
          (
            GroupEntry::TypeGroupname(TypeGroupnameEntry {
              occur: None,
              name: Identifier {
                ident: "int".into(),
                socket: None,
                span: (2, 5, 1),
              },
              generic_arg: None,
            }),
            true,
          ),
          (
            GroupEntry::TypeGroupname(TypeGroupnameEntry {
              occur: None,
              name: Identifier {
                ident: "int".into(),
                socket: None,
                span: (7, 10, 1),
              },
              generic_arg: None,
            }),
            false,
          ),
        ]),
        GroupChoice(vec![
          (
            GroupEntry::TypeGroupname(TypeGroupnameEntry {
              occur: None,
              name: Identifier {
                ident: "int".into(),
                socket: None,
                span: (14, 17, 1),
              },
              generic_arg: None,
            }),
            true,
          ),
          (
            GroupEntry::TypeGroupname(TypeGroupnameEntry {
              occur: None,
              name: Identifier {
                ident: "tstr".into(),
                socket: None,
                span: (19, 23, 1),
              },
              generic_arg: None,
            }),
            false,
          ),
        ]),
      ])),
      Type2::Map(Group(vec![GroupChoice(vec![
        (
          GroupEntry::TypeGroupname(TypeGroupnameEntry {
            occur: None,
            name: Identifier {
              ident: "int".into(),
              socket: None,
              span: (2, 5, 1),
            },
            generic_arg: None,
          }),
          true,
        ),
        (
          GroupEntry::TypeGroupname(TypeGroupnameEntry {
            occur: None,
            name: Identifier {
              ident: "int".into(),
              socket: None,
              span: (7, 10, 1),
            },
            generic_arg: None,
          }),
          true,
        ),
        (
          GroupEntry::TypeGroupname(TypeGroupnameEntry {
            occur: None,
            name: Identifier {
              ident: "int".into(),
              socket: None,
              span: (12, 15, 1),
            },
            generic_arg: None,
          }),
          true,
        ),
        (
          GroupEntry::TypeGroupname(TypeGroupnameEntry {
            occur: None,
            name: Identifier {
              ident: "tstr".into(),
              socket: None,
              span: (17, 21, 1),
            },
            generic_arg: None,
          }),
          false,
        ),
      ])])),
    ];

    for (idx, expected_output) in expected_ouputs.iter().enumerate() {
      let l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(l)?;

      let t2 = p.parse_type2()?;

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
    ];

    let expected_outputs = [
      GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
        occur: Some(Occur::ZeroOrMore),
        member_key: Some(MemberKey::Type1(Box::from((
          Type1 {
            type2: Type2::Typename((
              Identifier {
                ident: "type1".into(),
                socket: None,
                span: (2, 7, 1),
              },
              None,
            )),
            operator: None,
          },
          true,
        )))),
        entry_type: Type(vec![Type1 {
          type2: Type2::TextValue("value".into()),
          operator: None,
        }]),
      })),
      GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
        occur: None,
        member_key: Some(MemberKey::Bareword(Identifier {
          ident: "type1".into(),
          socket: None,
          span: (0, 5, 1),
        })),
        entry_type: Type(vec![Type1 {
          type2: Type2::Typename((
            Identifier {
              ident: "type2".into(),
              socket: None,
              span: (7, 12, 1),
            },
            None,
          )),
          operator: None,
        }]),
      })),
      GroupEntry::TypeGroupname(TypeGroupnameEntry {
        occur: None,
        name: Identifier {
          ident: "typename".into(),
          socket: None,
          span: (0, 8, 1),
        },
        generic_arg: None,
      }),
      GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
        occur: Some(Occur::Optional),
        member_key: Some(MemberKey::Value(Value::UINT(0))),
        entry_type: Type(vec![Type1 {
          type2: Type2::Typename((
            Identifier {
              ident: "addrdistr".into(),
              socket: None,
              span: (5, 14, 1),
            },
            None,
          )),
          operator: None,
        }]),
      })),
      GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
        occur: None,
        member_key: Some(MemberKey::Value(Value::UINT(0))),
        entry_type: Type(vec![Type1 {
          type2: Type2::Typename((
            Identifier {
              ident: "finite_set".into(),
              socket: None,
              span: (3, 13, 1),
            },
            Some(GenericArg {
              args: vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "transaction_input".into(),
                    socket: None,
                    span: (14, 31, 1),
                  },
                  None,
                )),
                operator: None,
              }],
              span: (13, 32, 1),
            }),
          )),
          operator: None,
        }]),
      })),
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(l)?;

      let grpent = p.parse_grpent()?;

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
      MemberKey::Type1(Box::from((
        Type1 {
          type2: Type2::Typename((
            Identifier {
              ident: "type1".into(),
              socket: None,
              span: (0, 5, 1),
            },
            None,
          )),
          operator: None,
        },
        false,
      ))),
      MemberKey::Type1(Box::from((
        Type1 {
          type2: Type2::TextValue("mytype1".into()),
          operator: None,
        },
        true,
      ))),
      MemberKey::Bareword(Identifier {
        ident: "mybareword".into(),
        socket: None,
        span: (0, 10, 1),
      }),
      MemberKey::Bareword(Identifier {
        ident: "my..bareword".into(),
        socket: None,
        span: (0, 12, 1),
      }),
      MemberKey::Value(Value::TEXT("myvalue".into())),
      MemberKey::Value(Value::UINT(0)),
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(l)?;

      let mk = p.parse_memberkey(false)?;

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
      Occur::Exact((Some(1), Some(3))),
      Occur::ZeroOrMore,
      Occur::OneOrMore,
      Occur::Exact((Some(5), None)),
      Occur::Exact((None, Some(3))),
      Occur::Optional,
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(l)?;

      let o = p.parse_occur(false)?;

      if let Some(o) = o {
        assert_eq!(&o, expected_output);
        assert_eq!(o.to_string(), expected_output.to_string());
      }
    }

    Ok(())
  }
}
