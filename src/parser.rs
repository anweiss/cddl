use super::{
  ast::*,
  lexer::{Lexer, LexerError, Position},
  token::{self, Tag, Token, Value},
};
use annotate_snippets::{
  display_list::DisplayList,
  formatter::DisplayListFormatter,
  snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use regex;
use std::{fmt, mem, result};

#[cfg(feature = "std")]
use std::error::Error;

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::ToOwned,
  boxed::Box,
  string::{String, ToString},
  vec::Vec,
};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

/// Alias for `Result` with an error of type `cddl::ParserError`
pub type Result<T> = result::Result<T, ParserError>;

/// Parser type
pub struct Parser<'a> {
  l: Lexer<'a>,
  cur_token: Token<'a>,
  peek_token: Token<'a>,
  lexer_position: Position,
  peek_lexer_position: Position,
  parser_position: Position,
  errors: Vec<ParserError>,
}

/// Parsing error types
#[derive(Debug)]
pub enum ParserError {
  /// Parsing error
  PARSER((Vec<u8>, Position, String)),
  /// Lexing error
  LEXER(LexerError),
  /// Regex error
  REGEX(regex::Error),
}

impl fmt::Display for ParserError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ParserError::PARSER((input, position, error)) => {
        let dlf = DisplayListFormatter::new(false, false);

        write!(
          f,
          "{}",
          dlf.format(&DisplayList::from(Snippet {
            title: Some(Annotation {
              label: Some(error.to_string()),
              id: None,
              annotation_type: AnnotationType::Error,
            }),
            footer: vec![],
            slices: vec![Slice {
              source: std::str::from_utf8(input)
                .map_err(|_| fmt::Error)?
                .to_string(),
              line_start: position.line,
              origin: Some("input".to_string()),
              fold: false,
              annotations: vec![SourceAnnotation {
                range: position.range,
                label: error.to_string(),
                annotation_type: AnnotationType::Error,
              }],
            }],
          }))
        )
      }
      ParserError::LEXER(e) => write!(f, "{}", e),
      ParserError::REGEX(e) => write!(f, "{}", e),
    }
  }
}

#[cfg(feature = "std")]
impl Error for ParserError {
  fn source(&self) -> Option<&(dyn Error + 'static)> {
    match self {
      ParserError::LEXER(le) => Some(le),
      ParserError::REGEX(re) => Some(re),
      _ => None,
    }
  }
}

impl<'a> From<(&'a [u8], Position, String)> for ParserError {
  fn from(e: (&'a [u8], Position, String)) -> Self {
    ParserError::PARSER((e.0.to_owned(), e.1, e.2))
  }
}

impl<'a> From<(&'a [u8], Position, &'static str)> for ParserError {
  fn from(e: (&'a [u8], Position, &'static str)) -> Self {
    ParserError::PARSER((e.0.to_owned(), e.1, e.2.to_string()))
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

  fn next_token(&mut self) -> Result<()> {
    mem::swap(&mut self.cur_token, &mut self.peek_token);
    mem::swap(&mut self.lexer_position, &mut self.peek_lexer_position);
    let nt = self.l.next_token().map_err(ParserError::LEXER)?;
    self.peek_lexer_position = nt.0;
    self.peek_token = nt.1;

    Ok(())
  }

  /// Parses into a `CDDL` AST
  pub fn parse_cddl(&mut self) -> Result<CDDL<'a>> {
    let mut c = CDDL::default();

    while self.cur_token != Token::EOF {
      let r = self.parse_rule()?;

      let rule_exists = |existing_rule: &&Rule| {
        r.name() == existing_rule.name() && !existing_rule.is_choice_alternate()
      };

      if let Some(r) = c.rules.iter().find(rule_exists) {
        self.parser_position.range = r.range();

        return Err(
          (
            self.l.str_input,
            self.parser_position,
            format!("Rule with name '{}' already defined", r.name()),
          )
            .into(),
        );
      }

      c.rules.push(r);
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

  fn parse_rule(&mut self) -> Result<Rule<'a>> {
    let begin_range = self.lexer_position.range.0;

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    let ident = match &self.cur_token {
      Token::IDENT(i) => *i,
      _ => {
        self.parser_position.range = (begin_range, self.lexer_position.range.0);
        self.parser_position.line = self.lexer_position.line;

        return Err(
          (
            self.l.str_input,
            self.parser_position,
            format!("expected rule identifier. Got '{}'", self.cur_token),
          )
            .into(),
        );
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
      self.parser_position.range = (begin_range, self.lexer_position.range.1);
      self.parser_position.line = self.lexer_position.line;

      return Err(
        (
          self.l.str_input,
          self.parser_position,
          format!(
            "Expected assignment '=' after identifier {}",
            self.cur_token
          ),
        )
          .into(),
      );
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
          let end_range = self.lexer_position.range.1 + 1;

          let ge = self.parse_grpent()?;

          Ok(Rule::Group(Box::from(GroupRule {
            name: ident.into(),
            generic_param: gp,
            is_group_choice_alternate,
            entry: ge,
            range: (begin_range, end_range),
          })))
        } else {
          let end_range = self.lexer_position.range.1 + 1;

          let t = self.parse_type(None)?;

          Ok(Rule::Type(TypeRule {
            name: ident.into(),
            generic_param: gp,
            is_type_choice_alternate,
            value: t,
            range: (begin_range, end_range),
          }))
        }
      }
      Token::LPAREN | Token::ASTERISK | Token::ONEORMORE | Token::OPTIONAL => {
        let end_range = self.lexer_position.range.1 + 1;
        let ge = self.parse_grpent()?;

        // If a group entry is an inline group with no leading occurrence
        // indicator, and its group has only a single element that is not
        // preceded by an occurrence indicator nor member key, treat it as a
        // parenthesized type, subsequently parsing the reamining type and
        // returning the type rule. This is the only situation where `clone` is
        // required
        if let GroupEntry::InlineGroup((occur, g)) = &ge {
          if occur.is_none() && g.0.len() == 1 {
            if let Some(gc) = g.0.get(0) {
              if gc.0.len() == 1 {
                if let Some(ge) = gc.0.get(0) {
                  if !ge.1 {
                    if let GroupEntry::ValueMemberKey(vmke) = &ge.0 {
                      if vmke.occur.is_none() && vmke.member_key.is_none() {
                        return Ok(Rule::Type(TypeRule {
                          name: ident.into(),
                          generic_param: gp,
                          is_type_choice_alternate,
                          value: self
                            .parse_type(Some(Type2::ParenthesizedType(vmke.entry_type.clone())))?,
                          range: (begin_range, end_range),
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
          name: ident.into(),
          generic_param: gp,
          is_group_choice_alternate,
          entry: ge,
          range: (begin_range, end_range),
        })))
      }
      _ => {
        let end_range = self.lexer_position.range.1 + 1;

        let t = self.parse_type(None)?;

        Ok(Rule::Type(TypeRule {
          name: ident.into(),
          generic_param: gp,
          is_type_choice_alternate,
          value: t,
          range: (begin_range, end_range),
        }))
      }
    }
  }

  fn parse_genericparm(&mut self) -> Result<GenericParm<'a>> {
    if self.cur_token_is(Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    let mut generic_params = GenericParm(Vec::new());

    let begin_range = self.lexer_position.range.0;

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      match &self.cur_token {
        Token::IDENT(i) => {
          generic_params.0.push((*i).into());
          self.next_token()?;

          if !self.cur_token_is(Token::COMMA) && !self.cur_token_is(Token::RANGLEBRACKET) {
            self.parser_position.range = (begin_range, self.lexer_position.range.0 + 1);
            self.parser_position.line = self.lexer_position.line;
            return Err(
              (
                self.l.str_input,
                self.parser_position,
                "Expecting comma between generic parameters or closing right angle bracket",
              )
                .into(),
            );
          }
        }
        Token::COMMA => self.next_token()?,
        Token::COMMENT(_) => self.next_token()?,
        _ => {
          self.parser_position.range = (begin_range, self.lexer_position.range.0 + 1);
          self.parser_position.line = self.lexer_position.line;
          return Err((self.l.str_input, self.parser_position, "Illegal token").into());
        }
      }
    }

    // Since generic params are only found after the identifier of a rule, don't
    // advance beyond the closing '>' to retain the expect_peek semantics for
    // '=', '/=' and '//='

    Ok(generic_params)
  }

  fn parse_genericarg(&mut self) -> Result<GenericArg<'a>> {
    self.next_token()?;

    // Required for type2 mutual recursion
    if self.cur_token_is(Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    let mut generic_args = GenericArg(Vec::new());

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      generic_args.0.push(self.parse_type1(None)?);
      if let Token::COMMA | Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }
    }

    if self.cur_token_is(Token::RANGLEBRACKET) {
      self.next_token()?;
    }

    Ok(generic_args)
  }

  fn parse_type(&mut self, parenthesized_type: Option<Type2<'a>>) -> Result<Type<'a>> {
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

  fn parse_type1(&mut self, parenthesized_type: Option<Type2<'a>>) -> Result<Type1<'a>> {
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

  fn parse_type2(&mut self) -> Result<Type2<'a>> {
    let t2 = match &self.cur_token {
      // value
      Token::VALUE(value) => Ok((*value).into()),
      Token::BYTESLICEVALUE(value) => Ok(value.into()),
      Token::BYTEVECVALUE(value) => Ok(value.clone().into()),

      // typename [genericarg]
      Token::IDENT(ident) => {
        // optional genericarg detected
        if self.peek_token_is(&Token::LANGLEBRACKET) {
          return Ok(Type2::Typename((
            (*ident).into(),
            Some(self.parse_genericarg()?),
          )));
        }

        Ok(Type2::Typename(((*ident).into(), None)))
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

        if let Token::IDENT(ident) = self.cur_token {
          if self.peek_token_is(&Token::LANGLEBRACKET) {
            self.next_token()?;

            return Ok(Type2::Unwrap((
              ident.into(),
              Some(self.parse_genericarg()?),
            )));
          }

          return Ok(Type2::Unwrap((ident.into(), None)));
        }

        Err((self.l.str_input, self.parser_position, "Invalid unwrap").into())
      }

      // & ( group )
      // & groupname [genericarg]
      Token::GTOCHOICE => {
        self.next_token()?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        match self.cur_token {
          Token::LPAREN => {
            self.next_token()?;

            Ok(Type2::ChoiceFromInlineGroup(self.parse_group()?))
          }
          Token::IDENT(ident) => {
            if self.peek_token_is(&Token::LANGLEBRACKET) {
              self.next_token()?;

              return Ok(Type2::ChoiceFromGroup((
                ident.into(),
                Some(self.parse_genericarg()?),
              )));
            }

            Ok(Type2::ChoiceFromGroup((ident.into(), None)))
          }
          _ => Err(
            (
              self.l.str_input,
              self.parser_position,
              "Invalid group to choice enumeration syntax",
            )
              .into(),
          ),
        }
      }

      // # 6 ["." uint] ( type )
      // # DIGIT ["." uint]   ; major/ai
      // #                    ; any
      Token::TAG(tag) => match tag {
        Tag::DATA(data) => Ok(Type2::TaggedData(*data)),
        Tag::MAJORTYPE(mt) => Ok(Type2::TaggedDataMajorType(*mt)),
        Tag::ANY => Ok(Type2::Any),
      },

      _ => {
        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        match self.cur_token.in_standard_prelude() {
          Some(s) => Ok(Type2::Typename(((s, None).into(), None))),
          None => Err(
            (
              self.l.str_input,
              self.parser_position,
              format!(
                "Unknown type2 alternative. Unknown token: {:#?}",
                self.cur_token
              ),
            )
              .into(),
          ),
        }
      }
    };

    self.next_token()?;

    t2
  }

  fn parse_group(&mut self) -> Result<Group<'a>> {
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

  fn parse_grpchoice(&mut self) -> Result<GroupChoice<'a>> {
    let mut grpchoice = GroupChoice(Vec::new());

    if self.cur_token_is(Token::LBRACE)
      || self.cur_token_is(Token::LPAREN)
      || self.cur_token_is(Token::LBRACKET)
    {
      self.next_token()?;
    }

    while !self.cur_token_is(Token::RBRACE)
      && !self.cur_token_is(Token::RPAREN)
      && !self.cur_token_is(Token::RBRACKET)
      && !self.cur_token_is(Token::EOF)
    {
      let ge = self.parse_grpent()?;

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

  fn parse_grpent(&mut self) -> Result<GroupEntry<'a>> {
    let occur = self.parse_occur(true)?;

    if occur.is_some() {
      while let Token::VALUE(Value::UINT(_))
      | Token::ASTERISK
      | Token::OPTIONAL
      | Token::ONEORMORE = self.cur_token
      {
        self.next_token()?;
      }
    }

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    let member_key = self.parse_memberkey(true)?;

    if member_key.is_some() {
      // Two member keys in a row indicates a malformed entry
      if let Some(mk) = self.parse_memberkey(true)? {
        return Err(
          (
            self.l.str_input,
            self.parser_position,
            format!(
              "Incomplete group entry for memberkey {}. Missing entry type",
              mk
            ),
          )
            .into(),
        );
      }
    }

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    if self.cur_token_is(Token::LPAREN) {
      self.next_token()?;

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
        self.next_token()?;
      }

      return Ok(ge);
    }

    let ga = if self.peek_token_is(&Token::LANGLEBRACKET) {
      Some(self.parse_genericarg()?)
    } else {
      None
    };

    match &self.cur_token {
      Token::IDENT(ident) => {
        // [occur S] [memberkey S] type
        if member_key.is_some() {
          return Ok(GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
            occur,
            member_key,
            entry_type: self.parse_type(None)?,
          })));
        }

        // Check for type choices in a group entry
        if self.peek_token_is(&Token::TCHOICE) {
          return Ok(GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
            occur,
            member_key,
            entry_type: self.parse_type(None)?,
          })));
        }

        // Otherwise it could be either typename or groupname. Requires context.
        // [occur S] [memberkey S] type
        // [occur S] groupname [genericarg]  ; preempted by above
        Ok(GroupEntry::TypeGroupname(TypeGroupnameEntry {
          occur,
          name: (*ident).into(),
          generic_arg: ga,
        }))
      }
      _ => Ok(GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
        occur,
        member_key,
        entry_type: self.parse_type(None)?,
      }))),
    }
  }

  fn parse_memberkey(&mut self, is_optional: bool) -> Result<Option<MemberKey<'a>>> {
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
          Token::IDENT(ident) => Some(MemberKey::Bareword((*ident).into())),
          Token::VALUE(value) => match value {
            v => Some(MemberKey::Value(*v)),
          },
          _ => {
            return Err(
              (
                self.l.str_input,
                self.parser_position,
                "Malformed memberkey",
              )
                .into(),
            )
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
          return Err(
            (
              self.l.str_input,
              self.lexer_position,
              "Malformed memberkey. Missing \":\" or \"=>\"",
            )
              .into(),
          );
        }

        Ok(None)
      }
    }
  }

  fn parse_occur(&mut self, is_optional: bool) -> Result<Option<Occur>> {
    match &self.cur_token {
      Token::OPTIONAL => Ok(Some(Occur::Optional)),
      Token::ONEORMORE => Ok(Some(Occur::OneOrMore)),
      Token::ASTERISK => {
        if let Token::VALUE(value) = &self.peek_token {
          if let Value::UINT(u) = value {
            return Ok(Some(Occur::Exact((None, Some(*u)))));
          }
        }

        Ok(Some(Occur::ZeroOrMore))
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

          return Err(
            (
              self.l.str_input,
              self.lexer_position,
              "Malformed occurrence syntax",
            )
              .into(),
          );
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
    self.errors.push(
      (
        self.l.str_input,
        self.lexer_position,
        format!(
          "expected next token to be {:?}, got {:?} instead",
          t, self.peek_token
        ),
      )
        .into(),
    )
  }
}

/// Returns a `ast::CDDL` from a `&str`
pub fn cddl_from_str<'a>(input: &'a str) -> Result<CDDL<'a>> {
  Parser::new(Lexer::new(input))?.parse_cddl()
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
    super::{ast, lexer::Lexer, token::SocketPlug, token::Tag},
    *,
  };

  #[test]
  fn verify_rule() -> Result<()> {
    let input = r#"myrule = secondrule
myrange = 10..upper
upper = 500 / 600
gr = 2* ( test )
messages = message<"reboot", "now">
message<t, v> = {type: 2, value: v}"#;

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    let cddl = p.parse_cddl()?;
    check_parser_errors(&p)?;

    assert!(cddl.rules.len() == 6);

    let expected_outputs = [
      Rule::Type(TypeRule {
        name: Identifier(("myrule", None)),
        generic_param: None,
        is_type_choice_alternate: false,
        value: Type(vec![Type1 {
          type2: Type2::Typename((Identifier(("secondrule", None)), None)),
          operator: None,
        }]),
        range: (0, 0),
      }),
      Rule::Type(TypeRule {
        name: Identifier(("myrange", None)),
        generic_param: None,
        is_type_choice_alternate: false,
        value: Type(vec![Type1 {
          type2: Type2::UintValue(10),
          operator: Some((
            RangeCtlOp::RangeOp(true),
            Type2::Typename((Identifier(("upper", None)), None)),
          )),
        }]),
        range: (0, 0),
      }),
      Rule::Type(TypeRule {
        name: Identifier(("upper", None)),
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
        range: (0, 0),
      }),
      Rule::Group(Box::from(GroupRule {
        name: Identifier(("gr", None)),
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          Some(Occur::Exact((Some(2), None))),
          Group(vec![GroupChoice(vec![(
            GroupEntry::TypeGroupname(TypeGroupnameEntry {
              occur: None,
              name: Identifier(("test", None)),
              generic_arg: None,
            }),
            false,
          )])]),
        )),
        range: (0, 0),
      })),
      Rule::Type(TypeRule {
        name: Identifier(("messages", None)),
        generic_param: None,
        is_type_choice_alternate: false,
        value: Type(vec![Type1 {
          type2: Type2::Typename((
            Identifier(("message", None)),
            Some(GenericArg(vec![
              Type1 {
                type2: Type2::TextValue("reboot"),
                operator: None,
              },
              Type1 {
                type2: Type2::TextValue("now"),
                operator: None,
              },
            ])),
          )),
          operator: None,
        }]),
        range: (0, 0),
      }),
      Rule::Type(TypeRule {
        name: Identifier(("message", None)),
        generic_param: Some(GenericParm(vec![
          Identifier(("t", None)),
          Identifier(("v", None)),
        ])),
        is_type_choice_alternate: false,
        value: Type(vec![Type1 {
          type2: Type2::Map(Group(vec![GroupChoice(vec![
            (
              GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
                occur: None,
                member_key: Some(MemberKey::Bareword(Identifier(("type", None)))),
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
                member_key: Some(MemberKey::Bareword(Identifier(("value", None)))),
                entry_type: Type(vec![Type1 {
                  type2: Type2::Typename((Identifier(("v", None)), None)),
                  operator: None,
                }]),
              })),
              false,
            ),
          ])])),
          operator: None,
        }]),
        range: (0, 0),
      }),
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      assert_eq!(cddl.rules[idx].to_string(), expected_output.to_string());
    }

    Ok(())
  }
  #[test]
  fn verify_rule_diagnostic() -> Result<()> {
    let input = r#"a = 1234

  a = b"#;

    match compile_cddl_from_str(input) {
      Ok(()) => Ok(()),
      Err(e) => {
        assert_eq!(
          e.to_string(),
          r#"error: Rule with name 'a' already defined
 --> input:1:0
  |
1 | a = 1234
  | ^^^^^^^^ Rule with name 'a' already defined
2 | 
3 |   a = b
  |"#
        );

        Ok(())
      }
    }
  }

  #[test]
  fn verify_genericparm() -> Result<()> {
    let input = r#"<t, v>"#;

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    let gps = p.parse_genericparm()?;
    check_parser_errors(&p)?;

    assert!(gps.0.len() == 2);

    let expected_generic_params = ["t", "v"];

    for (idx, expected_generic_param) in expected_generic_params.iter().enumerate() {
      assert_eq!(&gps.0[idx].to_string(), expected_generic_param);
    }

    Ok(())
  }

  #[test]
  fn verify_genericparm_diagnostic() -> Result<()> {
    let input = r#"<1, 2>"#;

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    match p.parse_genericparm() {
      Ok(_) => Ok(()),
      Err(e) => {
        assert_eq!(
          e.to_string(),
          r#"error: Illegal token
 --> input:1:1
  |
1 | <1, 2>
  |  ^ Illegal token
  |"#
        );

        Ok(())
      }
    }
  }

  #[test]
  fn verify_genericarg() -> Result<()> {
    let input = r#"<"reboot", "now">"#;

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    let generic_args = p.parse_genericarg()?;
    check_parser_errors(&p)?;

    assert!(generic_args.0.len() == 2);

    let expected_generic_args = ["\"reboot\"", "\"now\""];

    for (idx, expected_generic_arg) in expected_generic_args.iter().enumerate() {
      assert_eq!(&generic_args.0[idx].to_string(), expected_generic_arg);
    }

    Ok(())
  }

  #[test]
  fn verify_type() -> Result<()> {
    let input = r#"tchoice1 / tchoice2"#;

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    let t = p.parse_type(None)?;
    check_parser_errors(&p)?;

    assert!(t.0.len() == 2);

    let expected_t1_identifiers = ["tchoice1", "tchoice2"];

    for (idx, expected_t1_identifier) in expected_t1_identifiers.iter().enumerate() {
      assert_eq!(&t.0[idx].to_string(), expected_t1_identifier);
    }

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
        type2: Type2::Typename((Identifier(("my..lower", None)), None)),
        operator: Some((
          RangeCtlOp::RangeOp(false),
          Type2::Typename((Identifier(("upper", None)), None)),
        )),
      },
      Type1 {
        type2: Type2::Typename((Identifier(("target", None)), None)),
        operator: Some((
          RangeCtlOp::CtlOp(".lt"),
          Type2::Typename((Identifier(("controller", None)), None)),
        )),
      },
      Type1 {
        type2: Type2::ParenthesizedType(Type(vec![
          Type1 {
            type2: Type2::Typename((Identifier(("text", None)), None)),
            operator: None,
          },
          Type1 {
            type2: Type2::Typename((Identifier(("tstr", None)), None)),
            operator: None,
          },
        ])),
        operator: Some((RangeCtlOp::CtlOp(".eq"), Type2::TextValue("hello"))),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(l)?;

      let t1 = p.parse_type1(None)?;
      check_parser_errors(&p)?;

      assert_eq!(expected_output.to_string(), t1.to_string());
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
      r#"[ [* file-entry], [* directory-entry ] ]"#,
    ];

    let expected_outputs = [
      Type2::TextValue("myvalue"),
      Type2::Typename((
        Identifier(("message", None)),
        Some(GenericArg(vec![
          Type1 {
            type2: Type2::TextValue("reboot"),
            operator: None,
          },
          Type1 {
            type2: Type2::TextValue("now"),
            operator: None,
          },
        ])),
      )),
      Type2::Typename((Identifier(("tcp-option", Some(&SocketPlug::GROUP))), None)),
      Type2::Unwrap((Identifier(("group1", None)), None)),
      Type2::TaggedData((Some(997), "tstr")),
      Type2::TaggedDataMajorType((9, Some(9))),
      Type2::Any,
      Type2::Array(Group(vec![GroupChoice(vec![(
        GroupEntry::TypeGroupname(TypeGroupnameEntry {
          occur: Some(Occur::Exact((None, Some(3)))),
          name: Identifier(("reputon", None)),
          generic_arg: None,
        }),
        false,
      )])])),
      Type2::Array(Group(vec![GroupChoice(vec![(
        GroupEntry::TypeGroupname(TypeGroupnameEntry {
          occur: Some(Occur::OneOrMore),
          name: Identifier(("reputon", None)),
          generic_arg: None,
        }),
        false,
      )])])),
      Type2::ChoiceFromGroup((Identifier(("groupname", None)), None)),
      Type2::ChoiceFromInlineGroup(Group(vec![GroupChoice(vec![(
        GroupEntry::TypeGroupname(TypeGroupnameEntry {
          occur: None,
          name: Identifier(("inlinegroup", None)),
          generic_arg: None,
        }),
        false,
      )])])),
      Type2::Map(Group(vec![GroupChoice(vec![(
        GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
          occur: Some(Occur::Optional),
          member_key: Some(MemberKey::Type1(Box::from((
            Type1 {
              type2: Type2::TextValue("optional-key"),
              operator: None,
            },
            true,
          )))),
          entry_type: Type(vec![Type1 {
            type2: Type2::Typename((Identifier(("int", None)), None)),
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
      check_parser_errors(&p)?;

      assert_eq!(t2.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_type2_complex() -> Result<()> {
    let inputs = [r#"[ [* file-entry], [* directory-entry] ]"#];

    let expected_ouputs = [Type2::Array(Group(vec![GroupChoice(vec![
      (
        GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
          occur: None,
          member_key: None,
          entry_type: Type(vec![Type1 {
            type2: Type2::Array(Group(vec![GroupChoice(vec![(
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: Some(Occur::ZeroOrMore),
                name: Identifier(("file-entry", None)),
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
                name: Identifier(("directory-entry", None)),
                generic_arg: None,
              }),
              false,
            )])])),
            operator: None,
          }]),
        })),
        false,
      ),
    ])]))];

    for (idx, expected_output) in expected_ouputs.iter().enumerate() {
      let l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(l)?;

      let t2 = p.parse_type2()?;
      check_parser_errors(&p)?;

      assert_eq!(t2.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_grpent() -> Result<()> {
    let inputs = [r#"* type1 ^ => "value""#, r#"type1: type2"#, r#"typename"#];

    let expected_outputs = [
      GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
        occur: Some(Occur::ZeroOrMore),
        member_key: Some(MemberKey::Type1(Box::from((
          Type1 {
            type2: Type2::Typename((Identifier(("type1", None)), None)),
            operator: None,
          },
          true,
        )))),
        entry_type: Type(vec![Type1 {
          type2: Type2::TextValue("value"),
          operator: None,
        }]),
      })),
      GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
        occur: None,
        member_key: Some(MemberKey::Bareword(Identifier(("type1", None)))),
        entry_type: Type(vec![Type1 {
          type2: Type2::Typename((Identifier(("type2", None)), None)),
          operator: None,
        }]),
      })),
      GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
        occur: None,
        member_key: None,
        entry_type: Type(vec![Type1 {
          type2: Type2::Typename((Identifier(("typename", None)), None)),
          operator: None,
        }]),
      })),
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(l)?;

      let grpent = p.parse_grpent()?;
      check_parser_errors(&p)?;

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
    ];

    let expected_outputs = [
      MemberKey::Type1(Box::from((
        Type1 {
          type2: Type2::Typename((Identifier(("type1", None)), None)),
          operator: None,
        },
        false,
      ))),
      MemberKey::Type1(Box::from((
        Type1 {
          type2: Type2::TextValue("mytype1"),
          operator: None,
        },
        true,
      ))),
      MemberKey::Bareword(Identifier(("mybareword", None))),
      MemberKey::Bareword(Identifier(("my..bareword", None))),
      MemberKey::Value(Value::TEXT("myvalue")),
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(l)?;

      let mk = p.parse_memberkey(false)?;
      check_parser_errors(&p)?;

      assert_eq!(mk.unwrap().to_string(), expected_output.to_string());
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
      check_parser_errors(&p)?;

      assert_eq!(o.unwrap().to_string(), expected_output.to_string());
    }

    Ok(())
  }

  fn check_parser_errors(p: &Parser) -> Result<()> {
    if p.errors.is_empty() {
      return Ok(());
    }

    let mut errors = String::new();

    for err in p.errors.iter() {
      errors.push_str(&format!("parser error: {}\n", err));
    }

    Err((p.l.str_input, p.lexer_position, errors).into())
  }
}
