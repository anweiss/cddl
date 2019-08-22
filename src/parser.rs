use super::{
  ast::*,
  lexer::{Lexer, LexerError, Position},
  token::{self, Tag, Token, Value},
};
use regex;
use std::{fmt, mem, result};

#[cfg(feature = "std")]
use std::error::Error;

#[cfg(not(feature = "std"))]
use alloc::{
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
  position: Position,
  errors: Vec<ParserError>,
}

/// Parsing error types
#[derive(Debug)]
pub enum ParserError {
  /// Parsing error
  PARSER((Position, String)),
  /// Lexing error
  LEXER(LexerError),
  /// Regex error
  REGEX(regex::Error),
}

impl fmt::Display for ParserError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ParserError::PARSER((position, error)) => write!(
        f,
        "Parser error at ln {}, col {}. {}",
        position.0, position.1, error
      ),
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

impl From<(Position, String)> for ParserError {
  fn from(e: (Position, String)) -> Self {
    ParserError::PARSER(e)
  }
}

impl From<(Position, &'static str)> for ParserError {
  fn from(e: (Position, &'static str)) -> Self {
    ParserError::PARSER((e.0, (e.1.to_string())))
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
      position: (1, 1, 0),
    };

    p.next_token()?;
    p.next_token()?;

    Ok(p)
  }

  fn next_token(&mut self) -> Result<()> {
    mem::swap(&mut self.cur_token, &mut self.peek_token);
    let nt = self.l.next_token().map_err(ParserError::LEXER)?;
    self.position = nt.0;
    self.peek_token = nt.1;

    Ok(())
  }

  /// Parses into a `CDDL` AST
  pub fn parse_cddl(&mut self) -> Result<CDDL<'a>> {
    let mut c = CDDL::default();

    while self.cur_token != Token::EOF {
      let r = self.parse_rule()?;

      if c
        .rules
        .iter()
        .any(|existing_rule| r.name() == existing_rule.name() && !r.is_choice_alternate())
      {
        return Err(
          (
            self.position,
            format!("Rule with name {} already defined", r.name()),
          )
            .into(),
        );
      }

      c.rules.push(r);
    }

    Ok(c)
  }

  fn parse_rule(&mut self) -> Result<Rule<'a>> {
    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    let ident = match &self.cur_token {
      Token::IDENT(i) => *i,
      _ => {
        return Err(
          (
            self.position,
            format!("expected IDENT. Got {}", self.cur_token),
          )
            .into(),
        )
      }
    };

    let gp = if self.peek_token_is(&Token::LANGLEBRACKET) {
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
      return Err((self.position, "Expected ASSIGN(=). Got {}").into());
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
      Token::LPAREN | Token::ASTERISK | Token::ONEORMORE | Token::OPTIONAL => {
        Ok(Rule::Group(Box::from(GroupRule {
          name: ident.into(),
          generic_param: gp,
          is_group_choice_alternate,
          entry: self.parse_grpent()?,
        })))
      }
      _ => {
        let r = Rule::Type(TypeRule {
          name: ident.into(),
          generic_param: gp,
          is_type_choice_alternate,
          value: self.parse_type()?,
        });

        match self.cur_token {
          // If last (or only) type choice is a rangeop, advance token
          Token::RANGE(_) => {
            self.next_token()?;

            Ok(r)
          }
          _ => Ok(r),
        }
      }
    }
  }

  fn parse_genericparm(&mut self) -> Result<GenericParm<'a>> {
    self.next_token()?;

    let mut generic_params = GenericParm(Vec::new());

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      match &self.cur_token {
        Token::IDENT(i) => {
          generic_params.0.push((*i).into());
          self.next_token()?;
        }
        Token::COMMA => self.next_token()?,
        Token::COMMENT(_) => self.next_token()?,
        _ => return Err((self.position, "Illegal token").into()),
      }
    }

    self.next_token()?;

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
      generic_args.0.push(self.parse_type1()?);
      if let Token::COMMA | Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }
    }

    self.next_token()?;

    Ok(generic_args)
  }

  fn parse_type(&mut self) -> Result<Type<'a>> {
    let mut t = Type(Vec::new());

    t.0.push(self.parse_type1()?);

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    while self.cur_token_is(Token::TCHOICE) {
      self.next_token()?;

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      t.0.push(self.parse_type1()?);
    }

    Ok(t)
  }

  fn parse_type1(&mut self) -> Result<Type1<'a>> {
    match &self.cur_token {
      Token::RANGE((lower, upper, inclusive)) => Ok(Type1 {
        type2: (*lower).into(),
        operator: Some((RangeCtlOp::RangeOp(*inclusive), (*upper).into())),
      }),

      _ => {
        let target = self.parse_type2()?;

        if let Some(ctrl) = token::control_str_from_token(&self.cur_token) {
          self.next_token()?;

          return Ok(Type1 {
            type2: target,
            operator: Some((RangeCtlOp::CtlOp(ctrl), self.parse_type2()?)),
          });
        }

        Ok(Type1 {
          type2: target,
          operator: None,
        })
      }
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
      // TODO: This is used to define precedence of a type expression, but not
      // quite sure how to implement it. At the moment, this is never matched.
      Token::LPAREN => {
        self.next_token()?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        Ok(Type2::ParenthesizedType(self.parse_type()?))
      }

      // { group }
      Token::LBRACE => {
        self.next_token()?;

        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        Ok(Type2::Map(self.parse_group()?))
      }

      // [ group ]
      Token::LBRACKET => {
        self.next_token()?;

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

        Err((self.position, "Invalid unwrap").into())
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
          _ => Err((self.position, "Invalid group to choice enumeration syntax").into()),
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
              self.position,
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

    while !self.cur_token_is(Token::RBRACE)
      && !self.cur_token_is(Token::RPAREN)
      && !self.cur_token_is(Token::RBRACKET)
      && !self.cur_token_is(Token::EOF)
    {
      grpchoice.0.push(self.parse_grpent()?);

      // FYI, this was a really difficult bug to fix. When the last entry in a
      // map, group or array is of the form "memberkey: type" without a trailing
      // comma, don't advance the token
      if !self.cur_token_is(Token::RPAREN)
        && !self.cur_token_is(Token::RBRACE)
        && !self.cur_token_is(Token::RBRACKET)
      {
        self.next_token()?;
      }

      if self.cur_token_is(Token::COMMA) {
        self.next_token()?;
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
      while let Token::VALUE(Value::UINT(_)) | Token::ASTERISK | Token::OPTIONAL | Token::ONEORMORE = self.cur_token {
        self.next_token()?;
      }
    }

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    let member_key = self.parse_memberkey(true)?;

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    if self.cur_token_is(Token::LPAREN) {
      self.next_token()?;

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      let ge = GroupEntry::InlineGroup((occur, self.parse_group()?));

      while let Token::COMMENT(_) = self.cur_token {
        self.next_token()?;
      }

      if self.cur_token_is(Token::RPAREN) {
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
            entry_type: self.parse_type()?,
          })));
        }

        // Check for type choices in a group entry
        if self.peek_token_is(&Token::TCHOICE) {
          return Ok(GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
            occur,
            member_key,
            entry_type: self.parse_type()?,
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
        entry_type: self.parse_type()?,
      }))),
    }
  }

  fn parse_memberkey(&mut self, is_optional: bool) -> Result<Option<MemberKey<'a>>> {
    match &self.peek_token {
      Token::ARROWMAP | Token::CUT => {
        let t1 = self.parse_type1()?;

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
          _ => return Err((self.position, "Malformed memberkey").into()),
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
              self.position,
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
      _ => {
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

          return Err((self.position, "Malformed occurrence syntax").into());
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
        self.position,
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
    
    upper = 500 / 600"#;

    let l = Lexer::new(input);
    let mut p = Parser::new(l)?;

    let cddl = p.parse_cddl()?;
    check_parser_errors(&p)?;

    assert!(cddl.rules.len() == 3);

    let expected_outputs = [
      Rule::Type(TypeRule {
        name: Identifier(("myrule", None)),
        generic_param: None,
        is_type_choice_alternate: false,
        value: Type(vec![Type1 {
          type2: Type2::Typename((Identifier(("secondrule", None)), None)),
          operator: None,
        }]),
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
      }),
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      assert_eq!(cddl.rules[idx].to_string(), expected_output.to_string());
    }

    Ok(())
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

    let t = p.parse_type()?;
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
          RangeCtlOp::CtlOp("lt"),
          Type2::Typename((Identifier(("controller", None)), None)),
        )),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(l)?;

      let t1 = p.parse_type1()?;
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
      Type2::Array(Group(vec![GroupChoice(vec![GroupEntry::TypeGroupname(
        TypeGroupnameEntry {
          occur: Some(Occur::Exact((None, Some(3)))),
          name: Identifier(("reputon", None)),
          generic_arg: None,
        },
      )])])),
      Type2::Array(Group(vec![GroupChoice(vec![GroupEntry::TypeGroupname(
        TypeGroupnameEntry {
          occur: Some(Occur::OneOrMore),
          name: Identifier(("reputon", None)),
          generic_arg: None,
        },
      )])])),
      Type2::ChoiceFromGroup((Identifier(("groupname", None)), None)),
      Type2::ChoiceFromInlineGroup(Group(vec![GroupChoice(vec![GroupEntry::TypeGroupname(
        TypeGroupnameEntry {
          occur: None,
          name: Identifier(("inlinegroup", None)),
          generic_arg: None,
        },
      )])])),
      Type2::Map(Group(vec![GroupChoice(vec![GroupEntry::ValueMemberKey(
        Box::from(ValueMemberKeyEntry {
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
        }),
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

    Err((p.position, errors).into())
  }
}
