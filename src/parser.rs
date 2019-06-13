use super::ast::*;
use super::lexer::Lexer;
use super::token::{RangeValue, Token, Value};
use std::{error::Error, mem};

pub struct Parser<'a> {
  l: &'a mut Lexer<'a>,
  cur_token: Token<'a>,
  peek_token: Token<'a>,
  errors: Vec<Box<Error>>,
}

impl<'a> Parser<'a> {
  pub fn new(l: &'a mut Lexer<'a>) -> Result<Parser, Box<Error>> {
    let mut p = Parser {
      l,
      cur_token: Token::EOF,
      peek_token: Token::EOF,
      errors: Vec::default(),
    };

    p.next_token()?;
    p.next_token()?;

    Ok(p)
  }

  fn next_token(&mut self) -> Result<(), Box<Error>> {
    mem::swap(&mut self.cur_token, &mut self.peek_token);
    self.peek_token = self.l.next_token()?;
    Ok(())
  }

  pub fn parse_cddl(&mut self) -> Result<CDDL<'a>, Box<Error>> {
    let mut c = CDDL::default();

    while self.cur_token != Token::EOF {
      c.rules.push(self.parse_rule()?);
    }

    Ok(c)
  }

  fn parse_rule(&mut self) -> Result<Rule<'a>, Box<Error>> {
    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    let ident = match &self.cur_token {
      Token::IDENT(i) => *i,
      _ => return Err(format!("expected IDENT. Got {:#?}", self.cur_token).into()),
    };

    let gp = if self.peek_token_is(&Token::LANGLEBRACKET) {
      Some(self.parse_genericparm()?)
    } else {
      None
    };

    while let Token::COMMENT(_) = self.cur_token {
      self.next_token()?;
    }

    if !self.expect_peek(&Token::ASSIGN)
      && !self.expect_peek(&Token::TCHOICEALT)
      && !self.expect_peek(&Token::GCHOICEALT)
    {
      return Err("Expected ASSIGN".into());
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

    if self.cur_token_is(Token::LPAREN) {
      return Ok(Rule::Group(Box::from(GroupRule {
        name: Identifier(ident),
        generic_param: gp,
        is_group_choice_alternate,
        entry: self.parse_grpent()?,
      })));
    }

    Ok(Rule::Type(TypeRule {
      name: Identifier(ident),
      generic_param: gp,
      is_type_choice_alternate,
      value: self.parse_type()?,
    }))
  }

  fn parse_genericparm(&mut self) -> Result<GenericParm<'a>, Box<Error>> {
    self.next_token()?;

    let mut generic_params = GenericParm(Vec::new());

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      match &self.cur_token {
        Token::IDENT(i) => {
          generic_params.0.push(Identifier(*i));
          self.next_token()?;
        }
        Token::COMMA => self.next_token()?,
        Token::COMMENT(_) => self.next_token()?,
        _ => return Err("Illegal token".into()),
      }
    }

    self.next_token()?;

    Ok(generic_params)
  }

  fn parse_genericarg(&mut self) -> Result<GenericArg<'a>, Box<Error>> {
    self.next_token()?;

    // Required for type2 mutual recursion
    if self.cur_token_is(Token::LANGLEBRACKET) {
      self.next_token()?;
    }

    let mut generic_args = GenericArg(Vec::new());

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      generic_args.0.push(self.parse_type1()?);
      if self.cur_token_is(Token::COMMA) || self.cur_token_is(Token::COMMENT("")) {
        self.next_token()?;
      }
    }

    self.next_token()?;

    Ok(generic_args)
  }

  fn parse_type(&mut self) -> Result<Type<'a>, Box<Error>> {
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

  fn parse_type1(&mut self) -> Result<Type1<'a>, Box<Error>> {
    match &self.cur_token {
      Token::RANGE((lower, upper, inclusive)) => {
        let (lower_ident, upper_ident) = if let RangeValue::IDENT(li) = lower {
          if let RangeValue::IDENT(ui) = upper {
            (Some(Token::IDENT(*li)), Some(Token::IDENT(*ui)))
          } else {
            (Some(Token::IDENT(*li)), None)
          }
        } else if let RangeValue::IDENT(ui) = upper {
          (None, Some(Token::IDENT(*ui)))
        } else {
          (None, None)
        };

        if let Some(Token::IDENT(li)) = lower_ident {
          let mut t1 = Type1 {
            type2: Type2::Typename((Identifier(li), None)),
            operator: None,
          };

          if let Some(Token::IDENT(ui)) = upper_ident {
            t1.operator = Some((
              RangeCtlOp::RangeOp(*inclusive),
              Type2::Typename((Identifier(ui), None)),
            ));
          } else {
            t1.operator = Some((
              RangeCtlOp::RangeOp(*inclusive),
              Type2::Value(
                upper
                  .as_value()
                  .ok_or_else(|| "Illegal upper range value")?,
              ),
            ));
          }

          Ok(t1)
        } else {
          let mut t1 = Type1 {
            type2: Type2::Value(
              lower
                .as_value()
                .ok_or_else(|| "Illegal lower range value")?,
            ),
            operator: None,
          };

          if let Some(Token::IDENT(ui)) = upper_ident {
            t1.operator = Some((
              RangeCtlOp::RangeOp(*inclusive),
              Type2::Typename((Identifier(ui), None)),
            ));
          } else {
            t1.operator = Some((
              RangeCtlOp::RangeOp(*inclusive),
              Type2::Value(
                upper
                  .as_value()
                  .ok_or_else(|| "Illegal upper range value")?,
              ),
            ));
          }

          Ok(t1)
        }
      }
      _ => Ok(Type1 {
        type2: self.parse_type2()?,
        operator: None,
      }),
    }
  }

  fn parse_type2(&mut self) -> Result<Type2<'a>, Box<Error>> {
    let t2 = match &self.cur_token {
      // value
      Token::VALUE(value) => {
        match *value {
          // TODO: fix workaround for double escaping string literal values
          Value::TEXT(_) => Ok(Type2::Value(*value)),
          _ => Err("bad value".into()),
        }
      }

      // TODO: return Value type from lexer instead of these tokens. Duplicate
      // code
      Token::INTLITERAL(il) => Ok(Type2::Value(Value::INT(*il))),
      Token::FLOATLITERAL(fl) => Ok(Type2::Value(Value::FLOAT(*fl))),

      // typename [genericarg]
      Token::IDENT(ident) => {
        // optional genericarg detected
        if self.peek_token_is(&Token::LANGLEBRACKET) {
          return Ok(Type2::Typename((
            Identifier(*ident),
            Some(self.parse_genericarg()?),
          )));
        }

        Ok(Type2::Typename((Identifier(*ident), None)))
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
          self.next_token()?;

          if self.cur_token_is(Token::LANGLEBRACKET) {
            return Ok(Type2::Unwrap((Identifier(ident), Some(self.parse_genericarg()?))));
          }

          return Ok(Type2::Unwrap((Identifier(ident), None)));
        }
        
        Err("Invalid unwrap".into())
      }

      // & ( group )
      // & groupname [genericarg]
      Token::GTOCHOICE => unimplemented!(),

      // # 6 ["." uint] ( type )
      // # DIGIT ["." uint]   ; major/ai
      // #                    ; any
      Token::TAG(_) => unimplemented!(),

      _ => {
        while let Token::COMMENT(_) = self.cur_token {
          self.next_token()?;
        }

        if let Some(s) = self.cur_token.in_standard_prelude() {
          Ok(Type2::Typename((Identifier((s, None)), None)))
        } else {
          println!("Unknown token: {:#?}", self.cur_token);
          Err("Unknown type2 alternative".into())
        }
      }
    };

    self.next_token()?;

    t2
  }

  fn parse_group(&mut self) -> Result<Group<'a>, Box<Error>> {
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

  fn parse_grpchoice(&mut self) -> Result<GroupChoice<'a>, Box<Error>> {
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

  fn parse_grpent(&mut self) -> Result<GroupEntry<'a>, Box<Error>> {
    let occur = self.parse_occur(true)?;

    if occur.is_some() {
      self.next_token()?;
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
          name: Identifier(*ident),
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

  fn parse_memberkey(&mut self, is_optional: bool) -> Result<Option<MemberKey<'a>>, Box<Error>> {
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
          Token::IDENT(ident) => Some(MemberKey::Bareword(Identifier(*ident))),
          Token::VALUE(value) => Some(MemberKey::Value(*value)),
          _ => return Err("Malformed memberkey".into()),
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
          return Err("Malformed memberkey. Missing \":\" or \"=>\"".into());
        }

        Ok(None)
      }
    }
  }

  fn parse_occur(&mut self, is_optional: bool) -> Result<Option<Occur>, Box<Error>> {
    match &self.cur_token {
      Token::OPTIONAL => Ok(Some(Occur::Optional)),
      Token::ONEORMORE => Ok(Some(Occur::OneOrMore)),
      Token::ASTERISK => {
        if let Token::INTLITERAL(u) = &self.peek_token {
          return Ok(Some(Occur::Exact((None, Some(*u)))));
        }

        Ok(Some(Occur::ZeroOrMore))
      }
      _ => {
        let lower = if let Token::INTLITERAL(li) = &self.cur_token {
          Some(*li)
        } else {
          None
        };

        if !self.peek_token_is(&Token::ASTERISK) {
          if is_optional {
            return Ok(None);
          }

          return Err("Malformed occurrence syntax".into());
        }

        self.next_token()?;
        self.next_token()?;

        let upper = if let Token::INTLITERAL(ui) = &self.cur_token {
          Some(*ui)
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

  fn expect_peek(&mut self, t: &Token) -> bool {
    if self.peek_token_is(t) {
      return self.next_token().is_ok();
    }

    self.peek_error(t);

    false
  }

  fn peek_error(&mut self, t: &Token) {
    self.errors.push(
      format!(
        "expected next token to be {:?}, got {:?} instead",
        t, self.peek_token
      )
      .into(),
    )
  }
}

#[cfg(test)]
#[allow(unused_imports)]
mod tests {
  use super::super::{ast, lexer::Lexer, token::SocketPlug};
  use super::*;

  #[test]
  fn verify_rule() -> Result<(), Box<Error>> {
    let input = r#"myrule = myotherrule

secondrule = thirdrule"#;

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l)?;

    let cddl = p.parse_cddl()?;
    check_parser_errors(&p)?;

    if cddl.rules.len() != 2 {
      eprintln!(
        "cddl.rules does not contain 2 statements. got='{}'",
        cddl.rules.len()
      );
    }

    let expected_identifiers = ["myrule", "secondrule"];

    for (idx, expected_identifier) in expected_identifiers.iter().enumerate() {
      let rule = &cddl.rules[idx];
      assert!(test_rule(rule, expected_identifier));
    }

    Ok(())
  }

  fn test_rule(r: &Rule, name: &str) -> bool {
    match r {
      Rule::Type(tr) => {
        if (tr.name.0).0 != name {
          eprintln!("rule.name.value not '{}'. got={}", name, tr.name,);
          return false;
        }

        if tr.name.token_literal().unwrap() != Identifier((name, None)).token_literal().unwrap() {
          eprintln!(
            "rule.value not '{}'. got={}",
            name,
            tr.name.token_literal().unwrap()
          );
          return false;
        }

        true
      }
      _ => false,
    }
  }

  #[test]
  fn verify_genericparm() -> Result<(), Box<Error>> {
    let input = r#"<t, v>"#;

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l)?;

    let gps = p.parse_genericparm()?;
    check_parser_errors(&p)?;

    if gps.0.len() != 2 {
      eprintln!(
        "GenericParm does not contain 2 generic parameters. got='{}'",
        gps.0.len()
      );
    }

    let expected_generic_params = ["t", "v"];

    for (idx, expected_generic_param) in expected_generic_params.iter().enumerate() {
      let gp = &gps.0[idx];
      assert_eq!(gp.to_string(), *expected_generic_param);
    }

    Ok(())
  }

  #[test]
  fn verify_genericarg() -> Result<(), Box<Error>> {
    let input = r#"<"reboot", "now">"#;

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l)?;

    let generic_args = p.parse_genericarg()?;
    check_parser_errors(&p)?;

    if generic_args.0.len() != 2 {
      eprintln!(
        "generic_args does not contain 2 generic args. got='{}'",
        generic_args.0.len()
      );
    }

    let expected_generic_args = ["\"reboot\"", "\"now\""];

    for (idx, expected_generic_arg) in expected_generic_args.iter().enumerate() {
      let ga = &generic_args.0[idx];
      assert_eq!(ga.to_string(), *expected_generic_arg);
    }

    Ok(())
  }

  #[test]
  fn verify_type() -> Result<(), Box<Error>> {
    let input = r#"tchoice1 / tchoice2"#;

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l)?;

    let t = p.parse_type()?;
    check_parser_errors(&p)?;

    if t.0.len() != 2 {
      eprintln!(
        "type.0 does not contain 2 type choices. got='{}'",
        t.0.len()
      );
    }

    let expected_t1_identifiers = ["tchoice1", "tchoice2"];

    for (idx, expected_t1_identifier) in expected_t1_identifiers.iter().enumerate() {
      let t_choice = &t.0[idx];
      assert_eq!(t_choice.type2.to_string(), *expected_t1_identifier);
    }

    Ok(())
  }

  #[test]
  fn verify_type1() -> Result<(), Box<Error>> {
    let input = r#"mynumber..100.5"#;

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l)?;

    let t1 = p.parse_type1()?;
    check_parser_errors(&p)?;

    assert_eq!(t1.type2.to_string(), "mynumber");

    let (op, t2) = t1.operator.unwrap();
    assert_eq!((op, &*t2.to_string()), (RangeCtlOp::RangeOp(true), "100.5"));

    Ok(())
  }

  #[test]
  fn verify_type2() -> Result<(), Box<Error>> {
    let inputs = [
      r#""myvalue""#,
      r#"message<"reboot", "now">"#,
      r#"$$tcp-option"#,
      r#"~group1"#,
    ];

    let expected_outputs = [
      Type2::Value(Value::TEXT("\"myvalue\"")),
      Type2::Typename((
        Identifier(("message", None)),
        Some(GenericArg(vec![
          Type1 {
            type2: Type2::Value(Value::TEXT("\"reboot\"")),
            operator: None,
          },
          Type1 {
            type2: Type2::Value(Value::TEXT("\"now\"")),
            operator: None,
          },
        ])),
      )),
      Type2::Typename((Identifier(("tcp-option", Some(&SocketPlug::GROUP))), None)),
      Type2::Unwrap((Identifier(("group1", None)), None)),
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let mut l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(&mut l)?;

      let t2 = p.parse_type2()?;
      check_parser_errors(&p)?;

      assert_eq!(t2.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_grpent() -> Result<(), Box<Error>> {
    let inputs = [r#"* type1 => "value""#, r#"type1: type2"#, r#"typename"#];

    let expected_outputs = [
      GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
        occur: Some(Occur::ZeroOrMore),
        member_key: Some(MemberKey::Type1(Box::from((
          Type1 {
            type2: Type2::Typename((Identifier(("type1", None)), None)),
            operator: None,
          },
          false,
        )))),
        entry_type: Type(vec![Type1 {
          type2: Type2::Value(Value::TEXT("\"value\"")),
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
      let mut l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(&mut l)?;

      let grpent = p.parse_grpent()?;
      check_parser_errors(&p)?;

      assert_eq!(grpent.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_memberkey() -> Result<(), Box<Error>> {
    let inputs = [
      r#"type1 =>"#,
      r#""mytype1" ^ =>"#,
      r#"mybareword:"#,
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
          type2: Type2::Value(Value::TEXT("\"mytype1\"")),
          operator: None,
        },
        true,
      ))),
      MemberKey::Bareword(Identifier(("mybareword", None))),
      MemberKey::Value(Value::TEXT("\"myvalue\"")),
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let mut l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(&mut l)?;

      let mk = p.parse_memberkey(false)?;
      check_parser_errors(&p)?;

      assert_eq!(mk.unwrap().to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_occur() -> Result<(), Box<Error>> {
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
      let mut l = Lexer::new(&inputs[idx]);
      let mut p = Parser::new(&mut l)?;

      let o = p.parse_occur(false)?;
      check_parser_errors(&p)?;

      assert_eq!(o.unwrap().to_string(), expected_output.to_string());
    }

    Ok(())
  }

  fn check_parser_errors(p: &Parser) -> Result<(), Box<Error>> {
    if p.errors.is_empty() {
      return Ok(());
    }

    for err in p.errors.iter() {
      eprintln!("parser error: {}", err.to_string());
    }

    Err("Parser has errors".into())
  }
}
