use super::ast::*;
use super::lexer::Lexer;
use super::token::{Token, Value};
use std::{error::Error, mem};

struct Parser<'a> {
  l: &'a mut Lexer<'a>,
  cur_token: Token<'a>,
  peek_token: Token<'a>,
  errors: Vec<Box<Error>>,
}

impl<'a> Parser<'a> {
  fn new(l: &'a mut Lexer<'a>) -> Result<Parser, Box<Error>> {
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

  fn parse_cddl(&mut self) -> Result<CDDL<'a>, Box<Error>> {
    let mut c = CDDL::default();

    while self.cur_token != Token::EOF {
      c.rules.push(self.parse_rule()?);
    }

    Ok(c)
  }

  fn parse_rule(&mut self) -> Result<Rule<'a>, Box<Error>> {
    let name = match &self.cur_token {
      // Have to copy SocketPlug here
      Token::IDENT(i) => Token::IDENT(*i),
      _ => return Err("expected IDENT".into()),
    };

    let gp = if self.peek_token_is(&Token::LANGLEBRACKET) {
      Some(self.parse_genericparm()?)
    } else {
      None
    };

    if !self.expect_peek(&Token::ASSIGN)
      && !self.expect_peek(&Token::TCHOICEALT)
      && !self.expect_peek(&Token::GCHOICEALT)
    {
      return Err("Expected ASSIGN".into());
    }

    let mut is_type_choice_alternate = false;
    let mut _is_group_choice_alternate = false;

    if self.cur_token_is(Token::TCHOICEALT) {
      is_type_choice_alternate = true;
    } else if self.cur_token_is(Token::GCHOICEALT) {
      _is_group_choice_alternate = true;
    }

    self.next_token()?;

    let mut t: Type;

    // Parse grpent
    if self.cur_token_is(Token::LPAREN) {
      unimplemented!();
    } else {
      t = self.parse_type()?;
    }

    let tr = TypeRule {
      name: Identifier(name),
      generic_param: gp,
      is_type_choice_alternate,
      value: t,
    };

    Ok(Rule::Type(tr))
  }

  fn parse_genericparm(&mut self) -> Result<GenericParm<'a>, Box<Error>> {
    self.next_token()?;

    let mut generic_params = GenericParm(Vec::new());

    while !self.cur_token_is(Token::RANGLEBRACKET) {
      match &self.cur_token {
        Token::IDENT(i) => {
          generic_params.0.push(Identifier::from(i.0));
          self.next_token()?;
        }
        Token::COMMA => self.next_token()?,
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
      if self.cur_token_is(Token::COMMA) {
        self.next_token()?;
      }
    }

    self.next_token()?;

    Ok(generic_args)
  }

  fn parse_type(&mut self) -> Result<Type<'a>, Box<Error>> {
    let mut t = Type(Vec::new());

    t.0.push(self.parse_type1()?);

    while self.cur_token_is(Token::TCHOICE) {
      self.next_token()?;
      t.0.push(self.parse_type1()?);
    }

    Ok(t)
  }

  fn parse_type1(&mut self) -> Result<Type1<'a>, Box<Error>> {
    match &self.cur_token {
      Token::RANGE((lower, upper, inclusive)) => {
        let lower_ident = if let Token::IDENT(ident) = lower.as_ref() {
          Some(Token::IDENT(*ident))
        } else {
          None
        };

        let upper_ident = if let Token::IDENT(ident) = upper.as_ref() {
          Some(Token::IDENT(*ident))
        } else {
          None
        };

        let lower_value = lower.as_value();
        let upper_value = upper.as_value();

        if let Some(li) = lower_ident {
          let mut t1 = Type1 {
            type2: Type2::Typename((Identifier(li), None)),
            operator: None,
          };

          if let Some(ui) = upper_ident {
            t1.operator = Some((
              RangeCtlOp::RangeOp(*inclusive),
              Type2::Typename((Identifier(ui), None)),
            ));
          } else {
            t1.operator = Some((
              RangeCtlOp::RangeOp(*inclusive),
              Type2::Value(upper_value.ok_or_else(|| "Illegal upper range value")?),
            ));
          }

          Ok(t1)
        } else {
          let mut t1 = Type1 {
            type2: Type2::Value(lower_value.ok_or_else(|| "Illegal lower range value")?),
            operator: None,
          };

          if let Some(ui) = upper_ident {
            t1.operator = Some((
              RangeCtlOp::RangeOp(*inclusive),
              Type2::Typename((Identifier(ui), None)),
            ));
          } else {
            t1.operator = Some((
              RangeCtlOp::RangeOp(*inclusive),
              Type2::Value(upper_value.ok_or_else(|| "Illegal upper range value")?),
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
          Value::TEXT(text) => Ok(Type2::Value(Value::TEXT(text))),
          _ => Err("bad value".into()),
        }
      }
      // typename [genericarg]
      Token::IDENT(ident) => {
        // optional genericarg detected
        if self.peek_token_is(&Token::LANGLEBRACKET) {          
          return Ok(Type2::Typename((
            Identifier::from(ident.0),
            Some(self.parse_genericarg()?),
          )));
        }

        Ok(Type2::Typename((Identifier::from(ident.0), None)))
      }
      _ => return Err("Unknown".into()),
    };

    self.next_token()?;

    t2
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
  use super::super::{ast, lexer::Lexer};
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
        if tr.name.0.to_string() != name {
          eprintln!(
            "rule.name.value not '{}'. got={}",
            name,
            tr.name.0.to_string()
          );
          return false;
        }

        if tr.name.token_literal().unwrap() != format!("{:?}", Token::IDENT((name, None))) {
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
    ];

    let expected_outputs = [
      Type2::Value(Value::TEXT("\"myvalue\"")),
      Type2::Typename((Identifier(Token::IDENT(("message", None))), Some(GenericArg(vec![
        Type1{type2: Type2::Value(Value::TEXT("\"reboot\"")), operator: None},
        Type1{type2: Type2::Value(Value::TEXT("\"now\"")), operator: None},
      ])))),
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
