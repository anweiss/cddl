use super::token;
use super::token::{Token, Value};
use std::{error::Error, iter::Peekable, str::CharIndices};

pub struct Lexer<'a> {
  str_input: &'a [u8],
  input: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Lexer<'a> {
    Lexer {
      str_input: input.as_bytes(),
      input: input.char_indices().peekable(),
    }
  }

  fn read_char(&mut self) -> Result<(usize, char), Box<Error>> {
    self
      .input
      .next()
      .ok_or_else(|| "Unable to advance to the next token".into())
  }

  pub fn next_token(&mut self) -> Result<Token<'a>, Box<Error>> {
    self.skip_whitespace();

    if let Ok(c) = self.read_char() {
      match c {
        (_, '=') => match self.peek_char() {
          Some(&c) if c.1 == '>' => {
            let _ = self.read_char()?;
            Ok(Token::ARROWMAP)
          }
          _ => Ok(Token::ASSIGN),
        },
        (_, '+') => Ok(Token::PLUS),
        (_, '?') => Ok(Token::OPTIONAL),
        (_, '*') => Ok(Token::ASTERISK),
        (_, '(') => Ok(Token::LPAREN),
        (_, ')') => Ok(Token::RPAREN),
        (_, '<') => Ok(Token::LANGLEBRACKET),
        (idx, '"') => Ok(Token::VALUE(Value::TEXT(self.read_text_value(idx)?))),
        (_, '{') => Ok(Token::LBRACE),
        (_, '}') => Ok(Token::RBRACE),
        (_, ',') => Ok(Token::COMMA),
        (_, ';') => Ok(Token::SEMICOLON),
        (_, ':') => Ok(Token::COLON),
        (_, '^') => Ok(Token::CUT),
        (_, '&') => Ok(Token::GTOCHOICE),
        (_, '>') => Ok(Token::RANGLEBRACKET),
        (_, '/') => match self.peek_char() {
          Some(&c) if c.1 == '/' => {
            let _ = self.read_char()?;

            match self.peek_char() {
              Some(&c) if c.1 == '=' => {
                let _ = self.read_char()?;
                Ok(Token::GCHOICEALT)
              }
              _ => Ok(Token::GCHOICE),
            }
          }
          Some(&c) if c.1 == '=' => {
            let _ = self.read_char()?;
            Ok(Token::TCHOICEALT)
          }
          _ => Ok(Token::TCHOICE),
        },
        (_, '#') => match self.peek_char() {
          Some(&c) if c.1 == '6' => {
            let _ = self.read_char()?;
            Ok(Token::TAG(self.read_tag()?))
          }
          None => Ok(Token::ANY),
          _ => Ok(Token::ILLEGAL), // Temporary ... need to lex Some(c)
        },
        (_, '.') => {
          let (idx, _) = self.read_char()?;

          Ok(token::lookup_control(self.read_identifier(idx)?))
        }
        (idx, ch) => {
          if is_ealpha(ch) {
            let ident = token::lookup_ident(self.read_identifier(idx)?);

            // Range detected
            match self.peek_char() {
              Some(&c) if c.1 == '.' => {
                let _ = self.read_char()?;

                return self.read_range(ident);
              }
              _ => return Ok(ident),
            }
          } else if is_digit(ch) {
            let number = self.read_int_or_float(idx)?;
            // Range detected
            match self.read_char() {
              Ok(c) if c.1 == '.' => {
                let _ = self.read_char()?;

                return self.read_range(number);
              }
              _ => return Ok(number),
            }
          }

          Ok(Token::ILLEGAL)
        }
      }
    } else {
      Ok(Token::EOF)
    }
  }

  fn read_identifier(&mut self, idx: usize) -> Result<&'a str, Box<Error>> {
    let mut end_idx = idx;

    while let Some(&c) = self.peek_char() {
      if is_ealpha(c.1) || is_digit(c.1) || c.1 == '.' || c.1 == '-' {
        match c.1 {
          // Check for range
          '.' => {
            end_idx = self.read_char()?.0;

            if let Some(&c) = self.peek_char() {
              if c.1 == '.' {
                return Ok(std::str::from_utf8(&self.str_input[idx..end_idx])?);
              }
            }
          }
          _ => end_idx = self.read_char()?.0,
        }
      } else {
        break;
      }
    }
    Ok(std::str::from_utf8(&self.str_input[idx..=end_idx])?)
  }

  fn read_text_value(&mut self, idx: usize) -> Result<&'a str, Box<Error>> {
    while let Some(&(_, ch)) = self.peek_char() {
      // TODO: support SESC = "\" (%x20-7E / %x80-10FFFD)
      if ch == '\x21'
        || (ch >= '\x23' && ch <= '\x5b')
        || (ch >= '\x5d' && ch <= '\x7e')
        || (ch >= '\u{0128}' && ch <= '\u{10FFFD}')
      {
        let _ = self.read_char()?;
      } else {
        match self.peek_char() {
          Some(&(_, ch)) if ch != '"' => return Err("Expecting closing \" in text value".into()),
          _ => {
            return Ok(std::str::from_utf8(
              &self.str_input[idx..=self.read_char()?.0],
            )?)
          }
        }
      }
    }

    Ok("")
  }

  fn skip_whitespace(&mut self) {
    while let Some(&(_, ch)) = self.peek_char() {
      if ch.is_whitespace() {
        let _ = self.read_char();
      } else {
        break;
      }
    }
  }

  fn read_int_or_float(&mut self, idx: usize) -> Result<Token<'a>, Box<Error>> {
    let i = self.read_number(idx)?;

    if let Some(&c) = self.peek_char() {
      if c.1 == '.' {
        let _ = self.read_char()?;

        if let Some(&c) = self.peek_char() {
          if is_digit(c.1) {
            return Ok(Token::FLOATLITERAL(
              format!("{}.{}", i, self.read_number(c.0)?).parse::<f64>()?,
            ));
          }
        }
      }
    }

    Ok(Token::INTLITERAL(i))
  }

  fn read_number(&mut self, idx: usize) -> Result<usize, Box<Error>> {
    let mut end_index = idx;

    while let Some(&c) = self.peek_char() {
      if is_digit(c.1) {
        let (ei, _) = self.read_char()?;

        end_index = ei;
      } else {
        break;
      }
    }

    Ok(std::str::from_utf8(&self.str_input[idx..=end_index])?.parse::<usize>()?)
  }

  fn peek_char(&mut self) -> Option<&(usize, char)> {
    self.input.peek()
  }

  fn read_tag(&mut self) -> Result<(usize, &'a str), Box<Error>> {
    if let Ok(c) = self.read_char() {
      if c.1 == '.' {
        let (idx, _) = self.read_char()?;

        let t = self.read_number(idx)?;

        if let Ok(c) = self.read_char() {
          if c.1 == '(' {
            let (idx, _) = self.read_char()?;

            return Ok((t, self.read_identifier(idx)?));
          }
        }

        return Ok((t, ""));
      }
    }

    Ok((0, ""))
  }

  fn read_range(&mut self, lower: Token<'a>) -> Result<Token<'a>, Box<Error>> {
    let mut is_inclusive = true;
    let mut t = Token::ILLEGAL;

    if let Some(&c) = self.peek_char() {
      if c.1 == '.' {
        is_inclusive = false;

        let _ = self.read_char()?;
      }
    }

    if let Ok(c) = self.read_char() {
      if is_digit(c.1) {
        let upper = self.read_int_or_float(c.0)?;

        match lower {
          Token::INTLITERAL(_) => {
            if let Token::INTLITERAL(_) = upper {
              return Ok(Token::RANGE((
                Box::from(lower),
                Box::from(upper),
                is_inclusive,
              )));
            } else {
              return Err(
                "Only numerical ranges between integers or floating point values are allowed"
                  .into(),
              );
            }
          }
          Token::FLOATLITERAL(_) => {
            if let Token::FLOATLITERAL(_) = upper {
              return Ok(Token::RANGE((
                Box::from(lower),
                Box::from(upper),
                is_inclusive,
              )));
            } else {
              return Err(
                "Only numerical ranges between integers or floating point values are allowed"
                  .into(),
              );
            }
          }
          _ => {
            return Ok(Token::RANGE((
              Box::from(lower),
              Box::from(upper),
              is_inclusive,
            )))
          }
        }
      } else if is_ealpha(c.1) {
        t = Token::RANGE((
          Box::from(lower),
          Box::from(token::lookup_ident(self.read_identifier(c.0)?)),
          is_inclusive,
        ));
      }
    }

    Ok(t)
  }
}

fn is_ealpha(ch: char) -> bool {
  ch.is_alphabetic() || ch == '@' || ch == '_' || ch == '$'
}

fn is_digit(ch: char) -> bool {
  ch.is_digit(10)
}

#[cfg(test)]
mod tests {
  use super::super::token::{SocketPlug, Token::*};
  use super::*;

  #[test]
  fn verify_next_token() {
    let input = r#"mynumber = 10.5
    
myfirstrule = "myotherrule"

mysecondrule = mynumber..100.5

@terminal-color = basecolors / othercolors
    
messages = message<"reboot", "now">

address = { delivery }

delivery = (
  street: tstr, ? number ^ => uint, city //
  po-box: uint, city //
  per-pickup: true
)

city = (
  name: tstr,
  zip-code: uint,
  * $$tcp-option,
)"#;

    let expected_tok = [
      (IDENT(("mynumber", None)), "mynumber"),
      (ASSIGN, "="),
      (FLOATLITERAL(10.5), "10.5"),
      (IDENT(("myfirstrule", None)), "myfirstrule"),
      (ASSIGN, "="),
      (VALUE(Value::TEXT("\"myotherrule\"")), "\"myotherrule\""),
      (IDENT(("mysecondrule", None)), "mysecondrule"),
      (ASSIGN, "="),
      (
        RANGE((
          Box::from(IDENT(("mynumber", None))),
          Box::from(FLOATLITERAL(100.5)),
          true,
        )),
        "mynumber..100.5",
      ),
      (IDENT(("@terminal-color", None)), "@terminal-color"),
      (ASSIGN, "="),
      (IDENT(("basecolors", None)), "basecolors"),
      (TCHOICE, "/"),
      (IDENT(("othercolors", None)), "othercolors"),
      (IDENT(("messages", None)), "messages"),
      (ASSIGN, "="),
      (IDENT(("message", None)), "message"),
      (LANGLEBRACKET, "<"),
      (VALUE(Value::TEXT("\"reboot\"")), "\"reboot\""),
      (COMMA, ","),
      (VALUE(Value::TEXT("\"now\"")), "\"now\""),
      (RANGLEBRACKET, ">"),
      (IDENT(("address", None)), "address"),
      (ASSIGN, "="),
      (LBRACE, "{"),
      (IDENT(("delivery", None)), "delivery"),
      (RBRACE, "}"),
      (IDENT(("delivery", None)), "delivery"),
      (ASSIGN, "="),
      (LPAREN, "("),
      (IDENT(("street", None)), "street"),
      (COLON, ":"),
      (TSTR, "tstr"),
      (COMMA, ","),
      (OPTIONAL, "?"),
      (NUMBER, "number"),
      (CUT, "^"),
      (ARROWMAP, "=>"),
      (UINT, "uint"),
      (COMMA, ","),
      (IDENT(("city", None)), "city"),
      (GCHOICE, "//"),
      (IDENT(("po-box", None)), "po-box"),
      (COLON, ":"),
      (UINT, "uint"),
      (COMMA, ","),
      (IDENT(("city", None)), "city"),
      (GCHOICE, "//"),
      (IDENT(("per-pickup", None)), "per-pickup"),
      (COLON, ":"),
      (TRUE, "true"),
      (RPAREN, ")"),
      (IDENT(("city", None)), "city"),
      (ASSIGN, "="),
      (LPAREN, "("),
      (IDENT(("name", None)), "name"),
      (COLON, ":"),
      (TSTR, "tstr"),
      (COMMA, ","),
      (IDENT(("zip-code", None)), "zip-code"),
      (COLON, ":"),
      (UINT, "uint"),
      (COMMA, ","),
      (ASTERISK, "*"),
      (
        IDENT(("tcp-option", Some(&SocketPlug::GROUP))),
        "$$tcp-option",
      ),
      (COMMA, ","),
      (RPAREN, ")"),
    ];

    let mut l = Lexer::new(input);

    for (expected_tok, literal) in expected_tok.iter() {
      let tok = l.next_token().unwrap();
      assert_eq!((expected_tok, *literal), (&tok, &*tok.to_string()))
    }
  }
}
