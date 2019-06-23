use super::token;
use super::token::{RangeValue, Tag, Token, Value};
use std::{convert::TryFrom, error::Error, iter::Peekable, str::CharIndices};

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
        (_, '+') => Ok(Token::ONEORMORE),
        (_, '?') => Ok(Token::OPTIONAL),
        (_, '*') => Ok(Token::ASTERISK),
        (_, '(') => Ok(Token::LPAREN),
        (_, ')') => Ok(Token::RPAREN),
        (_, '[') => Ok(Token::LBRACKET),
        (_, ']') => Ok(Token::RBRACKET),
        (_, '<') => Ok(Token::LANGLEBRACKET),
        (idx, '"') => Ok(Token::VALUE(Value::TEXT(self.read_text_value(idx)?))),
        (_, '{') => Ok(Token::LBRACE),
        (_, '}') => Ok(Token::RBRACE),
        (_, ',') => Ok(Token::COMMA),
        (idx, ';') => Ok(Token::COMMENT(self.read_comment(idx)?)),
        (_, ':') => Ok(Token::COLON),
        (_, '^') => Ok(Token::CUT),
        (_, '&') => Ok(Token::GTOCHOICE),
        (_, '>') => Ok(Token::RANGLEBRACKET),
        (_, '~') => Ok(Token::UNWRAP),
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
          Some(&c) if is_digit(c.1) => Ok(Token::TAG(self.read_tag()?)),
          None => Ok(Token::TAG(Tag::ANY)),
          _ => Ok(Token::ILLEGAL),
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

            match self.peek_char() {
              // Range detected
              Some(&c) if c.1 == '.' => {
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
              &self.str_input[idx + 1..self.read_char()?.0],
            )?)
          }
        }
      }
    }

    Ok("")
  }

  fn read_comment(&mut self, idx: usize) -> Result<&'a str, Box<Error>> {
    while let Some(&(_, ch)) = self.peek_char() {
      if ch != '\x0a' && ch != '\x0d' {
        let _ = self.read_char()?;
      } else {
        return Ok(std::str::from_utf8(
          &self.str_input[idx + 1..self.read_char()?.0],
        )?);
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

  fn read_tag(&mut self) -> Result<Tag<'a>, Box<Error>> {
    match self.read_char() {
      Ok(c) if c.1 == '6' => {
        let (_, ch) = self.read_char()?;

        if ch == '.' {
          let (idx, _) = self.read_char()?;

          let t = self.read_number(idx)?;

          if let Ok(c) = self.read_char() {
            if c.1 == '(' {
              let (idx, _) = self.read_char()?;
              let tag = Tag::DATA((Some(t), self.read_identifier(idx)?));

              if let Some(c) = self.peek_char() {
                if c.1 == ')' {
                  let _ = self.read_char()?;
                } else {
                  return Err("Malformed tag".into());
                }
              }

              return Ok(tag);
            }
          }
        }

        let (idx, _) = self.read_char()?;

        Ok(Tag::DATA((None, self.read_identifier(idx)?)))
      }
      Ok(c) if is_digit(c.1) => {
        let mt = self.read_number(c.0)? as u8;

        let (_, ch) = self.read_char()?;

        if ch == '.' {
          let (idx, _) = self.read_char()?;

          let t = self.read_number(idx)?;
          return Ok(Tag::MAJORTYPE((mt, Some(t))));
        }

        Ok(Tag::MAJORTYPE((mt, None)))
      }
      _ => Err("Malformed tag".into()),
    }
  }

  fn read_range(&mut self, lower: Token<'a>) -> Result<Token<'a>, Box<Error>> {
    let mut is_inclusive = true;

    if let Some(&c) = self.peek_char() {
      if c.1 == '.' {
        is_inclusive = false;

        let _ = self.read_char()?;
      }
    }

    let c = self.read_char()?;

    if is_digit(c.1) {
      let upper = self.read_int_or_float(c.0)?;

      match lower {
        Token::INTLITERAL(li) => {
          if let Token::INTLITERAL(ui) = upper {
            return Ok(Token::RANGE((
              RangeValue::INT(li),
              RangeValue::INT(ui),
              is_inclusive,
            )));
          } else {
            return Err(
              "Only numerical ranges between integers or floating point values are allowed".into(),
            );
          }
        }
        Token::FLOATLITERAL(lf) => {
          if let Token::FLOATLITERAL(uf) = upper {
            return Ok(Token::RANGE((
              RangeValue::FLOAT(lf),
              RangeValue::FLOAT(uf),
              is_inclusive,
            )));
          } else {
            return Err(
              "Only numerical ranges between integers or floating point values are allowed".into(),
            );
          }
        }
        _ => {
          return Ok(Token::RANGE((
            RangeValue::try_from(lower)?,
            RangeValue::try_from(upper)?,
            is_inclusive,
          )))
        }
      }
    } else if is_ealpha(c.1) {
      return Ok(Token::RANGE((
        RangeValue::try_from(lower)?,
        RangeValue::try_from(token::lookup_ident(self.read_identifier(c.0)?))?,
        is_inclusive,
      )));
    }

    Err("invalid range syntax".into())
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
    let input = r#"; this is a comment
; this is another comment

mynumber = 10.5

mytag = #6.1234(tstr)
    
myfirstrule = "myotherrule"

mysecondrule = mynumber..100.5

@terminal-color = basecolors / othercolors ; an inline comment
    
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
  1*3 $$tcp-option,
)"#;

    let expected_tok = [
      (COMMENT(" this is a comment"), "; this is a comment"),
      (
        COMMENT(" this is another comment"),
        "; this is another comment",
      ),
      (IDENT(("mynumber", None)), "mynumber"),
      (ASSIGN, "="),
      (FLOATLITERAL(10.5), "10.5"),
      (IDENT(("mytag", None)), "mytag"),
      (ASSIGN, "="),
      (TAG(Tag::DATA((Some(1234), "tstr"))), "#6.1234(tstr)"),
      (IDENT(("myfirstrule", None)), "myfirstrule"),
      (ASSIGN, "="),
      (VALUE(Value::TEXT("myotherrule")), "myotherrule"),
      (IDENT(("mysecondrule", None)), "mysecondrule"),
      (ASSIGN, "="),
      (
        RANGE((
          RangeValue::IDENT(("mynumber", None)),
          RangeValue::FLOAT(100.5),
          true,
        )),
        "mynumber..100.5",
      ),
      (IDENT(("@terminal-color", None)), "@terminal-color"),
      (ASSIGN, "="),
      (IDENT(("basecolors", None)), "basecolors"),
      (TCHOICE, "/"),
      (IDENT(("othercolors", None)), "othercolors"),
      (COMMENT(" an inline comment"), "; an inline comment"),
      (IDENT(("messages", None)), "messages"),
      (ASSIGN, "="),
      (IDENT(("message", None)), "message"),
      (LANGLEBRACKET, "<"),
      (VALUE(Value::TEXT("reboot")), "reboot"),
      (COMMA, ","),
      (VALUE(Value::TEXT("now")), "now"),
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
      (INTLITERAL(1), "1"),
      (ASTERISK, "*"),
      (INTLITERAL(3), "3"),
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
