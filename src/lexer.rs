use super::token;
use super::token::{Token, Value};
use std::error::Error;
use std::iter::Peekable;
use std::str::CharIndices;

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
      .ok_or("Unable to advance to the next token".into())
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
        (_, '$') => match self.peek_char() {
          Some(&c) if c.1 == '$' => {
            let _ = self.read_char()?;

            Ok(Token::GSOCKET)
          }
          _ => Ok(Token::TSOCKET),
        },
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
        _ => {
          if is_ealpha(c.1) {
            let ident = token::lookup_ident(self.read_identifier(c.0)?);

            // Range detected
            match self.peek_char() {
              Some(&c) if c.1 == '.' => {
                let _ = self.read_char()?;

                return self.read_range(&ident);
              }
              _ => return Ok(ident),
            }
          } else if is_digit(c.1) {
            let number = self.read_int_or_float()?;

            // Range detected
            match self.read_char() {
              Ok(c) if c.1 == '.' => return self.read_range(&number),
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

    let mut special_char_count = 0;

    while let Some(&c) = self.peek_char() {
      if is_ealpha(c.1) || is_digit(c.1) || c.1 == '.' || c.1 == '-' {
        // Illegal to have multiple "."'s or "-"'s in an identifier
        if c.1 == '.' || c.1 == '-' {
          if special_char_count > 1 {
            return Err("Invalid identifier".into());
          }

          special_char_count += 1;
        }

        let (ei, _) = self.read_char()?;

        end_idx = ei;
      } else {
        break;
      }
    }
    Ok(std::str::from_utf8(&self.str_input[idx..=end_idx])?)
  }

  fn read_text_value(&mut self, idx: usize) -> Result<&'a str, Box<Error>> {
    let mut end_index = idx;

    while let Some(&c) = self.peek_char() {
      // TODO: support SESC = "\" (%x20-7E / %x80-10FFFD)
      if c.1 == '\x21'
        || (c.1 >= '\x23' && c.1 <= '\x5b')
        || (c.1 >= '\x5d' && c.1 <= '\x7e')
        || (c.1 >= '\u{0128}' && c.1 <= '\u{10FFFD}')
      {
        let (ei, _) = self.read_char()?;

        end_index = ei;
      } else {
        break;
      }
    }

    match self.peek_char() {
      Some(&c) if c.1 != '"' => return Err("Expecting closing \" in text value".into()),
      _ => {
        let (ei, _) = self.read_char()?;
        end_index = ei;

        Ok(std::str::from_utf8(&self.str_input[idx..=end_index])?)
      }
    }    
  }

  fn skip_whitespace(&mut self) {
    while let Some(&c) = self.peek_char() {
      if c.1.is_whitespace() {
        let _ = self.read_char();
      } else {
        break;
      }
    }
  }

  fn read_int_or_float(&mut self) -> Result<Token<'a>, Box<Error>> {
    let (idx, _) = self.read_char()?;

    let i = self.read_number(idx)?;

    if let Some(&c) = self.peek_char() {
      if c.1 == '.' {
        let _ = self.read_char()?;

        if let Some(&c) = self.peek_char() {
          if is_digit(c.1) {
            return Ok(Token::FLOATLITERAL(
              format!("{}.{}", i, self.read_number(idx)?).parse::<f64>()?,
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

  fn read_range(&mut self, lower: &Token) -> Result<Token<'a>, Box<Error>> {
    let mut is_inclusive = true;
    let mut t = Token::ILLEGAL;

    if let Ok(c) = self.read_char() {
      if c.1 == '.' {
        is_inclusive = false;
      }

      if is_digit(c.1) {
        t = Token::RANGE((
          lower.to_string(),
          self.read_int_or_float()?.to_string(),
          is_inclusive,
        ));
      } else if is_ealpha(c.1) {
        t = Token::RANGE((lower.to_string(), self.read_identifier(c.0)?.to_string(), is_inclusive));
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
  use super::super::token::Token::*;
  use super::*;

  #[test]
  fn verify_next_token() {
    let input = r#"myfirstrule = "myotherrule"

mysecondrule = mythirdrule

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
  zip-code: uint
)"#;

    let expected_tok = [
      (IDENT("myfirstrule"), "myfirstrule"),
      (ASSIGN, "="),
      (VALUE(Value::TEXT("\"myotherrule\"")), "\"myotherrule\""),
      (IDENT("mysecondrule"), "mysecondrule"),
      (ASSIGN, "="),
      (IDENT("mythirdrule"), "mythirdrule"),
      (IDENT("@terminal-color"), "@terminal-color"),
      (ASSIGN, "="),
      (IDENT("basecolors"), "basecolors"),
      (TCHOICE, "/"),
      (IDENT("othercolors"), "othercolors"),
      (IDENT("messages"), "messages"),
      (ASSIGN, "="),
      (IDENT("message"), "message"),
      (LANGLEBRACKET, "<"),
      (VALUE(Value::TEXT("\"reboot\"")), "\"reboot\""),
      (COMMA, ","),
      (VALUE(Value::TEXT("\"now\"")), "\"now\""),
      (RANGLEBRACKET, ">"),
      (IDENT("address"), "address"),
      (ASSIGN, "="),
      (LBRACE, "{"),
      (IDENT("delivery"), "delivery"),
      (RBRACE, "}"),
      (IDENT("delivery"), "delivery"),
      (ASSIGN, "="),
      (LPAREN, "("),
      (IDENT("street"), "street"),
      (COLON, ":"),
      (TSTR, "tstr"),
      (COMMA, ","),
      (OPTIONAL, "?"),
      (NUMBER, "number"),
      (CUT, "^"),
      (ARROWMAP, "=>"),
      (UINT, "uint"),
      (COMMA, ","),
      (IDENT("city"), "city"),
      (GCHOICE, "//"),
      (IDENT("po-box"), "po-box"),
      (COLON, ":"),
      (UINT, "uint"),
      (COMMA, ","),
      (IDENT("city"), "city"),
      (GCHOICE, "//"),
      (IDENT("per-pickup"), "per-pickup"),
      (COLON, ":"),
      (TRUE, "true"),
      (RPAREN, ")"),
      (IDENT("city".into()), "city"),
      (ASSIGN, "="),
      (LPAREN, "("),
      (IDENT("name"), "name"),
      (COLON, ":"),
      (TSTR, "tstr"),
      (COMMA, ","),
      (IDENT("zip-code"), "zip-code"),
      (COLON, ":"),
      (UINT, "uint"),
      (RPAREN, ")"),
    ];

    let mut l = Lexer::new(input);

    for (expected_tok, literal) in expected_tok.iter() {
      let tok = l.next_token().unwrap();
      assert_eq!((expected_tok, *literal), (&tok, &*tok.to_string()))
    }
  }
}
