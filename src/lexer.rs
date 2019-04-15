use super::token;
use super::token::{Token, Value};
use std::error::Error;
use std::iter::Peekable;
use std::str::Chars;

// pub struct Lexer<'a> {
//   input: &'a str,
//   position: usize,
//   read_position: usize,
//   ch: char,
// }

pub struct Lexer<'a> {
  input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Lexer<'a> {
    Lexer {
      input: input.chars().peekable(),
    }
  }

  fn read_char(&mut self) -> Result<char, Box<Error>> {
    self
      .input
      .next()
      .ok_or("Unable to advance to the next token".into())
  }

  pub fn next_token(&mut self) -> Result<Token, Box<Error>> {
    self.skip_whitespace();

    if let Ok(c) = self.read_char() {
      match c {
        '=' => match self.peek_char() {
          Some(&c) if c == '>' => {
            let _ = self.read_char()?;
            Ok(Token::ARROWMAP)
          }
          _ => Ok(Token::ASSIGN),
        },
        '+' => Ok(Token::PLUS),
        '?' => Ok(Token::OPTIONAL),
        '*' => Ok(Token::ASTERISK),
        '(' => Ok(Token::LPAREN),
        ')' => Ok(Token::RPAREN),
        '<' => Ok(Token::LANGLEBRACKET),
        '"' => {
          let v = Token::VALUE(Value::TEXT(self.read_text_value()?));
          match self.peek_char() {
            Some(&c) if c != '"' => return Err("Expecting closing \" in text value".into()),
            _ => {
              let _ = self.read_char()?;
              Ok(v)
            }
          }
        }
        '{' => Ok(Token::LBRACE),
        '}' => Ok(Token::RBRACE),
        ',' => Ok(Token::COMMA),
        ';' => Ok(Token::SEMICOLON),
        ':' => Ok(Token::COLON),
        '^' => Ok(Token::CUT),
        '&' => Ok(Token::GTOCHOICE),
        '>' => Ok(Token::RANGLEBRACKET),
        '$' => match self.peek_char() {
          Some(&c) if c == '$' => {
            let _ = self.read_char()?;

            Ok(Token::GSOCKET)
          }
          _ => Ok(Token::TSOCKET),
        },
        '/' => match self.peek_char() {
          Some(&c) if c == '/' => {
            let _ = self.read_char()?;

            match self.peek_char() {
              Some(&c) if c == '=' => {
                let _ = self.read_char()?;
                Ok(Token::GCHOICEALT)
              }
              _ => Ok(Token::GCHOICE),
            }
          }
          Some(&c) if c == '=' => {
            let _ = self.read_char()?;
            Ok(Token::TCHOICEALT)
          }
          _ => Ok(Token::TCHOICE),
        },
        '#' => match self.peek_char() {
          Some(&c) if c == '6' => {
            let _ = self.read_char()?;
            Ok(Token::TAG(self.read_tag()?))
          }
          None => Ok(Token::ANY),
          _ => Ok(Token::ILLEGAL), // Temporary ... need to lex Some(c)
        },
        '.' => {
          let ch = self.read_char()?;

          Ok(token::lookup_control(&*self.read_identifier(ch)?))
        }
        _ => {
          if is_ealpha(c) {
            let ident = token::lookup_ident(&*self.read_identifier(c)?);

            // Range detected
            match self.peek_char() {
              Some(&c) if c == '.' => {
                let _ = self.read_char()?;

                return self.read_range(ident);
              }
              _ => return Ok(ident),
            }
          } else if is_digit(c) {
            let number = self.read_int_or_float()?;

            // Range detected
            match self.read_char() {
              Ok(c) if c == '.' => return self.read_range(number),
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

  fn read_identifier(&mut self, c: char) -> Result<String, Box<Error>> {
    let mut ident = String::new();
    ident.push(c);

    let mut special_char_count = 0;

    while let Some(&c) = self.peek_char() {
      if is_ealpha(c) || is_digit(c) || c == '.' || c == '-' {
        // Illegal to have multiple "."'s or "-"'s in an identifier
        if c == '.' || c == '-' {
          if special_char_count > 1 {
            return Err("Invalid identifier".into());
          }

          special_char_count += 1;
        }

        ident.push(self.read_char()?);
      } else {
        break;
      }
    }
    Ok(ident)
  }

  fn read_text_value(&mut self) -> Result<String, Box<Error>> {
    let mut text_value = String::new();

    while let Some(&c) = self.peek_char() {
      // TODO: support SESC = "\" (%x20-7E / %x80-10FFFD)
      if c == '\x21'
        || (c >= '\x23' && c <= '\x5b')
        || (c >= '\x5d' && c <= '\x7e')
        || (c >= '\u{0128}' && c <= '\u{10FFFD}')
      {
        text_value.push(self.read_char()?);
      } else {
        break;
      }
    }

    Ok(text_value)
  }

  fn skip_whitespace(&mut self) {
    while let Some(&c) = self.peek_char() {
      if c.is_whitespace() {
        let _ = self.read_char();
      } else {
        break;
      }
    }
  }

  fn read_int_or_float(&mut self) -> Result<Token, Box<Error>> {
    let ch = self.read_char()?;

    let i = self.read_number(ch)?;

    if let Some(&c) = self.peek_char() {
      if c == '.' {
        let _ = self.read_char()?;

        if let Some(&c) = self.peek_char() {
          if is_digit(c) {
            return Ok(Token::FLOATLITERAL(
              format!("{}.{}", i, self.read_number(c)?).parse::<f64>()?,
            ));
          }
        }
      }
    }

    Ok(Token::INTLITERAL(i))
  }

  fn read_number(&mut self, c: char) -> Result<usize, Box<Error>> {
    let mut number = String::new();
    number.push(c);

    while let Some(&c) = self.peek_char() {
      if is_digit(c) {
        number.push(self.read_char()?);
      } else {
        break;
      }
    }

    Ok(number.parse::<usize>()?)
  }

  fn peek_char(&mut self) -> Option<&char> {
    self.input.peek()
  }

  fn read_tag(&mut self) -> Result<(usize, String), Box<Error>> {
    if let Ok(c) = self.read_char() {
      if c == '.' {
        let ch = self.read_char()?;

        let t = self.read_number(ch)?;

        if let Ok(c) = self.read_char() {
          if c == '(' {
            let ch = self.read_char()?;

            return Ok((t, self.read_identifier(ch)?));
          }
        }

        return Ok((t, String::default()));
      }
    }

    Ok((0, String::default()))
  }

  fn read_range(&mut self, lower: Token) -> Result<Token, Box<Error>> {
    let mut is_inclusive = true;
    let mut t = Token::ILLEGAL;

    if let Ok(c) = self.read_char() {
      if c == '.' {
        is_inclusive = false;
      }

      if is_digit(c) {
        t = Token::RANGE((
          lower.to_string(),
          self.read_int_or_float()?.to_string(),
          is_inclusive,
        ));
      } else if is_ealpha(c) {
        t = Token::RANGE((lower.to_string(), self.read_identifier(c)?, is_inclusive));
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
      (IDENT("myfirstrule".into()), "myfirstrule"),
      (ASSIGN, "="),
      (VALUE(Value::TEXT("myotherrule".into())), "\"myotherrule\""),
      (IDENT("mysecondrule".into()), "mysecondrule"),
      (ASSIGN, "="),
      (IDENT("mythirdrule".into()), "mythirdrule"),
      (IDENT("@terminal-color".into()), "@terminal-color"),
      (ASSIGN, "="),
      (IDENT("basecolors".into()), "basecolors"),
      (TCHOICE, "/"),
      (IDENT("othercolors".into()), "othercolors"),
      (IDENT("messages".into()), "messages"),
      (ASSIGN, "="),
      (IDENT("message".into()), "message"),
      (LANGLEBRACKET, "<"),
      (VALUE(Value::TEXT("reboot".into())), "\"reboot\""),
      (COMMA, ","),
      (VALUE(Value::TEXT("now".into())), "\"now\""),
      (RANGLEBRACKET, ">"),
      (IDENT("address".into()), "address"),
      (ASSIGN, "="),
      (LBRACE, "{"),
      (IDENT("delivery".into()), "delivery"),
      (RBRACE, "}"),
      (IDENT("delivery".into()), "delivery"),
      (ASSIGN, "="),
      (LPAREN, "("),
      (IDENT("street".into()), "street"),
      (COLON, ":"),
      (TSTR, "tstr"),
      (COMMA, ","),
      (OPTIONAL, "?"),
      (NUMBER, "number"),
      (CUT, "^"),
      (ARROWMAP, "=>"),
      (UINT, "uint"),
      (COMMA, ","),
      (IDENT("city".into()), "city"),
      (GCHOICE, "//"),
      (IDENT("po-box".into()), "po-box"),
      (COLON, ":"),
      (UINT, "uint"),
      (COMMA, ","),
      (IDENT("city".into()), "city"),
      (GCHOICE, "//"),
      (IDENT("per-pickup".into()), "per-pickup"),
      (COLON, ":"),
      (TRUE, "true"),
      (RPAREN, ")"),
      (IDENT("city".into()), "city"),
      (ASSIGN, "="),
      (LPAREN, "("),
      (IDENT("name".into()), "name"),
      (COLON, ":"),
      (TSTR, "tstr"),
      (COMMA, ","),
      (IDENT("zip-code".into()), "zip-code"),
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
