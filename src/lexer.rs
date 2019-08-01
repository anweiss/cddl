use super::token::{self, ByteSliceValue, ByteVecValue, RangeValue, Tag, Token, Value};
use lexical_core;
use std::{
  borrow::Cow,
  convert::TryFrom,
  fmt,
  iter::Peekable,
  num, result,
  str::{self, CharIndices},
};

#[cfg(feature = "std")]
use std::error::Error;

pub type Result<T> = result::Result<T, LexerError>;

#[derive(Debug)]
pub enum LexerError {
  UTF8(str::Utf8Error),
  LEXER(&'static str),
  PARSEINT(num::ParseIntError),
}

#[cfg(feature = "std")]
impl Error for LexerError {
  fn source(&self) -> Option<&(dyn Error + 'static)> {
    match self {
      LexerError::UTF8(utf8e) => Some(utf8e),
      LexerError::PARSEINT(pie) => Some(pie),
      _ => None,
    }
  }
}

impl fmt::Display for LexerError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      LexerError::LEXER(e) => write!(f, "{}", e),
      LexerError::UTF8(e) => write!(f, "{}", e),
      LexerError::PARSEINT(e) => write!(f, "{}", e),
    }
  }
}

impl From<&'static str> for LexerError {
  fn from(e: &'static str) -> Self {
    LexerError::LEXER(e)
  }
}

impl From<str::Utf8Error> for LexerError {
  fn from(e: str::Utf8Error) -> Self {
    LexerError::UTF8(e)
  }
}

/// Lexer which holds a byte slice and iterator over the byte slice
pub struct Lexer<'a> {
  str_input: &'a [u8],
  input: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
  /// Creates a new `Lexer` from a given `&str` input
  pub fn new(input: &'a str) -> Lexer<'a> {
    Lexer {
      str_input: input.as_bytes(),
      input: input.char_indices().peekable(),
    }
  }

  fn read_char(&mut self) -> Result<(usize, char)> {
    self
      .input
      .next()
      .ok_or_else(|| "Unable to advance to the next token".into())
  }

  /// Advances the index of the str iterator over the input and returns a
  /// `Token`
  pub fn next_token(&mut self) -> Result<Token<'a>> {
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
        (idx, '#') => match self.peek_char() {
          Some(&c) if is_digit(c.1) => Ok(Token::TAG(self.read_tag()?)),
          None => Ok(Token::TAG(Tag::ANY)),
          _ => Ok(Token::ILLEGAL(
            str::from_utf8(&self.str_input[idx..=idx + 1]).map_err(LexerError::UTF8)?,
          )),
        },
        (_, '.') => {
          let (idx, _) = self.read_char()?;

          Ok(token::lookup_control(self.read_identifier(idx)?))
        }
        (idx, ch) => {
          if is_ealpha(ch) {
            if ch == 'h' {
              if let Some(&c) = self.peek_char() {
                if c.1 == '\'' {
                  let _ = self.read_char()?;
                  let (idx, _) = self.read_char()?;

                  match self.read_prefixed_byte_string(idx)? {
                    Cow::Borrowed(bs) => {
                      return Ok(Token::BYTESLICEVALUE(ByteSliceValue::B16(bs.as_bytes())))
                    }
                    Cow::Owned(bs) => {
                      return Ok(Token::BYTEVECVALUE(ByteVecValue::B16(bs.into_bytes())))
                    }
                  }
                }
              }
            }

            if ch == 'b' {
              if let Some(&c) = self.peek_char() {
                if c.1 == '6' {
                  let _ = self.read_char()?;
                  if let Some(&c) = self.peek_char() {
                    if c.1 == '4' {
                      let _ = self.read_char()?;
                      if let Some(&c) = self.peek_char() {
                        if c.1 == '\'' {
                          let _ = self.read_char()?;
                          let (idx, _) = self.read_char()?;

                          match self.read_prefixed_byte_string(idx)? {
                            Cow::Borrowed(bs) => {
                              return Ok(Token::BYTESLICEVALUE(ByteSliceValue::B64(bs.as_bytes())))
                            }
                            Cow::Owned(bs) => {
                              return Ok(Token::BYTEVECVALUE(ByteVecValue::B64(bs.into_bytes())))
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }

            let ident = token::lookup_ident(self.read_identifier(idx)?);

            // Range detected
            match self.peek_char() {
              Some(&c) if c.1 == '.' => {
                let _ = self.read_char()?;

                return self.read_range(ident);
              }
              _ => return Ok(ident),
            }
          } else if is_digit(ch) || ch == '-' {
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

          Ok(Token::ILLEGAL(
            str::from_utf8(&self.str_input[idx..=idx]).map_err(LexerError::UTF8)?,
          ))
        }
      }
    } else {
      Ok(Token::EOF)
    }
  }

  fn read_identifier(&mut self, idx: usize) -> Result<&'a str> {
    let mut end_idx = idx;

    while let Some(&c) = self.peek_char() {
      if is_ealpha(c.1) || is_digit(c.1) || c.1 == '.' || c.1 == '-' {
        match c.1 {
          // Check for range
          '.' => {
            end_idx = self.read_char()?.0;

            if let Some(&c) = self.peek_char() {
              if c.1 == '.' {
                return Ok(
                  str::from_utf8(&self.str_input[idx..end_idx]).map_err(LexerError::UTF8)?,
                );
              }
            }
          }
          _ => end_idx = self.read_char()?.0,
        }
      } else {
        break;
      }
    }
    Ok(str::from_utf8(&self.str_input[idx..=end_idx]).map_err(LexerError::UTF8)?)
  }

  fn read_text_value(&mut self, idx: usize) -> Result<&'a str> {
    while let Some(&(_, ch)) = self.peek_char() {
      match ch {
        // SCHAR
        '\x20'...'\x21' | '\x23'...'\x5b' | '\x5d'...'\x7e' | '\u{0128}'...'\u{10FFFD}' => {
          let _ = self.read_char()?;
        }
        // SESC
        '\\' => {
          let _ = self.read_char();
          if let Some(&(_, ch)) = self.peek_char() {
            match ch {
              '\x20'...'\x7e' | '\u{0128}'...'\u{10FFFD}' => {
                let _ = self.read_char()?;
              }
              _ => return Err("Unexpected escape character in text string".into()),
            }
          }
        }
        // Closing "
        '\x22' => {
          return Ok(
            str::from_utf8(&self.str_input[idx + 1..self.read_char()?.0])
              .map_err(LexerError::UTF8)?,
          )
        }
        _ => return Err("Unexpected char in text string. Expected closing \"".into()),
      }
    }

    Err("Empty text value".into())
  }

  fn read_prefixed_byte_string(&mut self, idx: usize) -> Result<Cow<'a, str>> {
    let mut has_whitespace = false;

    while let Some(&(_, ch)) = self.peek_char() {
      match ch {
        // BCHAR
        '\x20'...'\x26' | '\x28'...'\x5b' | '\x5d'...'\u{10FFFD}' => {
          let _ = self.read_char();
        }
        // SESC
        '\\' => {
          let _ = self.read_char();
          if let Some(&(_, ch)) = self.peek_char() {
            match ch {
              '\x20'...'\x7e' | '\u{0128}'...'\u{10FFFD}' => {
                let _ = self.read_char()?;
              }
              _ => return Err("Unexpected escape character in byte string".into()),
            }
          }
        }
        // Closing '
        '\x27' => {
          // Whitespace is ignored for prefixed byte strings and requires allocation
          if has_whitespace {
            return Ok(
              str::from_utf8(&self.str_input[idx..self.read_char()?.0])
                .map_err(LexerError::UTF8)?
                .replace(" ", "")
                .into(),
            );
          }

          return Ok(
            str::from_utf8(&self.str_input[idx..self.read_char()?.0])
              .map_err(LexerError::UTF8)?
              .into(),
          );
        }
        // CRLF
        _ => {
          // TODO: if user forgets closing "'", but another "'" is found later
          // in the string, the error emitted here can be confusing
          if ch.is_ascii_whitespace() {
            has_whitespace = true;
            let _ = self.read_char()?;
          } else {
            return Err("Unexpected char in byte string. Expected closing '".into());
          }
        }
      }
    }

    Err("Empty byte string".into())
  }

  fn read_comment(&mut self, idx: usize) -> Result<&'a str> {
    while let Some(&(_, ch)) = self.peek_char() {
      if ch != '\x0a' && ch != '\x0d' {
        let _ = self.read_char()?;
      } else {
        return Ok(
          str::from_utf8(&self.str_input[idx + 1..self.read_char()?.0])
            .map_err(LexerError::UTF8)?,
        );
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

  fn read_int_or_float(&mut self, mut idx: usize) -> Result<Token<'a>> {
    let mut is_signed = false;
    let mut signed_idx = 0;

    if self.str_input[idx] == b'-' {
      is_signed = true;
      signed_idx = idx;

      idx = self.read_char()?.0;
    }

    let (end_idx, i) = self.read_number(idx)?;

    if let Some(&c) = self.peek_char() {
      if c.1 == '.' {
        let _ = self.read_char()?;

        if let Some(&c) = self.peek_char() {
          if is_digit(c.1) {
            let (fraction_idx, _) = self.read_number(c.0)?;

            if is_signed {
              return Ok(Token::VALUE(Value::FLOAT(lexical_core::atof64_slice(
                &self.str_input[signed_idx..=fraction_idx],
              ))));
            }

            return Ok(Token::VALUE(Value::FLOAT(lexical_core::atof64_slice(
              &self.str_input[idx..=fraction_idx],
            ))));
          }
        }
      }
    }

    if is_signed {
      return Ok(Token::VALUE(Value::INT(
        str::from_utf8(&self.str_input[signed_idx..=end_idx])
          .map_err(LexerError::UTF8)?
          .parse()
          .map_err(LexerError::PARSEINT)?,
      )));
    }

    Ok(Token::VALUE(Value::UINT(i)))
  }

  fn read_number(&mut self, idx: usize) -> Result<(usize, usize)> {
    let mut end_index = idx;

    while let Some(&c) = self.peek_char() {
      if is_digit(c.1) {
        let (ei, _) = self.read_char()?;

        end_index = ei;
      } else {
        break;
      }
    }

    Ok((
      end_index,
      str::from_utf8(&self.str_input[idx..=end_index])
        .map_err(LexerError::UTF8)?
        .parse()
        .map_err(LexerError::PARSEINT)?,
    ))
  }

  fn peek_char(&mut self) -> Option<&(usize, char)> {
    self.input.peek()
  }

  fn read_tag(&mut self) -> Result<Tag<'a>> {
    match self.read_char() {
      Ok(c) if c.1 == '6' => {
        let (_, ch) = self.read_char()?;

        if ch == '.' {
          let (idx, _) = self.read_char()?;

          let (_, t) = self.read_number(idx)?;

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
        let mt = self.read_number(c.0)?.1 as u8;

        let (_, ch) = self.read_char()?;

        if ch == '.' {
          let (idx, _) = self.read_char()?;

          let (_, t) = self.read_number(idx)?;
          return Ok(Tag::MAJORTYPE((mt, Some(t))));
        }

        Ok(Tag::MAJORTYPE((mt, None)))
      }
      _ => Err("Malformed tag".into()),
    }
  }

  fn read_range(&mut self, lower: Token<'a>) -> Result<Token<'a>> {
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
        Token::VALUE(value) => match value {
          Value::UINT(li) => {
            if let Token::VALUE(value) = upper {
              if let Value::UINT(ui) = value {
                return Ok(Token::RANGE((
                  RangeValue::UINT(li),
                  RangeValue::UINT(ui),
                  is_inclusive,
                )));
              }
            } else {
              return Err(
                "Only numerical ranges between integers or floating point values are allowed"
                  .into(),
              );
            }
          }
          Value::FLOAT(lf) => {
            if let Token::VALUE(value) = upper {
              if let Value::FLOAT(uf) = value {
                return Ok(Token::RANGE((
                  RangeValue::FLOAT(lf),
                  RangeValue::FLOAT(uf),
                  is_inclusive,
                )));
              }
            } else {
              return Err(
                "Only numerical ranges between integers or floating point values are allowed"
                  .into(),
              );
            }
          }
          _ => {
            return Err(
              "Only numerical ranges between integers or floating point values are allowed".into(),
            )
          }
        },
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
  use super::{
    super::token::{SocketPlug, Token::*},
    *,
  };

  #[cfg(not(feature = "std"))]
  use super::super::alloc::string::ToString;

  #[test]
  fn verify_next_token() {
    let input = r#"; this is a comment
; this is another comment

mynumber = 10.5

mytag = #6.1234(tstr)
    
myfirstrule = "myotherrule"

mybase16rule = h'68656c6c6f20776f726c64'

mybase64rule = b64'aGVsbG8gd29ybGQ='

mysecondrule = mynumber..100.5

myintrule = -10

mysignedfloat = -10.5

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
      (VALUE(Value::FLOAT(10.5)), "10.5"),
      (IDENT(("mytag", None)), "mytag"),
      (ASSIGN, "="),
      (TAG(Tag::DATA((Some(1234), "tstr"))), "#6.1234(tstr)"),
      (IDENT(("myfirstrule", None)), "myfirstrule"),
      (ASSIGN, "="),
      (VALUE(Value::TEXT("myotherrule")), "\"myotherrule\""),
      (IDENT(("mybase16rule", None)), "mybase16rule"),
      (ASSIGN, "="),
      (
        BYTESLICEVALUE(ByteSliceValue::B16(b"68656c6c6f20776f726c64")),
        "h'68656c6c6f20776f726c64'",
      ),
      (IDENT(("mybase64rule", None)), "mybase64rule"),
      (ASSIGN, "="),
      (
        BYTESLICEVALUE(ByteSliceValue::B64(b"aGVsbG8gd29ybGQ=")),
        "b64'aGVsbG8gd29ybGQ='",
      ),
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
      (IDENT(("myintrule", None)), "myintrule"),
      (ASSIGN, "="),
      (VALUE(Value::INT(-10)), "-10"),
      (IDENT(("mysignedfloat", None)), "mysignedfloat"),
      (ASSIGN, "="),
      (VALUE(Value::FLOAT(-10.5)), "-10.5"),
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
      (VALUE(Value::TEXT("reboot")), "\"reboot\""),
      (COMMA, ","),
      (VALUE(Value::TEXT("now")), "\"now\""),
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
      (VALUE(Value::UINT(1)), "1"),
      (ASTERISK, "*"),
      (VALUE(Value::UINT(3)), "3"),
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
