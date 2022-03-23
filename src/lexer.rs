use super::{
  error::{
    ErrorMsg,
    MsgType::{self, *},
  },
  token::{self, ByteValue, Token, Value},
};
use codespan_reporting::{
  diagnostic::{Diagnostic, Label},
  files::SimpleFiles,
  term,
};
use std::{
  fmt,
  iter::Peekable,
  num, result,
  str::{self, CharIndices},
};

#[cfg(feature = "std")]
use std::{borrow::Cow, string};

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::Cow,
  string::{self, String, ToString},
  vec::Vec,
};
use lexical_core as lexical;

#[cfg(target_arch = "wasm32")]
use serde::Serialize;

/// Alias for `Result` with an error of type `cddl::LexerError`
pub type Result<T> = result::Result<T, Error>;

/// Lexer position
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Copy, Clone)]
pub struct Position {
  /// Line number
  pub line: usize,
  /// Column number
  pub column: usize,
  /// Token begin and end index range
  pub range: (usize, usize),
  /// Lexer index
  pub index: usize,
}

impl Default for Position {
  fn default() -> Self {
    Position {
      line: 1,
      column: 1,
      range: (0, 0),
      index: 0,
    }
  }
}

/// Lexer error
#[derive(Debug)]
pub struct Error {
  /// Error type
  pub error_type: LexerErrorType,
  input: String,
  position: Position,
}

/// Various error types emitted by the lexer
#[derive(Debug)]
pub enum LexerErrorType {
  /// CDDL lexing syntax error
  LEXER(MsgType),
  /// UTF-8 parsing error
  UTF8(string::FromUtf8Error),
  /// Byte string not properly encoded as base 16
  BASE16(String),
  /// Byte string not properly encoded as base 64
  BASE64(String),
  /// Error parsing integer
  PARSEINT(num::ParseIntError),
  /// Error parsing float
  PARSEFLOAT(lexical::Error),
  /// Error parsing hexfloat
  PARSEHEXF(hexf_parse::ParseHexfError),
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut files = SimpleFiles::new();
    let file_id = files.add("input", self.input.as_str());
    let config = term::Config::default();
    let mut buffer = Vec::new();
    let mut writer = term::termcolor::NoColor::new(&mut buffer);

    match &self.error_type {
      LexerErrorType::LEXER(le) => {
        let diagnostic = Diagnostic::error()
          .with_message("lexer error")
          .with_labels(vec![Label::primary(
            file_id,
            self.position.range.0..self.position.range.1,
          )
          .with_message(ErrorMsg::from(*le).to_string())]);

        term::emit(&mut writer, &config, &files, &diagnostic).map_err(|_| fmt::Error)?;

        write!(f, "{}", String::from_utf8(buffer).map_err(|_| fmt::Error)?)
      }
      LexerErrorType::UTF8(utf8e) => {
        let diagnostic = Diagnostic::error()
          .with_message("lexer error")
          .with_labels(vec![Label::primary(
            file_id,
            self.position.range.0..self.position.range.1,
          )
          .with_message(utf8e.to_string())]);

        term::emit(&mut writer, &config, &files, &diagnostic).map_err(|_| fmt::Error)?;

        write!(f, "{}", String::from_utf8(buffer).map_err(|_| fmt::Error)?)
      }
      LexerErrorType::BASE16(b16e) => {
        let diagnostic = Diagnostic::error()
          .with_message("lexer error")
          .with_labels(vec![Label::primary(
            file_id,
            self.position.range.0..self.position.range.1,
          )
          .with_message(b16e.to_string())]);

        term::emit(&mut writer, &config, &files, &diagnostic).map_err(|_| fmt::Error)?;

        write!(f, "{}", String::from_utf8(buffer).map_err(|_| fmt::Error)?)
      }
      LexerErrorType::BASE64(b64e) => {
        let diagnostic = Diagnostic::error()
          .with_message("lexer error")
          .with_labels(vec![Label::primary(
            file_id,
            self.position.range.0..self.position.range.1,
          )
          .with_message(b64e.to_string())]);

        term::emit(&mut writer, &config, &files, &diagnostic).map_err(|_| fmt::Error)?;

        write!(f, "{}", String::from_utf8(buffer).map_err(|_| fmt::Error)?)
      }
      LexerErrorType::PARSEINT(pie) => {
        let diagnostic = Diagnostic::error()
          .with_message("lexer error")
          .with_labels(vec![Label::primary(
            file_id,
            self.position.range.0..self.position.range.1,
          )
          .with_message(pie.to_string())]);

        term::emit(&mut writer, &config, &files, &diagnostic).map_err(|_| fmt::Error)?;

        write!(f, "{}", String::from_utf8(buffer).map_err(|_| fmt::Error)?)
      }
      LexerErrorType::PARSEFLOAT(pfe) => {
        let diagnostic = Diagnostic::error()
          .with_message("lexer error")
          .with_labels(vec![Label::primary(
            file_id,
            self.position.range.0..self.position.range.1,
          )
          .with_message(format!("{:#?}", pfe))]);

        term::emit(&mut writer, &config, &files, &diagnostic).map_err(|_| fmt::Error)?;

        write!(f, "{}", String::from_utf8(buffer).map_err(|_| fmt::Error)?)
      }
      LexerErrorType::PARSEHEXF(phf) => {
        let diagnostic = Diagnostic::error()
          .with_message("lexer error")
          .with_labels(vec![Label::primary(
            file_id,
            self.position.range.0..self.position.range.1,
          )
          .with_message(format!("{:#?}", phf))]);

        term::emit(&mut writer, &config, &files, &diagnostic).map_err(|_| fmt::Error)?;

        write!(f, "{}", String::from_utf8(buffer).map_err(|_| fmt::Error)?)
      }
    }
  }
}

impl From<(&str, Position, MsgType)> for Error {
  fn from(e: (&str, Position, MsgType)) -> Self {
    Error {
      error_type: LexerErrorType::LEXER(e.2),
      input: e.0.to_string(),
      position: e.1,
    }
  }
}

impl From<(&str, Position, string::FromUtf8Error)> for Error {
  fn from(e: (&str, Position, string::FromUtf8Error)) -> Self {
    Error {
      error_type: LexerErrorType::UTF8(e.2),
      input: e.0.to_string(),
      position: e.1,
    }
  }
}

impl From<(&str, Position, base16::DecodeError)> for Error {
  fn from(e: (&str, Position, base16::DecodeError)) -> Self {
    Error {
      error_type: LexerErrorType::BASE16(e.2.to_string()),
      input: e.0.to_string(),
      position: e.1,
    }
  }
}

impl From<(&str, Position, base64::DecodeError)> for Error {
  fn from(e: (&str, Position, base64::DecodeError)) -> Self {
    Error {
      error_type: LexerErrorType::BASE64(e.2.to_string()),
      input: e.0.to_string(),
      position: e.1,
    }
  }
}

impl From<(&str, Position, num::ParseIntError)> for Error {
  fn from(e: (&str, Position, num::ParseIntError)) -> Self {
    Error {
      error_type: LexerErrorType::PARSEINT(e.2),
      input: e.0.to_string(),
      position: e.1,
    }
  }
}

impl From<(&str, Position, lexical::Error)> for Error {
  fn from(e: (&str, Position, lexical::Error)) -> Self {
    Error {
      error_type: LexerErrorType::PARSEFLOAT(e.2),
      input: e.0.to_string(),
      position: e.1,
    }
  }
}

impl From<(&str, Position, hexf_parse::ParseHexfError)> for Error {
  fn from(e: (&str, Position, hexf_parse::ParseHexfError)) -> Self {
    Error {
      error_type: LexerErrorType::PARSEHEXF(e.2),
      input: e.0.to_string(),
      position: e.1,
    }
  }
}

/// Lexer which holds a byte slice and iterator over the byte slice
#[derive(Debug)]
pub struct Lexer<'a> {
  /// CDDL input string
  pub str_input: &'a str,
  // TODO: Remove duplicate iterator in favor of multipeek
  input: Peekable<CharIndices<'a>>,
  multipeek: itertools::MultiPeek<CharIndices<'a>>,
  /// Lexer position in input
  pub position: Position,
}

/// Iterator over a lexer
pub struct LexerIter<'a> {
  l: &'a mut Lexer<'a>,
}

/// Iterated lexer token item
pub type Item<'a> = std::result::Result<(Position, Token<'a>), Error>;

impl<'a> Iterator for LexerIter<'a> {
  type Item = Item<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    let next_token = self.l.next_token();

    Some(next_token)
  }
}

/// Creates a `Lexer` from a string slice
///
/// # Arguments
///
/// `str_input` - String slice with input
pub fn lexer_from_str(str_input: &str) -> Lexer {
  Lexer::new(str_input)
}

impl<'a> Lexer<'a> {
  /// Creates a new `Lexer` from a given `&str` input
  pub fn new(str_input: &'a str) -> Lexer<'a> {
    Lexer {
      str_input,
      input: str_input.char_indices().peekable(),
      multipeek: itertools::multipeek(str_input.char_indices()),
      position: Position {
        line: 1,
        column: 1,
        range: (0, 0),
        index: 0,
      },
    }
  }

  /// Returns an iterator over a lexer
  pub fn iter(&'a mut self) -> LexerIter<'a> {
    LexerIter { l: self }
  }

  fn read_char(&mut self) -> Result<(usize, char)> {
    self.multipeek.next();

    self
      .input
      .next()
      .map(|c| {
        if c.1 == '\n' {
          self.position.line += 1;
          self.position.column = 1;
        } else {
          self.position.column += 1;
        }

        if !c.1.is_ascii_whitespace() {
          self.position.index = c.0;
        }

        c
      })
      .ok_or_else(|| (self.str_input, self.position, UnableToAdvanceToken).into())
  }

  /// Advances the index of the str iterator over the input and returns a
  /// `Token`
  pub fn next_token(&mut self) -> Result<(Position, Token<'a>)> {
    self.skip_whitespace()?;

    let token_offset = self.position.index;

    if let Ok(c) = self.read_char() {
      match c {
        (_, '\n') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::NEWLINE))
        }
        (_, '=') => match self.peek_char() {
          Some(&c) if c.1 == '>' => {
            let _ = self.read_char()?;
            self.position.range = (token_offset, self.position.index + 1);
            Ok((self.position, Token::ARROWMAP))
          }
          _ => {
            self.position.range = (token_offset, self.position.index + 1);
            Ok((self.position, Token::ASSIGN))
          }
        },
        (_, '+') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::ONEORMORE))
        }
        (_, '?') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::OPTIONAL))
        }
        (_, '*') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::ASTERISK))
        }
        (_, '(') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::LPAREN))
        }
        (_, ')') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::RPAREN))
        }
        (_, '[') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::LBRACKET))
        }
        (_, ']') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::RBRACKET))
        }
        (_, '<') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::LANGLEBRACKET))
        }
        (idx, '"') => {
          let tv = self.read_text_value(idx)?;
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::VALUE(Value::TEXT(tv.into()))))
        }
        (_, '{') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::LBRACE))
        }
        (_, '}') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::RBRACE))
        }
        (_, ',') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::COMMA))
        }
        (idx, ';') => {
          let comment = self.read_comment(idx)?;
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::COMMENT(comment)))
        }
        (_, ':') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::COLON))
        }
        (_, '^') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::CUT))
        }
        (_, '&') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::GTOCHOICE))
        }
        (_, '>') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::RANGLEBRACKET))
        }
        (_, '~') => {
          self.position.range = (token_offset, self.position.index + 1);
          Ok((self.position, Token::UNWRAP))
        }
        (_, '/') => match self.peek_char() {
          Some(&c) if c.1 == '/' => {
            let _ = self.read_char()?;

            match self.peek_char() {
              Some(&c) if c.1 == '=' => {
                let _ = self.read_char()?;
                self.position.range = (token_offset, self.position.index + 1);
                Ok((self.position, Token::GCHOICEALT))
              }
              _ => {
                self.position.range = (token_offset, self.position.index + 1);
                Ok((self.position, Token::GCHOICE))
              }
            }
          }
          Some(&c) if c.1 == '=' => {
            let _ = self.read_char()?;
            self.position.range = (token_offset, self.position.index + 1);
            Ok((self.position, Token::TCHOICEALT))
          }
          _ => {
            self.position.range = (token_offset, self.position.index + 1);
            Ok((self.position, Token::TCHOICE))
          }
        },
        (_, '#') => match self.peek_char() {
          Some(&c) if is_digit(c.1) => {
            let (idx, _) = self.read_char()?;
            let t = self.read_number(idx)?.1;

            match self.peek_char() {
              Some(&c) if c.1 == '.' => {
                let _ = self.read_char()?;
                let (idx, _) = self.read_char()?;

                self.position.range = (token_offset, self.position.index + 1);

                Ok((
                  self.position,
                  Token::TAG(Some(t as u8), Some(self.read_number(idx)?.1)),
                ))
              }
              _ => {
                self.position.range = (token_offset, self.position.index + 1);

                Ok((self.position, Token::TAG(Some(t as u8), None)))
              }
            }
          }
          _ => {
            self.position.range = (token_offset, self.position.index + 1);
            Ok((self.position, Token::TAG(None, None)))
          }
        },
        (_, '\'') => {
          let (idx, _) = self.read_char()?;

          let bsv = self.read_byte_string(idx)?;
          self.position.range = (token_offset, self.position.index + 1);

          Ok((
            self.position,
            Token::VALUE(Value::BYTE(ByteValue::UTF8(bsv.as_bytes().into()))),
          ))
        }
        (idx, '.') => {
          if let Some(&c) = self.peek_char() {
            if c.1 == '.' {
              // Rangeop
              let _ = self.read_char()?;

              if let Some(&c) = self.peek_char() {
                if c.1 == '.' {
                  let _ = self.read_char()?;

                  self.position.range = (token_offset, self.position.index + 1);

                  return Ok((self.position, Token::RANGEOP(false)));
                }
              }

              self.position.range = (token_offset, self.position.index + 1);

              return Ok((self.position, Token::RANGEOP(true)));
            } else if is_ealpha(c.1) {
              // Controlop
              let ctrlop =
                token::lookup_control_from_str(self.read_identifier(idx)?).ok_or_else(|| {
                  self.position.range = (token_offset, self.position.index + 1);

                  Error::from((self.str_input, self.position, InvalidControlOperator))
                })?;

              self.position.range = (token_offset, self.position.index + 1);
              return Ok((self.position, ctrlop));
            }
          }

          self.position.range = (token_offset, self.position.index + 1);
          Err((self.str_input, self.position, InvalidCharacter).into())
        }
        (idx, ch) => {
          if is_ealpha(ch) {
            // base 16 (hex) encoded byte string
            if ch == 'h' {
              if let Some(&c) = self.peek_char() {
                if c.1 == '\'' {
                  let _ = self.read_char()?;
                  let (idx, _) = self.read_char()?;

                  // Ensure that the byte string has been properly encoded.
                  let b = self.read_prefixed_byte_string(idx)?;
                  let mut buf = [0u8; 1024];
                  return base16::decode_slice(&b[..], &mut buf)
                    .map_err(|e| (self.str_input, self.position, e).into())
                    .map(|_| {
                      self.position.range = (token_offset, self.position.index + 1);

                      (self.position, Token::VALUE(Value::BYTE(ByteValue::B16(b))))
                    });
                }
              }
            }

            // base 64 encoded byte string
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

                          // Ensure that the byte string has been properly
                          // encoded
                          let bs = self.read_prefixed_byte_string(idx)?;
                          let mut buf = [0u8; 1024];
                          return base64::decode_config_slice(&bs, base64::URL_SAFE, &mut buf)
                            .map_err(|e| (self.str_input, self.position, e).into())
                            .map(|_| {
                              self.position.range = (token_offset, self.position.index + 1);

                              (self.position, Token::VALUE(Value::BYTE(ByteValue::B64(bs))))
                            });
                        }
                      }
                    }
                  }
                }
              }
            }

            let ident = token::lookup_ident(self.read_identifier(idx)?);

            self.position.range = (token_offset, self.position.index + 1);

            return Ok((self.position, ident));
          } else if is_digit(ch) || ch == '-' {
            let number = self.read_int_or_float(idx)?;

            self.position.range = (token_offset, self.position.index + 1);

            return Ok((self.position, number));
          }

          self.position.range = (token_offset, self.position.index + 1);

          Ok((self.position, Token::ILLEGAL(&self.str_input[idx..=idx])))
        }
      }
    } else {
      self.position.range = (token_offset, self.position.index + 1);
      Ok((self.position, Token::EOF))
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
              if c.1 == '\u{0020}' {
                return Ok(&self.str_input[idx..end_idx]);
              }
            }
          }
          _ => end_idx = self.read_char()?.0,
        }
      } else {
        break;
      }
    }
    Ok(&self.str_input[idx..=end_idx])
  }

  fn read_text_value(&mut self, idx: usize) -> Result<&'a str> {
    while let Some(&(_, ch)) = self.peek_char() {
      match ch {
        // SCHAR
        '\x20'..='\x21' | '\x23'..='\x5b' | '\x5d'..='\x7e' | '\u{0080}'..='\u{10FFFD}' => {
          let _ = self.read_char()?;
        }
        // SESC
        '\\' => {
          let _ = self.read_char();
          if let Some(&(_, ch)) = self.peek_char() {
            match ch {
              '\x20'..='\x7e' | '\u{0080}'..='\u{10FFFD}' => {
                let _ = self.read_char()?;
              }
              _ => return Err((self.str_input, self.position, InvalidEscapeCharacter).into()),
            }
          }
        }
        // Closing "
        '\x22' => {
          return Ok(&self.str_input[idx + 1..self.read_char()?.0]);
        }
        _ => {
          return Err(
            (
              self.str_input,
              self.position,
              InvalidTextStringLiteralCharacter,
            )
              .into(),
          )
        }
      }
    }

    Err((self.str_input, self.position, EmptyTextStringLiteral).into())
  }

  fn read_byte_string(&mut self, idx: usize) -> Result<&'a str> {
    while let Some(&(_, ch)) = self.peek_char() {
      match ch {
        // BCHAR
        '\x20'..='\x26' | '\x28'..='\x5b' | '\x5d'..='\x7e' | '\u{0080}'..='\u{10FFFD}' => {
          let _ = self.read_char();
        }
        // SESC
        '\\' => {
          let _ = self.read_char();
          if let Some(&(_, ch)) = self.peek_char() {
            match ch {
              '\x20'..='\x7e' | '\u{0080}'..='\u{10FFFD}' => {
                let _ = self.read_char()?;
              }
              _ => return Err((self.str_input, self.position, InvalidEscapeCharacter).into()),
            }
          }
        }
        // Closing '
        '\x27' => return Ok(&self.str_input[idx..self.read_char()?.0]),
        _ => {
          if ch.is_ascii_whitespace() {
            let _ = self.read_char()?;
          } else {
            return Err(
              (
                self.str_input,
                self.position,
                InvalidByteStringLiteralCharacter,
              )
                .into(),
            );
          }
        }
      }
    }

    Err((self.str_input, self.position, EmptyByteStringLiteral).into())
  }

  fn read_prefixed_byte_string(&mut self, idx: usize) -> Result<Cow<'a, [u8]>> {
    let mut has_whitespace = false;

    while let Some(&(_, ch)) = self.peek_char() {
      match ch {
        // BCHAR
        '\x20'..='\x26' | '\x28'..='\x5b' | '\x5d'..='\x7e' | '\u{0080}'..='\u{10FFFD}' => {
          let _ = self.read_char();
        }
        // SESC
        '\\' => {
          let _ = self.read_char();
          if let Some(&(_, ch)) = self.peek_char() {
            match ch {
              '\x20'..='\x7e' | '\u{0080}'..='\u{10FFFD}' => {
                let _ = self.read_char()?;
              }
              _ => return Err((self.str_input, self.position, InvalidEscapeCharacter).into()),
            }
          }
        }
        // Closing '
        '\x27' => {
          // Whitespace is ignored for prefixed byte strings and requires allocation
          if has_whitespace {
            return Ok(
              self.str_input[idx..self.read_char()?.0]
                .to_string()
                .replace(' ', "")
                .into_bytes()
                .into(),
            );
          }

          return Ok(self.str_input[idx..self.read_char()?.0].as_bytes().into());
        }
        // CRLF
        _ => {
          // TODO: if user forgets closing "'", but another "'" is found later
          // in the string, the error emitted here can be confusing
          if ch.is_ascii_whitespace() {
            has_whitespace = true;
            let _ = self.read_char()?;
          } else {
            return Err(
              (
                self.str_input,
                self.position,
                InvalidByteStringLiteralCharacter,
              )
                .into(),
            );
          }
        }
      }
    }

    Err((self.str_input, self.position, EmptyByteStringLiteral).into())
  }

  fn read_comment(&mut self, idx: usize) -> Result<&'a str> {
    let mut comment_char = (idx, char::default());

    while let Some(&(_, ch)) = self.peek_char() {
      if ch != '\x0a' && ch != '\x0d' {
        comment_char = self.read_char()?;
      } else {
        return Ok(&self.str_input[idx + 1..self.read_char()?.0]);
      }
    }

    Ok(&self.str_input[idx + 1..=comment_char.0])
  }

  fn skip_whitespace(&mut self) -> Result<()> {
    while let Some(&(idx, ch)) = self.peek_char() {
      if ch == '\n' {
        self.position.index = idx;
        return Ok(());
      }

      if ch.is_whitespace() {
        let _ = self.read_char()?;
      } else {
        self.position.index = idx;
        break;
      }
    }

    Ok(())
  }

  fn read_int_or_float(&mut self, mut idx: usize) -> Result<Token<'a>> {
    let mut is_signed = false;
    let mut signed_idx = 0;

    if self.str_input.as_bytes()[idx] == b'-' {
      is_signed = true;
      signed_idx = idx;

      idx = self.read_char()?.0;
    }

    let (mut end_idx, i) = self.read_number(idx)?;

    if let Some(&c) = self.multipeek.peek() {
      let mut hexfloat = false;

      if i == 0 && c.0 - idx == 1 && c.1 == 'x' {
        let _ = self.read_char()?;
        if self.multipeek.peek().is_none() {
          return Err((self.str_input, self.position, InvalidHexFloat).into());
        }

        let (idx, _) = self.read_char()?;
        let _ = self.read_hexdigit(idx)?;
        hexfloat = true;
      }

      if c.1 == '.' || c.1 == 'x' {
        if c.1 == 'x' {
          let _ = self.read_char()?;
        }

        if let Some(&c) = self.multipeek.peek() {
          if hexfloat && is_hexdigit(c.1) {
            let _ = self.read_char()?;
            let _ = self.read_hexdigit(c.0)?;
            if self.read_char()?.1 != 'p' {
              return Err((self.str_input, self.position, InvalidHexFloat).into());
            }

            let (exponent_idx, _) = self.read_char()?;
            end_idx = self.read_exponent(exponent_idx)?.0;

            if is_signed {
              return Ok(Token::VALUE(Value::FLOAT(
                hexf_parse::parse_hexf64(&self.str_input[signed_idx..=end_idx], false)
                  .map_err(|e| Error::from((self.str_input, self.position, e)))?,
              )));
            }

            return Ok(Token::VALUE(Value::FLOAT(
              hexf_parse::parse_hexf64(&self.str_input[idx..=end_idx], false)
                .map_err(|e| Error::from((self.str_input, self.position, e)))?,
            )));
          }

          if is_digit(c.1) {
            let _ = self.read_char()?;
            end_idx = self.read_number(c.0)?.0;

            if let Some(&(_, 'e')) = self.peek_char() {
              let _ = self.read_char()?;
              let (exponent_idx, _) = self.read_char()?;
              end_idx = self.read_exponent(exponent_idx)?.0;
            }

            if is_signed {
              return Ok(Token::VALUE(Value::FLOAT(
                lexical::parse::<f64>(self.str_input[signed_idx..=end_idx].as_bytes())
                  .map_err(|e| Error::from((self.str_input, self.position, e)))?,
              )));
            }

            return Ok(Token::VALUE(Value::FLOAT(
              lexical::parse::<f64>(self.str_input[idx..=end_idx].as_bytes())
                .map_err(|e| Error::from((self.str_input, self.position, e)))?,
            )));
          }
        }
      }
    }

    let mut is_exponent = false;
    if let Some(&(_, 'e')) = self.peek_char() {
      let _ = self.read_char()?;
      let (exponent_idx, _) = self.read_char()?;

      end_idx = self.read_exponent(exponent_idx)?.0;
      is_exponent = true;
    }

    if is_signed {
      if is_exponent {
        return Ok(Token::VALUE(Value::INT(
          lexical::parse::<f64>(self.str_input[signed_idx..=end_idx].as_bytes())
            .map_err(|e| Error::from((self.str_input, self.position, e)))? as isize,
        )));
      } else {
        return Ok(Token::VALUE(Value::INT(
          self.str_input[signed_idx..=end_idx]
            .parse()
            .map_err(|e| Error::from((self.str_input, self.position, e)))?,
        )));
      }
    }

    if is_exponent {
      return Ok(Token::VALUE(Value::UINT(
        lexical::parse::<f64>(self.str_input[idx..=end_idx].as_bytes())
          .map_err(|e| Error::from((self.str_input, self.position, e)))? as usize,
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
      self.str_input[idx..=end_index]
        .parse()
        .map_err(|e| Error::from((self.str_input, self.position, e)))?,
    ))
  }

  fn read_exponent(&mut self, idx: usize) -> Result<(usize, &str)> {
    let mut end_index = idx;

    if let Some(&c) = self.peek_char() {
      if c.1 != '-' && c.1 != '+' && !is_digit(c.1) {
        return Err((self.str_input, self.position, InvalidExponent).into());
      }
    }

    while let Some(&c) = self.peek_char() {
      if is_digit(c.1) {
        let (ei, _) = self.read_char()?;

        end_index = ei;
      } else {
        break;
      }
    }

    Ok((end_index, &self.str_input[idx..=end_index]))
  }

  fn read_hexdigit(&mut self, idx: usize) -> Result<(usize, &str)> {
    let mut end_index = idx;

    while let Some(&c) = self.peek_char() {
      if is_hexdigit(c.1) {
        let (ei, _) = self.read_char()?;

        end_index = ei;
      } else {
        break;
      }
    }

    Ok((end_index, &self.str_input[idx..=end_index]))
  }

  fn peek_char(&mut self) -> Option<&(usize, char)> {
    self.input.peek()
  }
}

fn is_ealpha(ch: char) -> bool {
  ch.is_alphabetic() || ch == '@' || ch == '_' || ch == '$'
}

fn is_digit(ch: char) -> bool {
  ch.is_digit(10)
}

fn is_hexdigit(ch: char) -> bool {
  ch.is_digit(16)
}

#[cfg(test)]
mod tests {
  use super::{
    super::token::{SocketPlug, Token::*},
    *,
  };
  use pretty_assertions::assert_eq;

  #[cfg(not(feature = "std"))]
  use super::super::alloc::string::ToString;
  use indoc::indoc;

  #[test]
  fn verify_next_token() -> Result<()> {
    let input = indoc!(
      r#"
        ; this is a comment
        ; this is another comment

        mynumber = 10.5

        mytag = #6.1234(tstr)
            
        myfirstrule = "myotherrule"

        mybytestring = 'hello there'

        mybase16rule = h'68656c6c6f20776f726c64'

        mybase64rule = b64'aGVsbG8gd29ybGQ='

        mysecondrule = mynumber .. 100.5

        myintrule = -10

        mysignedfloat = -10.5

        myintrange = -10..10

        mycontrol = mynumber .gt 0

        @terminal-color = basecolors / othercolors ; an inline comment
            
        messages = message<"reboot", "now">

        address = { delivery }

        delivery = (
          street: tstr, ? number ^ => uint, city //
          po-box: uint, city //
          per-pickup: true
        )

        city = (
          name: tstr
          zip-code: uint
          1*3 $$tcp-option,
        ) ; test"#
    );

    let expected_tok = [
      (COMMENT(" this is a comment".into()), "; this is a comment"),
      (
        COMMENT(" this is another comment".into()),
        "; this is another comment",
      ),
      (NEWLINE, ""),
      (IDENT("mynumber".into(), None), "mynumber"),
      (ASSIGN, "="),
      (VALUE(Value::FLOAT(10.5)), "10.5"),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("mytag".into(), None), "mytag"),
      (ASSIGN, "="),
      (TAG(Some(6), Some(1234)), "#6.1234"),
      (LPAREN, "("),
      (TSTR, "tstr"),
      (RPAREN, ")"),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("myfirstrule".into(), None), "myfirstrule"),
      (ASSIGN, "="),
      (VALUE(Value::TEXT("myotherrule".into())), "\"myotherrule\""),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("mybytestring".into(), None), "mybytestring"),
      (ASSIGN, "="),
      (
        VALUE(Value::BYTE(ByteValue::UTF8(b"hello there".as_ref().into()))),
        "'hello there'",
      ),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("mybase16rule".into(), None), "mybase16rule"),
      (ASSIGN, "="),
      (
        VALUE(Value::BYTE(ByteValue::B16(
          b"68656c6c6f20776f726c64".as_ref().into(),
        ))),
        "h'68656c6c6f20776f726c64'",
      ),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("mybase64rule".into(), None), "mybase64rule"),
      (ASSIGN, "="),
      (
        VALUE(Value::BYTE(ByteValue::B64(
          b"aGVsbG8gd29ybGQ=".as_ref().into(),
        ))),
        "b64'aGVsbG8gd29ybGQ='",
      ),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("mysecondrule".into(), None), "mysecondrule"),
      (ASSIGN, "="),
      (IDENT("mynumber".into(), None), "mynumber"),
      (RANGEOP(true), ".."),
      (VALUE(Value::FLOAT(100.5)), "100.5"),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("myintrule".into(), None), "myintrule"),
      (ASSIGN, "="),
      (VALUE(Value::INT(-10)), "-10"),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("mysignedfloat".into(), None), "mysignedfloat"),
      (ASSIGN, "="),
      (VALUE(Value::FLOAT(-10.5)), "-10.5"),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("myintrange".into(), None), "myintrange"),
      (ASSIGN, "="),
      (VALUE(Value::INT(-10)), "-10"),
      (RANGEOP(true), ".."),
      (VALUE(Value::UINT(10)), "10"),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("mycontrol".into(), None), "mycontrol"),
      (ASSIGN, "="),
      (IDENT("mynumber".into(), None), "mynumber"),
      (GT, ".gt"),
      (VALUE(Value::UINT(0)), "0"),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("@terminal-color".into(), None), "@terminal-color"),
      (ASSIGN, "="),
      (IDENT("basecolors".into(), None), "basecolors"),
      (TCHOICE, "/"),
      (IDENT("othercolors".into(), None), "othercolors"),
      (COMMENT(" an inline comment".into()), "; an inline comment"),
      (NEWLINE, ""),
      (IDENT("messages".into(), None), "messages"),
      (ASSIGN, "="),
      (IDENT("message".into(), None), "message"),
      (LANGLEBRACKET, "<"),
      (VALUE(Value::TEXT("reboot".into())), "\"reboot\""),
      (COMMA, ","),
      (VALUE(Value::TEXT("now".into())), "\"now\""),
      (RANGLEBRACKET, ">"),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("address".into(), None), "address"),
      (ASSIGN, "="),
      (LBRACE, "{"),
      (IDENT("delivery".into(), None), "delivery"),
      (RBRACE, "}"),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("delivery".into(), None), "delivery"),
      (ASSIGN, "="),
      (LPAREN, "("),
      (NEWLINE, ""),
      (IDENT("street".into(), None), "street"),
      (COLON, ":"),
      (TSTR, "tstr"),
      (COMMA, ","),
      (OPTIONAL, "?"),
      (NUMBER, "number"),
      (CUT, "^"),
      (ARROWMAP, "=>"),
      (UINT, "uint"),
      (COMMA, ","),
      (IDENT("city".into(), None), "city"),
      (GCHOICE, "//"),
      (NEWLINE, ""),
      (IDENT("po-box".into(), None), "po-box"),
      (COLON, ":"),
      (UINT, "uint"),
      (COMMA, ","),
      (IDENT("city".into(), None), "city"),
      (GCHOICE, "//"),
      (NEWLINE, ""),
      (IDENT("per-pickup".into(), None), "per-pickup"),
      (COLON, ":"),
      (TRUE, "true"),
      (NEWLINE, ""),
      (RPAREN, ")"),
      (NEWLINE, ""),
      (NEWLINE, ""),
      (IDENT("city".into(), None), "city"),
      (ASSIGN, "="),
      (LPAREN, "("),
      (NEWLINE, ""),
      (IDENT("name".into(), None), "name"),
      (COLON, ":"),
      (TSTR, "tstr"),
      (NEWLINE, ""),
      (IDENT("zip-code".into(), None), "zip-code"),
      (COLON, ":"),
      (UINT, "uint"),
      (NEWLINE, ""),
      (VALUE(Value::UINT(1)), "1"),
      (ASTERISK, "*"),
      (VALUE(Value::UINT(3)), "3"),
      (
        IDENT("tcp-option".into(), Some(SocketPlug::GROUP)),
        "$$tcp-option",
      ),
      (COMMA, ","),
      (NEWLINE, ""),
      (RPAREN, ")"),
      (COMMENT(" test".into()), "; test"),
    ];

    let mut l = Lexer::new(input);

    for (expected_tok, literal) in expected_tok.iter() {
      let tok = l.next_token()?;
      assert_eq!((&tok.1, &*tok.1.to_string()), (expected_tok, *literal))
    }

    Ok(())
  }

  #[test]
  fn verify_controlop() -> Result<()> {
    let input = r#".size"#;
    let expected_tok = Token::SIZE;

    let mut l = Lexer::new(input);

    assert_eq!(expected_tok.to_string(), l.next_token()?.1.to_string());

    Ok(())
  }

  #[test]
  fn verify_range() -> Result<()> {
    let input = r#"-10.5..10.5"#;

    let mut l = Lexer::new(input);

    let expected_tokens = [
      (VALUE(Value::FLOAT(-10.5)), "-10.5"),
      (RANGEOP(true), ".."),
      (VALUE(Value::FLOAT(10.5)), "10.5"),
    ];

    for (expected_tok, literal) in expected_tokens.iter() {
      let tok = l.next_token()?;
      assert_eq!((expected_tok, *literal), (&tok.1, &*tok.1.to_string()))
    }

    Ok(())
  }

  #[test]
  fn verify_multiline_byte_string() -> Result<()> {
    let input = r#"'test
      test'"#;

    let mut l = Lexer::new(input);
    let tok = l.next_token()?;

    assert_eq!(
      (
        &VALUE(Value::BYTE(ByteValue::UTF8(Cow::Borrowed(
          b"test\n      test"
        )))),
        "'test\n      test'"
      ),
      (&tok.1, &*tok.1.to_string())
    );

    Ok(())
  }

  #[test]
  fn verify_hexfloat() -> Result<()> {
    let input = r#"0x1.999999999999ap-4"#;

    let mut l = Lexer::new(input);
    let tok = l.next_token()?;
    assert_eq!(
      (&VALUE(Value::FLOAT(0.1)), "0.1"),
      (&tok.1, &*tok.1.to_string())
    );

    Ok(())
  }

  #[test]
  fn verify_exponent() -> Result<()> {
    let input = r#"-100.7e-1"#;

    let mut l = Lexer::new(input);
    let tok = l.next_token()?;
    assert_eq!(
      (&VALUE(Value::FLOAT(-10.07)), "-10.07"),
      (&tok.1, &*tok.1.to_string())
    );

    Ok(())
  }

  #[test]
  fn verify_lexer_diagnostic() -> Result<()> {
    let input = r#"myrule = number .asdf 10"#;

    let mut l = Lexer::new(input);

    l.next_token()?;
    l.next_token()?;
    l.next_token()?;

    match l.next_token() {
      Ok(_) => Ok(()),
      Err(e) => {
        #[cfg(feature = "std")]
        println!("{}", e);

        assert_eq!(
          e.to_string(),
          indoc!(
            r#"
              error: lexer error
                ┌─ input:1:17
                │
              1 │ myrule = number .asdf 10
                │                 ^^^^^ invalid control operator

            "#
          )
        );

        Ok(())
      }
    }
  }
}
