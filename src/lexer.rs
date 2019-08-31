use super::token::{self, ByteSliceValue, ByteVecValue, RangeValue, Tag, Token, Value};
use annotate_snippets::{
  display_list::DisplayList,
  formatter::DisplayListFormatter,
  snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use base16;
use base64;
use itertools;
use lexical_core;
use std::{
  convert::TryFrom,
  fmt,
  iter::Peekable,
  num, result,
  str::{self, CharIndices},
};

#[cfg(feature = "std")]
use std::{borrow::Cow, error::Error};

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::{Cow, ToOwned},
  string::{String, ToString},
  vec::Vec,
};

/// Alias for `Result` with an error of type `cddl::LexerError`
pub type Result<T> = result::Result<T, LexerError>;

/// Lexer position
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

/// Lexer error
#[derive(Debug)]
pub struct LexerError {
  error_type: LexerErrorType,
  input: Vec<u8>,
  position: Position,
}

/// Various error types emitted by the lexer
#[derive(Debug)]
pub enum LexerErrorType {
  /// CDDL lexing syntax error
  LEXER(&'static str),
  /// UTF-8 parsing error
  UTF8(str::Utf8Error),
  /// Byte string not properly encoded as base 16
  BASE16(base16::DecodeError),
  /// Byte string not properly encoded as base 64
  BASE64(base64::DecodeError),
  /// Error parsing integer
  PARSEINT(num::ParseIntError),
}

#[cfg(feature = "std")]
impl Error for LexerError {
  fn source(&self) -> Option<&(dyn Error + 'static)> {
    match &self.error_type {
      LexerErrorType::UTF8(utf8e) => Some(utf8e),
      LexerErrorType::BASE16(b16e) => Some(b16e),
      LexerErrorType::BASE64(b64e) => Some(b64e),
      LexerErrorType::PARSEINT(pie) => Some(pie),
      _ => None,
    }
  }
}

impl fmt::Display for LexerError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let dlf = DisplayListFormatter::new(false, false);

    match &self.error_type {
      LexerErrorType::LEXER(le) => write!(
        f,
        "{}",
        dlf.format(&DisplayList::from(Snippet {
          title: Some(Annotation {
            label: Some(le.to_string()),
            id: None,
            annotation_type: AnnotationType::Error,
          }),
          footer: vec![],
          slices: vec![Slice {
            source: String::from_utf8(self.input.clone()).map_err(|_| fmt::Error)?,
            line_start: self.position.line,
            origin: Some("input".to_string()),
            fold: false,
            annotations: vec![SourceAnnotation {
              range: self.position.range,
              label: le.to_string(),
              annotation_type: AnnotationType::Error,
            }],
          }],
        }))
      ),
      LexerErrorType::UTF8(utf8e) => write!(
        f,
        "{}",
        dlf.format(&DisplayList::from(Snippet {
          title: Some(Annotation {
            label: Some(utf8e.to_string()),
            id: None,
            annotation_type: AnnotationType::Error,
          }),
          footer: vec![],
          slices: vec![Slice {
            source: String::from_utf8(self.input.clone()).map_err(|_| fmt::Error)?,
            line_start: self.position.line,
            origin: Some("input".to_string()),
            fold: false,
            annotations: vec![SourceAnnotation {
              range: self.position.range,
              label: utf8e.to_string(),
              annotation_type: AnnotationType::Error,
            }],
          }],
        }))
      ),
      LexerErrorType::BASE16(b16e) => write!(
        f,
        "{}",
        dlf.format(&DisplayList::from(Snippet {
          title: Some(Annotation {
            label: Some(b16e.to_string()),
            id: None,
            annotation_type: AnnotationType::Error,
          }),
          footer: vec![],
          slices: vec![Slice {
            source: String::from_utf8(self.input.clone()).map_err(|_| fmt::Error)?,
            line_start: self.position.line,
            origin: Some("input".to_string()),
            fold: false,
            annotations: vec![SourceAnnotation {
              range: self.position.range,
              label: b16e.to_string(),
              annotation_type: AnnotationType::Error,
            }],
          }],
        }))
      ),
      LexerErrorType::BASE64(b64e) => write!(
        f,
        "{}",
        dlf.format(&DisplayList::from(Snippet {
          title: Some(Annotation {
            label: Some(b64e.to_string()),
            id: None,
            annotation_type: AnnotationType::Error,
          }),
          footer: vec![],
          slices: vec![Slice {
            source: String::from_utf8(self.input.clone()).map_err(|_| fmt::Error)?,
            line_start: self.position.line,
            origin: Some("input".to_string()),
            fold: false,
            annotations: vec![SourceAnnotation {
              range: self.position.range,
              label: b64e.to_string(),
              annotation_type: AnnotationType::Error,
            }],
          }],
        }))
      ),
      LexerErrorType::PARSEINT(pie) => write!(
        f,
        "{}",
        dlf.format(&DisplayList::from(Snippet {
          title: Some(Annotation {
            label: Some(pie.to_string()),
            id: None,
            annotation_type: AnnotationType::Error,
          }),
          footer: vec![],
          slices: vec![Slice {
            source: String::from_utf8(self.input.clone()).map_err(|_| fmt::Error)?,
            line_start: self.position.line,
            origin: Some("input".to_string()),
            fold: false,
            annotations: vec![SourceAnnotation {
              range: self.position.range,
              label: pie.to_string(),
              annotation_type: AnnotationType::Error,
            }],
          }],
        }))
      ),
    }
  }
}

impl<'a> From<(&'a [u8], Position, &'static str)> for LexerError {
  fn from(e: (&'a [u8], Position, &'static str)) -> Self {
    LexerError {
      error_type: LexerErrorType::LEXER(e.2),
      input: e.0.to_owned(),
      position: e.1,
    }
  }
}

impl<'a> From<(&'a [u8], Position, str::Utf8Error)> for LexerError {
  fn from(e: (&'a [u8], Position, str::Utf8Error)) -> Self {
    LexerError {
      error_type: LexerErrorType::UTF8(e.2),
      input: e.0.to_owned(),
      position: e.1,
    }
  }
}

impl<'a> From<(&'a [u8], Position, base16::DecodeError)> for LexerError {
  fn from(e: (&'a [u8], Position, base16::DecodeError)) -> Self {
    LexerError {
      error_type: LexerErrorType::BASE16(e.2),
      input: e.0.to_owned(),
      position: e.1,
    }
  }
}

impl<'a> From<(&'a [u8], Position, base64::DecodeError)> for LexerError {
  fn from(e: (&'a [u8], Position, base64::DecodeError)) -> Self {
    LexerError {
      error_type: LexerErrorType::BASE64(e.2),
      input: e.0.to_owned(),
      position: e.1,
    }
  }
}

impl<'a> From<(&'a [u8], Position, num::ParseIntError)> for LexerError {
  fn from(e: (&'a [u8], Position, num::ParseIntError)) -> Self {
    LexerError {
      error_type: LexerErrorType::PARSEINT(e.2),
      input: e.0.to_owned(),
      position: e.1,
    }
  }
}

/// Lexer which holds a byte slice and iterator over the byte slice
pub struct Lexer<'a> {
  str_input: &'a [u8],
  // TODO: Remove duplicate iterator in favor of multipeek
  input: Peekable<CharIndices<'a>>,
  multipeek: itertools::MultiPeek<CharIndices<'a>>,
  position: Position,
}

impl<'a> Lexer<'a> {
  /// Creates a new `Lexer` from a given `&str` input
  pub fn new(input: &'a str) -> Lexer<'a> {
    Lexer {
      str_input: input.as_bytes(),
      input: input.char_indices().peekable(),
      multipeek: itertools::multipeek(input.char_indices()),
      position: Position {
        line: 1,
        column: 1,
        range: (0, 0),
        index: 0,
      },
    }
  }

  fn read_char(&mut self) -> Result<(usize, char)> {
    self.multipeek.next();

    self
      .input
      .next()
      .and_then(|c| {
        if c.1 == '\n' {
          self.position.line += 1;
          self.position.column = 1;
        } else {
          self.position.column += 1;
        }

        self.position.index = c.0;
        Some(c)
      })
      .ok_or_else(|| {
        (
          self.str_input,
          self.position,
          "Unable to advance to the next token",
        )
          .into()
      })
  }

  /// Advances the index of the str iterator over the input and returns a
  /// `Token`
  pub fn next_token(&mut self) -> Result<(Position, Token<'a>)> {
    self.skip_whitespace();

    self.position.range.0 = self.position.index + 1;
    self.position.range.1 = self.position.index + 1;

    if let Ok(c) = self.read_char() {
      match c {
        (_, '=') => match self.peek_char() {
          Some(&c) if c.1 == '>' => {
            let _ = self.read_char()?;
            self.position.range.1 = self.position.index;
            Ok((self.position, Token::ARROWMAP))
          }
          _ => {
            self.position.range.1 = self.position.index;
            Ok((self.position, Token::ASSIGN))
          }
        },
        (_, '+') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::ONEORMORE))
        }
        (_, '?') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::OPTIONAL))
        }
        (_, '*') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::ASTERISK))
        }
        (_, '(') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::LPAREN))
        }
        (_, ')') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::RPAREN))
        }
        (_, '[') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::LBRACKET))
        }
        (_, ']') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::RBRACKET))
        }
        (_, '<') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::LANGLEBRACKET))
        }
        (idx, '"') => {
          let tv = self.read_text_value(idx)?;
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::VALUE(Value::TEXT(tv))))
        }
        (_, '{') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::LBRACE))
        }
        (_, '}') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::RBRACE))
        }
        (_, ',') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::COMMA))
        }
        (idx, ';') => {
          let comment = self.read_comment(idx)?;
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::COMMENT(comment)))
        }
        (_, ':') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::COLON))
        }
        (_, '^') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::CUT))
        }
        (_, '&') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::GTOCHOICE))
        }
        (_, '>') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::RANGLEBRACKET))
        }
        (_, '~') => {
          self.position.range.1 = self.position.index;
          Ok((self.position, Token::UNWRAP))
        }
        (_, '/') => match self.peek_char() {
          Some(&c) if c.1 == '/' => {
            let _ = self.read_char()?;

            match self.peek_char() {
              Some(&c) if c.1 == '=' => {
                let _ = self.read_char()?;
                self.position.range.1 = self.position.index;
                Ok((self.position, Token::GCHOICEALT))
              }
              _ => {
                self.position.range.1 = self.position.index;
                Ok((self.position, Token::GCHOICE))
              }
            }
          }
          Some(&c) if c.1 == '=' => {
            let _ = self.read_char()?;
            self.position.range.1 = self.position.index;
            Ok((self.position, Token::TCHOICEALT))
          }
          _ => {
            self.position.range.1 = self.position.index;
            Ok((self.position, Token::TCHOICE))
          }
        },
        (idx, '#') => match self.peek_char() {
          Some(&c) if is_digit(c.1) => {
            let tag = self.read_tag()?;
            self.position.range.1 = self.position.index;
            Ok((self.position, Token::TAG(tag)))
          }
          None => {
            self.position.range.1 = self.position.index;
            Ok((self.position, Token::TAG(Tag::ANY)))
          }
          _ => {
            self.position.range.1 = self.position.index;
            Ok((
              self.position,
              Token::ILLEGAL(
                str::from_utf8(&self.str_input[idx..=idx + 1])
                  .map_err(|e| (self.str_input, self.position, e))?,
              ),
            ))
          }
        },
        (_, '\'') => {
          let (idx, _) = self.read_char()?;

          let bsv = self.read_byte_string(idx)?;
          self.position.range.1 = self.position.index;

          Ok((
            self.position,
            Token::BYTESLICEVALUE(ByteSliceValue::UTF8(bsv)),
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

                  self.position.range.1 = self.position.index;

                  return Ok((self.position, Token::RANGEOP(false)));
                }
              }

              self.position.range.1 = self.position.index;

              return Ok((self.position, Token::RANGEOP(true)));
            } else if is_ealpha(c.1) {
              // Controlop
              let ctrlop =
                token::lookup_control_from_str(self.read_identifier(idx)?).ok_or_else(|| {
                  self.position.range.1 = self.position.index + 1;

                  LexerError::from((self.str_input, self.position, "Invalid control operator"))
                })?;

              self.position.range.1 = self.position.index;
              return Ok((self.position, ctrlop));
            }
          }

          self.position.range.1 = idx;
          Err((self.str_input, self.position, "Invalid character").into())
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
                  match self.read_prefixed_byte_string(idx)? {
                    Cow::Borrowed(bs) => {
                      let mut buf = [0u8; 1024];
                      return base16::decode_slice(&bs[..], &mut buf)
                        .map_err(|e| (self.str_input, self.position, e).into())
                        .and_then(|_| {
                          self.position.range.1 = self.position.index;

                          Ok((
                            self.position,
                            Token::BYTESLICEVALUE(ByteSliceValue::B16(bs.as_bytes())),
                          ))
                        });
                    }
                    Cow::Owned(bs) => {
                      let b = bs.as_bytes();
                      return base16::decode(b)
                        .map_err(|e| (self.str_input, self.position, e).into())
                        .and_then(|_| {
                          self.position.range.1 = self.position.index;

                          Ok((
                            self.position,
                            Token::BYTEVECVALUE(ByteVecValue::B16(b.to_owned())),
                          ))
                        });
                    }
                  }
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
                          match self.read_prefixed_byte_string(idx)? {
                            Cow::Borrowed(bs) => {
                              return base64::decode_config(bs, base64::URL_SAFE)
                                .map_err(|e| (self.str_input, self.position, e).into())
                                .and_then(|_| {
                                  self.position.range.1 = self.position.index;

                                  Ok((
                                    self.position,
                                    Token::BYTESLICEVALUE(ByteSliceValue::B64(bs.as_bytes())),
                                  ))
                                });
                            }
                            Cow::Owned(bs) => {
                              return base64::decode_config(&bs, base64::URL_SAFE)
                                .map_err(|e| (self.str_input, self.position, e).into())
                                .and_then(|_| {
                                  self.position.range.1 = self.position.index;

                                  Ok((
                                    self.position,
                                    Token::BYTEVECVALUE(ByteVecValue::B64(bs.into_bytes())),
                                  ))
                                });
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

            self.position.range.1 = self.position.index;

            return Ok((self.position, ident));
          } else if is_digit(ch) || ch == '-' {
            let number = self.read_int_or_float(idx)?;

            self.position.range.1 = self.position.index;

            return Ok((self.position, number));
          }

          self.position.range.1 = self.position.index;

          Ok((
            self.position,
            Token::ILLEGAL(
              str::from_utf8(&self.str_input[idx..=idx])
                .map_err(|e| LexerError::from((self.str_input, self.position, e)))?,
            ),
          ))
        }
      }
    } else {
      self.position.range.1 = self.position.index;
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
                return Ok(
                  str::from_utf8(&self.str_input[idx..end_idx])
                    .map_err(|e| LexerError::from((self.str_input, self.position, e)))?,
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
    Ok(
      str::from_utf8(&self.str_input[idx..=end_idx])
        .map_err(|e| LexerError::from((self.str_input, self.position, e)))?,
    )
  }

  fn read_text_value(&mut self, idx: usize) -> Result<&'a str> {
    while let Some(&(_, ch)) = self.peek_char() {
      match ch {
        // SCHAR
        '\x20'..='\x21' | '\x23'..='\x5b' | '\x5d'..='\x7e' | '\u{0128}'..='\u{10FFFD}' => {
          let _ = self.read_char()?;
        }
        // SESC
        '\\' => {
          let _ = self.read_char();
          if let Some(&(_, ch)) = self.peek_char() {
            match ch {
              '\x20'..='\x7e' | '\u{0128}'..='\u{10FFFD}' => {
                let _ = self.read_char()?;
              }
              _ => {
                return Err(
                  (
                    self.str_input,
                    self.position,
                    "Unexpected escape character in text string",
                  )
                    .into(),
                )
              }
            }
          }
        }
        // Closing "
        '\x22' => {
          return Ok(
            str::from_utf8(&self.str_input[idx + 1..self.read_char()?.0])
              .map_err(|e| LexerError::from((self.str_input, self.position, e)))?,
          );
        }
        _ => {
          return Err(
            (
              self.str_input,
              self.position,
              "Unexpected char in text string. Expected closing \"",
            )
              .into(),
          )
        }
      }
    }

    Err((self.str_input, self.position, "Empty text value").into())
  }

  fn read_byte_string(&mut self, idx: usize) -> Result<&'a [u8]> {
    while let Some(&(_, ch)) = self.peek_char() {
      match ch {
        // BCHAR
        '\x20'..='\x26' | '\x28'..='\x5b' | '\x5d'..='\u{10FFFD}' => {
          let _ = self.read_char();
        }
        // Closing '
        '\x27' => return Ok(&self.str_input[idx..self.read_char()?.0]),
        _ => {
          return Err(
            (
              self.str_input,
              self.position,
              "Unexpected character in byte string. Expected closing '",
            )
              .into(),
          )
        }
      }
    }

    Err((self.str_input, self.position, "Empty byte string").into())
  }

  fn read_prefixed_byte_string(&mut self, idx: usize) -> Result<Cow<'a, str>> {
    let mut has_whitespace = false;

    while let Some(&(_, ch)) = self.peek_char() {
      match ch {
        // BCHAR
        '\x20'..='\x26' | '\x28'..='\x5b' | '\x5d'..='\u{10FFFD}' => {
          let _ = self.read_char();
        }
        // SESC
        '\\' => {
          let _ = self.read_char();
          if let Some(&(_, ch)) = self.peek_char() {
            match ch {
              '\x20'..='\x7e' | '\u{0128}'..='\u{10FFFD}' => {
                let _ = self.read_char()?;
              }
              _ => {
                return Err(
                  (
                    self.str_input,
                    self.position,
                    "Unexpected escape character in byte string",
                  )
                    .into(),
                )
              }
            }
          }
        }
        // Closing '
        '\x27' => {
          // Whitespace is ignored for prefixed byte strings and requires allocation
          if has_whitespace {
            return Ok(
              str::from_utf8(&self.str_input[idx..self.read_char()?.0])
                .map_err(|e| LexerError::from((self.str_input, self.position, e)))?
                .replace(" ", "")
                .into(),
            );
          }

          return Ok(
            str::from_utf8(&self.str_input[idx..self.read_char()?.0])
              .map_err(|e| LexerError::from((self.str_input, self.position, e)))?
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
            return Err(
              (
                self.str_input,
                self.position,
                "Unexpected char in byte string. Expected closing '",
              )
                .into(),
            );
          }
        }
      }
    }

    Err((self.str_input, self.position, "Empty byte string").into())
  }

  fn read_comment(&mut self, idx: usize) -> Result<&'a str> {
    while let Some(&(_, ch)) = self.peek_char() {
      if ch != '\x0a' && ch != '\x0d' {
        let _ = self.read_char()?;
      } else {
        return Ok(
          str::from_utf8(&self.str_input[idx + 1..self.read_char()?.0])
            .map_err(|e| LexerError::from((self.str_input, self.position, e)))?,
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

    if let Some(&c) = self.multipeek.peek() {
      if c.1 == '.' {
        if let Some(&c) = self.multipeek.peek() {
          if is_digit(c.1) {
            let _ = self.read_char()?;
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
          .map_err(|e| LexerError::from((self.str_input, self.position, e)))?
          .parse()
          .map_err(|e| LexerError::from((self.str_input, self.position, e)))?,
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
        .map_err(|e| LexerError::from((self.str_input, self.position, e)))?
        .parse()
        .map_err(|e| LexerError::from((self.str_input, self.position, e)))?,
    ))
  }

  fn peek_char(&mut self) -> Option<&(usize, char)> {
    self.input.peek()
  }

  fn read_tag(&mut self) -> Result<Tag<'a>> {
    self.position.range.0 = self.position.index;

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
                  return Err((self.str_input, self.position, "Malformed tag").into());
                }
              }

              self.position.range.1 = self.position.index;
              return Ok(tag);
            }
          }
        }

        let (idx, _) = self.read_char()?;

        self.position.range.1 = idx;

        Ok(Tag::DATA((None, self.read_identifier(idx)?)))
      }
      Ok(c) if is_digit(c.1) => {
        let mt = self.read_number(c.0)?.1 as u8;

        let (_, ch) = self.read_char()?;

        if ch == '.' {
          let (idx, _) = self.read_char()?;

          let (_, t) = self.read_number(idx)?;

          self.position.range.1 = self.position.index;

          return Ok(Tag::MAJORTYPE((mt, Some(t))));
        }

        self.position.range.1 = self.position.index;

        Ok(Tag::MAJORTYPE((mt, None)))
      }
      _ => Err((self.str_input, self.position, "Malformed tag").into()),
    }
  }

  fn read_range(&mut self, lower: Token<'a>) -> Result<(Position, Token<'a>)> {
    let token_position = self.position;

    let mut is_inclusive = true;

    if let Some(&c) = self.peek_char() {
      if c.1 == '.' {
        let _ = self.read_char()?;

        if let Some(&c) = self.peek_char() {
          if c.1 == '.' {
            let _ = self.read_char()?;

            is_inclusive = false;
          }
        }
      }
    }

    if let Some(&c) = self.peek_char() {
      if c.1 == '\u{0020}' {
        let _ = self.read_char()?;
      }
    }

    let c = self.read_char()?;

    if is_digit(c.1) || c.1 == '-' {
      let upper = self.read_int_or_float(c.0)?;

      match lower {
        Token::VALUE(value) => {
          match value {
            Value::INT(li) => {
              if let Token::VALUE(value) = upper {
                match value {
                  Value::INT(ui) => {
                    return Ok((
                      token_position,
                      Token::RANGE((RangeValue::INT(li), RangeValue::INT(ui), is_inclusive)),
                    ))
                  }
                  Value::UINT(ui) => {
                    return Ok((
                      token_position,
                      Token::RANGE((RangeValue::INT(li), RangeValue::UINT(ui), is_inclusive)),
                    ))
                  }
                  _ => return Err(
                    (
                      self.str_input,
                      self.position,
                      "Only numerical ranges between integers or floating point values are allowed",
                    )
                      .into(),
                  ),
                }
              }
            }
            Value::UINT(li) => {
              if let Token::VALUE(value) = upper {
                if let Value::UINT(ui) = value {
                  return Ok((
                    token_position,
                    Token::RANGE((RangeValue::UINT(li), RangeValue::UINT(ui), is_inclusive)),
                  ));
                }
              } else {
                return Err(
                  (
                    self.str_input,
                    self.position,
                    "Only numerical ranges between integers or floating point values are allowed",
                  )
                    .into(),
                );
              }
            }
            Value::FLOAT(lf) => {
              if let Token::VALUE(value) = upper {
                if let Value::FLOAT(uf) = value {
                  return Ok((
                    token_position,
                    Token::RANGE((RangeValue::FLOAT(lf), RangeValue::FLOAT(uf), is_inclusive)),
                  ));
                }
              } else {
                return Err(
                  (
                    self.str_input,
                    self.position,
                    "Only numerical ranges between integers or floating point values are allowed",
                  )
                    .into(),
                );
              }
            }
            _ => {
              return Err(
                (
                  self.str_input,
                  self.position,
                  "Only numerical ranges between integers or floating point values are allowed",
                )
                  .into(),
              )
            }
          }
        }
        _ => {
          return Ok((
            token_position,
            Token::RANGE((
              RangeValue::try_from(lower)
                .map_err(|e| LexerError::from((self.str_input, self.position, e)))?,
              RangeValue::try_from(upper)
                .map_err(|e| LexerError::from((self.str_input, self.position, e)))?,
              is_inclusive,
            )),
          ))
        }
      }
    } else if is_ealpha(c.1) {
      return Ok((
        token_position,
        Token::RANGE((
          RangeValue::try_from(lower)
            .map_err(|e| LexerError::from((self.str_input, self.position, e)))?,
          RangeValue::try_from(token::lookup_ident(self.read_identifier(c.0)?))
            .map_err(|e| LexerError::from((self.str_input, self.position, e)))?,
          is_inclusive,
        )),
      ));
    }

    Err((self.str_input, self.position, "Invalid range syntax. Ranges must be between integers (matching integer values) or between floating-point values (matching floating-point values)").into())
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
  fn verify_next_token() -> Result<()> {
    let input = r#"; this is a comment
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
      (IDENT(("mybytestring", None)), "mybytestring"),
      (ASSIGN, "="),
      (
        BYTESLICEVALUE(ByteSliceValue::UTF8(b"hello there")),
        "'hello there'",
      ),
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
      (IDENT(("mynumber", None)), "mynumber"),
      (RANGEOP(true), ".."),
      (VALUE(Value::FLOAT(100.5)), "100.5"),
      (IDENT(("myintrule", None)), "myintrule"),
      (ASSIGN, "="),
      (VALUE(Value::INT(-10)), "-10"),
      (IDENT(("mysignedfloat", None)), "mysignedfloat"),
      (ASSIGN, "="),
      (VALUE(Value::FLOAT(-10.5)), "-10.5"),
      (IDENT(("myintrange", None)), "myintrange"),
      (ASSIGN, "="),
      (VALUE(Value::INT(-10)), "-10"),
      (RANGEOP(true), ".."),
      (VALUE(Value::UINT(10)), "10"),
      (IDENT(("mycontrol", None)), "mycontrol"),
      (ASSIGN, "="),
      (IDENT(("mynumber", None)), "mynumber"),
      (GT, ".gt"),
      (VALUE(Value::UINT(0)), "0"),
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
      (IDENT(("zip-code", None)), "zip-code"),
      (COLON, ":"),
      (UINT, "uint"),
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
      let tok = l.next_token()?;
      assert_eq!((expected_tok, *literal), (&tok.1, &*tok.1.to_string()))
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
    let input = r#"100.5..150.5"#;

    let mut l = Lexer::new(input);

    let expected_tokens = [
      (VALUE(Value::FLOAT(100.5)), "100.5"),
      (RANGEOP(true), ".."),
      (VALUE(Value::FLOAT(150.5)), "150.5"),
    ];

    for (expected_tok, literal) in expected_tokens.iter() {
      let tok = l.next_token()?;
      assert_eq!((expected_tok, *literal), (&tok.1, &*tok.1.to_string()))
    }

    Ok(())
  }

  #[test]
  fn verify_diagnostic() -> Result<()> {
    let input = r#"myrule = number .asdf 10"#;

    let mut l = Lexer::new(input);

    l.next_token()?;
    l.next_token()?;
    l.next_token()?;

    match l.next_token() {
      Ok(_) => Ok(()),
      Err(e) => {
        assert_eq!(e.to_string(), r#"error: Invalid control operator
 --> input:1:16
  |
1 | myrule = number .asdf 10
  |                 ^^^^^ Invalid control operator
  |"#);

        Ok(())
      }
    }
  }
}
