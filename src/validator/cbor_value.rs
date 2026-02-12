//! Custom CBOR value type that extends ciborium's Value with support for
//! non-standard simple values (CBOR major type 7, values outside of
//! false/true/null/undefined).
//!
//! This module exists because ciborium's deserializer intentionally rejects
//! unassigned simple values (see <https://github.com/enarx/ciborium/issues/60>),
//! which makes it impossible to validate CBOR documents containing such values
//! (e.g. `#7.32`). We use `ciborium-ll` to decode CBOR at a lower level and
//! represent simple values in our own `Value` enum.

#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(feature = "lsp"))]

use std::convert::TryFrom;
use std::fmt;

use ciborium::value::Integer;
use ciborium_io::Read as _;
use ciborium_ll::{simple, Decoder, Header};

/// A CBOR value representation that supports non-standard simple values.
///
/// This mirrors `ciborium::value::Value` but adds a `Simple(u8)` variant for
/// CBOR simple values outside the well-known set (false, true, null, undefined).
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
  /// An integer
  Integer(Integer),
  /// Bytes
  Bytes(Vec<u8>),
  /// A float
  Float(f64),
  /// A string
  Text(String),
  /// A boolean
  Bool(bool),
  /// Null
  Null,
  /// Tag
  Tag(u64, Box<Value>),
  /// An array
  Array(Vec<Value>),
  /// A map
  Map(Vec<(Value, Value)>),
  /// A non-standard CBOR simple value (major type 7).
  ///
  /// Standard simple values (false=20, true=21, null=22, undefined=23) are
  /// represented by their dedicated variants (`Bool`, `Null`). This variant
  /// captures all other simple values (e.g. `#7.0` through `#7.19`, `#7.32`
  /// through `#7.255`).
  Simple(u8),
}

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Value::Integer(i) => write!(f, "{:?}", i),
      Value::Bytes(b) => write!(f, "h'{}'", hex::encode(b)),
      Value::Float(v) => write!(f, "{}", v),
      Value::Text(s) => write!(f, "\"{}\"", s),
      Value::Bool(b) => write!(f, "{}", b),
      Value::Null => write!(f, "null"),
      Value::Tag(tag, val) => write!(f, "{}({})", tag, val),
      Value::Array(a) => {
        write!(f, "[")?;
        for (i, v) in a.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", v)?;
        }
        write!(f, "]")
      }
      Value::Map(m) => {
        write!(f, "{{")?;
        for (i, (k, v)) in m.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}: {}", k, v)?;
        }
        write!(f, "}}")
      }
      Value::Simple(s) => write!(f, "simple({})", s),
    }
  }
}

impl Value {
  /// Returns `true` if this value is an `Array`.
  pub fn is_array(&self) -> bool {
    matches!(self, Value::Array(_))
  }
}

/// Convert from `ciborium::value::Value` to our `Value`.
///
/// This conversion never produces `Value::Simple` since ciborium itself
/// cannot parse non-standard simple values. It exists for interoperability
/// with code that constructs `ciborium::value::Value` directly (e.g. tests
/// that use `ciborium::ser`).
impl From<ciborium::value::Value> for Value {
  fn from(v: ciborium::value::Value) -> Self {
    match v {
      ciborium::value::Value::Integer(i) => Value::Integer(i),
      ciborium::value::Value::Bytes(b) => Value::Bytes(b),
      ciborium::value::Value::Float(f) => Value::Float(f),
      ciborium::value::Value::Text(s) => Value::Text(s),
      ciborium::value::Value::Bool(b) => Value::Bool(b),
      ciborium::value::Value::Null => Value::Null,
      ciborium::value::Value::Tag(tag, inner) => Value::Tag(tag, Box::new(Value::from(*inner))),
      ciborium::value::Value::Array(arr) => {
        Value::Array(arr.into_iter().map(Value::from).collect())
      }
      ciborium::value::Value::Map(map) => Value::Map(
        map
          .into_iter()
          .map(|(k, v)| (Value::from(k), Value::from(v)))
          .collect(),
      ),
      // ciborium::value::Value is #[non_exhaustive]
      _ => Value::Null,
    }
  }
}

/// Convert from our `Value` back to `ciborium::value::Value`.
///
/// Note: `Value::Simple` is mapped to `ciborium::value::Value::Null` since
/// ciborium has no `Simple` variant. This is only used for interoperability
/// and should not be relied upon for simple value round-tripping.
impl From<Value> for ciborium::value::Value {
  fn from(v: Value) -> Self {
    match v {
      Value::Integer(i) => ciborium::value::Value::Integer(i),
      Value::Bytes(b) => ciborium::value::Value::Bytes(b),
      Value::Float(f) => ciborium::value::Value::Float(f),
      Value::Text(s) => ciborium::value::Value::Text(s),
      Value::Bool(b) => ciborium::value::Value::Bool(b),
      Value::Null => ciborium::value::Value::Null,
      Value::Tag(tag, inner) => {
        ciborium::value::Value::Tag(tag, Box::new(ciborium::value::Value::from(*inner)))
      }
      Value::Array(arr) => {
        ciborium::value::Value::Array(arr.into_iter().map(ciborium::value::Value::from).collect())
      }
      Value::Map(map) => ciborium::value::Value::Map(
        map
          .into_iter()
          .map(|(k, v)| {
            (
              ciborium::value::Value::from(k),
              ciborium::value::Value::from(v),
            )
          })
          .collect(),
      ),
      // Simple values have no ciborium equivalent
      Value::Simple(_) => ciborium::value::Value::Null,
    }
  }
}

/// Error type for CBOR decoding
#[derive(Debug)]
pub enum DecodeError {
  /// I/O error during reading
  Io(std::io::Error),
  /// Syntax error at the given byte offset
  Syntax(usize),
  /// Unexpected end of input
  UnexpectedEof,
  /// Unexpected break marker
  UnexpectedBreak,
}

impl fmt::Display for DecodeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      DecodeError::Io(e) => write!(f, "I/O error: {}", e),
      DecodeError::Syntax(offset) => write!(f, "syntax error at offset {}", offset),
      DecodeError::UnexpectedEof => write!(f, "unexpected end of input"),
      DecodeError::UnexpectedBreak => write!(f, "unexpected break"),
    }
  }
}

impl std::error::Error for DecodeError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      DecodeError::Io(e) => Some(e),
      _ => None,
    }
  }
}

impl From<ciborium_ll::Error<std::io::Error>> for DecodeError {
  fn from(e: ciborium_ll::Error<std::io::Error>) -> Self {
    match e {
      ciborium_ll::Error::Io(io) => DecodeError::Io(io),
      ciborium_ll::Error::Syntax(offset) => DecodeError::Syntax(offset),
    }
  }
}

/// Decode a CBOR value from a byte slice, with support for non-standard
/// simple values.
///
/// Unlike `ciborium::de::from_reader`, this decoder does not reject
/// unassigned simple values (CBOR major type 7 with values outside
/// false/true/null/undefined). Such values are represented as
/// `Value::Simple(n)`.
pub fn decode_cbor(input: &[u8]) -> Result<Value, DecodeError> {
  let cursor = std::io::Cursor::new(input);
  let mut decoder = Decoder::from(cursor);
  decode_value(&mut decoder)
}

fn decode_value<R: ciborium_io::Read>(decoder: &mut Decoder<R>) -> Result<Value, DecodeError>
where
  ciborium_ll::Error<R::Error>: Into<DecodeError>,
{
  let header = decoder.pull().map_err(Into::into)?;
  match header {
    Header::Positive(v) => Ok(Value::Integer(Integer::from(v))),
    Header::Negative(v) => {
      // ciborium-ll stores negative as the raw value; the actual number is -1 - v
      // ciborium::value::Integer can be created from i128
      let n: i128 = -1i128 - (v as i128);
      // Try to convert to i64 first, then fall back to i128
      if let Ok(n64) = i64::try_from(n) {
        Ok(Value::Integer(Integer::from(n64)))
      } else {
        // For very large negatives, use the raw encoding
        // ciborium::value::Integer supports i128 via TryFrom
        match Integer::try_from(n) {
          Ok(i) => Ok(Value::Integer(i)),
          Err(_) => Err(DecodeError::Syntax(decoder.offset())),
        }
      }
    }
    Header::Float(f) => Ok(Value::Float(f)),
    Header::Simple(s) => match s {
      simple::FALSE => Ok(Value::Bool(false)),
      simple::TRUE => Ok(Value::Bool(true)),
      simple::NULL | simple::UNDEFINED => Ok(Value::Null),
      _ => Ok(Value::Simple(s)),
    },
    Header::Bytes(len) => {
      let bytes = read_bytes(decoder, len)?;
      Ok(Value::Bytes(bytes))
    }
    Header::Text(len) => {
      let text = read_text(decoder, len)?;
      Ok(Value::Text(text))
    }
    Header::Tag(tag) => {
      let inner = decode_value(decoder)?;
      Ok(Value::Tag(tag, Box::new(inner)))
    }
    Header::Array(len) => {
      let items = decode_array(decoder, len)?;
      Ok(Value::Array(items))
    }
    Header::Map(len) => {
      let entries = decode_map(decoder, len)?;
      Ok(Value::Map(entries))
    }
    Header::Break => Err(DecodeError::UnexpectedBreak),
  }
}

fn read_bytes<R: ciborium_io::Read>(
  decoder: &mut Decoder<R>,
  len: Option<usize>,
) -> Result<Vec<u8>, DecodeError>
where
  ciborium_ll::Error<R::Error>: Into<DecodeError>,
{
  match len {
    Some(n) => {
      let mut buf = vec![0u8; n];
      decoder.read_exact(&mut buf).map_err(|e| {
        let io_err: ciborium_ll::Error<R::Error> = ciborium_ll::Error::Io(e);
        io_err.into()
      })?;
      Ok(buf)
    }
    None => {
      // Indefinite-length bytes: read segments until break
      let mut result = Vec::new();
      loop {
        let h = decoder.pull().map_err(Into::into)?;
        match h {
          Header::Break => break,
          Header::Bytes(seg_len) => {
            let seg = read_bytes(decoder, seg_len)?;
            result.extend_from_slice(&seg);
          }
          _ => return Err(DecodeError::Syntax(decoder.offset())),
        }
      }
      Ok(result)
    }
  }
}

fn read_text<R: ciborium_io::Read>(
  decoder: &mut Decoder<R>,
  len: Option<usize>,
) -> Result<String, DecodeError>
where
  ciborium_ll::Error<R::Error>: Into<DecodeError>,
{
  match len {
    Some(n) => {
      let mut buf = vec![0u8; n];
      decoder.read_exact(&mut buf).map_err(|e| {
        let io_err: ciborium_ll::Error<R::Error> = ciborium_ll::Error::Io(e);
        io_err.into()
      })?;
      String::from_utf8(buf).map_err(|_| DecodeError::Syntax(decoder.offset()))
    }
    None => {
      // Indefinite-length text: read segments until break
      let mut result = String::new();
      loop {
        let h = decoder.pull().map_err(Into::into)?;
        match h {
          Header::Break => break,
          Header::Text(seg_len) => {
            let seg = read_text(decoder, seg_len)?;
            result.push_str(&seg);
          }
          _ => return Err(DecodeError::Syntax(decoder.offset())),
        }
      }
      Ok(result)
    }
  }
}

fn decode_array<R: ciborium_io::Read>(
  decoder: &mut Decoder<R>,
  len: Option<usize>,
) -> Result<Vec<Value>, DecodeError>
where
  ciborium_ll::Error<R::Error>: Into<DecodeError>,
{
  match len {
    Some(n) => {
      let mut items = Vec::with_capacity(n);
      for _ in 0..n {
        items.push(decode_value(decoder)?);
      }
      Ok(items)
    }
    None => {
      // Indefinite-length array
      let mut items = Vec::new();
      loop {
        // Peek at the next header to check for break
        let h = decoder.pull().map_err(Into::into)?;
        if h == Header::Break {
          break;
        }
        decoder.push(h);
        items.push(decode_value(decoder)?);
      }
      Ok(items)
    }
  }
}

fn decode_map<R: ciborium_io::Read>(
  decoder: &mut Decoder<R>,
  len: Option<usize>,
) -> Result<Vec<(Value, Value)>, DecodeError>
where
  ciborium_ll::Error<R::Error>: Into<DecodeError>,
{
  match len {
    Some(n) => {
      let mut entries = Vec::with_capacity(n);
      for _ in 0..n {
        let key = decode_value(decoder)?;
        let val = decode_value(decoder)?;
        entries.push((key, val));
      }
      Ok(entries)
    }
    None => {
      // Indefinite-length map
      let mut entries = Vec::new();
      loop {
        let h = decoder.pull().map_err(Into::into)?;
        if h == Header::Break {
          break;
        }
        decoder.push(h);
        let key = decode_value(decoder)?;
        let val = decode_value(decoder)?;
        entries.push((key, val));
      }
      Ok(entries)
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn decode_standard_simple_values() {
    // false (0xf4)
    assert_eq!(decode_cbor(&[0xf4]).unwrap(), Value::Bool(false));
    // true (0xf5)
    assert_eq!(decode_cbor(&[0xf5]).unwrap(), Value::Bool(true));
    // null (0xf6)
    assert_eq!(decode_cbor(&[0xf6]).unwrap(), Value::Null);
    // undefined (0xf7)
    assert_eq!(decode_cbor(&[0xf7]).unwrap(), Value::Null);
  }

  #[test]
  fn decode_nonstandard_simple_values() {
    // simple(0) - 0xe0
    assert_eq!(decode_cbor(&[0xe0]).unwrap(), Value::Simple(0));
    // simple(19) - 0xf3
    assert_eq!(decode_cbor(&[0xf3]).unwrap(), Value::Simple(19));
    // simple(32) - 0xf8 0x20
    assert_eq!(decode_cbor(&[0xf8, 0x20]).unwrap(), Value::Simple(32));
    // simple(255) - 0xf8 0xff
    assert_eq!(decode_cbor(&[0xf8, 0xff]).unwrap(), Value::Simple(255));
  }

  #[test]
  fn decode_integer() {
    assert_eq!(decode_cbor(&[0x00]).unwrap(), Value::Integer(0.into()));
    assert_eq!(decode_cbor(&[0x01]).unwrap(), Value::Integer(1.into()));
    assert_eq!(decode_cbor(&[0x17]).unwrap(), Value::Integer(23.into()));
    assert_eq!(
      decode_cbor(&[0x18, 0x18]).unwrap(),
      Value::Integer(24.into())
    );
    // Negative: -1
    assert_eq!(
      decode_cbor(&[0x20]).unwrap(),
      Value::Integer((-1i64).into())
    );
  }

  #[test]
  fn decode_text() {
    // Empty text
    assert_eq!(decode_cbor(&[0x60]).unwrap(), Value::Text("".into()));
    // "IETF"
    assert_eq!(
      decode_cbor(&[0x64, 0x49, 0x45, 0x54, 0x46]).unwrap(),
      Value::Text("IETF".into())
    );
  }

  #[test]
  fn decode_bytes() {
    // Empty bytes
    assert_eq!(decode_cbor(&[0x40]).unwrap(), Value::Bytes(vec![]));
    // h'01020304'
    assert_eq!(
      decode_cbor(&[0x44, 0x01, 0x02, 0x03, 0x04]).unwrap(),
      Value::Bytes(vec![1, 2, 3, 4])
    );
  }

  #[test]
  fn decode_array() {
    // []
    assert_eq!(decode_cbor(&[0x80]).unwrap(), Value::Array(vec![]));
    // [1, 2, 3]
    assert_eq!(
      decode_cbor(&[0x83, 0x01, 0x02, 0x03]).unwrap(),
      Value::Array(vec![
        Value::Integer(1.into()),
        Value::Integer(2.into()),
        Value::Integer(3.into()),
      ])
    );
  }

  #[test]
  fn decode_map() {
    // {}
    assert_eq!(decode_cbor(&[0xa0]).unwrap(), Value::Map(vec![]));
  }

  #[test]
  fn decode_tag() {
    // Tag 42 wrapping text "test"
    let input = [0xd8, 0x2a, 0x64, 0x74, 0x65, 0x73, 0x74];
    assert_eq!(
      decode_cbor(&input).unwrap(),
      Value::Tag(42, Box::new(Value::Text("test".into())))
    );
  }

  #[test]
  fn decode_float() {
    // 0.0 as f16 (0xf9 0x00 0x00)
    assert_eq!(decode_cbor(&[0xf9, 0x00, 0x00]).unwrap(), Value::Float(0.0));
    // 1.0 as f16 (0xf9 0x3c 0x00)
    assert_eq!(decode_cbor(&[0xf9, 0x3c, 0x00]).unwrap(), Value::Float(1.0));
  }

  #[test]
  fn decode_array_with_simple_values() {
    // [simple(32), 1, true]
    let input = [0x83, 0xf8, 0x20, 0x01, 0xf5];
    assert_eq!(
      decode_cbor(&input).unwrap(),
      Value::Array(vec![
        Value::Simple(32),
        Value::Integer(1.into()),
        Value::Bool(true),
      ])
    );
  }

  #[test]
  fn from_ciborium_value() {
    let cv = ciborium::value::Value::Integer(42.into());
    let v: Value = cv.into();
    assert_eq!(v, Value::Integer(42.into()));

    let cv = ciborium::value::Value::Tag(1, Box::new(ciborium::value::Value::Text("hello".into())));
    let v: Value = cv.into();
    assert_eq!(v, Value::Tag(1, Box::new(Value::Text("hello".into()))));
  }
}
