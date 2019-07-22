use super::lexer::LexerError;
use std::{convert::TryFrom, fmt};

#[derive(PartialEq, Debug)]
pub enum Token<'a> {
  ILLEGAL(&'a str),
  EOF,

  IDENT((&'a str, Option<&'a SocketPlug>)),
  VALUE(Value<'a>),
  INTLITERAL(isize),
  UINTLITERAL(usize),
  FLOATLITERAL(f64),
  TAG(Tag<'a>),

  // Operators
  ASSIGN,
  OPTIONAL,
  ASTERISK,
  ONEORMORE,
  UNWRAP,
  CONTROL(&'a str),

  // Delimiters
  COMMA,
  COLON,

  COMMENT(&'a str),

  TCHOICE,
  GCHOICE,
  TCHOICEALT,
  GCHOICEALT,
  ARROWMAP,
  CUT,

  // lower, upper and is_inclusive
  RANGE((RangeValue<'a>, RangeValue<'a>, bool)),

  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,
  LBRACKET,
  RBRACKET,
  LANGLEBRACKET,
  RANGLEBRACKET,

  // Control operators
  SIZE,
  BITS,
  REGEXP,
  CBOR,
  CBORSEQ,
  WITHIN,
  AND,
  LT,
  LE,
  GT,
  GE,
  EQ,
  NE,
  DEFAULT,

  GTOCHOICE,

  // Standard prelude
  FALSE,
  TRUE,
  BOOL,
  NIL,
  NULL,
  UINT,
  NINT,
  INT,
  FLOAT16,
  FLOAT32,
  FLOAT64,
  FLOAT1632,
  FLOAT3264,
  FLOAT,
  BSTR,
  TSTR,
  ANY,
  BYTES,
  TEXT,
  TDATE,
  TIME,
  NUMBER,
  BIGUINT,
  BIGNINT,
  BIGINT,
  INTEGER,
  UNSIGNED,
  DECFRAC,
  BIGFLOAT,
  EB64URL,
  EB64LEGACY,
  EB16,
  ENCODEDCBOR,
  URI,
  B64URL,
  B64LEGACY,
  TREGEXP,
  MIMEMESSAGE,
  CBORANY,
  UNDEFINED,
}

impl<'a> Token<'a> {
  pub fn in_standard_prelude(&self) -> Option<&'a str> {
    match self {
      Token::ANY => Some("any"),
      Token::UINT => Some("uint"),
      Token::NINT => Some("nint"),
      Token::INT => Some("int"),
      Token::BSTR => Some("bstr"),
      Token::BYTES => Some("bytes"),
      Token::TSTR => Some("tstr"),
      Token::TEXT => Some("text"),
      Token::TDATE => Some("tdate"),
      Token::TIME => Some("time"),
      Token::NUMBER => Some("number"),
      Token::BIGUINT => Some("biguint"),
      Token::BIGNINT => Some("bignint"),
      Token::BIGINT => Some("bigint"),
      Token::INTEGER => Some("integer"),
      Token::UNSIGNED => Some("unsigned"),
      Token::DECFRAC => Some("decfrac"),
      Token::BIGFLOAT => Some("bigfloat"),
      Token::EB64URL => Some("eb64url"),
      Token::EB64LEGACY => Some("eb64legacy"),
      Token::EB16 => Some("eb16"),
      Token::ENCODEDCBOR => Some("encoded-cbor"),
      Token::URI => Some("uri"),
      Token::B64URL => Some("b64url"),
      Token::B64LEGACY => Some("b64legacy"),
      Token::REGEXP => Some("regexp"),
      Token::MIMEMESSAGE => Some("mime-message"),
      Token::CBORANY => Some("cbor-any"),
      Token::FLOAT16 => Some("float16"),
      Token::FLOAT32 => Some("float32"),
      Token::FLOAT64 => Some("float64"),
      Token::FLOAT1632 => Some("float16-32"),
      Token::FLOAT3264 => Some("float32-64"),
      Token::FLOAT => Some("float"),
      Token::FALSE => Some("false"),
      Token::TRUE => Some("true"),
      Token::BOOL => Some("bool"),
      Token::NIL => Some("nil"),
      Token::NULL => Some("null"),
      Token::UNDEFINED => Some("undefined"),
      _ => None,
    }
  }
}

#[derive(PartialEq, Debug)]
pub enum Tag<'a> {
  DATA((Option<usize>, &'a str)),
  MAJORTYPE((u8, Option<usize>)),
  ANY,
}

impl<'a> fmt::Display for Tag<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Tag::DATA((tag_uint, tagged_value)) => {
        if let Some(t) = tag_uint {
          return write!(f, "#6.{}({})", t, tagged_value);
        }

        write!(f, "#6({})", tagged_value)
      }
      Tag::MAJORTYPE((major_type, tag_uint)) => {
        if let Some(t) = tag_uint {
          return write!(f, "{}.{}", major_type, t);
        }

        write!(f, "{}", major_type)
      }
      Tag::ANY => write!(f, "#"),
    }
  }
}

#[derive(Debug, PartialEq)]
pub enum RangeValue<'a> {
  IDENT((&'a str, Option<&'a SocketPlug>)),
  UINT(usize),
  FLOAT(f64),
}

impl<'a> TryFrom<Token<'a>> for RangeValue<'a> {
  type Error = LexerError;

  fn try_from(t: Token<'a>) -> Result<Self, Self::Error> {
    match t {
      Token::IDENT(ident) => Ok(RangeValue::IDENT(ident)),
      Token::UINTLITERAL(i) => Ok(RangeValue::UINT(i)),
      Token::FLOATLITERAL(f) => Ok(RangeValue::FLOAT(f)),
      _ => Err("Invalid range token".into()),
    }
  }
}

impl<'a> RangeValue<'a> {
  pub fn as_value(&self) -> Option<Value<'a>> {
    match &self {
      RangeValue::UINT(ui) => Some(Value::UINT(*ui)),
      RangeValue::FLOAT(f) => Some(Value::FLOAT(*f)),
      _ => None,
    }
  }
}

impl<'a> fmt::Display for RangeValue<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      RangeValue::IDENT(ident) => write!(f, "{}", ident.0),
      RangeValue::UINT(i) => write!(f, "{}", i),
      RangeValue::FLOAT(fl) => write!(f, "{}", fl),
    }
  }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Value<'a> {
  // TODO: support hexfloat and exponent
  INT(isize),
  UINT(usize),
  FLOAT(f64),
  TEXT(&'a str),

  // TODO: support raw byte string
  BYTES(&'a str),
}

impl<'a> fmt::Display for Value<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Value::TEXT(text) => write!(f, "\"{}\"", text),
      Value::INT(i) => write!(f, "{}", i),
      Value::UINT(ui) => write!(f, "{}", ui),
      Value::FLOAT(float) => write!(f, "{}", float),
      Value::BYTES(b) => write!(f, "{}", b),
    }
  }
}

impl<'a> From<&'static str> for Value<'a> {
  fn from(value: &'static str) -> Self {
    Value::TEXT(value)
  }
}

#[derive(Debug, PartialEq)]
pub enum SocketPlug {
  TYPE,
  GROUP,
}

impl SocketPlug {
  pub fn from_str(s: &str) -> Option<&Self> {
    if let Some(c) = s.chars().next() {
      if c == '$' {
        if let Some(c) = s.chars().nth(1) {
          if c == '$' {
            return Some(&SocketPlug::GROUP);
          }
        }

        return Some(&SocketPlug::TYPE);
      }
    }

    None
  }
}

impl<'a> fmt::Display for SocketPlug {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      SocketPlug::TYPE => write!(f, "$"),
      SocketPlug::GROUP => write!(f, "$$"),
    }
  }
}

impl<'a> fmt::Display for Token<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Token::IDENT((ident, socket_plug)) => {
        if let Some(sp) = socket_plug {
          return write!(f, "{}{}", sp, ident);
        }

        write!(f, "{}", ident)
      }
      Token::ILLEGAL(s) => write!(f, "ILLEGAL({})", s),
      Token::ASSIGN => write!(f, "="),
      Token::ONEORMORE => write!(f, "+"),
      Token::OPTIONAL => write!(f, "?"),
      Token::ASTERISK => write!(f, "*"),
      Token::LPAREN => write!(f, "("),
      Token::RPAREN => write!(f, ")"),
      Token::LBRACE => write!(f, "{{"),
      Token::RBRACE => write!(f, "}}"),
      Token::TCHOICE => write!(f, "/"),
      Token::TCHOICEALT => write!(f, "/="),
      Token::GCHOICEALT => write!(f, "//="),
      Token::COMMA => write!(f, ","),
      Token::COMMENT(c) => write!(f, ";{}", c),
      Token::COLON => write!(f, ":"),
      Token::CUT => write!(f, "^"),
      Token::EOF => write!(f, ""),
      Token::TSTR => write!(f, "tstr"),
      Token::LANGLEBRACKET => write!(f, "<"),
      Token::RANGLEBRACKET => write!(f, ">"),
      Token::INT => write!(f, "int"),
      Token::UINT => write!(f, "uint"),
      Token::INTLITERAL(i) => write!(f, "{}", i),
      Token::UINTLITERAL(ui) => write!(f, "{}", ui),
      Token::FLOATLITERAL(fl) => write!(f, "{}", fl),
      Token::ARROWMAP => write!(f, "=>"),
      Token::SIZE => write!(f, ".size"),
      Token::BITS => write!(f, ".bits"),
      Token::REGEXP => write!(f, ".regexp"),
      Token::CBOR => write!(f, ".cbor"),
      Token::CBORSEQ => write!(f, ".cborseq"),
      Token::WITHIN => write!(f, ".within"),
      Token::AND => write!(f, ".and"),
      Token::LT => write!(f, ".lt"),
      Token::LE => write!(f, ".le"),
      Token::GT => write!(f, ".gt"),
      Token::GE => write!(f, ".ge"),
      Token::EQ => write!(f, ".eq"),
      Token::NE => write!(f, ".ne"),
      Token::DEFAULT => write!(f, ".default"),
      Token::NUMBER => write!(f, "number"),
      Token::BSTR => write!(f, "bstr"),
      Token::GCHOICE => write!(f, "//"),
      Token::TRUE => write!(f, "true"),
      Token::GTOCHOICE => write!(f, "&"),
      Token::VALUE(value) => write!(f, "{}", value),
      Token::RANGE((l, u, i)) => {
        if *i {
          return write!(f, "{}..{}", l, u);
        }

        write!(f, "{}...{}", l, u)
      }
      Token::TAG(tag) => write!(f, "{}", tag),
      _ => write!(f, ""),
    }
  }
}

pub fn lookup_control(ident: &str) -> Token {
  match ident {
    "size" => Token::SIZE,
    "bits" => Token::BITS,
    "regexp" => Token::REGEXP,
    "cbor" => Token::CBOR,
    "cborseq" => Token::CBORSEQ,
    "within" => Token::WITHIN,
    "and" => Token::AND,
    "lt" => Token::LT,
    "le" => Token::LE,
    "gt" => Token::GT,
    "ge" => Token::GE,
    "eq" => Token::EQ,
    "ne" => Token::NE,
    "default" => Token::DEFAULT,
    _ => Token::ILLEGAL(ident),
  }
}

pub fn lookup_ident(ident: &str) -> Token {
  match ident {
    "false" => Token::FALSE,
    "true" => Token::TRUE,
    "bool" => Token::BOOL,
    "nil" => Token::NIL,
    "null" => Token::NULL,
    "uint" => Token::UINT,
    "nint" => Token::NINT,
    "int" => Token::INT,
    "float16" => Token::FLOAT16,
    "float32" => Token::FLOAT32,
    "float64" => Token::FLOAT64,
    "float16-32" => Token::FLOAT1632,
    "float32-64" => Token::FLOAT3264,
    "float" => Token::FLOAT,
    "bstr" => Token::BSTR,
    "tstr" => Token::TSTR,
    "any" => Token::ANY,
    "bytes" => Token::BYTES,
    "text" => Token::TEXT,
    "tdate" => Token::TDATE,
    "time" => Token::TIME,
    "number" => Token::NUMBER,
    "biguint" => Token::BIGUINT,
    "bignint" => Token::BIGNINT,
    "integer" => Token::INTEGER,
    "unsigned" => Token::UNSIGNED,
    "decfrac" => Token::DECFRAC,
    "bigfloat" => Token::BIGFLOAT,
    "eb64url" => Token::EB64URL,
    "eb64legacy" => Token::EB64LEGACY,
    "eb16" => Token::EB16,
    "encoded-cbor" => Token::ENCODEDCBOR,
    "uri" => Token::URI,
    "b64url" => Token::B64URL,
    "b64legacy" => Token::B64LEGACY,
    "regexp" => Token::TREGEXP,
    "mime-message" => Token::MIMEMESSAGE,
    "cbor-any" => Token::CBORANY,
    "undefined" => Token::UNDEFINED,
    _ => {
      if let Some(c) = ident.chars().next() {
        if c == '$' {
          if let Some(c) = ident.chars().nth(1) {
            if c == '$' {
              return Token::IDENT((&ident[2..], Some(&SocketPlug::GROUP)));
            }
          }

          return Token::IDENT((&ident[1..], Some(&SocketPlug::TYPE)));
        }
      }

      Token::IDENT((ident, None))
    }
  }
}
