use std::{convert::TryFrom, fmt};

#[cfg(feature = "std")]
use std::borrow::Cow;

#[cfg(target_arch = "wasm32")]
use serde::{Deserialize, Serialize};

#[cfg(not(feature = "std"))]
use alloc::{borrow::Cow, string::String};

/// Token which represents a valid CDDL character or sequence
#[derive(PartialEq, Debug, Clone)]
pub enum Token<'a> {
  /// Illegal sequence of characters
  ILLEGAL(&'a str),
  /// End of file
  EOF,

  /// Identifier
  IDENT(
    /// Identifier
    &'a str,
    /// Socket/plug
    Option<SocketPlug>,
  ),
  /// Value
  VALUE(Value<'a>),
  /// CBOR tag '#'
  TAG(
    /// Major type
    Option<u8>,
    /// Optional constraint
    Option<usize>,
  ),

  // Operators
  /// Assignment operator '='
  ASSIGN,
  /// Optional occurrence indicator '?'
  OPTIONAL,
  /// Zero or more occurrence indicator '*'
  ASTERISK,
  /// One or more occurrence indicator '+'
  ONEORMORE,
  /// Unwrap operator '~'
  UNWRAP,

  // Delimiters
  /// Comma ','
  COMMA,
  /// Colon ':'
  COLON,

  /// Comment text
  COMMENT(&'a str),

  /// Type choice indicator '/'
  TCHOICE,
  /// Group choice indicator '//'
  GCHOICE,
  /// Type choice alternative '/='
  TCHOICEALT,
  /// Group choice alternative '//='
  GCHOICEALT,
  /// Arrow map '=>'
  ARROWMAP,
  /// Cut '^'
  CUT,

  /// Range operator. Inclusive '..' if true, otherwise exclusive '...'s
  RANGEOP(bool),

  /// Range
  RANGE(
    /// Lower bound
    RangeValue<'a>,
    /// Upper bound
    RangeValue<'a>,
    /// Inclusive
    bool,
  ),

  /// Left opening parend
  LPAREN,
  /// Right closing parend
  RPAREN,
  /// Left opening brace
  LBRACE,
  /// Right closing brace
  RBRACE,
  /// Left opening bracket
  LBRACKET,
  /// Right closing bracket
  RBRACKET,
  /// Left opening angle bracket
  LANGLEBRACKET,
  /// Right closing angle bracket
  RANGLEBRACKET,

  // Control operators
  /// .size control operator
  SIZE,
  /// .bits control operator
  BITS,
  /// .regexp control operator
  CREGEXP,
  /// .cbor control operator
  CBOR,
  /// .cborseq control operator
  CBORSEQ,
  /// .within control operator
  WITHIN,
  /// .and control operator
  AND,
  /// .lt control operator
  LT,
  /// .le control operator
  LE,
  /// .gt control operator
  GT,
  /// .ge control operator
  GE,
  /// .eq control operator
  EQ,
  /// .ne control operator
  NE,
  /// .default control operator
  DEFAULT,
  /// .pcre control operator
  /// Proposed control extension to support Perl-Compatible Regular Expressions
  /// (PCREs). See <https://tools.ietf.org/html/rfc8610#section-3.8.3.2s>
  PCRE,
  #[cfg(feature = "additional-controls")]
  /// .cat control operator (rfc 9165)
  CAT,
  #[cfg(feature = "additional-controls")]
  /// .det control operator (rfc 9165)
  DET,
  #[cfg(feature = "additional-controls")]
  /// .plus control operator (rfc 9165)
  PLUS,
  #[cfg(feature = "additional-controls")]
  /// .abnf control operator (rfc 9165)
  ABNF,
  #[cfg(feature = "additional-controls")]
  /// .abnfb control operator (rfc 9165)
  ABNFB,
  #[cfg(feature = "additional-controls")]
  /// .feature control operator (rfc 9165)
  FEATURE,

  /// group to choice enumeration '&'
  GTOCHOICE,

  // Standard prelude
  /// false
  FALSE,
  /// true
  TRUE,
  /// bool
  BOOL,
  /// nil
  NIL,
  /// null
  NULL,
  /// uint
  UINT,
  /// nint
  NINT,
  /// int
  INT,
  /// float16
  FLOAT16,
  /// float32
  FLOAT32,
  /// float64
  FLOAT64,
  /// float16-32
  FLOAT1632,
  /// float32-64
  FLOAT3264,
  ///float
  FLOAT,
  /// bstr
  BSTR,
  /// tstr
  TSTR,
  /// any
  ANY,
  /// bytes
  BYTES,
  /// text
  TEXT,
  /// tdate
  TDATE,
  /// time
  TIME,
  /// number
  NUMBER,
  /// biguint
  BIGUINT,
  /// bignint
  BIGNINT,
  /// bigint
  BIGINT,
  /// integer
  INTEGER,
  /// unsigned
  UNSIGNED,
  /// decfrac
  DECFRAC,
  /// bigfloat
  BIGFLOAT,
  /// eb64url
  EB64URL,
  /// eb64legacy
  EB64LEGACY,
  /// eb16k
  EB16,
  /// encoded-cbor
  ENCODEDCBOR,
  /// uri
  URI,
  /// b64url
  B64URL,
  /// b64legacy
  B64LEGACY,
  /// regexp
  REGEXP,
  /// mime-message
  MIMEMESSAGE,
  /// cbor-any
  CBORANY,
  /// undefined
  UNDEFINED,
  /// newline (used only for comment formatting when compiled with the "lsp"
  /// feature)
  NEWLINE,
}

impl<'a> Token<'a> {
  /// Returns optional string literal of token if it is in the standard prelude
  ///
  /// # Example
  ///
  /// ```
  /// use cddl::token::Token;
  ///
  /// let t = Token::ANY;
  /// assert_eq!(t.in_standard_prelude(), Some("any"));
  /// ```
  pub fn in_standard_prelude(&self) -> Option<&'static str> {
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

/// Range value
#[derive(Debug, PartialEq, Clone)]
pub enum RangeValue<'a> {
  /// Identifier
  IDENT(
    /// Identifier
    &'a str,
    /// Socket/plug
    Option<SocketPlug>,
  ),
  /// Integer
  INT(isize),
  /// Unsigned integer
  UINT(usize),
  /// Float
  FLOAT(f64),
}

impl<'a> TryFrom<Token<'a>> for RangeValue<'a> {
  type Error = &'static str;

  fn try_from(t: Token<'a>) -> Result<Self, Self::Error> {
    match t {
      Token::IDENT(ident, socket) => Ok(RangeValue::IDENT(ident, socket)),
      Token::VALUE(value) => match value {
        Value::INT(i) => Ok(RangeValue::INT(i)),
        Value::UINT(ui) => Ok(RangeValue::UINT(ui)),
        Value::FLOAT(f) => Ok(RangeValue::FLOAT(f)),
        _ => Err("Invalid range token"),
      },
      _ => Err("Invalid range token"),
    }
  }
}

impl<'a> RangeValue<'a> {
  /// Returns `Value` from given `RangeValue`
  pub fn as_value(&self) -> Option<Value> {
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
      RangeValue::IDENT(ident, _) => write!(f, "{}", ident),
      RangeValue::INT(i) => write!(f, "{}", i),
      RangeValue::UINT(i) => write!(f, "{}", i),
      RangeValue::FLOAT(fl) => write!(f, "{}", fl),
    }
  }
}

/// Literal value
// TODO: support hexfloat and exponent
#[cfg_attr(target_arch = "wasm32", derive(Serialize, Deserialize))]
#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
  /// Integer value
  INT(isize),
  /// Unsigned integer value
  UINT(usize),
  /// Float value
  FLOAT(f64),
  /// Text value
  #[cfg_attr(target_arch = "wasm32", serde(borrow))]
  TEXT(Cow<'a, str>),
  /// Byte value
  #[cfg_attr(target_arch = "wasm32", serde(borrow))]
  BYTE(ByteValue<'a>),
}

/// Numeric value
#[derive(Debug, PartialEq)]
pub enum Numeric {
  /// Integer
  INT(isize),
  /// Unsigned integer
  UINT(usize),
  /// Float
  FLOAT(f64),
}

impl<'a> fmt::Display for Value<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Value::TEXT(text) => write!(f, "\"{}\"", text),
      Value::INT(i) => write!(f, "{}", i),
      Value::UINT(ui) => write!(f, "{}", ui),
      Value::FLOAT(float) => write!(f, "{}", float),
      Value::BYTE(bv) => write!(f, "{}", bv),
    }
  }
}

impl<'a> From<&'a str> for Value<'a> {
  fn from(value: &'a str) -> Self {
    Value::TEXT(value.into())
  }
}

/// Byte string values
#[cfg_attr(target_arch = "wasm32", derive(Serialize, Deserialize))]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ByteValue<'a> {
  /// Unprefixed byte string value
  UTF8(Cow<'a, [u8]>),
  /// Prefixed base16 encoded byte string value
  B16(Cow<'a, [u8]>),
  /// Prefixed base64 encoded (URL safe) byte string value
  B64(Cow<'a, [u8]>),
}

impl<'a> fmt::Display for ByteValue<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ByteValue::UTF8(b) => write!(f, "'{}'", std::str::from_utf8(b).map_err(|_| fmt::Error)?),
      ByteValue::B16(b) => write!(
        f,
        "h'{}'",
        String::from_utf8(b.to_vec())
          .map_err(|_| fmt::Error)?
          .replace(' ', "")
      ),
      ByteValue::B64(b) => write!(
        f,
        "b64'{}'",
        String::from_utf8(b.to_vec())
          .map_err(|_| fmt::Error)?
          .replace(' ', "")
      ),
    }
  }
}

/// Socket/plug prefix
#[cfg_attr(target_arch = "wasm32", derive(Serialize, Deserialize))]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SocketPlug {
  /// Type socket `$`
  TYPE,
  /// Group socket `$$`
  GROUP,
}

impl std::str::FromStr for SocketPlug {
  type Err = &'static str;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    if let Some(c) = s.chars().next() {
      if c == '$' {
        if let Some(c) = s.chars().nth(1) {
          if c == '$' {
            return Ok(SocketPlug::GROUP);
          }
        }

        return Ok(SocketPlug::TYPE);
      }
    }

    Err("Malformed socket plug string")
  }
}

impl fmt::Display for SocketPlug {
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
      Token::IDENT(ident, socket_plug) => {
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
      Token::LBRACKET => write!(f, "["),
      Token::RBRACKET => write!(f, "]"),
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
      Token::ARROWMAP => write!(f, "=>"),
      Token::SIZE => write!(f, ".size"),
      Token::BITS => write!(f, ".bits"),
      Token::REGEXP => write!(f, ".regexp"),
      Token::PCRE => write!(f, ".pcre"),
      Token::CBOR => write!(f, ".cbor"),
      Token::CBORSEQ => write!(f, ".cborseq"),
      Token::WITHIN => write!(f, ".within"),
      #[cfg(feature = "additional-controls")]
      Token::CAT => write!(f, ".cat"),
      #[cfg(feature = "additional-controls")]
      Token::DET => write!(f, ".det"),
      #[cfg(feature = "additional-controls")]
      Token::PLUS => write!(f, ".plus"),
      #[cfg(feature = "additional-controls")]
      Token::ABNF => write!(f, ".abnf"),
      #[cfg(feature = "additional-controls")]
      Token::ABNFB => write!(f, ".abnfb"),
      #[cfg(feature = "additional-controls")]
      Token::FEATURE => write!(f, ".feature"),
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
      Token::BYTES => write!(f, "bytes"),
      Token::GCHOICE => write!(f, "//"),
      Token::TRUE => write!(f, "true"),
      Token::GTOCHOICE => write!(f, "&"),
      Token::VALUE(value) => write!(f, "{}", value),
      Token::RANGEOP(i) => {
        if *i {
          write!(f, "..")
        } else {
          write!(f, "...")
        }
      }
      Token::RANGE(l, u, i) => match l {
        RangeValue::IDENT(..) if *i => write!(f, "{} .. {}", l, u),
        RangeValue::IDENT(..) => write!(f, "{} ... {}", l, u),
        _ => {
          if *i {
            write!(f, "{}..{}", l, u)
          } else {
            write!(f, "{}...{}", l, u)
          }
        }
      },
      Token::TAG(mt, tag) => {
        if let Some(m) = mt {
          if let Some(t) = tag {
            return write!(f, "#{}.{}", m, t);
          }

          return write!(f, "#{}", m);
        }
        write!(f, "#")
      }
      _ => write!(f, ""),
    }
  }
}

/// Return an optional control token from a given string
///
/// # Arguments
///
/// `ident` - String slice with ident literal
///
/// # Example
///
/// ```
/// use cddl::token::{lookup_control_from_str, Token};
///
/// assert_eq!(lookup_control_from_str(".size"), Some(Token::SIZE));
/// ```
pub fn lookup_control_from_str<'a>(ident: &str) -> Option<Token<'a>> {
  match ident {
    ".size" => Some(Token::SIZE),
    ".bits" => Some(Token::BITS),
    ".regexp" => Some(Token::REGEXP),
    ".cbor" => Some(Token::CBOR),
    ".cborseq" => Some(Token::CBORSEQ),
    ".within" => Some(Token::WITHIN),
    ".and" => Some(Token::AND),
    ".lt" => Some(Token::LT),
    ".le" => Some(Token::LE),
    ".gt" => Some(Token::GT),
    ".ge" => Some(Token::GE),
    ".eq" => Some(Token::EQ),
    ".ne" => Some(Token::NE),
    ".default" => Some(Token::DEFAULT),
    ".pcre" => Some(Token::PCRE),
    #[cfg(feature = "additional-controls")]
    ".cat" => Some(Token::CAT),
    #[cfg(feature = "additional-controls")]
    ".det" => Some(Token::DET),
    #[cfg(feature = "additional-controls")]
    ".plus" => Some(Token::PLUS),
    #[cfg(feature = "additional-controls")]
    ".abnf" => Some(Token::ABNF),
    #[cfg(feature = "additional-controls")]
    ".abnfb" => Some(Token::ABNFB),
    #[cfg(feature = "additional-controls")]
    ".feature" => Some(Token::FEATURE),
    _ => None,
  }
}

/// Return an optional string from a given token if it is a control operator.
/// Inverse of `lookup_control_from_str`
///
/// # Arguments
///
/// `t` - Reference to a `Token`
///
/// # Example
///
/// ```
/// use cddl::token::{control_str_from_token, Token};
///
/// assert_eq!(control_str_from_token(&Token::SIZE), Some(".size"));
/// ```
pub fn control_str_from_token(t: &Token) -> Option<&'static str> {
  match t {
    Token::SIZE => Some(".size"),
    Token::BITS => Some(".bits"),
    Token::REGEXP => Some(".regexp"),
    Token::CBOR => Some(".cbor"),
    Token::CBORSEQ => Some(".cborseq"),
    Token::WITHIN => Some(".within"),
    Token::AND => Some(".and"),
    Token::LT => Some(".lt"),
    Token::LE => Some(".le"),
    Token::GT => Some(".gt"),
    Token::GE => Some(".ge"),
    Token::EQ => Some(".eq"),
    Token::NE => Some(".ne"),
    Token::DEFAULT => Some(".default"),
    Token::PCRE => Some(".pcre"),
    #[cfg(feature = "additional-controls")]
    Token::CAT => Some(".cat"),
    #[cfg(feature = "additional-controls")]
    Token::DET => Some(".det"),
    #[cfg(feature = "additional-controls")]
    Token::PLUS => Some(".plus"),
    #[cfg(feature = "additional-controls")]
    Token::ABNF => Some(".abnf"),
    #[cfg(feature = "additional-controls")]
    Token::ABNFB => Some(".abnfb"),
    #[cfg(feature = "additional-controls")]
    Token::FEATURE => Some(".feature"),
    _ => None,
  }
}

/// Returns token in standard prelude from given string
///
/// # Arguments
///
/// `ident` - String slice with the token literal
///
/// # Example
///
/// ```
/// use cddl::token::{lookup_ident, Token};
///
/// assert_eq!(lookup_ident("false"), Token::FALSE);
/// ```
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
    "bigint" => Token::BIGINT,
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
    "regexp" => Token::REGEXP,
    "mime-message" => Token::MIMEMESSAGE,
    "cbor-any" => Token::CBORANY,
    "undefined" => Token::UNDEFINED,
    _ => {
      if let Some(c) = ident.chars().next() {
        if c == '$' {
          if let Some(c) = ident.chars().nth(1) {
            if c == '$' {
              return Token::IDENT(&ident[2..], Some(SocketPlug::GROUP));
            }
          }

          return Token::IDENT(&ident[1..], Some(SocketPlug::TYPE));
        }
      }

      Token::IDENT(ident, None)
    }
  }
}

/// If token is an opening delimiter, return its matching closing delimiter
pub fn closing_delimiter<'a>(token: &Token) -> Option<Token<'a>> {
  match token {
    Token::LBRACE => Some(Token::RBRACE),
    Token::LBRACKET => Some(Token::RBRACKET),
    Token::LPAREN => Some(Token::RPAREN),
    Token::LANGLEBRACKET => Some(Token::RANGLEBRACKET),
    _ => None,
  }
}
