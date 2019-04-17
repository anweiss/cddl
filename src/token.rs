use std::fmt;

#[derive(PartialEq, Debug)]
pub enum Token<'a> {
  ILLEGAL,
  EOF,

  IDENT(&'a str),
  VALUE(Value<'a>),
  INTLITERAL(usize),
  FLOATLITERAL(f64),
  TAG((usize, &'a str)),

  // Operators
  ASSIGN,
  OPTIONAL,
  ASTERISK,
  OCCURENCE((usize, usize)),
  PLUS,
  UNWRAP,
  CONTROL(&'a str),

  // Delimiters
  COMMA,
  COLON,
  SEMICOLON,

  TCHOICE,
  GCHOICE,
  TCHOICEALT,
  GCHOICEALT,
  ARROWMAP,
  CUT,
  TSOCKET,
  GSOCKET,

  RANGE((Box<Token<'a>>, Box<Token<'a>>, bool)),

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
  pub fn as_value(&self) -> Option<Value<'a>> {
    match &self {
      Token::INTLITERAL(i) => Some(Value::INT(*i)),
      Token::FLOATLITERAL(f) => Some(Value::FLOAT(*f)),
      _ => None,
    }
  }
}

#[derive(Debug, PartialEq)]
pub enum Value<'a> {
  // TODO: verify HEXFLOAT works
  INT(usize),
  FLOAT(f64),
  TEXT(&'a str),

  // TODO: support raw byte string
  BYTES(&'a str),
}

impl<'a> fmt::Display for Value<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Value::TEXT(text) => write!(f, "{}", text),
      _ => write!(f, ""),
    }
  }
}

impl<'a> fmt::Display for Token<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Token::IDENT(ident) => write!(f, "{}", ident),
      Token::ILLEGAL => write!(f, ""),
      Token::ASSIGN => write!(f, "="),
      Token::PLUS => write!(f, "+"),
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
      Token::SEMICOLON => write!(f, ";"),
      Token::COLON => write!(f, ":"),
      Token::CUT => write!(f, "^"),
      Token::EOF => write!(f, ""),
      Token::TSTR => write!(f, "tstr"),
      Token::LANGLEBRACKET => write!(f, "<"),
      Token::RANGLEBRACKET => write!(f, ">"),
      Token::INT => write!(f, "int"),
      Token::UINT => write!(f, "uint"),
      Token::INTLITERAL(i) => write!(f, "{}", i),
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
      Token::TAG((tn, tt)) => {
        if *tt != "" {
          return write!(f, "#6.{}({})", tn, tt);
        }

        write!(f, "#6.{}", tn)
      }
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
    _ => Token::ILLEGAL,
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
    _ => Token::IDENT(ident),
  }
}
