use super::token::{ByteValue, RangeValue, SocketPlug, Value};
use std::fmt;

#[cfg(target_arch = "wasm32")]
use serde::Serialize;

#[cfg(not(feature = "std"))]
use alloc::{
  boxed::Box,
  string::{String, ToString},
  vec::Vec,
};

/// Starting index, ending index and line number
pub type Span = (usize, usize, usize);

/// CDDL AST
///
/// ```abnf
/// cddl = S 1*(rule S)
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Default, Debug, PartialEq)]
pub struct CDDL {
  /// Zero or more production rules
  pub rules: Vec<Rule>,
}

impl fmt::Display for CDDL {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut cddl_output = String::new();

    for rule in self.rules.iter() {
      cddl_output.push_str(&format!("{}\n\n", rule));
    }

    write!(f, "{}", cddl_output)
  }
}

/// Identifier for a type name, group name or bareword, with an optional socket
///
/// ```abnf
/// id = EALPHA *(*("-" / ".") (EALPHA / DIGIT))
/// ALPHA = %x41-5A / %x61-7A
/// EALPHA = ALPHA / "@" / "_" / "$"
/// DIGIT = %x30-39
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
  /// Identifier
  pub ident: String,
  /// Optional socket
  pub socket: Option<SocketPlug>,
  /// Span
  pub span: Span,
}

impl fmt::Display for Identifier {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(sp) = &self.socket {
      return write!(f, "{}{}", sp, self.ident);
    }

    write!(f, "{}", self.ident)
  }
}

impl From<&'static str> for Identifier {
  fn from(ident: &'static str) -> Self {
    let mut socket = ident.chars().take(2);

    if let Some(c) = socket.next() {
      if c == '$' {
        if let Some(c) = socket.next() {
          if c == '$' {
            return Identifier {
              ident: ident.into(),
              socket: Some(SocketPlug::GROUP),
              span: (0, 0, 0),
            };
          }
        }

        return Identifier {
          ident: ident.into(),
          socket: Some(SocketPlug::TYPE),
          span: (0, 0, 0),
        };
      }
    }

    Identifier {
      ident: ident.into(),
      socket: None,
      span: (0, 0, 0),
    }
  }
}

/// Type or group expression
///
/// ```abnf
/// rule = typename [genericparm] S assignt S type
///     / groupname [genericparm] S assigng S grpent
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq)]
pub enum Rule {
  /// Type expression
  Type(TypeRule),
  /// Group expression
  Group(Box<GroupRule>),
}

impl fmt::Display for Rule {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Rule::Type(tr) => write!(f, "{}", tr),
      Rule::Group(gr) => write!(f, "{}", gr),
    }
  }
}

impl Rule {
  /// Returns the name id of a rule
  pub fn name(&self) -> String {
    match self {
      Rule::Type(tr) => tr.name.ident.to_string(),
      Rule::Group(gr) => gr.name.ident.to_string(),
    }
  }

  /// Returns whether or not a rule extends an existing type or group rule with
  /// additional choices
  pub fn is_choice_alternate(&self) -> bool {
    match self {
      Rule::Type(tr) => tr.is_type_choice_alternate,
      Rule::Group(gr) => gr.is_group_choice_alternate,
    }
  }

  /// Returns the beginning and ending range indices of a rule
  pub fn span(&self) -> Span {
    match self {
      Rule::Type(tr) => tr.span,
      Rule::Group(gr) => gr.span,
    }
  }
}

/// Type expression
///
/// ```abnf
/// typename [genericparm] S assignt S type
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq)]
pub struct TypeRule {
  /// Type name identifier
  pub name: Identifier,
  /// Optional generic parameters
  pub generic_param: Option<GenericParm>,
  /// Extends an existing type choice
  pub is_type_choice_alternate: bool,
  /// Type value
  pub value: Type,
  /// Span
  pub span: Span,
}

impl fmt::Display for TypeRule {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut tr_output = self.name.to_string();

    if let Some(gp) = &self.generic_param {
      tr_output.push_str(&gp.to_string());
    }

    if self.is_type_choice_alternate {
      tr_output.push_str(" /= ");
    } else {
      tr_output.push_str(" = ");
    }

    tr_output.push_str(&self.value.to_string());

    write!(f, "{}", tr_output)
  }
}

/// Group expression
///
/// ```abnf
/// groupname [genericparm] S assigng S grpent
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq)]
pub struct GroupRule {
  /// Group name identifier
  pub name: Identifier,
  /// Optional generic parameters
  pub generic_param: Option<GenericParm>,
  /// Extends an existing group choice
  pub is_group_choice_alternate: bool,
  /// Group entry
  pub entry: GroupEntry,
  /// Span
  pub span: Span,
}

impl fmt::Display for GroupRule {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut gr_output = self.name.to_string();

    if let Some(gp) = &self.generic_param {
      gr_output.push_str(&gp.to_string());
    }

    if self.is_group_choice_alternate {
      gr_output.push_str(" //= ");
    } else {
      gr_output.push_str(" = ");
    }

    gr_output.push_str(&self.entry.to_string());

    write!(f, "{}", gr_output)
  }
}

/// Generic parameters
///
/// ```abnf
/// genericparm =  "<" S id S *("," S id S ) ">"
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Default, PartialEq)]
pub struct GenericParm {
  /// List of generic parameters
  pub params: Vec<Identifier>,
  /// Span
  pub span: Span,
}

impl fmt::Display for GenericParm {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut gp = String::from("<");
    for (idx, parm) in self.params.iter().enumerate() {
      if idx != 0 {
        gp.push_str(", ");
      }

      gp.push_str(&parm.to_string());
    }

    gp.push('>');

    write!(f, "{}", gp)
  }
}

/// Generic arguments
///
/// ```abnf
/// genericarg = "<" S type1 S *("," S type1 S )  ">"
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct GenericArg {
  /// Generic arguments
  pub args: Vec<Type1>,
  /// Span
  pub span: Span,
}

impl fmt::Display for GenericArg {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut ga = String::from("<");
    for (idx, arg) in self.args.iter().enumerate() {
      if idx != 0 {
        ga.push_str(", ");
      }

      ga.push_str(&arg.to_string());
    }

    ga.push('>');

    write!(f, "{}", ga)
  }
}

impl GenericArg {
  pub fn new() -> Self {
    GenericArg {
      args: Vec::new(),
      span: (0, 0, 0),
    }
  }
}

/// Type choices
///
/// ```abnf
/// type = type1 *(S "/" S  type1)
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Type(pub Vec<Type1>);

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut types = String::new();

    for (idx, t) in self.0.iter().enumerate() {
      if idx == 0 {
        types.push_str(&t.to_string());
        continue;
      }

      types.push_str(&format!(" / {}", t.to_string()));
    }

    write!(f, "{}", types)
  }
}

impl Type {
  /// Used to delineate between grpent with `Type` and group entry with group
  /// name identifier `id`
  pub fn groupname_entry(&self) -> Option<(Identifier, Option<GenericArg>)> {
    if self.0.len() == 1 {
      if let Some(t1) = self.0.first() {
        if t1.operator.is_none() {
          if let Type2::Typename((ident, ga)) = &t1.type2 {
            return Some((ident.clone(), ga.clone()));
          }
        }
      }
    }

    None
  }
}

/// Type with optional range or control operator
///
/// ```abnf
/// type1 = type2 [S (rangeop / ctlop) S type2]
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Type1 {
  /// Type
  pub type2: Type2,
  /// Range or control operator over a second type
  pub operator: Option<(RangeCtlOp, Type2)>,
}

impl fmt::Display for Type1 {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut t1 = String::new();

    t1.push_str(&self.type2.to_string());

    if let Type2::Typename(_) = self.type2 {
      if self.operator.is_some() {
        t1.push_str(" ");
      }
    }

    if let Some((rco, t2)) = &self.operator {
      t1.push_str(&rco.to_string());

      if let Type2::Typename(_) = self.type2 {
        t1.push_str(" ");
      }

      t1.push_str(&t2.to_string());
    }

    write!(f, "{}", t1)
  }
}

/// Range or control operator
///
/// ```abnf
/// rangeop = "..." / ".."
/// ctlop = "." id
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq, Clone)]
pub enum RangeCtlOp {
  /// Range operator where value is `true` if inclusive
  RangeOp(bool),
  /// Control operator where value is the identifier of the operator
  CtlOp(&'static str),
}

impl fmt::Display for RangeCtlOp {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      RangeCtlOp::RangeOp(false) => write!(f, "..."),
      RangeCtlOp::RangeOp(true) => write!(f, ".."),
      RangeCtlOp::CtlOp(ctrl) => write!(f, "{}", ctrl),
    }
  }
}

/// Type
///
/// ```abnf
/// type2 = value
///     / typename [genericarg]
///     / "(" S type S ")"
///     / "{" S group S "}"
///     / "[" S group S "]"
///     / "~" S typename [genericarg]
///     / "&" S "(" S group S ")"
///     / "&" S groupname [genericarg]
///     / "#" "6" ["." uint] "(" S type S ")"
///     / "#" DIGIT ["." uint]                ; major/ai
///     / "#"                                 ; any
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum Type2 {
  /// Integer value
  IntValue(isize),
  /// Unsigned integer value
  UintValue(usize),
  /// Float value
  FloatValue(f64),
  /// Text string value (enclosed by '"')
  TextValue(String),
  /// UTF-8 encoded byte string (enclosed by '')
  UTF8ByteString(Vec<u8>),
  /// Base 16 encoded prefixed byte string
  B16ByteString(Vec<u8>),
  /// Base 64 encoded (URL safe) prefixed byte string
  B64ByteString(Vec<u8>),
  /// Type name identifier with optional generic arguments
  Typename((Identifier, Option<GenericArg>)),
  /// Parenthesized type expression (for operator precedence)
  ParenthesizedType(Type),
  /// Map expression
  Map(Group),
  /// Array expression
  Array(Group),
  /// Unwrapped group
  Unwrap((Identifier, Option<GenericArg>)),
  /// Enumeration expression over an inline group
  ChoiceFromInlineGroup(Group),
  /// Enumeration expression over previously defined group
  ChoiceFromGroup((Identifier, Option<GenericArg>)),
  /// Tagged data item where the first element is an optional tag and the second
  /// is the type of the tagged value
  TaggedData((Option<usize>, Type)),
  /// Data item of a major type with optional data constraint
  TaggedDataMajorType((u8, Option<usize>)),
  /// Any data item
  Any,
}

impl fmt::Display for Type2 {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Type2::IntValue(value) => write!(f, "{}", value),
      Type2::UintValue(value) => write!(f, "{}", value),
      Type2::FloatValue(value) => write!(f, "{}", value),
      Type2::TextValue(value) => write!(f, "\"{}\"", value),
      Type2::UTF8ByteString(value) => write!(
        f,
        "'{}'",
        std::str::from_utf8(value).map_err(|_| fmt::Error)?
      ),
      Type2::B16ByteString(value) => {
        write!(f, "{}", std::str::from_utf8(value).map_err(|_| fmt::Error)?)
      }
      Type2::B64ByteString(value) => {
        write!(f, "{}", std::str::from_utf8(value).map_err(|_| fmt::Error)?)
      }
      Type2::Typename((tn, ga)) => {
        if let Some(args) = ga {
          return write!(f, "{}{}", tn, args);
        }

        write!(f, "{}", tn)
      }
      Type2::ParenthesizedType(t) => write!(f, "({})", t),
      Type2::Map(g) => write!(f, "{{{}}}", g),
      Type2::Array(g) => write!(f, "[{}]", g),
      Type2::Unwrap((ident, ga)) => {
        if let Some(args) = ga {
          return write!(f, "{}{}", ident, args);
        }

        write!(f, "{}", ident)
      }
      Type2::ChoiceFromInlineGroup(g) => write!(f, "&({})", g),
      Type2::ChoiceFromGroup((ident, generic_arg)) => {
        if let Some(ga) = generic_arg {
          return write!(f, "&{}{}", ident, ga);
        }

        write!(f, "&{}", ident)
      }
      Type2::TaggedData((tag_uint, tagged_value)) => {
        if let Some(t) = tag_uint {
          return write!(f, "#6.{}({})", t, tagged_value);
        }

        write!(f, "#6({})", tagged_value)
      }
      Type2::TaggedDataMajorType((major_type, tag_uint)) => {
        if let Some(t) = tag_uint {
          return write!(f, "{}.{}", major_type, t);
        }

        write!(f, "{}", major_type)
      }
      Type2::Any => write!(f, "#"),
    }
  }
}

impl From<&Value> for Type2 {
  fn from(value: &Value) -> Self {
    match value {
      Value::TEXT(t) => Type2::TextValue(t.to_string()),
      Value::INT(i) => Type2::IntValue(*i),
      Value::UINT(ui) => Type2::UintValue(*ui),
      Value::FLOAT(f) => Type2::FloatValue(*f),
      Value::BYTE(bv) => bv.into(),
    }
  }
}

impl From<RangeValue> for Type2 {
  fn from(rv: RangeValue) -> Self {
    match rv {
      RangeValue::IDENT(ident) => Type2::Typename((
        Identifier {
          ident: ident.0,
          socket: ident.1,
          span: (0, 0, 0),
        },
        None,
      )),
      RangeValue::INT(i) => Type2::IntValue(i),
      RangeValue::UINT(ui) => Type2::UintValue(ui),
      RangeValue::FLOAT(f) => Type2::FloatValue(f),
    }
  }
}

impl From<&ByteValue> for Type2 {
  fn from(value: &ByteValue) -> Self {
    match value {
      ByteValue::UTF8(utf8) => Type2::UTF8ByteString(utf8.to_vec()),
      ByteValue::B16(b16) => Type2::B16ByteString(b16.to_vec()),
      ByteValue::B64(b64) => Type2::B64ByteString(b64.to_vec()),
    }
  }
}

/// Group choices
///
/// ```abnf
/// group = grpchoice * (S "//" S grpchoice)
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Group(pub Vec<GroupChoice>);

impl fmt::Display for Group {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut group_choices = String::new();

    for (idx, gc) in self.0.iter().enumerate() {
      if idx == 0 {
        group_choices.push_str(&gc.to_string());
        continue;
      }

      group_choices.push_str(&format!(" / {}", gc));
    }

    write!(f, "{}", group_choices)
  }
}

/// Group entries
///
/// ```abnf
/// grpchoice = *(grpent optcom)
/// ```
///
/// If tuple is true, then entry is marked by a trailing comma
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct GroupChoice(pub Vec<(GroupEntry, bool)>);

impl fmt::Display for GroupChoice {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.0.len() == 1 {
      return write!(f, "{}", self.0[0].0);
    }

    let mut group_entries = String::new();

    for ge in self.0.iter() {
      if ge.1 {
        group_entries.push_str(&format!("\t{},\n", ge.0));
      } else {
        group_entries.push_str(&format!("\t{}\n", ge.0));
      }
    }

    write!(f, "{}", group_entries)
  }
}

/// Group entry
///
/// ```abnf
/// grpent = [occur S] [memberkey S] type
///       / [occur S] groupname [genericarg]  ; preempted by above
///       / [occur S] "(" S group S ")"
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum GroupEntry {
  /// Value group entry type
  ValueMemberKey(Box<ValueMemberKeyEntry>),
  /// Group entry from a named group or type
  TypeGroupname(TypeGroupnameEntry),
  /// Parenthesized group with optional occurrence indicator
  InlineGroup((Option<Occur>, Group)),
}

impl fmt::Display for GroupEntry {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      GroupEntry::ValueMemberKey(vmke) => write!(f, "{}", vmke),
      GroupEntry::TypeGroupname(gne) => write!(f, "{}", gne),
      GroupEntry::InlineGroup((occur, group)) => {
        if let Some(o) = occur {
          return write!(f, "{} ({})", o, group);
        }

        write!(f, "({})", group)
      }
    }
  }
}

/// Value group entry type with optional occurrence indicator and optional
/// member key
///
/// ```abnf
/// [occur S] [memberkey S] type
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ValueMemberKeyEntry {
  /// Optional occurrence indicator
  pub occur: Option<Occur>,
  /// Optional member key
  pub member_key: Option<MemberKey>,
  /// Entry type
  pub entry_type: Type,
}

impl fmt::Display for ValueMemberKeyEntry {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(o) = &self.occur {
      if let Some(mk) = &self.member_key {
        return write!(f, "{} {} {}", o, mk, self.entry_type);
      }

      return write!(f, "{} {}", o, self.entry_type);
    }

    if let Some(mk) = &self.member_key {
      return write!(f, "{} {}", mk, self.entry_type);
    }

    write!(f, "{}", self.entry_type)
  }
}

/// Group entry from a named type or group
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TypeGroupnameEntry {
  /// Optional occurrence indicator
  pub occur: Option<Occur>,
  /// Type or group name identifier
  pub name: Identifier,
  /// Optional generic arguments
  pub generic_arg: Option<GenericArg>,
}

impl fmt::Display for TypeGroupnameEntry {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(o) = &self.occur {
      if let Some(ga) = &self.generic_arg {
        return write!(f, "{} {} {}", o, self.name, ga);
      }

      return write!(f, "{} {}", o, self.name);
    }

    if let Some(ga) = &self.generic_arg {
      return write!(f, "{} {}", self.name, ga);
    }

    write!(f, "{}", self.name)
  }
}

/// Member key
/// ```abnf
/// memberkey = type1 S ["^" S] "=>"
///           / bareword S ":"
///           / value S ":"
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum MemberKey {
  /// Type expression. If second value in tuple is `true`, a cut is present
  Type1(Box<(Type1, bool)>),
  /// Bareword string type
  Bareword(Identifier),
  /// Value type
  Value(Value),
}

impl fmt::Display for MemberKey {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      MemberKey::Type1(t1) => {
        if t1.1 {
          return write!(f, "{} ^ =>", t1.0);
        }

        write!(f, "{} =>", t1.0)
      }
      MemberKey::Bareword(ident) => write!(f, "{}:", ident),
      MemberKey::Value(value) => write!(f, "{}:", value),
    }
  }
}

/// Occurrence indicator
/// ```abnf
/// occur = [uint] "*" [uint]
///       / "+"
///       / "?"
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum Occur {
  /// Occurrence indicator in the form n*m, where n is an optional lower limit
  /// and m is an optional upper limit
  Exact((Option<usize>, Option<usize>)),
  /// Occurrence indicator in the form *, allowing zero or more occurrences
  ZeroOrMore,
  /// Occurrence indicator in the form +, allowing one or more occurrences
  OneOrMore,
  /// Occurrence indicator in the form ?, allowing an optional occurrence
  Optional,
}

impl fmt::Display for Occur {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Occur::ZeroOrMore => write!(f, "*"),
      Occur::Exact((l, u)) => {
        if let Some(li) = l {
          if let Some(ui) = u {
            return write!(f, "{}*{}", li, ui);
          }

          return write!(f, "{}*", li);
        }

        if let Some(ui) = u {
          return write!(f, "*{}", ui);
        }

        write!(f, "*")
      }
      Occur::OneOrMore => write!(f, "+"),
      Occur::Optional => write!(f, "?"),
    }
  }
}

#[cfg(test)]
#[allow(unused_imports)]
mod tests {
  use super::*;

  #[test]
  fn verify_groupentry_output() {
    assert_eq!(
      GroupEntry::TypeGroupname(TypeGroupnameEntry {
        occur: None,
        name: Identifier::from("entry1"),
        generic_arg: None,
      })
      .to_string(),
      "entry1".to_string()
    )
  }

  #[test]
  fn verify_group_output() {
    assert_eq!(
      Group(vec![GroupChoice(vec![
        (
          GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
            occur: None,
            member_key: Some(MemberKey::Bareword("key1".into())),
            entry_type: Type(vec![Type1 {
              type2: Type2::TextValue("value1".into()),
              operator: None,
            }]),
          })),
          true
        ),
        (
          GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
            occur: None,
            member_key: Some(MemberKey::Bareword("key2".into())),
            entry_type: Type(vec![Type1 {
              type2: Type2::TextValue("value2".into()),
              operator: None,
            }]),
          })),
          true
        ),
      ])])
      .to_string(),
      "\tkey1: \"value1\",\n\tkey2: \"value2\",\n".to_string()
    )
  }
}
