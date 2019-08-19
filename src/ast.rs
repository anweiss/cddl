use super::token::{ByteSliceValue, ByteVecValue, RangeValue, SocketPlug, Value};
use std::fmt;

#[cfg(feature = "std")]
use std::borrow::Cow;

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::Cow,
  boxed::Box,
  string::{String, ToString},
  vec::Vec,
};

/// Describes the literal formatting of an AST node
pub trait Node {
  /// Returns an optional formatted token literal string of the AST node
  fn token_literal(&self) -> Option<String>;
}

/// CDDL AST
///
/// ```abnf
/// cddl = S 1*(rule S)
/// ```
#[derive(Default, Debug)]
pub struct CDDL<'a> {
  /// Zero or more production rules
  pub rules: Vec<Rule<'a>>,
}

impl<'a> fmt::Display for CDDL<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut cddl_output = String::new();

    for r in self.rules.iter() {
      cddl_output.push_str(&format!("{}\n\n", r));
    }

    write!(f, "{}", cddl_output)
  }
}

impl<'a> Node for CDDL<'a> {
  fn token_literal(&self) -> Option<String> {
    if !self.rules.is_empty() {
      return self.rules[0].token_literal();
    }

    None
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
#[derive(Debug, PartialEq)]
pub struct Identifier<'a>(pub (&'a str, Option<&'a SocketPlug>));

impl<'a> Node for Identifier<'a> {
  fn token_literal(&self) -> Option<String> {
    Some(format!("{:?}", self))
  }
}

impl<'a> fmt::Display for Identifier<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(sp) = (self.0).1 {
      return write!(f, "{}{}", sp, (self.0).0);
    }

    write!(f, "{}", (self.0).0)
  }
}

impl<'a> From<(&'a str, Option<&'a SocketPlug>)> for Identifier<'a> {
  fn from(ident: (&'a str, Option<&'a SocketPlug>)) -> Self {
    Identifier(ident)
  }
}

impl<'a> From<&'static str> for Identifier<'a> {
  fn from(ident: &'static str) -> Self {
    // TODO: support socketplug
    Identifier((ident, None))
  }
}

/// Type or group expression
///
/// ```abnf
/// rule = typename [genericparm] S assignt S type
///     / groupname [genericparm] S assigng S grpent
/// ```
#[derive(Debug)]
pub enum Rule<'a> {
  /// Type expression
  Type(TypeRule<'a>),
  /// Group expression
  Group(Box<GroupRule<'a>>),
}

impl<'a> fmt::Display for Rule<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Rule::Type(tr) => write!(f, "{}", tr),
      Rule::Group(gr) => write!(f, "{}", gr),
    }
  }
}

impl<'a> Node for Rule<'a> {
  fn token_literal(&self) -> Option<String> {
    match self {
      Rule::Type(tr) => tr.token_literal(),
      Rule::Group(gr) => gr.token_literal(),
    }
  }
}

impl<'a> Rule<'a> {
  /// Returns the name id of a rule
  pub fn name(&self) -> &str {
    match self {
      Rule::Type(tr) => (tr.name.0).0,
      Rule::Group(gr) => (gr.name.0).0,
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
}

/// Type expression
///
/// ```abnf
/// typename [genericparm] S assignt S type
/// ```
#[derive(Debug)]
pub struct TypeRule<'a> {
  /// Type name identifier
  pub name: Identifier<'a>,
  /// Optional generic parameters
  pub generic_param: Option<GenericParm<'a>>,
  /// Extends an existing type choice
  pub is_type_choice_alternate: bool,
  /// Type value
  pub value: Type<'a>,
}

impl<'a> fmt::Display for TypeRule<'a> {
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

impl<'a> Node for TypeRule<'a> {
  fn token_literal(&self) -> Option<String> {
    Some(format!("{:?}", self))
  }
}

/// Group expression
///
/// ```abnf
/// groupname [genericparm] S assigng S grpent
/// ```
#[derive(Debug)]
pub struct GroupRule<'a> {
  /// Group name identifier
  pub name: Identifier<'a>,
  /// Optional generic parameters
  pub generic_param: Option<GenericParm<'a>>,
  /// Extends an existing group choice
  pub is_group_choice_alternate: bool,
  /// Group entry
  pub entry: GroupEntry<'a>,
}

impl<'a> fmt::Display for GroupRule<'a> {
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

impl<'a> Node for GroupRule<'a> {
  fn token_literal(&self) -> Option<String> {
    Some("".into())
  }
}

/// Generic parameters
///
/// ```abnf
/// genericparm =  "<" S id S *("," S id S ) ">"
/// ```
#[derive(Default, Debug)]
pub struct GenericParm<'a>(pub Vec<Identifier<'a>>);

impl<'a> fmt::Display for GenericParm<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut gp = String::from("<");
    for (idx, parm) in self.0.iter().enumerate() {
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
#[derive(Debug)]
pub struct GenericArg<'a>(pub Vec<Type1<'a>>);

impl<'a> fmt::Display for GenericArg<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut ga = String::from("<");
    for (idx, arg) in self.0.iter().enumerate() {
      if idx != 0 {
        ga.push_str(", ");
      }

      ga.push_str(&arg.to_string());
    }

    ga.push('>');

    write!(f, "{}", ga)
  }
}

/// Type choices
///
/// ```abnf
/// type = type1 *(S "/" S  type1)
/// ```
#[derive(Debug)]
pub struct Type<'a>(pub Vec<Type1<'a>>);

impl<'a> fmt::Display for Type<'a> {
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

/// Type with optional range or control operator
///
/// ```abnf
/// type1 = type2 [S (rangeop / ctlop) S type2]
/// ```
#[derive(Debug)]
pub struct Type1<'a> {
  /// Type
  pub type2: Type2<'a>,
  /// Range or control operator over a second type
  pub operator: Option<(RangeCtlOp, Type2<'a>)>,
}

impl<'a> fmt::Display for Type1<'a> {
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
#[derive(Debug, PartialEq)]
pub enum RangeCtlOp {
  /// Range operator where value is `true` if inclusive
  RangeOp(bool),
  /// Control operator where value is the identifier of the operator
  CtlOp(&'static str),
}

impl<'a> fmt::Display for RangeCtlOp {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      RangeCtlOp::RangeOp(false) => write!(f, "..."),
      RangeCtlOp::RangeOp(true) => write!(f, ".."),
      RangeCtlOp::CtlOp(ctrl) => write!(f, ".{}", ctrl),
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
#[derive(Debug)]
pub enum Type2<'a> {
  /// Integer value
  IntValue(isize),
  /// Unsigned integer value
  UintValue(usize),
  /// Float value
  FloatValue(f64),
  /// Text string value (enclosed by '"')
  TextValue(&'a str),
  /// UTF-8 encoded byte string (enclosed by '')
  UTF8ByteString(&'a [u8]),
  /// Base 16 encoded prefixed byte string
  B16ByteString(Cow<'a, [u8]>),
  /// Base 64 encoded (URL safe) prefixed byte string
  B64ByteString(Cow<'a, [u8]>),
  /// Type name identifier with optional generic arguments
  Typename((Identifier<'a>, Option<GenericArg<'a>>)),
  /// Parenthesized type expression (for operator precedence)
  ParenthesizedType(Type<'a>),
  /// Map expression
  Map(Group<'a>),
  /// Array expression
  Array(Group<'a>),
  /// Unwrapped group
  Unwrap((Identifier<'a>, Option<GenericArg<'a>>)),
  /// Enumeration expression over an inline group
  ChoiceFromInlineGroup(Group<'a>),
  /// Enumeration expression over previously defined group
  ChoiceFromGroup((Identifier<'a>, Option<GenericArg<'a>>)),
  /// Tagged data item
  TaggedData((Option<usize>, &'a str)),
  /// Data item of a major type with optional data constraint
  TaggedDataMajorType((u8, Option<usize>)),
  /// Any data item
  Any,
}

impl<'a> fmt::Display for Type2<'a> {
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
      Type2::ParenthesizedType(_t) => unimplemented!(),
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

impl<'a> From<Value<'a>> for Type2<'a> {
  fn from(value: Value<'a>) -> Self {
    match value {
      Value::TEXT(t) => Type2::TextValue(t),
      Value::INT(i) => Type2::IntValue(i),
      Value::UINT(ui) => Type2::UintValue(ui),
      Value::FLOAT(f) => Type2::FloatValue(f),
    }
  }
}

impl<'a> From<RangeValue<'a>> for Type2<'a> {
  fn from(rv: RangeValue<'a>) -> Self {
    match rv {
      RangeValue::IDENT(ident) => Type2::Typename((ident.into(), None)),
      RangeValue::INT(i) => Type2::IntValue(i),
      RangeValue::UINT(ui) => Type2::UintValue(ui),
      RangeValue::FLOAT(f) => Type2::FloatValue(f),
    }
  }
}

impl<'a> From<&ByteSliceValue<'a>> for Type2<'a> {
  fn from(value: &ByteSliceValue<'a>) -> Self {
    match value {
      ByteSliceValue::UTF8(utf8) => Type2::UTF8ByteString(utf8),
      ByteSliceValue::B16(b16) => Type2::B16ByteString(Cow::from(*b16)),
      ByteSliceValue::B64(b64) => Type2::B64ByteString(Cow::from(*b64)),
    }
  }
}

impl<'a> From<ByteVecValue> for Type2<'a> {
  fn from(value: ByteVecValue) -> Self {
    match value {
      ByteVecValue::B16(b16) => Type2::B16ByteString(Cow::from(b16)),
      ByteVecValue::B64(b64) => Type2::B64ByteString(Cow::from(b64)),
    }
  }
}

/// Group choices
///
/// ```abnf
/// group = grpchoice * (S "//" S grpchoice)
/// ```
#[derive(Debug)]
pub struct Group<'a>(pub Vec<GroupChoice<'a>>);

impl<'a> fmt::Display for Group<'a> {
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
#[derive(Debug)]
pub struct GroupChoice<'a>(pub Vec<GroupEntry<'a>>);

impl<'a> fmt::Display for GroupChoice<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.0.len() == 1 {
      return write!(f, "{}", self.0[0]);
    }

    let mut group_entries = String::new();

    for ge in self.0.iter() {
      group_entries.push_str(&format!("\t{},\n", ge));
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
#[derive(Debug)]
pub enum GroupEntry<'a> {
  /// Value group entry type
  ValueMemberKey(Box<ValueMemberKeyEntry<'a>>),
  /// Group entry from a named group or type
  TypeGroupname(TypeGroupnameEntry<'a>),
  /// Parenthesized group with optional occurrence indicator
  InlineGroup((Option<Occur>, Group<'a>)),
}

impl<'a> fmt::Display for GroupEntry<'a> {
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
#[derive(Debug)]
pub struct ValueMemberKeyEntry<'a> {
  /// Optional occurrence indicator
  pub occur: Option<Occur>,
  /// Optional member key
  pub member_key: Option<MemberKey<'a>>,
  /// Entry type
  pub entry_type: Type<'a>,
}

impl<'a> fmt::Display for ValueMemberKeyEntry<'a> {
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
#[derive(Debug)]
pub struct TypeGroupnameEntry<'a> {
  /// Optional occurrence indicator
  pub occur: Option<Occur>,
  /// Type or group name identifier
  pub name: Identifier<'a>,
  /// Optional generic arguments
  pub generic_arg: Option<GenericArg<'a>>,
}

impl<'a> fmt::Display for TypeGroupnameEntry<'a> {
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
#[derive(Debug)]
pub enum MemberKey<'a> {
  /// Type expression. If second value in tuple is `true`, a cut is present
  Type1(Box<(Type1<'a>, bool)>),
  /// Bareword string type
  Bareword(Identifier<'a>),
  /// Value type
  Value(Value<'a>),
}

impl<'a> fmt::Display for MemberKey<'a> {
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
#[derive(Debug)]
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
        GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
          occur: None,
          member_key: Some(MemberKey::Bareword("key1".into())),
          entry_type: Type(vec![Type1 {
            type2: Type2::TextValue("value1"),
            operator: None,
          }]),
        })),
        GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
          occur: None,
          member_key: Some(MemberKey::Bareword("key2".into())),
          entry_type: Type(vec![Type1 {
            type2: Type2::TextValue("value2"),
            operator: None,
          }]),
        })),
      ])])
      .to_string(),
      "\tkey1: \"value1\",\n\tkey2: \"value2\",\n".to_string()
    )
  }
}
