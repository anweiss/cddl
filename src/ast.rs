use super::token::{RangeValue, SocketPlug, Value};
use std::fmt;

#[cfg(feature = "std")]
use std::borrow::Cow;

#[cfg(target_arch = "wasm32")]
use serde::Serialize;

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::Cow,
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
pub struct CDDL<'a> {
  /// Zero or more production rules
  pub rules: Vec<Rule<'a>>,
}

impl<'a> fmt::Display for CDDL<'a> {
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
pub struct Identifier<'a> {
  /// Identifier
  pub ident: &'a str,
  /// Optional socket
  pub socket: Option<SocketPlug>,
  /// Span
  pub span: Span,
}

impl<'a> fmt::Display for Identifier<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(sp) = &self.socket {
      return write!(f, "{}{}", sp, self.ident);
    }

    write!(f, "{}", self.ident)
  }
}

impl<'a> From<&'static str> for Identifier<'a> {
  fn from(ident: &'static str) -> Self {
    let mut socket = ident.chars().take(2);

    if let Some(c) = socket.next() {
      if c == '$' {
        if let Some(c) = socket.next() {
          if c == '$' {
            return Identifier {
              ident,
              socket: Some(SocketPlug::GROUP),
              span: (0, 0, 0),
            };
          }
        }

        return Identifier {
          ident,
          socket: Some(SocketPlug::TYPE),
          span: (0, 0, 0),
        };
      }
    }

    Identifier {
      ident,
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
#[allow(missing_docs)]
pub enum Rule<'a> {
  /// Type expression
  Type { rule: TypeRule<'a>, span: Span },
  /// Group expression
  Group {
    rule: Box<GroupRule<'a>>,
    span: Span,
  },
}

impl<'a> Rule<'a> {
  /// Return `Span` for `Rule`
  pub fn span(&self) -> Span {
    match self {
      Rule::Type { span, .. } => *span,
      Rule::Group { span, .. } => *span,
    }
  }
}

impl<'a> fmt::Display for Rule<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Rule::Type { rule, .. } => write!(f, "{}", rule),
      Rule::Group { rule, .. } => write!(f, "{}", rule),
    }
  }
}

impl<'a> Rule<'a> {
  /// Returns the name id of a rule
  pub fn name(&self) -> String {
    match self {
      Rule::Type { rule, .. } => rule.name.ident.to_string(),
      Rule::Group { rule, .. } => rule.name.ident.to_string(),
    }
  }

  /// Returns whether or not a rule extends an existing type or group rule with
  /// additional choices
  pub fn is_choice_alternate(&self) -> bool {
    match self {
      Rule::Type { rule, .. } => rule.is_type_choice_alternate,
      Rule::Group { rule, .. } => rule.is_group_choice_alternate,
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

/// Group expression
///
/// ```abnf
/// groupname [genericparm] S assigng S grpent
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq)]
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

/// Generic parameters
///
/// ```abnf
/// genericparm =  "<" S id S *("," S id S ) ">"
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Default, PartialEq)]
pub struct GenericParm<'a> {
  /// List of generic parameters
  pub params: Vec<Identifier<'a>>,
  /// Span
  pub span: Span,
}

impl<'a> fmt::Display for GenericParm<'a> {
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
pub struct GenericArg<'a> {
  /// Generic arguments
  pub args: Vec<Type1<'a>>,
  /// Span
  pub span: Span,
}

impl<'a> fmt::Display for GenericArg<'a> {
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

impl<'a> GenericArg<'a> {
  /// Default `GenericArg`
  pub fn default() -> Self {
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
pub struct Type<'a> {
  /// Type choices
  pub type_choices: Vec<Type1<'a>>,
  /// Span
  pub span: Span,
}

impl<'a> fmt::Display for Type<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut types = String::new();

    for (idx, t1) in self.type_choices.iter().enumerate() {
      if idx == 0 {
        types.push_str(&t1.to_string());
        continue;
      }

      types.push_str(&format!(" / {}", t1.to_string()));
    }

    write!(f, "{}", types)
  }
}

impl<'a> Type<'a> {
  /// Used to delineate between grpent with `Type` and group entry with group
  /// name identifier `id`
  pub fn groupname_entry(&self) -> Option<(Identifier<'a>, Option<GenericArg<'a>>, Span)> {
    if self.type_choices.len() == 1 {
      if let Some(t1) = self.type_choices.first() {
        if t1.operator.is_none() {
          if let Type2::Typename {
            ident,
            generic_arg,
            span,
          } = &t1.type2
          {
            return Some((ident.clone(), generic_arg.clone(), *span));
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
pub struct Type1<'a> {
  /// Type
  pub type2: Type2<'a>,
  /// Range or control operator over a second type
  pub operator: Option<(RangeCtlOp, Type2<'a>)>,
  /// Span
  pub span: Span,
}

impl<'a> fmt::Display for Type1<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut t1 = String::new();

    t1.push_str(&self.type2.to_string());

    if let Type2::Typename { .. } = self.type2 {
      if self.operator.is_some() {
        t1.push_str(" ");
      }
    }

    if let Some((rco, t2)) = &self.operator {
      t1.push_str(&rco.to_string());

      if let Type2::Typename { .. } = self.type2 {
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
#[allow(missing_docs)]
pub enum RangeCtlOp {
  /// Range operator
  RangeOp { is_inclusive: bool, span: Span },
  /// Control operator
  CtlOp { ctrl: &'static str, span: Span },
}

impl fmt::Display for RangeCtlOp {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      RangeCtlOp::RangeOp {
        is_inclusive: false,
        ..
      } => write!(f, "..."),
      RangeCtlOp::RangeOp {
        is_inclusive: true, ..
      } => write!(f, ".."),
      RangeCtlOp::CtlOp { ctrl, .. } => write!(f, "{}", ctrl),
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
#[allow(missing_docs)]
pub enum Type2<'a> {
  /// Integer value
  IntValue { value: isize, span: Span },
  /// Unsigned integer value
  UintValue { value: usize, span: Span },
  /// Float value
  FloatValue { value: f64, span: Span },
  /// Text string value (enclosed by '"')
  TextValue { value: &'a str, span: Span },
  /// UTF-8 encoded byte string (enclosed by '')
  UTF8ByteString { value: Cow<'a, [u8]>, span: Span },
  /// Base 16 encoded prefixed byte string
  B16ByteString { value: Cow<'a, [u8]>, span: Span },
  /// Base 64 encoded (URL safe) prefixed byte string
  B64ByteString { value: Cow<'a, [u8]>, span: Span },
  /// Type name identifier with optional generic arguments
  Typename {
    ident: Identifier<'a>,
    generic_arg: Option<GenericArg<'a>>,
    span: Span,
  },
  /// Parenthesized type expression (for operator precedence)
  ParenthesizedType { pt: Type<'a>, span: Span },
  /// Map expression
  Map { group: Group<'a>, span: Span },
  /// Array expression
  Array { group: Group<'a>, span: Span },
  /// Unwrapped group
  Unwrap {
    ident: Identifier<'a>,
    generic_arg: Option<GenericArg<'a>>,
    span: Span,
  },
  /// Enumeration expression over an inline group
  ChoiceFromInlineGroup { group: Group<'a>, span: Span },
  /// Enumeration expression over previously defined group
  ChoiceFromGroup {
    ident: Identifier<'a>,
    generic_arg: Option<GenericArg<'a>>,
    span: Span,
  },
  /// Tagged data item where the first element is an optional tag and the second
  /// is the type of the tagged value
  TaggedData {
    tag: Option<usize>,
    t: Type<'a>,
    span: Span,
  },
  /// Data item of a major type with optional data constraint
  TaggedDataMajorType {
    mt: u8,
    constraint: Option<usize>,
    span: Span,
  },
  /// Any data item
  Any(Span),
}

impl<'a> fmt::Display for Type2<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Type2::IntValue { value, .. } => write!(f, "{}", value),
      Type2::UintValue { value, .. } => write!(f, "{}", value),
      Type2::FloatValue { value, .. } => write!(f, "{}", value),
      Type2::TextValue { value, .. } => write!(f, "\"{}\"", value),
      Type2::UTF8ByteString { value, .. } => write!(
        f,
        "'{}'",
        std::str::from_utf8(value).map_err(|_| fmt::Error)?
      ),
      Type2::B16ByteString { value, .. } => {
        write!(f, "{}", std::str::from_utf8(value).map_err(|_| fmt::Error)?)
      }
      Type2::B64ByteString { value, .. } => {
        write!(f, "{}", std::str::from_utf8(value).map_err(|_| fmt::Error)?)
      }
      Type2::Typename {
        ident, generic_arg, ..
      } => {
        if let Some(args) = generic_arg {
          return write!(f, "{}{}", ident, args);
        }

        write!(f, "{}", ident)
      }
      Type2::ParenthesizedType { pt, .. } => write!(f, "({})", pt),
      Type2::Map { group, .. } => write!(f, "{{{}}}", group),
      Type2::Array { group, .. } => write!(f, "[{}]", group),
      Type2::Unwrap {
        ident, generic_arg, ..
      } => {
        if let Some(args) = generic_arg {
          return write!(f, "{}{}", ident, args);
        }

        write!(f, "{}", ident)
      }
      Type2::ChoiceFromInlineGroup { group, .. } => write!(f, "&({})", group),
      Type2::ChoiceFromGroup {
        ident, generic_arg, ..
      } => {
        if let Some(ga) = generic_arg {
          return write!(f, "&{}{}", ident, ga);
        }

        write!(f, "&{}", ident)
      }
      Type2::TaggedData { tag, t, .. } => {
        if let Some(tag_uint) = tag {
          return write!(f, "#6.{}({})", tag_uint, t);
        }

        write!(f, "#6({})", t)
      }
      Type2::TaggedDataMajorType { mt, constraint, .. } => {
        if let Some(c) = constraint {
          return write!(f, "{}.{}", mt, c);
        }

        write!(f, "{}", mt)
      }
      Type2::Any(_) => write!(f, "#"),
    }
  }
}

impl<'a> From<RangeValue<'a>> for Type2<'a> {
  fn from(rv: RangeValue<'a>) -> Self {
    let span = (0, 0, 0);

    match rv {
      RangeValue::IDENT(ident) => Type2::Typename {
        ident: Identifier {
          ident: ident.0,
          socket: ident.1,
          span,
        },
        generic_arg: None,
        span,
      },
      RangeValue::INT(value) => Type2::IntValue { value, span },
      RangeValue::UINT(value) => Type2::UintValue { value, span },
      RangeValue::FLOAT(value) => Type2::FloatValue { value, span },
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
#[allow(missing_docs)]
pub struct Group<'a> {
  /// Group choices
  pub group_choices: Vec<GroupChoice<'a>>,
  pub span: Span,
}

impl<'a> fmt::Display for Group<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut group_choices = String::new();

    for (idx, gc) in self.group_choices.iter().enumerate() {
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
pub struct GroupChoice<'a> {
  /// Group entries where the second item in the tuple indicates where or not a
  /// trailing comma is present
  pub group_entries: Vec<(GroupEntry<'a>, bool)>,
  /// Span
  pub span: Span,
}

impl<'a> fmt::Display for GroupChoice<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.group_entries.len() == 1 {
      return write!(f, "{}", self.group_entries[0].0);
    }

    let mut group_entries = String::new();

    for ge in self.group_entries.iter() {
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
#[allow(missing_docs)]
pub enum GroupEntry<'a> {
  /// Value group entry type
  ValueMemberKey {
    ge: Box<ValueMemberKeyEntry<'a>>,
    span: Span,
  },
  /// Group entry from a named group or type
  TypeGroupname {
    ge: TypeGroupnameEntry<'a>,
    span: Span,
  },
  /// Parenthesized group with optional occurrence indicator
  InlineGroup {
    occur: Option<Occur>,
    group: Group<'a>,
    span: Span,
  },
}

impl<'a> fmt::Display for GroupEntry<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      GroupEntry::ValueMemberKey { ge, .. } => write!(f, "{}", ge),
      GroupEntry::TypeGroupname { ge, .. } => write!(f, "{}", ge),
      GroupEntry::InlineGroup { occur, group, .. } => {
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
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
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
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub enum MemberKey<'a> {
  /// Type expression
  Type1 {
    t1: Box<Type1<'a>>,
    is_cut: bool,
    span: Span,
  },
  /// Bareword string type
  Bareword {
    ident: Identifier<'a>,
    span: Span,
  },
  /// Value type
  Value {
    value: Value<'a>,
    span: Span,
  },
  NonMemberKey(NonMemberKey<'a>),
}

#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub enum NonMemberKey<'a> {
  Group(Group<'a>),
  Type(Type<'a>),
}

impl<'a> fmt::Display for MemberKey<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      MemberKey::Type1 { t1, is_cut, .. } => {
        if *is_cut {
          return write!(f, "{} ^ =>", t1);
        }

        write!(f, "{} =>", t1)
      }
      MemberKey::Bareword { ident, .. } => write!(f, "{}:", ident),
      MemberKey::Value { value, .. } => write!(f, "{}:", value),
      MemberKey::NonMemberKey(NonMemberKey::Group(g)) => write!(f, "{}", g),
      MemberKey::NonMemberKey(NonMemberKey::Type(t)) => write!(f, "{}", t),
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
#[allow(missing_docs)]
pub enum Occur {
  /// Occurrence indicator in the form n*m, where n is an optional lower limit
  /// and m is an optional upper limit
  Exact {
    lower: Option<usize>,
    upper: Option<usize>,
    span: Span,
  },
  /// Occurrence indicator in the form *, allowing zero or more occurrences
  ZeroOrMore(Span),
  /// Occurrence indicator in the form +, allowing one or more occurrences
  OneOrMore(Span),
  /// Occurrence indicator in the form ?, allowing an optional occurrence
  Optional(Span),
}

impl fmt::Display for Occur {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Occur::ZeroOrMore(_) => write!(f, "*"),
      Occur::Exact { lower, upper, .. } => {
        if let Some(li) = lower {
          if let Some(ui) = upper {
            return write!(f, "{}*{}", li, ui);
          }

          return write!(f, "{}*", li);
        }

        if let Some(ui) = upper {
          return write!(f, "*{}", ui);
        }

        write!(f, "*")
      }
      Occur::OneOrMore(_) => write!(f, "+"),
      Occur::Optional(_) => write!(f, "?"),
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
      GroupEntry::TypeGroupname {
        ge: TypeGroupnameEntry {
          occur: None,
          name: Identifier::from("entry1"),
          generic_arg: None,
        },
        span: (0, 0, 0),
      }
      .to_string(),
      "entry1".to_string()
    )
  }

  #[test]
  fn verify_group_output() {
    assert_eq!(
      Group {
        group_choices: vec![GroupChoice {
          group_entries: vec![
            (
              GroupEntry::ValueMemberKey {
                ge: Box::from(ValueMemberKeyEntry {
                  occur: None,
                  member_key: Some(MemberKey::Bareword {
                    ident: "key1".into(),
                    span: (0, 0, 0),
                  }),
                  entry_type: Type {
                    type_choices: vec![Type1 {
                      type2: Type2::TextValue {
                        value: "value1".into(),
                        span: (0, 0, 0),
                      },
                      operator: None,
                      span: (0, 0, 0),
                    }],
                    span: (0, 0, 0),
                  },
                }),
                span: (0, 0, 0),
              },
              true
            ),
            (
              GroupEntry::ValueMemberKey {
                ge: Box::from(ValueMemberKeyEntry {
                  occur: None,
                  member_key: Some(MemberKey::Bareword {
                    ident: "key2".into(),
                    span: (0, 0, 0),
                  }),
                  entry_type: Type {
                    type_choices: vec![Type1 {
                      type2: Type2::TextValue {
                        value: "value2".into(),
                        span: (0, 0, 0),
                      },
                      operator: None,
                      span: (0, 0, 0),
                    }],
                    span: (0, 0, 0),
                  },
                }),
                span: (0, 0, 0),
              },
              true
            ),
          ],
          span: (0, 0, 0),
        }],
        span: (0, 0, 0),
      }
      .to_string(),
      "\tkey1: \"value1\",\n\tkey2: \"value2\",\n".to_string()
    )
  }
}
