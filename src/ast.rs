use super::token::{SocketPlug, Value};
use std::fmt;

#[cfg(not(feature = "std"))]
use alloc::{
  boxed::Box,
  string::{String, ToString},
  vec::Vec,
};

pub trait Node {
  fn token_literal(&self) -> Option<String>;
}

#[derive(Default, Debug)]
pub struct CDDL<'a> {
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
    unimplemented!()
  }
}

#[derive(Debug)]
pub enum Rule<'a> {
  Type(TypeRule<'a>),
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

#[derive(Debug)]
pub struct TypeRule<'a> {
  pub name: Identifier<'a>,
  pub generic_param: Option<GenericParm<'a>>,
  pub is_type_choice_alternate: bool,
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

#[derive(Debug)]
pub struct GroupRule<'a> {
  pub name: Identifier<'a>,
  pub generic_param: Option<GenericParm<'a>>,
  pub is_group_choice_alternate: bool,
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

#[derive(Debug)]
pub struct Type1<'a> {
  pub type2: Type2<'a>,
  pub operator: Option<(RangeCtlOp, Type2<'a>)>,
}

impl<'a> fmt::Display for Type1<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut t1 = String::new();

    t1.push_str(&self.type2.to_string());

    if let Some((rco, t2)) = &self.operator {
      t1.push_str(&rco.to_string());
      t1.push_str(&t2.to_string());
    }

    write!(f, "{}", t1)
  }
}

#[derive(Debug, PartialEq)]
pub enum RangeCtlOp {
  RangeOp(bool),
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

#[derive(Debug)]
pub enum Type2<'a> {
  Value(Value<'a>),
  Typename((Identifier<'a>, Option<GenericArg<'a>>)),
  ParenthesizedType(Type<'a>),
  Map(Group<'a>),
  Array(Group<'a>),
  Unwrap((Identifier<'a>, Option<GenericArg<'a>>)),
  ChoiceFromInlineGroup(Group<'a>),
  ChoiceFromGroup((Identifier<'a>, Option<GenericArg<'a>>)),
  TaggedData((Option<usize>, &'a str)),
  TaggedDataMajorType((u8, Option<usize>)),
  Any,
}

impl<'a> fmt::Display for Type2<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Type2::Value(value) => write!(f, "{}", value),
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
      Type2::ChoiceFromInlineGroup(g) => write!(f, "{}", g),
      Type2::ChoiceFromGroup((ident, generic_arg)) => {
        if let Some(ga) = generic_arg {
          return write!(f, "{}{}", ident, ga);
        }

        write!(f, "{}", ident)
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

#[derive(Debug)]
pub struct GroupChoice<'a>(pub Vec<GroupEntry<'a>>);

impl<'a> fmt::Display for GroupChoice<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut group_entries = String::new();

    for ge in self.0.iter() {
      group_entries.push_str(&ge.to_string());
    }

    write!(f, "{}", group_entries)
  }
}

#[derive(Debug)]
pub enum GroupEntry<'a> {
  ValueMemberKey(Box<ValueMemberKeyEntry<'a>>),
  TypeGroupname(TypeGroupnameEntry<'a>),
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

#[derive(Debug)]
pub struct ValueMemberKeyEntry<'a> {
  pub occur: Option<Occur>,
  pub member_key: Option<MemberKey<'a>>,
  pub entry_type: Type<'a>,
}

impl<'a> fmt::Display for ValueMemberKeyEntry<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(o) = &self.occur {
      if let Some(mk) = &self.member_key {
        return writeln!(f, "{} {} {},", o, mk, self.entry_type);
      }

      return writeln!(f, "{} {},", o, self.entry_type);
    }

    if let Some(mk) = &self.member_key {
      return writeln!(f, "{} {},", mk, self.entry_type);
    }

    writeln!(f, "{},", self.entry_type)
  }
}

#[derive(Debug)]
pub struct TypeGroupnameEntry<'a> {
  pub occur: Option<Occur>,
  pub name: Identifier<'a>,
  pub generic_arg: Option<GenericArg<'a>>,
}

impl<'a> fmt::Display for TypeGroupnameEntry<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(o) = &self.occur {
      if let Some(ga) = &self.generic_arg {
        return writeln!(f, "{} {} {},", o, self.name, ga);
      }

      return writeln!(f, "{} {},", o, self.name);
    }

    if let Some(ga) = &self.generic_arg {
      return writeln!(f, "{} {},", self.name, ga);
    }

    writeln!(f, "{},", self.name)
  }
}

#[derive(Debug)]
pub enum MemberKey<'a> {
  // if true, cut is present
  Type1(Box<(Type1<'a>, bool)>),
  Bareword(Identifier<'a>),
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

#[derive(Debug)]
pub enum Occur {
  Exact((Option<usize>, Option<usize>)),
  ZeroOrMore,
  OneOrMore,
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
