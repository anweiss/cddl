use super::token::Token;
use std::fmt;

pub trait Node {
  fn token_literal(&self) -> Option<String>;
}

#[derive(Default, Debug)]
pub struct CDDL<'a> {
  pub rules: Vec<Rule<'a>>,
}

impl<'a> Node for CDDL<'a> {
  fn token_literal(&self) -> Option<String> {
    if !self.rules.is_empty() {
      return self.rules[0].token_literal();
    }

    None
  }
}

#[derive(Debug)]
pub struct Identifier<'a>(pub Token<'a>);

impl<'a> Node for Identifier<'a> {
  fn token_literal(&self) -> Option<String> {
    Some(format!("{:?}", self.0))
  }
}

impl<'a> From<&'a str> for Identifier<'a> {
  fn from(s: &'a str) -> Self {
    Identifier(Token::IDENT(s))
  }
}

impl<'a> fmt::Display for Identifier<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.0.to_string())
  }
}

#[derive(Debug)]
pub enum Rule<'a> {
  Type(TypeRule<'a>),
  Group(Box<GroupRule<'a>>),
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

impl<'a> TypeRule<'a> {
  pub fn token_literal(&self) -> Option<String> {
    self.name.token_literal()
  }
}

#[derive(Debug)]
pub struct GroupRule<'a> {
  pub name: Identifier<'a>,
  pub generic_para: Option<GenericParm<'a>>,
  pub is_group_choice_alternate: bool,
  pub entry: GroupEntry<'a>,
}

impl<'a> Node for GroupRule<'a> {
  fn token_literal(&self) -> Option<String> {
    Some("".into())
  }
}

#[derive(Default, Debug)]
pub struct GenericParm<'a>(pub Vec<Identifier<'a>>);

#[derive(Debug)]
pub struct GenericArg<'a>(pub Vec<Type1<'a>>);

#[derive(Debug)]
pub struct Type<'a>(pub Vec<Type1<'a>>);

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

#[derive(Debug)]
pub enum RangeCtlOp {
  RangeOp(bool),
  CtlOp(String),
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
  // TODO: figure out zero-copy range Value(&'a str)
  Value(String),
  Typename((Identifier<'a>, Option<GenericArg<'a>>)),
  Group(Type<'a>),
  Map(Group<'a>),
  Array(Group<'a>),
  Unwrap((Identifier<'a>, Option<GenericArg<'a>>)),
  ChoiceFromInlineGroup(Group<'a>),
  ChoiceFromGroup((Identifier<'a>, Option<GenericArg<'a>>)),
  TaggedData(String),
  TaggedDataMajorType(String),
  Any,
}

impl<'a> fmt::Display for Type2<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Type2::Value(value) => write!(f, "{}", value),
      Type2::Typename((tn, _)) => write!(f, "{}", tn.0),
      _ => write!(f, ""),
    }
  }
}

#[derive(Debug)]
pub struct Group<'a>(Vec<GroupChoice<'a>>);

#[derive(Debug)]
pub struct GroupChoice<'a>(Vec<GroupEntry<'a>>);

#[derive(Debug)]
pub enum GroupEntry<'a> {
  MemberKey(MemberKeyEntry<'a>),
  Groupname(GroupnameEntry<'a>),
  InlineGroup((Option<Occur>, Group<'a>)),
}

#[derive(Debug)]
pub struct MemberKeyEntry<'a> {
  pub occur: Option<Occur>,
  pub member_key: Option<MemberKey<'a>>,
  pub entry_type: Type<'a>,
}

#[derive(Debug)]
pub struct GroupnameEntry<'a> {
  pub occur: Option<Occur>,
  pub name: Identifier<'a>,
  pub generic_arg: Option<GenericArg<'a>>,
}

#[derive(Debug)]
pub enum MemberKey<'a> {
  // if true, cut is present
  Type1((Type1<'a>, bool)),
  Bareword(Identifier<'a>),
  Value(String),
}

#[derive(Debug)]
pub enum Occur {
  Exact((usize, usize)),
  OneOrMore,
  Optional,
}
