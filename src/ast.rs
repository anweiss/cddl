use super::token::Token;
use std::fmt;

pub trait Node {
  fn token_literal(&self) -> Option<String>;
}

#[derive(Default, Debug)]
pub struct CDDL {
  pub rules: Vec<Rule>,
}

impl Node for CDDL {
  fn token_literal(&self) -> Option<String> {
    if self.rules.len() > 0 {
      return self.rules[0].token_literal();
    }

    None
  }
}

#[derive(Debug)]
pub struct Identifier(pub Token);

impl Node for Identifier {
  fn token_literal(&self) -> Option<String> {
    Some(format!("{:?}", self.0))
  }
}

impl From<String> for Identifier {
  fn from(s: String) -> Self {
    Identifier(Token::IDENT(s))
  }
}

impl ToString for Identifier {
  fn to_string(&self) -> String {
    format!("{}", self.0.to_string())
  }
}

#[derive(Debug)]
pub enum Rule {
  Type(TypeRule),
  Group(GroupRule),
}

impl Node for Rule {
  fn token_literal(&self) -> Option<String> {
    match self {
      Rule::Type(tr) => tr.token_literal(),
      Rule::Group(gr) => gr.token_literal(),
    }
  }
}

#[derive(Debug)]
pub struct TypeRule {
  pub name: Identifier,
  pub generic_param: Option<GenericParm>,
  pub is_type_choice_alternate: bool,
  pub value: Type,
}

impl TypeRule {
  pub fn token_literal(&self) -> Option<String> {
    self.name.token_literal()
  }
}

#[derive(Debug)]
pub struct GroupRule {
  pub name: Identifier,
  pub generic_para: Option<GenericParm>,
  pub is_group_choice_alternate: bool,
  pub entry: GroupEntry,
}

impl Node for GroupRule {
  fn token_literal(&self) -> Option<String> {
    Some("".into())
  }
}

#[derive(Default, Debug)]
pub struct GenericParm(pub Vec<Identifier>);

#[derive(Debug)]
pub struct GenericArg(pub Vec<Type1>);

#[derive(Debug)]
pub struct Type(pub Vec<Type1>);

#[derive(Debug)]
pub struct Type1 {
  pub type2: Type2,
  pub operator: Option<(RangeCtlOp, Type2)>,
}

impl<'a> fmt::Display for Type1 {
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
pub enum Type2 {
  Value(String),
  Typename((Identifier, Option<GenericArg>)),
  Group(Type),
  Map(Group),
  Array(Group),
  Unwrap((Identifier, Option<GenericArg>)),
  ChoiceFromInlineGroup(Group),
  ChoiceFromGroup((Identifier, Option<GenericArg>)),
  TaggedData(String),
  TaggedDataMajorType(String),
  Any,
}

impl<'a> fmt::Display for Type2 {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Type2::Value(value) => write!(f, "\"{}\"", value),
      Type2::Typename((tn, _)) => write!(f, "{}", tn.0),
      _ => write!(f, ""),
    }
  }
}

#[derive(Debug)]
pub struct Group(Vec<GroupChoice>);

#[derive(Debug)]
pub struct GroupChoice(Vec<GroupEntry>);

#[derive(Debug)]
pub enum GroupEntry {
  MemberKey(MemberKeyEntry),
  Groupname(GroupnameEntry),
  InlineGroup((Option<Occur>, Group)),
}

#[derive(Debug)]
pub struct MemberKeyEntry {
  pub occur: Option<Occur>,
  pub member_key: Option<MemberKey>,
  pub entry_type: Type,
}

#[derive(Debug)]
pub struct GroupnameEntry {
  pub occur: Option<Occur>,
  pub name: Identifier,
  pub generic_arg: Option<GenericArg>,
}

#[derive(Debug)]
pub enum MemberKey {
  // if true, cut is present
  Type1((Type1, bool)),
  Bareword(Identifier),
  Value(String),
}

#[derive(Debug)]
pub enum Occur {
  Exact((usize, usize)),
  OneOrMore,
  Optional,
}
