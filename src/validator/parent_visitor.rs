#![cfg(feature = "ast-parent")]

use crate::{
  ast::*,
  token::{self, Token},
  validator::group_choice_alternates_from_ident,
  visitor::{self, *},
};

use std::{borrow::Cow, collections::HashMap, convert::TryFrom, fmt};

/// validation Result
pub type Result = std::result::Result<(), Error>;

/// validation error
#[derive(Debug)]
pub enum Error {
  /// Tree overwrite error
  Overwrite,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Error::Overwrite => write!(f, "attempt to overwrite existing tree node"),
    }
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      _ => None,
    }
  }
}

/// JSON validation error
#[derive(Clone, Debug)]
pub struct ValidationError {
  /// Error message
  pub reason: String,
}

impl fmt::Display for ValidationError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut error_str = String::from("error validating");
    write!(f, "{}: {}", error_str, self.reason)
  }
}

impl std::error::Error for ValidationError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

impl ValidationError {
  fn from_validator(pv: &ParentVisitor, reason: String) -> Self {
    ValidationError { reason }
  }
}

type RulePointer = u16; // TODO

#[derive(Clone, Debug, PartialEq)]
enum CDDLType<'a, 'b: 'a> {
  CDDL(&'b CDDL<'a>),
  Rule(&'b Rule<'a>),
  TypeRule(&'b TypeRule<'a>),
  GroupRule(&'b GroupRule<'a>),
  GenericParams(&'b GenericParams<'a>),
  GenericParam(&'b GenericParam<'a>),
  GroupEntry(&'b GroupEntry<'a>),
  Identifier(&'b Identifier<'a>),
  Type(&'b Type<'a>),
  TypeChoice(&'b TypeChoice<'a>),
  Type1(&'b Type1<'a>),
  Type2(&'b Type2<'a>),
  Operator(&'b Operator<'a>),
}

#[derive(Debug, Default, Clone)]
struct ArenaTree<'a, 'b: 'a> {
  arena: Vec<Node<'a, 'b>>,
}

impl<'a, 'b: 'a> ArenaTree<'a, 'b> {
  fn node(&mut self, val: CDDLType<'a, 'b>) -> usize {
    for node in self.arena.iter() {
      if node.val == val {
        return node.idx;
      }
    }

    let idx = self.arena.len();
    self.arena.push(Node::new(idx, val));
    idx
  }
}

#[derive(Debug, Clone)]
struct Node<'a, 'b: 'a> {
  idx: usize,
  val: CDDLType<'a, 'b>,
  parent: Option<usize>,
  children: Vec<usize>,
}

impl<'a, 'b: 'a> Node<'a, 'b> {
  fn new(idx: usize, val: CDDLType<'a, 'b>) -> Self {
    Self {
      idx,
      val,
      parent: None,
      children: vec![],
    }
  }
}

/// validator type
// #[derive(Clone)]
pub struct ParentVisitor<'a, 'b: 'a> {
  cddl: &'a CDDL<'a>,
  errors: Vec<ValidationError>,
  arena_tree: ArenaTree<'a, 'b>,
}

impl<'a, 'b: 'a> ParentVisitor<'a, 'b> {
  pub fn new(cddl: &'a CDDL<'a>) -> Self {
    ParentVisitor {
      cddl,
      errors: Vec::default(),
      arena_tree: ArenaTree {
        arena: Vec::default(),
      },
    }
  }
}

impl<'a, 'b: 'a> ParentVisitor<'a, 'b> {
  fn insert(&mut self, parent: usize, child: usize) -> Result {
    match self.arena_tree.arena[child].parent {
      Some(_) => {
        return Err(Error::Overwrite);
      }
      None => {
        self.arena_tree.arena[child].parent = Some(parent);
      }
    }

    self.arena_tree.arena[parent].children.push(child);

    Ok(())
  }
}

impl<'a, 'b: 'a> Visitor<'a, 'b, Error> for ParentVisitor<'a, 'b> {
  fn visit_cddl(&mut self, cddl: &'b CDDL<'a>) -> visitor::Result<Error> {
    let parent = self.arena_tree.node(CDDLType::CDDL(cddl));
    for rule in cddl.rules.iter() {
      let child = self.arena_tree.node(CDDLType::Rule(rule));

      self.insert(parent, child)?;

      self.visit_rule(rule)?;
    }

    Ok(())
  }

  fn visit_rule(&mut self, rule: &'b Rule<'a>) -> visitor::Result<Error> {
    let parent = self.arena_tree.node(CDDLType::Rule(rule));

    match rule {
      Rule::Group { rule, .. } => {
        let child = self.arena_tree.node(CDDLType::GroupRule(rule));

        self.insert(parent, child)?;

        self.visit_group_rule(rule)?;
      }
      Rule::Type { rule, .. } => {
        let child = self.arena_tree.node(CDDLType::TypeRule(rule));

        self.insert(parent, child)?;

        self.visit_type_rule(rule)?;
      }
    }

    Ok(())
  }

  fn visit_type_rule(&mut self, tr: &'b TypeRule<'a>) -> visitor::Result<Error> {
    let parent = self.arena_tree.node(CDDLType::TypeRule(tr));

    let child = self.arena_tree.node(CDDLType::Identifier(&tr.name));
    self.insert(parent, child)?;

    if let Some(params) = &tr.generic_params {
      let child = self.arena_tree.node(CDDLType::GenericParams(params));
      self.insert(parent, child)?;
      walk_generic_params(self, params)?;
    }

    let child = self.arena_tree.node(CDDLType::Type(&tr.value));
    self.insert(parent, child)?;

    self.visit_type(&tr.value)
  }

  fn visit_group_rule(&mut self, gr: &'b GroupRule<'a>) -> visitor::Result<Error> {
    let parent = self.arena_tree.node(CDDLType::GroupRule(gr));

    let child = self.arena_tree.node(CDDLType::Identifier(&gr.name));
    self.insert(parent, child)?;
    self.visit_identifier(&gr.name)?;

    if let Some(params) = &gr.generic_params {
      let child = self.arena_tree.node(CDDLType::GenericParams(params));
      self.insert(parent, child)?;
      walk_generic_params(self, params)?;
    }

    let child = self.arena_tree.node(CDDLType::GroupEntry(&gr.entry));
    self.insert(parent, child)?;

    self.visit_group_entry(&gr.entry)
  }

  fn visit_type(&mut self, t: &'b Type<'a>) -> visitor::Result<Error> {
    let parent = self.arena_tree.node(CDDLType::Type(t));

    for tc in t.type_choices.iter() {
      let child = self.arena_tree.node(CDDLType::TypeChoice(tc));
      self.insert(parent, child)?;

      self.visit_type_choice(tc)?;
    }

    Ok(())
  }

  fn visit_type_choice(&mut self, tc: &'a TypeChoice<'a>) -> visitor::Result<Error> {
    let parent = self.arena_tree.node(CDDLType::TypeChoice(tc));

    let child = self.arena_tree.node(CDDLType::Type1(&tc.type1));
    self.insert(parent, child)?;

    self.visit_type1(&tc.type1)
  }

  fn visit_type1(&mut self, t1: &'b Type1<'a>) -> visitor::Result<Error> {
    let parent = self.arena_tree.node(CDDLType::Type1(t1));

    if let Some(operator) = &t1.operator {
      let child = self.arena_tree.node(CDDLType::Operator(operator));
      self.insert(parent, child)?;

      self.visit_operator(t1, operator)?;
    }

    let child = self.arena_tree.node(CDDLType::Type2(&t1.type2));
    self.insert(parent, child)?;

    self.visit_type2(&t1.type2)
  }

  fn visit_operator(
    &mut self,
    target: &'b Type1<'a>,
    o: &'b Operator<'a>,
  ) -> visitor::Result<Error> {
    let parent = self.arena_tree.node(CDDLType::Operator(o));
    let child = self.arena_tree.node(CDDLType::Type2(&o.type2));
    self.insert(parent, child)?;

    walk_operator(self, target, o)
  }

  fn visit_type2(&mut self, t2: &'b Type2<'a>) -> visitor::Result<Error> {
    let parent = self.arena_tree.node(CDDLType::Type2(t2));

    match t2 {
      Type2::IntValue {
        value,
        span,
        parent,
      } => todo!(),
      Type2::UintValue {
        value,
        span,
        parent,
      } => todo!(),
      Type2::FloatValue {
        value,
        span,
        parent,
      } => todo!(),
      Type2::TextValue {
        value,
        span,
        parent,
      } => todo!(),
      Type2::UTF8ByteString {
        value,
        span,
        parent,
      } => todo!(),
      Type2::B16ByteString {
        value,
        span,
        parent,
      } => todo!(),
      Type2::B64ByteString {
        value,
        span,
        parent,
      } => todo!(),
      Type2::Typename {
        ident,
        generic_args,
        span,
        parent,
      } => todo!(),
      Type2::ParenthesizedType {
        pt,
        span,
        comments_before_type,
        comments_after_type,
        parent,
      } => todo!(),
      Type2::Map {
        group,
        span,
        comments_before_group,
        comments_after_group,
        parent,
      } => todo!(),
      Type2::Array {
        group,
        span,
        comments_before_group,
        comments_after_group,
        parent,
      } => todo!(),
      Type2::Unwrap {
        ident,
        generic_args,
        span,
        comments,
        parent,
      } => todo!(),
      Type2::ChoiceFromInlineGroup {
        group,
        span,
        comments,
        comments_before_group,
        comments_after_group,
        parent,
      } => todo!(),
      Type2::ChoiceFromGroup {
        ident,
        generic_args,
        span,
        comments,
        parent,
      } => todo!(),
      Type2::TaggedData {
        tag,
        t,
        span,
        comments_before_type,
        comments_after_type,
        parent,
      } => todo!(),
      Type2::DataMajorType {
        mt,
        constraint,
        span,
        parent,
      } => todo!(),
      Type2::Any { span, parent } => todo!(),
    }
  }

  fn visit_group(&mut self, g: &'b Group<'a>) -> visitor::Result<Error> {
    walk_group(self, g)
  }

  fn visit_group_choice(&mut self, gc: &'b GroupChoice<'a>) -> visitor::Result<Error> {
    walk_group_choice(self, gc)
  }

  fn visit_group_entry(&mut self, entry: &'b GroupEntry<'a>) -> visitor::Result<Error> {
    walk_group_entry(self, entry)
  }

  fn visit_value_member_key_entry(
    &mut self,
    entry: &'b ValueMemberKeyEntry<'a>,
  ) -> visitor::Result<Error> {
    walk_value_member_key_entry(self, entry)
  }

  fn visit_type_groupname_entry(
    &mut self,
    entry: &'b TypeGroupnameEntry<'a>,
  ) -> visitor::Result<Error> {
    walk_type_groupname_entry(self, entry)
  }

  fn visit_inline_group_entry(
    &mut self,
    occur: Option<&Occurrence<'a>>,
    g: &'b Group<'a>,
  ) -> visitor::Result<Error> {
    walk_inline_group_entry(self, occur, g)
  }

  fn visit_occurrence(&mut self, _o: &Occurrence<'a>) -> visitor::Result<Error> {
    Ok(())
  }

  fn visit_memberkey(&mut self, mk: &'b MemberKey<'a>) -> visitor::Result<Error> {
    walk_memberkey(self, mk)
  }

  fn visit_generic_args(&mut self, args: &'b GenericArgs<'a>) -> visitor::Result<Error> {
    walk_generic_args(self, args)
  }

  fn visit_generic_arg(&mut self, arg: &'b GenericArg<'a>) -> visitor::Result<Error> {
    walk_generic_arg(self, arg)
  }

  fn visit_generic_params(&mut self, params: &'b GenericParams<'a>) -> visitor::Result<Error> {
    let parent = self.arena_tree.node(CDDLType::GenericParams(params));

    for param in params.params.iter() {
      let child = self.arena_tree.node(CDDLType::GenericParam(param));
      self.insert(parent, child)?;

      self.visit_generic_param(param)?;
    }

    Ok(())
  }

  fn visit_generic_param(&mut self, param: &GenericParam<'a>) -> visitor::Result<Error> {
    walk_generic_param(self, param)
  }

  fn visit_nonmemberkey(&mut self, nmk: &'b NonMemberKey<'a>) -> visitor::Result<Error> {
    walk_nonmemberkey(self, nmk)
  }
}

#[cfg(test)]
#[cfg(not(target_arch = "wasm32"))]
mod tests {
  #![allow(unused_imports)]

  use crate::cddl_from_str;

  use super::*;

  #[test]
  fn testing() {
    let c = cddl_from_str(
      r#"a = b
    b = "test""#,
      true,
    )
    .unwrap();
    let mut t = ParentVisitor::new(&c);
    t.visit_cddl(&c).unwrap();
  }
}
