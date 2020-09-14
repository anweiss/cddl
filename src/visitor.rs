// Temporary
#![allow(missing_docs, unused_variables)]
#![cfg(feature = "std")]

use crate::{ast::*, token::ByteValue, token::Value};
use std::error::Error;

pub type Result<T> = std::result::Result<(), T>;

pub trait Visitor<'a, E: Error> {
  fn visit_rule(&mut self, rule: &Rule<'a>) -> Result<E> {
    walk_rule(self, rule)
  }

  fn visit_identifier(&mut self, ident: &Identifier<'a>) -> Result<E> {
    Ok(())
  }

  fn visit_value(&mut self, value: &Value<'a>) -> Result<E> {
    Ok(())
  }

  fn visit_type_rule(&mut self, tr: &TypeRule<'a>) -> Result<E> {
    walk_type_rule(self, tr)
  }

  fn visit_group_rule(&mut self, gr: &GroupRule<'a>) -> Result<E> {
    walk_group_rule(self, gr)
  }

  fn visit_type(&mut self, t: &Type<'a>) -> Result<E> {
    walk_type(self, t)
  }

  fn visit_type_choice(&mut self, tc: &TypeChoice<'a>) -> Result<E> {
    walk_type_choice(self, tc)
  }

  fn visit_type1(&mut self, t1: &Type1<'a>) -> Result<E> {
    walk_type1(self, t1)
  }

  fn visit_operator(&mut self, target: &Type1<'a>, o: &Operator<'a>) -> Result<E> {
    walk_operator(self, target, o)
  }

  fn visit_range(&mut self, lower: &Type2<'a>, upper: &Type2<'a>, is_inclusive: bool) -> Result<E> {
    walk_range(self, lower, upper)
  }

  fn visit_control_operator(
    &mut self,
    target: &Type2<'a>,
    ctrl: &str,
    controller: &Type2<'a>,
  ) -> Result<E> {
    walk_control_operator(self, target, controller)
  }

  fn visit_type2(&mut self, t2: &Type2<'a>) -> Result<E> {
    walk_type2(self, t2)
  }

  fn visit_group(&mut self, g: &Group<'a>) -> Result<E> {
    walk_group(self, g)
  }

  fn visit_group_choice(&mut self, gc: &GroupChoice<'a>) -> Result<E> {
    walk_group_choice(self, gc)
  }

  fn visit_group_entry(&mut self, entry: &GroupEntry<'a>) -> Result<E> {
    walk_group_entry(self, entry)
  }

  fn visit_value_member_key_entry(&mut self, entry: &ValueMemberKeyEntry<'a>) -> Result<E> {
    walk_value_member_key_entry(self, entry)
  }

  fn visit_type_groupname_entry(&mut self, entry: &TypeGroupnameEntry<'a>) -> Result<E> {
    walk_type_groupname_entry(self, entry)
  }

  fn visit_inline_group_entry(
    &mut self,
    occur: Option<&Occurrence<'a>>,
    g: &Group<'a>,
  ) -> Result<E> {
    walk_inline_group_entry(self, occur, g)
  }

  fn visit_occurrence(&mut self, o: &Occurrence<'a>) -> Result<E> {
    Ok(())
  }

  fn visit_memberkey(&mut self, mk: &MemberKey<'a>) -> Result<E> {
    walk_memberkey(self, mk)
  }

  fn visit_genericargs(&mut self, args: &GenericArgs<'a>) -> Result<E> {
    walk_genericargs(self, args)
  }

  fn visit_genericarg(&mut self, arg: &GenericArg<'a>) -> Result<E> {
    walk_genericarg(self, arg)
  }

  fn visit_nonmemberkey(&mut self, nmk: &NonMemberKey<'a>) -> Result<E> {
    walk_nonmemberkey(self, nmk)
  }
}

pub fn walk_rule<'a, E, V>(visitor: &mut V, rule: &Rule<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  match rule {
    Rule::Type { rule, .. } => visitor.visit_type_rule(rule),
    Rule::Group { rule, .. } => visitor.visit_group_rule(rule),
  }
}

pub fn walk_type_rule<'a, E, V>(visitor: &mut V, tr: &TypeRule<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  visitor.visit_type(&tr.value)
}

pub fn walk_group_rule<'a, E, V>(visitor: &mut V, gr: &GroupRule<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  visitor.visit_group_entry(&gr.entry)
}

pub fn walk_type<'a, E, V>(visitor: &mut V, t: &Type<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  for tc in t.type_choices.iter() {
    visitor.visit_type_choice(tc)?;
  }

  Ok(())
}

pub fn walk_type_choice<'a, E, V>(visitor: &mut V, tc: &TypeChoice<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  visitor.visit_type1(&tc.type1)
}

pub fn walk_type1<'a, E, V>(visitor: &mut V, t1: &Type1<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  if let Some(o) = &t1.operator {
    return visitor.visit_operator(&t1, o);
  }

  visitor.visit_type2(&t1.type2)
}

pub fn walk_operator<'a, E, V>(visitor: &mut V, target: &Type1<'a>, o: &Operator<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  match &o.operator {
    RangeCtlOp::RangeOp { is_inclusive, .. } => {
      visitor.visit_range(&target.type2, &o.type2, *is_inclusive)
    }
    RangeCtlOp::CtlOp { ctrl, .. } => visitor.visit_control_operator(&target.type2, ctrl, &o.type2),
  }
}

pub fn walk_range<'a, E, V>(visitor: &mut V, lower: &Type2<'a>, upper: &Type2<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  visitor.visit_type2(lower)?;
  visitor.visit_type2(upper)
}

pub fn walk_control_operator<'a, E, V>(
  visitor: &mut V,
  target: &Type2<'a>,
  controller: &Type2<'a>,
) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  visitor.visit_type2(target)?;
  visitor.visit_type2(controller)
}

pub fn walk_type2<'a, E, V>(visitor: &mut V, t2: &Type2<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  match t2 {
    Type2::Array { group, .. } => visitor.visit_group(group),
    Type2::Map { group, .. } => visitor.visit_group(group),
    Type2::ChoiceFromGroup {
      generic_args,
      ident,
      ..
    } => {
      if let Some(ga) = generic_args {
        visitor.visit_genericargs(ga)?;
      }

      visitor.visit_identifier(ident)
    }
    Type2::ChoiceFromInlineGroup { group, .. } => visitor.visit_group(group),
    Type2::TaggedData { t, .. } => visitor.visit_type(t),
    Type2::Typename { ident, .. } => visitor.visit_identifier(ident),
    Type2::Unwrap {
      generic_args,
      ident,
      ..
    } => {
      if let Some(ga) = generic_args {
        visitor.visit_genericargs(ga)?;
      }

      visitor.visit_identifier(ident)
    }
    Type2::ParenthesizedType { pt, .. } => visitor.visit_type(pt),
    Type2::B16ByteString { value, .. } => {
      visitor.visit_value(&Value::BYTE(ByteValue::B16(value.clone())))
    }
    Type2::B64ByteString { value, .. } => {
      visitor.visit_value(&Value::BYTE(ByteValue::B64(value.clone())))
    }
    Type2::UTF8ByteString { value, .. } => {
      visitor.visit_value(&Value::BYTE(ByteValue::UTF8(value.clone())))
    }
    Type2::FloatValue { value, .. } => visitor.visit_value(&Value::FLOAT(*value)),
    Type2::IntValue { value, .. } => visitor.visit_value(&Value::INT(*value)),
    Type2::UintValue { value, .. } => visitor.visit_value(&Value::UINT(*value)),
    Type2::TextValue { value, .. } => visitor.visit_value(&Value::TEXT(value)),
    _ => Ok(()),
  }
}

pub fn walk_group<'a, E, V>(visitor: &mut V, g: &Group<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  for gc in &g.group_choices {
    visitor.visit_group_choice(gc)?;
  }

  Ok(())
}

pub fn walk_group_choice<'a, E, V>(visitor: &mut V, gc: &GroupChoice<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  for ge in &gc.group_entries {
    visitor.visit_group_entry(&ge.0)?;
  }

  Ok(())
}

pub fn walk_group_entry<'a, E, V>(visitor: &mut V, entry: &GroupEntry<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  match entry {
    GroupEntry::ValueMemberKey { ge, .. } => visitor.visit_value_member_key_entry(ge),
    GroupEntry::TypeGroupname { ge, .. } => visitor.visit_type_groupname_entry(ge),
    GroupEntry::InlineGroup { occur, group, .. } => {
      visitor.visit_inline_group_entry(occur.as_ref(), group)
    }
  }
}

pub fn walk_value_member_key_entry<'a, E, V>(
  visitor: &mut V,
  entry: &ValueMemberKeyEntry<'a>,
) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  if let Some(occur) = &entry.occur {
    visitor.visit_occurrence(occur)?;
  }

  if let Some(mk) = &entry.member_key {
    visitor.visit_memberkey(mk)?;
  }

  visitor.visit_type(&entry.entry_type)
}

pub fn walk_type_groupname_entry<'a, E, V>(
  visitor: &mut V,
  entry: &TypeGroupnameEntry<'a>,
) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  if let Some(o) = &entry.occur {
    visitor.visit_occurrence(o)?;
  }

  if let Some(ga) = &entry.generic_args {
    visitor.visit_genericargs(ga)?;
  }

  visitor.visit_identifier(&entry.name)
}

pub fn walk_inline_group_entry<'a, E, V>(
  visitor: &mut V,
  occur: Option<&Occurrence<'a>>,
  g: &Group<'a>,
) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  if let Some(o) = occur {
    visitor.visit_occurrence(o)?;
  }

  visitor.visit_group(g)
}

pub fn walk_memberkey<'a, E, V>(visitor: &mut V, mk: &MemberKey<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  match mk {
    MemberKey::Type1 { t1, .. } => visitor.visit_type1(t1),
    MemberKey::Bareword { ident, .. } => visitor.visit_identifier(ident),
    MemberKey::Value { value, .. } => visitor.visit_value(value),
    MemberKey::NonMemberKey { non_member_key, .. } => visitor.visit_nonmemberkey(non_member_key),
  }
}

pub fn walk_genericargs<'a, E, V>(visitor: &mut V, args: &GenericArgs<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  for arg in args.args.iter() {
    visitor.visit_genericarg(arg)?;
  }

  Ok(())
}

pub fn walk_genericarg<'a, E, V>(visitor: &mut V, arg: &GenericArg<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  visitor.visit_type1(&arg.arg)
}

pub fn walk_nonmemberkey<'a, E, V>(visitor: &mut V, nmk: &NonMemberKey<'a>) -> Result<E>
where
  E: Error,
  V: Visitor<'a, E> + ?Sized,
{
  match nmk {
    NonMemberKey::Group(group) => visitor.visit_group(group),
    NonMemberKey::Type(t) => visitor.visit_type(t),
  }
}
