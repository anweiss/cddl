// Temporary
#![allow(missing_docs, unused_variables)]

use crate::ast::*;

pub type Result = std::result::Result<(), Box<dyn std::error::Error>>;

pub trait Visitor {
  fn visit_rule(&self, rule: &Rule) -> Result {
    walk_rule(self, rule)
  }

  fn visit_identifier(&self, ident: &Identifier) -> Result {
    walk_identifier(self, ident)
  }

  fn visit_type_rule(&self, tr: &TypeRule) -> Result {
    walk_type_rule(self, tr)
  }

  fn visit_group_rule(&self, gr: &GroupRule) -> Result {
    walk_group_rule(self, gr)
  }

  fn visit_type(&self, t: &Type) -> Result {
    walk_type(self, t)
  }

  fn visit_type_choice(&self, tc: &TypeChoice) -> Result {
    walk_type_choice(self, tc)
  }

  fn visit_type1(&self, t1: &Type1) -> Result {
    walk_type1(self, t1)
  }

  fn visit_operator(&self, o: &Operator) -> Result {
    walk_operator(self, o)
  }

  fn visit_type2(&self, t2: &Type2) -> Result {
    walk_type2(self, t2)
  }

  fn visit_group(&self, g: &Group) -> Result {
    walk_group(self, g)
  }

  fn visit_group_choice(&self, gc: &GroupChoice) -> Result {
    walk_group_choice(self, gc)
  }

  fn visit_group_entry(&self, entry: &GroupEntry) -> Result {
    walk_group_entry(self, entry)
  }

  fn visit_value_member_key_entry(&self, entry: &ValueMemberKeyEntry) -> Result {
    walk_value_member_key_entry(self, entry)
  }

  fn visit_type_groupname_entry(&self, entry: &TypeGroupnameEntry) -> Result {
    walk_type_groupname_entry(self, entry)
  }

  fn visit_occurrence(&self, o: &Occurrence) -> Result {
    walk_occurrence(self, o)
  }

  fn visit_memberkey(&self, mk: &MemberKey) -> Result {
    walk_memberkey(self, mk)
  }

  fn visit_genericargs(&self, args: &GenericArgs) -> Result {
    walk_genericargs(self, args)
  }

  fn visit_genericarg(&self, arg: &GenericArg) -> Result {
    walk_genericarg(self, arg)
  }

  fn visit_genericparams(&self, params: &GenericParams) -> Result {
    walk_genericparams(self, params)
  }

  fn visit_genericparam(&self, param: &GenericParam) -> Result {
    walk_genericparam(self, param)
  }

  fn visit_nonmemberkey(&self, nmk: &NonMemberKey) -> Result {
    walk_nonmemberkey(self, nmk)
  }
}

pub fn walk_rule<V: Visitor + ?Sized>(visitor: &V, rule: &Rule) -> Result {
  todo!()
}

pub fn walk_identifier<V: Visitor + ?Sized>(visitor: &V, ident: &Identifier) -> Result {
  todo!()
}

pub fn walk_type_rule<V: Visitor + ?Sized>(visitor: &V, tr: &TypeRule) -> Result {
  todo!()
}

pub fn walk_group_rule<V: Visitor + ?Sized>(visitor: &V, gr: &GroupRule) -> Result {
  todo!()
}

fn walk_type<V: Visitor + ?Sized>(visitor: &V, t: &Type) -> Result {
  todo!()
}

fn walk_type_choice<V: Visitor + ?Sized>(visitor: &V, tc: &TypeChoice) -> Result {
  todo!()
}

fn walk_type1<V: Visitor + ?Sized>(visitor: &V, t1: &Type1) -> Result {
  todo!()
}

fn walk_operator<V: Visitor + ?Sized>(visitor: &V, o: &Operator) -> Result {
  todo!()
}

fn walk_type2<V: Visitor + ?Sized>(visitor: &V, t2: &Type2) -> Result {
  todo!()
}

fn walk_group<V: Visitor + ?Sized>(visitor: &V, g: &Group) -> Result {
  todo!()
}

fn walk_group_choice<V: Visitor + ?Sized>(visitor: &V, gc: &GroupChoice) -> Result {
  todo!()
}

fn walk_group_entry<V: Visitor + ?Sized>(visitor: &V, entry: &GroupEntry) -> Result {
  todo!()
}

fn walk_value_member_key_entry<V: Visitor + ?Sized>(
  visitor: &V,
  entry: &ValueMemberKeyEntry,
) -> Result {
  todo!()
}

fn walk_type_groupname_entry<V: Visitor + ?Sized>(
  visitor: &V,
  entry: &TypeGroupnameEntry,
) -> Result {
  todo!()
}

fn walk_occurrence<V: Visitor + ?Sized>(visitor: &V, o: &Occurrence) -> Result {
  todo!()
}

fn walk_memberkey<V: Visitor + ?Sized>(visitor: &V, mk: &MemberKey) -> Result {
  todo!()
}

fn walk_genericargs<V: Visitor + ?Sized>(visitor: &V, args: &GenericArgs) -> Result {
  todo!()
}

fn walk_genericarg<V: Visitor + ?Sized>(visitor: &V, arg: &GenericArg) -> Result {
  todo!()
}

fn walk_genericparams<V: Visitor + ?Sized>(visitor: &V, params: &GenericParams) -> Result {
  todo!()
}

fn walk_genericparam<V: Visitor + ?Sized>(visitor: &V, param: &GenericParam) -> Result {
  todo!()
}

fn walk_nonmemberkey<V: Visitor + ?Sized>(visitor: &V, nmk: &NonMemberKey) -> Result {
  todo!()
}
