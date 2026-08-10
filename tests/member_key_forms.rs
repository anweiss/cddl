#![cfg(feature = "std")]
#![cfg(not(feature = "lsp"))]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{
  ast::{GroupEntry, MemberKey, Rule, Type2},
  parser::cddl_from_str,
};

/// Tests documenting the member key variant produced by each syntactic form.
///
/// See https://github.com/anweiss/cddl/issues/619

fn first_member_key(src: &str) -> MemberKey<'_> {
  let cddl = Box::leak(Box::new(cddl_from_str(src, true).unwrap()));

  let group = match &cddl.rules[0] {
    Rule::Type { rule, .. } => match &rule.value.type_choices[0].type1.type2 {
      Type2::Map { group, .. } => group,
      other => panic!("expected a map, got {:?}", other),
    },
    other => panic!("expected a type rule, got {:?}", other),
  };

  match &group.group_choices[0].group_entries[0].0 {
    GroupEntry::ValueMemberKey { ge, .. } => ge.member_key.clone().unwrap(),
    other => panic!("expected a value member key, got {:?}", other),
  }
}

#[test]
fn arrow_form_literal_key_is_a_type1_key() {
  assert!(matches!(
    first_member_key("a = { 0 => uint }"),
    MemberKey::Type1 { .. }
  ));
}

#[test]
fn colon_form_literal_key_is_a_value_key() {
  assert!(matches!(
    first_member_key("a = { 0: uint }"),
    MemberKey::Value { .. }
  ));
}

#[test]
fn bareword_key_is_a_bareword_key() {
  assert!(matches!(
    first_member_key("a = { b: uint }"),
    MemberKey::Bareword { .. }
  ));
}

/// `MemberKey::Value` carries no cut indicator, which is why the arrow form
/// cannot be collapsed into it.
#[test]
fn arrow_form_preserves_the_cut_indicator() {
  match first_member_key("a = { 0 ^ => uint }") {
    MemberKey::Type1 { is_cut, .. } => assert!(is_cut, "cut indicator was lost"),
    other => panic!("expected a type1 key, got {:?}", other),
  }
}

/// Collapsing the arrow form into `MemberKey::Value` would also rewrite the
/// source, since `MemberKey::Value` renders with a trailing colon.
#[test]
fn member_key_forms_round_trip_unchanged() {
  for src in [
    "a = { 0 => uint }",
    "a = { 0 ^ => uint }",
    "a = { 0: uint }",
    "a = { b: uint }",
  ] {
    let cddl = cddl_from_str(src, true).unwrap();
    assert_eq!(format!("{}", cddl).trim(), src);
  }
}
