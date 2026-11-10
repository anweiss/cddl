use cddl_derive::cddl_typegen;
use serde_json::json;

cddl_typegen!("tests/fixtures/group_inclusions.cddl");

#[test]
fn included_group_entries_are_flat_and_keep_optionality() {
  let absent: Item = serde_json::from_value(json!({"a": 1})).unwrap();
  assert_eq!(absent.d, None);
  let present: Item = serde_json::from_value(json!({"a": 1, "d": "value"})).unwrap();
  assert_eq!(present.d.as_deref(), Some("value"));
  assert_eq!(
    serde_json::to_value(present).unwrap(),
    json!({"a": 1, "d": "value"})
  );
  let recursive: RecursiveItem =
    serde_json::from_value(json!({"a": 1, "d": "value", "e": 2})).unwrap();
  assert_eq!(recursive.d.as_deref(), Some("value"));
  assert_eq!(recursive.e, Some(2));
}

#[test]
fn included_group_choice_preserves_both_branches_on_the_wire() {
  for value in [
    json!({"a": 1}),
    json!({"a": 1, "when": "now"}),
    json!({"a": 1, "start": "then"}),
    json!({"a": 1, "start": "then", "end": "later"}),
  ] {
    let item: ChoiceItem = serde_json::from_value(value.clone()).unwrap();
    assert_eq!(serde_json::to_value(&item).unwrap(), value);
    let mut bytes = Vec::new();
    ciborium::into_writer(&item, &mut bytes).unwrap();
    let decoded: ChoiceItem = ciborium::from_reader(bytes.as_slice()).unwrap();
    assert_eq!(serde_json::to_value(decoded).unwrap(), value);
    let inline: InlineChoice = serde_json::from_value(value.clone()).unwrap();
    assert_eq!(serde_json::to_value(inline).unwrap(), value);
    let named: NamedChoiceItem = serde_json::from_value(value.clone()).unwrap();
    assert_eq!(serde_json::to_value(named).unwrap(), value);
  }
}

#[test]
fn group_choice_is_an_enum_and_rejects_combined_branches() {
  let exact: ChoiceItem = serde_json::from_value(json!({"a": 1, "when": "now"})).unwrap();
  assert!(matches!(
    exact.choice_item_group1,
    ChoiceItemGroup1::ExactGroup(_)
  ));
  let range: ChoiceItem = serde_json::from_value(json!({"a": 1, "end": "later"})).unwrap();
  assert!(matches!(
    range.choice_item_group1,
    ChoiceItemGroup1::RangeGroup(_)
  ));
  assert!(
    serde_json::from_value::<ChoiceItem>(json!({"a": 1, "when": "now", "start": "then"})).is_err()
  );
}

#[test]
fn sibling_and_nested_choices_only_consume_their_own_keys() {
  for value in [json!({"a": 1, "c": 2}), json!({"b": 1, "d": 2})] {
    let item: SiblingChoices = serde_json::from_value(value.clone()).unwrap();
    assert_eq!(serde_json::to_value(&item).unwrap(), value);
    let mut bytes = Vec::new();
    ciborium::into_writer(&item, &mut bytes).unwrap();
    let decoded: SiblingChoices = ciborium::from_reader(bytes.as_slice()).unwrap();
    assert_eq!(serde_json::to_value(decoded).unwrap(), value);
  }
  for value in [
    json!({"a": 1, "c": 2}),
    json!({"a": 1, "d": 2}),
    json!({"b": 1}),
  ] {
    let item: NestedChoices = serde_json::from_value(value.clone()).unwrap();
    assert_eq!(serde_json::to_value(item).unwrap(), value);
  }
  for value in [
    json!({"a": 1, "b": 2, "c": 3}),
    json!({"a": 1, "c": 2, "d": 3}),
  ] {
    assert!(serde_json::from_value::<SiblingChoices>(value).is_err());
  }
  for value in [json!({"c": 2}), json!({"a": 1, "c": 2})] {
    let item: OptionalChoices = serde_json::from_value(value.clone()).unwrap();
    assert_eq!(serde_json::to_value(item).unwrap(), value);
  }
  assert!(serde_json::from_value::<OptionalChoices>(json!({"a": 1, "b": 2, "c": 3})).is_err());
}

mod single_rule {
  use cddl_derive::cddl;

  #[cddl(
    path = "tests/fixtures/group_inclusions.cddl",
    rule = "choice-item",
    fundamental_aliases = true
  )]
  struct Renamed;

  #[cddl(
    path = "tests/fixtures/group_inclusions.cddl",
    rule = "choice-item",
    fundamental_aliases = true
  )]
  struct Second;

  #[test]
  fn single_rule_emits_its_choice_helpers() {
    let value = serde_json::json!({"a": 1, "end": "later"});
    let item: Renamed = serde_json::from_value(value.clone()).unwrap();
    assert_eq!(serde_json::to_value(item).unwrap(), value);
    let second: Second = serde_json::from_value(value.clone()).unwrap();
    assert_eq!(serde_json::to_value(second).unwrap(), value);
  }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct CustomGroup {
  replacement: u64,
}

mod substitutions {
  cddl_derive::cddl_typegen!(
    "tests/fixtures/group_inclusions.cddl",
    substitute("common" = "crate::CustomGroup")
  );

  #[cddl_derive::cddl(
      path = "tests/fixtures/group_inclusions.cddl",
      rule = "item",
      substitute("common" = "crate::CustomGroup")
    )]
  struct CustomItem;

  #[test]
  fn included_group_substitutions_keep_the_replacement_flattened() {
    let value = serde_json::json!({"a": 1, "replacement": 7});
    let item: Item = serde_json::from_value(value.clone()).unwrap();
    assert_eq!(item.common.replacement, 7);
    assert_eq!(serde_json::to_value(item).unwrap(), value);
    let item: CustomItem = serde_json::from_value(value.clone()).unwrap();
    assert_eq!(item.common.replacement, 7);
    assert_eq!(serde_json::to_value(item).unwrap(), value);
    for value in [
      serde_json::json!({"a": 1}),
      serde_json::json!({"a": 1, "replacement": 7}),
    ] {
      let item: OptionalSubstitutedItem = serde_json::from_value(value.clone()).unwrap();
      assert_eq!(item.common.is_some(), value.get("replacement").is_some());
      assert_eq!(serde_json::to_value(item).unwrap(), value);
    }
  }
}

mod other_variants {
  cddl_derive::cddl_typegen!("tests/fixtures/group_inclusions.cddl", other_variant = true);

  #[cddl_derive::cddl(
    path = "tests/fixtures/group_inclusions.cddl",
    rule = "choice-item",
    other_variant = true
  )]
  struct ChoiceItemWithOther;

  #[test]
  fn group_choices_never_have_an_unflattenable_string_catch_all() {
    let value = serde_json::json!({"a": 1, "when": "now"});
    let item: ChoiceItem = serde_json::from_value(value.clone()).unwrap();
    match &item.choice_item_group1 {
      ChoiceItemGroup1::ExactGroup(_) | ChoiceItemGroup1::RangeGroup(_) => {}
    }
    assert_eq!(serde_json::to_value(item).unwrap(), value);
    let item: ChoiceItemWithOther = serde_json::from_value(value.clone()).unwrap();
    use __cddl_groups_ChoiceItemWithOther::ChoiceItemGroup1 as Variants;
    match &item.choice_item_group1 {
      Variants::ExactGroup(_) | Variants::RangeGroup(_) => {}
    }
    assert_eq!(serde_json::to_value(item).unwrap(), value);
  }
}
