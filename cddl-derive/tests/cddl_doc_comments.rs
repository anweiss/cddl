use cddl_derive::cddl_typegen;

// Generate types from a schema whose rules and fields carry CDDL comments.
// This is a regression test ensuring that CDDL comments do not break code
// generation and that the resulting types are usable. The exact rendering of
// CDDL comments into Rust doc comments is asserted in the codegen unit tests.
cddl_typegen!("tests/fixtures/documented.cddl");

#[test]
fn test_documented_person_usable() {
  let p = Person {
    name: "Alice".to_string(),
    age: 30,
    email: Some("alice@example.com".to_string()),
  };
  assert_eq!(p.name, "Alice");
  assert_eq!(p.age, 30);
}

#[test]
fn test_documented_score_alias() {
  let s: Score = 99;
  assert_eq!(s, 99);
}

#[test]
fn test_documented_status_enum_exists() {
  // The enum is generated from a documented type-choice rule.
  let status = Status::Active;
  let debug = format!("{:?}", status);
  assert!(debug.contains("Active"));
}
