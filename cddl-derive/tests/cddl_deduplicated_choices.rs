use cddl_derive::cddl_typegen;
use serde_json::json;

cddl_typegen!("tests/fixtures/deduplicated_choices.cddl");

#[test]
fn regexp_choices_compile_and_round_trip() {
  for text in ["a", "b"] {
    let value: U = serde_json::from_value(json!(text)).unwrap();
    assert!(matches!(&value, U::Tstr(s) if s == text));
    assert_eq!(serde_json::to_value(value).unwrap(), json!(text));
  }
}

#[test]
fn deduplicated_mixed_choices_keep_distinct_representations() {
  for (input, expected) in [
    (json!("a"), Mixed::A),
    (json!("b"), Mixed::Tstr("b".into())),
    (json!(42), Mixed::Uint(42)),
  ] {
    let value: Mixed = serde_json::from_value(input.clone()).unwrap();
    assert_eq!(
      std::mem::discriminant(&value),
      std::mem::discriminant(&expected)
    );
    assert_eq!(serde_json::to_value(value).unwrap(), input);
  }
  let value = Mixed::Bstr(vec![1, 2]);
  let mut bytes = Vec::new();
  ciborium::into_writer(&value, &mut bytes).unwrap();
  assert!(matches!(
    ciborium::from_reader::<Mixed, _>(bytes.as_slice()).unwrap(),
    Mixed::Bstr(b) if b == [1, 2]
  ));
}

#[test]
fn colliding_literal_names_keep_their_wire_values() {
  for text in ["foo-bar", "foo_bar", "foo-bar2"] {
    let value: Labels = serde_json::from_value(json!(text)).unwrap();
    assert_eq!(serde_json::to_value(value).unwrap(), json!(text));
  }
}
