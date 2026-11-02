#![cfg(all(feature = "json", feature = "cbor"))]
#![cfg(not(feature = "lsp"))]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{validate_cbor_from_slice, validate_json_from_str};
use ciborium::value::Value;

fn assert_cbor_validation(schema: &str, value: Value, expected: bool) {
  let mut bytes = Vec::new();
  ciborium::into_writer(&value, &mut bytes).unwrap();
  #[cfg(feature = "additional-controls")]
  let result = validate_cbor_from_slice(schema, &bytes, None);
  #[cfg(not(feature = "additional-controls"))]
  let result = validate_cbor_from_slice(schema, &bytes);
  assert_eq!(result.is_ok(), expected);
}

#[test]
fn base64_url_prelude_keeps_url_safe_unpadded_validation() {
  for (value, expected) in [
    ("", true),
    ("Zg", true),
    ("Zm8", true),
    ("Zm9v", true),
    ("-_8", true),
    ("Zg==", false),
    ("+/8", false),
    ("a", false),
    ("Zh", false),
    ("Zm 8", false),
  ] {
    assert_cbor_validation(
      "root = b64url",
      Value::Tag(33, Box::new(Value::Text(value.into()))),
      expected,
    );
    let json = serde_json::to_string(value).unwrap();
    #[cfg(feature = "additional-controls")]
    let json_result = validate_json_from_str("root = b64url", &json, None);
    #[cfg(not(feature = "additional-controls"))]
    let json_result = validate_json_from_str("root = b64url", &json);
    assert_eq!(json_result.is_ok(), expected);
  }
}

#[test]
fn cbor_base64_url_requires_tag_33_and_text_payload() {
  for value in [
    Value::Text("Zg".into()),
    Value::Tag(32, Box::new(Value::Text("Zg".into()))),
    Value::Tag(34, Box::new(Value::Text("Zg".into()))),
    Value::Tag(33, Box::new(Value::Bytes(b"Zg".to_vec()))),
    Value::Tag(33, Box::new(Value::Null)),
    Value::Tag(33, Box::new(Value::Integer(1.into()))),
    Value::Tag(
      33,
      Box::new(Value::Tag(33, Box::new(Value::Text("Zg".into())))),
    ),
  ] {
    assert_cbor_validation("root = b64url", value, false);
  }
}

#[test]
fn cbor_base64_url_validates_aliases_in_arrays() {
  for (text, expected) in [("Zg", true), ("+/8", false)] {
    assert_cbor_validation(
      "root = [encoded]\nencoded = b64url",
      Value::Array(vec![Value::Tag(33, Box::new(Value::Text(text.into())))]),
      expected,
    );
  }
}
