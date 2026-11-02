#![cfg(all(feature = "json", feature = "cbor"))]
#![cfg(not(feature = "lsp"))]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{validate_cbor_from_slice, validate_json_from_str};

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
    let mut bytes = Vec::new();
    ciborium::into_writer(&value, &mut bytes).unwrap();
    let json = serde_json::to_string(value).unwrap();
    #[cfg(feature = "additional-controls")]
    let cbor_result = validate_cbor_from_slice("root = b64url", &bytes, None);
    #[cfg(not(feature = "additional-controls"))]
    let cbor_result = validate_cbor_from_slice("root = b64url", &bytes);
    #[cfg(feature = "additional-controls")]
    let json_result = validate_json_from_str("root = b64url", &json, None);
    #[cfg(not(feature = "additional-controls"))]
    let json_result = validate_json_from_str("root = b64url", &json);
    assert_eq!(cbor_result.is_ok(), expected);
    assert_eq!(json_result.is_ok(), expected);
  }
}
