//! RFC 8610 Appendix D and RFC 8949 sections 5.3.1/5.6.1 regressions.
#![cfg(all(feature = "std", feature = "cbor", not(feature = "lsp")))]

fn accepts(schema: &str, bytes: &[u8]) -> bool {
  #[cfg(feature = "additional-controls")]
  let result = cddl::validator::validate_cbor_from_slice(schema, bytes, None);
  #[cfg(not(feature = "additional-controls"))]
  let result = cddl::validator::validate_cbor_from_slice(schema, bytes);
  result.is_ok()
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn integer_signs_and_controls() {
  for ty in ["uint", "unsigned", "nint", "int", "integer", "number"] {
    for (bytes, negative) in [(&[0x20][..], true), (&[0x00][..], false)] {
      let expected = match ty {
        "uint" | "unsigned" => !negative,
        "nint" => negative,
        _ => true,
      };
      for schema in [
        format!("x = {ty}"),
        format!("x = alias\nalias = {ty}"),
        format!("x = {ty} .eq {}", if negative { "-1" } else { "0" }),
        format!(
          "x = alias .eq {}\nalias = {ty}",
          if negative { "-1" } else { "0" }
        ),
      ] {
        assert_eq!(accepts(&schema, bytes), expected, "{schema}");
      }
    }
  }
  assert!(accepts("x = alias .eq -1\nalias = uint / nint", &[0x20]));
  assert!(accepts("x = alias .eq -1\nalias = unsigned / any", &[0x20]));
  assert!(accepts("x = alias .eq 1\nalias = 1 / nint", &[0x01]));
  assert!(!accepts(
    "x = alias .eq -1\nalias = unsigned / float",
    &[0x20]
  ));
  assert!(!accepts("x = unsigned .eq 1", &[0x00]));
  for (major, valid, invalid) in [(0x1b, "unsigned", "nint"), (0x3b, "nint", "unsigned")] {
    let bytes = [major, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff];
    assert!(accepts(&format!("x = {valid}"), &bytes));
    assert!(!accepts(&format!("x = {invalid}"), &bytes));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn tagged_primitive_membership() {
  for ty in ["tstr", "bool", "uint", "number", "null", "undefined"] {
    assert!(
      !accepts(&format!("x = {ty}"), &[0xc2, 0x41, 0x01]),
      "{}",
      ty
    );
  }
  for (ty, positive, negative) in [
    ("biguint", true, false),
    ("bignint", false, true),
    ("bigint", true, true),
    ("unsigned", true, false),
    ("integer", true, true),
  ] {
    for (tag, expected) in [(0xc2, positive), (0xc3, negative)] {
      assert_eq!(
        accepts(&format!("x = {ty}"), &[tag, 0x41, 0x01]),
        expected,
        "{ty}"
      );
      assert!(!accepts(&format!("x = {ty}"), &[tag, 0x01]), "{}", ty);
    }
  }
  assert!(accepts("x = any", &[0xd8, 42, 0x01]));
  assert!(accepts("x = tstr / #6.42(uint)", &[0xd8, 42, 0x01]));
  assert!(!accepts("x = tstr / #6.42(uint)", &[0xd8, 42, 0x61, b'x']));
  assert!(!accepts("x = tstr / #6.42(uint)", &[0xd8, 43, 0x01]));
  for schema in ["x = { unsigned => uint }", "x = { * unsigned => uint }"] {
    assert!(accepts(schema, &[0xa1, 0xc2, 0x41, 0x01, 0x01]));
    assert!(!accepts(schema, &[0xa1, 0xc3, 0x41, 0x01, 0x01]));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn prelude_payloads_and_base64_policy() {
  for (ty, tag, payload) in [
    ("uri", 32, &[0x61, b'x'][..]),
    ("b64url", 33, &[0x62, b'Z', b'g'][..]),
    ("b64legacy", 34, &[0x64, b'Z', b'g', b'=', b'='][..]),
    ("regexp", 35, &[0x61, b'x'][..]),
    ("mime-message", 36, &[0x61, b'x'][..]),
    ("encoded-cbor", 24, &[0x41, 0x01][..]),
  ] {
    let mut bytes = vec![0xd8, tag];
    bytes.extend_from_slice(payload);
    assert!(accepts(&format!("x = {ty}"), &bytes), "{}", ty);
    assert!(!accepts(&format!("x = {ty}"), &[0xd8, tag, 0x01]), "{}", ty);
    bytes[1] = 42;
    assert!(!accepts(&format!("x = {ty}"), &bytes), "{}", ty);
  }
  assert!(!accepts("x = b64url", &[0xd8, 33, 0x61, b'x']));
  assert!(accepts("x = eb64url", &[0xd5, 0x01]));
  assert!(accepts("x = cbor-any", &[0xd9, 0xd9, 0xf7, 0x01]));
  assert!(accepts("x = time", &[0xc1, 0x01]));
  assert!(!accepts("x = time", &[0xc1, 0x61, b'x']));
  assert!(!accepts(
    "x = time",
    &[0xc1, 0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn undefined_is_not_null() {
  for schema in ["x = undefined", "x = #7.23", "x = alias\nalias = undefined"] {
    assert!(accepts(schema, &[0xf7]), "{}", schema);
    assert!(!accepts(schema, &[0xf6]), "{}", schema);
  }
  for schema in ["x = null", "x = nil", "x = #7.22"] {
    assert!(!accepts(schema, &[0xf7]), "{}", schema);
    assert!(accepts(schema, &[0xf6]), "{}", schema);
  }
  assert!(accepts("x = [undefined, null]", &[0x82, 0xf7, 0xf6]));
  assert!(accepts(
    "x = { undefined => uint, null => uint }",
    &[0xa2, 0xf7, 0x01, 0xf6, 0x02]
  ));
  assert!(accepts("x = any", &[0xf7]));
  assert!(accepts("x = { * undefined => uint }", &[0xa1, 0xf7, 0x01]));
  assert!(!accepts("x = { * undefined => uint }", &[0xa1, 0xf6, 0x01]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn duplicate_text_keys_are_invalid_even_under_any() {
  let duplicate = [0xa2, 0x61, b'a', 0x01, 0x61, b'a', 0x02];
  for schema in ["x = { * tstr => uint }", "x = any"] {
    assert!(!accepts(schema, &duplicate), "{}", schema);
  }
  let mut nested = vec![0x81];
  nested.extend_from_slice(&duplicate);
  assert!(!accepts("x = any", &nested));
  let mut encoded = vec![0x40 + duplicate.len() as u8];
  encoded.extend_from_slice(&duplicate);
  assert!(!accepts("x = bstr .cbor any", &encoded));
  assert!(!accepts(
    "x = any",
    &[0xbf, 0x61, b'a', 1, 0x7f, 0x61, b'a', 0xff, 2, 0xff]
  ));
  assert!(accepts("x = any", &[0xa2, 0x61, b'a', 1, 0x41, b'a', 2]));
  assert!(accepts(
    "x = { * tstr => uint }",
    &[0xa2, 0x61, b'a', 1, 0x61, b'b', 2]
  ));
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn constructed_values_cannot_bypass_duplicate_detection() {
  use cddl::validator::{cbor::CBORValidator, cbor_value::Value, Validator};
  let cddl = cddl::cddl_from_str("x = any", false).unwrap();
  let value = Value::Map(vec![
    (Value::Text("a".into()), Value::Integer(1.into())),
    (Value::Text("a".into()), Value::Integer(2.into())),
  ]);
  #[cfg(feature = "additional-controls")]
  let mut validator = CBORValidator::new(&cddl, value, None);
  #[cfg(not(feature = "additional-controls"))]
  let mut validator = CBORValidator::new(&cddl, value);
  let result = Validator::<cddl::validator::cbor::Error<std::io::Error>>::validate(&mut validator);
  assert!(result
    .unwrap_err()
    .to_string()
    .contains("duplicate text key"));
}
