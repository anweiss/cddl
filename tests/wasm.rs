#![cfg(target_arch = "wasm32")]

use wasm_bindgen_test::*;

use cddl::{ast::Type2, cddl_from_str, token::ByteValue};
use indoc::indoc;
use std::borrow::Cow;

#[wasm_bindgen_test]
fn pass() {
  let input = indoc!(
    r#"
            Version = [
                major: tstr, ; comment
                minor: tstr,
                patch: tstr,
            ]
        "#
  );
  let _ = cddl_from_str(input).unwrap();
}

#[wasm_bindgen_test]
fn fail() {
  let input = "invalid = {";
  assert!(cddl_from_str(input).is_err());
}

#[wasm_bindgen_test]
fn decoded_byte_literals_format() {
  for (value, expected) in [
    (ByteValue::B16(Cow::Borrowed(&[0xaa])), "h'aa'"),
    (ByteValue::B64(Cow::Borrowed(&[0xfb, 0xff])), "b64'-_8'"),
  ] {
    assert_eq!(value.to_string(), expected);
    assert_eq!(Type2::from(value).to_string(), expected);
  }
}
