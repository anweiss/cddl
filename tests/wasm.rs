#![cfg(target_arch = "wasm32")]

use wasm_bindgen_test::*;

use cddl::cddl_from_str;
use indoc::indoc;

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
