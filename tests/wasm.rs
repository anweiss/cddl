#![cfg(target_arch = "wasm32")]

use cddl::cddl_from_str;
use indoc::indoc;
use wasm_bindgen_test::*;

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
