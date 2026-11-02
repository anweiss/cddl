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

#[cfg(feature = "modules")]
#[wasm_bindgen_test]
fn modules_validate_sources_and_preserve_literal_directive_text() {
  use cddl::modules::{resolve_modules, MemoryModuleSource, ModuleError, ResolveOptions};

  let mut source = MemoryModuleSource::new();
  source.insert("broken", "value = {");
  assert!(matches!(
    resolve_modules(";# include broken\n", &source, &ResolveOptions::default()),
    Err(ModuleError::ModuleParse { .. })
  ));

  source.insert("literal", "value = 'before\n;# import ignored\nafter'\n");
  let output = resolve_modules(
    ";# import literal as ns\n",
    &source,
    &ResolveOptions {
      start_rule: Some("ns.value".to_string()),
      ..ResolveOptions::default()
    },
  )
  .unwrap();
  assert!(output.contains("ns.value = 'before\n;# import ignored\nafter'"));
  assert!(cddl_from_str(&output).is_ok());
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
