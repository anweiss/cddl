#![cfg(target_arch = "wasm32")]

use wasm_bindgen_test::*;

use cddl::{ast::Type2, cddl_from_str, token::ByteValue};
use indoc::indoc;
use std::borrow::Cow;

#[path = "json_no_cut_fallthrough.rs"]
mod json_no_cut_fallthrough;

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

#[cfg(feature = "modules")]
#[wasm_bindgen_test]
fn owning_document_and_javascript_module_map() {
  use cddl::modules::{resolve_cddl_modules, MemoryModuleSource, ResolveOptions, ResolvedCddl};
  use wasm_bindgen::JsValue;

  let modules = js_sys::Object::new();
  js_sys::Reflect::set(
    &modules,
    &JsValue::from_str("numbers"),
    &JsValue::from_str("value = uint\n"),
  )
  .unwrap();
  let output = resolve_cddl_modules(
    ";# import numbers as n\n",
    modules.into(),
    Some("n.value".to_string()),
  )
  .unwrap();
  assert!(output.starts_with("$.start.$ = n.value"));
  assert!(cddl_from_str(&output).is_ok());

  let document = ResolvedCddl::resolve(
    &output,
    &MemoryModuleSource::new(),
    &ResolveOptions::default(),
  )
  .unwrap();
  assert_eq!(document.parse().unwrap().rules.len(), 2);
}

#[cfg(feature = "modules")]
#[wasm_bindgen_test]
fn javascript_module_map_reports_invalid_inputs() {
  use cddl::modules::resolve_cddl_modules;
  use wasm_bindgen::JsValue;

  let error = resolve_cddl_modules("", JsValue::NULL, None).unwrap_err();
  assert_eq!(
    error.as_string().unwrap(),
    "modules must map names to CDDL strings"
  );
  let error =
    resolve_cddl_modules(";# include missing\n", js_sys::Object::new().into(), None).unwrap_err();
  assert!(error.as_string().unwrap().contains("line 1"));

  let modules = js_sys::Object::new();
  js_sys::Reflect::set(
    &modules,
    &JsValue::from_str("broken"),
    &JsValue::from_f64(1.0),
  )
  .unwrap();
  assert!(resolve_cddl_modules("", modules.into(), None).is_err());
}
