#![cfg(feature = "json")]
#![cfg(not(feature = "lsp"))]

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test;

fn validates(schema: &str, instance: &str) -> bool {
  cddl::validator::validate_json_from_str(
    schema,
    instance,
    #[cfg(feature = "additional-controls")]
    None,
  )
  .is_ok()
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn optional_non_cut_members_leave_failed_pairs_for_later_members() {
  for key in [r#""extension""#, "tstr", "any"] {
    let schema = format!("root = {{ ? {} => uint, * tstr => tstr }}", key);
    assert!(validates(&schema, r#"{"extension":"value"}"#), "{}", schema);
    assert!(!validates(&schema, r#"{"extension":false}"#), "{}", schema);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cut_and_required_members_do_not_skip_value_errors() {
  for member in [
    r#"? "extension" ^ => uint"#,
    "? tstr ^ => uint",
    "? extension: uint",
    r#"? "extension": uint"#,
    r#""extension" => uint"#,
    "tstr => uint",
  ] {
    let schema = format!("root = {{ {}, * tstr => any }}", member);
    assert!(
      !validates(&schema, r#"{"extension":"value"}"#),
      "{}",
      schema
    );
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn skipped_member_restores_claims_moved_during_key_lookup() {
  // Looking up "a" moves the first member's claim to "b". Rejecting the
  // optional value must undo that move as well as release its tentative key.
  let schema = r#"root = { tstr => tstr, ? "a" => uint, b: tstr }"#;
  assert!(validates(schema, r#"{"a":"first","b":"second"}"#));
  assert!(!validates(schema, r#"{"a":"first"}"#));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn optional_members_preserve_prior_claims_and_required_occurrences() {
  let schema = r#"root = { fixed: uint, ? tstr => uint, required: tstr }"#;
  assert!(validates(schema, r#"{"fixed":1,"required":"value"}"#));
  assert!(!validates(schema, r#"{"fixed":1,"other":"value"}"#));
  assert!(!validates(schema, r#"{"required":"value"}"#));
  assert!(!validates(
    r#"root = { ? "a" => uint, required: tstr }"#,
    r#"{"a":1}"#,
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn successful_optional_members_keep_ownership_and_failed_ones_keep_coverage() {
  assert!(!validates(
    r#"root = { ? "a" => uint, a: uint }"#,
    r#"{"a":1}"#,
  ));
  assert!(!validates(
    r#"root = { ? "a" => uint }"#,
    r#"{"a":"unclaimed"}"#,
  ));
  assert!(validates(r#"root = { ? "a" => uint }"#, "{}"));
}
