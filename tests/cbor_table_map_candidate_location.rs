//! Recursive CBOR table-map value regressions.
//!
//! RFC 8610 Sections 2.1.2 and 3.5.2 require both sides of every table-map
//! pair to match: in `{* x => y}`, each key has type `x` and each value has
//! type `y`. A recursion guard must therefore not waive validation of a
//! nested pair value.

#![cfg(feature = "std")]
#![cfg(all(feature = "cbor", feature = "json"))]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{validate_cbor_from_slice, validate_json_from_str};

fn json_validates(schema: &str, instance: &str) -> bool {
  validate_json_from_str(schema, instance, None).is_ok()
}

fn cbor_validates(schema: &str, hex: &str) -> bool {
  let bytes = hex::decode(hex).unwrap();
  validate_cbor_from_slice(schema, &bytes, None).is_ok()
}

fn both_accept(schema: &str, json: &str, cbor_hex: &str) {
  let json_result = json_validates(schema, json);
  let cbor_result = cbor_validates(schema, cbor_hex);
  assert!(
    json_result && cbor_result,
    "expected both formats to accept; JSON {}: {}, CBOR {}: {}\nschema:\n{}",
    json,
    json_result,
    cbor_hex,
    cbor_result,
    schema,
  );
}

fn both_reject(schema: &str, json: &str, cbor_hex: &str) {
  let json_result = json_validates(schema, json);
  let cbor_result = cbor_validates(schema, cbor_hex);
  assert!(
    !json_result && !cbor_result,
    "expected both formats to reject; JSON {}: {}, CBOR {}: {}\nschema:\n{}",
    json,
    json_result,
    cbor_hex,
    cbor_result,
    schema,
  );
}

#[test]
fn recursive_table_values_are_validated_at_every_depth() {
  let schema = "t = uint\nt /= {* tstr => t}\n";

  both_accept(schema, r#"{"k":0}"#, "a1616b00");
  both_accept(schema, r#"{"k":{"k":0}}"#, "a1616ba1616b00");
  both_accept(schema, r#"{"k":{"k":{"k":0}}}"#, "a1616ba1616ba1616b00");

  // Ruby cddl 0.12.14 rejects both ill-typed vectors in JSON and CBOR.
  both_reject(schema, r#"{"k":{"k":true}}"#, "a1616ba1616bf5");
  both_reject(schema, r#"{"k":{"k":{"k":true}}}"#, "a1616ba1616ba1616bf5");
}

#[test]
fn recursive_table_values_remain_checked_behind_an_alias() {
  let schema = "root = t\nt = uint\nt /= {* tstr => t}\n";

  both_accept(schema, r#"{"k":0}"#, "a1616b00");
  both_accept(schema, r#"{"k":{"k":0}}"#, "a1616ba1616b00");

  // Ruby cddl 0.12.14 rejects these at the first ill-typed value.
  both_reject(schema, r#"{"k":true}"#, "a1616bf5");
  both_reject(schema, r#"{"k":"x"}"#, "a1616b6178");
  both_reject(schema, r#"{"k":{"k":true}}"#, "a1616ba1616bf5");
}

#[test]
fn inline_recursive_table_choice_checks_nested_values() {
  let schema = "t = uint / {* tstr => t}\n";

  both_accept(schema, r#"{"k":{"k":0}}"#, "a1616ba1616b00");
  // This predates incremental-choice support; Ruby cddl 0.12.14 rejects it.
  both_reject(schema, r#"{"k":{"k":true}}"#, "a1616ba1616bf5");
}

#[test]
fn alternate_only_recursive_table_checks_nested_values() {
  let schema = "t /= uint\nt /= {* tstr => t}\n";

  // Ruby cddl 0.12.14 accepts these well-typed controls in both encodings.
  both_accept(schema, r#"{"k":0}"#, "a1616b00");
  both_accept(schema, r#"{"k":{"k":0}}"#, "a1616ba1616b00");

  // This is the sharp S02/M04 coupling vector: pre-S02 Rust and Ruby reject,
  // while S02 alone accepts the nested true in CBOR.
  both_reject(schema, r#"{"k":true}"#, "a1616bf5");
  both_reject(schema, r#"{"k":"x"}"#, "a1616b6178");
  both_reject(schema, r#"{"k":{"k":true}}"#, "a1616ba1616bf5");
}

#[test]
fn multiple_table_arms_do_not_hide_a_bad_recursive_value() {
  let schema = "t /= {* tstr => t}\nt /= {\"z\" => uint}\n";

  both_reject(schema, r#"{"k":{"k":true}}"#, "a1616ba1616bf5");
}

#[test]
fn non_recursive_and_specific_key_map_controls_keep_their_verdicts() {
  let table = "m = {* tstr => int}\n";
  both_accept(table, r#"{"k":1}"#, "a1616b01");
  both_reject(table, r#"{"k":"x"}"#, "a1616b6178");

  // Specific-key descent already carries the pair key in both validators.
  let specific = "t = uint\nt /= {\"k\" => t}\n";
  both_accept(specific, r#"{"k":0}"#, "a1616b00");
  both_accept(specific, r#"{"k":{"k":0}}"#, "a1616ba1616b00");
  both_accept(specific, r#"{"k":{"k":{"k":0}}}"#, "a1616ba1616ba1616b00");
  both_reject(specific, r#"{"k":{"k":true}}"#, "a1616ba1616bf5");
}
