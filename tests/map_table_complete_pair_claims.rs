#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(feature = "json")]
#![cfg(not(feature = "lsp"))]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{validate_cbor_from_slice, validate_json_from_str};

fn json_validates(schema: &str, instance: &str) -> bool {
  validate_json_from_str(schema, instance, None).is_ok()
}

fn cbor_validates(schema: &str, hex: &str) -> bool {
  let bytes = hex::decode(hex).unwrap();
  validate_cbor_from_slice(schema, &bytes, None).is_ok()
}

fn both_validate(schema: &str, json: &str, cbor_hex: &str) {
  let json_result = json_validates(schema, json);
  let cbor_result = cbor_validates(schema, cbor_hex);
  assert!(
    json_result && cbor_result,
    "expected both formats to match {schema}; JSON {json}: {json_result}, CBOR {cbor_hex}: {cbor_result}",
    schema = schema,
    json = json,
    json_result = json_result,
    cbor_hex = cbor_hex,
    cbor_result = cbor_result,
  );
}

fn both_reject(schema: &str, json: &str, cbor_hex: &str) {
  let json_result = json_validates(schema, json);
  let cbor_result = cbor_validates(schema, cbor_hex);
  assert!(
    !json_result && !cbor_result,
    "expected both formats to reject {schema}; JSON {json}: {json_result}, CBOR {cbor_hex}: {cbor_result}",
    schema = schema,
    json = json,
    json_result = json_result,
    cbor_hex = cbor_hex,
    cbor_result = cbor_result,
  );
}

#[test]
fn repeating_tables_claim_only_complete_key_value_matches() {
  // RFC 8610 Appendix C matches map pairs, not keys in isolation. The broad
  // table cannot claim this pair because its value is not a uint, so the
  // later specific member remains able to own it.
  both_validate(
    "m = { * any => uint, extra: tstr }",
    r#"{"extra":"hi"}"#,
    "a1656578747261626869",
  );

  // Two tables with the same key domain can partition pairs by value type.
  both_validate(
    "m = { * any => uint, * any => tstr }",
    r#"{"a":"x","b":1}"#,
    "a261616178616201",
  );

  // The already-working specific-before-general order remains valid.
  both_validate(
    "m = { extra: tstr, * any => uint }",
    r#"{"extra":"hi","b":1}"#,
    "a2656578747261626869616201",
  );
}

#[test]
fn occurrence_bounds_count_successful_pair_matches() {
  // The upper bound applies after complete-pair probing. The first candidate
  // fails the uint value type, but probing must continue until one compatible
  // pair has been found; the later table then owns the text-valued pair.
  both_validate(
    "m = { 1*1 any => uint, * any => tstr }",
    r#"{"a":"x","b":1}"#,
    "a261616178616201",
  );

  // A lower bound is likewise based on complete pairs, not key candidates.
  both_reject(
    "m = { + any => uint, * any => tstr }",
    r#"{"a":"x"}"#,
    "a161616178",
  );
}

#[test]
fn complete_pair_claims_preserve_greedy_and_cut_behavior() {
  // A compatible pair is greedily claimed by the first table. A later
  // required member cannot reuse it.
  both_reject(
    "m = { * any => uint, extra: uint }",
    r#"{"extra":1}"#,
    "a165657874726101",
  );

  // With no compatible owner, a bad table value remains invalid.
  both_reject("m = { * any => uint }", r#"{"a":"x"}"#, "a161616178");

  // The colon shortcut carries a cut (RFC 8610 Section 3.5.4). Once its key
  // matches, a later table must not rescue its bad value.
  both_reject(
    "m = { extra: uint, * any => tstr }",
    r#"{"extra":"hi"}"#,
    "a1656578747261626869",
  );
}
