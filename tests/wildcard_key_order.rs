#![cfg(feature = "std")]
#![cfg(not(feature = "lsp"))]

use cddl::validate_cbor_from_slice;

/// Regression tests for https://github.com/anweiss/cddl/issues/643
///
/// Wildcard map entries used to reject any key that did not match their own key
/// type, which made validation depend on the order the entries were written in.
/// A key that one entry cannot claim must be left for a later entry; only keys
/// that no entry claims are an error.

fn validates(cddl: &str, hex: &str) -> bool {
  let cbor = hex::decode(hex).unwrap();
  validate_cbor_from_slice(cddl, &cbor, None).is_ok()
}

/// `{"a": 1, 2: 3}`
const MIXED_MAP: &str = "a26161010203";

#[test]
fn wildcard_entry_order_is_irrelevant() {
  assert!(validates(
    "start = { * tstr => int, * int => int }",
    MIXED_MAP
  ));
  assert!(validates(
    "start = { * int => int, * tstr => int }",
    MIXED_MAP
  ));
}

#[test]
fn wildcard_entry_order_is_irrelevant_for_three_key_types() {
  // {"a": 1, 2: 3, true: 4}
  let m = "a36161010203f504";

  assert!(validates(
    "start = { * tstr => int, * int => int, * bool => int }",
    m
  ));
  assert!(validates(
    "start = { * bool => int, * tstr => int, * int => int }",
    m
  ));
  assert!(validates(
    "start = { * int => int, * bool => int, * tstr => int }",
    m
  ));
}

#[test]
fn unclaimed_keys_are_still_rejected() {
  // No entry can claim the integer key 2.
  assert!(!validates("start = { * tstr => int }", MIXED_MAP));
  // No entry can claim the text key "a".
  assert!(!validates("start = { * int => int }", MIXED_MAP));
}

#[test]
fn wildcard_entries_still_enforce_the_value_type() {
  // {"a": "a"} against * tstr => int
  assert!(!validates("start = { * tstr => int }", "a161616161"));
  assert!(validates("start = { * tstr => int }", "a1616101"));
}

#[test]
fn wildcard_occurrence_bounds_are_preserved() {
  // {} is fine for `*` but not for `+`
  assert!(validates("start = { * tstr => int }", "a0"));
  assert!(!validates("start = { + tstr => int }", "a0"));
}
