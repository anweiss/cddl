//! Incremental type-choice resolution regressions.
//!
//! RFC 8610 §2.2.2 makes every `/=` right-hand side an additional arm of
//! the named type choice, and Appendix C requires those arms to be
//! populated in source order.

#![cfg(feature = "std")]
#![cfg(all(feature = "cbor", feature = "json"))]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{cddl_from_str, validate_cbor_from_slice, validate_json_from_str};

const BASE_FIRST_ROOT: &str = r#"
extended = bool
extended /= text
extended /= uint
"#;

const BASE_FIRST_ALIAS: &str = r#"
root = extended
extended = bool
extended /= text
extended /= uint
"#;

const ALTERNATE_ONLY_ROOT: &str = r#"
extended /= text
extended /= uint
"#;

const ALTERNATE_ONLY_ALIAS: &str = r#"
root = extended
extended /= text
extended /= uint
"#;

fn assert_json_three_arm_choice(schema: &str) {
  // Accept the base after two failed additions, and accept either addition.
  validate_json_from_str(schema, "true", None).unwrap();
  validate_json_from_str(schema, r#""x""#, None).unwrap();
  validate_json_from_str(schema, "0", None).unwrap();

  // Reject a value outside every arm.
  validate_json_from_str(schema, "null", None).unwrap_err();
}

fn assert_cbor_three_arm_choice(schema: &str) {
  // Accept the base after two failed additions, and accept either addition.
  validate_cbor_from_slice(schema, &[0xf5], None).unwrap();
  validate_cbor_from_slice(schema, &[0x61, b'x'], None).unwrap();
  validate_cbor_from_slice(schema, &[0x00], None).unwrap();

  // Reject a value outside every arm.
  validate_cbor_from_slice(schema, &[0xf6], None).unwrap_err();
}

#[test]
fn json_incremental_choice_is_root_independent_and_transactional() {
  assert_json_three_arm_choice(BASE_FIRST_ROOT);
  assert_json_three_arm_choice(BASE_FIRST_ALIAS);
}

#[test]
fn cbor_incremental_choice_is_root_independent_and_transactional() {
  assert_cbor_three_arm_choice(BASE_FIRST_ROOT);
  assert_cbor_three_arm_choice(BASE_FIRST_ALIAS);
}

#[test]
fn alternate_only_choice_remains_valid_at_root_and_through_alias() {
  for schema in [ALTERNATE_ONLY_ROOT, ALTERNATE_ONLY_ALIAS] {
    validate_json_from_str(schema, r#""x""#, None).unwrap();
    validate_json_from_str(schema, "0", None).unwrap();
    validate_json_from_str(schema, "true", None).unwrap_err();

    validate_cbor_from_slice(schema, &[0x61, b'x'], None).unwrap();
    validate_cbor_from_slice(schema, &[0x00], None).unwrap();
    validate_cbor_from_slice(schema, &[0xf5], None).unwrap_err();
  }
}

#[test]
fn generic_rule_extended_by_a_matching_arm_keeps_validating() {
  // A rule that carries generic parameters may be extended with `/=` under
  // the same name. RFC 8610's grammar allows it (`rule = typename
  // [genericparm] S assignt S type` with `assignt = "=" / "/="`), and both
  // arms must honor the argument bound at the citation site.
  //
  // Pinned because the follow-up that teaches the duplicate check about
  // generic arity could reject this schema by accident, and because the
  // arms only resolve while the `/=` contribution spells its parameter the
  // same way the plain `=` definition does — binding is keyed by parameter
  // name. Ruby 0.12.14 is no oracle: it refuses the schema outright with
  // `Augment "/=" not implemented for generics`.
  let schema = "root = a<int>\na<t> = [t]\na<t> /= {k: t}\n";
  cddl_from_str(schema, false).unwrap();

  // Either arm matches, with `t` bound to `int` in both.
  validate_json_from_str(schema, "[1]", None).unwrap();
  validate_json_from_str(schema, r#"{"k":1}"#, None).unwrap();
  validate_cbor_from_slice(schema, &[0x81, 0x01], None).unwrap();
  validate_cbor_from_slice(schema, &[0xa1, 0x61, b'k', 0x01], None).unwrap();

  // The binding is real, not vacuous: a `tstr` where the argument says
  // `int` fails in both arms and both encodings.
  validate_json_from_str(schema, r#"["x"]"#, None).unwrap_err();
  validate_json_from_str(schema, r#"{"k":"x"}"#, None).unwrap_err();
  validate_cbor_from_slice(schema, &[0x81, 0x61, b'x'], None).unwrap_err();
  validate_cbor_from_slice(schema, &[0xa1, 0x61, b'k', 0x61, b'x'], None).unwrap_err();
}

#[test]
fn recursive_choice_arms_consume_nested_data() {
  // RFC 8610 §2.2.2 and Appendix C resolve every schema below to the choice
  // (uint / [t]) no matter how `t` is reached, and PEG matching recurses
  // into the array arm one data level at a time. Ruby cddl 0.12.14 accepts
  // every accepting row here, root and alias alike.
  const ROOT: &str = "t = uint\nt /= [t]\n";
  const ALIAS: &str = "root = t\nt = uint\nt /= [t]\n";
  const ALTERNATE_ONLY: &str = "t /= uint\nt /= [t]\n";

  for schema in [ROOT, ALIAS, ALTERNATE_ONLY] {
    for value in ["0", "[0]", "[[0]]", "[[[0]]]"] {
      validate_json_from_str(schema, value, None)
        .unwrap_or_else(|e| panic!("JSON {:?} rejected {}: {}", schema, value, e));
    }
    validate_json_from_str(schema, r#""x""#, None).unwrap_err();

    for value in [
      &[0x00][..],
      &[0x81, 0x00],
      &[0x81, 0x81, 0x00],
      &[0x81, 0x81, 0x81, 0x00],
    ] {
      validate_cbor_from_slice(schema, value, None)
        .unwrap_or_else(|e| panic!("CBOR {:?} rejected {:x?}: {}", schema, value, e));
    }
    validate_cbor_from_slice(schema, &[0x61, b'x'], None).unwrap_err();
  }

  // A value outside both arms must fail at every nesting depth instead of
  // being waved through by a coarse recursion guard once `t` repeats.
  for schema in [ROOT, ALIAS, ALTERNATE_ONLY] {
    validate_json_from_str(schema, "[[true]]", None).unwrap_err();
    validate_cbor_from_slice(schema, &[0x81, 0x81, 0xf5], None).unwrap_err();
  }
}

#[test]
fn mutually_recursive_incremental_choice_consumes_nested_data() {
  // A realistic recursive shape: a list whose items are scalars or lists.
  let schema = "list = [* item]\nitem = int\nitem /= list\n";

  validate_json_from_str(schema, "[[1], 2]", None).unwrap();
  validate_json_from_str(schema, "[[[3]], 4]", None).unwrap();
  validate_json_from_str(schema, r#"[["x"]]"#, None).unwrap_err();

  // [[1], 2] and [[[3]], 4]
  validate_cbor_from_slice(schema, &[0x82, 0x81, 0x01, 0x02], None).unwrap();
  validate_cbor_from_slice(schema, &[0x82, 0x81, 0x81, 0x03, 0x04], None).unwrap();
  // [["x"]]
  validate_cbor_from_slice(schema, &[0x81, 0x81, 0x61, b'x'], None).unwrap_err();
}

#[test]
fn recursive_alternate_only_reference_completes_instead_of_crashing() {
  // Before the shared resolver, the JSON validator overflowed its stack and
  // aborted the process on this schema.
  for schema in ["a /= a\n", "root = a\na /= a\n"] {
    validate_json_from_str(schema, "0", None).unwrap_err();

    // The CBOR recursion guard treats a revisited rule as satisfied, so the
    // verdict for this degenerate schema is a vacuous accept (`a = a`
    // behaves the same way); the regression pinned here is only that
    // validation completes.
    let _ = validate_cbor_from_slice(schema, &[0x00], None);
  }
}
