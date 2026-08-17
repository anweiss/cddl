//! Incremental (`/=` / `//=`) redefinition regressions.
//!
//! RFC 8610 Appendix C states one redefinition rule for `/=` and `//=`
//! together: the incremental operators populate a named choice in source
//! order, and a plain `=` is an error when the name is "already defined
//! with a different expression". Appendix C does not settle how a late
//! plain `=` orders against arms an earlier `/=` or `//=` already
//! contributed, and it would permit an identical redefinition; rejecting
//! every later plain `=` once the name exists is this crate's policy —
//! the policy the legacy pre-Pest parser enforced for all rules, and the
//! one Ruby cddl 0.12.14 applies when the expressions differ; Ruby accepts
//! an identical redefinition with a warning. The duplicate check in
//! `pest_bridge::convert_cddl` originally ignored incremental
//! definitions entirely, so a plain `=` after `/=` or `//=` parsed
//! successfully — in any kind combination — and the validators then
//! silently dropped one family's choice arms, giving the schema a
//! meaning Appendix C never defines.
//!
//! Ruby cddl 0.12.14 corroborates the rejection: it reports "Duplicate
//! rule definition t" for the cross-kind ordering and aborts in rule
//! merging (`strip_nodes` NoMethodError) on the group ordering, so
//! neither schema gets a verdict there either.

#![cfg(feature = "std")]
#![cfg(all(feature = "cbor", feature = "json"))]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{cddl_from_str, validate_cbor_from_slice, validate_json_from_str};

#[test]
fn base_group_definition_cannot_follow_an_incremental_group_definition() {
  for schema in [
    // Plain group rule after a group-choice alternate (probe P9a).
    "r = {g}\ng //= (k: int)\ng = (j: int)\n",
    // Same ordering behind no alias.
    "g //= (k: int)\ng = (j: int)\n",
    // Socket names share the rule namespace with every other name.
    "$$foo //= (k: int)\n$$foo = (j: int)\nroot = {$$foo}\n",
  ] {
    let error = cddl_from_str(schema, false).unwrap_err();
    assert!(
      error.contains("already defined"),
      "unexpected parser error for {:?}: {}",
      schema,
      error
    );
  }
}

#[test]
fn base_type_definition_cannot_follow_an_incremental_type_definition() {
  for schema in [
    "extended /= text\nextended = bool\n",
    "root = extended\nextended /= text\nextended = bool\n",
    // Ruby cddl 0.12.14 also rejects this ordering for socket names
    // ("Duplicate rule definition $foo").
    "$foo /= int\n$foo = text\nroot = text\n",
    // Stricter than Appendix C's literal wording ("already defined with
    // a *different* expression") and than Ruby, which accepts an
    // identical redefinition with only a warning; the crate has always
    // rejected duplicate plain definitions without comparing
    // expressions, and the incremental check inherits that policy.
    "a /= text\na = text\n",
    // A generic head after `/=` of the same base name is likewise
    // rejected, for one reason: RFC 8610 makes the bare `id` the rule's
    // identity (`typename = id`, and §3.10 adds formal parameters "after
    // the name being defined" — the parameters follow the name and are
    // not part of it). So this is the same `=`-after-`/=` case as above,
    // not a generics rule: the `<t>` never enters the argument. Ruby
    // keeps both names live by dispatching on citation arity, an
    // extension with no RFC basis it does not even carry to the
    // definition side.
    "a /= text\na<t> = int\n",
  ] {
    let error = cddl_from_str(schema, false).unwrap_err();
    assert!(
      error.contains("already defined"),
      "unexpected parser error for {:?}: {}",
      schema,
      error
    );
  }
}

#[test]
fn base_definition_cannot_change_kind_after_an_incremental_definition() {
  for schema in [
    // Type-choice alternate, then a plain rule whose parenthesized
    // right-hand side parses as a group rule (probe P9b). Ruby 0.12.14
    // rejects this ordering with "Duplicate rule definition t".
    "r = t\nt /= int\nt = (j: int)\n",
    // Group-choice alternate, then a plain type rule.
    "r = {g}\ng //= (k: int)\ng = tstr\n",
  ] {
    let error = cddl_from_str(schema, false).unwrap_err();
    assert!(
      error.contains("already defined"),
      "unexpected parser error for {:?}: {}",
      schema,
      error
    );
  }
}

#[cfg(feature = "ast-span")]
#[test]
fn duplicate_rule_errors_carry_the_offending_rule_position() {
  // Every duplicate-rule arm must report the later, offending rule's real
  // span, not `Position::default()`'s line 1, column 1. One schema per
  // arm: plain-after-plain, plain-after-`/=`, plain-after-`//=`, and the
  // cross-kind orderings.
  for schema in [
    "a = int\nb = tstr\na = tstr\n",
    "t /= int\nx = tstr\nt = tstr\n",
    "g //= (k: int)\nx = tstr\ng = (j: int)\n",
    "t /= int\nx = tstr\nt = (j: int)\n",
    "g //= (k: int)\nx = tstr\ng = tstr\n",
  ] {
    let error = cddl_from_str(schema, false).unwrap_err();
    assert!(
      error.contains("already defined"),
      "unexpected parser error for {:?}: {}",
      schema,
      error
    );
    // The offending duplicate sits on line 3 in every schema above; the
    // parser error Display includes the debug-formatted position.
    assert!(
      error.contains("line: 3"),
      "duplicate-rule error for {:?} lost the rule position: {}",
      schema,
      error
    );
  }

  // Exercise the column calculation and byte offsets, rather than only the
  // line stored in the AST span. The multibyte comment makes the byte range
  // differ from a character-counted offset, and the duplicate is indented so
  // a default column cannot satisfy the assertion.
  let schema = "a = int\n; é\n  a = tstr\n";
  let error = cddl::pest_bridge::cddl_from_pest_str(schema).unwrap_err();
  if let cddl::parser::Error::PARSER { position, msg } = error {
    assert!(
      msg.short.contains("already defined"),
      "unexpected parser error for {:?}: {}",
      schema,
      msg
    );
    assert_eq!(position.line, 3);
    assert_eq!(position.column, 3);
    assert_eq!(position.range, (15, 24));
    assert_eq!(position.index, 15);
  } else {
    panic!("unexpected parser error for {:?}: {}", schema, error);
  }
}

#[test]
fn incremental_definitions_after_a_plain_base_stay_valid() {
  // Accepting control: extending an existing plain type rule with `/=`
  // is the RFC's own example shape and must keep parsing, including an
  // identical arm (only a plain `=` redefinition is rejected).
  cddl_from_str("a = text\na /= text\n", false).unwrap();
  cddl_from_str("a = bool\na /= text\na /= uint\n", false).unwrap();

  // The check is name-based and generic-blind, so a rule carrying
  // generic parameters may still be extended under the same name (RFC
  // 8610 grammar: `rule = typename [genericparm] S assignt S type` with
  // `assignt = "=" / "/="`). Parse acceptance only; the arms' binding
  // semantics are validator scope.
  cddl_from_str("root = a<int>\na<t> = [t]\na<t> /= {k: t}\n", false).unwrap();
}

#[test]
fn incremental_group_chain_stays_valid_at_root_and_through_alias() {
  // Accepting controls: a pure `//=` chain is the well-formed way to build
  // a named group choice, whether or not a plain base rule ever existed.
  // Ruby 0.12.14 agrees on all of these vectors, in both encodings.
  const ALIAS: &str = "r = {g}\ng //= (k: int)\ng //= (j: int)\n";
  const REORDERED: &str = "g //= (k: int)\ng //= (j: int)\nr = {g}\n";

  for schema in [ALIAS, REORDERED] {
    validate_json_from_str(schema, r#"{"k":1}"#, None).unwrap();
    validate_json_from_str(schema, r#"{"j":1}"#, None).unwrap();
    validate_json_from_str(schema, r#"{"x":1}"#, None).unwrap_err();

    // {"k":1} / {"j":1} / {"x":1}
    validate_cbor_from_slice(schema, &[0xa1, 0x61, b'k', 0x01], None).unwrap();
    validate_cbor_from_slice(schema, &[0xa1, 0x61, b'j', 0x01], None).unwrap();
    validate_cbor_from_slice(schema, &[0xa1, 0x61, b'x', 0x01], None).unwrap_err();
  }
}

// NOTE deliberately absent here: the base-first ordering `g = (k: int)`
// then `g //= (j: int)` is valid per Appendix C (base arm first, extension
// arms after), and Ruby 0.12.14 accepts both keys — but this crate's
// validators currently drop the plain base arm and honor only the `//=`
// arms. That is a pre-existing validator-side resolution defect, separate
// from the parse-time duplicate check pinned by this file, and is tracked
// as its own issue.
