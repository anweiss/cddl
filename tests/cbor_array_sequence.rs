//! Array group/occurrence sequence-matching tests.
//!
//! Generated from a vector set cross-checked against the Ruby reference
//! implementation (cddl gem 0.12.14)
//!
//! - `regression`: behavior that was already correct before the sequence
//!   matcher landed and must not change.
//! - `post_fix`: spec-correct behavior (confirmed by the Ruby reference
//!   implementation) that the old positional array validator got wrong, in
//!   both directions: spec-valid instances rejected AND spec-invalid
//!   instances accepted. Fixed by the array sequence matcher.
#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::validate_cbor_from_slice;

// RFC 8610 Section 3.3 defines the primitive names used throughout
// these tests (for example, int, tstr, and bool). Per-test comments cite
// the structural rules that motivate each expected match result.
//
// Two normative appendices underpin every verdict in this file:
// - Appendix C (Matching Rules): an array matches when its element sequence
//   matches the group, and "an occurrence indicator modifies the group given
//   to its right by requiring the group to match the sequence ... in
//   sequence", i.e. repetition quantifies the group's whole entry sequence,
//   not the array length.
// - Appendix A (PEGs): matching semantics are PEG — occurrence indicators
//   are greedy with no backtracking out of a repetition ('"*a a" in CDDL
//   syntax never can match anything'), and "/" and "//" are prioritized
//   choice that locks in the first successful alternative.
//
// Known tension between the two: Appendix C's "a (possibly infinite) group
// choice" wording, read alone, could permit shorter-than-greedy matches
// (e.g. [* int, int] matching [1]). Appendix A's explicit '*a a' example
// resolves it as greedy, and the Ruby reference implementation agrees; the
// greedy_star_then_int tests encode that resolution.

mod regression {
  use super::*;

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.8, 3.8.4.
  #[test]
  fn cbor_control_elem() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* bytes .cbor b]\nb = [int, tstr]", &[0x80], None).unwrap();
    // ACCEPT [h'82016178']
    validate_cbor_from_slice(
      "a = [* bytes .cbor b]\nb = [int, tstr]",
      &[0x81, 0x44, 0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
    // REJECT [h'8101']
    validate_cbor_from_slice(
      "a = [* bytes .cbor b]\nb = [int, tstr]",
      &[0x81, 0x42, 0x81, 0x01],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.8, 3.8.4.
  #[test]
  fn cbor_control_group_inside() {
    // REJECT [h'8101']
    validate_cbor_from_slice(
      "a = [bytes .cbor b]\nb = [* (int, tstr)]",
      &[0x81, 0x42, 0x81, 0x01],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.2, 3.2, 3.4, 3.11.
  #[test]
  fn choice_diff_lengths() {
    // ACCEPT [true]
    validate_cbor_from_slice("a = [(bool // int, tstr)]", &[0x81, 0xf5], None).unwrap();
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [(bool // int, tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
    // REJECT [1]
    validate_cbor_from_slice("a = [(bool // int, tstr)]", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.2.2.2, 3.2, 3.4; Appendix B (& group choice).
  #[test]
  fn choice_from_group() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* &(1, 2)]", &[0x80], None).unwrap();
    // ACCEPT [1, 2, 1]
    validate_cbor_from_slice("a = [* &(1, 2)]", &[0x83, 0x01, 0x02, 0x01], None).unwrap();
    // REJECT [3]
    validate_cbor_from_slice("a = [* &(1, 2)]", &[0x81, 0x03], None).unwrap_err();
  }

  // RFC 8610: Sections 2.2.2.2, 3.2, 3.4; Appendix B (& groupname).
  #[test]
  fn choice_from_named_group() {
    // REJECT [3]
    validate_cbor_from_slice("a = [* e]\ne = &g\ng = (1, 2)", &[0x81, 0x03], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.4.
  #[test]
  fn empty_array() {
    // ACCEPT []
    validate_cbor_from_slice("a = []", &[0x80], None).unwrap();
    // REJECT [1]
    validate_cbor_from_slice("a = []", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix A (greedy PEG occurrence must terminate
  // on zero-width repetitions); Appendix B (parenthesized group).
  #[test]
  fn empty_group_star() {
    // NOTE: Ruby gem infinite-loops on zero-width group repetition; expected per RFC: () matches trivially, then int matches. Matcher must terminate.
    // ACCEPT [1]
    validate_cbor_from_slice("a = [* (), int]", &[0x81, 0x01], None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn exact_1_1() {
    // REJECT []
    validate_cbor_from_slice("a = [1*1 (int, tstr)]", &[0x80], None).unwrap_err();
    // REJECT [1]
    validate_cbor_from_slice("a = [1*1 (int, tstr)]", &[0x81, 0x01], None).unwrap_err();
    // REJECT [1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [1*1 (int, tstr)]",
      &[0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap_err();
    // REJECT ["x", 1]
    validate_cbor_from_slice("a = [1*1 (int, tstr)]", &[0x82, 0x61, 0x78, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn exact_2_2() {
    // REJECT [1, "x"]
    validate_cbor_from_slice("a = [2*2 (int, tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap_err();
    // REJECT [1, "x", 2, "y", 3, "z"]
    validate_cbor_from_slice(
      "a = [2*2 (int, tstr)]",
      &[0x86, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79, 0x03, 0x61, 0x7a],
      None,
    )
    .unwrap_err();
    // REJECT [1, "x", 2]
    validate_cbor_from_slice(
      "a = [2*2 (int, tstr)]",
      &[0x83, 0x01, 0x61, 0x78, 0x02],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.10.
  #[test]
  fn generic_elem() {
    // REJECT [["x"]]
    validate_cbor_from_slice(
      "a = [* box<int>]\nbox<T> = [T]",
      &[0x81, 0x81, 0x61, 0x78],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.10; Appendix B (groupname entry).
  #[test]
  fn generic_group_elem() {
    // REJECT [1] (Ruby-verified)
    validate_cbor_from_slice("a = [* g<int>]\ng<T> = (T, T)", &[0x81, 0x01], None).unwrap_err();
    // REJECT [1, "x"] (Ruby-verified)
    validate_cbor_from_slice(
      "a = [* g<int>]\ng<T> = (T, T)",
      &[0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn group_between_siblings() {
    // REJECT [true, 1, false]
    validate_cbor_from_slice(
      "a = [bool, * (int, tstr), bool]",
      &[0x83, 0xf5, 0x01, 0xf4],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.2, 3.2, 3.4.
  #[test]
  fn group_choice_inside() {
    // REJECT [1, 2]
    validate_cbor_from_slice(
      "a = [1*1 (int, tstr // tstr, int)]",
      &[0x82, 0x01, 0x02],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_after_sibling() {
    // REJECT ["h", 1]
    validate_cbor_from_slice(
      "a = [tstr, * pair]\npair = (int, tstr)",
      &[0x82, 0x61, 0x68, 0x01],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_exact() {
    // REJECT [1]
    validate_cbor_from_slice("a = [1*1 b]\nb = (int, tstr)", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_star() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* pair]\npair = (int, tstr)", &[0x80], None).unwrap();
    // REJECT [1, "x", 2]
    validate_cbor_from_slice(
      "a = [* pair]\npair = (int, tstr)",
      &[0x83, 0x01, 0x61, 0x78, 0x02],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_alias() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* zip]\nzip = int", &[0x80], None).unwrap();
    // ACCEPT [1, 2]
    validate_cbor_from_slice("a = [* zip]\nzip = int", &[0x82, 0x01, 0x02], None).unwrap();
    // REJECT ["x"]
    validate_cbor_from_slice("a = [* zip]\nzip = int", &[0x81, 0x61, 0x78], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_exact() {
    // ACCEPT [1, 2, 3]
    validate_cbor_from_slice("a = [3*3 int]", &[0x83, 0x01, 0x02, 0x03], None).unwrap();
    // REJECT [1, 2]
    validate_cbor_from_slice("a = [3*3 int]", &[0x82, 0x01, 0x02], None).unwrap_err();
    // REJECT [1, 2, 3, 4]
    validate_cbor_from_slice("a = [3*3 int]", &[0x84, 0x01, 0x02, 0x03, 0x04], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_lower() {
    // ACCEPT [1, 2, 3]
    validate_cbor_from_slice("a = [3* int]", &[0x83, 0x01, 0x02, 0x03], None).unwrap();
    // ACCEPT [1, 2, 3, 4]
    validate_cbor_from_slice("a = [3* int]", &[0x84, 0x01, 0x02, 0x03, 0x04], None).unwrap();
    // REJECT [1, 2]
    validate_cbor_from_slice("a = [3* int]", &[0x82, 0x01, 0x02], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_opt() {
    // ACCEPT []
    validate_cbor_from_slice("a = [? int]", &[0x80], None).unwrap();
    // ACCEPT [1]
    validate_cbor_from_slice("a = [? int]", &[0x81, 0x01], None).unwrap();
    // REJECT [1, 2]
    validate_cbor_from_slice("a = [? int]", &[0x82, 0x01, 0x02], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_plus() {
    // ACCEPT [1]
    validate_cbor_from_slice("a = [+ int]", &[0x81, 0x01], None).unwrap();
    // ACCEPT [1, 2, 3]
    validate_cbor_from_slice("a = [+ int]", &[0x83, 0x01, 0x02, 0x03], None).unwrap();
    // REJECT []
    validate_cbor_from_slice("a = [+ int]", &[0x80], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_range() {
    // ACCEPT [1]
    validate_cbor_from_slice("a = [1*3 int]", &[0x81, 0x01], None).unwrap();
    // ACCEPT [1, 2, 3]
    validate_cbor_from_slice("a = [1*3 int]", &[0x83, 0x01, 0x02, 0x03], None).unwrap();
    // REJECT []
    validate_cbor_from_slice("a = [1*3 int]", &[0x80], None).unwrap_err();
    // REJECT [1, 2, 3, 4]
    validate_cbor_from_slice("a = [1*3 int]", &[0x84, 0x01, 0x02, 0x03, 0x04], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_star() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* int]", &[0x80], None).unwrap();
    // ACCEPT [1, 2, 3]
    validate_cbor_from_slice("a = [* int]", &[0x83, 0x01, 0x02, 0x03], None).unwrap();
    // REJECT [1, "x"]
    validate_cbor_from_slice("a = [* int]", &[0x82, 0x01, 0x61, 0x78], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.1, 3.4.
  #[test]
  fn literal_elements() {
    // ACCEPT [1, 2, 3]
    validate_cbor_from_slice("a = [1, 2, 3]", &[0x83, 0x01, 0x02, 0x03], None).unwrap();
    // REJECT [1, 2]
    validate_cbor_from_slice("a = [1, 2, 3]", &[0x82, 0x01, 0x02], None).unwrap_err();
    // REJECT [1, 2, 4]
    validate_cbor_from_slice("a = [1, 2, 3]", &[0x83, 0x01, 0x02, 0x04], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn lower_bound_only() {
    // REJECT [1, "x"]
    validate_cbor_from_slice("a = [2* (int, tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.5, 3.5.1; Appendix B (groupname entry).
  #[test]
  fn map_group_ref() {
    // ACCEPT {"x": 1, "y": "h"}
    validate_cbor_from_slice(
      "a = {g}\ng = (x: int, y: tstr)",
      &[0xa2, 0x61, 0x78, 0x01, 0x61, 0x79, 0x61, 0x68],
      None,
    )
    .unwrap();
    // REJECT {"x": 1}
    validate_cbor_from_slice(
      "a = {g}\ng = (x: int, y: tstr)",
      &[0xa1, 0x61, 0x78, 0x01],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.5.1.
  #[test]
  fn map_in_array() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* {k: int}]", &[0x80], None).unwrap();
    // ACCEPT [{"k": 1}]
    validate_cbor_from_slice("a = [* {k: int}]", &[0x81, 0xa1, 0x61, 0x6b, 0x01], None).unwrap();
    // REJECT [{"k": "x"}]
    validate_cbor_from_slice(
      "a = [* {k: int}]",
      &[0x81, 0xa1, 0x61, 0x6b, 0x61, 0x78],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.5.1; Appendix B (groupname entry).
  #[test]
  fn map_in_array_with_group() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* {g}]\ng = (x: int)", &[0x80], None).unwrap();
    // ACCEPT [{"x": 1}]
    validate_cbor_from_slice(
      "a = [* {g}]\ng = (x: int)",
      &[0x81, 0xa1, 0x61, 0x78, 0x01],
      None,
    )
    .unwrap();
    // REJECT [{"x": "h"}]
    validate_cbor_from_slice(
      "a = [* {g}]\ng = (x: int)",
      &[0x81, 0xa1, 0x61, 0x78, 0x61, 0x68],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.5.1; Appendix B (parenthesized group).
  #[test]
  fn map_inline_group() {
    // ACCEPT {"x": 1, "y": "h"}
    validate_cbor_from_slice(
      "a = {(x: int, y: tstr)}",
      &[0xa2, 0x61, 0x78, 0x01, 0x61, 0x79, 0x61, 0x68],
      None,
    )
    .unwrap();
    // REJECT {"x": 1}
    validate_cbor_from_slice("a = {(x: int, y: tstr)}", &[0xa1, 0x61, 0x78, 0x01], None)
      .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.5.1.
  #[test]
  fn map_optional_member() {
    // ACCEPT {"x": 1}
    validate_cbor_from_slice("a = {x: int, ? y: tstr}", &[0xa1, 0x61, 0x78, 0x01], None).unwrap();
    // ACCEPT {"x": 1, "y": "h"}
    validate_cbor_from_slice(
      "a = {x: int, ? y: tstr}",
      &[0xa2, 0x61, 0x78, 0x01, 0x61, 0x79, 0x61, 0x68],
      None,
    )
    .unwrap();
    // REJECT {}
    validate_cbor_from_slice("a = {x: int, ? y: tstr}", &[0xa0], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.5.1.
  #[test]
  fn map_record() {
    // ACCEPT {"x": 1, "y": "h"}
    validate_cbor_from_slice(
      "a = {x: int, y: tstr}",
      &[0xa2, 0x61, 0x78, 0x01, 0x61, 0x79, 0x61, 0x68],
      None,
    )
    .unwrap();
    // REJECT {"x": 1}
    validate_cbor_from_slice("a = {x: int, y: tstr}", &[0xa1, 0x61, 0x78, 0x01], None).unwrap_err();
    // REJECT {"x": "h", "y": "h"}
    validate_cbor_from_slice(
      "a = {x: int, y: tstr}",
      &[0xa2, 0x61, 0x78, 0x61, 0x68, 0x61, 0x79, 0x61, 0x68],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.5.2.
  #[test]
  fn map_star_members() {
    // ACCEPT {}
    validate_cbor_from_slice("a = {* tstr => int}", &[0xa0], None).unwrap();
    // ACCEPT {"a": 1, "b": 2}
    validate_cbor_from_slice(
      "a = {* tstr => int}",
      &[0xa2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x02],
      None,
    )
    .unwrap();
    // REJECT {"a": "x"}
    validate_cbor_from_slice("a = {* tstr => int}", &[0xa1, 0x61, 0x61, 0x61, 0x78], None)
      .unwrap_err();
  }

  // RFC 8610: Sections 2.2.2, 3.2, 3.4; Appendix B (choice extension).
  #[test]
  fn named_type_choice_elem() {
    // REJECT [true]
    validate_cbor_from_slice(
      "a = [* elem]\nelem = int\nelem /= tstr",
      &[0x81, 0xf5],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn nested_array() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* [int]]", &[0x80], None).unwrap();
    // ACCEPT [[1], [2]]
    validate_cbor_from_slice("a = [* [int]]", &[0x82, 0x81, 0x01, 0x81, 0x02], None).unwrap();
    // REJECT [1]
    validate_cbor_from_slice("a = [* [int]]", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.4.
  #[test]
  fn nested_array_literal() {
    // ACCEPT [1, [2, 3], [4, 5]]
    validate_cbor_from_slice(
      "a = [int, [int, int], [int, int]]",
      &[0x83, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05],
      None,
    )
    .unwrap();
    // REJECT [1, [2, 3]]
    validate_cbor_from_slice(
      "a = [int, [int, int], [int, int]]",
      &[0x82, 0x01, 0x82, 0x02, 0x03],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn nested_parens() {
    // REJECT [1]
    validate_cbor_from_slice("a = [1*1 (int, (tstr))]", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn nested_star_of_star() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* [* (int, tstr)]]", &[0x80], None).unwrap();
    // ACCEPT [[]]
    validate_cbor_from_slice("a = [* [* (int, tstr)]]", &[0x81, 0x80], None).unwrap();
    // REJECT [[1]]
    validate_cbor_from_slice("a = [* [* (int, tstr)]]", &[0x81, 0x81, 0x01], None).unwrap_err();
    // REJECT [1, "x"]
    validate_cbor_from_slice("a = [* [* (int, tstr)]]", &[0x82, 0x01, 0x61, 0x78], None)
      .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn occur_inside_occur() {
    // REJECT [1, "x"]
    validate_cbor_from_slice("a = [* (int, 2*2 tstr)]", &[0x82, 0x01, 0x61, 0x78], None)
      .unwrap_err();
    // REJECT [1, "x", "y", "z"]
    validate_cbor_from_slice(
      "a = [* (int, 2*2 tstr)]",
      &[0x84, 0x01, 0x61, 0x78, 0x61, 0x79, 0x61, 0x7a],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn occur_on_single_entry_group() {
    // REJECT [1, "x", 2]
    validate_cbor_from_slice(
      "a = [2*2 (int, 1*1 (tstr))]",
      &[0x83, 0x01, 0x61, 0x78, 0x02],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn one_or_more() {
    // REJECT []
    validate_cbor_from_slice("a = [+ (int, tstr)]", &[0x80], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn optional() {
    // ACCEPT []
    validate_cbor_from_slice("a = [? (int, tstr)]", &[0x80], None).unwrap();
    // REJECT [1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [? (int, tstr)]",
      &[0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn optional_middle() {
    // REJECT [1, "x"]
    validate_cbor_from_slice("a = [int, ? tstr, bool]", &[0x82, 0x01, 0x61, 0x78], None)
      .unwrap_err();
    // REJECT [1, "x", true, true]
    validate_cbor_from_slice(
      "a = [int, ? tstr, bool]",
      &[0x84, 0x01, 0x61, 0x78, 0xf5, 0xf5],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn prefix_then_star() {
    // REJECT [1]
    validate_cbor_from_slice("a = [tstr, * int]", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.2, 3.2, 3.4; Appendix A (prioritized choice).
  #[test]
  fn prioritized_choice_locks() {
    // ACCEPT [1]
    validate_cbor_from_slice("a = [(int // int, tstr)]", &[0x81, 0x01], None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn range_1_2() {
    // REJECT []
    validate_cbor_from_slice("a = [1*2 (int, tstr)]", &[0x80], None).unwrap_err();
    // REJECT [1, "x", 2, "y", 3, "z"]
    validate_cbor_from_slice(
      "a = [1*2 (int, tstr)]",
      &[0x86, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79, 0x03, 0x61, 0x7a],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.4, 3.5.1.
  #[test]
  fn record() {
    // ACCEPT [1, 2, 3]
    validate_cbor_from_slice(
      "a = [x: int, y: int, z: int]",
      &[0x83, 0x01, 0x02, 0x03],
      None,
    )
    .unwrap();
    // REJECT [1, 2]
    validate_cbor_from_slice("a = [x: int, y: int, z: int]", &[0x82, 0x01, 0x02], None)
      .unwrap_err();
    // REJECT [1, 2, 3, 4]
    validate_cbor_from_slice(
      "a = [x: int, y: int, z: int]",
      &[0x84, 0x01, 0x02, 0x03, 0x04],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.4, 3.5.1.
  #[test]
  fn record_mixed() {
    // ACCEPT ["h", 1]
    validate_cbor_from_slice("a = [x: tstr, y: int]", &[0x82, 0x61, 0x68, 0x01], None).unwrap();
    // REJECT [1, "h"]
    validate_cbor_from_slice("a = [x: tstr, y: int]", &[0x82, 0x01, 0x61, 0x68], None).unwrap_err();
    // REJECT ["h"]
    validate_cbor_from_slice("a = [x: tstr, y: int]", &[0x81, 0x61, 0x68], None).unwrap_err();
    // REJECT ["h", 1, 2]
    validate_cbor_from_slice(
      "a = [x: tstr, y: int]",
      &[0x83, 0x61, 0x68, 0x01, 0x02],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_after() {
    // REJECT [1, "x"]
    validate_cbor_from_slice("a = [(int, tstr), bool]", &[0x82, 0x01, 0x61, 0x78], None)
      .unwrap_err();
    // REJECT [1, true]
    validate_cbor_from_slice("a = [(int, tstr), bool]", &[0x82, 0x01, 0xf5], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_before_no_occur() {
    // REJECT [true, 1]
    validate_cbor_from_slice("a = [bool, (int, tstr)]", &[0x82, 0xf5, 0x01], None).unwrap_err();
    // REJECT [true, "x", 1]
    validate_cbor_from_slice(
      "a = [bool, (int, tstr)]",
      &[0x83, 0xf5, 0x61, 0x78, 0x01],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_before_occur() {
    // REJECT [true]
    validate_cbor_from_slice("a = [bool, 1*1 (int, tstr)]", &[0x81, 0xf5], None).unwrap_err();
    // REJECT [true, 1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [bool, 1*1 (int, tstr)]",
      &[0x85, 0xf5, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.8, 3.8.1.
  #[test]
  fn size_control_elem() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* tstr .size 2]", &[0x80], None).unwrap();
    // ACCEPT ["ok"]
    validate_cbor_from_slice("a = [* tstr .size 2]", &[0x81, 0x62, 0x6f, 0x6b], None).unwrap();
    // REJECT ["x"]
    validate_cbor_from_slice("a = [* tstr .size 2]", &[0x81, 0x61, 0x78], None).unwrap_err();
    // REJECT ["abc"]
    validate_cbor_from_slice(
      "a = [* tstr .size 2]",
      &[0x81, 0x63, 0x61, 0x62, 0x63],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sole_group_no_occur_inline() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [(int, tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
    // REJECT [1]
    validate_cbor_from_slice("a = [(int, tstr)]", &[0x81, 0x01], None).unwrap_err();
    // REJECT [1, "x", 2]
    validate_cbor_from_slice("a = [(int, tstr)]", &[0x83, 0x01, 0x61, 0x78, 0x02], None)
      .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn sole_group_no_occur_ref() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [b]\nb = (int, tstr)", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
    // REJECT [1]
    validate_cbor_from_slice("a = [b]\nb = (int, tstr)", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_group_then_int() {
    // REJECT [1, "x"]
    validate_cbor_from_slice("a = [* (int, tstr), int]", &[0x82, 0x01, 0x61, 0x78], None)
      .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_of_var_arity() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* (int, * tstr)]", &[0x80], None).unwrap();
    // REJECT ["x"]
    validate_cbor_from_slice("a = [* (int, * tstr)]", &[0x81, 0x61, 0x78], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_then_tstr() {
    // REJECT [1]
    validate_cbor_from_slice("a = [* int, tstr]", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.3, 3.2, 3.4, 3.6 (#6.nnn(type) tag notation).
  #[test]
  fn tagged_elem() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* #6.42(tstr)]", &[0x80], None).unwrap();
    // ACCEPT [42("x")]
    validate_cbor_from_slice("a = [* #6.42(tstr)]", &[0x81, 0xd8, 0x2a, 0x61, 0x78], None).unwrap();
    // REJECT [43("x")]
    validate_cbor_from_slice("a = [* #6.42(tstr)]", &[0x81, 0xd8, 0x2b, 0x61, 0x78], None)
      .unwrap_err();
    // REJECT ["x"]
    validate_cbor_from_slice("a = [* #6.42(tstr)]", &[0x81, 0x61, 0x78], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.3, 3.2, 3.4, 3.6 (#6.nnn(type) tag notation).
  #[test]
  fn tagged_elem_in_group() {
    // ACCEPT [1, 42("x")]
    validate_cbor_from_slice(
      "a = [* (int, #6.42(tstr))]",
      &[0x82, 0x01, 0xd8, 0x2a, 0x61, 0x78],
      None,
    )
    .unwrap();
    // REJECT [1, "x"]
    validate_cbor_from_slice(
      "a = [* (int, #6.42(tstr))]",
      &[0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn three_level_nesting() {
    // REJECT [1, "x"]
    validate_cbor_from_slice(
      "a = [1*1 (int, (tstr, (bool)))]",
      &[0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn tuple_elements() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* [int, tstr]]", &[0x80], None).unwrap();
    // ACCEPT [[1, "x"]]
    validate_cbor_from_slice("a = [* [int, tstr]]", &[0x81, 0x82, 0x01, 0x61, 0x78], None).unwrap();
    // ACCEPT [[1, "x"], [2, "y"]]
    validate_cbor_from_slice(
      "a = [* [int, tstr]]",
      &[0x82, 0x82, 0x01, 0x61, 0x78, 0x82, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap();
    // REJECT [[1]]
    validate_cbor_from_slice("a = [* [int, tstr]]", &[0x81, 0x81, 0x01], None).unwrap_err();
    // REJECT [[1, "x", 2]]
    validate_cbor_from_slice(
      "a = [* [int, tstr]]",
      &[0x81, 0x83, 0x01, 0x61, 0x78, 0x02],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.2.2, 3.2, 3.4, 3.11.
  #[test]
  fn type_choice_element() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* (int / tstr)]", &[0x80], None).unwrap();
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [* (int / tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
    // REJECT [true]
    validate_cbor_from_slice("a = [* (int / tstr)]", &[0x81, 0xf5], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.1, 3.2, 3.4.
  #[test]
  fn type_choice_of_arrays_elem() {
    // ACCEPT [[1]]
    validate_cbor_from_slice("a = [* ([int] / [tstr])]", &[0x81, 0x81, 0x01], None).unwrap();
    // REJECT [[true]]
    validate_cbor_from_slice("a = [* ([int] / [tstr])]", &[0x81, 0x81, 0xf5], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_after_sibling() {
    // REJECT [true, 1]
    validate_cbor_from_slice("a = [bool, ~b]\nb = [int, tstr]", &[0x82, 0xf5, 0x01], None)
      .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4 (member keys are annotation-only in an
  // array context), 3.7.
  #[test]
  fn unwrap_labeled() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice(
      "a = [x: ~b]\nb = [int, tstr]",
      &[0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
    // REJECT [1]
    validate_cbor_from_slice("a = [x: ~b]\nb = [int, tstr]", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_sole() {
    // REJECT [1]
    validate_cbor_from_slice("a = [~b]\nb = [int, tstr]", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_with_occur() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* ~b]\nb = [int, tstr]", &[0x80], None).unwrap();
    // REJECT [1]
    validate_cbor_from_slice("a = [* ~b]\nb = [int, tstr]", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn upper_only_group() {
    // ACCEPT []
    validate_cbor_from_slice("a = [*2 (int, tstr)]", &[0x80], None).unwrap();
    // REJECT [1, "x", 2, "y", 3, "z"]
    validate_cbor_from_slice(
      "a = [*2 (int, tstr)]",
      &[0x86, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79, 0x03, 0x61, 0x7a],
      None,
    )
    .unwrap_err();
    // REJECT [1]
    validate_cbor_from_slice("a = [*2 (int, tstr)]", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn upper_only_homogeneous() {
    // ACCEPT []
    validate_cbor_from_slice("a = [*2 int]", &[0x80], None).unwrap();
    // ACCEPT [1, 2]
    validate_cbor_from_slice("a = [*2 int]", &[0x82, 0x01, 0x02], None).unwrap();
    // REJECT [1, 2, 3]
    validate_cbor_from_slice("a = [*2 int]", &[0x83, 0x01, 0x02, 0x03], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn upstream_ignored_case() {
    // ACCEPT [1, 2, 3]
    validate_cbor_from_slice("a = [int, (int, int)]", &[0x83, 0x01, 0x02, 0x03], None).unwrap();
    // REJECT [1, 2]
    validate_cbor_from_slice("a = [int, (int, int)]", &[0x82, 0x01, 0x02], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn var_arity_inside() {
    // REJECT []
    validate_cbor_from_slice("a = [1*1 (int, * tstr)]", &[0x80], None).unwrap_err();
    // REJECT ["x"]
    validate_cbor_from_slice("a = [1*1 (int, * tstr)]", &[0x81, 0x61, 0x78], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn zero_or_more() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* (int, tstr)]", &[0x80], None).unwrap();
    // REJECT [1]
    validate_cbor_from_slice("a = [* (int, tstr)]", &[0x81, 0x01], None).unwrap_err();
    // REJECT [1, "x", 2]
    validate_cbor_from_slice("a = [* (int, tstr)]", &[0x83, 0x01, 0x61, 0x78, 0x02], None)
      .unwrap_err();
    // REJECT ["x", 1]
    validate_cbor_from_slice("a = [* (int, tstr)]", &[0x82, 0x61, 0x78, 0x01], None).unwrap_err();
  }
}

mod post_fix {
  use super::*;

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7, 3.10.
  #[test]
  fn generic_group_choice_no_arg_leak() {
    // Generic args registered while a failing choice alternative was tried
    // speculatively must not leak into the next alternative.
    // ACCEPT ["x", "x"] (Ruby-verified)
    validate_cbor_from_slice(
      "a = [(g<int> // g<tstr>)]\ng<T> = (T, T)",
      &[0x82, 0x61, 0x78, 0x61, 0x78],
      None,
    )
    .unwrap();
    // ACCEPT [1, 1] (Ruby-verified)
    validate_cbor_from_slice(
      "a = [(g<int> // g<tstr>)]\ng<T> = (T, T)",
      &[0x82, 0x01, 0x01],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7, 3.10.
  #[test]
  fn unwrap_generic_choice_no_arg_leak() {
    // ACCEPT ["x"] (Ruby-verified)
    validate_cbor_from_slice(
      "a = [~box<int> // ~box<tstr>]\nbox<T> = [T]",
      &[0x81, 0x61, 0x78],
      None,
    )
    .unwrap();
    // ACCEPT [1] (Ruby-verified)
    validate_cbor_from_slice(
      "a = [~box<int> // ~box<tstr>]\nbox<T> = [T]",
      &[0x81, 0x01],
      None,
    )
    .unwrap();
    // REJECT [true] (Ruby-verified)
    validate_cbor_from_slice(
      "a = [~box<int> // ~box<tstr>]\nbox<T> = [T]",
      &[0x81, 0xf5],
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_recursive() {
    // Recursive unwrap references must terminate instead of overflowing the
    // stack; the rule is unsatisfiable.
    // REJECT [] (Ruby-verified)
    validate_cbor_from_slice("a = [~b]\nb = [~b]", &[0x80], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.8, 3.8.4.
  #[test]
  fn cbor_control_group_inside() {
    // ACCEPT [h'82016178']
    validate_cbor_from_slice(
      "a = [bytes .cbor b]\nb = [* (int, tstr)]",
      &[0x81, 0x44, 0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.2.2.2, 3.2, 3.4; Appendix B (& groupname).
  #[test]
  fn choice_from_named_group() {
    // ACCEPT [1, 2]
    validate_cbor_from_slice("a = [* e]\ne = &g\ng = (1, 2)", &[0x82, 0x01, 0x02], None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix A (greedy PEG occurrence must terminate
  // on zero-width repetitions); Appendix B (parenthesized group).
  #[test]
  fn empty_group_star() {
    // NOTE: Ruby gem infinite-loops on zero-width group repetition; expected per RFC: trailing int unmatched in []. Matcher must terminate.
    // REJECT []
    validate_cbor_from_slice("a = [* (), int]", &[0x80], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn exact_1_1() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [1*1 (int, tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn exact_2_2() {
    // ACCEPT [1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [2*2 (int, tstr)]",
      &[0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.10.
  #[test]
  fn generic_elem() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* box<int>]\nbox<T> = [T]", &[0x80], None).unwrap();
    // ACCEPT [[1]]
    validate_cbor_from_slice("a = [* box<int>]\nbox<T> = [T]", &[0x81, 0x81, 0x01], None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.10; Appendix B (groupname entry).
  #[test]
  fn generic_group_elem() {
    // ACCEPT [] (Ruby-verified)
    validate_cbor_from_slice("a = [* g<int>]\ng<T> = (T, T)", &[0x80], None).unwrap();
    // ACCEPT [1, 2] (Ruby-verified)
    validate_cbor_from_slice("a = [* g<int>]\ng<T> = (T, T)", &[0x82, 0x01, 0x02], None).unwrap();
    // ACCEPT [1, 2, 3, 4] (Ruby-verified)
    validate_cbor_from_slice(
      "a = [* g<int>]\ng<T> = (T, T)",
      &[0x84, 0x01, 0x02, 0x03, 0x04],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix A (greedy PEG occurrence behavior).
  #[test]
  fn greedy_star_then_int() {
    // REJECT [1]
    validate_cbor_from_slice("a = [* int, int]", &[0x81, 0x01], None).unwrap_err();
    // REJECT [1, 2]
    validate_cbor_from_slice("a = [* int, int]", &[0x82, 0x01, 0x02], None).unwrap_err();
    // REJECT []
    validate_cbor_from_slice("a = [* int, int]", &[0x80], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn group_between_siblings() {
    // ACCEPT [true, false]
    validate_cbor_from_slice("a = [bool, * (int, tstr), bool]", &[0x82, 0xf5, 0xf4], None).unwrap();
    // ACCEPT [true, 1, "x", false]
    validate_cbor_from_slice(
      "a = [bool, * (int, tstr), bool]",
      &[0x84, 0xf5, 0x01, 0x61, 0x78, 0xf4],
      None,
    )
    .unwrap();
    // ACCEPT [true, 1, "x", 2, "y", false]
    validate_cbor_from_slice(
      "a = [bool, * (int, tstr), bool]",
      &[0x86, 0xf5, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79, 0xf4],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 2.2.2, 3.2, 3.4.
  #[test]
  fn group_choice_inside() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice(
      "a = [1*1 (int, tstr // tstr, int)]",
      &[0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
    // ACCEPT ["x", 1]
    validate_cbor_from_slice(
      "a = [1*1 (int, tstr // tstr, int)]",
      &[0x82, 0x61, 0x78, 0x01],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_after_sibling() {
    // ACCEPT ["h"]
    validate_cbor_from_slice(
      "a = [tstr, * pair]\npair = (int, tstr)",
      &[0x81, 0x61, 0x68],
      None,
    )
    .unwrap();
    // ACCEPT ["h", 1, "x"]
    validate_cbor_from_slice(
      "a = [tstr, * pair]\npair = (int, tstr)",
      &[0x83, 0x61, 0x68, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_exact() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice(
      "a = [1*1 b]\nb = (int, tstr)",
      &[0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_star() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice(
      "a = [* pair]\npair = (int, tstr)",
      &[0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
    // ACCEPT [1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [* pair]\npair = (int, tstr)",
      &[0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn lower_bound_only() {
    // ACCEPT [1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [2* (int, tstr)]",
      &[0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap();
    // ACCEPT [1, "x", 2, "y", 3, "z"]
    validate_cbor_from_slice(
      "a = [2* (int, tstr)]",
      &[0x86, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79, 0x03, 0x61, 0x7a],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.2.2, 3.2, 3.4; Appendix B (choice extension).
  #[test]
  fn named_type_choice_elem() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* elem]\nelem = int\nelem /= tstr", &[0x80], None).unwrap();
    // ACCEPT [1, "x"]
    validate_cbor_from_slice(
      "a = [* elem]\nelem = int\nelem /= tstr",
      &[0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn nested_parens() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [1*1 (int, (tstr))]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn nested_star_of_star() {
    // ACCEPT [[1, "x"]]
    validate_cbor_from_slice(
      "a = [* [* (int, tstr)]]",
      &[0x81, 0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
    // ACCEPT [[1, "x", 2, "y"], []]
    validate_cbor_from_slice(
      "a = [* [* (int, tstr)]]",
      &[0x82, 0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79, 0x80],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn occur_inside_occur() {
    // ACCEPT []
    validate_cbor_from_slice("a = [* (int, 2*2 tstr)]", &[0x80], None).unwrap();
    // ACCEPT [1, "x", "y"]
    validate_cbor_from_slice(
      "a = [* (int, 2*2 tstr)]",
      &[0x83, 0x01, 0x61, 0x78, 0x61, 0x79],
      None,
    )
    .unwrap();
    // ACCEPT [1, "x", "y", 2, "p", "q"]
    validate_cbor_from_slice(
      "a = [* (int, 2*2 tstr)]",
      &[
        0x86, 0x01, 0x61, 0x78, 0x61, 0x79, 0x02, 0x61, 0x70, 0x61, 0x71,
      ],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn occur_on_single_entry_group() {
    // ACCEPT [1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [2*2 (int, 1*1 (tstr))]",
      &[0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn one_or_more() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [+ (int, tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
    // ACCEPT [1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [+ (int, tstr)]",
      &[0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn optional() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [? (int, tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
    // REJECT [1]
    validate_cbor_from_slice("a = [? (int, tstr)]", &[0x81, 0x01], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn optional_middle() {
    // ACCEPT [1, true]
    validate_cbor_from_slice("a = [int, ? tstr, bool]", &[0x82, 0x01, 0xf5], None).unwrap();
    // ACCEPT [1, "x", true]
    validate_cbor_from_slice(
      "a = [int, ? tstr, bool]",
      &[0x83, 0x01, 0x61, 0x78, 0xf5],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn prefix_then_star() {
    // ACCEPT ["h"]
    validate_cbor_from_slice("a = [tstr, * int]", &[0x81, 0x61, 0x68], None).unwrap();
    // ACCEPT ["h", 1, 2]
    validate_cbor_from_slice("a = [tstr, * int]", &[0x83, 0x61, 0x68, 0x01, 0x02], None).unwrap();
  }

  // RFC 8610: Sections 2.1, 2.2.2, 3.2, 3.4; Appendix A (prioritized choice).
  #[test]
  fn prioritized_choice_locks() {
    // REJECT [1, "x"]
    validate_cbor_from_slice("a = [(int // int, tstr)]", &[0x82, 0x01, 0x61, 0x78], None)
      .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.1, 3.2, 3.4. Errors recorded while trying a
  // failing type-choice alternative must not poison a successful one,
  // regardless of the order the alternatives appear in.
  #[test]
  fn type_choice_alternative_order() {
    // ACCEPT [["x"]]
    validate_cbor_from_slice("a = [* ([int] / [tstr])]", &[0x81, 0x81, 0x61, 0x78], None).unwrap();
    // ACCEPT [[1]]
    validate_cbor_from_slice("a = [* ([tstr] / [int])]", &[0x81, 0x81, 0x01], None).unwrap();
  }

  // RFC 8610: Section 2.2.1. A type choice must reject an array when no
  // alternative matches it, including alternatives that are not array-shaped
  // (a non-array alternative failing against an array must count as a
  // failure, not be skipped silently).
  #[test]
  fn type_choice_non_array_alternates() {
    // REJECT [1]
    validate_cbor_from_slice("a = tstr / bool", &[0x81, 0x01], None).unwrap_err();
    // REJECT [1, "x"]
    validate_cbor_from_slice("a = [int, int] / tstr", &[0x82, 0x01, 0x61, 0x78], None).unwrap_err();
    // REJECT [[1]]
    validate_cbor_from_slice("a = [* (tstr / bool)]", &[0x81, 0x81, 0x01], None).unwrap_err();
    // ACCEPT "hi"
    validate_cbor_from_slice("a = [int, int] / tstr", &[0x62, 0x68, 0x69], None).unwrap();
    // ACCEPT [1]
    validate_cbor_from_slice("a = tstr / [int]", &[0x81, 0x01], None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn range_1_2() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [1*2 (int, tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
    // ACCEPT [1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [1*2 (int, tstr)]",
      &[0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_after() {
    // ACCEPT [1, "x", true]
    validate_cbor_from_slice(
      "a = [(int, tstr), bool]",
      &[0x83, 0x01, 0x61, 0x78, 0xf5],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_before_no_occur() {
    // ACCEPT [true, 1, "x"]
    validate_cbor_from_slice(
      "a = [bool, (int, tstr)]",
      &[0x83, 0xf5, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_before_occur() {
    // ACCEPT [true, 1, "x"]
    validate_cbor_from_slice(
      "a = [bool, 1*1 (int, tstr)]",
      &[0x83, 0xf5, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_group_then_int() {
    // ACCEPT [5]
    validate_cbor_from_slice("a = [* (int, tstr), int]", &[0x81, 0x05], None).unwrap();
    // ACCEPT [1, "x", 5]
    validate_cbor_from_slice(
      "a = [* (int, tstr), int]",
      &[0x83, 0x01, 0x61, 0x78, 0x05],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_of_var_arity() {
    // ACCEPT [1]
    validate_cbor_from_slice("a = [* (int, * tstr)]", &[0x81, 0x01], None).unwrap();
    // ACCEPT [1, "x", 2]
    validate_cbor_from_slice(
      "a = [* (int, * tstr)]",
      &[0x83, 0x01, 0x61, 0x78, 0x02],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_then_tstr() {
    // ACCEPT ["x"]
    validate_cbor_from_slice("a = [* int, tstr]", &[0x81, 0x61, 0x78], None).unwrap();
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [* int, tstr]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
    // ACCEPT [1, 2, "x"]
    validate_cbor_from_slice("a = [* int, tstr]", &[0x83, 0x01, 0x02, 0x61, 0x78], None).unwrap();
    // REJECT []
    validate_cbor_from_slice("a = [* int, tstr]", &[0x80], None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn three_level_nesting() {
    // ACCEPT [1, "x", true]
    validate_cbor_from_slice(
      "a = [1*1 (int, (tstr, (bool)))]",
      &[0x83, 0x01, 0x61, 0x78, 0xf5],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_after_sibling() {
    // ACCEPT [true, 1, "x"]
    validate_cbor_from_slice(
      "a = [bool, ~b]\nb = [int, tstr]",
      &[0x83, 0xf5, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_sole() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [~b]\nb = [int, tstr]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_with_occur() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice(
      "a = [* ~b]\nb = [int, tstr]",
      &[0x82, 0x01, 0x61, 0x78],
      None,
    )
    .unwrap();
    // ACCEPT [1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [* ~b]\nb = [int, tstr]",
      &[0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn upper_only_group() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [*2 (int, tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
    // ACCEPT [1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [*2 (int, tstr)]",
      &[0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn var_arity_inside() {
    // ACCEPT [1]
    validate_cbor_from_slice("a = [1*1 (int, * tstr)]", &[0x81, 0x01], None).unwrap();
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [1*1 (int, * tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
    // ACCEPT [1, "x", "y"]
    validate_cbor_from_slice(
      "a = [1*1 (int, * tstr)]",
      &[0x83, 0x01, 0x61, 0x78, 0x61, 0x79],
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn zero_or_more() {
    // ACCEPT [1, "x"]
    validate_cbor_from_slice("a = [* (int, tstr)]", &[0x82, 0x01, 0x61, 0x78], None).unwrap();
    // ACCEPT [1, "x", 2, "y"]
    validate_cbor_from_slice(
      "a = [* (int, tstr)]",
      &[0x84, 0x01, 0x61, 0x78, 0x02, 0x61, 0x79],
      None,
    )
    .unwrap();
  }
}
