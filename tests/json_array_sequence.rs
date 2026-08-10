//! Array group/occurrence sequence-matching tests (JSON validator).
//!
//! Port of tests/cbor_array_sequence.rs to the JSON validator: same vector
//! set minus CBOR-only shapes (tags, byte strings, .cbor). Vectors were
//! cross-checked against the Ruby reference implementation (cddl gem
//! 0.12.14)
//!
//! - `regression`: behavior that was already correct before the sequence
//!   matcher landed and must not change.
//! - `post_fix`: spec-correct behavior (confirmed by the Ruby reference
//!   implementation) that the old positional array validator got wrong.
//!   Fixed by the array sequence matcher.
//!
//! A vector's regression/post_fix placement follows the JSON validator's own
//! pre-matcher behavior, which differed from the CBOR validator's on a few
//! vectors (choice_from_group's `[1, 2, 1]`, nested_array's `[1]`), so their
//! placement differs between the two files. When syncing the files, keep
//! each file's own placement rather than mirroring the other's.
//!
//! The normative basis (RFC 8610 Appendix A PEG semantics + Appendix C
//! matching rules, and the known tension between them) is documented in the
//! header of tests/cbor_array_sequence.rs.
#![cfg(feature = "std")]
#![cfg(feature = "json")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::validate_json_from_str;

mod regression {
  use super::*;

  // RFC 8610: Sections 2.1, 2.2.2, 3.2, 3.4, 3.11.
  #[test]
  fn choice_diff_lengths() {
    validate_json_from_str("a = [(bool // int, tstr)]", r#"[true]"#, None).unwrap();
    validate_json_from_str("a = [(bool // int, tstr)]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [(bool // int, tstr)]", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.2.2.2, 3.2, 3.4; Appendix B (& group choice).
  #[test]
  fn choice_from_group() {
    validate_json_from_str("a = [* &(1, 2)]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* &(1, 2)]", r#"[3]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.2.2.2, 3.2, 3.4; Appendix B (& groupname).
  #[test]
  fn choice_from_named_group() {
    validate_json_from_str("a = [* e]\ne = &g\ng = (1, 2)", r#"[3]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.4.
  #[test]
  fn empty_array() {
    validate_json_from_str("a = []", r#"[]"#, None).unwrap();
    validate_json_from_str("a = []", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix A (greedy PEG occurrence must terminate
  // on zero-width repetitions); Appendix B (parenthesized group).
  #[test]
  fn empty_group_star() {
    // NOTE: Ruby gem infinite-loops on zero-width group repetition; expected per RFC: () matches trivially, then int matches. Matcher must terminate.
    validate_json_from_str("a = [* (), int]", r#"[1]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn exact_1_1() {
    validate_json_from_str("a = [1*1 (int, tstr)]", r#"[]"#, None).unwrap_err();
    validate_json_from_str("a = [1*1 (int, tstr)]", r#"[1]"#, None).unwrap_err();
    validate_json_from_str("a = [1*1 (int, tstr)]", r#"[1, "x", 2, "y"]"#, None).unwrap_err();
    validate_json_from_str("a = [1*1 (int, tstr)]", r#"["x", 1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn exact_2_2() {
    validate_json_from_str("a = [2*2 (int, tstr)]", r#"[1, "x"]"#, None).unwrap_err();
    validate_json_from_str("a = [2*2 (int, tstr)]", r#"[1, "x", 2, "y", 3, "z"]"#, None)
      .unwrap_err();
    validate_json_from_str("a = [2*2 (int, tstr)]", r#"[1, "x", 2]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.10.
  #[test]
  fn generic_elem() {
    validate_json_from_str("a = [* box<int>]\nbox<T> = [T]", r#"[["x"]]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.10; Appendix B (groupname entry).
  #[test]
  fn generic_group_elem() {
    // REJECT [1] (Ruby-verified)
    validate_json_from_str("a = [* g<int>]\ng<T> = (T, T)", r#"[1]"#, None).unwrap_err();
    // REJECT [1, "x"] (Ruby-verified)
    validate_json_from_str("a = [* g<int>]\ng<T> = (T, T)", r#"[1, "x"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn group_between_siblings() {
    validate_json_from_str(
      "a = [bool, * (int, tstr), bool]",
      r#"[true, 1, false]"#,
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.2, 3.2, 3.4.
  #[test]
  fn group_choice_inside() {
    validate_json_from_str("a = [1*1 (int, tstr // tstr, int)]", r#"[1, 2]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_after_sibling() {
    validate_json_from_str(
      "a = [tstr, * pair]\npair = (int, tstr)",
      r#"["h", 1]"#,
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_exact() {
    validate_json_from_str("a = [1*1 b]\nb = (int, tstr)", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_star() {
    validate_json_from_str("a = [* pair]\npair = (int, tstr)", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* pair]\npair = (int, tstr)", r#"[1, "x", 2]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_alias() {
    validate_json_from_str("a = [* zip]\nzip = int", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* zip]\nzip = int", r#"[1, 2]"#, None).unwrap();
    validate_json_from_str("a = [* zip]\nzip = int", r#"["x"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_exact() {
    validate_json_from_str("a = [3*3 int]", r#"[1, 2, 3]"#, None).unwrap();
    validate_json_from_str("a = [3*3 int]", r#"[1, 2]"#, None).unwrap_err();
    validate_json_from_str("a = [3*3 int]", r#"[1, 2, 3, 4]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_lower() {
    validate_json_from_str("a = [3* int]", r#"[1, 2, 3]"#, None).unwrap();
    validate_json_from_str("a = [3* int]", r#"[1, 2, 3, 4]"#, None).unwrap();
    validate_json_from_str("a = [3* int]", r#"[1, 2]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_opt() {
    validate_json_from_str("a = [? int]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [? int]", r#"[1]"#, None).unwrap();
    validate_json_from_str("a = [? int]", r#"[1, 2]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_plus() {
    validate_json_from_str("a = [+ int]", r#"[1]"#, None).unwrap();
    validate_json_from_str("a = [+ int]", r#"[1, 2, 3]"#, None).unwrap();
    validate_json_from_str("a = [+ int]", r#"[]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_range() {
    validate_json_from_str("a = [1*3 int]", r#"[1]"#, None).unwrap();
    validate_json_from_str("a = [1*3 int]", r#"[1, 2, 3]"#, None).unwrap();
    validate_json_from_str("a = [1*3 int]", r#"[]"#, None).unwrap_err();
    validate_json_from_str("a = [1*3 int]", r#"[1, 2, 3, 4]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn homogeneous_star() {
    validate_json_from_str("a = [* int]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* int]", r#"[1, 2, 3]"#, None).unwrap();
    validate_json_from_str("a = [* int]", r#"[1, "x"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.1, 3.4.
  #[test]
  fn literal_elements() {
    validate_json_from_str("a = [1, 2, 3]", r#"[1, 2, 3]"#, None).unwrap();
    validate_json_from_str("a = [1, 2, 3]", r#"[1, 2]"#, None).unwrap_err();
    validate_json_from_str("a = [1, 2, 3]", r#"[1, 2, 4]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn lower_bound_only() {
    validate_json_from_str("a = [2* (int, tstr)]", r#"[1, "x"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.5, 3.5.1; Appendix B (groupname entry).
  #[test]
  fn map_group_ref() {
    validate_json_from_str(
      "a = {g}\ng = (x: int, y: tstr)",
      r#"{"x": 1, "y": "h"}"#,
      None,
    )
    .unwrap();
    validate_json_from_str("a = {g}\ng = (x: int, y: tstr)", r#"{"x": 1}"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.5.1.
  #[test]
  fn map_in_array() {
    validate_json_from_str("a = [* {k: int}]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* {k: int}]", r#"[{"k": 1}]"#, None).unwrap();
    validate_json_from_str("a = [* {k: int}]", r#"[{"k": "x"}]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.5.1; Appendix B (groupname entry).
  #[test]
  fn map_in_array_with_group() {
    validate_json_from_str("a = [* {g}]\ng = (x: int)", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* {g}]\ng = (x: int)", r#"[{"x": 1}]"#, None).unwrap();
    validate_json_from_str("a = [* {g}]\ng = (x: int)", r#"[{"x": "h"}]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.5.1; Appendix B (parenthesized group).
  #[test]
  fn map_inline_group() {
    validate_json_from_str("a = {(x: int, y: tstr)}", r#"{"x": 1, "y": "h"}"#, None).unwrap();
    validate_json_from_str("a = {(x: int, y: tstr)}", r#"{"x": 1}"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.5.1.
  #[test]
  fn map_optional_member() {
    validate_json_from_str("a = {x: int, ? y: tstr}", r#"{"x": 1}"#, None).unwrap();
    validate_json_from_str("a = {x: int, ? y: tstr}", r#"{"x": 1, "y": "h"}"#, None).unwrap();
    validate_json_from_str("a = {x: int, ? y: tstr}", r#"{}"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.5.1.
  #[test]
  fn map_record() {
    validate_json_from_str("a = {x: int, y: tstr}", r#"{"x": 1, "y": "h"}"#, None).unwrap();
    validate_json_from_str("a = {x: int, y: tstr}", r#"{"x": 1}"#, None).unwrap_err();
    validate_json_from_str("a = {x: int, y: tstr}", r#"{"x": "h", "y": "h"}"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.5.2.
  #[test]
  fn map_star_members() {
    validate_json_from_str("a = {* tstr => int}", r#"{}"#, None).unwrap();
    validate_json_from_str("a = {* tstr => int}", r#"{"a": 1, "b": 2}"#, None).unwrap();
    validate_json_from_str("a = {* tstr => int}", r#"{"a": "x"}"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.2.2, 3.2, 3.4; Appendix B (choice extension).
  #[test]
  fn named_type_choice_elem() {
    validate_json_from_str("a = [* elem]\nelem = int\nelem /= tstr", r#"[true]"#, None)
      .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn nested_array() {
    validate_json_from_str("a = [* [int]]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* [int]]", r#"[[1], [2]]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.4.
  #[test]
  fn nested_array_literal() {
    validate_json_from_str(
      "a = [int, [int, int], [int, int]]",
      r#"[1, [2, 3], [4, 5]]"#,
      None,
    )
    .unwrap();
    validate_json_from_str("a = [int, [int, int], [int, int]]", r#"[1, [2, 3]]"#, None)
      .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn nested_parens() {
    validate_json_from_str("a = [1*1 (int, (tstr))]", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn nested_star_of_star() {
    validate_json_from_str("a = [* [* (int, tstr)]]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* [* (int, tstr)]]", r#"[[]]"#, None).unwrap();
    validate_json_from_str("a = [* [* (int, tstr)]]", r#"[[1]]"#, None).unwrap_err();
    validate_json_from_str("a = [* [* (int, tstr)]]", r#"[1, "x"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn occur_inside_occur() {
    validate_json_from_str("a = [* (int, 2*2 tstr)]", r#"[1, "x"]"#, None).unwrap_err();
    validate_json_from_str("a = [* (int, 2*2 tstr)]", r#"[1, "x", "y", "z"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn occur_on_single_entry_group() {
    validate_json_from_str("a = [2*2 (int, 1*1 (tstr))]", r#"[1, "x", 2]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn one_or_more() {
    validate_json_from_str("a = [+ (int, tstr)]", r#"[]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn optional() {
    validate_json_from_str("a = [? (int, tstr)]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [? (int, tstr)]", r#"[1, "x", 2, "y"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn optional_middle() {
    validate_json_from_str("a = [int, ? tstr, bool]", r#"[1, "x"]"#, None).unwrap_err();
    validate_json_from_str("a = [int, ? tstr, bool]", r#"[1, "x", true, true]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn prefix_then_star() {
    validate_json_from_str("a = [tstr, * int]", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.2, 3.2, 3.4; Appendix A (prioritized choice).
  #[test]
  fn prioritized_choice_locks() {
    validate_json_from_str("a = [(int // int, tstr)]", r#"[1]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn range_1_2() {
    validate_json_from_str("a = [1*2 (int, tstr)]", r#"[]"#, None).unwrap_err();
    validate_json_from_str("a = [1*2 (int, tstr)]", r#"[1, "x", 2, "y", 3, "z"]"#, None)
      .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.4, 3.5.1.
  #[test]
  fn record() {
    validate_json_from_str("a = [x: int, y: int, z: int]", r#"[1, 2, 3]"#, None).unwrap();
    validate_json_from_str("a = [x: int, y: int, z: int]", r#"[1, 2]"#, None).unwrap_err();
    validate_json_from_str("a = [x: int, y: int, z: int]", r#"[1, 2, 3, 4]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.4, 3.5.1.
  #[test]
  fn record_mixed() {
    validate_json_from_str("a = [x: tstr, y: int]", r#"["h", 1]"#, None).unwrap();
    validate_json_from_str("a = [x: tstr, y: int]", r#"[1, "h"]"#, None).unwrap_err();
    validate_json_from_str("a = [x: tstr, y: int]", r#"["h"]"#, None).unwrap_err();
    validate_json_from_str("a = [x: tstr, y: int]", r#"["h", 1, 2]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_after() {
    validate_json_from_str("a = [(int, tstr), bool]", r#"[1, "x"]"#, None).unwrap_err();
    validate_json_from_str("a = [(int, tstr), bool]", r#"[1, true]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_before_no_occur() {
    validate_json_from_str("a = [bool, (int, tstr)]", r#"[true, 1]"#, None).unwrap_err();
    validate_json_from_str("a = [bool, (int, tstr)]", r#"[true, "x", 1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_before_occur() {
    validate_json_from_str("a = [bool, 1*1 (int, tstr)]", r#"[true]"#, None).unwrap_err();
    validate_json_from_str(
      "a = [bool, 1*1 (int, tstr)]",
      r#"[true, 1, "x", 2, "y"]"#,
      None,
    )
    .unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.8, 3.8.1.
  #[test]
  fn size_control_elem() {
    validate_json_from_str("a = [* tstr .size 2]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* tstr .size 2]", r#"["ok"]"#, None).unwrap();
    validate_json_from_str("a = [* tstr .size 2]", r#"["x"]"#, None).unwrap_err();
    validate_json_from_str("a = [* tstr .size 2]", r#"["abc"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sole_group_no_occur_inline() {
    validate_json_from_str("a = [(int, tstr)]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [(int, tstr)]", r#"[1]"#, None).unwrap_err();
    validate_json_from_str("a = [(int, tstr)]", r#"[1, "x", 2]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn sole_group_no_occur_ref() {
    validate_json_from_str("a = [b]\nb = (int, tstr)", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [b]\nb = (int, tstr)", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_group_then_int() {
    validate_json_from_str("a = [* (int, tstr), int]", r#"[1, "x"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_of_var_arity() {
    validate_json_from_str("a = [* (int, * tstr)]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* (int, * tstr)]", r#"["x"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_then_tstr() {
    validate_json_from_str("a = [* int, tstr]", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn three_level_nesting() {
    validate_json_from_str("a = [1*1 (int, (tstr, (bool)))]", r#"[1, "x"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn tuple_elements() {
    validate_json_from_str("a = [* [int, tstr]]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* [int, tstr]]", r#"[[1, "x"]]"#, None).unwrap();
    validate_json_from_str("a = [* [int, tstr]]", r#"[[1, "x"], [2, "y"]]"#, None).unwrap();
    validate_json_from_str("a = [* [int, tstr]]", r#"[[1]]"#, None).unwrap_err();
    validate_json_from_str("a = [* [int, tstr]]", r#"[[1, "x", 2]]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.2.2, 3.2, 3.4, 3.11.
  #[test]
  fn type_choice_element() {
    validate_json_from_str("a = [* (int / tstr)]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* (int / tstr)]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [* (int / tstr)]", r#"[true]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.1, 3.2, 3.4.
  #[test]
  fn type_choice_of_arrays_elem() {
    validate_json_from_str("a = [* ([int] / [tstr])]", r#"[[1]]"#, None).unwrap();
    validate_json_from_str("a = [* ([int] / [tstr])]", r#"[[true]]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_after_sibling() {
    validate_json_from_str("a = [bool, ~b]\nb = [int, tstr]", r#"[true, 1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4 (member keys are annotation-only in an
  // array context), 3.7.
  #[test]
  fn unwrap_labeled() {
    validate_json_from_str("a = [x: ~b]\nb = [int, tstr]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [x: ~b]\nb = [int, tstr]", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_sole() {
    validate_json_from_str("a = [~b]\nb = [int, tstr]", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_with_occur() {
    validate_json_from_str("a = [* ~b]\nb = [int, tstr]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* ~b]\nb = [int, tstr]", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn upper_only_group() {
    validate_json_from_str("a = [*2 (int, tstr)]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [*2 (int, tstr)]", r#"[1, "x", 2, "y", 3, "z"]"#, None)
      .unwrap_err();
    validate_json_from_str("a = [*2 (int, tstr)]", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn upper_only_homogeneous() {
    validate_json_from_str("a = [*2 int]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [*2 int]", r#"[1, 2]"#, None).unwrap();
    validate_json_from_str("a = [*2 int]", r#"[1, 2, 3]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn upstream_ignored_case() {
    validate_json_from_str("a = [int, (int, int)]", r#"[1, 2, 3]"#, None).unwrap();
    validate_json_from_str("a = [int, (int, int)]", r#"[1, 2]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn var_arity_inside() {
    validate_json_from_str("a = [1*1 (int, * tstr)]", r#"[]"#, None).unwrap_err();
    validate_json_from_str("a = [1*1 (int, * tstr)]", r#"["x"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn zero_or_more() {
    validate_json_from_str("a = [* (int, tstr)]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* (int, tstr)]", r#"[1]"#, None).unwrap_err();
    validate_json_from_str("a = [* (int, tstr)]", r#"[1, "x", 2]"#, None).unwrap_err();
    validate_json_from_str("a = [* (int, tstr)]", r#"["x", 1]"#, None).unwrap_err();
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
    validate_json_from_str(
      "a = [(g<int> // g<tstr>)]\ng<T> = (T, T)",
      r#"["x", "x"]"#,
      None,
    )
    .unwrap();
    // ACCEPT [1, 1] (Ruby-verified)
    validate_json_from_str(
      "a = [(g<int> // g<tstr>)]\ng<T> = (T, T)",
      r#"[1, 1]"#,
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7, 3.10.
  #[test]
  fn unwrap_generic_choice_no_arg_leak() {
    // ACCEPT ["x"] (Ruby-verified)
    validate_json_from_str(
      "a = [~box<int> // ~box<tstr>]\nbox<T> = [T]",
      r#"["x"]"#,
      None,
    )
    .unwrap();
    // ACCEPT [1] (Ruby-verified)
    validate_json_from_str(
      "a = [~box<int> // ~box<tstr>]\nbox<T> = [T]",
      r#"[1]"#,
      None,
    )
    .unwrap();
    // REJECT [true] (Ruby-verified)
    validate_json_from_str(
      "a = [~box<int> // ~box<tstr>]\nbox<T> = [T]",
      r#"[true]"#,
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
    validate_json_from_str("a = [~b]\nb = [~b]", r#"[]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.2.2.2, 3.2, 3.4; Appendix B (& group choice).
  #[test]
  fn choice_from_group() {
    // ACCEPT [1, 2, 1] (Ruby-verified)
    validate_json_from_str("a = [* &(1, 2)]", r#"[1, 2, 1]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn nested_array() {
    // REJECT [1] (Ruby-verified)
    validate_json_from_str("a = [* [int]]", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.2.2.2, 3.2, 3.4; Appendix B (& groupname).
  #[test]
  fn choice_from_named_group() {
    validate_json_from_str("a = [* e]\ne = &g\ng = (1, 2)", r#"[1, 2]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix A (greedy PEG occurrence must terminate
  // on zero-width repetitions); Appendix B (parenthesized group).
  #[test]
  fn empty_group_star() {
    // NOTE: Ruby gem infinite-loops on zero-width group repetition; expected per RFC: trailing int unmatched in []. Matcher must terminate.
    validate_json_from_str("a = [* (), int]", r#"[]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn exact_1_1() {
    validate_json_from_str("a = [1*1 (int, tstr)]", r#"[1, "x"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn exact_2_2() {
    validate_json_from_str("a = [2*2 (int, tstr)]", r#"[1, "x", 2, "y"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.10.
  #[test]
  fn generic_elem() {
    validate_json_from_str("a = [* box<int>]\nbox<T> = [T]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* box<int>]\nbox<T> = [T]", r#"[[1]]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.10; Appendix B (groupname entry).
  #[test]
  fn generic_group_elem() {
    // ACCEPT [] (Ruby-verified)
    validate_json_from_str("a = [* g<int>]\ng<T> = (T, T)", r#"[]"#, None).unwrap();
    // ACCEPT [1, 2] (Ruby-verified)
    validate_json_from_str("a = [* g<int>]\ng<T> = (T, T)", r#"[1, 2]"#, None).unwrap();
    // ACCEPT [1, 2, 3, 4] (Ruby-verified)
    validate_json_from_str("a = [* g<int>]\ng<T> = (T, T)", r#"[1, 2, 3, 4]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix A (greedy PEG occurrence behavior).
  #[test]
  fn greedy_star_then_int() {
    validate_json_from_str("a = [* int, int]", r#"[1]"#, None).unwrap_err();
    validate_json_from_str("a = [* int, int]", r#"[1, 2]"#, None).unwrap_err();
    validate_json_from_str("a = [* int, int]", r#"[]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn group_between_siblings() {
    validate_json_from_str("a = [bool, * (int, tstr), bool]", r#"[true, false]"#, None).unwrap();
    validate_json_from_str(
      "a = [bool, * (int, tstr), bool]",
      r#"[true, 1, "x", false]"#,
      None,
    )
    .unwrap();
    validate_json_from_str(
      "a = [bool, * (int, tstr), bool]",
      r#"[true, 1, "x", 2, "y", false]"#,
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 2.2.2, 3.2, 3.4.
  #[test]
  fn group_choice_inside() {
    validate_json_from_str("a = [1*1 (int, tstr // tstr, int)]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [1*1 (int, tstr // tstr, int)]", r#"["x", 1]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_after_sibling() {
    validate_json_from_str("a = [tstr, * pair]\npair = (int, tstr)", r#"["h"]"#, None).unwrap();
    validate_json_from_str(
      "a = [tstr, * pair]\npair = (int, tstr)",
      r#"["h", 1, "x"]"#,
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_exact() {
    validate_json_from_str("a = [1*1 b]\nb = (int, tstr)", r#"[1, "x"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (groupname entry).
  #[test]
  fn groupref_star() {
    validate_json_from_str("a = [* pair]\npair = (int, tstr)", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str(
      "a = [* pair]\npair = (int, tstr)",
      r#"[1, "x", 2, "y"]"#,
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn lower_bound_only() {
    validate_json_from_str("a = [2* (int, tstr)]", r#"[1, "x", 2, "y"]"#, None).unwrap();
    validate_json_from_str("a = [2* (int, tstr)]", r#"[1, "x", 2, "y", 3, "z"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.2.2, 3.2, 3.4; Appendix B (choice extension).
  #[test]
  fn named_type_choice_elem() {
    validate_json_from_str("a = [* elem]\nelem = int\nelem /= tstr", r#"[]"#, None).unwrap();
    validate_json_from_str(
      "a = [* elem]\nelem = int\nelem /= tstr",
      r#"[1, "x"]"#,
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn nested_parens() {
    validate_json_from_str("a = [1*1 (int, (tstr))]", r#"[1, "x"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn nested_star_of_star() {
    validate_json_from_str("a = [* [* (int, tstr)]]", r#"[[1, "x"]]"#, None).unwrap();
    validate_json_from_str("a = [* [* (int, tstr)]]", r#"[[1, "x", 2, "y"], []]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn occur_inside_occur() {
    validate_json_from_str("a = [* (int, 2*2 tstr)]", r#"[]"#, None).unwrap();
    validate_json_from_str("a = [* (int, 2*2 tstr)]", r#"[1, "x", "y"]"#, None).unwrap();
    validate_json_from_str(
      "a = [* (int, 2*2 tstr)]",
      r#"[1, "x", "y", 2, "p", "q"]"#,
      None,
    )
    .unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn occur_on_single_entry_group() {
    validate_json_from_str("a = [2*2 (int, 1*1 (tstr))]", r#"[1, "x", 2, "y"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn one_or_more() {
    validate_json_from_str("a = [+ (int, tstr)]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [+ (int, tstr)]", r#"[1, "x", 2, "y"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn optional() {
    validate_json_from_str("a = [? (int, tstr)]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [? (int, tstr)]", r#"[1]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn optional_middle() {
    validate_json_from_str("a = [int, ? tstr, bool]", r#"[1, true]"#, None).unwrap();
    validate_json_from_str("a = [int, ? tstr, bool]", r#"[1, "x", true]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn prefix_then_star() {
    validate_json_from_str("a = [tstr, * int]", r#"["h"]"#, None).unwrap();
    validate_json_from_str("a = [tstr, * int]", r#"["h", 1, 2]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 2.2.2, 3.2, 3.4; Appendix A (prioritized choice).
  #[test]
  fn prioritized_choice_locks() {
    validate_json_from_str("a = [(int // int, tstr)]", r#"[1, "x"]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 2.2.1, 3.2, 3.4. Errors recorded while trying a
  // failing type-choice alternative must not poison a successful one,
  // regardless of the order the alternatives appear in.
  #[test]
  fn type_choice_alternative_order() {
    validate_json_from_str("a = [* ([int] / [tstr])]", r#"[["x"]]"#, None).unwrap();
    validate_json_from_str("a = [* ([tstr] / [int])]", r#"[[1]]"#, None).unwrap();
  }

  // RFC 8610: Section 2.2.1. A type choice must reject an array when no
  // alternative matches it, including alternatives that are not array-shaped
  // (a non-array alternative failing against an array must count as a
  // failure, not be skipped silently).
  #[test]
  fn type_choice_non_array_alternates() {
    validate_json_from_str("a = tstr / bool", r#"[1]"#, None).unwrap_err();
    validate_json_from_str("a = [int, int] / tstr", r#"[1, "x"]"#, None).unwrap_err();
    validate_json_from_str("a = [* (tstr / bool)]", r#"[[1]]"#, None).unwrap_err();
    validate_json_from_str("a = [int, int] / tstr", r#""hi""#, None).unwrap();
    validate_json_from_str("a = tstr / [int]", r#"[1]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn range_1_2() {
    validate_json_from_str("a = [1*2 (int, tstr)]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [1*2 (int, tstr)]", r#"[1, "x", 2, "y"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_after() {
    validate_json_from_str("a = [(int, tstr), bool]", r#"[1, "x", true]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_before_no_occur() {
    validate_json_from_str("a = [bool, (int, tstr)]", r#"[true, 1, "x"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn sibling_before_occur() {
    validate_json_from_str("a = [bool, 1*1 (int, tstr)]", r#"[true, 1, "x"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_group_then_int() {
    validate_json_from_str("a = [* (int, tstr), int]", r#"[5]"#, None).unwrap();
    validate_json_from_str("a = [* (int, tstr), int]", r#"[1, "x", 5]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_of_var_arity() {
    validate_json_from_str("a = [* (int, * tstr)]", r#"[1]"#, None).unwrap();
    validate_json_from_str("a = [* (int, * tstr)]", r#"[1, "x", 2]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn star_then_tstr() {
    validate_json_from_str("a = [* int, tstr]", r#"["x"]"#, None).unwrap();
    validate_json_from_str("a = [* int, tstr]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [* int, tstr]", r#"[1, 2, "x"]"#, None).unwrap();
    validate_json_from_str("a = [* int, tstr]", r#"[]"#, None).unwrap_err();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4; Appendix B (parenthesized group).
  #[test]
  fn three_level_nesting() {
    validate_json_from_str("a = [1*1 (int, (tstr, (bool)))]", r#"[1, "x", true]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_after_sibling() {
    validate_json_from_str("a = [bool, ~b]\nb = [int, tstr]", r#"[true, 1, "x"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_sole() {
    validate_json_from_str("a = [~b]\nb = [int, tstr]", r#"[1, "x"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4, 3.7.
  #[test]
  fn unwrap_with_occur() {
    validate_json_from_str("a = [* ~b]\nb = [int, tstr]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [* ~b]\nb = [int, tstr]", r#"[1, "x", 2, "y"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn upper_only_group() {
    validate_json_from_str("a = [*2 (int, tstr)]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [*2 (int, tstr)]", r#"[1, "x", 2, "y"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn var_arity_inside() {
    validate_json_from_str("a = [1*1 (int, * tstr)]", r#"[1]"#, None).unwrap();
    validate_json_from_str("a = [1*1 (int, * tstr)]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [1*1 (int, * tstr)]", r#"[1, "x", "y"]"#, None).unwrap();
  }

  // RFC 8610: Sections 2.1, 3.2, 3.4.
  #[test]
  fn zero_or_more() {
    validate_json_from_str("a = [* (int, tstr)]", r#"[1, "x"]"#, None).unwrap();
    validate_json_from_str("a = [* (int, tstr)]", r#"[1, "x", 2, "y"]"#, None).unwrap();
  }
}
