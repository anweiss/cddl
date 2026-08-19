#![cfg(feature = "std")]
#![cfg(not(feature = "lsp"))]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{
  ast::{GroupEntry, MemberKey, Rule, Type2},
  parser::cddl_from_str,
};
use std::time::{Duration, Instant};

/// Regression tests for exponential parse time on deeply nested arrays.
///
/// `group_entry` used to begin two separate alternatives with
/// `occur? ~ S ~ member_key`, and `member_key`'s first alternative is
/// `type1 ~ &("=>")`. A group entry that is not a member -- every plain array
/// element -- therefore parsed its entire nested subtree once per `member_key`
/// attempt and again for the bare `type_expr` alternative: three full parses
/// per nesting level, i.e. O(3^depth).
///
/// Measured on the pre-fix grammar: depth 12 took 1.9s, depth 14 took 16.8s and
/// depth 16 took 152s. After factoring the shared prefix, the same depths take
/// 13ms, 50ms and 208ms.
///
/// The bounds below are deliberately loose -- many times the post-fix cost --
/// so they stay reliable on slow or contended CI hardware while still failing
/// by a wide margin on the old grammar. Depth 14 rather than 16 is used for the
/// absolute bound so that a genuine regression fails in about 17s instead of
/// about 96s.
fn nested_array(depth: usize) -> String {
  format!("a = {}int{}", "[".repeat(depth), "]".repeat(depth))
}

/// Best of several runs, so that one descheduled thread cannot skew a timing.
fn best_parse_time(src: &str, runs: u32) -> Duration {
  let mut best = Duration::MAX;
  for _ in 0..runs {
    let start = Instant::now();
    let parsed = cddl_from_str(src, true);
    let elapsed = start.elapsed();
    assert!(parsed.is_ok(), "expected {} to parse", src);
    best = best.min(elapsed);
  }
  best
}

#[test]
fn deeply_nested_arrays_parse_in_reasonable_time() {
  let src = nested_array(14);

  let start = Instant::now();
  let parsed = cddl_from_str(&src, true);
  let elapsed = start.elapsed();

  assert!(parsed.is_ok(), "depth-14 nested array should parse");
  assert!(
    elapsed < Duration::from_secs(5),
    "depth-14 nested array took {:?}; the pre-fix grammar took about 16.8s and \
     the post-fix grammar takes about 50ms, so this indicates the exponential \
     group_entry backtracking has returned",
    elapsed
  );
}

/// The cost curve must stay sub-exponential. Going from depth 7 to depth 14 grew
/// the old grammar by roughly 3^7 (~2000x) and the new one by roughly 2^7
/// (~130x). Comparing two depths rather than asserting an absolute time keeps
/// this meaningful across hardware.
#[test]
fn nested_array_parse_cost_is_not_exponential() {
  let shallow = nested_array(7);
  let deep = nested_array(14);

  // warm up so one-time initialisation is not attributed to the shallow parse
  let _ = cddl_from_str(&shallow, true);

  let shallow_ns = best_parse_time(&shallow, 5).as_nanos().max(1);
  let deep_ns = best_parse_time(&deep, 3).as_nanos().max(1);

  let ratio = deep_ns as f64 / shallow_ns as f64;
  assert!(
    ratio < 600.0,
    "depth 14 was {:.0}x the cost of depth 7 (shallow {}ns, deep {}ns); the \
     pre-fix grammar was roughly 2000x and the post-fix grammar is roughly \
     130x, so this looks exponential again",
    ratio,
    shallow_ns,
    deep_ns
  );
}

/// Nesting through the other bracketing forms must stay fast too. Parenthesised
/// groups route through `type_expr` and map entries use a cheap bareword key, so
/// neither hit the pathological path -- these guard against a "fix" that helps
/// arrays by pushing the cost somewhere else.
#[test]
fn deeply_nested_groups_and_maps_parse_quickly() {
  let parens = format!("a = {}int{}", "(".repeat(16), ")".repeat(16));
  let mut inner = String::from("int");
  for _ in 0..16 {
    inner = format!("{{ k: {} }}", inner);
  }
  let maps = format!("a = {}", inner);

  for src in [parens, maps] {
    let start = Instant::now();
    let parsed = cddl_from_str(&src, true);
    let elapsed = start.elapsed();
    assert!(parsed.is_ok(), "depth-16 nesting should parse: {}", src);
    assert!(
      elapsed < Duration::from_secs(5),
      "depth-16 nesting took {:?} for {}",
      elapsed,
      src
    );
  }
}

// ---------------------------------------------------------------------------
// Semantic equivalence
//
// The perf fix factored two `group_entry` alternatives that shared the prefix
// `occur? ~ S ~ member_key` into one alternative with an inner `("=>" | ":")`
// choice. Because `member_key` is itself an ordered choice whose first branch
// is `type1 ~ &("=>")`, the delicate part is that the arrow and colon forms
// must still select the same `member_key` branch as before. These tests pin
// down the member key variant produced by each syntactic form, so a future
// grammar edit cannot silently change which one is produced.
// ---------------------------------------------------------------------------

fn first_member_key(src: &str) -> (MemberKey<'_>, bool) {
  let cddl = Box::leak(Box::new(cddl_from_str(src, true).unwrap()));

  let group = match &cddl.rules[0] {
    Rule::Type { rule, .. } => match &rule.value.type_choices[0].type1.type2 {
      Type2::Map { group, .. } => group,
      Type2::Array { group, .. } => group,
      other => panic!("expected a map or array, got {:?}", other),
    },
    other => panic!("expected a type rule, got {:?}", other),
  };

  match &group.group_choices[0].group_entries[0].0 {
    GroupEntry::ValueMemberKey { ge, .. } => {
      let mk = ge.member_key.clone().expect("expected a member key");
      let is_cut = match &mk {
        MemberKey::Type1 { is_cut, .. } => *is_cut,
        _ => false,
      };
      (mk, is_cut)
    }
    other => panic!("expected a value member key entry, got {:?}", other),
  }
}

#[test]
fn colon_forms_produce_bareword_or_value_keys() {
  assert!(
    matches!(
      first_member_key("a = { key: int }").0,
      MemberKey::Bareword { .. }
    ),
    "bareword colon key should stay a Bareword"
  );
  assert!(
    matches!(
      first_member_key("a = { 1: tstr }").0,
      MemberKey::Value { .. }
    ),
    "literal integer colon key should stay a Value"
  );
  assert!(
    matches!(
      first_member_key(r#"a = { "key": int }"#).0,
      MemberKey::Value { .. }
    ),
    "literal text colon key should stay a Value"
  );
}

#[test]
fn arrow_forms_produce_type1_keys_and_preserve_cut() {
  let (mk, is_cut) = first_member_key("a = { key => int }");
  assert!(
    matches!(mk, MemberKey::Type1 { .. }),
    "arrow key should be a Type1 key"
  );
  assert!(!is_cut, "plain arrow key should not be cut");

  let (mk, is_cut) = first_member_key("a = { key ^ => int }");
  assert!(
    matches!(mk, MemberKey::Type1 { .. }),
    "cut arrow key should be a Type1 key"
  );
  assert!(is_cut, "`^ =>` must still be recorded as a cut");

  assert!(
    matches!(
      first_member_key("a = { (int / tstr) => bool }").0,
      MemberKey::Type1 { .. }
    ),
    "computed arrow key should be a Type1 key"
  );
}

#[test]
fn generic_args_pick_the_same_branch_for_both_delimiters() {
  assert!(
    cddl_from_str("foo<a> = [a]\nb = { foo<int> => tstr }", true).is_ok(),
    "generic typename with an arrow key should parse"
  );
  assert!(
    cddl_from_str("foo<a> = [a]\nb = [ foo<int> ]", true).is_ok(),
    "generic typename as a plain entry should parse"
  );
  // A generic typename used as a *colon* key is rejected, and was rejected
  // identically before the alternatives were factored (same message, same
  // position). Pinned here so the factoring cannot silently start accepting it.
  assert!(
    cddl_from_str("foo<a> = [a]\nb = { foo<int>: tstr }", true).is_err(),
    "generic typename with a colon key is rejected, as it was before"
  );
}

#[test]
fn cut_before_a_colon_is_still_rejected() {
  // `^` is only meaningful before `=>`. Factoring the alternatives must not
  // make `key ^ : int` suddenly parse.
  assert!(
    cddl_from_str("a = { key ^ : int }", true).is_err(),
    "`^` before `:` must remain a parse error"
  );
}

#[test]
fn non_member_entries_still_parse() {
  for src in [
    "a = [ int ]",
    "a = [ ? int ]",
    "a = [ * int ]",
    "a = [ 1*3 int ]",
    "g = ( x: int )\na = { g }",
    "a = [ (int, tstr) ]",
  ] {
    assert!(
      cddl_from_str(src, true).is_ok(),
      "non-member group entry should still parse: {}",
      src
    );
  }
}

// ---------------------------------------------------------------------------
// Linearity
//
// Factoring the shared `member_key` prefix removed one of three redundant full
// parses per nesting level, taking the curve from O(3^depth) to O(2^depth) --
// better, but still exponential. The residual cost was the remaining pair of
// full parses: `member_key`'s first branch is `type1 ~ &("=>")`, which parses
// an entire nested subtree and only then checks a lookahead that can never
// succeed for a plain array element, after which the bare `type_expr`
// alternative parsed the very same subtree again.
//
// Trying the bare-type alternative first, guarded by a cheap negative lookahead
// for the `=>`/`:` delimiter, means a non-member entry is parsed exactly once.
// The curve is now linear in depth.
//
// Measured (release, parse only): depth 16 went from 222ms to 0.05ms, and
// depth 24 from roughly 68s to 0.07ms. Doubling the depth now doubles the cost.
//
// The bounds below are hundreds of times the post-fix cost so they stay
// reliable on slow CI, while still failing by a wide margin on the previous
// grammar. Depths are ordered cheapest-first so a genuine regression fails in
// a few seconds rather than after a minute.
// ---------------------------------------------------------------------------

#[test]
fn nested_array_parse_time_is_linear_in_depth() {
  // Depth 20 costs about 3.9s on the previous grammar and 0.06ms now, so this
  // trips quickly if the exponential behaviour returns.
  for depth in [20usize, 24] {
    let src = nested_array(depth);
    let start = Instant::now();
    let parsed = cddl_from_str(&src, true);
    let elapsed = start.elapsed();
    assert!(parsed.is_ok(), "depth-{} nested array should parse", depth);
    assert!(
      elapsed < Duration::from_secs(1),
      "depth-{} nested array took {:?}; the previous grammar needed about 3.9s \
       at depth 20 and 68s at depth 24, so parse cost is exponential again",
      depth,
      elapsed
    );
  }
}

/// Arrays of parenthesised groups were the worst shape of all: every `[( ... )]`
/// level compounded the array and paren entries, giving roughly 3x per level
/// even after the earlier factoring (131s at depth 16).
#[test]
fn nested_arrays_of_parenthesised_groups_stay_fast() {
  for depth in [12usize, 14] {
    let mut inner = String::from("int");
    for _ in 0..depth {
      inner = format!("[({})]", inner);
    }
    let src = format!("a = {}", inner);

    let start = Instant::now();
    let parsed = cddl_from_str(&src, true);
    let elapsed = start.elapsed();
    assert!(parsed.is_ok(), "depth-{} [( )] nesting should parse", depth);
    assert!(
      elapsed < Duration::from_secs(1),
      "depth-{} [( )] nesting took {:?}; the previous grammar needed about 1.6s \
       at depth 12 and 14.5s at depth 14",
      depth,
      elapsed
    );
  }
}

/// Doubling the depth should roughly double the cost. Comparing two depths
/// rather than asserting an absolute time keeps this meaningful across
/// hardware. Linear is 2x; the previous grammar was about 2^8 (~256x). The
/// depths are kept small deliberately: if the exponential behaviour ever
/// regresses, depth 16 still fails in a fraction of a second rather than
/// hanging CI the way depth 32 would.
#[test]
fn doubling_nested_depth_roughly_doubles_parse_cost() {
  let shallow = nested_array(8);
  let deep = nested_array(16);

  let _ = cddl_from_str(&shallow, true);

  let shallow_ns = best_parse_time(&shallow, 5).as_nanos().max(1);
  let deep_ns = best_parse_time(&deep, 5).as_nanos().max(1);

  let ratio = deep_ns as f64 / shallow_ns as f64;
  assert!(
    ratio < 10.0,
    "depth 16 was {:.1}x the cost of depth 8 (shallow {}ns, deep {}ns); linear \
     growth is 2x and the previous grammar was about 256x",
    ratio,
    shallow_ns,
    deep_ns
  );
}

// ---------------------------------------------------------------------------
// Guard correctness
//
// The bare-type alternative is now tried before the member alternative, so a
// negative lookahead is the only thing stopping it from consuming the *key*
// half of `key: value` and leaving `: value` stranded. These tests pin that
// down, along with the AST shapes that the reordering must not disturb.
// ---------------------------------------------------------------------------

#[test]
fn the_delimiter_guard_does_not_steal_member_keys() {
  // Each of these would fail to parse if the bare-type alternative were allowed
  // to match the key and stop.
  for src in [
    "g = ( x: int )\na = { g }",
    "a = { x: int }",
    "a = { x: int, y: tstr }",
    "a = [ ( x: int, y: tstr ) ]",
    "a = { x => int }",
    "a = { x ^ => int }",
    "a = ( x: int )",
    "a = ( x => int )",
    "a = { * tstr => any }",
    "a = [ * ( x: int ) ]",
  ] {
    assert!(
      cddl_from_str(src, true).is_ok(),
      "delimiter guard must not break: {}",
      src
    );
  }
}

/// A parenthesised group entry must still be a group entry, not a
/// parenthesised *type*. The bare-type alternative can match `(int)` as a type,
/// so the parenthesised-group alternative has to keep winning.
#[test]
fn parenthesised_group_entries_are_still_groups() {
  let cddl = cddl_from_str("a = [ (int) ]", true).unwrap();
  let group = match &cddl.rules[0] {
    Rule::Type { rule, .. } => match &rule.value.type_choices[0].type1.type2 {
      Type2::Array { group, .. } => group,
      other => panic!("expected an array, got {:?}", other),
    },
    other => panic!("expected a type rule, got {:?}", other),
  };
  assert!(
    matches!(
      &group.group_choices[0].group_entries[0].0,
      GroupEntry::InlineGroup { .. }
    ),
    "`(int)` inside an array must stay an inline group entry, got {:?}",
    &group.group_choices[0].group_entries[0].0
  );
}

/// The guard must not consume the whitespace it looks past. An earlier version
/// of this fix placed the lookahead after the closing `)`, which let pest's
/// implicit whitespace skip run first and silently extended the enclosing
/// rule's span over the trailing newline.
#[test]
fn guard_lookahead_does_not_extend_rule_spans() {
  let src = "b = ( x: int ) \n a = { b }";
  let cddl = cddl_from_str(src, true).unwrap();
  let span = match &cddl.rules[0] {
    Rule::Group { span, .. } => *span,
    other => panic!("expected a group rule, got {:?}", other),
  };
  assert_eq!(
    span.1,
    14,
    "rule span must end at the closing paren, not swallow trailing whitespace; \
     got {:?} which covers {:?}",
    span,
    &src[span.0..span.1]
  );
}
