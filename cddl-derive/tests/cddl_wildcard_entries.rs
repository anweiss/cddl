use cddl_derive::cddl_typegen;
use std::collections::HashMap;

// Regression test for https://github.com/anweiss/cddl/issues/640
//
// Every wildcard map entry (`* key => value`) is emitted as a field named
// `entries`, so a map with more than one of them used to fail to compile with
// "field `entries` specified more than once". Repeats now get a `_N` suffix.
//
// This test is primarily a *compilation* test: if the generated structs had
// duplicate field names, this file would not build.
cddl_typegen!("tests/fixtures/wildcard_entries.cddl");

#[test]
fn two_wildcard_entries_get_distinct_fields() {
  let m = Mixed {
    entries: HashMap::from([("a".to_string(), 1i64)]),
    entries_1: HashMap::from([(2i64, "b".to_string())]),
  };

  assert_eq!(m.entries.get("a"), Some(&1));
  assert_eq!(m.entries_1.get(&2), Some(&"b".to_string()));
}

#[test]
fn three_wildcard_entries_get_distinct_fields() {
  let t = Triple {
    entries: HashMap::from([("a".to_string(), 1i64)]),
    entries_1: HashMap::from([(2i64, "b".to_string())]),
    entries_2: HashMap::from([(true, 3i64)]),
  };

  assert_eq!(t.entries.len(), 1);
  assert_eq!(t.entries_1.len(), 1);
  assert_eq!(t.entries_2.len(), 1);
}

#[test]
fn named_fields_are_unaffected_by_wildcard_deduplication() {
  let n = NamedAndWildcard {
    id: "abc".to_string(),
    entries: HashMap::from([(1i64, "x".to_string())]),
  };

  assert_eq!(n.id, "abc");
  assert_eq!(n.entries.get(&1), Some(&"x".to_string()));
}
