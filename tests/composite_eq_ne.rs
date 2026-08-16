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

fn both_validate_with_features(schema: &str, json: &str, cbor_hex: &str, features: &[&str]) {
  let bytes = hex::decode(cbor_hex).unwrap();
  let json_result = validate_json_from_str(schema, json, Some(features)).is_ok();
  let cbor_result = validate_cbor_from_slice(schema, &bytes, Some(features)).is_ok();
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

fn both_reject_with_features(schema: &str, json: &str, cbor_hex: &str, features: &[&str]) {
  let bytes = hex::decode(cbor_hex).unwrap();
  let json_result = validate_json_from_str(schema, json, Some(features)).is_ok();
  let cbor_result = validate_cbor_from_slice(schema, &bytes, Some(features)).is_ok();
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
fn array_eq_ne_apply_one_aggregate_predicate() {
  let ne = "x = [uint, uint] .ne [1, 2]";
  both_reject(ne, "[1,2]", "820102");
  both_validate(ne, "[1,3]", "820103");

  let eq = "x = [uint, uint] .eq [1, 2]";
  both_validate(eq, "[1,2]", "820102");
  both_reject(eq, "[1,3]", "820103");

  // A length difference proves aggregate inequality; it is not an error from
  // the controller's element-count check.
  both_validate("x = [* uint] .ne [1, 2]", "[1]", "8101");
  both_reject("x = [* uint] .eq [1, 2]", "[1]", "8101");
}

#[test]
fn map_eq_ne_are_order_independent_complete_pair_predicates() {
  let ne = r#"x = { tstr => uint } .ne { "a": 1 }"#;
  both_reject(ne, r#"{"a":1}"#, "a1616101");
  both_validate(ne, r#"{"a":2}"#, "a1616102");
  both_validate(ne, r#"{"b":1}"#, "a1616201");

  let eq = r#"x = { * tstr => uint } .eq { "a": 1, "b": 2 }"#;
  both_validate(eq, r#"{"b":2,"a":1}"#, "a2616202616101");
  both_reject(eq, r#"{"a":1}"#, "a1616101");

  // A pair-count difference likewise proves inequality as a whole.
  both_validate(
    r#"x = { * tstr => uint } .ne { "a": 1, "b": 2 }"#,
    r#"{"a":1}"#,
    "a1616101",
  );
}

#[test]
fn composite_controls_validate_the_left_hand_side_first() {
  both_reject(
    r#"x = { "required": uint } .eq { "other": 1 }"#,
    r#"{"other":1}"#,
    "a1656f7468657201",
  );
  both_reject(
    r#"x = { "required": uint } .ne { "other": 1 }"#,
    r#"{"other":2}"#,
    "a1656f7468657202",
  );
  both_reject("x = [uint, uint] .ne [1, 2]", r#"[1,"x"]"#, "82016178");
  both_reject("x = [uint] .ne [1]", r#"{"a":1}"#, "a1616101");
}

#[test]
fn nested_and_numeric_composite_equality_follows_rfc_8610() {
  let nested_eq = r#"x = [any] .eq [{ "a": [1, 2] }]"#;
  both_validate(nested_eq, r#"[{"a":[1,2]}]"#, "81a16161820102");
  both_reject(nested_eq, r#"[{"a":[1,3]}]"#, "81a16161820103");
  both_validate(
    r#"x = [any] .ne [{ "a": [1, 2] }]"#,
    r#"[{"a":[1,3]}]"#,
    "81a16161820103",
  );

  // Section 3.8.6 keeps numeric values inside composites unequal when one is
  // an integer and the other is floating point; that distinction exists in
  // the CBOR data model. Normative Appendix E gives JSON only one kind of
  // number, so the same numeric value compares equal in JSON.
  assert!(!cbor_validates("x = [number] .eq [1.0]", "8101"));
  assert!(json_validates("x = [number] .eq [1.0]", "[1]"));
  assert!(cbor_validates("x = [number] .ne [1.0]", "8101"));
  assert!(!json_validates("x = [number] .ne [1.0]", "[1]"));
  both_validate("x = [number] .eq [1.0]", "[1.0]", "81f93c00");
  both_reject("x = [number] .ne [1.0]", "[1.0]", "81f93c00");

  let map_eq = r#"x = { * tstr => number } .eq { "a": 1.0 }"#;
  let map_ne = r#"x = { * tstr => number } .ne { "a": 1.0 }"#;
  assert!(!cbor_validates(map_eq, "a1616101"));
  assert!(json_validates(map_eq, r#"{"a":1}"#));
  assert!(cbor_validates(map_ne, "a1616101"));
  assert!(!json_validates(map_ne, r#"{"a":1}"#));
}

#[test]
fn cbor_composites_preserve_tags_and_major_types() {
  let tagged_eq = "x = [any] .eq [#6.42(1)]";
  assert!(cbor_validates(tagged_eq, "81d82a01"));
  assert!(!cbor_validates(tagged_eq, "81d82b01"));
  assert!(!cbor_validates(tagged_eq, "8101"));

  let tagged_ne = "x = [any] .ne [#6.42(1)]";
  assert!(!cbor_validates(tagged_ne, "81d82a01"));
  assert!(cbor_validates(tagged_ne, "81d82b01"));
  assert!(cbor_validates(tagged_ne, "8101"));

  // A text controller remains distinct from a different CBOR major type.
  assert!(!cbor_validates(r#"x = [any] .eq ["a"]"#, "8101"));
  assert!(cbor_validates(r#"x = [any] .ne ["a"]"#, "8101"));
}

#[test]
fn composite_default_is_aggregate_ne_in_an_optional_context() {
  let schema = "x = { ? value: ([uint, uint] .default [1, 2]) }";
  both_validate(schema, "{}", "a0");
  both_reject(schema, r#"{"value":[1,2]}"#, "a16576616c7565820102");
  both_validate(schema, r#"{"value":[1,3]}"#, "a16576616c7565820103");
  both_reject(schema, r#"{"value":[1,"x"]}"#, "a16576616c756582016178");
}

#[test]
fn composite_equality_counts_extra_pairs_and_elements() {
  // An extra pair or element proves aggregate inequality even when every
  // controller pair or element matches. This direction depends on the
  // controller probe rejecting unclaimed keys and surplus elements, so it
  // gets its own pinning vectors.
  both_validate(
    r#"x = { * tstr => uint } .ne { "a": 1 }"#,
    r#"{"a":1,"b":2}"#,
    "a2616101616202",
  );
  both_reject(
    r#"x = { * tstr => uint } .eq { "a": 1 }"#,
    r#"{"a":1,"b":2}"#,
    "a2616101616202",
  );
  both_validate("x = [* uint] .ne [1, 2]", "[1,2,3]", "83010203");
  both_reject("x = [* uint] .eq [1, 2]", "[1,2,3]", "83010203");
}

#[test]
fn empty_composite_controllers_compare_as_aggregate_values() {
  // An empty controller is an ordinary aggregate value: only an empty data
  // item equals it. The base instead let the empty-container guard reject
  // `[1]` against `.ne []` and accept `[]` as differing from itself.
  both_validate("x = [* uint] .ne []", "[1]", "8101");
  both_reject("x = [* uint] .ne []", "[]", "80");
  both_validate("x = [* uint] .eq []", "[]", "80");
  both_reject("x = [* uint] .eq []", "[1]", "8101");

  both_validate(r#"x = { * tstr => uint } .ne {}"#, r#"{"a":1}"#, "a1616101");
  both_reject(r#"x = { * tstr => uint } .ne {}"#, "{}", "a0");
  both_validate(r#"x = { * tstr => uint } .eq {}"#, "{}", "a0");
  both_reject(r#"x = { * tstr => uint } .eq {}"#, r#"{"a":1}"#, "a1616101");
}

#[test]
fn cross_kind_composite_controllers_are_never_equal() {
  // Section 3.8.6: "all other cases are not equal". An array data item is
  // unequal to a map controller and vice versa, so `.ne` accepts and `.eq`
  // rejects whenever the target itself matches. The base applied the
  // controller as the target's shape and rejected the `.ne` rows.
  both_validate(r#"x = [* uint] .ne { "a": 1 }"#, "[1]", "8101");
  both_reject(r#"x = [* uint] .eq { "a": 1 }"#, "[1]", "8101");
  both_validate(
    r#"x = { * tstr => uint } .ne [1]"#,
    r#"{"a":1}"#,
    "a1616101",
  );
  both_reject(
    r#"x = { * tstr => uint } .eq [1]"#,
    r#"{"a":1}"#,
    "a1616101",
  );
}

#[test]
fn eq_rejects_data_items_that_fail_the_composite_target() {
  // The complete left-hand side stays authoritative for `.eq` too: a
  // wrong-kind data item fails LHS membership rather than falling
  // through unvalidated.
  both_reject("x = [uint, uint] .eq [1, 2]", r#""hi""#, "626869");
  both_reject(r#"x = { a: uint } .eq { "a": 1 }"#, "[1]", "8101");
  // The `.ne` mirror shares the guard: a wrong-kind data item fails the
  // left-hand side before any equality question arises. Ruby 0.12.14
  // rejects in both encodings.
  both_reject("x = [uint, uint] .ne [1, 2]", r#""hi""#, "626869");
}

#[test]
fn member_key_controls_keep_zero_occurrence_entries_satisfiable() {
  // A control in member-key position stays outside the aggregate arms (the
  // candidate-key route is a separate, pre-existing gap), so an empty map
  // must keep satisfying zero-occurrence entries. The JSON `?` and
  // `.default` key verdicts belong to that separate class and are not
  // pinned here.
  both_validate("x = { * ([uint] .eq [1]) => tstr }", "{}", "a0");
  assert!(cbor_validates("x = { ? ([uint] .eq [1]) => tstr }", "a0"));
  assert!(cbor_validates(
    "x = { * ([uint] .default [1]) => tstr }",
    "a0"
  ));
}

#[test]
fn map_target_member_key_controls_are_inert_like_array_targets() {
  // A control in member-key position never reaches the candidate-key
  // machinery; the member-key guard makes map targets the same silent
  // no-op as array targets, in both encodings, so the occurrence logic
  // stays in charge of the entry: an empty map satisfies the
  // zero-occurrence entries.
  both_validate(
    r#"x = { * ({ "a": 1 } .eq { "a": 1 }) => tstr }"#,
    "{}",
    "a0",
  );
  both_validate(
    r#"x = { * ({ "a": 1 } .ne { "b": 2 }) => tstr }"#,
    "{}",
    "a0",
  );
}

#[test]
fn occurrence_bearing_controllers_compare_as_membership() {
  // An occurrence-bearing controller sits outside Section 3.8.6's "just
  // that single value" contract the same way a type choice does, and the
  // probe applies the same reading: the data item is "equal" when the
  // controller type matches it. Ruby 0.12.14 gives these verdicts in both
  // encodings (with parenthesized operands).
  both_validate("x = [uint, uint] .eq [* uint]", "[1,2]", "820102");
  both_reject("x = [uint] .ne [* uint]", "[1]", "8101");
  both_validate("x = [uint / tstr] .ne [* uint]", r#"["a"]"#, "816161");
}

#[test]
fn scalar_target_controller_traversal_stays_outside_the_aggregate_arms() {
  // A scalar-target control visits its controller on the outer validator
  // with `state.ctrl` set. When that controller is a rule resolving to a
  // composite control, the visit re-enters the control arms; the aggregate
  // probes must not treat the enclosing traversal's data item as their own
  // target. A data item of a different kind is simply unequal, so the outer
  // `.ne` stays satisfiable. Ruby 0.12.14 accepts the tstr row in both
  // encodings.
  let ne = "x = tstr .ne c\nc = [uint] .eq [1]";
  both_validate(ne, r#""hi""#, "626869");
  let numeric_ne = "x = uint .ne c\nc = [uint] .eq [1]";
  both_validate(numeric_ne, "5", "05");

  // The `.eq` mirror visits its controller without `state.ctrl`, so the
  // aggregate probes do run: "hi" equals no array, and Ruby 0.12.14
  // rejects in both encodings.
  let eq = "x = tstr .eq c\nc = [uint] .eq [1]";
  both_reject(eq, r#""hi""#, "626869");
}

#[test]
fn and_within_operands_apply_aggregate_composite_semantics() {
  // RFC 8610 Section 3.8.5: `.and` means the data item "matches both the
  // left-hand-side type and the type given as the right-hand side"
  // (formally an intersection), and `.within` has "the identical formal
  // semantics". Each operand is therefore a type of the current data item,
  // not a comparison value, so a composite control reached as an operand
  // keeps its aggregate probes. Ruby 0.12.14 gives every verdict below in
  // both encodings.
  let and_ne = "x = [uint, uint] .and c\nc = [uint, uint] .ne [1, 2]";
  both_validate(and_ne, "[3,4]", "820304");
  both_reject(and_ne, "[1,2]", "820102");
  // One differing component is aggregate inequality here too —
  // propagating `.ne` into the element visitors would instead require
  // every component to differ.
  both_validate(and_ne, "[1,3]", "820103");

  let and_eq = "x = [uint, uint] .and c2\nc2 = [uint, uint] .eq [1, 2]";
  both_validate(and_eq, "[1,2]", "820102");
  both_reject(and_eq, "[1,3]", "820103");

  let within_ne = "x = d .within [uint, uint]\nd = [uint, uint] .ne [1, 2]";
  both_validate(within_ne, "[3,4]", "820304");
  both_reject(within_ne, "[1,2]", "820102");
  let within_controller = "x = [uint, uint] .within e\ne = [uint, uint] .ne [1, 2]";
  both_validate(within_controller, "[3,4]", "820304");
  both_reject(within_controller, "[1,2]", "820102");

  // An intersection with a wrong-kind composite operand is unsatisfiable;
  // the pre-probe arms fell through silently and accepted the data item.
  let and_wrong_kind = "x = tstr .and d2\nd2 = [uint] .ne [2]";
  both_reject(and_wrong_kind, r#""hi""#, "626869");

  let and_map = r#"x = { tstr => uint } .and m
m = { tstr => uint } .ne { "a": 1 }"#;
  both_validate(and_map, r#"{"a":2}"#, "a1616102");
  both_reject(and_map, r#"{"a":1}"#, "a1616101");
}

#[test]
fn feature_wrapped_composite_controls_keep_aggregate_semantics() {
  // RFC 9165 Section 4: `.feature` "annotates the target as making use of
  // the feature named by the controller" — the target stays the type of
  // the data item, so an enabled feature must not demote a wrapped
  // composite control to operand behavior. Ruby 0.12.14 gives every
  // verdict below in both encodings.
  let ne = r#"x = ([uint, uint] .ne [1, 2]) .feature "f""#;
  both_validate_with_features(ne, "[3,4]", "820304", &["f"]);
  both_reject_with_features(ne, "[1,2]", "820102", &["f"]);
  both_validate_with_features(ne, "[1,3]", "820103", &["f"]);

  let eq = r#"x = ([uint, uint] .eq [1, 2]) .feature "f""#;
  both_validate_with_features(eq, "[1,2]", "820102", &["f"]);
  both_reject_with_features(eq, "[1,3]", "820103", &["f"]);
}

#[test]
fn default_operands_of_scalar_ne_traversals_fall_through_by_kind() {
  // A composite `.default` reached as the controller of an enclosing
  // scalar-target `.ne` follows the operand guard: a wrong-kind data item
  // falls through silently (nothing equals an array, so the outer `.ne`
  // stays satisfied), and a matched-kind data item visits the controller
  // under the enclosing control's leaf semantics. Ruby 0.12.14 accepts the
  // tstr row and rejects [1] in both encodings.
  let ne = "x = tstr .ne c\nc = [uint] .default [1]";
  both_validate(ne, r#""hi""#, "626869");
  both_reject(ne, "[1]", "8101");
  // The [2] cell keeps the enclosing scalar arms' pre-existing JSON/CBOR
  // split (the JSON arm never validates the target kind); the scalar
  // operand-composition class is tracked separately.
  assert!(json_validates(ne, "[2]"));
  assert!(!cbor_validates(ne, "8102"));
}

#[test]
fn scalar_controllers_of_composite_targets_compare_cross_kind() {
  // Section 3.8.6 ends with "all other cases are not equal": an array or a
  // map is never equal to a text string or a number, so `.ne` with a scalar
  // controller accepts every LHS-valid data item and the `.eq` mirror
  // rejects it. Ruby 0.12.14 gives these verdicts in both encodings.
  both_validate(r#"x = { "a": uint } .ne tstr"#, r#"{"a":1}"#, "a1616101");
  both_reject(r#"x = { "a": uint } .eq tstr"#, r#"{"a":1}"#, "a1616101");
  both_validate("x = [uint] .ne uint", "[1]", "8101");
  both_reject("x = [uint] .eq uint", "[1]", "8101");
  both_validate("x = [uint] .ne 1", "[1]", "8101");

  // `any` contains every value, so under the membership reading nothing
  // satisfies `.ne any` and everything satisfies `.eq any`. Ruby 0.12.14
  // rejects both `.ne any` rows in both encodings.
  both_reject("x = [uint] .ne any", "[1]", "8101");
  both_reject(r#"x = { "a": uint } .ne any"#, r#"{"a":1}"#, "a1616101");
  both_validate("x = [uint] .eq any", "[1]", "8101");
}

#[test]
fn text_controllers_never_key_match_map_targets() {
  // A text literal in value position must not claim a map entry whose key
  // happens to share its spelling; entry claiming belongs to member-key
  // validation (RFC 8610 Section 3.5.1), and Section 3.8.6 makes a map
  // unequal to every text string. The JSON validator's TEXT-vs-object arm
  // used to key-find in any position, which made these `.ne` forms
  // unsatisfiable and the `.eq` forms vacuously true. Ruby 0.12.14 accepts
  // the `.ne "a"` row and rejects the `.eq "a"` row in both encodings.
  both_validate(r#"x = { "a": uint } .ne "a""#, r#"{"a":1}"#, "a1616101");
  both_reject(r#"x = { "a": uint } .eq "a""#, r#"{"a":1}"#, "a1616101");

  // An unresolvable typename controller is a schema error, not a text
  // literal: the aggregate `.ne` fails closed instead of accepting through
  // the bareword-to-text fallback (RFC 8610 Section 3.9 reserves the
  // undefined-name laxity for socket names). A choice of text literals
  // reaches the gated arm per alternative.
  both_reject(r#"x = { "a": uint } .ne a"#, r#"{"a":1}"#, "a1616101");
  both_validate(
    r#"x = { "a": uint } .ne ("a" / "z")"#,
    r#"{"a":1}"#,
    "a1616101",
  );

  // The literal "any" is only a string; the arm's bareword-`any` shortcut
  // must not make it equal to a map either.
  both_validate(r#"x = { "a": uint } .ne "any""#, r#"{"a":1}"#, "a1616101");

  // The composite `.default` intercept shares the probes, so a present,
  // LHS-valid member must not be rejected through the same false equality.
  both_validate(
    r#"x = { ? v: ({ "a": uint } .default "a") }"#,
    r#"{"v":{"a":1}}"#,
    "a16176a1616101",
  );

  // Direct validation through the same arm: a text literal type never
  // matches a map data item. Ruby 0.12.14 rejects in both encodings.
  both_reject(r#"x = "a""#, r#"{"a":1}"#, "a1616101");
}

#[test]
fn composite_controllers_evaluate_rule_resolved_control_operands() {
  // A controller may resolve through a rule to an inner control whose own
  // target is written as a typename or a parenthesized type. The equality
  // probe must evaluate that inner control as aggregate equality; treating
  // its former no-op (typename) or catch-all error (parenthesized) as an
  // equality verdict made the outer `.ne` unsatisfiable or universally
  // satisfied. Ruby 0.12.14 gives these verdicts in both encodings.
  let named_inner_target = r#"x = [uint] .ne c
c = arr .eq [1]
arr = [uint]"#;
  both_reject(named_inner_target, "[1]", "8101");
  both_validate(named_inner_target, "[2]", "8102");

  let named_inner_target_eq = r#"x = [uint] .eq c
c = arr .eq [1]
arr = [uint]"#;
  both_validate(named_inner_target_eq, "[1]", "8101");
  both_reject(named_inner_target_eq, "[2]", "8102");

  let parenthesized_inner_target = r#"x = [uint] .ne c
c = ([uint]) .eq [1]"#;
  both_reject(parenthesized_inner_target, "[1]", "8101");
  both_validate(parenthesized_inner_target, "[2]", "8102");
}

#[test]
fn typename_and_parenthesized_composite_targets_apply_aggregate_equality() {
  // RFC 8610 Section 3.8 relates a target type with a controller type; a
  // composite target written as a typename or a parenthesized type denotes
  // the same array or map type as its literal spelling, so it takes the
  // same aggregate equality. Ruby 0.12.14 gives these verdicts in both
  // encodings.
  let named_eq = "x = arr .eq [1, 2]\narr = [uint, uint]";
  both_validate(named_eq, "[1,2]", "820102");
  both_reject(named_eq, "[1,3]", "820103");
  both_reject(named_eq, "\"hi\"", "626869");

  let named_ne = "x = arr .ne [1, 2]\narr = [uint, uint]";
  both_reject(named_ne, "[1,2]", "820102");
  both_validate(named_ne, "[1,3]", "820103");
  both_reject(named_ne, "\"hi\"", "626869");

  let parenthesized_eq = "x = ([uint, uint]) .eq [1, 2]";
  both_validate(parenthesized_eq, "[1,2]", "820102");
  both_reject(parenthesized_eq, "[1,3]", "820103");

  let named_map_ne = "x = m .ne { \"a\": 1 }\nm = { tstr => uint }";
  both_reject(named_map_ne, r#"{"a":1}"#, "a1616101");
  both_validate(named_map_ne, r#"{"a":2}"#, "a1616102");

  let typename_chain = "x = a2 .eq [1]\na2 = a1\na1 = [uint]";
  both_validate(typename_chain, "[1]", "8101");
  both_reject(typename_chain, "[2]", "8102");
}

#[test]
fn typename_composite_defaults_apply_the_implied_ne() {
  // The composite `.default` intercept follows the same target resolution,
  // so a typename target takes the implied `.ne` instead of the scalar
  // path's target-only visit.
  let top_level = "x = d .default [1]\nd = [uint]";
  both_reject(top_level, "[1]", "8101");
  both_validate(top_level, "[2]", "8102");

  let optional_member = "x = { ? v: (d .default [1]) }\nd = [uint]";
  both_validate(optional_member, r#"{"v":[2]}"#, "a161768102");
  both_reject(optional_member, r#"{"v":[1]}"#, "a161768101");
  both_validate(optional_member, "{}", "a0");
}

#[test]
fn value_position_text_literals_reject_objects_in_every_route() {
  // The value-position finder gate reaches beyond composite controllers:
  // scalar `.eq`/`.ne` text controllers evaluated against object data and
  // text-literal member values whose data value is an object take the same
  // cross-kind rejection (RFC 8610 Sections 3.5.1 and 3.8.6). Ruby 0.12.14
  // rejects every rejecting row here in both encodings.
  both_reject(r#"x = tstr .ne "a""#, r#"{"b":2}"#, "a1616202");
  both_reject(r#"x = tstr .ne "a""#, r#"{"a":1}"#, "a1616101");
  both_reject(r#"x = tstr .eq "a""#, r#"{"a":1}"#, "a1616101");
  both_reject(r#"x = { ? v: "a" }"#, r#"{"v":{"a":9}}"#, "a16176a1616109");
  both_reject(
    r#"x = { * tstr => "a" }"#,
    r#"{"k":{"a":1}}"#,
    "a1616ba1616101",
  );
  both_reject(r#"x = { v: "a" }"#, r#"{"v":{"a":9}}"#, "a16176a1616109");
  both_validate(r#"x = { v: "a" }"#, r#"{"v":"a"}"#, "a161766161");
}

#[test]
fn composite_default_applies_implied_ne_outside_optional_contexts() {
  // Section 3.8.6 defines .default as a variant of .ne; the implied .ne
  // holds wherever the control appears, not only in an optional member.
  let schema = "x = [uint, uint] .default [1, 2]";
  both_reject(schema, "[1,2]", "820102");
  both_validate(schema, "[1,3]", "820103");
}

#[test]
fn alternate_bearing_typename_targets_keep_the_fall_through() {
  // Alternate-bearing typename targets deliberately stay on their
  // pre-existing fall-through route. The exclusion is conservative, not
  // load-bearing for correctness: routing these targets into the probes
  // yields the correct aggregate verdicts, but the change interacts
  // with the CBOR membership pre-gate below and lands as its own
  // reviewed row, which owns removing the exclusion. These vectors pin
  // the fall-through verdicts until that row removes them.
  let composite_alternate = "x = arr .eq [1]\narr = [1]\narr /= [2]";
  both_validate(composite_alternate, "[1]", "8101");
  both_validate(composite_alternate, "[3]", "8103");

  // The scalar-alternate spelling has an encoding split: JSON falls
  // through and accepts, while CBOR's control-target membership
  // pre-gate reads the bool alternate and rejects.
  let scalar_alternate_eq = "x = arr .eq [1]\narr = [uint]\narr /= bool";
  assert!(json_validates(scalar_alternate_eq, "[1]"));
  assert!(!cbor_validates(scalar_alternate_eq, "8101"));

  let scalar_alternate_ne = "x = arr .ne [1]\narr = [uint]\narr /= bool";
  assert!(json_validates(scalar_alternate_ne, "[2]"));
  assert!(!cbor_validates(scalar_alternate_ne, "8102"));
}

#[test]
fn undefined_rule_controllers_fail_closed_for_composite_ne() {
  // RFC 8610 Section 3.9 reserves the undefined-name laxity for socket
  // names: tools do not raise an error only "if such a type or group"
  // carries the $/$$ convention. An ordinary undefined name in a
  // controller is a schema error, and the aggregate .ne inversion must
  // not convert the controller probe's failure into an accept.
  both_reject("x = [uint] .ne nosuchrule", "[1]", "8101");
  both_reject("x = [uint] .ne nosuchrule", "[2]", "8102");
  both_reject("x = [uint] .ne [nosuchrule]", "[1]", "8101");
  both_reject("x = [uint] .ne c\nc = [nosuchrule]", "[1]", "8101");

  // An unplugged type socket stays the empty type choice: .ne holds
  // vacuously.
  both_validate("x = [uint] .ne $sock", "[1]", "8101");
}

#[test]
fn choice_targets_apply_aggregate_equality() {
  // Every alternative of these targets is composite, so the resolver
  // routes them; the data item is a member of the choice and the
  // aggregate predicate compares it against the controller once.
  let paren_eq = "x = ([1] / [2]) .eq [1]";
  both_validate(paren_eq, "[1]", "8101");
  both_reject(paren_eq, "[2]", "8102");
  both_reject(paren_eq, "[3]", "8103");

  let paren_ne = "x = ([1] / [2]) .ne [1]";
  both_reject(paren_ne, "[1]", "8101");
  both_validate(paren_ne, "[2]", "8102");
  both_reject(paren_ne, "[3]", "8103");

  let typename_ne = "x = arr .ne [1]\narr = [1] / [2]";
  both_reject(typename_ne, "[1]", "8101");
  both_validate(typename_ne, "[2]", "8102");
  both_reject(typename_ne, "[3]", "8103");
}

#[test]
fn composite_controls_keep_aggregate_semantics_in_group_entry_positions() {
  // The typename and parenthesized routings fire in array-element,
  // occurrence, and map-value positions exactly like the literal
  // spelling; zero-occurrence entries stay satisfiable.
  let element = "x = [arr .eq [1]]\narr = [uint]";
  both_validate(element, "[[1]]", "818101");
  both_reject(element, "[[2]]", "818102");

  let paren_element = "x = [(([uint]) .eq [1])]";
  both_validate(paren_element, "[[1]]", "818101");

  let star = "x = [* (arr .eq [1])]\narr = [uint]";
  both_validate(star, "[]", "80");
  both_validate(star, "[[1],[1]]", "8281018101");
  both_reject(star, "[[2]]", "818102");

  let member_value = r#"x = { k: (arr .ne [1]) }
arr = [uint]"#;
  both_reject(member_value, r#"{"k":[1]}"#, "a1616b8101");
  both_validate(member_value, r#"{"k":[2]}"#, "a1616b8102");

  let table_value = r#"x = { * tstr => (arr .eq [1]) }
arr = [uint]"#;
  both_validate(table_value, "{}", "a0");
  both_validate(table_value, r#"{"a":[1]}"#, "a161618101");
  both_reject(table_value, r#"{"a":[2]}"#, "a161618102");

  let default_value = r#"x = { ? v: (arr .default [1]) }
arr = [uint]"#;
  both_validate(default_value, "{}", "a0");
  both_reject(default_value, r#"{"v":[1]}"#, "a161768101");
  both_validate(default_value, r#"{"v":[2]}"#, "a161768102");
}

#[test]
fn defined_generic_controllers_stay_invertible_under_composite_ne() {
  // RFC 8610 Section 3.10: a generic rule's formal parameters "are bound
  // to the actual arguments supplied ..., within the scope of the generic
  // rule (as if there were a rule of the form parameter = argument)", so
  // a parameter name inside its rule's body is a bound identifier, not an
  // undefined rule reference, and a defined generic controller must not
  // trip the fail-closed undefined-name walker: the aggregate `.ne`
  // inversion stays in force. Ruby 0.12.14 gives every accepting and
  // rejecting verdict below in both encodings.
  let direct = "x = [uint] .ne g<1>\ng<T> = [T]";
  both_reject(direct, "[1]", "8101");
  both_validate(direct, "[2]", "8102");

  let rule_resolved = "x = [uint] .ne c\nc = g<1>\ng<T> = [T]";
  both_reject(rule_resolved, "[1]", "8101");
  both_validate(rule_resolved, "[2]", "8102");

  // The nested-literal spelling: the controller [g<1>] denotes [[1]],
  // so [[2]] is unequal and the inversion must accept it (a walker that
  // misread generic parameters as undefined names would fail closed
  // here).
  let nested_literal = "x = [[uint]] .ne [g<1>]\ng<T> = [T]";
  both_reject(nested_literal, "[[1]]", "818101");
  both_validate(nested_literal, "[[2]]", "818102");

  let map_form = "x = { \"a\": uint } .ne m<1>\nm<V> = { \"a\": V }";
  both_reject(map_form, "{\"a\":1}", "a1616101");
  both_validate(map_form, "{\"a\":2}", "a1616102");

  // The control may itself sit inside a generic rule body, where the
  // controller references the enclosing rule's own parameter; the
  // in-scope instantiation binds it.
  let enclosing = "x = w<2>\nw<T> = [uint] .ne [T]";
  both_reject(enclosing, "[2]", "8102");
  both_validate(enclosing, "[1]", "8101");

  // An undefined name in a generic ARGUMENT is still a schema error
  // (RFC 8610 Section 3.9 reserves the undefined-name laxity for socket
  // names): binding the parameters must not unfix the fail-closed route.
  let undefined_arg = "x = [uint] .ne g<nosuchrule>\ng<T> = [T]";
  both_reject(undefined_arg, "[2]", "8102");

  // A `.cbor`-embedded spelling resolves through the same walker in the
  // nested validator.
  let embedded = "x = bstr .cbor y\ny = [uint] .ne g<1>\ng<T> = [T]";
  assert!(cbor_validates(embedded, "428102"));
  assert!(!cbor_validates(embedded, "428101"));
}

#[test]
fn alternate_bearing_rules_inside_literal_controllers_compare_by_membership() {
  // A `/=`-extended rule referenced INSIDE a literal composite controller
  // is resolved by the controller probe like any other element type, so
  // the controller denotes the full choice and aggregate `.ne` takes the
  // membership reading: [[1]] is equal to an alternative of [c] and is
  // rejected, [[3]] is unequal to every alternative and is accepted.
  // Ruby 0.12.14 gives both verdicts in both encodings. (The same rule
  // used directly AS the controller resolves the same way — pinned by
  // alternate_bearing_direct_controllers_compare_by_membership below.)
  let schema = "x = [[uint]] .ne [c]\nc = [1]\nc /= [2]";
  both_reject(schema, "[[1]]", "818101");
  both_validate(schema, "[[3]]", "818103");
}

#[test]
fn alternate_bearing_direct_controllers_compare_by_membership() {
  // A `/=`-extended rule used directly AS the controller resolves to its
  // complete named choice, so the controller denotes every alternative
  // and the membership reading applies, consistent with the
  // literal-controller spelling pinned above: `.ne c` rejects a data
  // item equal to any alternative and accepts the rest, and `.eq`
  // mirrors. Ruby 0.12.14 corroborates every cell in both encodings.
  let ne = "x = [uint] .ne c\nc = [1]\nc /= [2]";
  both_reject(ne, "[1]", "8101");
  both_reject(ne, "[2]", "8102");
  both_validate(ne, "[3]", "8103");

  let eq = "x = [uint] .eq c\nc = [1]\nc /= [2]";
  both_validate(eq, "[1]", "8101");
  both_validate(eq, "[2]", "8102");
  both_reject(eq, "[3]", "8103");
}

#[test]
fn byte_literal_composite_controllers_compare_as_aggregate_values() {
  // Byte-string literals inside composite operands are ordinary element
  // types to the probes: equality is bytewise, and the aggregate result
  // inverts once for `.ne` and `.default` like any other controller.
  assert!(cbor_validates("x = [bstr] .eq [h'FF']", "8141ff"));
  assert!(!cbor_validates("x = [bstr] .eq [h'FF']", "8141fe"));
  assert!(!cbor_validates("x = [bstr] .ne [h'FF']", "8141ff"));

  // A scalar byte-literal controller of a typename composite target is a
  // cross-kind comparison: no array equals a byte string, so `.eq`
  // rejects.
  let scalar = "x = arr .eq h'FF'\narr = [uint]";
  both_reject(scalar, "[1]", "8101");

  // An inner byte-literal `.eq` controller resolves through the probes;
  // its own target fails on this data item, so nothing equals `c` and
  // the outer `.ne` holds.
  let outer = "x = [uint] .ne c\nc = [bstr] .eq [h'FF']";
  both_validate(outer, "[1]", "8101");

  // The `.default` spellings apply the implied `.ne` like every other
  // composite default, in optional-member and `.cbor`-embedded position.
  let opt = "x = { ? v: ([bstr] .default [h'FF']) }";
  assert!(cbor_validates(opt, "a0"));
  assert!(!cbor_validates(opt, "a161768141ff"));
  assert!(cbor_validates(opt, "a161768141fe"));

  let embedded = "x = bstr .cbor y\ny = [bstr] .default [h'FF']";
  assert!(!cbor_validates(embedded, "438141ff"));
  assert!(cbor_validates(embedded, "438141fe"));
}

#[test]
fn self_referential_controllers_keep_the_left_hand_side_authoritative() {
  // Degenerate self-referential controller: the per-location recursion
  // guard trips with zero progress at the same data location, and the
  // encodings split on the matching instance (JSON rejects through the
  // aggregate `.ne` inversion; CBOR's silent-revisit policy accepts
  // vacuously). The left-hand side stays authoritative regardless: a
  // data item that is not a `[* uint]` at all rejects in both encodings
  // on the plain arity error before any recursion question arises.
  // Ruby 0.12.14 rejects `[[1]]` in both encodings; its deep-recursion
  // abort on the matching `[1]` leaves that cell without an oracle.
  let schema = "x = [* uint] .ne x";
  assert!(!json_validates(schema, "[1]"));
  assert!(cbor_validates(schema, "8101"));
  both_reject(schema, "[[1]]", "818101");
}
