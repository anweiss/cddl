#![cfg(feature = "std")]
#![cfg(not(feature = "lsp"))]

#[cfg(feature = "cbor")]
fn check_cbor(schema: &str, hex: &str, expected: bool) {
  let bytes = hex::decode(hex).unwrap();
  let result = cddl::validator::validate_cbor_from_slice(
    schema,
    &bytes,
    #[cfg(feature = "additional-controls")]
    None,
  );
  assert_eq!(
    result.is_ok(),
    expected,
    "schema: {schema}, CBOR: {hex}, result: {result:?}"
  );
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_match_each_alternative() {
  for schema in [
    "m = { * (int / tstr) => any }",
    "m = { * key => any }\nkey = int / tstr",
    "m = { * key => any }\nkey = name\nname = int / tstr",
    "m = table<(int / tstr)>\ntable<K> = { * K => any }",
    "m = { * key => any }\nkey = int\nkey /= tstr",
    "m = { * key => any }\nkey /= int\nkey /= tstr",
  ] {
    for hex in ["a0", "a10301", "a12001", "a1617801", "a303012002617803"] {
      check_cbor(schema, hex, true);
    }
    for hex in ["a1f501", "a1417801", "a20301f501"] {
      check_cbor(schema, hex, false);
    }
  }
  for hex in ["a10301", "a12001", "a203012001"] {
    check_cbor("m = { * (uint / nint) => any }", hex, true);
  }
  check_cbor("m = { * (uint / nint) => any }", "a1617801", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_preserve_occurrences() {
  for occurrence in ["", "?", "+", "1*1"] {
    let schema = format!("m = {{ {occurrence} (int / tstr) => uint }}");
    for hex in ["a10301", "a12001", "a1617801"] {
      check_cbor(&schema, hex, true);
    }
    check_cbor(&schema, "a0", occurrence == "?");
    check_cbor(&schema, "a20301617801", occurrence == "+");
    check_cbor(&schema, "a1036178", false);
  }
  check_cbor("m = { 2*2 (int / tstr) => uint }", "a20301617801", true);
  check_cbor("m = { 2*2 (int / uint) => uint }", "a10301", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_preserve_pair_ownership() {
  check_cbor(
    "m = { * (int / tstr) => uint, extra: tstr }",
    "a26565787472616268690301",
    true,
  );
  check_cbor(
    "m = { * (int / tstr) => uint, * (int / tstr) => tstr }",
    "a2030161786179",
    true,
  );
  check_cbor(
    "m = { * (int / tstr) => uint, extra: uint }",
    "a165657874726101",
    false,
  );
  check_cbor("m = { * (int / tstr) => uint }", "a161786179", false);
  check_cbor(
    "m = { (int / tstr) => any, (int / tstr) => uint }",
    "a2030161786179",
    true,
  );
  check_cbor(
    "m = { (int / tstr) ^ => uint, * any => tstr }",
    "a161786179",
    false,
  );
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_named_choice_keys_enforce_controls() {
  let schema = "m = { * unknownExtension }\nunknownExtension = (name => any)\nname = int / tstr .size (1..255)";
  for hex in ["a10301", "a12001", "a1617801", "a20301617801"] {
    check_cbor(schema, hex, true);
  }
  check_cbor(schema, "a16001", false);
  check_cbor(schema, &format!("a1790100{}01", "78".repeat(256)), false);
  check_cbor(schema, &format!("a178ff{}01", "78".repeat(255)), true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_check_each_alternative_domain() {
  // `{3: 1}`, `{-1: 1}` and `{2(h'01'): 1}`
  check_cbor("m = { * (nint / tstr) => any }", "a10301", false);
  check_cbor("m = { * (nint / tstr) => any }", "a12001", true);
  check_cbor("m = { * (uint / tstr) => any }", "a12001", false);
  check_cbor("m = { * (int / tstr) => any }", "a1c2410101", false);
  check_cbor("m = { * (biguint / tstr) => any }", "a1c2410101", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_match_complete_pairs() {
  // `{"a": "x", "b": 1}`: the first member must take "b", not "a".
  check_cbor(
    "m = { (int / tstr) => uint, * any => tstr }",
    "a261616178616201",
    true,
  );
  // `{"x": "y"}`: an optional member leaves a failing pair to later members.
  check_cbor(
    "m = { ? (int / tstr) => uint, * any => tstr }",
    "a161786179",
    true,
  );
  // A cut commits the matching key, so its failing value rejects the map.
  for schema in [
    "m = { * (int / tstr) ^ => uint, * any => tstr }",
    "m = { (int / tstr) ^ => uint, * any => tstr }",
    "m = { * (\"x\" / \"y\") ^ => uint, * any => tstr }",
  ] {
    check_cbor(schema, "a161786179", false);
  }
  check_cbor(
    "m = { * (int / tstr) ^ => uint, * any => tstr }",
    "a1617801",
    true,
  );
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_type_choice_keys_match_and_enforce_constraints() {
  for schema in [
    "m = { * (int / tstr .size (1..3)) => uint }",
    "m = { * key => uint }\nkey = int / tstr .size (1..3)",
    "m = { * key => uint }\nkey = int\nkey /= tstr .size (1..3)",
  ] {
    for (json, expected) in [
      ("{}", true),
      (r#"{"x":1,"abc":2}"#, true),
      (r#"{"":1}"#, false),
      (r#"{"abcd":1}"#, false),
      (r#"{"x":"bad"}"#, false),
    ] {
      let result = cddl::validator::validate_json_from_str(
        schema,
        json,
        #[cfg(feature = "additional-controls")]
        None,
      );
      assert_eq!(
        result.is_ok(),
        expected,
        "schema: {schema}, JSON: {json}, result: {result:?}"
      );
    }
  }
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_type_choice_keys_preserve_occurrences_and_values() {
  for (schema, json, expected) in [
    ("m = { (int / tstr) => uint }", r#"{"x":1}"#, true),
    ("m = { (int / tstr) => uint }", "{}", false),
    ("m = { ? (int / tstr) => uint }", "{}", true),
    ("m = { + (int / tstr) => uint }", "{}", false),
    ("m = { 2*2 (int / tstr) => uint }", r#"{"x":1,"y":2}"#, true),
    ("m = { 2*2 (int / tstr) => uint }", r#"{"x":1}"#, false),
    (
      "m = { 1*1 (int / tstr) => uint }",
      r#"{"x":1,"y":2}"#,
      false,
    ),
    (
      "m = { * (int / tstr) => uint, extra: tstr }",
      r#"{"x":1,"extra":"y"}"#,
      true,
    ),
  ] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(
      result.is_ok(),
      expected,
      "schema: {schema}, JSON: {json}, result: {result:?}"
    );
  }
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_type_choice_keys_match_complete_pairs() {
  for (schema, json, expected) in [
    (
      "m = { (int / tstr) => uint, * any => tstr }",
      r#"{"a":"x","b":1}"#,
      true,
    ),
    (
      "m = { ? (int / tstr) => uint, * any => tstr }",
      r#"{"x":"y"}"#,
      true,
    ),
    (
      "m = { * (int / tstr) ^ => uint, * any => tstr }",
      r#"{"x":"y"}"#,
      false,
    ),
    (
      "m = { * (\"x\" / \"y\") ^ => uint, * any => tstr }",
      r#"{"x":"y"}"#,
      false,
    ),
    (
      "m = { * (int / tstr) ^ => uint, * any => tstr }",
      r#"{"x":1}"#,
      true,
    ),
  ] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(
      result.is_ok(),
      expected,
      "schema: {schema}, JSON: {json}, result: {result:?}"
    );
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_check_singleton_and_alias_domains() {
  // Tagged keys are not booleans, and sibling aliases expand fully:
  // with `a = nint` and `b = a`, both alternatives reject `{3: 1}`.
  check_cbor("m = { * (true / false) => any }", "a1c2410101", false);
  check_cbor("m = { * (true / false) => any }", "a1f501", true);
  let alias = "m = { * (a / b) => any }\na = nint\nb = a";
  check_cbor(alias, "a10301", false);
  check_cbor(alias, "a12001", true);
  check_cbor(alias, "a1617801", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_reassign_claimed_pairs() {
  // `{"x": 1, 3: 2}`: the second member needs "x", so the first takes 3.
  let schema = "m = { (int / tstr) => any, (\"x\" / \"y\") => uint }";
  check_cbor(schema, "a26178010302", true);
  check_cbor(schema, "a2617861780302", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_support_structured_keys_and_nested_values() {
  // `{{"b": 1}: 1}`: a structured map key can match one alternative.
  check_cbor(
    "m = { * ({ tstr => uint } / tstr) => any }",
    "a1a161620101",
    true,
  );
  check_cbor(
    "m = { * ({ tstr => uint } / tstr) => any }",
    "a1a16162617801",
    false,
  );
  // `{"x": {"y": 1}}`: primitive keys keep working inside a map value.
  check_cbor(
    "m = { (int / tstr) => { tstr => uint } }",
    "a16178a1617901",
    true,
  );
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_type_choice_keys_reassign_claimed_pairs() {
  // `{"a": 1, "b": "x"}`: the second member needs "a", so the first
  // takes "b". No assignment satisfies `{"a": "x", "b": "y"}`.
  let schema = "m = { (int / tstr) => any, (int / tstr) => uint }";
  for (json, expected) in [
    (r#"{"a":1,"b":"x"}"#, true),
    (r#"{"a":"x","b":"y"}"#, false),
  ] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(
      result.is_ok(),
      expected,
      "schema: {schema}, JSON: {json}, result: {result:?}"
    );
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_resolve_constrained_alias_domains() {
  // A constrained alias inherits its underlying key domain, and the control
  // operator is still enforced by the probe.
  let negative = "m = { * (negative .ne 0 / tstr) => any }\nnegative = nint";
  check_cbor(negative, "a10301", false);
  check_cbor(negative, "a12001", true);
  check_cbor(negative, "a1617801", true);
  let nonzero = "m = { * (nonzero / tstr) => any }\nnonzero = uint .ne 0";
  check_cbor(nonzero, "a10001", false);
  check_cbor(nonzero, "a10301", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_restrict_time_and_tdate_tags() {
  // `time` keys admit tag 1 only, `tdate` keys tag 0 only; other tags are
  // not timestamps or dates.
  check_cbor("m = { * (time / tstr) => any }", "a1c2410101", false);
  check_cbor("m = { * (time / tstr) => any }", "a1c11a0000000101", true);
  check_cbor("m = { * (tdate / tstr) => any }", "a1c2410101", false);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_type_choice_keys_reassign_through_chains() {
  // Four members, four pairs: only the assignment "c", "d", "b", "a" works,
  // which requires moving a pair claimed by a member satisfied through an
  // earlier reassignment.
  let schema = "m = { (int / tstr) => uint, (\"b\" / \"d\") => uint, (\"a\" / \"b\") => uint, (\"a\" / \"z\") => uint }";
  for (json, expected) in [
    (r#"{"a":1,"b":1,"c":1,"d":1}"#, true),
    (r#"{"a":1,"b":1,"c":1}"#, false),
  ] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(
      result.is_ok(),
      expected,
      "schema: {schema}, JSON: {json}, result: {result:?}"
    );
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_resolve_generic_alias_domains() {
  // A generic alias instantiation inherits the domain of its argument.
  let schema = "m = { * (key<nint> / tstr) => any }\nkey<T> = T";
  check_cbor(schema, "a10301", false);
  check_cbor(schema, "a12001", true);
  check_cbor(schema, "a1617801", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_check_multi_alternative_alias_domains() {
  // Every underlying alternative constrains the domain, including literals;
  // the control operator itself is still enforced by the probe.
  let schema = "m = { * (nonpositive .ne 0 / tstr) => any }\nnonpositive = nint / 0";
  check_cbor(schema, "a10301", false);
  check_cbor(schema, "a12001", true);
  check_cbor(schema, "a10001", false);
  check_cbor(schema, "a1617801", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_restrict_tagged_prelude_domains() {
  // Tagged prelude types admit only their declared tag with a payload in the
  // declared payload domain.
  check_cbor("m = { * (uri / tstr) => any }", "a1c2410101", false);
  check_cbor("m = { * (uri / tstr) => any }", "a1d8206361626301", true);
  check_cbor("m = { * (b64url / tstr) => any }", "a1d5616b01", false);
  check_cbor("m = { * (b64url / tstr) => any }", "a1d8216361626301", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_expand_shared_aliases_once() {
  // 24 acyclic aliases, each doubling its predecessor, would produce 2^24
  // alternatives without memoized, deduplicated expansion and could exhaust
  // memory. With caching this validates instantly.
  let mut schema = String::from("m = { * k24 => any }\nk0 = int / tstr\n");
  for n in 1..=24 {
    schema.push_str(&format!("k{n} = k{} / k{}\n", n - 1, n - 1));
  }
  check_cbor(&schema, "a0", true);
  check_cbor(&schema, "a10301", true);
  check_cbor(&schema, "a1617801", true);
  check_cbor(&schema, "a1f501", false);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_type_choice_keys_reassign_plain_member_claims() {
  // The pair claimed by a plain `tstr => any` member may move to satisfy a
  // later choice-key member.
  for (json, expected) in [
    (r#"{"a":1,"b":"x"}"#, true),
    (r#"{"a":1}"#, false),
    (r#"{"a":"x","b":2}"#, true),
  ] {
    let result = cddl::validator::validate_json_from_str(
      "m = { tstr => any, (int / tstr) => uint }",
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), expected, "JSON: {json}, result: {result:?}");
  }
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_type_choice_keys_reassign_generic_group_claims() {
  // Claims made inside a generic group stay movable at the parent level
  // under their captured bindings.
  let schema = "m = { g<(int / tstr)>, (\"a\" / \"z\") => uint }\ng<K> = (K => any)";
  for (json, expected) in [(r#"{"a":1,"b":"x"}"#, true), (r#"{"a":1}"#, false)] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), expected, "JSON: {json}, result: {result:?}");
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_reassign_generic_group_claims() {
  let schema = "m = { g<(int / tstr)>, (\"a\" / \"z\") => uint }\ng<K> = (K => any)";
  check_cbor(schema, "a261610161626178", true);
  check_cbor(schema, "a1616101", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_resolve_nested_generic_aliases() {
  // Generic arguments resolve in the caller's environment: `inner<T>` inside
  // `outer<T>` binds T to nint, not to itself.
  let schema = "m = { * (outer<nint> / tstr) => any }\nouter<T> = inner<T>\ninner<T> = T";
  check_cbor(schema, "a1617801", true);
  check_cbor(schema, "a12001", true);
  check_cbor(schema, "a10301", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_admit_range_domains() {
  // A range alternative admits its whole interval, reached directly or
  // through an alias; the probe enforces the bounds.
  check_cbor("m = { * (1..3 / tstr) => any }", "a10201", true);
  check_cbor("m = { * (1..3 / tstr) => any }", "a10501", false);
  check_cbor(
    "m = { * (small / tstr) => any }\nsmall = 1..3",
    "a10201",
    true,
  );
  check_cbor(
    "m = { * (small / tstr) => any }\nsmall = 1..3",
    "a10501",
    false,
  );
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_include_generic_alias_extensions() {
  // `/=` extensions of a generic alias contribute to the key domain under
  // the same bindings.
  let schema = "m = { * (key<uint> / tstr) => any }\nkey<T> = T\nkey<T> /= bool";
  check_cbor(schema, "a1f501", true);
  check_cbor(schema, "a10301", true);
  check_cbor(schema, "a12001", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_memoize_constrained_alias_domains() {
  // A constrained alternative walks the alias DAG itself, so the domain walk
  // memoizes per name: 24 doubling aliases reject {3: 1} instantly instead of
  // exploring 2^24 branches.
  let mut schema = String::from("m = { * (k24 .ne 0 / tstr) => any }\nk0 = nint\n");
  for n in 1..=24 {
    schema.push_str(&format!("k{n} = k{} / k{}\n", n - 1, n - 1));
  }
  check_cbor(&schema, "a10301", false);
  // The accept path probes core choice validation, which stays exponential
  // for deep alias DAGs independent of this guard; keep it shallow here.
  let mut shallow = String::from("m = { * (k3 .ne 0 / tstr) => any }\nk0 = nint\n");
  for n in 1..=3 {
    shallow.push_str(&format!("k{n} = k{} / k{}\n", n - 1, n - 1));
  }
  check_cbor(&shallow, "a12001", true);
  check_cbor(&shallow, "a10001", false);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_generic_groups_inherit_parent_claims() {
  // The generic-group child shares the parent's ownership ledger, so two
  // required members need distinct pairs.
  let schema = "m = { (int / tstr) => any, g<(int / tstr)> }\ng<K> = (K => any)";
  for (json, expected) in [(r#"{"a":1}"#, false), (r#"{"a":1,"b":2}"#, true)] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), expected, "JSON: {json}, result: {result:?}");
  }
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_plain_members_trigger_reassignment() {
  // A failing plain member retries complete-pair assignment too: "b" moves
  // to the first member and "a" satisfies the uint member.
  let schema = "m = { (int / tstr) => any, tstr => uint }";
  for (json, expected) in [
    (r#"{"a":1,"b":"x"}"#, true),
    (r#"{"a":"x"}"#, false),
    (r#"{"a":1}"#, false),
  ] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), expected, "JSON: {json}, result: {result:?}");
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_match_range_domains_by_key_kind() {
  // A numeric range admits only numeric keys (byte-string lengths would
  // otherwise compare as numbers); the probe still enforces the interval.
  check_cbor("m = { * (1..3 / tstr) => any }", "a142010201", false);
  check_cbor("m = { * (1..3 / tstr) => any }", "a10201", true);
  check_cbor(
    "m = { * (small / tstr) => any }\nsmall = 1..3",
    "a142010201",
    false,
  );
  check_cbor(
    "m = { * (small / tstr) => any }\nsmall = 1..3",
    "a10201",
    true,
  );
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_require_the_declared_tag() {
  // time is #6.1(number): an untagged number matches neither alternative.
  check_cbor("m = { * (time / tstr) => any }", "a10101", false);
  check_cbor("m = { * (time / tstr) => any }", "a1c10101", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_expand_generic_choice_arguments() {
  // A generic instantiation whose argument is a choice is a choice key:
  // each pair is matched against the substituted alternatives.
  let schema = "m = { * key<(int / tstr)> => any }\nkey<K> = K";
  check_cbor(schema, "a10301", true);
  check_cbor(schema, "a1617801", true);
  check_cbor(schema, "a1f501", false);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_type_choice_keys_expand_generic_choice_arguments() {
  let schema = "m = { * key<(int / tstr)> => any }\nkey<K> = K";
  for (json, expected) in [(r#"{"x":1}"#, true), (r#"{"x":"y"}"#, true)] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), expected, "JSON: {json}, result: {result:?}");
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_socket_prefixed_aliases_do_not_share_domain_cache() {
  // `$s` and `s` are different identifiers: neither the expansion memo nor
  // the domain walk may key them by the bare name.
  let schema = "m = { * (s / $s) => any }\ns = nint\n$s /= tstr";
  check_cbor(schema, "a1617801", true);
  check_cbor(schema, "a12001", true);
  check_cbor(schema, "a10301", false);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_socket_prefixed_aliases_do_not_share_domain_cache() {
  let schema = "m = { * (s / $s) => any }\ns = nint\n$s /= tstr";
  let result = cddl::validator::validate_json_from_str(
    schema,
    r#"{"x":1}"#,
    #[cfg(feature = "additional-controls")]
    None,
  );
  assert!(result.is_ok(), "result: {:?}", result);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_group_choice_arms_do_not_share_claims() {
  // A failed // arm must not leave its validated keys or member claims
  // behind for the next arm.
  let schema = "m = { (int / tstr) => uint, required: null // (int / tstr) => tstr }";
  for (json, expected) in [
    (r#"{"a":1,"b":"x"}"#, false),
    (r#"{"a":"x"}"#, true),
    (r#"{"a":1,"required":null}"#, true),
  ] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), expected, "JSON: {json}, result: {result:?}");
  }
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_cut_members_preserve_value_failures() {
  // A cut commits the member to the pair whose key it matched; the value
  // failure is reported, not retried away through reassignment.
  let schema = "m = { tstr ^ => uint, * any => tstr }";
  for (json, expected) in [(r#"{"a":"x","b":1}"#, false), (r#"{"a":1,"b":"x"}"#, true)] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), expected, "JSON: {json}, result: {result:?}");
  }
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_empty_string_key_is_not_a_reassignment_sentinel() {
  // One pair cannot satisfy two required members, even when its key is the
  // empty string; a real empty-string claim stays movable for valid maps.
  let schema = "m = { \"\": any, (int / tstr) => uint }";
  let result = cddl::validator::validate_json_from_str(
    schema,
    r#"{"":1}"#,
    #[cfg(feature = "additional-controls")]
    None,
  );
  assert!(result.is_err(), "result: {:?}", result);
  let schema = "m = { \"\": uint, (int / tstr) => tstr }";
  let result = cddl::validator::validate_json_from_str(
    schema,
    r#"{"":1,"a":"x"}"#,
    #[cfg(feature = "additional-controls")]
    None,
  );
  assert!(result.is_ok(), "result: {:?}", result);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_failed_group_arm_restores_moved_claims() {
  // The first arm moves the outer claim to "b" and fails; the retry must see
  // the original claim on "a" for the second arm to match.
  let schema = "m = { (int / tstr) => any, ((\"a\" / \"z\") => uint, required: null // (\"a\" / \"y\") => uint) }";
  for (json, expected) in [
    (r#"{"a":1,"b":"x"}"#, true),
    (r#"{"a":1,"required":null}"#, true),
    (r#"{"a":"x","b":1}"#, false),
  ] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), expected, "JSON: {json}, result: {result:?}");
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_resolve_named_range_endpoints() {
  // A named endpoint contributes its literal kind: `lo..hi` admits the whole
  // numeric domain, and the probe enforces the resolved bounds.
  let schema = "m = { * (lo..hi / tstr) => any }\nlo = 1\nhi = 3";
  check_cbor(schema, "a10201", true);
  check_cbor(schema, "a10501", false);
  check_cbor(schema, "a142010201", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_type_choice_keys_substitute_structured_generic_alternatives() {
  // Parameters inside arrays, maps and tagged payloads are substituted when
  // a generic choice key expands, so expanded alternatives never probe with
  // an unbound parameter.
  let schema = "m = { * key<uint> => any }\nkey<K> = [K] / tstr";
  check_cbor(schema, "a1810100", true);
  check_cbor(schema, "a181f500", false);
  check_cbor(schema, "a1617801", true);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_type_choice_keys_substitute_structured_generic_alternatives() {
  let schema = "m = { * key<uint> => any }\nkey<K> = [K] / tstr";
  for (json, expected) in [(r#"{"x":1}"#, true)] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), expected, "JSON: {json}, result: {result:?}");
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_cut_choice_key_claims_stay_committed() {
  // A cut member owns its matching pair; reassignment cannot move it to
  // another key to free the pair for a later member.
  let schema = "m = { (int / tstr) ^ => any, (\"x\" / \"y\") => uint }";
  check_cbor(schema, "a26178010302", false);
  check_cbor(schema, "a20301617802", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_recursive_generic_choice_keys_terminate() {
  let schema = "m = { * (key<nint> / tstr) => any }\nkey<T> = T / key<T>";
  check_cbor(schema, "a1617801", true);
  check_cbor(schema, "a12001", true);
  check_cbor(schema, "a10301", false);
  let schema = "m = { * key<nint> => any }\nkey<T> = T / key<T>";
  check_cbor(schema, "a12001", true);
  check_cbor(schema, "a1617801", false);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_recursive_generic_choice_keys_terminate() {
  let schema = "m = { * (key<nint> / tstr) => any }\nkey<T> = T / key<T>";
  for json in [r#"{"x":1}"#, "{}"] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(result.is_ok(), "JSON: {}, result: {:?}", json, result);
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_generic_choice_key_dag_expands_each_instantiation_once() {
  let mut schema = String::from("m = { * k24<uint> => any }\nk0<T> = T / tstr\n");
  for n in 1..=24 {
    schema.push_str(&format!("k{n}<T> = k{}<T> / k{}<T>\n", n - 1, n - 1));
  }
  check_cbor(&schema, "a0", true);
  check_cbor(&schema, "a10301", true);
  check_cbor(&schema, "a1617801", true);
  check_cbor(&schema, "a1f501", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_generic_choice_keys_substitute_control_operands() {
  let schema = "m = { * key<1> => any }\nkey<N> = tstr .size N / int";
  check_cbor(schema, "a1617800", true);
  check_cbor(schema, "a162787900", false);
  check_cbor(schema, "a10300", true);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_generic_choice_keys_substitute_control_operands() {
  let schema = "m = { * key<1> => any }\nkey<N> = tstr .size N / int";
  for (json, expected) in [(r#"{"x":0}"#, true), (r#"{"xy":0}"#, false)] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), expected, "JSON: {json}, result: {result:?}");
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_generic_choice_keys_substitute_constrained_arguments() {
  let schema = "m = { * key<uint .gt 0> => any }\nkey<K> = [K] / tstr";
  check_cbor(schema, "a1810100", true);
  check_cbor(schema, "a1810000", false);
  check_cbor(schema, "a1617800", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_argument_changing_generic_recursion_terminates() {
  let schema = "m = { * (key<nint> / tstr) => any }\nkey<T> = T / key<[T]>";
  check_cbor(schema, "a1617801", true);
  check_cbor(schema, "a12001", true);
  check_cbor(schema, "a1812001", true);
  check_cbor(schema, "a1f501", false);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_argument_changing_generic_recursion_terminates() {
  let schema = "m = { * (key<nint> / tstr) => any }\nkey<T> = T / key<[T]>";
  let result = cddl::validator::validate_json_from_str(
    schema,
    r#"{"x":1}"#,
    #[cfg(feature = "additional-controls")]
    None,
  );
  assert!(result.is_ok(), "result: {:?}", result);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_cut_member_satisfied_by_reassignment_stays_committed() {
  let schema = "m = { (int / tstr) => any, (int / tstr) => any, (\"x\" / \"y\") ^ => any, (\"x\" / \"u\") => uint }";
  // x:1, y:1, z:1, w:1
  check_cbor(schema, "a4617801617901617a01617701", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_undefined_choice_key_rejects_unrelated_tags() {
  let schema = "m = { * (undefined / tstr) => any }";
  check_cbor(schema, "a1c2410101", false);
  check_cbor(schema, "a1617801", true);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_choice_key_reassignment_and_cuts() {
  for (schema, json, expected) in [
    // A reassignment probe validates a recursive value at its own location.
    (
      "root = node\nnode = { (int / tstr) => any, (\"a\" / \"z\") => node } / null",
      r#"{"a":null,"b":"x"}"#,
      true,
    ),
    // A cut member that has not matched a key may still take part in the
    // retry.
    (
      "m = { (int / tstr) => any, (\"x\" / \"y\") ^ => uint }",
      r#"{"x":1,"z":"s"}"#,
      true,
    ),
    (
      "m = { (int / tstr) => any, (\"x\" / \"y\") ^ => uint }",
      r#"{"x":"s","z":"s"}"#,
      false,
    ),
  ] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(
      result.is_ok(),
      expected,
      "{schema} JSON: {json}, result: {result:?}"
    );
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_cut_member_without_a_match_may_be_reassigned() {
  let schema = "m = { (int / tstr) => any, (\"x\" / \"y\") ^ => uint }";
  check_cbor(schema, "a2617801617a6173", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_control_over_choice_key_matches_each_key() {
  let schema = "m = { * key => any }\nkey = (int / tstr) .and any";
  check_cbor(schema, "a1617801", true);
  check_cbor(schema, "a10301", true);
  check_cbor(schema, "a1f501", false);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_control_over_choice_key_matches_each_key() {
  let schema = "m = { * key => any }\nkey = (int / tstr) .and any";
  let result = cddl::validator::validate_json_from_str(
    schema,
    r#"{"x":1}"#,
    #[cfg(feature = "additional-controls")]
    None,
  );
  assert!(result.is_ok(), "result: {:?}", result);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_generic_instantiations_keep_argument_types_distinct() {
  let schema = "m = { * (key<1> / key<1.0>) => any }\nkey<K> = K";
  check_cbor(schema, "a1f93c0001", true);
  check_cbor(schema, "a10101", true);
  check_cbor(schema, "a10201", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_socket_names_are_not_generic_parameters() {
  let schema = "m = { * key<uint> => any }\nkey<K> = K / $K\n$K /= tstr";
  check_cbor(schema, "a1617801", true);
  check_cbor(schema, "a10301", true);
  check_cbor(schema, "a1f501", false);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_socket_names_are_not_generic_parameters() {
  let schema = "m = { * key<uint> => any }\nkey<K> = K / $K\n$K /= tstr";
  let result = cddl::validator::validate_json_from_str(
    schema,
    r#"{"x":1}"#,
    #[cfg(feature = "additional-controls")]
    None,
  );
  assert!(result.is_ok(), "result: {:?}", result);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_generic_choice_keys_substitute_inline_group_choices() {
  let schema = "m = { * key<uint> => any }\nkey<K> = &(kind: K) / tstr";
  check_cbor(schema, "a10100", true);
  check_cbor(schema, "a1617800", true);
  check_cbor(schema, "a1f500", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_choice_key_in_optional_group_is_required_when_group_present() {
  let schema = "m = { ? ((int / tstr) => uint, flag: bool) }";
  // {"flag": true}
  check_cbor(schema, "a164666c6167f5", false);
  check_cbor(schema, "a261780164666c6167f5", true);
  check_cbor(schema, "a0", true);
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_choice_key_round_nine_cases() {
  for (schema, json, expected) in [
    // A member inside a present optional group is still required.
    (
      "m = { ? ((int / tstr) => uint, flag: bool) }",
      r#"{"flag":true}"#,
      false,
    ),
    (
      "m = { ? ((int / tstr) => uint, flag: bool) }",
      r#"{"x":1,"flag":true}"#,
      true,
    ),
    ("m = { ? ((int / tstr) => uint, flag: bool) }", "{}", true),
    // A claim captured without generic bindings keeps the global meaning
    // of its names when rechecked inside a generic group.
    (
      "m = { X => any, g<(int / tstr)> }\nX = \"a\" / \"b\"\ng<X> = (X => uint)",
      r#"{"a":1,"z":"x"}"#,
      false,
    ),
    (
      "m = { X => any, g<(int / tstr)> }\nX = \"a\" / \"b\"\ng<X> = (X => uint)",
      r#"{"a":"x","z":1}"#,
      true,
    ),
    // Each use of a generic group validates with its own arguments.
    (
      "m = { g<any>, g<uint> }\ng<V> = ((int / tstr) => V)",
      r#"{"a":"x","b":"y"}"#,
      false,
    ),
    (
      "m = { g<any>, g<uint> }\ng<V> = ((int / tstr) => V)",
      r#"{"a":"x","b":1}"#,
      true,
    ),
    // An optional member that matched nothing may win a held pair.
    (
      "m = { (int / tstr) => any, ? (\"a\" / \"z\") => uint }",
      r#"{"a":1,"b":"x"}"#,
      true,
    ),
    (
      "m = { (int / tstr) => any, ? (\"a\" / \"z\") => uint }",
      r#"{"a":1}"#,
      true,
    ),
  ] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(
      result.is_ok(),
      expected,
      "{schema} JSON: {json}, result: {result:?}"
    );
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_optional_choice_member_may_win_a_held_pair() {
  let schema = "m = { (int / tstr) => any, ? (\"a\" / \"z\") => uint }";
  // {"a": 1, "b": "x"}
  check_cbor(schema, "a261610161626178", true);
  // {"a": 1}
  check_cbor(schema, "a1616101", true);
  // {"a": "x", "b": "x"}
  check_cbor(schema, "a26161617861626178", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_controlled_choice_keys_check_each_alternative_whole() {
  let schema = "m = { * key => any }\nkey = (nint / tstr) .and any";
  // {3: 1}: neither alternative admits 3.
  check_cbor(schema, "a10301", false);
  // {-1: 1} and {"x": 1}
  check_cbor(schema, "a12001", true);
  check_cbor(schema, "a1617801", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_wrapping_generic_choice_keys_follow_key_depth() {
  let schema = "m = { * key<nint> => any }\nkey<T> = T / key<[T]>";
  // A key of eight nested arrays around -1.
  check_cbor(schema, "a181818181818181812001", true);
  // Twenty nested arrays around -1.
  check_cbor(schema, &format!("a1{}2001", "81".repeat(20)), true);
  // Eight nested arrays around 1, which is not nint.
  check_cbor(schema, "a181818181818181810100", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_branching_generic_choice_keys_stay_bounded_for_deep_keys() {
  let schema = "m = { * key<nint> => any }\nkey<T> = T / key<[T]> / key<[T, T]>";
  check_cbor(schema, "a1812001", true);
  // Thirty nested arrays around a text string: no alternative matches, and
  // the expansion budget keeps the rejection fast.
  check_cbor(schema, &format!("a1{}617800", "81".repeat(30)), false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_reassigned_pairs_validate_nested_values() {
  let schema = "root = node\nnode = { (int / tstr) => any, (\"a\" / \"z\") => node } / null";
  // {"a": {"a": false, "b": "x"}, "b": "x"}: false is not a node.
  check_cbor(schema, "a26161a26161f46162617861626178", false);
  // {"a": {"a": null, "b": "x"}, "b": "x"}
  check_cbor(schema, "a26161a26161f66162617861626178", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_recursive_choice_keys_probe_at_their_own_location() {
  let schema = "root = node\nnode = { * (node .and any / int) => uint } / tstr";
  check_cbor(schema, "a1f501", false);
  check_cbor(schema, "a1617801", true);
  check_cbor(schema, "a10101", true);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_controlled_recursive_generic_choice_keys_terminate() {
  let schema = "m = { * (key<nint> .and any / tstr) => any }\nkey<T> = T / key<[T]>";
  check_cbor(schema, "a1617801", true);
  check_cbor(schema, "a1812001", true);
  check_cbor(schema, "a1810101", false);
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_generic_choice_keys_substitute_group_choice_arguments() {
  let schema = "m = { * key<1> => any }\nkey<K> = &choices<K> / tstr\nchoices<T> = (kind: T)";
  check_cbor(schema, "a10100", true);
  check_cbor(schema, "a1617800", true);
  check_cbor(schema, "a10200", false);
}

#[cfg(all(feature = "cbor", feature = "additional-controls"))]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_computed_control_choice_keys_check_the_result() {
  let schema = "m = { * (1 .plus 2 / tstr) => any }";
  check_cbor(schema, "a10300", true);
  check_cbor(schema, "a10400", false);
  check_cbor(schema, "a1617800", true);
  for schema in [
    "m = { * (\"a\" .cat \"b\" / int) => any }",
    "m = { * (\"a\" .det \"b\" / int) => any }",
  ] {
    check_cbor(schema, "a162616200", true);
    check_cbor(schema, "a162616300", false);
    check_cbor(schema, "a10100", true);
  }
}

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_choice_key_round_ten_cases() {
  for (schema, json, expected) in [
    // A key probe validates at its own location, not the object's.
    (
      "root = node\nnode = { * (node .and any / int) => uint } / tstr",
      r#"{"x":1}"#,
      true,
    ),
    (
      "root = node\nnode = { * (node .and any / int) => uint } / tstr",
      r#"{"x":"y"}"#,
      false,
    ),
    // A controlled recursive generic stays under the expansion guards.
    (
      "m = { * (key<nint> .and any / tstr) => any }\nkey<T> = T / key<[T]>",
      r#"{"x":1}"#,
      true,
    ),
    (
      "m = { * key => any }\nkey = (nint / tstr) .and any",
      r#"{"x":1}"#,
      true,
    ),
  ] {
    let result = cddl::validator::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(
      result.is_ok(),
      expected,
      "{schema} JSON: {json}, result: {result:?}"
    );
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_array_choice_keys_check_items_independently() {
  // A variable-width entry shifts the positions of the items after it.
  let schema = "m = { * ([* uint, tstr, * uint] / tstr) => any }";
  check_cbor(schema, "a1836178010200", true);
  check_cbor(schema, "a182010200", false);
  let schema = "m = { * ([uint, tstr] / int) => any }";
  check_cbor(schema, "a18201617800", true);
  check_cbor(schema, "a182010100", false);
  // Each item starts its own domain walk.
  let schema = "m = { * ([a / tstr, a] / tstr) => any }\na = nint";
  check_cbor(schema, "a18261782000", true);
  check_cbor(schema, "a18261780100", false);
  let schema = "m = { * a => any }\na = [a] / uint";
  check_cbor(schema, "a181810100", true);
  check_cbor(schema, "a181812000", false);
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn fixed_keys_take_pairs_from_movable_claims() {
  for (schema, json, hex, expected) in [
    (
      "m = { (int / tstr) => any, a: uint }",
      r#"{"a":1,"b":"x"}"#,
      "a261610161626178",
      true,
    ),
    (
      "m = { (int / tstr) => any, a: uint }",
      r#"{"b":"x","a":1}"#,
      "a261626178616101",
      true,
    ),
    (
      "m = { tstr => any, a: uint }",
      r#"{"a":1,"b":"x"}"#,
      "a261610161626178",
      true,
    ),
    // The other pair does not satisfy the choice member.
    (
      "m = { (int / tstr) => uint, a: uint }",
      r#"{"a":1,"b":"x"}"#,
      "a261610161626178",
      false,
    ),
    // A cut commits the first matching pair.
    (
      "m = { (int / tstr) ^ => any, a: uint }",
      r#"{"a":1,"b":"x"}"#,
      "a261610161626178",
      false,
    ),
    (
      "m = { tstr ^ => any, a: uint }",
      r#"{"a":1,"b":"x"}"#,
      "a261610161626178",
      false,
    ),
    (
      "m = { tstr ^ => any, (\"a\" / \"z\") => uint }",
      r#"{"a":1,"b":"x"}"#,
      "a261610161626178",
      false,
    ),
  ] {
    #[cfg(feature = "json")]
    {
      let result = cddl::validator::validate_json_from_str(
        schema,
        json,
        #[cfg(feature = "additional-controls")]
        None,
      );
      assert_eq!(
        result.is_ok(),
        expected,
        "{schema} JSON: {json}, result: {result:?}"
      );
    }
    #[cfg(feature = "cbor")]
    check_cbor(schema, hex, expected);
    let _ = (json, hex);
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_map_choice_keys_check_member_domains() {
  let schema = "m = { * ({ a: nint } / tstr) => any }";
  check_cbor(schema, "a1a161612000", true);
  check_cbor(schema, "a1a161610100", false);
  check_cbor(schema, "a1a1616120616200", false);
  check_cbor(schema, "a1617800", true);
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn generic_choice_keys_expand_socket_arguments() {
  let schema = "m = { g<$K> }\ng<K> = ((K / int) => any)\n$K /= tstr";
  #[cfg(feature = "json")]
  {
    let result = cddl::validator::validate_json_from_str(
      schema,
      r#"{"x":1}"#,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(result.is_ok(), "{:?}", result);
  }
  #[cfg(feature = "cbor")]
  {
    check_cbor(schema, "a1617801", true);
    check_cbor(schema, "a10301", true);
    check_cbor(schema, "a1f501", false);
  }
  let _ = schema;
}
