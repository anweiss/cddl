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
