#![cfg(feature = "json")]
#![cfg(not(feature = "lsp"))]

use cddl::validator::Validator;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test;

#[cfg(not(target_arch = "wasm32"))]
const _: fn() = || {
  fn assert_send_sync<T: Send + Sync>() {}
  assert_send_sync::<cddl::validator::json::JSONValidator<'static>>();
};

fn validates(schema: &str, instance: &str) -> bool {
  let cddl = cddl::pest_bridge::cddl_from_pest_str(schema).expect("valid CDDL fixture");
  let json = serde_json::from_str(instance).expect("valid JSON fixture");
  let mut validator = cddl::validator::json::JSONValidator::new(
    &cddl,
    json,
    #[cfg(feature = "additional-controls")]
    None,
  );
  let result = validator.validate();
  let valid = match &result {
    Ok(()) => true,
    Err(cddl::validator::json::Error::Validation(_)) => false,
    Err(error) => panic!("unexpected hard validation error: {}", error),
  };
  let exported = cddl::validator::validate_json_from_str(
    schema,
    instance,
    #[cfg(feature = "additional-controls")]
    None,
  );
  #[cfg(not(target_arch = "wasm32"))]
  assert!(
    matches!(
      &exported,
      Ok(()) | Err(cddl::validator::json::Error::Validation(_))
    ),
    "unexpected hard error from string API: {:?}",
    exported
  );
  #[cfg(target_arch = "wasm32")]
  if let (Err(expected), Err(actual)) = (&result, &exported) {
    assert_eq!(actual.as_string(), Some(expected.to_string()));
  }
  assert_eq!(exported.is_ok(), valid, "typed and string APIs differ");
  valid
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn optional_non_cut_members_leave_failed_pairs_for_later_members() {
  for occurrence in ["?", "0*1", "*1", "*", "0*2", "0*0"] {
    for key in [r#""extension""#, "tstr", "any"] {
      let schema = format!(
        "root = {{ {} {} => uint, * tstr => tstr }}",
        occurrence, key
      );
      assert!(validates(&schema, r#"{"extension":"value"}"#), "{}", schema);
      assert!(!validates(&schema, r#"{"extension":false}"#), "{}", schema);
    }
  }
  assert!(!validates(
    r#"root = { 0*0 "a" => uint, * tstr => tstr }"#,
    r#"{"a":1}"#,
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cut_and_required_members_do_not_skip_value_errors() {
  for member in [
    r#"? "extension" ^ => uint"#,
    "? tstr ^ => uint",
    "? extension: uint",
    r#"? "extension": uint"#,
    r#""extension" => uint"#,
    r#"1*2 "extension" => uint"#,
    "tstr => uint",
  ] {
    let schema = format!("root = {{ {}, * tstr => any }}", member);
    assert!(
      !validates(&schema, r#"{"extension":"value"}"#),
      "{}",
      schema
    );
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn optional_map_value_diagnostics_survive_only_for_unclaimed_pairs() {
  for (schema, input, location) in [
    (
      r#"root = { ? "port" => uint }"#,
      r#"{"port":"80"}"#,
      "/port",
    ),
    (
      r#"root = { g<uint> } g<T> = (? "port" => T)"#,
      r#"{"port":"80"}"#,
      "/port",
    ),
    (
      r#"root = { 0*2 "port" => uint }"#,
      r#"{"port":"80"}"#,
      "/port",
    ),
    ("root = { * (tstr => uint) }", r#"{"port":"80"}"#, "/port"),
    (
      r#"root = { ? (? "port" => uint, required: bool) }"#,
      r#"{"port":"80"}"#,
      "/port",
    ),
    (
      r#"root = { nested: { ? "port" => uint } }"#,
      r#"{"nested":{"port":"80"}}"#,
      "/nested/port",
    ),
    (
      r#"root = { ? "odd/key" => uint }"#,
      r#"{"odd/key":"80"}"#,
      "/odd/key",
    ),
  ] {
    let error = cddl::validator::validate_json_from_str(
      schema,
      input,
      #[cfg(feature = "additional-controls")]
      None,
    )
    .unwrap_err();
    #[cfg(not(target_arch = "wasm32"))]
    let message = error.to_string();
    #[cfg(target_arch = "wasm32")]
    let message = error.as_string().unwrap();
    assert!(
      message.contains("expected type uint"),
      "{}: {}",
      schema,
      message
    );
    assert!(message.contains(location), "{}: {}", schema, message);
    for rejected_key in ["port", "odd/key"] {
      assert!(
        !message.contains(&format!("unexpected key \"{rejected_key}\"")),
        "{}: {}",
        schema,
        message
      );
    }
  }
  for schema in [
    r#"root = { ? "port" => uint, port: tstr }"#,
    r#"root = { ? "port" => uint // port: tstr }"#,
    r#"root = { g<uint>, port: tstr } g<T> = (? "port" => T)"#,
    r#"root = { ? (? "port" => uint, required: bool), * tstr => tstr }"#,
  ] {
    assert!(validates(schema, r#"{"port":"80"}"#), "{}", schema);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn pruned_map_paths_do_not_report_unvisited_keys() {
  let message = |schema: &str, input: &str| {
    let error = cddl::validator::validate_json_from_str(
      schema,
      input,
      #[cfg(feature = "additional-controls")]
      None,
    )
    .unwrap_err();
    #[cfg(not(target_arch = "wasm32"))]
    let message = error.to_string();
    #[cfg(target_arch = "wasm32")]
    let message = error.as_string().unwrap();
    message
  };
  for (schema, input, location) in [
    (
      "root = { (a: uint // b: uint), c: uint, d: uint }",
      r#"{"a":1,"c":"x","d":1}"#,
      "/c",
    ),
    (
      "root = { g<uint> } g<T> = ((a: T // b: T), c: T, d: T)",
      r#"{"a":1,"c":"x","d":1}"#,
      "/c",
    ),
    (
      "root = { nested: { (a: uint // b: uint), c: uint, d: uint }, kept: uint }",
      r#"{"nested":{"a":1,"c":"x","d":1},"kept":1}"#,
      "/nested/c",
    ),
  ] {
    let error = message(schema, input);
    assert!(
      error.contains("expected type uint"),
      "{}: {}",
      schema,
      error
    );
    assert!(error.contains(location), "{}: {}", schema, error);
    assert!(
      !error.contains("unexpected key \"d\""),
      "{}: {}",
      schema,
      error
    );
  }
  for (schema, input) in [
    (
      "root = { c: uint, d: uint }",
      r#"{"c":"x","d":1,"extra":2}"#,
    ),
    (
      "root = { ? (a: uint, x: uint // b: uint, x: uint), d: uint, bad: bool }",
      r#"{"a":"x","b":"x","d":1,"bad":0,"extra":2}"#,
    ),
    (
      "root = { (a: bool, d: uint // a: uint, d: uint) }",
      r#"{"a":1,"d":1,"extra":2}"#,
    ),
  ] {
    let error = message(schema, input);
    assert!(
      error.contains("unexpected key \"extra\""),
      "{}: {}",
      schema,
      error
    );
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn repeated_transparent_alias_keys_preserve_bounds_and_claims() {
  let input = format!(
    "{{{}}}",
    (0..256)
      .map(|n| format!("\"key{n}\":1"))
      .collect::<Vec<_>>()
      .join(",")
  );
  for schema in [
    "root = { * (label => any) } label = tstr",
    "root = { * (label => uint) } label = next next = text",
    "root = { * fields } fields = (label => uint) label = any",
    "root = { * fields<uint> } fields<T> = (label => T) label = tstr",
  ] {
    assert!(validates(schema, &input), "{}", schema);
  }
  assert!(validates(
    "root = { 0*1 (label => uint), a: uint } label = tstr",
    r#"{"a":1,"b":2}"#,
  ));
  for (schema, input) in [
    (
      "root = { 0*1 (label => uint) } label = tstr",
      r#"{"a":1,"b":2}"#,
    ),
    ("root = { 0*0 (label => uint) } label = tstr", r#"{"a":1}"#),
    (
      r#"root = { * (label => uint) } label = tstr .regexp "^a$""#,
      r#"{"b":1}"#,
    ),
    (
      "root = { * fields<uint> } fields<label> = (label => uint) label = tstr",
      r#"{"a":1}"#,
    ),
    // Preserve the existing visitor's alias-binding result; this is a
    // compatibility check, not a claim about CDDL lexical scoping.
    (
      "root = { * fields<uint> } fields<key> = (label => uint) label = key key = tstr",
      r#"{"a":1}"#,
    ),
    (
      "root = { * (label => uint) } label = next next = label",
      r#"{"a":1}"#,
    ),
  ] {
    assert!(!validates(schema, input), "{}", schema);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn heterogeneous_claims_preserve_displacement_order_for_later_members() {
  let schema = "root = { tstr => (1 / bool), tstr => uint, b: uint, * tstr => uint }";
  assert!(validates(schema, r#"{"a":1,"b":2,"c":3,"d":true}"#));
  assert!(!validates(schema, r#"{"a":1,"b":2,"c":3,"d":"wrong"}"#));
  let mixed = r#"root = { 0*256 (tstr => uint), tstr => any, "k0000" => uint, * tstr => uint }"#;
  let input = format!(
    "{{{}}}",
    (0..258)
      .map(|key| format!("\"k{key:04}\":1"))
      .collect::<Vec<_>>()
      .join(",")
  );
  assert!(validates(mixed, &input));
  assert!(!validates(
    mixed,
    &input.replace("\"k0000\":1", "\"k0000\":\"wrong\""),
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn repeated_choice_key_groups_preserve_domains_bounds_and_claims() {
  let input = format!(
    "{{{}}}",
    (0..128)
      .map(|n| format!("\"key{n}\":1"))
      .collect::<Vec<_>>()
      .join(",")
  );
  for schema in [
    "root = { * fields } fields = (key => any) key = int / tstr",
    "root = { * ((int / tstr) => uint) }",
    "root = { * fields<uint> } fields<T> = (key => T) key = int / tstr",
    "root = { * fields<(int / tstr)> } fields<K> = (K => uint)",
  ] {
    assert!(validates(schema, &input), "{}", schema);
  }
  assert!(validates(
    "root = { 0*1 fields, a: uint } fields = (key => uint) key = int / tstr",
    r#"{"a":1,"b":2}"#,
  ));
  assert!(validates(
    r#"root = { * fields, * tstr => tstr } fields = (("a" / "b") => uint)"#,
    r#"{"a":1,"b":2,"c":"text"}"#,
  ));
  for (schema, input) in [
    (
      "root = { 0*1 fields } fields = (key => uint) key = int / tstr",
      r#"{"a":1,"b":2}"#,
    ),
    (
      "root = { 0*0 fields } fields = (key => uint) key = int / tstr",
      r#"{"a":1}"#,
    ),
    (
      r#"root = { * fields } fields = (("a" / "b") => uint)"#,
      r#"{"c":1}"#,
    ),
    (
      "root = { * fields<uint> } fields<key> = (key => uint) key = int / tstr",
      r#"{"a":1}"#,
    ),
  ] {
    assert!(!validates(schema, input), "{}", schema);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_fallthrough_does_not_change_array_sequence_matching() {
  assert!(validates(r#"root = [ ? "a" => uint, bool ]"#, "[true]"));
  assert!(validates(r#"root = [ ? "a" => uint, bool ]"#, "[1,true]"));
  assert!(!validates(
    r#"root = [ ? "a" => uint, bool ]"#,
    r#"["wrong",true]"#,
  ));
  assert!(validates(
    "root = [ (bool // int, tstr) ]",
    r#"[1,"value"]"#,
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn replay_handles_different_widths_after_optional_group_rollback() {
  let schema = r#"
    root = {
      ? (x: uint / bool // y: uint / bool // z: uint / bool),
      ((a: uint // b: uint // c: uint) // (d: uint // e: uint)),
      ? x: tstr,
      tail: bool
    }
  "#;
  for key in ["a", "b", "c", "d", "e"] {
    assert!(validates(schema, &format!(r#"{{"{key}":1,"tail":true}}"#)));
    assert!(validates(
      schema,
      &format!(r#"{{"x":1,"{key}":1,"tail":true}}"#),
    ));
    assert!(validates(
      schema,
      &format!(r#"{{"x":"text","{key}":1,"tail":true}}"#),
    ));
    assert!(!validates(
      schema,
      &format!(r#"{{"{key}":"wrong","tail":true}}"#),
    ));
    assert!(!validates(schema, &format!(r#"{{"{key}":1}}"#)));
  }
  assert!(!validates(schema, r#"{"unknown":1,"tail":true}"#));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn failed_replay_reports_each_diagnostic_once() {
  for (schema, json, count) in [
    (
      "root = { (a: uint / bool // b: uint / bool) }",
      r#"{"a":1,"b":1,"unexpected":1}"#,
      1,
    ),
    (
      "root = { (? a: uint // ? b: uint), (? c: uint // ? d: uint) }",
      r#"{"unexpected":1}"#,
      1,
    ),
    (
      "root = { left: { ? a: uint }, right: { ? a: uint } }",
      r#"{"left":{"unexpected":1},"right":{"unexpected":1}}"#,
      2,
    ),
  ] {
    assert!(!validates(schema, json));
    #[cfg(feature = "additional-controls")]
    let error = cddl::validator::validate_json_from_str(schema, json, None).unwrap_err();
    #[cfg(not(feature = "additional-controls"))]
    let error = cddl::validator::validate_json_from_str(schema, json).unwrap_err();
    #[cfg(not(target_arch = "wasm32"))]
    let message = error.to_string();
    #[cfg(target_arch = "wasm32")]
    let message = error.as_string().expect("validation error message");
    assert_eq!(
      message.matches("unexpected key \"unexpected\"").count(),
      count
    );
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn generic_group_replay_rejects_cycles_but_preserves_productive_calls() {
  let recursive = r#"root = { g<uint> } g<T> = (? "a" => T // g<T>)"#;
  assert!(!validates(recursive, r#"{"extra":null}"#));
  assert!(validates(recursive, "{}"));
  assert!(validates(recursive, r#"{"a":1}"#));
  assert!(validates(
    "root = { g<uint> } g<T> = (g<T> // a: T)",
    r#"{"a":1}"#,
  ));
  assert!(validates(
    r#"root = { g<uint> } g<T> = (? "a" => T // g<tstr>)"#,
    r#"{"a":"value"}"#,
  ));
  assert!(validates(
    "root = { g<uint> } g<T> = (tstr => T, ? g<T>)",
    r#"{"a":1,"b":2}"#,
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn mixed_group_recursion_recognizes_consumed_map_pairs() {
  let schema = "root = { g<uint> } g<T> = (tstr => T, ? h) h = (g<uint>,)";
  assert!(validates(schema, r#"{"a":1,"b":2,"c":3}"#));
  assert!(!validates(schema, r#"{"a":1,"b":2,"c":false}"#));
  assert!(!validates(
    "root = { g<uint> } g<T> = (h,) h = (g<uint>,)",
    r#"{"a":1}"#,
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn colon_labels_do_not_collide_with_active_rule_names() {
  let schema = "root = m m = { g<uint> } g<T> = (m: T)";
  assert!(validates(schema, r#"{"m":1}"#));
  assert!(!validates(schema, r#"{"m":false}"#));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn generic_recursion_distinguishes_group_socket_names() {
  let schema = "root = { g<uint> } g<T> = ($$g<T>,) $$g<T> = (a: T)";
  assert!(validates(schema, r#"{"a":1}"#));
  assert!(!validates(schema, r#"{"a":false}"#));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn repeated_single_members_preserve_bounds_and_movable_claims() {
  let entries = (0..512)
    .map(|index| format!(r#""k{index:04}":1"#))
    .collect::<Vec<_>>()
    .join(",");
  let input = format!("{{{entries}}}");
  for group in ["(tstr => uint)", "g", "h<uint>"] {
    let rules = "g = (tstr => uint) h<T> = (tstr => T)";
    assert!(validates(
      &format!("root = {{ * {group} }} {rules}"),
      &input
    ));
    assert!(validates(
      &format!("root = {{ version: bool, * {group} }} {rules}"),
      &format!(r#"{{"version":true,{entries}}}"#),
    ));
    assert!(validates(
      &format!("root = {{ 0*512 {group} }} {rules}"),
      &input
    ));
    assert!(!validates(
      &format!("root = {{ 0*511 {group} }} {rules}"),
      &input
    ));
    for occurrence in ["?", "0*1", "*1"] {
      assert!(validates(
        &format!("root = {{ {occurrence} {group}, a: uint }} {rules}"),
        r#"{"a":1,"b":2}"#,
      ));
    }
    assert!(validates(
      &format!("root = {{ * {group}, * tstr => tstr }} {rules}"),
      r#"{"a":"text","b":1}"#,
    ));
    assert!(!validates(
      &format!("root = {{ 0*0 {group} }} {rules}"),
      r#"{"a":1}"#,
    ));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn repeated_single_members_do_not_bypass_key_domains() {
  let constrained = r#"root = { * (key => uint) } key = tstr .regexp "^a$""#;
  assert!(validates(constrained, "{}"));
  assert!(!validates(constrained, r#"{"b":1}"#));
  for schema in [
    "root = { * (tstr => uint) } tstr = uint",
    "root = { * g<uint> } g<tstr> = (tstr => uint)",
    "root = { g<uint> } g<tstr> = (* (tstr => uint))",
  ] {
    assert!(!validates(schema, r#"{"a":1}"#), "{}", schema);
  }
  assert!(validates(
    "root = [ * g<uint> ] g<T> = (tstr => T)",
    "[1,2]",
  ));
  assert!(!validates(
    "root = [ * g<uint> ] g<T> = (tstr => T)",
    "[true]",
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_batches_allow_prior_claim_reassignment() {
  for group in ["(tstr => uint)", "g", "h<uint>"] {
    let schema =
      format!("root = {{ tstr => any, ? {group} }} g = (tstr => uint) h<T> = (tstr => T)");
    assert!(validates(&schema, r#"{"a":1,"b":"text"}"#), "{}", schema);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_optional_groups_restore_enclosing_occurrence() {
  for group in ["(a: uint)", "(tstr => uint)", "g", "h<uint>"] {
    let schema =
      format!("root = {{ + (? {group}, tstr => tstr) }} g = (tstr => uint) h<T> = (tstr => T)");
    assert!(validates(&schema, r#"{"x":"1","y":"2"}"#), "{}", schema);
    assert!(!validates(&schema, r#"{"x":"1","y":false}"#), "{}", schema);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_generic_batch_bindings_are_child_local() {
  assert!(validates(
    "root = { g<uint> } g<T> = (* $$g<T>,) $$g<U> = (tstr => U)",
    r#"{"a":1}"#,
  ));
  let schema = "root = { g<uint> } g<T> = (0*1 $$g<tstr>, z: T) $$g<U> = (tstr => U)";
  assert!(validates(schema, r#"{"a":"text","z":1}"#));
  assert!(!validates(schema, r#"{"a":"text","z":false}"#));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_required_choices_prune_failed_prefixes() {
  let schema = format!(
    "root = {{ {} }}",
    (0..13)
      .map(|index| format!("(a{index}: uint // b{index}: uint)"))
      .collect::<Vec<_>>()
      .join(",")
  );
  let input = format!(
    "{{{}}}",
    (0..13)
      .map(|index| format!(r#""b{index}":1"#))
      .collect::<Vec<_>>()
      .join(",")
  );
  assert!(validates(&schema, &input));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn incremental_only_group_cycles_require_progress() {
  assert!(!validates(
    r#"root = { g } g //= (? "a" => uint) g //= (h,) h //= (h,)"#,
    r#"{"extra":1}"#,
  ));
  assert!(validates(
    "root = { g } g //= (tstr => uint, ? g)",
    r#"{"a":1,"b":2,"c":3}"#,
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn optional_named_groups_do_not_relax_required_siblings() {
  for outer in ["?", "*"] {
    for occurrence in ["?", "0*1", "*1", "*", "0*2"] {
      for reference in ["g", "h<uint>"] {
        let schema = format!(
          r#"root = {{ {outer} ({occurrence} {reference}, b: uint) }}
             g = (? "a" => uint)
             h<T> = (? "a" => T)"#
        );
        assert!(validates(&schema, "{}"), "{}", schema);
        assert!(validates(&schema, r#"{"a":1,"b":2}"#), "{}", schema);
        assert!(!validates(&schema, r#"{"a":1}"#), "{}", schema);
      }
    }
  }
  assert!(!validates(
    "root = { 0*0 g, * tstr => tstr } g = (a: uint)",
    r#"{"a":1}"#,
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn zero_minimum_groups_require_complete_iterations() {
  for occurrence in ["?", "*", "0*2"] {
    for schema in [
      format!("root = {{ {occurrence} g }}\ng = (z: uint)\ng //= (a: uint, b: uint)"),
      format!("root = {{ {occurrence} (a: uint, b: uint) }}"),
    ] {
      assert!(validates(&schema, "{}"), "{}", schema);
      assert!(validates(&schema, r#"{"a":1,"b":2}"#), "{}", schema);
      assert!(!validates(&schema, r#"{"a":1}"#), "{}", schema);
    }
  }
  for group in ["g", "h<uint>"] {
    let rules = "g = (tstr => uint)\nh<T> = (tstr => T)";
    assert!(validates(
      &format!("root = {{ * {group} }}\n{rules}"),
      r#"{"a":1,"b":2,"c":3}"#,
    ));
    assert!(validates(
      &format!("root = {{ 0*2 {group} }}\n{rules}"),
      r#"{"a":1,"b":2}"#,
    ));
    assert!(!validates(
      &format!("root = {{ 0*2 {group} }}\n{rules}"),
      r#"{"a":1,"b":2,"c":3}"#,
    ));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn skipped_member_restores_claims_moved_during_key_lookup() {
  // Looking up "a" moves the first member's claim to "b". Rejecting the
  // optional value must undo that move as well as release its tentative key.
  let schema = r#"root = { tstr => tstr, ? "a" => uint, b: tstr }"#;
  assert!(validates(schema, r#"{"a":"first","b":"second"}"#));
  assert!(!validates(schema, r#"{"a":"first"}"#));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn optional_members_preserve_prior_claims_and_required_occurrences() {
  let schema = r#"root = { fixed: uint, ? tstr => uint, required: tstr }"#;
  assert!(validates(schema, r#"{"fixed":1,"required":"value"}"#));
  assert!(!validates(schema, r#"{"fixed":1,"other":"value"}"#));
  assert!(!validates(schema, r#"{"required":"value"}"#));
  assert!(!validates(
    r#"root = { ? "a" => uint, required: tstr }"#,
    r#"{"a":1}"#,
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn successful_optional_members_keep_ownership_and_failed_ones_keep_coverage() {
  assert!(!validates(
    r#"root = { ? "a" => uint, a: uint }"#,
    r#"{"a":1}"#,
  ));
  assert!(!validates(
    r#"root = { ? "a" => uint }"#,
    r#"{"a":"unclaimed"}"#,
  ));
  assert!(validates(r#"root = { ? "a" => uint }"#, "{}"));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn skipped_members_retry_group_choices_until_the_map_is_covered() {
  for schema in [
    r#"root = { ? "a" => uint // a: tstr }"#,
    r#"root = { (? "a" => uint // a: tstr) }"#,
    "root = { g }\ng = (? \"a\" => uint // a: tstr)",
    "root = { g<uint> }\ng<T> = (? \"a\" => T // a: tstr)",
    "root = { g }\ng = (a: tstr)\ng //= (? \"a\" => uint)",
  ] {
    assert!(validates(schema, r#"{"a":"value"}"#), "{}", schema);
    assert!(!validates(schema, r#"{"a":false}"#), "{}", schema);
  }
  assert!(validates(
    r#"root = { (? "a" => uint // a: tstr), z: bool }"#,
    r#"{"a":"value","z":true}"#,
  ));
  assert!(validates(
    r#"root = { (? "a" => uint // a: bool), * tstr => tstr }"#,
    r#"{"a":"value"}"#,
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn group_retries_isolate_sibling_choices_and_nested_maps() {
  let schema = r#"root = {
    (? "a" => uint // a: tstr),
    (? "b" => uint // b: bool // b: tstr),
    nested: { ? "c" => uint // c: tstr }
  }"#;
  assert!(validates(
    schema,
    r#"{"a":"value","b":"other","nested":{"c":"inner"}}"#,
  ));
  assert!(!validates(
    schema,
    r#"{"a":"value","b":"other","nested":{"c":false}}"#,
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn nested_group_occurrences_do_not_relax_required_members() {
  for outer in ["?", "*"] {
    for inner in ["?", "0*1", "*1"] {
      let schema = format!("root = {{ {} ({} \"a\" => uint, b: uint) }}", outer, inner);
      assert!(!validates(&schema, r#"{"a":1}"#), "{}", schema);
      assert!(!validates(&schema, r#"{"a":"value"}"#), "{}", schema);
      assert!(validates(&schema, r#"{"b":2}"#), "{}", schema);
      assert!(validates(&schema, r#"{"a":1,"b":2}"#), "{}", schema);
    }
  }
}
