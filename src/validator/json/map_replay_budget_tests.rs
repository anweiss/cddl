use super::*;
use crate::pest_bridge::cddl_from_pest_str;

fn limited_validator<'a>(cddl: &'a CDDL<'a>, json: Value, limit: usize) -> JSONValidator<'a> {
  #[cfg(feature = "additional-controls")]
  let mut validator = JSONValidator::new(cddl, json, None);
  #[cfg(not(feature = "additional-controls"))]
  let mut validator = JSONValidator::new(cddl, json);
  validator.map_replay_budget = Arc::new(MapReplayBudget::new(limit));
  validator
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_identical_claims_share_probes_but_not_generic_bindings() {
  let cddl =
    cddl_from_pest_str("root = { tstr => uint } integer = uint text-value = tstr").unwrap();
  let Rule::Type { rule, .. } = &cddl.rules[0] else {
    panic!("root type")
  };
  let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
    panic!("map")
  };
  let validator = limited_validator(&cddl, serde_json::json!({}), 0);
  let member = validator
    .batchable_map_member(group, None, &validator, 0)
    .unwrap()
    .clone();
  let mut claims: Vec<_> = (0..64)
    .map(|index| MemberClaim {
      key: index.to_string(),
      entry: member.clone(),
      context: None,
    })
    .collect();
  let mut probes = 0;
  let incompatible = claim_compatibility(&claims, 64, |_, pair| {
    probes += 1;
    Ok(pair < 63)
  })
  .unwrap();
  assert!(incompatible.is_none());
  assert_eq!(probes, 64);
  let rows = claim_compatibility(&claims, 64, |_, _| Ok(true))
    .unwrap()
    .unwrap();
  assert!(Arc::ptr_eq(&rows[0], &rows[63]));

  for (claim, rule) in claims.iter_mut().zip(&cddl.rules[1..]) {
    let Rule::Type { rule, .. } = rule else {
      panic!("argument type")
    };
    claim.context = Some(ClaimEvalContext {
      rule_name: "g",
      generic_rules: vec![GenericRule {
        name: "g",
        params: vec!["T"],
        args: vec![rule.value.type_choices[0].type1.clone()],
      }],
    });
  }
  probes = 0;
  let rows = claim_compatibility(&claims[..2], 2, |_, _| {
    probes += 1;
    Ok(true)
  })
  .unwrap()
  .unwrap();
  assert_eq!(probes, 4);
  assert!(!Arc::ptr_eq(&rows[0], &rows[1]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_capacity_failures_preserve_later_claim_errors() {
  for (prefix, definitions, input) in [
    (
      "tstr => uint",
      "",
      serde_json::json!({"0":1,"1":true,"2":"text"}),
    ),
    (
      "g, g",
      "g = (tstr => uint)",
      serde_json::json!({"0":1,"1":2,"2":true,"3":"text"}),
    ),
    (
      "* (tstr => uint)",
      "",
      serde_json::json!({"0":1,"1":2,"2":true,"3":"text"}),
    ),
    (
      "* g",
      "g = (tstr => uint)",
      serde_json::json!({"0":1,"1":2,"2":true,"3":"text"}),
    ),
  ] {
    for fallback in ["", " / any"] {
      let schema = format!(
        r#"root = {{ {prefix}, tstr => bool / tstr .regexp "[", "0": uint, * tstr => any }}{fallback} {definitions}"#
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, input.clone(), 4096);
      match validator.validate() {
        Err(Error::Validation(errors)) => assert!(
          errors.len() == 1
            && errors[0].reason.contains("regex parse error")
            && errors[0].reason.contains("unclosed character class"),
          "{}: {:?}",
          schema,
          errors
        ),
        outcome => panic!("expected regex error for {}, got {:?}", schema, outcome),
      }

      let valid_regex = schema.replace(r#""[""#, r#""^z+$""#);
      let cddl = cddl_from_pest_str(&valid_regex).unwrap();
      let mut validator = limited_validator(&cddl, input.clone(), 4096);
      if fallback.is_empty() {
        assert!(matches!(
          validator.validate(),
          Err(Error::Validation(errors))
            if !errors.is_empty() && errors.iter().all(|error| !error.reason.contains("regex"))
        ));
      } else {
        assert!(validator.validate().is_ok());
      }
    }
  }
  for (schema, input) in [
    (
      r#"root = { 2*2 (tstr => uint), tstr => bool / tstr .regexp "[", "0": uint, * tstr => any } / any"#,
      serde_json::json!({"0":1,"1":2,"2":true,"3":"text"}),
    ),
    (
      r#"root = { + (tstr => uint), tstr => bool / tstr .regexp "[", "0": uint, * tstr => any } / any"#,
      serde_json::json!({"0":1,"1":2,"2":true,"3":"text"}),
    ),
    (
      r#"root = { tstr => uint, tstr => bool / tstr .regexp "[", tstr => tstr, "0": uint } / any"#,
      serde_json::json!({"0":1,"1":true,"2":"text"}),
    ),
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, input, 4096);
    assert!(validator.validate().is_ok(), "{}", schema);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_pruning_preserves_later_regex_errors() {
  for (schema, input) in [
    (
      r#"root = { (a: uint // b: uint), required: uint, boom: tstr .regexp "[" } / any"#,
      r#"{"a":1,"boom":"text"}"#,
    ),
    (
      r#"root = { (? "a" => uint // ? "b" => uint), required: uint, boom: tstr .regexp "[" } / any"#,
      r#"{"boom":"text"}"#,
    ),
    (
      r#"root = { g, required: uint, boom: tstr .regexp "[" } / any g = (a: uint // b: uint)"#,
      r#"{"a":1,"boom":"text"}"#,
    ),
    (
      r#"root = { ? (missing: uint, boom: tstr .regexp "[" // absent: uint, boom: tstr .regexp "["), * tstr => any } / any"#,
      r#"{"boom":"text"}"#,
    ),
    (
      r#"root = { ? (a: uint, boom: tstr .regexp "[" // b: uint, boom: tstr .regexp "["), * tstr => any } / any"#,
      r#"{"a":"x","b":"y","boom":"text"}"#,
    ),
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::from_str(input).unwrap(), 4096);
    match validator.validate() {
      Err(Error::Validation(errors)) => assert!(
        errors.len() == 1
          && errors[0].reason.contains("regex parse error")
          && errors[0].reason.contains("unclosed character class"),
        "{}: {:?}",
        schema,
        errors
      ),
      outcome => panic!("expected regex error for {}, got {:?}", schema, outcome),
    }
  }
  for (schema, input) in [
    (
      r#"root = { a: uint // b: tstr .regexp "[" } / any"#,
      r#"{"a":1}"#,
    ),
    (
      r#"root = { (? "a" => tstr .regexp "[" // ? "b" => tstr .regexp "["), * tstr => any } / any"#,
      r#"{"extra":"text"}"#,
    ),
    (
      r#"root = { ? (missing: uint, boom: tstr .regexp "[" // absent: uint, boom: tstr .regexp "["), * tstr => any } / any"#,
      r#"{"extra":1}"#,
    ),
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::from_str(input).unwrap(), 4096);
    assert!(validator.validate().is_ok(), "{}", schema);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_tail_proof_preserves_pending_generic_shadowing() {
  for (schema, input) in [
    (
      r#"root = { ? g<bad>, * tstr => any } / any g<uint> = (missing: uint, boom: uint // absent: uint, boom: uint) bad = tstr .regexp "[""#,
      r#"{"boom":"text"}"#,
    ),
    (
      r#"root = { ? g<bad>, * tstr => any } / any g<alias> = (missing: uint, boom: alias // absent: uint, boom: alias) alias = uint bad = tstr .regexp "[""#,
      r#"{"boom":"text"}"#,
    ),
    (
      r#"root = { ? g<bad>, * tstr => any } / any g<alias> = (a: uint, boom: alias // b: uint, boom: alias) alias = tstr bad = tstr .regexp "[""#,
      r#"{"a":"x","b":"y","boom":"text"}"#,
    ),
    (
      r#"root = { ? g<uint>, ? g<bad>, * tstr => any } / any g<alias> = (missing: uint, boom: alias // absent: uint, boom: alias) alias = uint bad = tstr .regexp "[""#,
      r#"{"boom":"text"}"#,
    ),
    (
      r#"root = { ? g<bad>, * tstr => any } / any g<T> = (missing: uint, boom: T // absent: uint, boom: T) bad = tstr .regexp "[""#,
      r#"{"boom":"text"}"#,
    ),
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::from_str(input).unwrap(), 4096);
    match validator.validate() {
      Err(Error::Validation(errors)) => assert!(
        errors.len() == 1
          && errors[0].reason.contains("regex parse error")
          && errors[0].reason.contains("unclosed character class"),
        "{}: {:?}",
        schema,
        errors
      ),
      outcome => panic!("expected regex error for {}, got {:?}", schema, outcome),
    }
    let valid_schema = schema.replace(".regexp \"[\"", ".regexp \"^text$\"");
    let cddl = cddl_from_pest_str(&valid_schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::from_str(input).unwrap(), 4096);
    assert!(validator.validate().is_ok(), "{}", valid_schema);
  }
  for (schema, input) in [
    (
      r#"root = { ? g<bad>, * tstr => any } / any g<uint> = (missing: uint, boom: uint // absent: uint, boom: uint) bad = tstr .regexp "[""#,
      r#"{"extra":1}"#,
    ),
    (
      r#"root = { 0*0 g<bad>, * tstr => any } / any g<uint> = (missing: uint, boom: uint // absent: uint, boom: uint) bad = tstr .regexp "[""#,
      r#"{"boom":"text"}"#,
    ),
    (
      r#"root = { ? g<bad>, * tstr => any } / any g<tstr> = (a: uint, boom: tstr // b: uint, boom: tstr) bad = tstr .regexp "[""#,
      r#"{"a":"x","b":"y","boom":"text"}"#,
    ),
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::from_str(input).unwrap(), 4096);
    assert!(validator.validate().is_ok(), "{}", schema);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_budget_boundary_and_repeated_validation() {
  let cddl = cddl_from_pest_str(r#"root = { ? "a" => uint // ? "a" => bool // a: tstr }"#).unwrap();
  let mut validator = limited_validator(&cddl, serde_json::json!({"a":"value"}), 1);
  for _ in 0..2 {
    let error = validator.validate().unwrap_err();
    assert!(matches!(error, Error::ResourceLimit { limit: 1 }));
    assert!(error.to_string().starts_with("JSON validation incomplete:"));
    assert!(validator.map_choice_search.is_none());
    assert!(validator.validated_keys.is_none());
    assert!(validator.deferred_map_errors.is_empty());
    assert!(!validator.state.is_root);
  }
  validator.map_replay_budget = Arc::new(MapReplayBudget::new(2));
  for _ in 0..2 {
    assert!(validator.validate().is_ok());
    assert_eq!(
      validator
        .map_replay_budget
        .remaining
        .load(Ordering::Relaxed),
      0
    );
  }
  let mut invalid = limited_validator(&cddl, serde_json::json!({"a":[]}), 2);
  assert!(matches!(invalid.validate(), Err(Error::Validation(_))));

  let schema = format!(
    "root = {{ {} }}",
    (0..13)
      .map(|index| format!("(a{index}: uint // b{index}: uint)"))
      .collect::<Vec<_>>()
      .join(",")
  );
  let cddl = cddl_from_pest_str(&schema).unwrap();
  let input = Value::Object(
    (0..13)
      .map(|index| (format!("b{index}"), serde_json::json!(1)))
      .collect(),
  );
  let mut pruned = limited_validator(&cddl, input.clone(), 13);
  assert!(pruned.validate().is_ok());
  assert_eq!(
    pruned.map_replay_budget.remaining.load(Ordering::Relaxed),
    0
  );
  let mut exhausted = limited_validator(&cddl, input, 12);
  assert!(matches!(
    exhausted.validate(),
    Err(Error::ResourceLimit { limit: 12 })
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_deferred_value_errors_reset_on_reuse() {
  let cddl = cddl_from_pest_str(r#"root = { ? "port" => uint }"#).unwrap();
  let mut validator = limited_validator(&cddl, serde_json::json!({"port":"80"}), 0);
  let Err(Error::Validation(errors)) = validator.validate() else {
    panic!("expected a value mismatch");
  };
  assert_eq!(errors.len(), 1);
  assert_eq!(errors[0].json_location, "/port");
  assert!(errors[0].reason.contains("expected type uint"));
  assert!(validator.deferred_map_errors.is_empty());
  validator.json = serde_json::json!({"other":true}).into();
  let Err(Error::Validation(errors)) = validator.validate() else {
    panic!("expected an unknown key");
  };
  assert_eq!(errors.len(), 1);
  assert_eq!(errors[0].reason, "unexpected key \"other\"");
  for json in [serde_json::json!({"port":80}), serde_json::json!({})] {
    validator.json = json.into();
    assert!(validator.validate().is_ok());
    assert!(validator.deferred_map_errors.is_empty());
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_failed_generic_children_preserve_unique_ownership() {
  let cddl = cddl_from_pest_str("root = { a: uint, g<uint> } g<T> = (b: T,)").unwrap();
  let Rule::Type { rule, .. } = &cddl.rules[0] else {
    panic!("root type");
  };
  let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
    panic!("map");
  };
  let entries = &group.group_choices[0].group_entries;
  let mut validator = limited_validator(&cddl, serde_json::json!({"a":1,"b":false}), 0);
  validator.visit_group_entry(&entries[0].0).unwrap();
  for _ in 0..8 {
    validator.visit_group_entry(&entries[1].0).unwrap();
    assert!(!validator.errors.is_empty());
    assert_eq!(validator.validated_keys, Some(vec!["a".to_owned()]));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_budget_is_cumulative_across_nested_and_child_validators() {
  for (schema, json, required) in [
    (
      r#"root = { left: branch, right: branch }
           branch = { ? "x" => uint // x: tstr }"#,
      serde_json::json!({"left":{"x":"one"},"right":{"x":"two"}}),
      2,
    ),
    (
      r#"root = [* { ? "x" => uint // x: tstr }]"#,
      serde_json::json!([{"x":"one"},{"x":"two"}]),
      2,
    ),
    (
      r#"root = { left: branch<tstr>, right: branch<tstr> }
           branch<T> = { ? "x" => uint // x: T }"#,
      serde_json::json!({"left":{"x":"one"},"right":{"x":"two"}}),
      2,
    ),
    (
      r#"root = { (left: branch, right: uint) // (left: branch, right: tstr) }
           branch = { ? "x" => uint // x: tstr }"#,
      serde_json::json!({"left":{"x":"value"},"right":"value"}),
      3,
    ),
    (
      r#"root = { * members<branch> } members<T> = (tstr => T)
           branch = { ? "x" => uint // x: tstr }"#,
      serde_json::json!({"left":{"x":"one"},"right":{"x":"two"}}),
      2,
    ),
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut exhausted = limited_validator(&cddl, json.clone(), required - 1);
    assert!(matches!(
      exhausted.validate(),
      Err(Error::ResourceLimit { limit }) if limit == required - 1
    ));
    let mut sufficient = limited_validator(&cddl, json, required);
    assert!(sufficient.validate().is_ok());
    assert_eq!(
      sufficient
        .map_replay_budget
        .remaining
        .load(Ordering::Relaxed),
      0
    );
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_resource_errors_cannot_be_swallowed_as_choice_mismatches() {
  for (root, json) in [
    ("root = branch / any", serde_json::json!({"x":"value"})),
    (
      "root = { left: branch // * tstr => any }",
      serde_json::json!({"left":{"x":"value"}}),
    ),
    (
      r#"root = { ? "left" => branch, * tstr => any }"#,
      serde_json::json!({"left":{"x":"value"}}),
    ),
    (
      "root = { ? members, * tstr => any } members = (left: branch)",
      serde_json::json!({"left":{"x":"value"}}),
    ),
    (
      "root = { ? (left: branch), * tstr => any }",
      serde_json::json!({"left":{"x":"value"}}),
    ),
    (
      "root = { * (tstr => branch), * tstr => any }",
      serde_json::json!({"left":{"x":"value"}}),
    ),
    (
      "root = { * members, * tstr => any } members = (tstr => branch)",
      serde_json::json!({"left":{"x":"value"}}),
    ),
    (
      "root = { * members<branch>, * tstr => any } members<T> = (tstr => T)",
      serde_json::json!({"left":{"x":"value"}}),
    ),
    ("root = [branch / any]", serde_json::json!([{"x":"value"}])),
  ] {
    let schema = format!("{root}\nbranch = {{ ? \"x\" => uint // x: tstr }}");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, json, 0);
    assert!(matches!(
      validator.validate(),
      Err(Error::ResourceLimit { limit: 0 })
    ));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_initial_paths_do_not_consume_the_budget() {
  let cddl = cddl_from_pest_str("root = [* { x: uint // x: tstr }]").unwrap();
  let json = Value::Array((0..128).map(|n| serde_json::json!({"x":n})).collect());
  let mut validator = limited_validator(&cddl, json, 0);
  assert!(validator.validate().is_ok());
  let cddl = cddl_from_pest_str("root = [* { ? (a: uint // b: uint), * tstr => tstr }]").unwrap();
  let json = Value::Array((0..128).map(|_| serde_json::json!({"a":"text"})).collect());
  let mut validator = limited_validator(&cddl, json, 0);
  assert!(validator.validate().is_ok());
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_absent_optional_choices_do_not_multiply_paths() {
  for form in 0..9 {
    let mut entries = Vec::new();
    let mut rules = Vec::new();
    let mut input = serde_json::json!({"m":1});
    for index in 0..14 {
      let body = if form / 3 == 1 {
        format!("(? p{index}: uint, a{index}: uint // ? q{index}: uint, b{index}: uint)")
      } else {
        if form / 3 == 2 {
          input[format!("a{index}")] = Value::String("text".into());
          input[format!("b{index}")] = Value::String("text".into());
        }
        format!("(a{index}: uint // b{index}: uint)")
      };
      match form % 3 {
        0 => entries.push(format!("? {body}")),
        1 => {
          entries.push(format!("? g{index}"));
          rules.push(format!("g{index} = {body}"));
        }
        _ => {
          entries.push(format!("? g{index}<uint>"));
          rules.push(format!("g{index}<T> = {body}"));
        }
      }
    }
    let schema = format!(
      "root = {{ ? (k: uint // m: uint), {}, * tstr => tstr }} {}",
      entries.join(","),
      rules.join("\n")
    );
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, input.clone(), 1);
    assert!(validator.validate().is_ok(), "{}", schema);
    assert_eq!(
      validator
        .map_replay_budget
        .remaining
        .load(Ordering::Relaxed),
      0
    );
    let mut exhausted = limited_validator(&cddl, input, 0);
    assert!(matches!(
      exhausted.validate(),
      Err(Error::ResourceLimit { limit: 0 })
    ));
  }
  let cddl = cddl_from_pest_str("root = { ? (? a: uint, m: uint // ? b: uint, m: uint) }").unwrap();
  let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 0);
  assert!(validator.validate().is_ok());
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_optional_mismatch_preserves_claim_revalidation_errors() {
  for (optional, definitions) in [
    ("? (a: uint // b: uint)", ""),
    ("? g", "g = (a: uint // b: uint)"),
    ("? g<uint>", "g<T> = (a: uint // b: uint)"),
  ] {
    for fallback in ["", " / any"] {
      let schema = format!(
        r#"root = {{ tstr => uint / tstr .regexp "[", {optional}, * tstr => any }}{fallback} {definitions}"#
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator =
        limited_validator(&cddl, serde_json::json!({"0":1,"a":"x","b":"y"}), 4096);
      let result = validator.validate();
      let Err(Error::Validation(errors)) = result else {
        panic!(
          "expected propagated regex error for {}, got {:?}",
          schema, result
        );
      };
      assert_eq!(errors.len(), 1, "{schema}: {errors:?}");
      assert!(
        errors[0].reason.contains("regex parse error")
          && errors[0].reason.contains("unclosed character class"),
        "{}: {:?}",
        schema,
        errors
      );
    }
  }
  for (schema, input) in [
    (
      "root = { ? (a: uint // b: uint), * tstr => any }",
      serde_json::json!({"a":"x","b":"y"}),
    ),
    (
      "root = { tstr => uint, ? (a: uint // b: uint), * tstr => any }",
      serde_json::json!({"0":1,"a":"x","b":"y"}),
    ),
    (
      r#"root = { tstr ^ => uint / tstr .regexp "[", ? (a: uint // b: uint), * tstr => any }"#,
      serde_json::json!({"0":1,"a":"x","b":"y"}),
    ),
    (
      r#"root = { tstr => uint / tstr .regexp "[", ? (a: uint // b: uint), * tstr => any }"#,
      serde_json::json!({"0":1,"other":"text"}),
    ),
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, input, 4096);
    assert!(validator.validate().is_ok(), "{}", schema);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_optional_impossibility_keeps_effectful_paths() {
  for (body, input, proof) in [
    ("a: uint // b: uint", r#"{"a":"text","b":"text"}"#, true),
    ("? p: uint, a: uint // ? q: uint, b: uint", "{}", true),
    ("? p: uint, a: uint // b: uint", r#"{"p":1}"#, false),
    (
      r#""a" ^ => uint // "b" ^ => uint"#,
      r#"{"a":"text","b":"text"}"#,
      false,
    ),
    (
      "a: uint .size 1 // b: uint",
      r#"{"a":"text","b":"text"}"#,
      false,
    ),
    ("a: uint // b: tstr", r#"{"a":"text","b":"text"}"#, false),
    ("a: uint // b: uint", r#"{"a":[],"b":[]}"#, false),
  ] {
    let schema = format!("root = {{ ? ({body}) }}");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let Rule::Type { rule, .. } = &cddl.rules[0] else {
      panic!("type rule")
    };
    let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("map")
    };
    let GroupEntry::InlineGroup { group, .. } = &group.group_choices[0].group_entries[0].0 else {
      panic!("inline group")
    };
    let mut validator = limited_validator(&cddl, serde_json::from_str(input).unwrap(), 0);
    validator.map_choice_search = Some(Arc::new(Mutex::new(MapChoiceSearch::default())));
    assert_eq!(
      validator.optional_choices_definitely_fail(group, None),
      proof,
      "{schema}"
    );
    if input.contains("\"a\":\"text\"") && proof {
      validator.validated_keys = Some(vec!["a".into()]);
      assert!(!validator.optional_choices_definitely_fail(group, None));
      validator.validated_keys = None;
      validator.cut_value = Some("outer".into());
      assert!(!validator.optional_choices_definitely_fail(group, None));
    }
  }
  for schema in [
    "root = { ? (a: uint // b: uint), * tstr => 0 } uint = tstr",
    "root = { ? (a: uint // b: uint), * tstr => 0 } uint /= tstr",
    "root = { ? g<tstr>, * tstr => 0 } g<uint> = (a: uint // b: uint)",
    "root = { ? g<tstr>, * tstr => 0 } g<T> = (a: T // b: T)",
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"a":"text"}), 0);
    assert!(validator.validate().is_ok(), "{}", schema);
  }
  for input in [serde_json::json!({"required":1}), serde_json::json!({})] {
    let cddl =
      cddl_from_pest_str("root = { ? (? p: uint, a: uint // ? q: uint, b: uint), required: uint }")
        .unwrap();
    let mut validator = limited_validator(&cddl, input.clone(), 0);
    assert_eq!(
      validator.validate().is_ok(),
      input.get("required").is_some()
    );
    assert_eq!(
      validator.validate().is_ok(),
      input.get("required").is_some()
    );
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_alias_impossibility_preserves_binding_and_budget_rules() {
  let entries = (0..14)
    .map(|i| format!("? (a{i}: alias // b{i}: alias)"))
    .collect::<Vec<_>>()
    .join(",");
  let schema = format!(
    "root = {{ ? (k: uint // m: uint), {entries}, * tstr => tstr }} alias = link link = uint"
  );
  let cddl = cddl_from_pest_str(&schema).unwrap();
  let mut input = serde_json::json!({"m":1});
  for i in 0..14 {
    input[format!("a{i}")] = Value::String("text".into());
    input[format!("b{i}")] = Value::String("text".into());
  }
  let mut validator = limited_validator(&cddl, input.clone(), 1);
  assert!(validator.validate().is_ok());
  let mut exhausted = limited_validator(&cddl, input, 0);
  assert!(matches!(
    exhausted.validate(),
    Err(Error::ResourceLimit { limit: 0 })
  ));
  for schema in [
    "root = { ? (a: alias // b: alias), * tstr => 0 } alias = uint uint = tstr",
    "root = { ? (a: alias // b: alias), * tstr => 0 } alias = uint alias /= tstr",
    "root = { ? g<tstr>, * tstr => 0 } g<alias> = (a: alias // b: alias) alias = uint",
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"a":"text"}), 0);
    assert!(validator.validate().is_ok(), "{}", schema);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_unknown_keys_do_not_enumerate_irrelevant_choices() {
  for named in [false, true] {
    let mut entries = Vec::new();
    let mut rules = Vec::new();
    let mut input = serde_json::json!({});
    for i in 0..13 {
      let body = format!("(a{i}: uint // b{i}: uint)");
      if named {
        entries.push(format!("? g{i}"));
        rules.push(format!("g{i} = {body}"));
      } else {
        entries.push(format!("? {body}"));
      }
      input[format!("a{i}")] = Value::from(1);
    }
    let schema = format!("root = {{ {} }} {}", entries.join(","), rules.join("\n"));
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut valid = limited_validator(&cddl, input.clone(), 0);
    assert!(valid.validate().is_ok());
    input["unknown"] = Value::Bool(true);
    let mut invalid = limited_validator(&cddl, input, 0);
    let Error::Validation(errors) = invalid.validate().unwrap_err() else {
      panic!("provably unknown key must not exhaust the replay budget");
    };
    assert_eq!(errors.len(), 1);
    assert!(errors[0].reason.contains("unexpected key \"unknown\""));
  }
  for schema in [
    "root = { fixed: uint // * tstr => any }",
    "root = { fixed: uint // all } all = (* tstr => any)",
    "root = { fixed: uint // unknown: uint, fixed: uint }",
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let input = serde_json::json!({"fixed":1,"unknown":2});
    let mut valid = limited_validator(&cddl, input.clone(), 1);
    assert!(valid.validate().is_ok(), "{}", schema);
    let mut exhausted = limited_validator(&cddl, input, 0);
    assert!(matches!(
      exhausted.validate(),
      Err(Error::ResourceLimit { limit: 0 })
    ));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_unknown_key_proof_retains_effects_and_recursive_paths() {
  for schema in [
    "root = { fixed: uint // fixed: uint .size 1 }",
    "root = { fixed: uint // dynamic } dynamic = (fixed: uint) dynamic //= (* tstr => any)",
    "root = { fixed: uint // dynamic<uint> } dynamic<T> = (fixed: T)",
    "root = { fixed: uint // recurse } recurse = (? recurse, fixed: uint)",
    "root = { fixed: uint // \"fixed\" ^ => uint }",
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"fixed":1,"unknown":true}), 0);
    assert!(
      matches!(validator.validate(), Err(Error::ResourceLimit { limit: 0 })),
      "{}",
      schema
    );
  }
  let cddl =
    cddl_from_pest_str("root = { fixed: any // fixed: branch } branch = { x: uint // x: tstr }")
      .unwrap();
  let mut validator = limited_validator(
    &cddl,
    serde_json::json!({"fixed":{"x":"text"},"unknown":true}),
    1,
  );
  assert!(matches!(
    validator.validate(),
    Err(Error::ResourceLimit { limit: 1 })
  ));

  // Shared named groups form a DAG, not a recursion cycle. Both alternatives
  // must contribute their literal keys before the proof can reject another key.
  let cddl = cddl_from_pest_str(
      "root = { left // right } left = (common,) right = (common, other: uint) common = (fixed: alias) alias = uint",
    ).unwrap();
  let mut valid = limited_validator(&cddl, serde_json::json!({"fixed":1,"other":2}), 1);
  assert!(valid.validate().is_ok());
  let mut invalid = limited_validator(&cddl, serde_json::json!({"fixed":1,"unknown":[]}), 0);
  assert!(matches!(invalid.validate(), Err(Error::Validation(_))));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_alias_proof_declines_cycles_controls_and_incremental_bindings() {
  for (definitions, expected) in [
    ("alias = link link = uint", true),
    ("alias = link link = alias", false),
    ("alias = uint .size 1", false),
    ("alias = uint alias /= tstr", false),
    ("alias = pick<uint> pick<T> = T", false),
    ("alias = any any = uint", false),
  ] {
    let schema = format!("root = {{ ? (a: alias // b: alias) }} {definitions}");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let Rule::Type { rule, .. } = &cddl.rules[0] else {
      panic!("root type")
    };
    let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("map")
    };
    let GroupEntry::InlineGroup { group, .. } = &group.group_choices[0].group_entries[0].0 else {
      panic!("choices")
    };
    let mut validator = limited_validator(&cddl, serde_json::json!({"a":"text","b":"text"}), 0);
    validator.map_choice_search = Some(Arc::new(Mutex::new(MapChoiceSearch::default())));
    assert_eq!(
      validator.optional_choices_definitely_fail(group, None),
      expected,
      "{schema}"
    );
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_unknown_key_rejection_restores_failed_candidate_state() {
  let cddl = cddl_from_pest_str("root = { fixed: uint // other: uint }").unwrap();
  let Rule::Type { rule, .. } = &cddl.rules[0] else {
    panic!("root")
  };
  let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
    panic!("map")
  };
  let mut validator = limited_validator(&cddl, serde_json::json!({"fixed":1,"unknown":true}), 0);
  validator.state.data_location = "/outer".into();
  let outer = Arc::new(Mutex::new(MapChoiceSearch::default()));
  validator.map_choice_search = Some(outer.clone());
  validator
    .visit_closed_map(group, &["fixed".into(), "unknown".into()])
    .unwrap();
  assert_eq!(validator.errors.len(), 1);
  assert!(validator.errors[0]
    .reason
    .contains("unexpected key \"unknown\""));
  assert!(validator.validated_keys.is_none());
  assert!(validator.member_claims.is_empty());
  assert!(validator.object_value.is_none());
  assert!(validator.object_value_key.is_none());
  assert_eq!(validator.state.data_location, "/outer");
  assert!(Arc::ptr_eq(
    validator.map_choice_search.as_ref().unwrap(),
    &outer
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_empty_successful_suffix_choices_do_not_multiply_paths() {
  let suffix = (0..13)
    .map(|i| format!("(? \"a{i}\" => uint // ? \"b{i}\" => uint)"))
    .collect::<Vec<_>>()
    .join(",");
  let schema = format!("root = {{ (? \"k\" => uint // m: uint), {suffix} }}");
  let cddl = cddl_from_pest_str(&schema).unwrap();
  let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
  assert!(validator.validate().is_ok());
  let mut exhausted = limited_validator(&cddl, serde_json::json!({"m":1}), 0);
  assert!(matches!(
    exhausted.validate(),
    Err(Error::ResourceLimit { limit: 0 })
  ));
  let cddl = cddl_from_pest_str("root = { (? \"a\" => uint // ? \"b\" => uint) }").unwrap();
  let mut present = limited_validator(&cddl, serde_json::json!({"b":1}), 1);
  assert!(present.validate().is_ok());
  let mut exhausted = limited_validator(&cddl, serde_json::json!({"b":1}), 0);
  assert!(matches!(
    exhausted.validate(),
    Err(Error::ResourceLimit { limit: 0 })
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_empty_named_suffixes_do_not_multiply_paths() {
  let suffix = (0..13)
    .map(|i| format!("(? g{i} // ? h{i})"))
    .collect::<Vec<_>>()
    .join(",");
  let rules = (0..13)
    .map(|i| format!("g{i} = (a{i}: uint) h{i} = (b{i}: uint)"))
    .collect::<Vec<_>>()
    .join("\n");
  let schema = format!("root = {{ (? \"k\" => uint // m: uint), {suffix} }} {rules}");
  let cddl = cddl_from_pest_str(&schema).unwrap();
  let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
  assert!(validator.validate().is_ok());
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_empty_named_proof_preserves_present_keys_and_depth_errors() {
  let cddl = cddl_from_pest_str("root = { (? g // ? h) } g = (a: uint) h = (b: uint)").unwrap();
  for budget in [0, 1] {
    let mut validator = limited_validator(&cddl, serde_json::json!({"b":1}), budget);
    let result = validator.validate();
    if budget == 0 {
      assert!(matches!(result, Err(Error::ResourceLimit { limit: 0 })));
    } else {
      assert!(result.is_ok());
    }
  }
  let rules = (0..80)
    .map(|i| format!("g{i} = (g{},)", i + 1))
    .collect::<Vec<_>>()
    .join("\n");
  for occurrence in ["?", "0*0"] {
    let schema = format!(
      "root = {{ (? \"k\" => uint // m: uint), (? shallow // {occurrence} g0) }}
         shallow = (absent: uint) {rules} g80 = (never: uint)"
    );
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
    let result = validator.validate();
    if occurrence == "?" {
      assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
    } else {
      assert!(result.is_ok());
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_batch_alias_depth_matches_ordinary_calls() {
  for (entries, pending) in [
    ("* (k0 => uint)", 0),
    ("* fields", 1),
    ("* generic-fields<uint>", 1),
  ] {
    for aliases in [65, 64, 63] {
      let rules = (0..aliases - 1)
        .map(|i| format!("k{i} = k{}", i + 1))
        .collect::<Vec<_>>()
        .join("\n");
      let schema = format!(
        "root = {{ {entries} }} {rules} k{} = tstr
           fields = (k0 => uint) generic-fields<T> = (k0 => T)",
        aliases - 1
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"a":1}), 0);
      let result = validator.validate();
      if aliases + pending > 64 {
        assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
      } else {
        assert!(result.is_ok());
      }
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_empty_proof_retains_quoted_colon_and_disabled_cut_choices() {
  for member in [
    "0*1 \"a\": uint",
    "*1 \"a\": uint",
    "0*2 \"a\": uint",
    "0*0 \"a\": uint",
    "0*0 \"a\" ^ => uint",
  ] {
    let schema = format!("root = {{ ({member} // ? b: uint) }}");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({}), 1);
    assert!(validator.validate().is_ok(), "{}", member);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_empty_proof_retains_bounded_bareword_values() {
  let mut failures = Vec::new();
  for occurrence in ["0*1", "*1", "0*2", "0*0", "*", "?"] {
    let schema = format!("root = {{ ({occurrence} a: 1 // ? b: uint) }}");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({}), 1);
    if validator.validate().is_err() {
      failures.push(occurrence);
    }
  }
  assert!(failures.is_empty(), "{:?}", failures);
  for quoted in [false, true] {
    let suffix = (0..13)
      .map(|i| {
        if quoted {
          format!("(? \"a{i}\": 1 // ? \"b{i}\": 1)")
        } else {
          format!("(? a{i}: 1 // ? b{i}: 1)")
        }
      })
      .collect::<Vec<_>>()
      .join(",");
    let schema = format!("root = {{ (? \"k\" => uint // m: uint), {suffix} }}");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
    assert!(validator.validate().is_ok());
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_unknown_key_proof_retains_absent_bareword_value_depth() {
  for aliases in [65, 64] {
    let rules = (0..aliases - 1)
      .map(|i| format!("v{i} = v{}", i + 1))
      .collect::<Vec<_>>()
      .join("\n");
    let schema = format!(
      "root = {{ fixed: uint // 0*1 absent: v0 }} {rules} v{} = uint",
      aliases - 1
    );
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"fixed":1,"unknown":true}), 1);
    let result = validator.validate();
    if aliases == 65 {
      assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
    } else {
      assert!(matches!(result, Err(Error::Validation(_))));
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_suppression_preserves_absent_bareword_value_depth() {
  for aliases in [65, 64] {
    let rules = (0..aliases - 1)
      .map(|i| format!("v{i} = v{}", i + 1))
      .collect::<Vec<_>>()
      .join("\n");
    let schema = format!(
      "root = {{ required: uint, (a: uint // 0*1 absent: v0) }} / any
         {rules} v{} = uint",
      aliases - 1
    );
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"required":"text"}), 1);
    let result = validator.validate();
    if aliases == 65 {
      assert!(
        matches!(result, Err(Error::RecursionLimit { limit: 64 })),
        "{:?}",
        result
      );
    } else {
      assert!(result.is_ok());
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_empty_arms_do_not_skip_sibling_values() {
  for aliases in [65, 64] {
    let rules = (0..aliases - 1)
      .map(|i| format!("v{i} = v{}", i + 1))
      .collect::<Vec<_>>()
      .join("\n");
    for prefix in [
      r#"(? "a" => uint // 0*0 "b" => uint)"#,
      r#"(0*0 "b" => uint // ? "a" => uint)"#,
      r#"(? a: uint // 0*0 "b" => uint)"#,
      r#"(? "a" => uint // 0*1 "b" => uint)"#,
      r#"(? "a" => uint // * "b" => uint)"#,
      r#"(? "a" => uint // ? missing)"#,
      r#"(? missing // ? "a" => uint)"#,
      r#"(? "a" => uint // ? ("b" => uint))"#,
      r#"? "a" => uint"#,
    ] {
      let schema = format!(
        "root = {{ {prefix}, 0*1 absent: v0 }} / any
           missing = (b: uint) {rules} v{} = uint",
        aliases - 1
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"extra":1}), 1);
      let result = validator.validate();
      if aliases == 65 {
        assert!(
          matches!(result, Err(Error::RecursionLimit { limit: 64 })),
          "{}: {:?}",
          prefix,
          result
        );
      } else {
        assert!(result.is_ok(), "{}: {:?}", prefix, result);
      }
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_required_colon_ranges_preserve_value_depth() {
  let mut failures = Vec::new();
  for occurrence in ["1*1", "1*2", "2*2", "+"] {
    for aliases in [65, 64] {
      let rules = (0..aliases - 1)
        .map(|i| format!("v{i} = v{}", i + 1))
        .collect::<Vec<_>>()
        .join("\n");
      for entries in [
        format!("? ({occurrence} absent: v0 // missing: uint)"),
        format!("fixed: uint // {occurrence} absent: v0"),
        format!("required: uint, (a: uint // {occurrence} absent: v0)"),
      ] {
        let schema = format!(
          "root = {{ {entries} }} / any {rules} v{} = uint",
          aliases - 1
        );
        let cddl = cddl_from_pest_str(&schema).unwrap();
        let mut validator = limited_validator(
          &cddl,
          serde_json::json!({"fixed":1,"required":"text","unknown":true}),
          2,
        );
        let result = validator.validate();
        if aliases == 65 {
          if !matches!(result, Err(Error::RecursionLimit { limit: 64 })) {
            failures.push(format!("{}: {:?}", entries, result));
          }
        } else {
          assert!(result.is_ok(), "{}: {:?}", entries, result);
        }
      }
    }
  }
  assert!(failures.is_empty(), "{:#?}", failures);
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_named_shortcuts_preserve_parameter_shadowing() {
  let mut failures = Vec::new();
  for (schema, input, valid) in [
    (
      "root = { outer<actual> } outer<g> = (? g,)
         g = (a: uint // b: uint) actual = (x: uint,)",
      serde_json::json!({"x":1}),
      true,
    ),
    (
      "root = { outer<actual> } outer<g> = (? g,)
         g = (a: uint // b: uint) actual = (x: uint,)",
      serde_json::json!({"x":"text"}),
      false,
    ),
    (
      "root = { outer<actual> } outer<g> = (* g,)
         g = (tstr => uint) actual = (tstr => tstr)",
      serde_json::json!({"x":1}),
      false,
    ),
    (
      "root = { outer<actual> } outer<g> = (* g,)
         g = (tstr => uint) actual = (tstr => tstr)",
      serde_json::json!({"x":"text"}),
      true,
    ),
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, input, 2);
    let result = validator.validate();
    let actual = match result {
      Ok(()) => true,
      Err(Error::Validation(_)) => false,
      error => panic!("{}: {:?}", schema, error),
    };
    if actual != valid {
      failures.push(format!("{}: expected {}, got {}", schema, valid, actual));
    }
  }
  assert!(failures.is_empty(), "{:#?}", failures);
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_empty_arms_preserve_inherited_occurrence() {
  for occurrence in ["+", "1*2"] {
    for arms in [
      r#"? "a" => uint // ? missing"#,
      r#"? missing // ? "a" => uint"#,
    ] {
      let schema =
        format!("root = {{ {occurrence} (({arms}), tstr => tstr) }} missing = (b: uint,)");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"x":"one","y":"two"}), 1);
      assert!(validator.validate().is_ok(), "{}", schema);
      let mut invalid = limited_validator(&cddl, serde_json::json!({"x":"one","y":false}), 1);
      assert!(matches!(invalid.validate(), Err(Error::Validation(_))));
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_inline_occurrence_proofs_preserve_depth() {
  let mut failures = Vec::new();
  for aliases in [65, 64] {
    let rules = (0..aliases - 1)
      .map(|i| format!("v{i} = v{}", i + 1))
      .collect::<Vec<_>>()
      .join("\n");
    for occurrence in ["+", "1*1", "1*2"] {
      for entries in [
        format!("fixed: uint // {occurrence} (absent: v0,)"),
        format!("required: uint, (a: uint // {occurrence} (absent: v0,))"),
      ] {
        let schema = format!(
          "root = {{ {entries} }} / any {rules} v{} = uint",
          aliases - 1
        );
        let cddl = cddl_from_pest_str(&schema).unwrap();
        let mut validator = limited_validator(
          &cddl,
          serde_json::json!({"fixed":1,"required":"text","unknown":true}),
          2,
        );
        let result = validator.validate();
        if aliases == 65 {
          if !matches!(result, Err(Error::RecursionLimit { limit: 64 })) {
            failures.push(format!("{}: {:?}", entries, result));
          }
        } else {
          assert!(result.is_ok(), "{}: {:?}", entries, result);
        }
      }
      assert!(failures.is_empty(), "{:#?}", failures);
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_named_occurrence_proofs_preserve_depth() {
  for occurrence in ["+", "1*1", "1*2"] {
    for (entries, pending) in [
      (format!("fixed: uint // {occurrence} fields"), 1),
      (r#"(? "a" => uint // ? outer)"#.into(), 2),
    ] {
      for depth in [65, 64] {
        let aliases = depth - pending;
        let rules = (0..aliases - 1)
          .map(|i| format!("v{i} = v{}", i + 1))
          .collect::<Vec<_>>()
          .join("\n");
        let schema = format!(
          "root = {{ {entries} }} / any fields = (absent: v0,)
             outer = ({occurrence} fields,) {rules} v{} = uint",
          aliases - 1
        );
        let cddl = cddl_from_pest_str(&schema).unwrap();
        let mut validator =
          limited_validator(&cddl, serde_json::json!({"fixed":1,"unknown":true}), 1);
        let result = validator.validate();
        if depth == 65 {
          assert!(
            matches!(result, Err(Error::RecursionLimit { limit: 64 })),
            "{}: {:?}",
            entries,
            result
          );
        } else {
          assert!(result.is_ok(), "{}: {:?}", entries, result);
        }
      }
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_deduplicates_diagnostics_after_actual_replay() {
  let cddl = cddl_from_pest_str("root = { (a: uint / bool // b: uint / bool) }").unwrap();
  for budget in [0, 1] {
    let mut validator = limited_validator(
      &cddl,
      serde_json::json!({"a":1,"b":1,"unexpected":1}),
      budget,
    );
    let result = validator.validate();
    if budget == 0 {
      assert!(matches!(result, Err(Error::ResourceLimit { limit: 0 })));
    } else {
      assert!(matches!(&result, Err(Error::Validation(_))));
      assert_eq!(
        result
          .unwrap_err()
          .to_string()
          .matches("unexpected key \"unexpected\"")
          .count(),
        1
      );
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_named_optional_proof_preserves_reference_depth() {
  let rules = (0..63)
    .map(|i| format!("g{i} = (g{},)", i + 1))
    .collect::<Vec<_>>()
    .join("\n");
  for reference in ["h", "generic-h<uint>"] {
    for occurrence in ["?", "0*0"] {
      let schema = format!(
        "root = {{ g0 }} {rules} g63 = ({occurrence} {reference},)
           h = (a: uint // b: uint) generic-h<T> = (a: T // b: T)"
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({}), 0);
      let result = validator.validate();
      if occurrence == "?" {
        assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
      } else {
        assert!(result.is_ok());
      }
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_generic_cycles_remain_mismatches_at_depth_boundary() {
  for (enclosing, argument) in [(63, "T"), (64, "T"), (63, "tstr")] {
    let rules = (0..enclosing - 1)
      .map(|i| format!("g{i} = (g{},)", i + 1))
      .collect::<Vec<_>>()
      .join("\n");
    let schema = format!(
      "root = {{ g0 }} {rules} g{} = (cycle<uint>,)
         cycle<T> = (cycle<{argument}> // a: T)",
      enclosing - 1
    );
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"a":1}), 1);
    let result = validator.validate();
    if enclosing == 63 && argument == "T" {
      assert!(result.is_ok());
    } else {
      assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_non_generic_batches_count_named_depth() {
  for enclosing in [64, 63] {
    let rules = (0..enclosing - 1)
      .map(|i| format!("g{i} = (g{},)", i + 1))
      .collect::<Vec<_>>()
      .join("\n");
    let schema = format!(
      "root = {{ g0 }} {rules} g{} = (? h,) h = (tstr => uint)",
      enclosing - 1
    );
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({}), 0);
    let result = validator.validate();
    if enclosing == 64 {
      assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
    } else {
      assert!(result.is_ok());
    }
  }
  for enclosing in [63, 62] {
    let rules = (0..enclosing - 1)
      .map(|i| format!("g{i} = (g{},)", i + 1))
      .collect::<Vec<_>>()
      .join("\n");
    let schema = format!(
      "root = {{ g0 }} {rules} g{} = (? h,)
         h = (tstr => {{ tail }}) tail = (a: uint)",
      enclosing - 1
    );
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"outer":{"a":1}}), 0);
    let result = validator.validate();
    if enclosing == 63 {
      assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
    } else {
      assert!(result.is_ok());
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_suppressed_nested_nodes_preserve_enclosing_effects() {
  let cddl = cddl_from_pest_str(
    "root = { (required: uint, (x: uint // y: uint)),
                (a: uint // b: tstr .regexp \"[\") } / any",
  )
  .unwrap();
  let mut validator = limited_validator(&cddl, serde_json::json!({"b":"x"}), 2);
  assert!(validator
    .validate()
    .unwrap_err()
    .to_string()
    .to_lowercase()
    .contains("regex"));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_zero_minimum_empty_choices_do_not_multiply_paths() {
  for occurrence in ["0*1", "*1", "*", "0*2", "0*0"] {
    let suffix = (0..13)
      .map(|i| format!("({occurrence} \"a{i}\" => uint // {occurrence} \"b{i}\" => uint)"))
      .collect::<Vec<_>>()
      .join(",");
    let schema = format!("root = {{ (? \"k\" => uint // m: uint), {suffix} }}");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
    assert!(validator.validate().is_ok(), "{}", occurrence);
    if occurrence != "0*0" {
      let schema =
        format!("root = {{ ({occurrence} \"a\" => uint // {occurrence} \"b\" => uint) }}");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"b":1}), 1);
      assert!(validator.validate().is_ok(), "{}", occurrence);
      let schema = format!(
        "root = {{ ({occurrence} \"a\" => uint //
                      {occurrence} \"b\" => tstr .regexp \"[\") }} / any"
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"b":"x"}), 1);
      assert!(validator
        .validate()
        .unwrap_err()
        .to_string()
        .to_lowercase()
        .contains("regex"));
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_zero_maximum_names_do_not_enter_depth_slots() {
  let rules = (0..63)
    .map(|i| format!("g{i} = (g{},)", i + 1))
    .collect::<Vec<_>>()
    .join("\n");
  for maximum in [0, 1] {
    let schema = format!("root = {{ g0 }} {rules} g63 = (0*{maximum} h<uint>,) h<T> = (tstr => T)");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({}), 0);
    let result = validator.validate();
    if maximum == 0 {
      assert!(result.is_ok());
    } else {
      assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_nested_suppression_preserves_enclosing_effects() {
  for (pattern, valid) in [("[", false), ("^x$", true)] {
    let schema =
      format!("root = {{ (required: uint,), (a: uint // b: tstr .regexp \"{pattern}\") }} / any");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"b":"x"}), 2);
    let result = validator.validate();
    assert_eq!(result.is_ok(), valid);
    if !valid {
      assert!(result
        .unwrap_err()
        .to_string()
        .to_lowercase()
        .contains("regex"));
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_wrapped_generic_repetitions_do_not_copy_per_iteration() {
  for schema in [
    "root = { * (g<uint>,) } g<T> = (tstr => T, ? \"x\" => T)",
    "root = { * outer<uint> } outer<T> = (inner<T>,) inner<T> = (tstr => T, ? \"x\" => T)",
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let input = Value::Object(
      (0..128)
        .map(|i| (format!("k{i:04}"), Value::from(i)))
        .collect(),
    );
    let mut validator = limited_validator(&cddl, input, 0);
    assert!(validator.validate().is_ok());
    assert_eq!(
      validator
        .map_replay_budget
        .generic_input_copies
        .load(Ordering::Relaxed),
      0
    );
    validator.json = serde_json::json!({"bad":false}).into();
    assert!(matches!(validator.validate(), Err(Error::Validation(_))));
    let schema = schema.replace("uint", "tstr .regexp \"[\"");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"bad":"x"}), 0);
    assert!(validator
      .validate()
      .unwrap_err()
      .to_string()
      .to_lowercase()
      .contains("regex"));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_required_prefix_failure_does_not_enumerate_suffixes() {
  let suffix = (0..13)
    .map(|i| format!("(a{i}: uint // b{i}: uint)"))
    .collect::<Vec<_>>()
    .join(",");
  let schema = format!("root = {{ required: uint, {suffix}, * tstr => uint }}");
  let cddl = cddl_from_pest_str(&schema).unwrap();
  let mut input = serde_json::json!({});
  for i in 0..13 {
    input[format!("a{i}")] = Value::from(1);
    input[format!("b{i}")] = Value::from(1);
  }
  let mut validator = limited_validator(&cddl, input, 0);
  let Error::Validation(errors) = validator.validate().unwrap_err() else {
    panic!("required prefix must retain its ordinary validation diagnostic")
  };
  assert!(errors
    .iter()
    .any(|e| e.reason.contains("missing key: required")));
  let cddl = cddl_from_pest_str(
    "root = { ? (required: uint, (a: uint // b: uint)), (missing: uint // m: uint) }",
  )
  .unwrap();
  let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
  assert!(
    validator.validate().is_ok(),
    "optional rollback must restore replay eligibility"
  );
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_generic_repetition_shares_input() {
  let cddl = cddl_from_pest_str("root = { * g<uint> } g<T> = (tstr => T, ? \"x\" => T)").unwrap();
  for size in [16, 128] {
    let input = Value::Object(
      (0..size)
        .map(|i| (format!("k{i:04}"), Value::from(i)))
        .collect(),
    );
    let mut validator = limited_validator(&cddl, input, 0);
    assert!(validator.validate().is_ok());
    assert_eq!(
      validator
        .map_replay_budget
        .generic_input_copies
        .load(Ordering::Relaxed),
      0
    );
  }
  for entries in [
    "* g<uint>, * g<tstr>",
    "* (g<uint>,), * (g<tstr>,)",
    "* outer<uint>, * outer<tstr>",
  ] {
    let schema =
      format!("root = {{ {entries} }} g<T> = (tstr => T, ? \"x\" => T) outer<T> = (g<T>,)");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(
      &cddl,
      serde_json::json!({"a":1,"b":2,"c":"three","x":"four"}),
      0,
    );
    for _ in 0..2 {
      assert!(validator.validate().is_ok());
      assert_eq!(
        validator
          .map_replay_budget
          .generic_input_copies
          .load(Ordering::Relaxed),
        0
      );
    }
    validator.json = serde_json::json!({"a":1,"x":false}).into();
    assert!(matches!(validator.validate(), Err(Error::Validation(_))));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_depth_errors_are_distinct_shared_and_reset() {
  let input = Value::Object(
    (0..128)
      .map(|i| (format!("k{i:04}"), Value::from(i)))
      .collect(),
  );
  for schema in [
    "root = { g } g = (tstr => uint, ? g)",
    "root = { ? g } g = (tstr => uint, ? g)",
    "root = { g } / any g = (tstr => uint, ? g)",
    "root = { g<uint> } g<T> = (tstr => T, ? g<T>)",
    "root = { g } g //= (tstr => uint, ? g)",
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, input.clone(), 0);
    let error = validator.validate().unwrap_err();
    assert!(
      matches!(
        error,
        Error::RecursionLimit {
          limit: MAX_ACTIVE_MAP_RULE_CALLS
        }
      ),
      "{}",
      schema
    );
    assert!(error.to_string().starts_with("JSON validation incomplete:"));
    assert_eq!(validator.map_rule_depth.active.load(Ordering::Relaxed), 0);
    validator.json = serde_json::json!({"small":1}).into();
    assert!(validator.validate().is_ok(), "{}", schema);
  }
  for schema in ["root = { g } g = (g,)", "root = { g<uint> } g<T> = (g<T>,)"] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({}), 0);
    assert!(matches!(validator.validate(), Err(Error::Validation(_))));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_unknown_key_proof_preserves_named_depth_errors() {
  let rules = (0..80)
    .map(|i| format!("g{i} = (g{},)", i + 1))
    .collect::<Vec<_>>()
    .join("\n");
  // Analyze the shared suffix first; a later caller has less remaining depth.
  let schema = format!("root = {{ fixed: uint // g0 // g20 }} {rules} g80 = (fixed: uint)");
  let cddl = cddl_from_pest_str(&schema).unwrap();
  let mut validator = limited_validator(&cddl, serde_json::json!({"fixed":1,"unknown":true}), 1);
  assert!(matches!(
    validator.validate(),
    Err(Error::RecursionLimit { .. })
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_proven_optional_choices_preserve_required_diagnostics() {
  for domain in ["alias", "uint"] {
    let schema =
      format!("root = {{ ? (a: {domain} // b: {domain}), required: uint, c: uint }} alias = uint");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator =
      limited_validator(&cddl, serde_json::json!({"a":"text","b":"text","c":1}), 1);
    let Error::Validation(errors) = validator.validate().unwrap_err() else {
      panic!("ordinary required mismatch")
    };
    assert_eq!(errors.len(), 1);
    assert!(errors[0].reason.contains("missing key: required"));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_complete_remaining_pairs_can_batch_after_movable_claims() {
  for (schema, eligible, valid) in [
    ("root = { * (tstr => uint), * (tstr => tstr) }", true, true),
    (
      "root = { * fields<uint>, * fields<tstr> } fields<T> = (tstr => T)",
      true,
      true,
    ),
    (
      "root = { 0*1 (tstr => uint), * (tstr => tstr), * tstr => uint }",
      false,
      true,
    ),
    (
      "root = { * (tstr => uint), * (tstr => (tstr .size 1)) }",
      false,
      true,
    ),
    (
      "root = { * (tstr => tstr), * (tstr => uint) } uint = tstr",
      false,
      false,
    ),
  ] {
    let cddl = cddl_from_pest_str(schema).unwrap();
    let Rule::Type { rule, .. } = &cddl.rules[0] else {
      panic!("expected type rule");
    };
    let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("expected map");
    };
    let entries = &group.group_choices[0].group_entries;
    let mut validator =
      limited_validator(&cddl, serde_json::json!({"a":1,"b":2,"c":"x","d":"y"}), 0);
    validator.visit_group_entry(&entries[0].0).unwrap();
    assert!(!validator.member_claims.is_empty());
    let member = match &entries[1].0 {
      GroupEntry::InlineGroup { group, .. } => validator
        .batchable_map_member(group, None, &validator, 0)
        .cloned(),
      GroupEntry::TypeGroupname { ge, .. } => validator.batchable_named_map_member(ge, false),
      _ => panic!("expected repeated group"),
    };
    assert_eq!(member.is_some(), eligible, "{schema}");
    assert_eq!(validator.validate().is_ok(), valid, "{schema}");
  }
  let cddl =
    cddl_from_pest_str("root = { tstr => any, * fields<uint, tstr> } fields<T> = (tstr => T)")
      .unwrap();
  let mut validator = limited_validator(&cddl, serde_json::json!({"a":1,"b":"x"}), 0);
  assert!(validator.validate().is_ok());
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_pruned_coverage_resets_on_reuse() {
  let cddl = cddl_from_pest_str("root = { (a: uint // b: uint), c: uint, d: uint }").unwrap();
  let input = serde_json::json!({"a":1,"c":"x","d":1});
  let mut validator = limited_validator(&cddl, input.clone(), 1);
  let error = validator.validate().unwrap_err().to_string();
  assert!(error.contains("expected type uint"));
  assert!(!error.contains("unexpected key \"d\""));
  validator.json = serde_json::json!({"a":1,"c":1,"d":1,"extra":2}).into();
  assert!(validator
    .validate()
    .unwrap_err()
    .to_string()
    .contains("unexpected key \"extra\""));
  validator.json = serde_json::json!({"a":1,"c":1,"d":1}).into();
  assert!(validator.validate().is_ok());
  let mut exhausted = limited_validator(&cddl, input, 0);
  assert!(matches!(
    exhausted.validate(),
    Err(Error::ResourceLimit { limit: 0 })
  ));
}

fn reference_assignment(
  claim: usize,
  rows: &[Arc<Vec<bool>>],
  visited: &mut [bool],
  owners: &mut [Option<usize>],
  checks: &mut usize,
) -> bool {
  for pair in 0..rows[claim].len() {
    *checks += 1;
    if !rows[claim][pair] || visited[pair] {
      continue;
    }
    visited[pair] = true;
    if owners[pair].is_none_or(|owner| reference_assignment(owner, rows, visited, owners, checks)) {
      owners[pair] = Some(claim);
      return true;
    }
  }
  false
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_class_cursors_preserve_dfs_assignments() {
  for pattern in 0usize..512 {
    for share_rows in [false, true] {
      let mut rows: ClaimCompatibility = Vec::new();
      for claim in 0..3 {
        let values: Vec<_> = (0..3)
          .map(|pair| pattern & (1 << (claim * 3 + pair)) != 0)
          .collect();
        let row = if share_rows {
          rows
            .iter()
            .find(|row| row.as_ref() == &values)
            .cloned()
            .unwrap_or_else(|| Arc::new(values))
        } else {
          Arc::new(values)
        };
        rows.push(row);
      }
      let (classes, count) = compatibility_classes(&rows);
      let mut actual = vec![None; 3];
      let mut expected = actual.clone();
      for claim in 0..3 {
        let matched = augment_choice_assignment(
          claim,
          &rows,
          &mut [false; 3],
          &mut actual,
          false,
          &classes,
          &mut vec![0; count],
        );
        let reference = reference_assignment(claim, &rows, &mut [false; 3], &mut expected, &mut 0);
        assert_eq!(matched, reference);
        assert_eq!(actual, expected, "graph {pattern}, sharing {share_rows}");
      }
    }
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn map_replay_mixed_classes_bound_pair_scans() {
  let pairs = 257;
  let mut rows = vec![Arc::new(vec![true; pairs]); pairs - 1];
  rows.push(Arc::new(vec![true; pairs]));
  let (classes, class_count) = compatibility_classes(&rows);
  let mut actual = vec![None; pairs];
  let mut expected = actual.clone();
  let mut scans = 0;
  let mut reference_scans = 0;
  for claim in 0..rows.len() {
    let mut positions = vec![0; class_count];
    assert!(augment_choice_assignment(
      claim,
      &rows,
      &mut vec![false; pairs],
      &mut actual,
      false,
      &classes,
      &mut positions,
    ));
    scans += positions.iter().sum::<usize>();
    assert!(reference_assignment(
      claim,
      &rows,
      &mut vec![false; pairs],
      &mut expected,
      &mut reference_scans,
    ));
    assert_eq!(actual, expected);
  }
  assert!(scans > 0 && scans <= class_count * pairs * rows.len());
  assert!(reference_scans > scans * 8);
  #[cfg(not(target_arch = "wasm32"))]
  eprintln!("mixed-class pair scans: {scans}; reference: {reference_scans}");
}
