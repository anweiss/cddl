//! RFC 8610 Appendix D and RFC 8949 sections 5.3.1/5.6.1 regressions.
#![cfg(all(feature = "std", feature = "cbor", not(feature = "lsp")))]

fn accepts(schema: &str, bytes: &[u8]) -> bool {
  #[cfg(feature = "additional-controls")]
  let result = cddl::validator::validate_cbor_from_slice(schema, bytes, None);
  #[cfg(not(feature = "additional-controls"))]
  let result = cddl::validator::validate_cbor_from_slice(schema, bytes);
  result.is_ok()
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn integer_signs_and_controls() {
  for ty in ["uint", "unsigned", "nint", "int", "integer", "number"] {
    for (bytes, negative) in [(&[0x20][..], true), (&[0x00][..], false)] {
      let expected = match ty {
        "uint" | "unsigned" => !negative,
        "nint" => negative,
        _ => true,
      };
      for schema in [
        format!("x = {ty}"),
        format!("x = alias\nalias = {ty}"),
        format!("x = {ty} .eq {}", if negative { "-1" } else { "0" }),
        format!(
          "x = alias .eq {}\nalias = {ty}",
          if negative { "-1" } else { "0" }
        ),
      ] {
        assert_eq!(accepts(&schema, bytes), expected, "{schema}");
      }
    }
  }
  assert!(accepts("x = alias .eq -1\nalias = uint / nint", &[0x20]));
  assert!(accepts("x = alias .eq -1\nalias = unsigned / any", &[0x20]));
  assert!(accepts("x = alias .eq 1\nalias = 1 / nint", &[0x01]));
  assert!(!accepts(
    "x = alias .eq -1\nalias = unsigned / float",
    &[0x20]
  ));
  assert!(!accepts("x = unsigned .eq 1", &[0x00]));
  for other in ["bool", "null", "undefined", "#7.23"] {
    assert!(!accepts(
      &format!("x = alias .eq -1\nalias = unsigned / {other}"),
      &[0x20]
    ));
  }
  assert!(!accepts("x = uint .eq [1]", &[0x81, 0x01]));
  assert!(!accepts(
    "x = alias .eq 0\nalias = unsigned .gt 10",
    &[0x00]
  ));
  for (major, valid, invalid) in [(0x1b, "unsigned", "nint"), (0x3b, "nint", "unsigned")] {
    let bytes = [major, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff];
    assert!(accepts(&format!("x = {valid}"), &bytes));
    assert!(!accepts(&format!("x = {invalid}"), &bytes));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn tagged_primitive_membership() {
  for ty in ["tstr", "bool", "uint", "number", "null", "undefined"] {
    assert!(
      !accepts(&format!("x = {ty}"), &[0xc2, 0x41, 0x01]),
      "{}",
      ty
    );
  }
  for (ty, positive, negative) in [
    ("biguint", true, false),
    ("bignint", false, true),
    ("bigint", true, true),
    ("unsigned", true, false),
    ("integer", true, true),
  ] {
    for (tag, expected) in [(0xc2, positive), (0xc3, negative)] {
      assert_eq!(
        accepts(&format!("x = {ty}"), &[tag, 0x41, 0x01]),
        expected,
        "{ty}"
      );
      assert!(!accepts(&format!("x = {ty}"), &[tag, 0x01]), "{}", ty);
    }
  }
  assert!(accepts("x = any", &[0xd8, 42, 0x01]));
  assert!(accepts("x = tstr / #6.42(uint)", &[0xd8, 42, 0x01]));
  assert!(!accepts("x = tstr / #6.42(uint)", &[0xd8, 42, 0x61, b'x']));
  assert!(!accepts("x = tstr / #6.42(uint)", &[0xd8, 43, 0x01]));
  for schema in ["x = { unsigned => uint }", "x = { * unsigned => uint }"] {
    assert!(accepts(schema, &[0xa1, 0xc2, 0x41, 0x01, 0x01]));
    assert!(!accepts(schema, &[0xa1, 0xc3, 0x41, 0x01, 0x01]));
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn prelude_payloads_and_base64_policy() {
  for (ty, tag, payload) in [
    ("uri", 32, &[0x61, b'x'][..]),
    ("b64url", 33, &[0x62, b'Z', b'g'][..]),
    ("b64legacy", 34, &[0x64, b'Z', b'g', b'=', b'='][..]),
    ("regexp", 35, &[0x61, b'x'][..]),
    ("mime-message", 36, &[0x61, b'x'][..]),
    ("encoded-cbor", 24, &[0x41, 0x01][..]),
  ] {
    let mut bytes = vec![0xd8, tag];
    bytes.extend_from_slice(payload);
    assert!(accepts(&format!("x = {ty}"), &bytes), "{}", ty);
    assert!(!accepts(&format!("x = {ty}"), &[0xd8, tag, 0x01]), "{}", ty);
    bytes[1] = 42;
    assert!(!accepts(&format!("x = {ty}"), &bytes), "{}", ty);
  }
  assert!(!accepts("x = b64url", &[0xd8, 33, 0x61, b'x']));
  assert!(accepts("x = eb64url", &[0xd5, 0x01]));
  assert!(accepts("x = cbor-any", &[0xd9, 0xd9, 0xf7, 0x01]));
  assert!(accepts("x = time", &[0xc1, 0x01]));
  assert!(!accepts("x = time", &[0xc1, 0x61, b'x']));
  assert!(!accepts(
    "x = time",
    &[0xc1, 0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn undefined_is_not_null() {
  for schema in ["x = undefined", "x = #7.23", "x = alias\nalias = undefined"] {
    assert!(accepts(schema, &[0xf7]), "{}", schema);
    assert!(!accepts(schema, &[0xf6]), "{}", schema);
  }
  for schema in ["x = null", "x = nil", "x = #7.22"] {
    assert!(!accepts(schema, &[0xf7]), "{}", schema);
    assert!(accepts(schema, &[0xf6]), "{}", schema);
  }
  assert!(accepts("x = [undefined, null]", &[0x82, 0xf7, 0xf6]));
  assert!(accepts(
    "x = { undefined => uint, null => uint }",
    &[0xa2, 0xf7, 0x01, 0xf6, 0x02]
  ));
  assert!(accepts("x = any", &[0xf7]));
  assert!(accepts("x = { * undefined => uint }", &[0xa1, 0xf7, 0x01]));
  assert!(!accepts("x = { * undefined => uint }", &[0xa1, 0xf6, 0x01]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn duplicate_text_keys_are_invalid_even_under_any() {
  let duplicate = [0xa2, 0x61, b'a', 0x01, 0x61, b'a', 0x02];
  for schema in ["x = { * tstr => uint }", "x = any"] {
    assert!(!accepts(schema, &duplicate), "{}", schema);
  }
  let mut nested = vec![0x81];
  nested.extend_from_slice(&duplicate);
  assert!(!accepts("x = any", &nested));
  let mut encoded = vec![0x40 + duplicate.len() as u8];
  encoded.extend_from_slice(&duplicate);
  assert!(!accepts("x = bstr .cbor any", &encoded));
  assert!(!accepts(
    "x = any",
    &[0xbf, 0x61, b'a', 1, 0x7f, 0x61, b'a', 0xff, 2, 0xff]
  ));
  assert!(accepts("x = any", &[0xa2, 0x61, b'a', 1, 0x41, b'a', 2]));
  assert!(accepts(
    "x = { * tstr => uint }",
    &[0xa2, 0x61, b'a', 1, 0x61, b'b', 2]
  ));
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn constructed_values_cannot_bypass_duplicate_detection() {
  use cddl::validator::{cbor::CBORValidator, cbor_value::Value, Validator};
  let cddl = cddl::cddl_from_str("x = any", false).unwrap();
  let value = Value::Map(vec![
    (Value::Text("a".into()), Value::Integer(1.into())),
    (Value::Text("a".into()), Value::Integer(2.into())),
  ]);
  #[cfg(feature = "additional-controls")]
  let mut validator = CBORValidator::new(&cddl, value, None);
  #[cfg(not(feature = "additional-controls"))]
  let mut validator = CBORValidator::new(&cddl, value);
  let result = Validator::<cddl::validator::cbor::Error<std::io::Error>>::validate(&mut validator);
  assert!(result
    .unwrap_err()
    .to_string()
    .contains("duplicate text key"));
}

#[cfg(feature = "additional-controls")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn computed_numeric_controls_validate_the_result_not_the_operand() {
  assert!(accepts("x = one .plus 1\none = 1 / uint .eq 1", &[0x02]));
  assert!(!accepts("x = one .plus 1\none = 1 / uint .eq 1", &[0x01]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn nested_target_validating_controls_do_not_repeat_target_probes() {
  for control in ["and", "within", "default"] {
    let mut schema = String::from("x = n32\nn0 = uint\n");
    for level in 1..=32 {
      schema.push_str(&format!("n{level} = n{} .{control} uint\n", level - 1));
    }
    assert!(accepts(&schema, &[0x01]), "{}", control);
    assert!(!accepts(&schema, &[0x20]), "{}", control);
  }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn default_controls_preserve_complete_target_membership() {
  for target in ["1 / uint", "1 / uint .eq 1"] {
    assert!(!accepts(
      &format!("x = t .default 2\nt = {target}"),
      &[0x20]
    ));
    assert!(!accepts(
      &format!("x = {{ ? a: t .default 2 }}\nt = {target}"),
      &[0xa1, 0x61, b'a', 0x20]
    ));
  }
  assert!(accepts("x = t .default 2\nt = nint / uint", &[0x20]));
  assert!(accepts(
    "x = { ? a: t .default 2 }\nt = nint / uint",
    &[0xa1, 0x61, b'a', 0x20]
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn comparison_controllers_do_not_constrain_the_instance_domain() {
  for (operator, controller, value) in [
    ("gt", "nint .eq -1", 0x00),
    ("ge", "nint .eq -1", 0x00),
    ("lt", "uint .eq 1", 0x20),
    ("le", "uint .eq 1", 0x20),
    ("ne", "nint .eq -1", 0x00),
  ] {
    assert!(
      accepts(
        &format!("x = int .{operator} bound\nbound = {controller}"),
        &[value]
      ),
      "{}",
      operator
    );
  }
  assert!(!accepts("x = int .gt bound\nbound = nint .eq -1", &[0x21]));
  assert!(!accepts("x = uint .gt bound\nbound = nint .eq -1", &[0x20]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn comparison_controllers_respect_their_own_domains() {
  for (schema, value) in [
    ("x = int .lt bound\nbound = uint .eq -1", 0x21),
    ("x = int .gt bound\nbound = nint .eq 1", 0x02),
    ("x = int .gt bound\nbound = uint .eq -1", 0x00),
    ("x = int .gt bound\nbound = nint .eq one\none = 1", 0x02),
    ("x = int .lt bound<-1>\nbound<T> = uint .eq T", 0x21),
  ] {
    assert!(!accepts(schema, &[value]), "{}", schema);
  }
  assert!(accepts("x = int .lt bound\nbound = uint .eq 1", &[0x21]));
  assert!(accepts(
    "x = int .gt bound<-1>\nbound<T> = nint .eq T",
    &[0x00]
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn mixed_numeric_targets_preserve_nonnumeric_members() {
  assert!(accepts("x = v .and any\nv = uint / any", &[0xf6]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn duplicate_failed_probes_do_not_accept_array_choices() {
  assert!(!accepts("x = (uint .eq 1 / uint .eq 2)", &[0x80]));
  assert!(accepts("x = (uint .eq 1 / any)", &[0x80]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn repeated_numeric_alias_probes_remain_bounded() {
  for (controller, depth) in [("n", 32), ("alias", 16)] {
    let mut schema = format!("x = n{depth}\nn0 = uint .eq 1\nalias0 = n0\n");
    for level in 1..=depth {
      schema.push_str(&format!(
        "n{level} = n{0} .eq {controller}{0}\nalias{level} = n{level}\n",
        level - 1
      ));
    }
    assert!(accepts(&schema, &[0x01]));
    assert!(!accepts(&schema, &[0x02]));
  }
  assert!(!accepts(
    "x = [cell, cell]\ncell = uint .eq 1",
    &[0x82, 0x01, 0x02]
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn numeric_probe_cache_preserves_generic_context() {
  assert!(!accepts(
    "x = g<1> .and nint / g<1.0> .and uint\ng<T> = uint .eq T",
    &[0x01]
  ));
  let rules = "\ncheck<T,U> = bound<T> .eq U\nbound<T> = uint .eq T";
  assert!(!accepts(
    &format!("x = check<0,1> / check<1,0>{rules}"),
    &[0x00]
  ));
  assert!(accepts(&format!("x = check<1,1>{rules}"), &[0x01]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn numeric_size_and_bits_controllers_use_counts_not_instances() {
  let rules = "\nbound = one .eq 1\none = uint .eq 1";
  assert!(accepts(
    &format!("x = uint .size bound{rules}"),
    &[0x18, 0xff]
  ));
  assert!(!accepts(
    &format!("x = uint .size bound{rules}"),
    &[0x19, 0x01, 0x00]
  ));
  assert!(accepts(&format!("x = uint .bits bound{rules}"), &[0x02]));
  assert!(!accepts(&format!("x = uint .bits bound{rules}"), &[0x01]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn repeated_generic_numeric_alias_probes_remain_bounded() {
  let mut schema = String::from("x = n32<1>\nn0<T> = uint .eq T\n");
  for level in 1..=32 {
    schema.push_str(&format!("n{level}<T> = n{0}<T> .eq n{0}<T>\n", level - 1));
  }
  assert!(accepts(&schema, &[0x01]));
  assert!(!accepts(&schema, &[0x02]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn alias_wrapped_generic_target_proofs_reuse_relevant_bindings() {
  let mut schema = String::from("x = n24<1>\nn0<T> = uint .eq T\na0<T> = n0<T>\n");
  for level in 1..=24 {
    schema.push_str(&format!(
      "n{level}<T> = n{prev}<T> .eq a{prev}<T>\na{level}<T> = n{level}<T>\n",
      prev = level - 1
    ));
  }
  assert!(accepts(&schema, &[0x01]));
  assert!(!accepts(&schema, &[0x02]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn failed_numeric_target_alias_proofs_remain_bounded() {
  let mut schema = String::from("x = n32\nn0 = uint .eq 1\n");
  for level in 1..=32 {
    schema.push_str(&format!(
      "n{level} = n{prev} .eq 1 / n{prev} .eq 2\n",
      prev = level - 1
    ));
  }
  assert!(!accepts(&schema, &[0x00]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn generic_controller_children_keep_size_and_bits_context() {
  assert!(accepts(
    "x = tstr .size bound<1>\nbound<T> = T",
    &[0x61, b'a']
  ));
  assert!(accepts(
    "x = bstr .bits bound<0>\nbound<T> = T",
    &[0x41, 0x01]
  ));
  assert!(accepts(
    "x = tstr .size bound<1>\nbound<T> = uint .eq T",
    &[0x61, b'a']
  ));
  assert!(accepts(
    "x = bstr .bits bound<0>\nbound<T> = uint .eq T",
    &[0x41, 0x01]
  ));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn comparison_controller_chain_proofs_are_reused() {
  let mut schema = String::from("x = n24\nn0 = uint .eq 1\n");
  for level in 1..=24 {
    schema.push_str(&format!(
      "n{level} = n{0} .ge b{level}\nb{level} = n{0} .eq 1\n",
      level - 1
    ));
  }
  assert!(accepts(&schema, &[0x01]));
  assert!(!accepts(&schema, &[0x02]));
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn generic_literal_controller_proofs_are_shared() {
  let mut schema = String::from("x = n24\nliteral<T> = T\nn0 = uint .eq 1\n");
  for level in 1..=24 {
    schema.push_str(&format!(
      "n{level} = n{prev} .ge b{level}\nb{level} = n{prev} .eq literal<1>\n",
      prev = level - 1
    ));
  }
  assert!(accepts(&schema, &[0x01]));
  assert!(!accepts(&schema, &[0x02]));
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn duplicate_text_keys_are_data_errors_not_schema_errors() {
  let bytes = [0xa2, 0x61, b'a', 0x01, 0x61, b'a', 0x02];
  #[cfg(feature = "additional-controls")]
  let error = cddl::validator::validate_cbor_from_slice("x = any", &bytes, None).unwrap_err();
  #[cfg(not(feature = "additional-controls"))]
  let error = cddl::validator::validate_cbor_from_slice("x = any", &bytes).unwrap_err();
  assert!(matches!(
    error,
    cddl::validator::cbor::Error::CBORParsing(_)
  ));
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn nontext_equivalent_pairs_still_exercise_claim_propagation() {
  let bytes = [0xa2, 0x01, 0x61, b'x', 0x01, 0x41, b'z'];
  for schema in [
    "m = { ? int => tstr, 1: tstr, ? int => uint }",
    "g<K,V> = (K => V)\nm = { g<int,tstr>, 1: tstr, ? int => uint }",
    "g<K,V> = (K => V)\nm = { ? int => tstr, g<int,tstr>, ? int => uint }",
  ] {
    #[cfg(feature = "additional-controls")]
    let error = cddl::validator::validate_cbor_from_slice(schema, &bytes, None).unwrap_err();
    #[cfg(not(feature = "additional-controls"))]
    let error = cddl::validator::validate_cbor_from_slice(schema, &bytes).unwrap_err();
    assert!(matches!(error, cddl::validator::cbor::Error::Validation(_)));
    assert!(
      error.to_string().contains("expected type tstr"),
      "{}",
      error
    );
  }
}
