#![cfg(feature = "std")]
#![cfg(not(feature = "lsp"))]
#![cfg(not(target_arch = "wasm32"))]

#[cfg(feature = "cbor")]
#[test]
fn cbor_undefined_group_socket_matches_no_map_entries() {
  for occurrence in ["*", "?", "0*2", "*2"] {
    for (schema, bytes, valid) in [
      (format!("m = {{ {occurrence} $$s }}"), vec![0xa0], true),
      (
        format!("m = {{ {occurrence} $$s }}"),
        vec![0xa1, 0x09, 0x01],
        false,
      ),
      (
        format!("m = {{ 9 => int, {occurrence} $$s }}"),
        vec![0xa1, 0x09, 0x01],
        true,
      ),
      (
        format!("m = {{ {occurrence} $$s, 9 => int }}"),
        vec![0xa0],
        false,
      ),
    ] {
      let result = cddl::validate_cbor_from_slice(
        &schema,
        &bytes,
        #[cfg(feature = "additional-controls")]
        None,
      );
      assert_eq!(result.is_ok(), valid, "{schema}: {result:?}");
    }
  }
}

#[cfg(feature = "json")]
#[test]
fn json_undefined_group_socket_matches_no_map_entries() {
  for occurrence in ["*", "?", "0*2", "*2"] {
    for (schema, json, valid) in [
      (format!("m = {{ {occurrence} $$s }}"), "{}", true),
      (format!("m = {{ {occurrence} $$s }}"), r#"{"k":1}"#, false),
      (
        format!("m = {{ k: int, {occurrence} $$s }}"),
        r#"{"k":1}"#,
        true,
      ),
      (format!("m = {{ {occurrence} $$s, k: int }}"), "{}", false),
    ] {
      let result = cddl::validate_json_from_str(
        &schema,
        json,
        #[cfg(feature = "additional-controls")]
        None,
      );
      assert_eq!(result.is_ok(), valid, "{schema}: {result:?}");
    }
  }
}

#[cfg(feature = "cbor")]
#[test]
fn cbor_defined_group_socket_still_validates_its_entries() {
  let schema = "m = { * $$s }\n$$s //= ( 9 => int )";
  for (bytes, valid) in [
    (vec![0xa1, 0x09, 0x01], true),
    (vec![0xa1, 0x09, 0xf6], false),
    (vec![0xa1, 0x0a, 0x01], false),
  ] {
    let result = cddl::validate_cbor_from_slice(
      schema,
      &bytes,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), valid, "{result:?}");
  }
}

#[cfg(feature = "json")]
#[test]
fn json_defined_group_socket_still_validates_its_entries() {
  let schema = "m = { * $$s }\n$$s //= ( k: int )";
  for (json, valid) in [
    (r#"{"k":1}"#, true),
    (r#"{"k":null}"#, false),
    (r#"{"other":1}"#, false),
  ] {
    let result = cddl::validate_json_from_str(
      schema,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), valid, "{result:?}");
  }
}

#[cfg(feature = "cbor")]
#[test]
fn cbor_undefined_group_socket_cannot_satisfy_required_occurrences() {
  for occurrence in ["", "+", "1*2"] {
    let schema = format!("m = {{ {occurrence} $$s }}");
    assert!(cddl::validate_cbor_from_slice(
      &schema,
      &[0xa0],
      #[cfg(feature = "additional-controls")]
      None,
    )
    .is_err());
  }
}

#[cfg(feature = "json")]
#[test]
fn json_undefined_group_socket_cannot_satisfy_required_occurrences() {
  for occurrence in ["", "+", "1*2"] {
    let schema = format!("m = {{ {occurrence} $$s }}");
    assert!(cddl::validate_json_from_str(
      &schema,
      "{}",
      #[cfg(feature = "additional-controls")]
      None,
    )
    .is_err());
  }
}

#[cfg(feature = "cbor")]
#[test]
fn cbor_optional_group_around_undefined_socket_may_be_absent() {
  for schema in ["m = { ? ($$s) }", "m = { * ($$s) }"] {
    for (bytes, valid) in [(vec![0xa0], true), (vec![0xa1, 0x09, 0x01], false)] {
      let result = cddl::validate_cbor_from_slice(
        schema,
        &bytes,
        #[cfg(feature = "additional-controls")]
        None,
      );
      assert_eq!(result.is_ok(), valid, "{schema}: {result:?}");
    }
  }
}

#[cfg(feature = "json")]
#[test]
fn json_optional_group_around_undefined_socket_may_be_absent() {
  for schema in ["m = { ? ($$s) }", "m = { * ($$s) }"] {
    for (json, valid) in [("{}", true), (r#"{"k":1}"#, false)] {
      let result = cddl::validate_json_from_str(
        schema,
        json,
        #[cfg(feature = "additional-controls")]
        None,
      );
      assert_eq!(result.is_ok(), valid, "{schema}: {result:?}");
    }
  }
}

#[cfg(feature = "cbor")]
#[test]
fn cbor_failed_optional_group_returns_claimed_pairs() {
  // `{"a": "x"}`: the failed optional group must not keep the "a" pair.
  for (schema, valid) in [
    ("m = { ? (a: int), * tstr => tstr }", true),
    ("m = { ? (a: int) }", false),
  ] {
    let result = cddl::validate_cbor_from_slice(
      schema,
      &[0xa1, 0x61, 0x61, 0x61, 0x78],
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), valid, "{schema}: {result:?}");
  }
}

#[cfg(feature = "cbor")]
#[test]
fn cbor_skipped_optional_group_keeps_later_members_required() {
  // Skipping `? ($$s)` must not leak its `?` into the required `k: int`.
  for schema in ["m = { ? ($$s), k: int }", "m = { * ($$s), k: int }"] {
    for (bytes, valid) in [(vec![0xa0], false), (vec![0xa1, 0x61, 0x6b, 0x01], true)] {
      let result = cddl::validate_cbor_from_slice(
        schema,
        &bytes,
        #[cfg(feature = "additional-controls")]
        None,
      );
      assert_eq!(result.is_ok(), valid, "{schema}: {result:?}");
    }
  }
}

#[cfg(feature = "json")]
#[test]
fn json_skipped_optional_group_keeps_later_members_required() {
  for schema in ["m = { ? ($$s), k: int }", "m = { * ($$s), k: int }"] {
    for (json, valid) in [("{}", false), (r#"{"k":1}"#, true)] {
      let result = cddl::validate_json_from_str(
        schema,
        json,
        #[cfg(feature = "additional-controls")]
        None,
      );
      assert_eq!(result.is_ok(), valid, "{schema}: {result:?}");
    }
  }
}
