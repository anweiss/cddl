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
fn cbor_range_keys_apply_to_each_key() {
  for schema in [
    "m = { * (10..20) => uint }",
    "m = { * key => uint }\nkey = 10..20",
    "m = { * (1 / 10..20) => uint }",
  ] {
    // {}, {10: 1}, {20: 1}, {10: 1, 15: 2}
    for hex in ["a0", "a10a01", "a11401", "a20a010f02"] {
      check_cbor(schema, hex, true);
    }
    // {21: 1}, {9: 1}, {10: "x"}, {10: 1, 21: 2}
    for hex in ["a1181501", "a10901", "a10a6178", "a20a01181502"] {
      check_cbor(schema, hex, false);
    }
  }
  // Range keys keep working next to other map entries.
  let schema = "m = { 0: uint, * (10..20) => uint }";
  check_cbor(schema, "a1000a", true);
  check_cbor(schema, "a2000a0a01", true);
  check_cbor(schema, "a2000a181501", false);
}
