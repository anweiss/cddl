#![cfg(all(feature = "std", not(feature = "lsp")))]

#[cfg(feature = "json")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn json_dates_and_times() {
  for (schema, input, expected) in [
    ("root = tdate", r#""2024-02-29T12:34:56Z""#, true),
    ("root = tdate", r#""2024-02-29t12:34:56.123+02:30""#, true),
    (
      "root = tdate",
      r#""2024-02-29T12:34:56.123456789123Z""#,
      true,
    ),
    ("root = tdate", r#""2023-02-29T12:34:56Z""#, false),
    ("root = tdate", r#""2024-02-29T12:34:56""#, false),
    ("root = tdate", r#""20240229T123456Z""#, false),
    ("root = tdate", r#""2024-02-29T12:34:56Z[UTC]""#, false),
    ("root = time", "0", true),
    ("root = time", "-1.25", true),
    ("root = time", "253402207200", true),
    ("root = time", "253402207201", false),
    ("root = time", "-377705023201", true),
    ("root = time", "-377705023202", false),
    ("root = time", "253402300800", false),
    ("root = time", "9223372036854775807", false),
    ("root = time", "1e100", false),
  ] {
    let result = cddl::validate_json_from_str(
      schema,
      input,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert_eq!(result.is_ok(), expected, "{schema}: {input}: {result:?}");
  }
}

#[cfg(feature = "cbor")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
#[cfg_attr(not(target_arch = "wasm32"), test)]
fn cbor_dates_and_times() {
  use ciborium::value::{Integer, Value};

  for (schema, value, expected) in [
    (
      "root = tdate",
      Value::Text("2024-02-29T12:34:56Z".into()),
      true,
    ),
    (
      "root = tdate",
      Value::Text("2023-02-29T12:34:56Z".into()),
      false,
    ),
    (
      "root = tdate",
      Value::Text("20240229T123456Z".into()),
      false,
    ),
    ("root = time", Value::Integer(Integer::from(0)), true),
    (
      "root = time",
      Value::Integer(Integer::from(u64::MAX)),
      false,
    ),
    ("root = time", Value::Float(-1.25), true),
    ("root = time", Value::Float(f64::NAN), false),
    ("root = time", Value::Float(f64::INFINITY), false),
    ("root = time", Value::Float(1e100), false),
  ] {
    for tagged in [false, true] {
      let tag = if schema == "root = tdate" { 0 } else { 1 };
      let value = if tagged {
        Value::Tag(tag, Box::new(value.clone()))
      } else {
        value.clone()
      };
      let mut bytes = Vec::new();
      ciborium::ser::into_writer(&value, &mut bytes).unwrap();
      let result = cddl::validate_cbor_from_slice(
        schema,
        &bytes,
        #[cfg(feature = "additional-controls")]
        None,
      );
      assert_eq!(result.is_ok(), expected, "{schema}: {value:?}: {result:?}");
    }
  }
}
