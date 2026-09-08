#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(feature = "additional-controls")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::validator::validate_cbor_from_slice;

fn cbor_string(major_type: u8, value: &[u8]) -> Vec<u8> {
  let mut encoded = vec![(major_type << 5) | value.len() as u8];
  encoded.extend_from_slice(value);
  encoded
}

fn cbor_bytes(value: &[u8]) -> Vec<u8> {
  cbor_string(2, value)
}

fn cbor_text(value: &str) -> Vec<u8> {
  cbor_string(3, value.as_bytes())
}

fn cbor_map(key: &[u8]) -> Vec<u8> {
  let mut encoded = vec![0xa1];
  encoded.extend_from_slice(key);
  encoded.push(0x01);
  encoded
}

fn cbor_array(item: &[u8]) -> Vec<u8> {
  let mut encoded = vec![0x81];
  encoded.extend_from_slice(item);
  encoded
}

fn assert_verdict(schema: &str, encoded: &[u8], accept: bool) {
  let result = validate_cbor_from_slice(schema, encoded, None);
  assert_eq!(
    result.is_ok(),
    accept,
    "schema {schema:?} with CBOR {encoded:02x?} should {}: {result:?}",
    if accept { "accept" } else { "reject" },
  );
}

#[test]
fn decoded_byte_literals_require_the_byte_string_major_type() {
  let bytes = cbor_bytes(b"AA");
  let text = cbor_text("AA");

  for literal in ["'AA'", "h'4141'", "b64'QUE='"] {
    let schema = format!("m = {literal}");
    assert_verdict(&schema, &bytes, true);
    assert_verdict(&schema, &text, false);
  }
}

#[test]
fn text_literals_reject_equal_bytes_without_panicking() {
  // Reverse control for the major-type boundary. The rejecting row is the
  // former absent-control unwrap panic: a text literal against CBOR bytes
  // must return a normal mismatch, not abort the process. Kept standalone
  // so it fails on its own on the pre-fix tree.
  assert_verdict("m = \"AA\"", &cbor_text("AA"), true);
  assert_verdict("m = \"AA\"", &cbor_bytes(b"AA"), false);
}

#[test]
fn aliased_decoded_byte_literals_keep_their_major_type() {
  let bytes = cbor_bytes(b"AA");
  let text = cbor_text("AA");

  for literal in ["'AA'", "h'4141'", "b64'QUE='"] {
    let schema = format!("m = payload\npayload = {literal}");
    assert_verdict(&schema, &bytes, true);
    assert_verdict(&schema, &text, false);
  }
}

#[test]
fn decoded_byte_literal_map_keys_keep_their_major_type() {
  let byte_key_map = cbor_map(&cbor_bytes(b"AA"));
  let text_key_map = cbor_map(&cbor_text("AA"));

  for literal in ["'AA'", "h'4141'", "b64'QUE='"] {
    let schema = format!("m = {{ {literal} => uint }}");
    assert_verdict(&schema, &byte_key_map, true);
    assert_verdict(&schema, &text_key_map, false);
  }
}

#[test]
fn decoded_byte_literal_array_elements_keep_their_major_type() {
  let byte_item = cbor_array(&cbor_bytes(b"AA"));
  let text_item = cbor_array(&cbor_text("AA"));

  for literal in ["'AA'", "h'4141'", "b64'QUE='"] {
    let schema = format!("m = [{literal}]");
    assert_verdict(&schema, &byte_item, true);
    assert_verdict(&schema, &text_item, false);
  }

  // Text-literal element controls; the rejecting row is the array route of
  // the former absent-control unwrap panic.
  assert_verdict("m = [\"AA\"]", &text_item, true);
  assert_verdict("m = [\"AA\"]", &byte_item, false);
}

#[test]
fn ne_byte_string_controllers_accept_every_text_string() {
  // RFC 8610 Section 3.8.6: "All other cases are not equal (e.g., comparing
  // a text string with a byte string)", so a text instance always satisfies
  // .ne against a byte-string controller, byte-equal or not.
  for literal in ["'AA'", "h'4141'", "b64'QUE='"] {
    let schema = format!("m = tstr .ne {literal}");
    assert_verdict(&schema, &cbor_text("AA"), true);
    assert_verdict(&schema, &cbor_text("AB"), true);
  }

  // Text controllers keep exact .ne semantics.
  assert_verdict("m = tstr .ne \"AA\"", &cbor_text("AA"), false);
  assert_verdict("m = tstr .ne \"AA\"", &cbor_text("AB"), true);
}

#[test]
fn ne_mixed_choice_controllers_accept_text_through_the_byte_branch() {
  // A choice controller applies per element of the controller type (the
  // cross-product reading of RFC 9165 Section 2), so the byte-string branch
  // satisfies .ne for every text instance under RFC 8610 Section 3.8.6's
  // cross-kind rule, whichever branch order is written. The all-text choice
  // control pins the pre-existing any-branch choice semantics these rows
  // inherit.
  assert_verdict("m = tstr .ne (\"AA\" / 'BB')", &cbor_text("AA"), true);
  assert_verdict("m = tstr .ne ('BB' / \"AA\")", &cbor_text("AA"), true);
  assert_verdict("m = tstr .ne (\"AA\" / \"BB\")", &cbor_text("AA"), true);
}

#[test]
fn eq_byte_string_controllers_match_no_text_string() {
  // The same RFC 8610 Section 3.8.6 cross-kind rule: equal bytes in a text
  // string do not satisfy .eq against a byte-string controller.
  for literal in ["'AA'", "h'4141'", "b64'QUE='"] {
    let schema = format!("m = tstr .eq {literal}");
    assert_verdict(&schema, &cbor_text("AA"), false);
  }

  assert_verdict("m = tstr .eq \"AA\"", &cbor_text("AA"), true);
  assert_verdict("m = tstr .eq \"AA\"", &cbor_text("AB"), false);
}

#[test]
fn composite_array_eq_ne_byte_literal_elements_follow_the_cross_kind_rule() {
  // RFC 8610 Section 3.8.6: arrays are equal only when their elements are
  // equal pairwise, and a text element is never equal to a byte-string
  // element, so .ne is satisfied and .eq is not regardless of the octets.
  let text_item = cbor_array(&cbor_text("AA"));

  assert_verdict("m = [\"AA\"] .ne ['BB']", &text_item, true);
  assert_verdict("m = [\"AA\"] .ne ['AA']", &text_item, true);
  assert_verdict("m = [tstr] .eq ['AA']", &text_item, false);
  assert_verdict("m = [tstr] .eq [\"AA\"]", &text_item, true);
}
