#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(target_arch = "wasm32"))]

use std::borrow::Cow;

#[cfg(feature = "json")]
use cddl::validator::validate_json_from_str;
use cddl::{
  ast::Type2, parser::cddl_from_str, token::ByteValue, validator::validate_cbor_from_slice,
};

#[test]
fn decoded_byte_values_and_ast_nodes_render_as_cddl_literals() {
  for (value, expected) in [
    (
      ByteValue::B16(Cow::Borrowed(&[0xaa, 0x00, 0xff])),
      "h'aa00ff'",
    ),
    (ByteValue::B16(Cow::Borrowed(b"AA")), "h'4141'"),
    (ByteValue::B64(Cow::Borrowed(&[0xfb, 0xff])), "b64'-_8'"),
    (ByteValue::B64(Cow::Borrowed(b"AA")), "b64'QUE'"),
  ] {
    assert_eq!(value.to_string(), expected);
    assert_eq!(Type2::from(value).to_string(), expected);
  }
}

#[test]
fn non_utf8_unqualified_byte_strings_render_as_base16() {
  // Display must be infallible: unqualified byte-string state whose bytes
  // are not valid UTF-8 falls back to h'…' notation, which denotes the same
  // byte string and reparses.
  let value = ByteValue::UTF8(Cow::Borrowed(&[0xff]));
  assert_eq!(value.to_string(), "h'ff'");
  assert_eq!(Type2::from(value).to_string(), "h'ff'");

  let mixed = ByteValue::UTF8(Cow::Borrowed(&[0x74, 0x65, 0xff]));
  assert_eq!(mixed.to_string(), "h'7465ff'");

  // Valid UTF-8 keeps the unqualified notation.
  let utf8 = ByteValue::UTF8(Cow::Borrowed(b"te"));
  assert_eq!(utf8.to_string(), "'te'");
  assert_eq!(Type2::from(utf8).to_string(), "'te'");
}

#[test]
fn decoded_byte_string_literals_format_and_reparse() {
  for (source, expected) in [
    ("m = b64'- _8 ='", "m = b64'-_8'"),
    ("m = b64'-_8'", "m = b64'-_8'"),
    ("m = b64'+/8='", "m = b64'-_8'"),
    ("m = b64'+/8'", "m = b64'-_8'"),
    ("m = b64'EjRWeA'", "m = b64'EjRWeA'"),
    ("m = b64'EjRWeA=='", "m = b64'EjRWeA'"),
    ("m = h'AA 00 FF'", "m = h'aa00ff'"),
  ] {
    let cddl = cddl_from_str(source, false).unwrap();
    let formatted = cddl.to_string();
    assert_eq!(formatted.trim(), expected);

    let reparsed = cddl_from_str(&formatted, false).unwrap();
    assert_eq!(reparsed.to_string(), formatted);
  }
}

#[test]
fn comments_inside_prefixed_byte_strings_are_ignored() {
  for (source, expected) in [
    (
      "m = h'4342 ; comment with hex digits 4F52\n4F52'",
      "m = h'43424f52'",
    ),
    ("m = b64'Ej ; note\nRWeA'", "m = b64'EjRWeA'"),
  ] {
    let cddl = cddl_from_str(source, false).unwrap();
    assert_eq!(cddl.to_string().trim(), expected);
  }
}

#[test]
fn non_utf8_byte_literal_mismatches_return_diagnostics() {
  for (literal, rendered) in [("h'AA'", "h'aa'"), ("b64'qg=='", "b64'qg'")] {
    let error = validate_cbor_from_slice(&format!("m = {}", literal), b"\x01", None)
      .expect_err("an integer must not match a byte-string literal")
      .to_string();

    assert!(
      error.contains(&format!("expected {}", rendered)) && error.contains("got"),
      "unexpected diagnostic for {}: {}",
      literal,
      error
    );
  }
}

#[test]
fn decoded_byte_literal_accepting_and_rejecting_controls() {
  for (literal, matching_cbor) in [
    ("h'AA'", b"\x41\xaa".as_slice()),
    ("b64'qg=='", b"\x41\xaa"),
    ("b64'qg'", b"\x41\xaa"),
  ] {
    let schema = format!("m = {}", literal);
    validate_cbor_from_slice(&schema, matching_cbor, None)
      .unwrap_or_else(|error| panic!("{} rejected matching bytes: {}", literal, error));
    validate_cbor_from_slice(&schema, b"\x41\xab", None)
      .expect_err("a distinct byte string must not match the literal");
  }
}

#[test]
fn base64_alphabet_and_padding_variants_decode_identically() {
  for literal in ["b64'-_8='", "b64'-_8'", "b64'+/8='", "b64'+/8'"] {
    let schema = format!("m = {}", literal);
    validate_cbor_from_slice(&schema, b"\x42\xfb\xff", None)
      .unwrap_or_else(|error| panic!("{} rejected matching bytes: {}", literal, error));
    validate_cbor_from_slice(&schema, b"\x42\xfb\xfe", None)
      .expect_err("a distinct byte string must not match the literal");
  }
}

#[test]
fn base64_literals_mixing_both_rfc_4648_alphabets_are_rejected() {
  // RFC 8610 §3.1 admits a base64 literal in the RFC 4648 §4 alphabet *or*
  // the §5 alphabet; RFC 4648 §3.3 requires rejecting characters outside the
  // chosen alphabet. A literal drawing from both belongs to neither.
  for schema in [
    "m = b64'+_8'",
    "m = b64'-/8'",
    "m = b64'+_8='",
    "m = b64'-/8='",
    "m = b64'Ej-W+A'",
  ] {
    let error = cddl_from_str(schema, false)
      .expect_err(&format!(
        "parser accepted mixed-alphabet literal: {}",
        schema
      ))
      .to_string();

    assert!(
      error.contains("mixes the RFC 4648 base64 and base64url alphabets"),
      "unexpected diagnostic for {}: {}",
      schema,
      error
    );
  }

  // Accepting controls: either alphabet alone still decodes, with or without
  // padding, and renders as unpadded base64url.
  for (schema, expected) in [
    ("m = b64'-_8'", "m = b64'-_8'"),
    ("m = b64'+/8'", "m = b64'-_8'"),
    ("m = b64'-_8='", "m = b64'-_8'"),
    ("m = b64'+/8='", "m = b64'-_8'"),
    ("m = b64'EjRWeA'", "m = b64'EjRWeA'"),
  ] {
    let cddl = cddl_from_str(schema, false)
      .unwrap_or_else(|error| panic!("parser rejected {}: {}", schema, error));
    assert_eq!(cddl.to_string().trim(), expected);
  }
}

#[cfg(feature = "json")]
#[cfg(feature = "additional-controls")]
#[test]
fn mixed_alphabet_rejection_is_shared_by_the_json_validator() {
  // The alphabet check lives in the parser, so CBOR and JSON validation
  // reject the same literals for the same reason.
  let error = validate_json_from_str("m = tstr .b64u b64'+_8'", "\"-_8\"", None)
    .expect_err("the JSON validator must reject a mixed-alphabet literal")
    .to_string();
  assert!(
    error.contains("mixes the RFC 4648 base64 and base64url alphabets"),
    "unexpected JSON diagnostic: {}",
    error
  );

  // Accepting control: the same schema with a single-alphabet controller
  // still validates.
  validate_json_from_str("m = tstr .b64u b64'-_8'", "\"-_8\"", None)
    .expect("a single-alphabet controller must still validate");
}

#[test]
fn malformed_encoded_byte_literals_are_rejected() {
  for schema in [
    "m = h'A'",
    "m = h'AG'",
    "m = b64'qg='",
    "m = b64'q'",
    "m = b64'q=g='",
  ] {
    assert!(
      cddl_from_str(schema, false).is_err(),
      "parser accepted malformed literal: {}",
      schema
    );
  }
}
