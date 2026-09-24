#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(feature = "json")]
#![cfg(feature = "additional-controls")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::validator::{validate_cbor_from_slice, validate_json_from_str};

fn cbor_string(major_type: u8, value: &[u8]) -> Vec<u8> {
  let mut encoded = if value.len() < 24 {
    vec![(major_type << 5) | value.len() as u8]
  } else {
    vec![(major_type << 5) | 24, value.len() as u8]
  };
  encoded.extend_from_slice(value);
  encoded
}

fn cbor_bytes(value: &[u8]) -> Vec<u8> {
  cbor_string(2, value)
}

fn cbor_text(value: &str) -> Vec<u8> {
  cbor_string(3, value.as_bytes())
}

fn assert_cbor_verdict(schema: &str, encoded: &[u8], accept: bool) {
  let result = validate_cbor_from_slice(schema, encoded, None);
  assert_eq!(
    result.is_ok(),
    accept,
    "schema {schema:?} with CBOR {encoded:02x?} should {}: {result:?}",
    if accept { "accept" } else { "reject" },
  );
}

fn assert_raw_json_verdict(schema: &str, json: &str, accept: bool) {
  let result = validate_json_from_str(schema, json, None);
  assert_eq!(
    result.is_ok(),
    accept,
    "schema {schema:?} with JSON {json} should {}: {result:?}",
    if accept { "accept" } else { "reject" },
  );
}

fn assert_json_verdict(schema: &str, text: &str, accept: bool) {
  assert_raw_json_verdict(schema, &format!("\"{}\"", text), accept);
}

#[test]
fn cat_uses_decoded_bytes_and_keeps_the_target_string_type() {
  let byte_claims = [
    "m = h'74657374' .cat h'33'",
    "m = b64'dGVzdA==' .cat b64'Mw=='",
    "m = h'74657374' .cat b64'Mw=='",
    "m = b64'dGVzdA==' .cat h'33'",
    "m = lhs .cat rhs\nlhs = h'74657374'\nrhs = b64'Mw=='",
  ];

  for schema in byte_claims {
    assert_cbor_verdict(schema, &cbor_bytes(b"test3"), true);
    assert_cbor_verdict(schema, &cbor_text("test3"), false);
    assert_cbor_verdict(schema, &cbor_bytes(b"test4"), false);
  }

  // The result type comes from the target, independent of whether its decoded
  // bytes happen to be valid UTF-8.
  assert_cbor_verdict("m = 'te' .cat h'7374'", &cbor_bytes(b"test"), true);
  assert_cbor_verdict("m = \"te\" .cat h'7374'", &cbor_text("test"), true);
  assert_cbor_verdict("m = \"te\" .cat h'7374'", &cbor_bytes(b"test"), false);
  assert_cbor_verdict("m = h'ff' .cat b64'gA=='", &cbor_bytes(&[0xff, 0x80]), true);
}

#[test]
fn det_operates_on_decoded_bytes_without_requiring_utf8() {
  for schema in [
    "m = h'ff' .det h'00'",
    "m = b64'_w==' .det b64'AA=='",
    "m = h'ff' .det b64'AA=='",
    "m = b64'_w==' .det h'00'",
  ] {
    assert_cbor_verdict(schema, &cbor_bytes(&[0xff, 0x00]), true);
    assert_cbor_verdict(schema, &cbor_bytes(&[0xff, 0x01]), false);
  }
}

#[test]
fn det_byte_targets_dedent_uniform_space_indentation() {
  // With uniform space-only indentation the smallest common indent equals
  // each non-blank line's own indent, so the current per-line strip and
  // RFC 9165 Section 2.3's common-indent definition agree on these vectors;
  // the claims hold through a conforming common-indent rewrite of `.det`.
  // h'0a202074650a202073740a' is "\n  te\n  st\n".
  let schema = "m = h'0a202074650a202073740a' .det h'ff'";
  assert_cbor_verdict(schema, &cbor_bytes(b"\nte\nst\n\xff"), true);
  assert_cbor_verdict(schema, &cbor_bytes(b"\n  te\n  st\n\xff"), false);

  // Non-UTF-8 content dedents as bytes: h'0a2020ff0a202062' is
  // "\n  \xff\n  b", which dedents to "\n\xff\nb".
  let non_utf8 = "m = h'0a2020ff0a202062' .det h''";
  assert_cbor_verdict(non_utf8, &cbor_bytes(&[0x0a, 0xff, 0x0a, 0x62]), true);
  assert_cbor_verdict(
    non_utf8,
    &cbor_bytes(&[0x0a, 0x20, 0x20, 0xff, 0x0a, 0x20, 0x20, 0x62]),
    false,
  );
}

#[test]
fn abnfb_reads_direct_aliased_and_composed_decoded_controllers() {
  let h16 = "h'44494749540a4449474954203d20257833302d33390a'";
  let b64 = "b64'RElHSVQKRElHSVQgPSAleDMwLTM5Cg=='";
  let schemas = [
    format!("m = bytes .abnfb {h16}"),
    format!("m = bytes .abnfb {b64}"),
    format!("m = bytes .abnfb grammar\ngrammar = {h16}"),
    format!("m = bytes .abnfb grammar\ngrammar = {b64}"),
    concat!(
      "m = bytes .abnfb grammar\n",
      "grammar = prefix .cat body\n",
      "prefix = h'4449474954'\n",
      "body = b64'CkRJR0lUID0gJXgzMC0zOQo='",
    )
    .to_string(),
  ];

  for schema in schemas {
    assert_cbor_verdict(&schema, &cbor_bytes(b"7"), true);
    assert_cbor_verdict(&schema, &cbor_bytes(b"x"), false);
  }
}

#[test]
fn sloppy_b64_variants_only_ignore_trailing_bits() {
  // RFC 9741 Section 2.1: "sloppy" relaxes exactly one thing — the text is
  // not validated for additional (trailing) bits being zero. The decoded
  // value must still equal the controller bytes, .b64u still forbids
  // padding, and .b64c still requires canonical padding.
  let claims = [
    // "AQZ" decodes to 01 06 with non-zero trailing bits; "AQY" is canonical.
    ("m = tstr .b64u-sloppy h'0106'", "AQZ", true),
    ("m = tstr .b64u-sloppy h'0106'", "AQY", true),
    ("m = tstr .b64u-sloppy h'01ff'", "AQZ", false),
    ("m = tstr .b64u-sloppy h'010c'", "AQZ", false),
    ("m = tstr .b64u-sloppy h'01'", "AQZ", false),
    // "QR" decodes to 41 with non-zero trailing bits.
    ("m = tstr .b64u-sloppy h'41'", "QR", true),
    ("m = tstr .b64u-sloppy h'01'", "AQ==", false),
    ("m = tstr .b64c-sloppy h'41'", "QR==", true),
    ("m = tstr .b64c-sloppy h'41'", "QQ==", true),
    ("m = tstr .b64c-sloppy h'41'", "QQ", false),
    // Strict variants keep rejecting non-zero trailing bits.
    ("m = tstr .b64u h'0106'", "AQZ", false),
    ("m = tstr .b64u h'0106'", "AQY", true),
  ];

  for (schema, text, accept) in claims {
    assert_cbor_verdict(schema, &cbor_text(text), accept);
    assert_json_verdict(schema, text, accept);
  }
}

#[test]
fn composed_controllers_resolve_to_computed_bytes() {
  // A .cat/.det expression in controller position matches the concatenated
  // bytes, both parenthesized in place and through a named rule.
  let claims = [
    ("m = tstr .b64u (h'ff' .cat h'00')", "_wA", true),
    ("m = tstr .b64u (h'ff' .cat h'00')", "_w", false),
    (
      "m = tstr .b64u payload\npayload = h'ff' .cat h'00'",
      "_wA",
      true,
    ),
    (
      "m = tstr .b64u payload\npayload = h'ff' .cat h'00'",
      "_w",
      false,
    ),
    ("m = tstr .hex (h'ff' .cat h'00')", "ff00", true),
    ("m = tstr .hex (h'ff' .cat h'00')", "ff", false),
    ("m = tstr .b32 (h'ff' .det h'00')", "74AA", true),
    ("m = tstr .b32 (h'ff' .det h'00')", "74", false),
    // Parenthesized single literals, without an operator.
    ("m = tstr .b64u (h'ff')", "_w", true),
    ("m = tstr .b64u (h'ff')", "_wA", false),
  ];

  for (schema, text, accept) in claims {
    assert_cbor_verdict(schema, &cbor_text(text), accept);
    assert_json_verdict(schema, text, accept);
  }
}

#[test]
fn cyclic_controller_rules_reject_without_crashing() {
  // A self-referential controller rule must produce a validation error, not
  // a stack overflow; a cycle alongside a usable literal is simply ignored.
  let claims = [
    ("m = tstr .b64u c\nc = c", "_w", false),
    ("m = tstr .b64u c\nc = h'ff' / c", "_w", true),
  ];

  for (schema, text, accept) in claims {
    assert_cbor_verdict(schema, &cbor_text(text), accept);
    assert_json_verdict(schema, text, accept);
  }

  assert_cbor_verdict("m = \"a\" .cat c\nc = c", &cbor_text("a"), false);
  assert_json_verdict("m = \"a\" .cat c\nc = c", "a", false);

  // The cycle guard is per resolution: choice-extension siblings of an
  // already-visited rule name still contribute their literals.
  let extended_cat = "m = \"a\" .cat b\nb = \"x\"\nb /= \"y\"";
  for (text, accept) in [("ax", true), ("ay", true), ("az", false)] {
    assert_cbor_verdict(extended_cat, &cbor_text(text), accept);
    assert_json_verdict(extended_cat, text, accept);
  }

  let extended_b64u = "m = tstr .b64u p\np = h'ff'\np /= h'00'";
  for (text, accept) in [("_w", true), ("AA", true), ("_wA", false)] {
    assert_cbor_verdict(extended_b64u, &cbor_text(text), accept);
    assert_json_verdict(extended_b64u, text, accept);
  }
}

#[test]
fn non_utf8_computed_byte_values_reject_without_panicking() {
  // RFC 9165 only requires the .cat/.det result to be valid UTF-8 when the
  // target is a text string, so this byte-string target legitimately
  // computes the non-UTF-8 bytes 74 65 ff. Every mismatching instance must
  // produce a validation error; none may abort while rendering the expected
  // value.
  let schema = "m = 'te' .cat h'ff'";

  assert_cbor_verdict(schema, &[0x43, 0x74, 0x65, 0xff], true);

  for instance in [
    &[0x41, 0x74][..],       // bytes "t"
    &[0x62, 0x74, 0x65][..], // text "te"
    &[0x01][..],             // int 1
    &[0xf9, 0x3e, 0x00][..], // float 1.5
    &[0xf5][..],             // true
    &[0xf6][..],             // null
    &[0xc1, 0x01][..],       // tag 1(1)
    &[0x80][..],             // empty array
    &[0xa0][..],             // empty map
  ] {
    assert_cbor_verdict(schema, instance, false);
  }

  assert_cbor_verdict("m = 'te' .det h'ff'", &[0x41, 0x74], false);
  assert_cbor_verdict("m = { ('a' .cat h'ff') => int }", &[0xa0], false);

  for json in ["\"te\"", "1", "[]"] {
    assert_raw_json_verdict(schema, json, false);
  }
}

#[test]
fn composed_alias_operands_fail_closed() {
  // A .cat/.det operand that is an alias for another composed rule is not
  // computed yet: the literal accumulator must not contribute the alias's
  // left operand alone, silently collapsing the computation. Until the
  // nested value is computed, resolution fails closed with a validation
  // error. The conforming instance for `c` below is h'ff0011' ("_wAR"), so
  // BOTH instances reject for now.
  // TODO(composed-operands): once nested composed operands are computed
  // (RFC 9165 Section 2.2 applied to an operand that is itself composed),
  // "_wAR" flips to accept; "_wA" must stay rejected.
  let nested_alias = "m = tstr .b64u c\nc = h'ff' .cat d\nd = h'00' .cat h'11'";
  for text in ["_wA", "_wAR"] {
    assert_cbor_verdict(nested_alias, &cbor_text(text), false);
    assert_json_verdict(nested_alias, text, false);
  }

  // The same guard closes the .cat route's silent collapse: `b` used to
  // resolve to "x" alone, wrongly accepting "ax". RFC 9165 accepts "axy"
  // only; until composed operands are computed, both instances reject.
  // TODO(composed-operands): "axy" flips to accept; "ax" must stay rejected.
  let nested_cat = "m = \"a\" .cat b\nb = \"x\" .cat \"y\"";
  for text in ["ax", "axy"] {
    assert_cbor_verdict(nested_cat, &cbor_text(text), false);
    assert_json_verdict(nested_cat, text, false);
  }

  // A plain literal choice alongside a composed sibling still contributes:
  // skipping the composed choice loses its value (under-acceptance until the
  // nested operand is computed) but never invents one.
  let mixed = "m = tstr .b64u c\nc = h'ff' .cat d\nd = h'aa' / h'00' .cat h'11'";
  assert_cbor_verdict(mixed, &cbor_text("_6o"), true); // h'ffaa'
  assert_json_verdict(mixed, "_6o", true);
  // h'ff0011' through the composed sibling.
  // TODO(composed-operands): this row flips to accept.
  assert_cbor_verdict(mixed, &cbor_text("_wAR"), false);
  assert_json_verdict(mixed, "_wAR", false);
}

#[test]
fn mixed_string_kind_controller_rules_match_through_byte_choices() {
  // RFC 9741 encoding controllers are byte strings. A controller rule that
  // carries a text-string choice alongside a byte-string choice matches
  // through its byte-string choices; the text choice contributes no
  // controller value ("eA" is the base64url of the text choice's own bytes
  // and must not match). Accept-direction change from the old base, which
  // rejected the whole rule as an invalid controller.
  let mixed = "m = tstr .b64u c\nc = \"x\" / h'ff'";
  assert_cbor_verdict(mixed, &cbor_text("_w"), true);
  assert_json_verdict(mixed, "_w", true);
  assert_cbor_verdict(mixed, &cbor_text("eA"), false);
  assert_json_verdict(mixed, "eA", false);

  // A controller rule with no byte-string choice at all remains an
  // invalid-controller error on every instance.
  let text_only = "m = tstr .b64u c\nc = \"x\"";
  for text in ["_w", "eA"] {
    assert_cbor_verdict(text_only, &cbor_text(text), false);
    assert_json_verdict(text_only, text, false);
  }
}

struct EncodingClaim {
  control: &'static str,
  b16: &'static str,
  b64: &'static str,
  text: &'static str,
}

#[test]
fn encoding_controls_compare_text_to_decoded_literal_bytes() {
  let claims = [
    EncodingClaim {
      control: ".b64u",
      b16: "h'ff'",
      b64: "b64'_w=='",
      text: "_w",
    },
    EncodingClaim {
      control: ".b64c",
      b16: "h'ff'",
      b64: "b64'_w=='",
      text: "/w==",
    },
    EncodingClaim {
      control: ".b64u-sloppy",
      b16: "h'ff'",
      b64: "b64'_w=='",
      text: "_w",
    },
    EncodingClaim {
      control: ".b64c-sloppy",
      b16: "h'ff'",
      b64: "b64'_w=='",
      text: "/w==",
    },
    EncodingClaim {
      control: ".hex",
      b16: "h'ab'",
      b64: "b64'qw=='",
      text: "aB",
    },
    EncodingClaim {
      control: ".hexlc",
      b16: "h'ab'",
      b64: "b64'qw=='",
      text: "ab",
    },
    EncodingClaim {
      control: ".hexuc",
      b16: "h'ab'",
      b64: "b64'qw=='",
      text: "AB",
    },
    EncodingClaim {
      control: ".b32",
      b16: "h'4142'",
      b64: "b64'QUI='",
      text: "IFBA",
    },
    EncodingClaim {
      control: ".h32",
      b16: "h'4142'",
      b64: "b64'QUI='",
      text: "8510",
    },
    EncodingClaim {
      control: ".b45",
      b16: "h'4142'",
      b64: "b64'QUI='",
      text: "BB8",
    },
  ];

  for claim in claims {
    for literal in [claim.b16, claim.b64] {
      for schema in [
        format!("m = tstr {} {}", claim.control, literal),
        format!("m = tstr {} payload\npayload = {}", claim.control, literal),
      ] {
        assert_cbor_verdict(&schema, &cbor_text(claim.text), true);
        assert_cbor_verdict(&schema, &cbor_text("not-the-encoding"), false);
        assert_json_verdict(&schema, claim.text, true);
        assert_json_verdict(&schema, "not-the-encoding", false);
      }
    }
  }
}
