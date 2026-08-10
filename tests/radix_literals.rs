#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{ast, parser, validator::validate_cbor_from_slice};

fn cbor(hex: &str) -> Vec<u8> {
  assert!(hex.len() % 2 == 0, "hex input must have an even length");
  (0..hex.len())
    .step_by(2)
    .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
    .collect()
}

fn assert_valid_claim(cddl_input: &str, cbor_hex: &str) {
  parser::cddl_from_str(cddl_input, false)
    .unwrap_or_else(|e| panic!("unchecked parser rejected {:?}: {}", cddl_input, e));
  ast::CDDL::from_slice(cddl_input.as_bytes())
    .unwrap_or_else(|e| panic!("checked parser rejected {:?}: {}", cddl_input, e));
  validate_cbor_from_slice(cddl_input, &cbor(cbor_hex), None)
    .unwrap_or_else(|e| panic!("validator rejected {:?}: {}", cddl_input, e));
}

fn assert_parse_error_claim(cddl_input: &str) {
  assert!(
    parser::cddl_from_str(cddl_input, false).is_err(),
    "unchecked parser accepted invalid CDDL: {:?}",
    cddl_input
  );
  assert!(
    ast::CDDL::from_slice(cddl_input.as_bytes()).is_err(),
    "checked parser accepted invalid CDDL: {:?}",
    cddl_input
  );
}

#[test]
fn radix_hex_uint_value() {
  // Claim radix_hex_uint_value. RFC 8610 Appendix B: uint = ... / "0x" 1*HEXDIG.
  assert_valid_claim("thing = 0x10", "10");
}

#[test]
fn radix_bin_uint_value() {
  // Claim radix_bin_uint_value. RFC 8610 Appendix B: uint = ... / "0b" 1*BINDIG.
  assert_valid_claim("thing = 0b1010", "0a");
}

#[test]
fn radix_hex_upper_prefix() {
  // Claim radix_hex_upper_prefix. RFC 8610 Section 3.1: hex numbers are case insensitive,
  // including the "0x" prefix (App. B: quote-delimited ABNF strings are case-insensitive).
  assert_valid_claim("thing = 0X10", "10");
}

#[test]
fn radix_bin_upper_prefix() {
  // Claim radix_bin_upper_prefix. RFC 8610 Section 3.1: binary numbers are case insensitive,
  // including the "0b" prefix (App. B: quote-delimited ABNF strings are case-insensitive).
  assert_valid_claim("thing = 0B1010", "0a");
}

#[test]
fn radix_hex_lowercase_digit() {
  // Claim radix_hex_lowercase_digit. RFC 8610 Section 3.1: hex numbers are case insensitive, so
  // lowercase a-f are valid HEXDIG matches (ABNF quoted literals are case-insensitive per RFC 5234).
  // No spec-vs-oracle deviation here: ruby agrees.
  assert_valid_claim("thing = 0x0f", "0f");
}

#[test]
fn radix_negative_hex_int() {
  // Claim radix_negative_hex_int. RFC 8610 Appendix B: int = ["-"] uint.
  assert_valid_claim("thing = -0x10", "2f");
}

#[test]
fn radix_negative_bin_int() {
  // Claim radix_negative_bin_int. RFC 8610 Appendix B: int = ["-"] uint.
  assert_valid_claim("thing = -0b101", "24");
}

#[test]
fn radix_hex_range_bounds() {
  // Claim radix_hex_range_bounds. RFC 8610 Section 2.2.2.1 and Appendix B rangeop.
  assert_valid_claim("thing = 0x00..0xff", "18ff");
}

#[test]
fn radix_hex_memberkey_value() {
  // Claim radix_hex_memberkey_value. RFC 8610 Appendix B: memberkey = value S ":".
  assert_valid_claim("thing = {0x10: tstr}", "a1106161");
}

#[test]
fn radix_bin_memberkey_value() {
  // Claim radix_bin_memberkey_value. RFC 8610 Appendix B: memberkey = value S ":".
  assert_valid_claim("thing = {0b1010: tstr}", "a10a6161");
}

#[test]
fn radix_major_ai_uint() {
  // Claim radix_major_ai_uint. RFC 8610 Section 2.2.3: "#" DIGIT ["." uint].
  assert_valid_claim("thing = #0.0x18", "1818");
}

#[test]
fn radix_tag_head_number() {
  // Claim radix_tag_head_number_oracle_deviation. RFC 8610 App. B `"#" "6" ["." uint]` and
  // RFC 9682 Section 3.2 `head-number = uint` permit radix tag numbers: #6.0x20 is tag 32.
  // Ruby cddl 0.12.14 disagrees (base-10 String#to_i on "0x20" yields tag 0) — confirmed RUBY_BUG
  assert_valid_claim("thing = #6.0x20(tstr)", "d8206161");
}

#[test]
fn radix_simple_head_number() {
  // Claim radix_simple_head_number_oracle_deviation. RFC 9682 Section 3.2: #7.<head-number> with
  // head-number 32..255 stands for that simple value, so #7.0x20 is simple(32).
  // Ruby cddl 0.12.14 disagrees (same base-10 to_i bug) — confirmed RUBY_BUG
  assert_valid_claim("thing = #7.0x20", "f820");
}

#[test]
fn radix_occurrence_bounds() {
  // Claim radix_occurrence_bounds_oracle_deviation. RFC 8610 App. B `occur = [uint] "*" [uint]`
  // with the same radix-capable uint production, so 0x2*0x4 means 2..4 occurrences. (App. B's
  // generative ABNF doesn't itself prioritize this over other derivations — App. A's PEG rules
  // govern data matching, not spec-text parsing — but the occurrence-indicator read is the only
  // one with defined semantics and is the de-facto convention.)
  // Ruby cddl 0.12.14 disagrees (decimal-only regex in `occur`, raises "huh") — confirmed RUBY_BUG,
  assert_valid_claim("thing = [0x2*0x4 tstr]", "8261616162");
}

#[test]
fn hexfloat_valid_with_p_exponent() {
  // Claim hexfloat_valid_with_p_exponent. RFC 8610 Appendix B: hexfloat ends with "p" exponent.
  assert_valid_claim("thing = 0x1.8p+1", "fb4008000000000000");
}

#[test]
fn hexfloat_missing_p_exponent() {
  // Claim hexfloat_missing_p_exponent. RFC 8610 Appendix B requires the "p" exponent.
  assert_parse_error_claim("thing = 0x1.8");
}

#[test]
fn radix_hex_upper_digits() {
  // Claim radix_hex_upper_digits. RFC 8610 Section 3.1: hex numbers are case insensitive.
  assert_valid_claim("thing = 0xFF", "18ff");
}

#[test]
fn radix_hex_zero() {
  // Claim radix_hex_zero. RFC 8610 Appendix B: uint = ... / "0x" 1*HEXDIG.
  assert_valid_claim("thing = 0x0", "00");
}

#[test]
fn radix_bin_zero() {
  // Claim radix_bin_zero. RFC 8610 Appendix B: uint = ... / "0b" 1*BINDIG.
  assert_valid_claim("thing = 0b0", "00");
}

#[test]
fn radix_hex_u64_max() {
  // Claim radix_hex_u64_max. RFC 8610 Appendix B uint has no size bound; the bridge conversion
  // must at least cover the full u64 range without overflow.
  assert_valid_claim("thing = 0xffffffffffffffff", "1bffffffffffffffff");
}

#[test]
fn radix_control_arg() {
  // Claim radix_control_arg. RFC 8610 Section 3.8.6: .lt controller is a type1; value = number.
  assert_valid_claim("thing = uint .lt 0x10", "0f");
}

#[test]
fn radix_hex_no_digits() {
  // Claim radix_hex_no_digits. RFC 8610 Appendix B: "0x" requires 1*HEXDIG, so "0x" is not a
  // uint literal. At top level the leftover "x" makes the whole document unparseable. (In group
  // contexts "0x" still parses as uint 0 followed by typename x — RFC-derivable, since group
  // entries need no separating whitespace.)
  assert_parse_error_claim("thing = 0x");
}

#[test]
fn radix_bin_bad_digit() {
  // Claim radix_bin_bad_digit. RFC 8610 Appendix B: "0b" requires 1*BINDIG; "0b2" must not parse.
  assert_parse_error_claim("thing = 0b2");
}

#[test]
fn hexfloat_no_fraction() {
  // Claim hexfloat_no_fraction. RFC 8610 Appendix B hexfloat: fraction optional, "p" required.
  assert_valid_claim("thing = 0x1p3", "fb4020000000000000");
}

#[test]
fn hexfloat_upper_p() {
  // Claim hexfloat_upper_p_oracle_deviation. RFC 8610 App. B "p" is a case-insensitive ABNF
  // literal, so 0x1P3 is a valid hexfloat (8.0). Ruby cddl 0.12.14 disagrees (lowercase-only
  // hexfloat regex at cddl.rb:1579, then Ruby eval SyntaxError) — confirmed RUBY_BUG
  assert_valid_claim("thing = 0x1P3", "fb4020000000000000");
}

#[test]
fn hexfloat_negative() {
  // Claim hexfloat_negative. RFC 8610 Appendix B: hexfloat = ["-"] "0x" ....
  assert_valid_claim("thing = -0x1.8p+1", "fbc008000000000000");
}

#[test]
fn radix_hex_tag_head_overflow() {
  // A head-number (RFC 9682 Section 3.2: head-number = uint) above u64::MAX must be a parse
  // error, not silently tag 0.
  assert_parse_error_claim("thing = #6.0x10000000000000000(bstr)");
}

#[test]
fn radix_hex_tag_head_u64_max() {
  // Tag numbers (RFC 8610 Section 3.6; RFC 9682 Section 3.2 head-number) cover the full u64
  // range; CBOR: tag(2^64-1) around tstr "a".
  assert_valid_claim(
    "thing = #6.0xffffffffffffffff(tstr)",
    "dbffffffffffffffff6161",
  );
}

#[test]
fn radix_occurrence_bound_overflow() {
  // An occurrence bound that overflows must be a parse error, not silently 0 (which would
  // invert the constraint from "at least 2^64 elements" to "any number of elements").
  assert_parse_error_claim("thing = [0x10000000000000000* tstr]");
}

#[test]
fn float_exponent_without_fraction() {
  // RFC 8610 Appendix B: number = hexfloat / (int ["." fraction] ["e" exponent]) — the
  // fraction is optional, so 1e5 is a valid float literal (100000.0).
  assert_valid_claim("thing = 1e5", "fb40f86a0000000000");
}

#[test]
fn decimal_leading_zero_rejected() {
  // RFC 8610 Appendix B: uint = DIGIT1 *DIGIT / ... / "0" — leading-zero decimals like 042
  // are not valid CDDL.
  assert_parse_error_claim("thing = 042");
}

#[test]
fn float_leading_zero_rejected() {
  // The float mantissa is an int (RFC 8610 App. B: int ["." fraction] ["e" exponent]), so it
  // must not have leading zeros either — 042e5 was a parse error before the exponent branch
  // existed and must stay one. Zero itself and zero-led fractions/exponents remain valid.
  assert_parse_error_claim("thing = 042e5");
  assert_parse_error_claim("thing = 01.5");
  assert_valid_claim("thing = 0.5", "fb3fe0000000000000");
  assert_valid_claim("thing = 1.05e05", "fb40f9a28000000000");
}

#[test]
fn decimal_leading_zero_regroups_in_arrays() {
  // Pins the re-tokenization quirk documented at uint_value in cddl.pest: "042" is not a
  // uint literal, but in group contexts (where no separating whitespace is required) it is
  // RFC-derivable as the two entries `0 42`, so [042] accepts CBOR [0, 42] — and only that.
  assert_valid_claim("thing = [042]", "8200182a");
  assert!(
    validate_cbor_from_slice("thing = [042]", &cbor("81182a"), None).is_err(),
    "[042] must not validate CBOR [42]"
  );
}

#[test]
fn guardrail_decimal_uint_value() {
  // Expected-green guardrail. RFC 8610 Appendix B: uint = DIGIT1 *DIGIT / "0".
  assert_valid_claim("thing = 16", "10");
}

#[test]
fn guardrail_decimal_occurrence_bounds() {
  // Expected-green guardrail. RFC 8610 Appendix B: occur = [uint] "*" [uint].
  assert_valid_claim("thing = [2*4 tstr]", "8261616162");
}
