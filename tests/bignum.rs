#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::validate_cbor_from_slice;

fn assert_valid(name: &str, cddl: &str, cbor: &[u8]) {
  if let Err(e) = validate_cbor_from_slice(cddl, cbor, None) {
    panic!("{}: expected valid, got error: {:?}", name, e);
  }
}

fn assert_invalid(name: &str, cddl: &str, cbor: &[u8]) {
  assert!(
    validate_cbor_from_slice(cddl, cbor, None).is_err(),
    "{}: expected invalid, but it validated",
    name
  );
}

/// `biguint = #6.2(bstr)`, `bignint = #6.3(bstr)`, `bigint = biguint / bignint`
#[test]
fn bignum_as_map_key() {
  assert_valid(
    "bignint key",
    "start = { bignint => int }",
    &[0xa1, 0xc3, 0x41, 0x01, 0x01],
  );
  assert_valid(
    "biguint key",
    "start = { biguint => int }",
    &[0xa1, 0xc2, 0x41, 0x01, 0x01],
  );
  assert_valid(
    "bigint key, tag 2",
    "start = { bigint => int }",
    &[0xa1, 0xc2, 0x41, 0x01, 0x01],
  );
  assert_valid(
    "bigint key, tag 3",
    "start = { bigint => int }",
    &[0xa1, 0xc3, 0x41, 0x01, 0x01],
  );
  assert_valid(
    "typename alias as key",
    "k = bignint\nstart = { k => int }",
    &[0xa1, 0xc3, 0x41, 0x01, 0x01],
  );

  assert_invalid(
    "bignint key given tag 2",
    "start = { bignint => int }",
    &[0xa1, 0xc2, 0x41, 0x01, 0x01],
  );
  assert_invalid(
    "biguint key given tag 3",
    "start = { biguint => int }",
    &[0xa1, 0xc3, 0x41, 0x01, 0x01],
  );
  assert_invalid(
    "bignum tag wrapping a non-bstr",
    "start = { bignint => int }",
    &[0xa1, 0xc3, 0x01, 0x01],
  );
  assert_invalid(
    "plain int key is not a bignum",
    "start = { bignint => int }",
    &[0xa1, 0x01, 0x01],
  );
  assert_invalid(
    "unknown tag as key",
    "start = { bignint => int }",
    &[0xa1, 0xc5, 0x41, 0x01, 0x01],
  );
  assert_invalid(
    "map value is still validated",
    "start = { bignint => tstr }",
    &[0xa1, 0xc3, 0x41, 0x01, 0x01],
  );
}

/// Occurrence indicators route through a different arm of `visit_identifier`
/// (the `Some(occur)` filter path), so cover them separately.
#[test]
fn bignum_as_map_key_with_occurrence() {
  assert_valid("* with empty map", "start = { * bignint => int }", &[0xa0]);
  assert_valid(
    "* with one entry",
    "start = { * bignint => int }",
    &[0xa1, 0xc3, 0x41, 0x01, 0x01],
  );
  assert_valid(
    "* with two entries",
    "start = { * bignint => int }",
    &[0xa2, 0xc3, 0x41, 0x01, 0x01, 0xc3, 0x41, 0x02, 0x02],
  );
  assert_valid(
    "+ with one entry",
    "start = { + bignint => int }",
    &[0xa1, 0xc3, 0x41, 0x01, 0x01],
  );
  assert_valid(
    "* behind .cbor, empty map",
    "start = [payload: x]\nx = bytes .cbor { * bignint => uint }",
    &[0x81, 0x41, 0xa0],
  );
  assert_valid(
    "* behind .cbor, one entry",
    "start = [payload: x]\nx = bytes .cbor { * bignint => uint }",
    &[0x81, 0x45, 0xa1, 0xc3, 0x41, 0x01, 0x01],
  );

  assert_invalid(
    "* with second value of wrong type",
    "start = { * bignint => int }",
    &[0xa2, 0xc3, 0x41, 0x01, 0x01, 0xc3, 0x41, 0x02, 0x61, 0x78],
  );
  assert_invalid(
    "* with foreign key",
    "start = { * bignint => int }",
    &[0xa1, 0x01, 0x01],
  );
  assert_invalid(
    "* with wrong-tag key",
    "start = { * bignint => int }",
    &[0xa1, 0xc2, 0x41, 0x01, 0x01],
  );
  assert_invalid("+ with empty map", "start = { + bignint => int }", &[0xa0]);
}

#[test]
fn bignum_as_value() {
  assert_valid(
    "[bignint] tag 3",
    "start = [bignint]",
    &[0x81, 0xc3, 0x41, 0x01],
  );
  assert_valid(
    "[biguint] tag 2",
    "start = [biguint]",
    &[0x81, 0xc2, 0x41, 0x01],
  );
  assert_valid(
    "[bigint] tag 2",
    "start = [bigint]",
    &[0x81, 0xc2, 0x41, 0x01],
  );
  assert_valid(
    "[bigint] tag 3",
    "start = [bigint]",
    &[0x81, 0xc3, 0x41, 0x01],
  );

  assert_invalid(
    "[bignint] tag 2",
    "start = [bignint]",
    &[0x81, 0xc2, 0x41, 0x01],
  );
  assert_invalid(
    "[biguint] tag 3",
    "start = [biguint]",
    &[0x81, 0xc3, 0x41, 0x01],
  );
  assert_invalid("[bignint] tag 1", "start = [bignint]", &[0x81, 0xc1, 0x01]);
  // Tag 5 has no dedicated `match *tag` arm — this is why the bignum check
  // sits before the tag dispatch, which can only reject tags it knows about.
  assert_invalid(
    "[bignint] unknown tag 5",
    "start = [bignint]",
    &[0x81, 0xc5, 0x41, 0x01],
  );
  assert_invalid(
    "[bignint] tag 3(1)",
    "start = [bignint]",
    &[0x81, 0xc3, 0x01],
  );
}
