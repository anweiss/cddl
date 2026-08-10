#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(target_arch = "wasm32"))]

// Range endpoint semantics per RFC 8610 Section 2.2.2.1: ".." includes both
// endpoints; "..." includes the lower endpoint and excludes only the upper.
// Ranges are defined between two integer values (matching integers) or
// between two floating-point values (matching floats) only.
//
// Every expected verdict below was cross-checked against the reference
// implementation (ruby cddl gem 0.12.14) before being committed.

use cddl::validator::validate_cbor_from_slice;

struct Claim {
  cddl: &'static str,
  cbor: &'static [u8],
  accept: bool,
  rationale: &'static str,
}

fn check(claims: &[Claim]) {
  for claim in claims {
    let result = validate_cbor_from_slice(claim.cddl, claim.cbor, None);
    assert_eq!(
      result.is_ok(),
      claim.accept,
      "{} with CBOR {:02x?} should {}: {} (got {:?})",
      claim.cddl,
      claim.cbor,
      if claim.accept { "accept" } else { "reject" },
      claim.rationale,
      result
    );
  }
}

#[test]
fn range_endpoints_nint_to_nint_inclusive() {
  // RFC 8610 Section 2.2.2.1: ".." includes both endpoints for integer ranges.
  check(&[
    Claim {
      cddl: "t = -10..-3",
      cbor: b"\x2a",
      accept: false,
      rationale: "-11 is below lower endpoint -10",
    },
    Claim {
      cddl: "t = -10..-3",
      cbor: b"\x29",
      accept: true,
      rationale: "lower endpoint -10 is included",
    },
    Claim {
      cddl: "t = -10..-3",
      cbor: b"\x26",
      accept: true,
      rationale: "-7 is mid-window",
    },
    Claim {
      cddl: "t = -10..-3",
      cbor: b"\x22",
      accept: true,
      rationale: "upper endpoint -3 is included",
    },
    Claim {
      cddl: "t = -10..-3",
      cbor: b"\x21",
      accept: false,
      rationale: "-2 is above upper endpoint -3",
    },
  ]);
}

#[test]
fn range_endpoints_nint_to_nint_exclusive_upper() {
  // RFC 8610 Section 2.2.2.1: "..." includes the lower endpoint and excludes
  // only the upper endpoint.
  check(&[
    Claim {
      cddl: "t = -10...-3",
      cbor: b"\x2a",
      accept: false,
      rationale: "-11 is below lower endpoint -10",
    },
    Claim {
      cddl: "t = -10...-3",
      cbor: b"\x29",
      accept: true,
      rationale: "lower endpoint -10 is included",
    },
    Claim {
      cddl: "t = -10...-3",
      cbor: b"\x26",
      accept: true,
      rationale: "-7 is mid-window",
    },
    Claim {
      cddl: "t = -10...-3",
      cbor: b"\x22",
      accept: false,
      rationale: "upper endpoint -3 is excluded",
    },
    Claim {
      cddl: "t = -10...-3",
      cbor: b"\x21",
      accept: false,
      rationale: "-2 is above upper endpoint -3",
    },
  ]);
}

#[test]
fn range_endpoints_nint_to_uint_inclusive() {
  // RFC 8610 Section 2.2.2.1: integer ranges match integer values, including
  // sign-spanning ranges.
  check(&[
    Claim {
      cddl: "t = -10..10",
      cbor: b"\x2a",
      accept: false,
      rationale: "-11 is below lower endpoint -10",
    },
    Claim {
      cddl: "t = -10..10",
      cbor: b"\x29",
      accept: true,
      rationale: "lower endpoint -10 is included",
    },
    Claim {
      cddl: "t = -10..10",
      cbor: b"\x00",
      accept: true,
      rationale: "0 is mid-window",
    },
    Claim {
      cddl: "t = -10..10",
      cbor: b"\x0a",
      accept: true,
      rationale: "upper endpoint 10 is included",
    },
    Claim {
      cddl: "t = -10..10",
      cbor: b"\x0b",
      accept: false,
      rationale: "11 is above upper endpoint 10",
    },
  ]);
}

#[test]
fn range_endpoints_nint_to_uint_exclusive_upper() {
  // RFC 8610 Section 2.2.2.1: sign-spanning "..." ranges include the lower
  // endpoint and exclude only the upper endpoint.
  check(&[
    Claim {
      cddl: "t = -10...10",
      cbor: b"\x2a",
      accept: false,
      rationale: "-11 is below lower endpoint -10",
    },
    Claim {
      cddl: "t = -10...10",
      cbor: b"\x29",
      accept: true,
      rationale: "lower endpoint -10 is included",
    },
    Claim {
      cddl: "t = -10...10",
      cbor: b"\x00",
      accept: true,
      rationale: "0 is mid-window",
    },
    Claim {
      cddl: "t = -10...10",
      cbor: b"\x0a",
      accept: false,
      rationale: "upper endpoint 10 is excluded",
    },
    Claim {
      cddl: "t = -10...10",
      cbor: b"\x0b",
      accept: false,
      rationale: "11 is above upper endpoint 10",
    },
  ]);
}

#[test]
fn range_endpoints_float_to_float_inclusive() {
  // RFC 8610 Section 2.2.2.1: float ranges match floating-point values only.
  check(&[
    Claim {
      cddl: "t = 0.5..10.5",
      cbor: b"\xfb\xbf\xe0\x00\x00\x00\x00\x00\x00",
      accept: false,
      rationale: "-0.5 is below lower endpoint 0.5",
    },
    Claim {
      cddl: "t = 0.5..10.5",
      cbor: b"\xfb\x3f\xe0\x00\x00\x00\x00\x00\x00",
      accept: true,
      rationale: "lower endpoint 0.5 is included",
    },
    Claim {
      cddl: "t = 0.5..10.5",
      cbor: b"\xfb\x40\x16\x00\x00\x00\x00\x00\x00",
      accept: true,
      rationale: "5.5 is mid-window",
    },
    Claim {
      cddl: "t = 0.5..10.5",
      cbor: b"\xfb\x40\x25\x00\x00\x00\x00\x00\x00",
      accept: true,
      rationale: "upper endpoint 10.5 is included",
    },
    Claim {
      cddl: "t = 0.5..10.5",
      cbor: b"\xfb\x40\x27\x00\x00\x00\x00\x00\x00",
      accept: false,
      rationale: "11.5 is above upper endpoint 10.5",
    },
    Claim {
      cddl: "t = 0.5..10.5",
      cbor: b"\xf9\x7e\x00",
      accept: false,
      rationale: "NaN is unordered and must not match a bounded range",
    },
    Claim {
      cddl: "t = 0.5..10.5",
      cbor: b"\x05",
      accept: false,
      rationale: "float ranges match floating-point values, not integer encodings",
    },
  ]);
}

#[test]
fn range_endpoints_float_to_float_exclusive_upper() {
  // RFC 8610 Section 2.2.2.1: float "..." ranges include the lower endpoint
  // and exclude only the upper endpoint.
  check(&[
    Claim {
      cddl: "t = 0.5...10.5",
      cbor: b"\xfb\xbf\xe0\x00\x00\x00\x00\x00\x00",
      accept: false,
      rationale: "-0.5 is below lower endpoint 0.5",
    },
    Claim {
      cddl: "t = 0.5...10.5",
      cbor: b"\xfb\x3f\xe0\x00\x00\x00\x00\x00\x00",
      accept: true,
      rationale: "lower endpoint 0.5 is included",
    },
    Claim {
      cddl: "t = 0.5...10.5",
      cbor: b"\xfb\x40\x16\x00\x00\x00\x00\x00\x00",
      accept: true,
      rationale: "5.5 is mid-window",
    },
    Claim {
      cddl: "t = 0.5...10.5",
      cbor: b"\xfb\x40\x25\x00\x00\x00\x00\x00\x00",
      accept: false,
      rationale: "upper endpoint 10.5 is excluded",
    },
    Claim {
      cddl: "t = 0.5...10.5",
      cbor: b"\xfb\x40\x27\x00\x00\x00\x00\x00\x00",
      accept: false,
      rationale: "11.5 is above upper endpoint 10.5",
    },
    Claim {
      cddl: "t = 0.5...10.5",
      cbor: b"\xf9\x7e\x00",
      accept: false,
      rationale: "NaN is unordered and must not match a bounded range",
    },
  ]);
}

#[test]
fn range_endpoints_uint_to_uint_controls() {
  // RFC 8610 Section 2.2.2.1: uint..uint ranges (already supported) keep
  // working, with half-open semantics on "...".
  check(&[
    Claim {
      cddl: "t = 5..10",
      cbor: b"\x04",
      accept: false,
      rationale: "4 is below lower endpoint 5",
    },
    Claim {
      cddl: "t = 5..10",
      cbor: b"\x05",
      accept: true,
      rationale: "lower endpoint 5 is included",
    },
    Claim {
      cddl: "t = 5..10",
      cbor: b"\x07",
      accept: true,
      rationale: "7 is mid-window",
    },
    Claim {
      cddl: "t = 5..10",
      cbor: b"\x0a",
      accept: true,
      rationale: "upper endpoint 10 is included",
    },
    Claim {
      cddl: "t = 5..10",
      cbor: b"\x0b",
      accept: false,
      rationale: "11 is above upper endpoint 10",
    },
    Claim {
      cddl: "t = 5...10",
      cbor: b"\x04",
      accept: false,
      rationale: "4 is below lower endpoint 5",
    },
    Claim {
      cddl: "t = 5...10",
      cbor: b"\x05",
      accept: true,
      rationale: "lower endpoint 5 is included",
    },
    Claim {
      cddl: "t = 5...10",
      cbor: b"\x07",
      accept: true,
      rationale: "7 is mid-window",
    },
    Claim {
      cddl: "t = 5...10",
      cbor: b"\x0a",
      accept: false,
      rationale: "upper endpoint 10 is excluded",
    },
    Claim {
      cddl: "t = 5...10",
      cbor: b"\x0b",
      accept: false,
      rationale: "11 is above upper endpoint 10",
    },
  ]);
}
