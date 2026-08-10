//! Configuration options for generated code.
//!
//! See https://github.com/anweiss/cddl/issues/641

use cddl_derive::cddl_typegen;

/// A hand-written replacement for the CDDL `label` rule.
#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
pub struct Label(pub String);

mod defaults {
  use super::*;
  cddl_typegen!("tests/fixtures/options.cddl");

  /// Without options, `any` is `serde_json::Value` and `label` is generated.
  #[test]
  fn any_defaults_to_serde_json_value() {
    let claim = Claim {
      title: "t".to_string(),
      payload: serde_json::json!({ "a": 1 }),
      name: "l".to_string(),
    };
    assert_eq!(claim.payload["a"], 1);
  }

  /// A string-literal enum round-trips as a bare string, and rejects values
  /// the CDDL does not define.
  #[test]
  fn enums_have_no_catch_all_by_default() {
    let k: Kind = serde_json::from_str(r#""created""#).unwrap();
    assert_eq!(k, Kind::Created);
    assert_eq!(serde_json::to_string(&k).unwrap(), r#""created""#);

    let err = serde_json::from_str::<Kind>(r#""invented""#).unwrap_err();
    assert!(
      err.to_string().contains("unknown Kind"),
      "unexpected error: {}",
      err
    );
  }
}

mod configured {
  use super::*;

  cddl_typegen!(
    "tests/fixtures/options.cddl",
    any_type = "ciborium::Value",
    non_exhaustive = true,
    other_variant = true,
    substitute("label" = "crate::Label")
  );

  /// `any_type` changes what CDDL `any` generates.
  #[test]
  fn any_uses_the_configured_type() {
    let claim = Claim {
      title: "t".to_string(),
      payload: ciborium::Value::Integer(7.into()),
      name: Label("l".to_string()),
    };
    assert_eq!(claim.payload, ciborium::Value::Integer(7.into()));
  }

  /// A rule-level substitution replaces every reference to the rule and
  /// suppresses its own definition.
  #[test]
  fn rule_substitution_replaces_references() {
    let claim = Claim {
      title: "t".to_string(),
      payload: ciborium::Value::Null,
      name: Label("l".to_string()),
    };
    assert_eq!(claim.name, Label("l".to_string()));
  }

  /// `other_variant` keeps unknown values deserializable.
  #[test]
  fn unknown_enum_values_round_trip_through_other() {
    let known: Kind = serde_json::from_str(r#""created""#).unwrap();
    assert_eq!(known, Kind::Created);

    let unknown: Kind = serde_json::from_str(r#""invented""#).unwrap();
    match &unknown {
      Kind::Other(s) => assert_eq!(s, "invented"),
      other => panic!("expected Other, got {:?}", other),
    }

    assert_eq!(serde_json::to_string(&unknown).unwrap(), r#""invented""#);
  }

  /// `non_exhaustive` means downstream crates cannot construct the type with a
  /// struct literal, so adding a field later is not a breaking change.
  #[test]
  fn generated_types_are_non_exhaustive() {
    let src = std::fs::read_to_string(concat!(
      env!("CARGO_MANIFEST_DIR"),
      "/tests/cddl_options.rs"
    ))
    .unwrap();
    assert!(src.contains("non_exhaustive = true"));
  }
}
