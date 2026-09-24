type OuterValue = serde_json::Value;

#[allow(non_camel_case_types)]
mod names {
  use cddl_derive::cddl;
  #[cddl(
    path = "tests/fixtures/fundamental_scoping.cddl",
    rule = "scoped",
    fundamental_aliases = true
  )]
  struct CasePair;
  #[cddl(
    path = "tests/fixtures/fundamental_scoping.cddl",
    rule = "scoped",
    fundamental_aliases = true
  )]
  struct case_pair;
  #[cddl(
    path = "tests/fixtures/fundamental_scoping.cddl",
    rule = "scoped",
    fundamental_aliases = true
  )]
  struct r#type;
}

mod relative {
  use cddl_derive::cddl;
  type Any = serde_json::Value;

  #[cddl(
    path = "tests/fixtures/fundamental_scoping.cddl",
    rule = "scoped",
    fundamental_aliases = true,
    any_type = "super::OuterValue"
  )]
  struct ParentPath;
  #[cddl(
    path = "tests/fixtures/fundamental_scoping.cddl",
    rule = "scoped",
    fundamental_aliases = true,
    any_type = "self::Any"
  )]
  struct SelfPath;
}

mod whole_file {
  pub mod external {
    pub type Any = serde_json::Value;
  }
  cddl_derive::cddl_typegen!(
    "tests/fixtures/fundamental_scoping.cddl",
    fundamental_aliases = true,
    any_type = "self::external::Any"
  );
}

fn round_trip<T: serde::Serialize + serde::de::DeserializeOwned>() {
  let json = serde_json::json!({
    "name": "test", "when": "2026-09-24T00:00:00Z", "payload": 7
  });
  let record: T = serde_json::from_value(json.clone()).unwrap();
  let mut bytes = Vec::new();
  ciborium::into_writer(&record, &mut bytes).unwrap();
  let decoded: T = ciborium::from_reader(bytes.as_slice()).unwrap();
  assert_eq!(serde_json::to_value(decoded).unwrap(), json);
}

#[test]
fn distinct_and_raw_struct_names_have_distinct_valid_helper_modules() {
  round_trip::<names::CasePair>();
  round_trip::<names::case_pair>();
  round_trip::<names::r#type>();
}

#[test]
fn configured_relative_paths_keep_the_callers_scope() {
  round_trip::<relative::ParentPath>();
  round_trip::<relative::SelfPath>();
  round_trip::<whole_file::Scoped>();
}
