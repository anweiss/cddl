use cddl_codegen_rust::cddl::parser::cddl_from_str;
use cddl_codegen_rust::{
  generate_rust_code, generate_rust_code_for_rule, generate_rust_code_for_rule_from_ast,
  generate_rust_code_from_ast, generate_rust_code_with_options, CodegenError, CodegenOptions,
};

#[test]
fn generates_all_types_from_source() {
  let generated = generate_rust_code(
    r#"
      person = { name: tstr }
      score = uint
    "#,
  )
  .unwrap();

  assert!(generated.contains("pub struct Person"));
  assert!(generated.contains("pub type Score = u64;"));
}

#[test]
fn generates_one_cddl_rule_with_an_output_name() {
  let generated = generate_rust_code_for_rule(
    r#"
      person-record = { name: tstr }
      score = uint
    "#,
    "person-record",
    Some("Person"),
    &CodegenOptions::default(),
  )
  .unwrap();

  assert!(generated.contains("pub struct Person"));
  assert!(!generated.contains("pub type Score"));
}

#[test]
fn generates_from_a_caller_managed_ast() {
  let source = "person = { name: tstr }";
  let cddl = cddl_from_str(source, false).unwrap();
  let generated = generate_rust_code_from_ast(&cddl, source, &CodegenOptions::default()).unwrap();

  assert!(generated.contains("pub struct Person"));
}

#[test]
fn applies_public_options() {
  let mut options = CodegenOptions::default();
  options.non_exhaustive = true;

  let generated = generate_rust_code_with_options("person = { name: tstr }", &options).unwrap();

  assert!(generated.contains("#[non_exhaustive]"));
}

#[test]
fn reports_parse_errors() {
  let error = generate_rust_code("person = {").unwrap_err();

  assert!(matches!(error, CodegenError::ParseError(_)));
}

#[test]
fn single_rule_ast_and_source_apis_agree() {
  let source = "person-record = { name: tstr }\nscore = uint";
  let cddl = cddl_from_str(source, false).unwrap();
  let options = CodegenOptions::default();
  let expected = generate_rust_code_for_rule(source, "person-record", None, &options).unwrap();
  let actual =
    generate_rust_code_for_rule_from_ast(&cddl, source, "person-record", None, &options).unwrap();
  assert_eq!(actual, expected);
  assert!(actual.contains("pub struct PersonRecord"));
  assert!(!actual.contains("pub type Score"));
}

#[test]
fn missing_rule_is_an_error() {
  let error = generate_rust_code_for_rule(
    "person = { name: tstr }",
    "missing",
    None,
    &CodegenOptions::default(),
  )
  .unwrap_err();
  assert!(error.to_string().contains("no rule matching 'missing'"));
}

#[test]
fn errors_support_standard_error_handling() {
  fn generate() -> Result<(), Box<dyn std::error::Error>> {
    generate_rust_code("person = {")?;
    Ok(())
  }
  let error = generate().unwrap_err();
  assert!(error.downcast_ref::<CodegenError>().is_some());
  let error = CodegenError::from(std::fmt::Error);
  assert!(std::error::Error::source(&error).is_some());
}

#[test]
fn public_options_preserve_substitutions_and_aliases() {
  let mut options = CodegenOptions::default();
  options.any_type = Some("ciborium::Value".into());
  options.other_variant = true;
  options.fundamental_aliases = true;
  options
    .substitutions
    .insert("record.name".into(), "crate::Name".into());
  let output = generate_rust_code_with_options(
    "record = { name: tstr, data: any, count: uint }\nkind = \"one\" / \"two\"",
    &options,
  )
  .unwrap();
  assert!(output.contains("pub name: crate::Name,"));
  assert!(output.contains("pub data: Any,"));
  assert!(output.contains("pub type Any = ciborium::Value;"));
  assert!(output.contains("pub type Uint = u64;"));
  assert!(output.contains("Other(String),"));
}

#[test]
fn mixed_literals_have_string_serialization() {
  let output = generate_rust_code(r#"action = "created" / tstr"#).unwrap();
  assert!(output.contains(r#"Self::Created => s.serialize_str("created")"#));
}

#[test]
fn equivalent_choice_variants_are_emitted_once() {
  for source in [
    r#"u = tstr .regexp "a" / tstr .regexp "b""#,
    r#"u = (tstr .regexp "a" / tstr .regexp "b")"#,
    "$u /= tstr .regexp \"a\"\n$u /= tstr .regexp \"b\"",
  ] {
    let output = generate_rust_code(source).unwrap();
    assert_eq!(output.matches("Tstr(String),").count(), 1, "{output}");
  }
}

#[test]
fn deduplicating_choices_preserves_distinct_types_and_literals() {
  let output = generate_rust_code(
    r#"u = "a" / tstr .regexp "a" / uint .size 1 / tstr .regexp "b" / uint .size 2 / bstr"#,
  )
  .unwrap();
  let definition = output.split("}\n").next().unwrap();
  for variant in ["A,", "Tstr(String),", "Uint(u64),", "Bstr(Vec<u8>),"] {
    assert_eq!(definition.matches(variant).count(), 1, "{output}");
  }
  assert!(output.contains(r#"Self::A => s.serialize_str("a")"#));
}

#[test]
fn distinct_literals_with_colliding_names_are_not_deduplicated() {
  let output = generate_rust_code(r#"u = "foo-bar" / "foo_bar" / "foo-bar" / "foo-bar2""#).unwrap();
  for text in ["foo-bar", "foo_bar", "foo-bar2"] {
    assert!(output.contains(&format!("{text:?} => Self::")), "{output}");
  }
  assert_eq!(output.matches("    FooBar,").count(), 1, "{output}");
  assert_eq!(output.matches("    FooBar2,").count(), 1, "{output}");
  assert_eq!(output.matches("    FooBar3,").count(), 1, "{output}");
}

#[test]
fn quoted_arrow_keys_are_fields_in_both_parser_apis() {
  let source = r#"record = { "name" => tstr, ? "count" => uint }"#;
  let output = generate_rust_code(source).unwrap();
  assert!(output.contains("pub name: String,"));
  assert!(output.contains("pub count: Option<u64>,"));
  let ast = cddl_from_str(source, false).unwrap();
  assert_eq!(
    generate_rust_code_from_ast(&ast, source, &CodegenOptions::default()).unwrap(),
    output
  );
}

#[test]
fn wildcard_entries_are_flattened() {
  let output = generate_rust_code("record = { name: tstr, * tstr => any }").unwrap();
  assert!(output.contains("#[serde(flatten)]"));
}

#[test]
fn included_groups_are_fields_but_keyed_groups_stay_nested() {
  let source = r#"
    common = ( ? "d": tstr )
    item = { "a": int, common, }
    nested = { "a": int, "common": common }
  "#;
  for output in [
    generate_rust_code(source).unwrap(),
    generate_rust_code_from_ast(
      &cddl_from_str(source, false).unwrap(),
      source,
      &CodegenOptions::default(),
    )
    .unwrap(),
  ] {
    let item = output
      .split("pub struct Item {")
      .nth(1)
      .unwrap()
      .split('}')
      .next()
      .unwrap();
    assert!(item.contains("pub a: i64,"));
    assert!(item.contains("pub d: Option<String>,"));
    assert!(!item.contains("common"));
    assert!(output.contains("pub common: Common,"));
  }
}

#[test]
fn recursive_group_inclusion_reports_an_error() {
  let error = generate_rust_code("common = (? key: tstr, common)\nitem = { common }").unwrap_err();
  assert!(
    error.to_string().contains("recursive group inclusion"),
    "{error}"
  );
}

#[test]
fn group_substitutions_skip_expansion_and_preserve_optional_references() {
  let mut options = CodegenOptions::default();
  options
    .substitutions
    .insert("common".into(), "crate::Custom".into());
  let source = "common = (d: tstr, common)\nitem = { ? common }";
  let output = generate_rust_code_with_options(source, &options).unwrap();
  assert!(output.contains("pub common: Option<crate::Custom>,"));
  assert!(!output.contains("pub struct Common"));
  for field in ["? 1 => int", "* tstr => int"] {
    let source = format!("common = (d: tstr)\nitem = {{ common, {field} }}");
    let error = generate_rust_code_with_options(&source, &options).unwrap_err();
    assert!(error.to_string().contains("not supported"), "{error}");
  }
}

#[test]
fn group_choices_do_not_silently_bypass_typed_key_dispatch() {
  for field in ["? 1 => int", "* tstr => int"] {
    for source in [
      format!("item = {{ {field}, (a: tstr // b: tstr) }}"),
      format!("item = {{ ({field} // b: tstr) }}"),
    ] {
      let error = generate_rust_code(&source).unwrap_err();
      assert!(
        error.to_string().contains("group choices combined"),
        "{error}"
      );
    }
  }
}

#[test]
fn embedded_acronyms_are_normalized() {
  let output = generate_rust_code(
    r#"hashedURI-map = { url: tstr }
status = "assertion.collectionHash.invalidURI" / "ok""#,
  )
  .unwrap();
  assert!(output.contains("pub struct HashedUriMap"));
  assert!(output.contains("AssertionCollectionHashInvalidUri"));
}

#[test]
fn explicit_cbor_tags_have_encoding_adapters() {
  let output = generate_rust_code("buuid = #6.37(bstr)\nrecord = { id: buuid }").unwrap();
  assert!(output.contains("Tagged<37, serde_with::Bytes>"));
  assert!(output.contains("pub type Buuid = Vec<u8>;"));
  assert!(!output.contains("__CddlExplicitTag"));
  let mut options = CodegenOptions::default();
  options
    .substitutions
    .insert("buuid".into(), "String".into());
  let output =
    generate_rust_code_with_options("buuid = #6.37(bstr)\nrecord = { id: buuid }", &options)
      .unwrap();
  assert!(!output.contains("Tagged<37"));
}
