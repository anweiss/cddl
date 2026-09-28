use cddl_codegen::cddl::parser::cddl_from_str;
use cddl_codegen::{
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
