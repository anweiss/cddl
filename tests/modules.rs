#![cfg(all(feature = "modules", feature = "std", not(target_arch = "wasm32")))]

//! Conformance tests for the CDDL module structure, drawn from the examples in
//! draft-ietf-cbor-cddl-modules-06.

use cddl::modules::{resolve_modules, MemoryModuleSource, ModuleError, ResolveOptions};

/// The CDDL of RFC 9052 used throughout §2.5 and §2.6 of the specification.
const RFC9052: &str = r#"COSE_Key = {
  1 => tstr / int,
  ? 2 => bstr,
  ? 3 => tstr / int,
  ? 4 => [+ tstr / int],
  ? 5 => bstr,
  * label => values,
}
empty_or_serialized_map = bstr .cbor header_map / bstr .size 0
header_map = {
  Generic_Headers,
  * label => values,
}
Generic_Headers = (
  ? 1 => int / tstr,
  ? 2 => [+ label],
  ? 3 => tstr / int,
  ? 4 => bstr,
  ? (5 => bstr // 6 => bstr),
  )
label = int / tstr
values = any
"#;

fn cose() -> MemoryModuleSource {
  let mut source = MemoryModuleSource::new();
  source.insert("rfc9052", RFC9052);
  source
}

fn resolve(input: &str) -> String {
  resolve_modules(input, &cose(), &ResolveOptions::default()).unwrap()
}

/// The order in which rules are defined in `output`.
fn defined_order(output: &str) -> Vec<String> {
  output
    .lines()
    .filter_map(|line| {
      if line.starts_with(' ') {
        return None;
      }

      let assign = line.find('=')?;

      // `=>` is an entry separator, not a rule definition.
      if line.as_bytes().get(assign + 1) == Some(&b'>') {
        return None;
      }

      let head = line[..assign].trim_end_matches('/').trim();

      if head.is_empty() || head.contains(' ') {
        return None;
      }

      Some(head.to_string())
    })
    .collect()
}

/// Every resolved document must be readable by a basic CDDL parser.
fn assert_parses(output: &str) {
  assert!(
    cddl::parser::cddl_from_str(output, false).is_ok(),
    "resolved output is not valid basic CDDL:\n{}",
    output
  );
  assert!(
    !output.contains(";#"),
    "resolved output still contains directives:\n{}",
    output
  );
}

#[test]
fn import_pulls_referenced_rules_transitively() {
  let output = resolve("start = COSE_Key\n;# import rfc9052\n");

  assert_eq!(
    defined_order(&output),
    ["start", "COSE_Key", "label", "values"]
  );
  assert!(!output.contains("header_map"));
  assert_parses(&output);
}

#[test]
fn import_as_namespaces_module_rules_but_not_the_prelude() {
  let output = resolve("start = cose.COSE_Key\n;# import rfc9052 as cose\n");

  assert_eq!(
    defined_order(&output),
    ["start", "cose.COSE_Key", "cose.label", "cose.values"]
  );
  assert!(output.contains("* cose.label => cose.values"));
  assert!(output.contains("cose.label = int / tstr"));
  // Prelude names are never prefixed.
  assert!(!output.contains("cose.tstr"));
  assert!(!output.contains("cose.int"));
  assert!(!output.contains("cose.any"));
  assert_parses(&output);
}

#[test]
fn generic_parameters_shadow_rules_only_within_their_own_rule() {
  let mut source = MemoryModuleSource::new();
  source.insert(
    "generic",
    "a = int\nb = bool\nmessages<a, b> = [a, b]\nother = a\n",
  );
  for (directive, prefix) in [("import generic", ""), ("import generic as ns", "ns.")] {
    let output = resolve_modules(
      &format!("start = {}messages<tstr, uint>\n;# {}\n", prefix, directive),
      &source,
      &ResolveOptions::default(),
    )
    .unwrap();
    assert!(output.contains(&format!("{}messages<a, b> = [a, b]", prefix)));
    assert!(!output.contains("a = int"));
    assert!(!output.contains("b = bool"));
    assert_parses(&output);
  }

  let output = resolve_modules(
    "start = [ns.messages<tstr, uint>, ns.other]\n;# import generic as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(output.contains("ns.other = ns.a"));
  assert!(output.contains("ns.a = int"));
  assert!(!output.contains("ns.b"));
  assert_parses(&output);
}

#[test]
fn bareword_keys_are_preserved_and_do_not_add_dependencies() {
  let mut source = MemoryModuleSource::new();
  source.insert(
    "keys",
    "label = int\nrecord = {label: tstr}\ncomputed = {* label => tstr}\n",
  );
  let output = resolve_modules(
    "start = ns.record\n;# import keys as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  assert_eq!(defined_order(&output), ["start", "ns.record"]);
  assert!(output.contains("ns.record = {label: tstr}"));
  assert_parses(&output);

  let output = resolve_modules(
    "start = [ns.record, ns.computed]\n;# import keys as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(output.contains("ns.record = {label: tstr}"));
  assert!(output.contains("ns.computed = {* ns.label => tstr}"));
  assert!(output.contains("ns.label = int"));
  assert_parses(&output);

  let output = resolve_modules(
    "start<a> = {label: a}\n;# import keys\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  assert_eq!(output, "start<a> = {label: a}\n");
  assert_parses(&output);
}

#[test]
fn byte_string_prefixes_are_preserved_and_do_not_add_dependencies() {
  let mut source = MemoryModuleSource::new();
  source.insert(
    "literals",
    "h = int\nb64 = int\npayload = [h'00', b64'AA==', h\"00\"]\nreferences = [h, b64]\n",
  );
  let output = resolve_modules(
    "start = ns.payload\n;# import literals as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  assert_eq!(defined_order(&output), ["start", "ns.payload"]);
  assert!(output.contains("ns.payload = [h'00', b64'AA==', h\"00\"]"));
  assert_parses(&output);

  let output = resolve_modules(
    "start = [ns.payload, ns.references]\n;# import literals as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(output.contains("ns.references = [ns.h, ns.b64]"));
  assert!(output.contains("ns.h = int"));
  assert!(output.contains("ns.b64 = int"));
  assert_parses(&output);
}

#[test]
fn include_from_takes_exactly_the_rules_named() {
  let output = resolve("mydata = {* label => values}\n;# include label, values from rfc9052\n");

  assert_eq!(defined_order(&output), ["mydata", "label", "values"]);
  assert!(!output.contains("COSE_Key"));
  assert_parses(&output);
}

#[test]
fn include_from_honors_an_already_namespaced_selector() {
  let output = resolve(
    "mydata = {* cose.label => cose.values}\n;# include cose.label, cose.values from rfc9052 as cose\n",
  );

  assert_eq!(
    defined_order(&output),
    ["mydata", "cose.label", "cose.values"]
  );
  assert_parses(&output);
}

#[test]
fn import_from_draws_in_the_transitive_closure_in_discovery_order() {
  let output = resolve(
    "mydata = {Fritz: cose.empty_or_serialized_map}\n;# import cose.empty_or_serialized_map from rfc9052 as cose\n",
  );

  assert_eq!(
    defined_order(&output),
    [
      "mydata",
      "cose.empty_or_serialized_map",
      "cose.header_map",
      "cose.Generic_Headers",
      "cose.label",
      "cose.values",
    ]
  );
  assert!(!output.contains("COSE_Key"));
  assert_parses(&output);
}

#[test]
fn import_from_without_the_prefix_also_defines_an_alias() {
  let output = resolve(
    "mydata = {Fritz: cose.empty_or_serialized_map}\n;# import empty_or_serialized_map from rfc9052 as cose\n",
  );

  assert!(output.contains("empty_or_serialized_map = cose.empty_or_serialized_map"));
  assert_eq!(
    defined_order(&output),
    [
      "mydata",
      "empty_or_serialized_map",
      "cose.empty_or_serialized_map",
      "cose.header_map",
      "cose.Generic_Headers",
      "cose.label",
      "cose.values",
    ]
  );
  assert_parses(&output);
}

#[test]
fn import_aliases_are_emitted_only_once() {
  let mut source = MemoryModuleSource::new();
  source.insert("module", "item = int\n");
  for directives in [
    ";# import item item from module as ns\n",
    ";# import item from module as ns\n;# import item from module as ns\n",
    ";# import item from module as ns\n;# include module\n",
    ";# import item from module as ns\n;# import item from module as other\n",
  ] {
    let output = resolve_modules(
      &format!("start = item\n{}", directives),
      &source,
      &ResolveOptions::default(),
    )
    .unwrap();
    assert_eq!(
      defined_order(&output)
        .iter()
        .filter(|n| *n == "item")
        .count(),
      1
    );
    assert!(output.contains("item = ns.item"));
    assert_parses(&output);
  }
}

#[cfg(all(feature = "json", feature = "cbor", feature = "csv-validate"))]
#[test]
fn filesystem_imports_validate_names_and_respect_empty_environment() {
  use cddl::modules::{FsModuleSource, ModuleSource, INCLUDE_PATH_VAR};
  use std::{fs, process::Command};

  let directory = std::env::temp_dir().join(format!("cddl-modules-{}", std::process::id()));
  fs::create_dir(&directory).unwrap();
  fs::create_dir(directory.join("modules")).unwrap();
  fs::write(directory.join("module.cddl"), "root = int\n").unwrap();

  let source = FsModuleSource::from_include_path(directory.to_str().unwrap());
  assert_eq!(
    source.load("module").unwrap().as_deref(),
    Some("root = int\n")
  );
  assert_eq!(
    source.load("module.cddl").unwrap().as_deref(),
    Some("root = int\n")
  );
  let restricted = FsModuleSource::from_include_path(directory.join("modules").to_str().unwrap());
  for name in [
    "../module".to_string(),
    directory.join("module.cddl").to_str().unwrap().to_string(),
  ] {
    let error = resolve_modules(
      "",
      &restricted,
      &ResolveOptions {
        start_rule: Some("ns.root".to_string()),
        command_line_imports: vec![("ns".to_string(), name)],
      },
    )
    .unwrap_err();
    assert!(matches!(error, ModuleError::Directive { line: 0, .. }));
  }

  let mut command = Command::new(env!("CARGO_BIN_EXE_cddl"));
  command
    .current_dir(&directory)
    .args(["resolve-modules", "-ins=module", "-sns.root"])
    .env_remove(INCLUDE_PATH_VAR);
  let unset = command.output().unwrap();
  assert!(unset.status.success(), "{:?}", unset);
  let empty = command.env(INCLUDE_PATH_VAR, "").output().unwrap();
  assert!(!empty.status.success(), "{:?}", empty);
  let unsafe_name = Command::new(env!("CARGO_BIN_EXE_cddl"))
    .current_dir(directory.join("modules"))
    .args(["resolve-modules", "-ins=../module", "-sns.root"])
    .env(INCLUDE_PATH_VAR, ".")
    .output()
    .unwrap();
  assert!(!unsafe_name.status.success(), "{:?}", unsafe_name);
  fs::remove_dir_all(directory).unwrap();
}

#[test]
fn include_without_a_from_clause_takes_every_rule() {
  let output = resolve("mydata = {* label => values}\n;# include rfc9052\n");
  let defined = defined_order(&output);

  for name in [
    "COSE_Key",
    "empty_or_serialized_map",
    "header_map",
    "Generic_Headers",
    "label",
    "values",
  ] {
    assert!(defined.iter().any(|d| d == name), "missing {}", name);
  }
  assert_parses(&output);
}

#[test]
fn wildcard_selector_takes_every_rule() {
  let output = resolve("mydata = {* label => values}\n;# include * from rfc9052\n");

  assert!(defined_order(&output).iter().any(|d| d == "COSE_Key"));
  assert_parses(&output);
}

#[test]
fn command_line_import_and_start_rule() {
  let options = ResolveOptions {
    start_rule: Some("cose.COSE_Key".to_string()),
    command_line_imports: vec![("cose".to_string(), "rfc9052".to_string())],
  };

  let output = resolve_modules("", &cose(), &options).unwrap();

  assert!(output.starts_with("$.start.$ = cose.COSE_Key\n"));
  assert_eq!(
    defined_order(&output),
    ["$.start.$", "cose.COSE_Key", "cose.label", "cose.values"]
  );
}

#[test]
fn command_line_imports_use_directive_validation() {
  for (namespace, module) in [
    ("bad..ns", "rfc9052"),
    ("bad--ns", "rfc9052"),
    ("bad.-ns", "rfc9052"),
    ("bad-.ns", "rfc9052"),
    ("bad.", "rfc9052"),
    ("bad-", "rfc9052"),
    ("", "rfc9052"),
    ("ns as other", "rfc9052"),
    ("ns\n;# include other", "rfc9052"),
    ("ns", ""),
    ("ns", "../rfc9052"),
    ("ns", "rfc9052 as other"),
    ("ns", "rfc9052\n;# include other"),
  ] {
    let options = ResolveOptions {
      command_line_imports: vec![(namespace.to_string(), module.to_string())],
      ..ResolveOptions::default()
    };
    assert!(matches!(
      resolve_modules("", &cose(), &options),
      Err(ModuleError::Directive { line: 0, .. })
    ));
  }

  for namespace in ["cose", "cose.ns", "cose-ns", "cose.ns-name"] {
    let input = format!("start = {}.COSE_Key\n", namespace);
    let options = ResolveOptions {
      command_line_imports: vec![(namespace.to_string(), "rfc9052".to_string())],
      ..ResolveOptions::default()
    };
    let output = resolve_modules(&input, &cose(), &options).unwrap();
    assert_eq!(
      output,
      resolve(&format!("{};# import rfc9052 as {}\n", input, namespace))
    );
    assert_parses(&output);
  }
}

#[test]
fn directives_are_resolved_transitively_through_modules() {
  let mut source = MemoryModuleSource::new();
  source.insert("rfc9052", RFC9052);
  source.insert(
    "middle",
    "wrapper = [* label]\n;# include label from rfc9052\n",
  );

  let output = resolve_modules(
    "top = wrapper\n;# import middle\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert_eq!(defined_order(&output), ["top", "wrapper", "label"]);
  assert_parses(&output);
}

#[test]
fn a_document_without_directives_is_unchanged() {
  let input = "a = int\nb = [* a]\n";
  let output = resolve(input);

  assert_eq!(output, input);
}

#[test]
fn ordinary_comments_survive_resolution() {
  let output = resolve("; a plain comment\na = int\n");

  assert!(output.contains("; a plain comment"));
}

#[test]
fn circular_module_references_are_detected() {
  let mut source = MemoryModuleSource::new();
  source.insert("one", "a = b\n;# include two\n");
  source.insert("two", "b = c\n;# include one\n");

  let error = resolve_modules(
    "start = a\n;# include one\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap_err();

  assert!(matches!(error, ModuleError::CircularReference { .. }));
}

#[test]
fn a_module_that_is_not_basic_cddl_is_reported_against_the_module() {
  let mut source = MemoryModuleSource::new();
  source.insert("broken", "label = \n");

  let error = resolve_modules(
    "start = broken.label\n;# import broken\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap_err();

  match error {
    ModuleError::ModuleParse { name, .. } => assert_eq!(name, "broken"),
    other => panic!("expected a ModuleParse error, got {:?}", other),
  }
}

#[test]
fn a_missing_module_is_reported_with_its_line() {
  let error = resolve_modules(
    "start = int\n;# import nowhere\n",
    &cose(),
    &ResolveOptions::default(),
  )
  .unwrap_err();

  assert_eq!(
    error,
    ModuleError::ModuleNotFound {
      name: "nowhere".to_string(),
      line: 2,
    }
  );
}

#[test]
fn a_missing_rule_is_reported_with_its_module() {
  let error = resolve_modules(
    "start = int\n;# include nonesuch from rfc9052\n",
    &cose(),
    &ResolveOptions::default(),
  )
  .unwrap_err();

  assert!(matches!(
    error,
    ModuleError::RuleNotFound { ref rule, ref module, line: 2 }
      if rule == "nonesuch" && module == "rfc9052"
  ));
}

#[test]
fn a_malformed_directive_is_an_error_not_a_comment() {
  let error = resolve_modules(
    "start = int\n;# improt rfc9052\n",
    &cose(),
    &ResolveOptions::default(),
  )
  .unwrap_err();

  assert!(matches!(error, ModuleError::Directive { line: 2, .. }));
}

#[test]
fn a_rule_carries_its_noncontiguous_extensions() {
  let mut source = MemoryModuleSource::new();
  source.insert("ext", "foo = int\nbar = baz\nbaz = tstr\nfoo /= tstr\n");

  let output = resolve_modules(
    "start = foo\n;# include foo from ext\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert!(output.contains("foo = int"), "{}", output);
  assert!(output.contains("foo /= tstr"), "{}", output);
  assert!(!output.contains("bar"), "{}", output);
  assert_parses(&output);
}

#[test]
fn a_later_import_sees_references_introduced_by_an_earlier_include() {
  let mut source = MemoryModuleSource::new();
  source.insert("module-a", "wrapper = [* external]\n");
  source.insert("module-b", "external = int\nunused = tstr\n");

  let output = resolve_modules(
    "root = wrapper\n;# include wrapper from module-a\n;# import module-b\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert_eq!(defined_order(&output), ["root", "wrapper", "external"]);
  assert_parses(&output);
}

#[test]
fn a_rule_already_defined_locally_is_not_pulled_in() {
  let output =
    resolve("label = tstr\nmydata = {* label => values}\n;# include label, values from rfc9052\n");

  let defined = defined_order(&output);
  assert_eq!(defined.iter().filter(|d| *d == "label").count(), 1);
  assert!(output.contains("label = tstr"));
  assert_parses(&output);
}
