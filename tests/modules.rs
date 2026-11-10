#![cfg(all(feature = "modules", feature = "std", not(target_arch = "wasm32")))]

//! Conformance tests for the CDDL module structure, drawn from the examples in
//! draft-ietf-cbor-cddl-modules-07.

use cddl::modules::{
  resolve_modules, MemoryModuleSource, ModuleError, ResolveOptions, ResolvedCddl,
};

#[cfg(all(any(feature = "json", feature = "cbor"), not(feature = "lsp")))]
fn assert_silent_validation(test_name: &str, validate: impl FnOnce()) {
  const CHILD_TEST: &str = "CDDL_SILENT_VALIDATION_TEST";
  if std::env::var(CHILD_TEST).as_deref() == Ok(test_name) {
    validate();
    return;
  }

  // Capture OS-level stderr in a child, including codespan diagnostics that
  // bypass the Rust test harness's output capture.
  let output = std::process::Command::new(std::env::current_exe().unwrap())
    .args(["--exact", test_name, "--nocapture"])
    .env(CHILD_TEST, test_name)
    .output()
    .unwrap();
  assert!(output.status.success(), "validation subprocess failed");
  assert!(output.stderr.is_empty(), "validation wrote to stderr");
  assert!(
    String::from_utf8_lossy(&output.stdout).contains("test result: ok. 1 passed; 0 failed;"),
    "validation subprocess did not run exactly one test"
  );
}

#[cfg(all(any(feature = "json", feature = "cbor"), not(feature = "lsp")))]
#[test]
#[should_panic(expected = "validation subprocess did not run exactly one test")]
fn owning_document_silent_validation_rejects_missing_tests() {
  assert_silent_validation("<missing validation test>", || {});
}

#[cfg(all(feature = "json", not(feature = "lsp")))]
#[test]
fn owning_document_json_parse_errors_are_silent() {
  assert_silent_validation("owning_document_json_parse_errors_are_silent", || {
    let document = ResolvedCddl::resolve(
      "private_schema = {",
      &MemoryModuleSource::new(),
      &ResolveOptions::default(),
    )
    .unwrap();
    let result = document.validate_json(
      "1",
      #[cfg(feature = "additional-controls")]
      None,
    );
    match result {
      Err(cddl::validator::json::Error::CDDLParsing(error)) => {
        assert_eq!(error, document.parse().unwrap_err().to_string());
      }
      _ => panic!("expected a CDDL parsing error"),
    }
  });
}

#[cfg(all(feature = "cbor", not(feature = "lsp")))]
#[test]
fn owning_document_cbor_parse_errors_are_silent() {
  assert_silent_validation("owning_document_cbor_parse_errors_are_silent", || {
    let document = ResolvedCddl::resolve(
      "private_schema = {",
      &MemoryModuleSource::new(),
      &ResolveOptions::default(),
    )
    .unwrap();
    let result = document.validate_cbor(
      &[0x01],
      #[cfg(feature = "additional-controls")]
      None,
    );
    match result {
      Err(cddl::validator::cbor::Error::CDDLParsing(error)) => {
        assert_eq!(error, document.parse().unwrap_err().to_string());
      }
      _ => panic!("expected a CDDL parsing error"),
    }
  });
}

#[test]
fn owning_document_outlives_input_and_source() {
  let document = {
    let mut source = MemoryModuleSource::new();
    source.insert("numbers", "value = uint\n");
    let input = String::from("start = n.value\n;# import numbers as n\n");
    ResolvedCddl::resolve(&input, &source, &ResolveOptions::default()).unwrap()
  };
  assert!(document.text().contains("n.value = uint"));
  assert_eq!(document.parse().unwrap().rules.len(), 2);
  assert_eq!(document.clone(), document);
}

#[test]
fn owning_document_distinguishes_resolution_and_root_parse_errors() {
  assert!(matches!(
    ResolvedCddl::resolve(
      ";# include missing\n",
      &MemoryModuleSource::new(),
      &ResolveOptions::default()
    ),
    Err(ModuleError::ModuleNotFound { line: 1, .. })
  ));
  let document = ResolvedCddl::resolve(
    "start = {",
    &MemoryModuleSource::new(),
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(document.parse().is_err());
}

#[cfg(all(any(feature = "json", feature = "cbor"), not(feature = "lsp")))]
#[test]
fn owning_document_validation_rejects_undefined_rules() {
  let document = ResolvedCddl::resolve(
    "start = undefined_rule\n",
    &MemoryModuleSource::new(),
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(document.parse().is_ok());
  #[cfg(feature = "json")]
  assert!(matches!(
    document.validate_json(
      "1",
      #[cfg(feature = "additional-controls")]
      None,
    ),
    Err(cddl::validator::json::Error::Validation(_))
  ));
  #[cfg(feature = "cbor")]
  assert!(matches!(
    document.validate_cbor(
      &[0x01],
      #[cfg(feature = "additional-controls")]
      None,
    ),
    Err(cddl::validator::cbor::Error::Validation(_))
  ));
}

#[cfg(all(any(feature = "json", feature = "cbor"), not(feature = "lsp")))]
#[test]
fn owning_document_validation_failures_are_silent() {
  assert_silent_validation("owning_document_validation_failures_are_silent", || {
    let document = ResolvedCddl::resolve(
      "start = uint\n",
      &MemoryModuleSource::new(),
      &ResolveOptions::default(),
    )
    .unwrap();
    #[cfg(feature = "json")]
    for json in ["-1", "{"] {
      assert!(document
        .validate_json(
          json,
          #[cfg(feature = "additional-controls")]
          None,
        )
        .is_err());
    }
    #[cfg(feature = "cbor")]
    for cbor in [&[0x20][..], &[]] {
      assert!(document
        .validate_cbor(
          cbor,
          #[cfg(feature = "additional-controls")]
          None,
        )
        .is_err());
    }
  });
}

#[cfg(all(feature = "json", not(feature = "lsp")))]
#[test]
fn owning_document_validates_json() {
  let mut source = MemoryModuleSource::new();
  source.insert("numbers", "value = uint\n");
  let document = ResolvedCddl::resolve(
    "start = value\n;# import numbers\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  #[cfg(feature = "additional-controls")]
  let validate = |json: &str| document.validate_json(json, None);
  #[cfg(not(feature = "additional-controls"))]
  let validate = |json: &str| document.validate_json(json);
  assert!(validate("1").is_ok());
  assert!(validate("-1").is_err());
  assert!(validate("{").is_err());
}

#[cfg(all(feature = "cbor", not(feature = "lsp")))]
#[test]
fn owning_document_validates_cbor() {
  let document = ResolvedCddl::resolve(
    "start = n.value\n;# import numbers as n\n",
    &{
      let mut source = MemoryModuleSource::new();
      source.insert("numbers", "value = uint\n");
      source
    },
    &ResolveOptions::default(),
  )
  .unwrap();
  #[cfg(feature = "additional-controls")]
  let validate = |bytes: &[u8]| document.validate_cbor(bytes, None);
  #[cfg(not(feature = "additional-controls"))]
  let validate = |bytes: &[u8]| document.validate_cbor(bytes);
  assert!(validate(&[0x01]).is_ok());
  assert!(validate(&[0x20]).is_err());
  for bytes in [&[][..], &[0x18], &[0xff], &[0x01, 0x02]] {
    let error = validate(bytes).unwrap_err();
    assert!(matches!(
      error,
      cddl::validator::cbor::Error::CBORParsing(_)
    ));
    assert!(error.to_string().starts_with("error parsing cbor:"));
  }
}

#[cfg(all(
  feature = "additional-controls",
  feature = "json",
  not(feature = "lsp")
))]
#[test]
fn owning_document_json_forwards_enabled_features() {
  let document = ResolvedCddl::resolve(
    "start = uint .feature \"numbers\"\n",
    &MemoryModuleSource::new(),
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(document.validate_json("1", Some(&["numbers"])).is_ok());
  assert!(document.validate_json("-1", Some(&["numbers"])).is_err());
  assert!(document.validate_json("-1", Some(&[])).is_ok());
}

#[cfg(all(
  feature = "additional-controls",
  feature = "cbor",
  not(feature = "lsp")
))]
#[test]
fn owning_document_cbor_forwards_enabled_features() {
  let document = ResolvedCddl::resolve(
    "start = 1 .feature \"fixed\"\n",
    &MemoryModuleSource::new(),
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(document.validate_cbor(&[0x01], Some(&["fixed"])).is_ok());
  assert!(document.validate_cbor(&[0x02], Some(&["fixed"])).is_err());
  assert!(document.validate_cbor(&[0x02], Some(&["other"])).is_ok());
  assert!(document.validate_cbor(&[0x02], Some(&[])).is_ok());
  assert!(document.validate_cbor(&[0x02], None).is_ok());
}

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
fn byte_strings_with_trailing_backslashes_preserve_following_rules() {
  for literal in [
    r#"h"raw\""#,
    r"'raw\'",
    "h'00 ; raw\\'",
    "b64'AA== ; raw\\'",
  ] {
    let mut source = MemoryModuleSource::new();
    source.insert(
      "literals",
      format!("payload = {}\nfollowing = int\n", literal),
    );
    let output = resolve_modules(
      "start = [ns.payload, ns.following]\n;# import literals as ns\n",
      &source,
      &ResolveOptions::default(),
    )
    .unwrap();
    assert_eq!(
      defined_order(&output),
      ["start", "ns.payload", "ns.following"]
    );
    assert!(output.contains(&format!("ns.payload = {}", literal)));
    assert!(output.contains("ns.following = int"));
    assert_parses(&output);
  }
}

#[test]
fn numeric_literals_are_preserved_and_do_not_add_dependencies() {
  for literal in [
    "1e+5",
    "-1e-5",
    "1E5",
    "1.05e05",
    "-0.5E+2",
    "0x10",
    "-0X10",
    "0b10",
    "-0B10",
    "0x1.8p+1",
    "-0X1.AP-1",
    "0x1p1",
    "#6.0x10(tstr)",
    "#7.0b100000",
  ] {
    let mut source = MemoryModuleSource::new();
    source.insert(
      "literals",
      format!(
        "e = int\nE5 = int\ne05 = int\nE = int\nx10 = int\nX10 = int\nb10 = int\nB10 = int\nx1 = int\nX1 = int\np = int\np1 = int\nb100000 = int\nvalue = {}\n",
        literal
      ),
    );
    let output = resolve_modules(
      "start = ns.value\n;# import literals as ns\n",
      &source,
      &ResolveOptions::default(),
    )
    .unwrap();
    assert_eq!(defined_order(&output), ["start", "ns.value"], "{}", literal);
    assert!(output.contains(&format!("ns.value = {}", literal)));
    assert_parses(&output);
  }
}

#[test]
fn range_endpoints_are_imported_and_namespaced() {
  for (range, expected) in [
    ("0..limit", "0..ns.limit"),
    ("0...limit", "0...ns.limit"),
    ("lower..limit", "ns.lower..ns.limit"),
    ("lower...limit", "ns.lower...ns.limit"),
    ("0x0..limit", "0x0..ns.limit"),
    ("0.5...limit", "0.5...ns.limit"),
    ("lower .. bounds.limit", "ns.lower .. ns.bounds.limit"),
    ("lower..bounds.limit", "ns.lower..ns.bounds.limit"),
  ] {
    let mut source = MemoryModuleSource::new();
    source.insert(
      "ranges",
      format!(
        "lower = 0\nlimit = 10\nbounds.limit = 10\nvalue = {}\n",
        range
      ),
    );
    let output = resolve_modules(
      "start = ns.value\n;# import ranges as ns\n",
      &source,
      &ResolveOptions::default(),
    )
    .unwrap();
    assert!(output.contains(&format!("ns.value = {}", expected)));
    let upper = if range.contains("bounds.limit") {
      "bounds.limit"
    } else {
      "limit"
    };
    assert!(output.contains(&format!("ns.{} = 10", upper)));
    assert_eq!(output.contains("ns.lower = 0"), range.contains("lower"));
    assert_parses(&output);
  }
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
  for args in [
    vec!["resolve-modules", "-i", "private-invalid-spec"],
    vec!["resolve-modules", "--cddl", "private-missing-file"],
  ] {
    let invalid = Command::new(env!("CARGO_BIN_EXE_cddl"))
      .current_dir(&directory)
      .args(args)
      .output()
      .unwrap();
    assert!(!invalid.status.success());
    assert!(invalid.stdout.is_empty());
    assert!(!String::from_utf8_lossy(&invalid.stderr).contains("private-"));
  }
  fs::write(
    directory.join("broken.cddl"),
    "private_rule = { private_content",
  )
  .unwrap();
  let broken = Command::new(env!("CARGO_BIN_EXE_cddl"))
    .current_dir(&directory)
    .args(["resolve-modules", "-ins=broken", "-sns.private_rule"])
    .env(INCLUDE_PATH_VAR, ".")
    .output()
    .unwrap();
  assert!(!broken.status.success());
  assert!(!String::from_utf8_lossy(&broken.stderr).contains("private_"));
  assert!(!String::from_utf8_lossy(&broken.stderr).contains("broken"));
  for start in ["", "ns.root other", "ns.root\ninjected = int"] {
    let invalid_start = Command::new(env!("CARGO_BIN_EXE_cddl"))
      .current_dir(&directory)
      .args(["resolve-modules", "-ins=module", "-s", start])
      .env(INCLUDE_PATH_VAR, ".")
      .output()
      .unwrap();
    assert!(!invalid_start.status.success(), "{:?}", invalid_start);
    assert!(invalid_start.stdout.is_empty());
    assert!(String::from_utf8_lossy(&invalid_start.stderr).contains("not a valid start rule"));
  }
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
  assert_parses(&output);
}

#[test]
fn start_rules_use_identifier_validation() {
  for name in [
    "",
    " ",
    "root name",
    "root\ninjected = int",
    "root; comment",
    "0root",
    "root..name",
    "root--name",
    "root.",
    "root-",
  ] {
    let error = resolve_modules(
      "root = int\n",
      &cose(),
      &ResolveOptions {
        start_rule: Some(name.to_string()),
        ..ResolveOptions::default()
      },
    )
    .unwrap_err();
    assert_eq!(
      error,
      ModuleError::Directive {
        line: 0,
        message: format!("\"{}\" is not a valid start rule", name),
      }
    );
  }
  for name in [
    "root",
    "ns.root",
    "root-name",
    "ns.0",
    "$socket",
    "@root",
    "_root",
  ] {
    let output = resolve_modules(
      &format!("{} = int\n", name),
      &cose(),
      &ResolveOptions {
        start_rule: Some(name.to_string()),
        ..ResolveOptions::default()
      },
    )
    .unwrap();
    assert!(output.starts_with(&format!("$.start.$ = {}\n", name)));
  }
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
fn directive_text_inside_literals_is_preserved() {
  for literal in [
    "'before\n;# import missing\nafter'",
    "'before\n;# not a directive\nafter'",
    "\"before\n;# import missing\nafter\"",
    "h\"before\n;# import missing\nafter\"",
    "\"escaped \\\" quote\n;# import missing\nafter\"",
    "'before\r\n;# import missing\r\nafter'",
  ] {
    let input = format!("value = {}\n;# include label from rfc9052\n", literal);
    let output = resolve(&input);
    assert!(output.contains(&format!("value = {}\n", literal)));
    assert!(output.contains("label = int / tstr"));
    assert!(cddl::parser::cddl_from_str(&output, false).is_ok());

    let mut source = cose();
    source.insert("literal", &input);
    let output = resolve_modules(
      "root = ns.value\n;# import literal as ns\n",
      &source,
      &ResolveOptions::default(),
    )
    .unwrap();
    assert!(output.contains(&format!("ns.value = {}\n", literal)));
    assert!(cddl::parser::cddl_from_str(&output, false).is_ok());
  }
}

#[test]
fn directive_lines_keep_their_source_line_after_multiline_literals() {
  let error = resolve_modules(
    "value = 'before\n;# import ignored\nafter'\n;# import missing\n",
    &cose(),
    &ResolveOptions::default(),
  )
  .unwrap_err();
  assert_eq!(
    error,
    ModuleError::ModuleNotFound {
      name: "missing".to_string(),
      line: 4,
    }
  );
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
fn an_imported_group_alias_stays_a_group() {
  let mut source = MemoryModuleSource::new();
  source.insert("m", "options = (one: 1, two: 2)\n");

  let output = resolve_modules(
    "root = &options\n;# import options from m as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert!(output.contains("options = (ns.options,)"), "{}", output);
  assert_parses(&output);

  #[cfg(all(
    feature = "json",
    not(feature = "lsp"),
    feature = "additional-controls"
  ))]
  cddl::validate_json_from_str(&output, "1", None).unwrap();
  #[cfg(all(
    feature = "json",
    not(feature = "lsp"),
    not(feature = "additional-controls")
  ))]
  cddl::validate_json_from_str(&output, "1").unwrap();
}

#[test]
fn explicitly_extended_prelude_rules_travel_with_their_dependents() {
  let mut source = MemoryModuleSource::new();
  source.insert("m", "value = int\nint /= tstr\n");
  for (input, head) in [
    ("root = value\n;# import m\n", "value = int"),
    ("root = ns.value\n;# import m as ns\n", "ns.value = int"),
  ] {
    let output = resolve_modules(input, &source, &ResolveOptions::default()).unwrap();
    assert!(output.contains(head), "{}", output);
    assert!(output.contains("int /= tstr"), "{}", output);
    assert!(!output.contains("ns.int"), "{}", output);
    assert_parses(&output);
  }
}

#[test]
fn namespaced_sockets_keep_their_socket_kind() {
  let mut source = MemoryModuleSource::new();
  source.insert(
    "m",
    "$$options = (one: 1)\n$kind = uint\nholder = [$$options, $kind]\n",
  );
  let output = resolve_modules(
    "root = [&$$ns.options, ns.holder]\n;# import m as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(output.contains("$$ns.options = (one: 1)"), "{}", output);
  assert!(output.contains("$ns.kind = uint"), "{}", output);
  assert!(
    output.contains("ns.holder = [$$ns.options, $ns.kind]"),
    "{}",
    output
  );
  assert_parses(&output);
  let cddl = cddl::parser::cddl_from_str(&output, false).unwrap();
  assert!(cddl.rules.iter().any(|rule| matches!(
    rule,
    cddl::ast::Rule::Group { rule, .. } if rule.name.to_string() == "$$ns.options"
  )));

  let output = resolve_modules(
    "root = &$$options\n;# import $$options from m as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(output.contains("$$options = ($$ns.options,)"), "{}", output);
  assert_parses(&output);
}

#[test]
fn dollar_identifiers_that_are_not_sockets_take_a_plain_prefix() {
  let mut source = MemoryModuleSource::new();
  source.insert("m", "$-value = int\n$1 = tstr\nholder = [$-value, $1]\n");
  for kind in ["include", "import"] {
    let input = format!("root = ns.holder\n;# {} m as ns\n", kind);
    let output = resolve_modules(&input, &source, &ResolveOptions::default()).unwrap();
    assert!(output.contains("ns.$-value = int"), "{}", output);
    assert!(output.contains("ns.$1 = tstr"), "{}", output);
    assert!(
      output.contains("ns.holder = [ns.$-value, ns.$1]"),
      "{}",
      output
    );
    assert_parses(&output);
  }
}

#[test]
fn rules_with_noncontiguous_extensions_keep_their_own_dependencies() {
  let mut module = String::new();
  for n in 0..20 {
    module.push_str(&format!(
      "rule{n} = [int, next{n}]\nnext{n} = tstr\nrule{n} /= uint\n",
      n = n
    ));
  }
  let mut source = MemoryModuleSource::new();
  source.insert("m", &module);
  let output = resolve_modules(
    "root = rule19\n;# import m\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(output.contains("rule19 = [int, next19]"), "{}", output);
  assert!(output.contains("rule19 /= uint"), "{}", output);
  assert!(output.contains("next19 = tstr"), "{}", output);
  assert!(!output.contains("rule18"), "{}", output);
  assert_parses(&output);
}

#[test]
fn spaced_tag_heads_keep_namespaced_references() {
  let mut source = MemoryModuleSource::new();
  source.insert("m", "e5 = int\nvalue = [#7. 32e5, # 6 . ; note\n 32e5]\n");
  let output = resolve_modules(
    "root = ns.value\n;# import m as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(output.contains("[#7. 32 ns.e5, # 6 . ; note"), "{}", output);
  assert!(output.contains(" 32 ns.e5]"), "{}", output);
  assert!(output.contains("ns.e5 = int"), "{}", output);
  assert_parses(&output);
}

#[test]
fn a_synthetic_start_rule_rejects_existing_definitions() {
  let mut source = MemoryModuleSource::new();
  source.insert("m", "$.start.$ = int\n");
  let options = ResolveOptions {
    start_rule: Some("root".to_string()),
    ..ResolveOptions::default()
  };
  for input in [
    "$.start.$ = int\nroot = tstr\n",
    "root = tstr\n;# include m\n",
    "root = tstr\n;# import $.start.$ from m\n",
  ] {
    assert!(matches!(
      resolve_modules(input, &source, &options),
      Err(ModuleError::Directive { line: 0, .. })
    ));
    let output = resolve_modules(input, &source, &ResolveOptions::default()).unwrap();
    assert_parses(&output);
  }
}

#[test]
fn explicit_prelude_selections_keep_their_unqualified_names() {
  let mut source = MemoryModuleSource::new();
  source.insert("m", "int = uint / nint\n");
  for kind in ["include", "import"] {
    let input = format!("root = int\n;# {} int from m as ns\n", kind);
    let output = resolve_modules(&input, &source, &ResolveOptions::default()).unwrap();
    assert!(output.contains("int = uint / nint"), "{}", output);
    assert!(!output.contains("ns.int"), "{}", output);
    assert_parses(&output);
  }
}

#[test]
fn namespace_prefixes_cannot_merge_with_adjacent_numbers() {
  let mut source = MemoryModuleSource::new();
  for (namespace, number) in [("e3", "1"), ("a", "0xff"), ("e3", "0b11")] {
    source.insert("m", format!("item = int\nvalue = [{}item]\n", number));
    for kind in ["include", "import"] {
      let input = format!(
        "root = {}.value\n;# {} m as {}\n",
        namespace, kind, namespace
      );
      let output = resolve_modules(&input, &source, &ResolveOptions::default()).unwrap();
      assert!(
        output.contains(&format!("[{} {}.item]", number, namespace)),
        "{}",
        output
      );
      assert!(
        output.contains(&format!("{}.item = int", namespace)),
        "{}",
        output
      );
      assert_parses(&output);
    }
  }
}

#[test]
fn tag_heads_and_occurrence_bounds_keep_namespaced_references() {
  let mut source = MemoryModuleSource::new();
  source.insert(
    "m",
    "eq = int\ne3 = int\ne5 = int\nvalue = [#7.eq 32, 1*3e3, #7.32e5]\n",
  );

  let output = resolve_modules(
    "start = ns.value\n;# import m as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert!(
    output.contains("ns.value = [#7.eq 32, 1*3 ns.e3, #7.32 ns.e5]"),
    "{}",
    output
  );
  assert!(output.contains("ns.e3 = int"), "{}", output);
  assert!(output.contains("ns.e5 = int"), "{}", output);
  assert!(!output.contains("ns.eq"), "{}", output);
  assert_parses(&output);
}

#[test]
fn spaced_control_operators_keep_their_names_under_a_namespace() {
  let mut source = MemoryModuleSource::new();
  source.insert(
    "m",
    "size = int\nvalue = bstr . size 4\nother = bstr . ; comment\n  size 8\ntight = bstr. size 2\n",
  );

  let output = resolve_modules(
    "start = [ns.value, ns.other, ns.tight]\n;# import m as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert!(output.contains("ns.value = bstr . size 4"), "{}", output);
  assert!(output.contains("  size 8"), "{}", output);
  assert!(output.contains("ns.tight = bstr. size 2"), "{}", output);
  assert!(!output.contains("ns.size"), "{}", output);
  assert_parses(&output);
}

#[test]
fn shared_dependencies_are_loaded_once() {
  use cddl::modules::ModuleSource;
  use std::cell::RefCell;

  struct Counting {
    inner: MemoryModuleSource,
    loads: RefCell<Vec<String>>,
  }

  impl ModuleSource for Counting {
    fn load(&self, name: &str) -> Result<Option<String>, ModuleError> {
      self.loads.borrow_mut().push(name.to_string());
      self.inner.load(name)
    }
  }

  // Each module imports its predecessor twice; without caching the number
  // of loads doubles with every level.
  let mut inner = MemoryModuleSource::new();
  inner.insert("m0", "t0 = int\n");
  for k in 1..=12 {
    inner.insert(
      format!("m{}", k),
      format!(
        "t{k} = [t{p}]\n;# import m{p}\n;# import m{p}\n",
        k = k,
        p = k - 1
      ),
    );
  }
  let source = Counting {
    inner,
    loads: RefCell::new(Vec::new()),
  };

  let output = resolve_modules(
    "root = t12\n;# import m12\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert_eq!(source.loads.borrow().len(), 13);
  assert!(output.contains("t0 = int"), "{}", output);
  assert_parses(&output);
}

#[test]
fn namespaced_generic_parameters_do_not_capture_module_names() {
  let mut source = MemoryModuleSource::new();
  source.insert("m", "item = int\nwrapper<ns.item> = [item, ns.item]\n");

  let output = resolve_modules(
    "start = ns.wrapper<tstr>\n;# import m as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert!(
    output.contains("ns.wrapper<ns-item> = [ns.item, ns-item]"),
    "{}",
    output
  );
  assert!(output.contains("ns.item = int"), "{}", output);
  assert_parses(&output);
}

#[test]
fn an_extension_only_include_suppresses_a_later_base_definition() {
  let mut source = MemoryModuleSource::new();
  source.insert("one", "item /= 1\n");
  source.insert("two", "item = uint\n");

  // The extension-only include defines `item`, so the second module's base
  // definition must be suppressed; emitting both would be a duplicate the
  // parser rejects.
  let output = resolve_modules(
    ";# include one\n;# include two\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert!(output.contains("item /= 1"), "{}", output);
  assert!(!output.contains("item = uint"), "{}", output);
  assert_parses(&output);

  // A module carrying both parts still keeps its own base definition.
  let mut source = MemoryModuleSource::new();
  source.insert("ext", "item = uint\nitem /= 1\n");
  let output = resolve_modules(";# include ext\n", &source, &ResolveOptions::default()).unwrap();
  assert!(output.contains("item = uint"), "{}", output);
  assert!(output.contains("item /= 1"), "{}", output);
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
fn local_definitions_keep_included_extensions() {
  let mut source = MemoryModuleSource::new();
  source.insert("ext", "item = int\nitem /= tstr\n");

  let output = resolve_modules(
    "item = bool\n;# include item from ext\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  // The base definition is already supplied locally, but the extension must
  // still apply to it.
  assert!(!output.contains("item = int\n"));
  assert!(output.contains("item /= tstr"), "{}", output);
  assert_parses(&output);

  // Two modules extending one socket both contribute.
  let mut source = MemoryModuleSource::new();
  source.insert("one", "plug /= 1\n");
  source.insert("two", "plug /= 2\n");
  let output = resolve_modules(
    "plug = uint\n;# include one\n;# include two\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();
  assert!(output.contains("plug /= 1"), "{}", output);
  assert!(output.contains("plug /= 2"), "{}", output);
  assert_parses(&output);
}

#[test]
fn imports_retain_extensions_of_suppressed_bases_and_their_dependencies() {
  let mut source = MemoryModuleSource::new();
  source.insert("ext", "item = int\nitem /= extra\nextra = tstr\n");

  let output = resolve_modules(
    "item = bool\nstart = item\n;# import item from ext\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert!(!output.contains("item = int\n"));
  assert!(output.contains("item /= extra"), "{}", output);
  assert!(output.contains("extra = tstr"), "{}", output);
  assert_parses(&output);
}

#[test]
fn suppressed_rules_do_not_pull_their_module_dependencies() {
  let mut source = MemoryModuleSource::new();
  source.insert("m", "foo = a\na = helper\nhelper = tstr\n");

  // Local `a` satisfies the reference from `foo`, so the module's own `a` and
  // its dependency `helper` must stay out of the output.
  let output = resolve_modules(
    "a = int\nstart = foo\n;# import foo from m\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert_eq!(output, "a = int\nstart = foo\nfoo = a\n");
  assert_parses(&output);
}

#[test]
fn a_generic_import_alias_forwards_its_parameters() {
  let mut source = MemoryModuleSource::new();
  source.insert("generic", "messages<a, b> = [a, b]\n");

  let output = resolve_modules(
    "start = ns.messages<int, bool>\n;# import messages from generic as ns\n",
    &source,
    &ResolveOptions::default(),
  )
  .unwrap();

  assert!(
    output.contains("messages<a, b> = ns.messages<a, b>"),
    "{}",
    output
  );
  assert!(output.contains("ns.messages<a, b> = [a, b]"), "{}", output);
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
