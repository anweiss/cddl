# cddl-codegen-rust

`cddl-codegen-rust` generates Rust source code from CDDL definitions. It is the
regular-library counterpart to the `cddl-derive` procedural macros and uses the
same generation engine and options.

Add `cddl-codegen-rust = "0.1"` to `[dependencies]` for an application, or to
`[build-dependencies]` for a build script.

```rust
use cddl_codegen_rust::{generate_rust_code, CodegenError};

fn generate(schema: &str) -> Result<String, CodegenError> {
  generate_rust_code(schema)
}
```

Customize output with `CodegenOptions`:

```rust
use cddl_codegen_rust::{generate_rust_code_with_options, CodegenOptions};

let mut options = CodegenOptions::default();
options.non_exhaustive = true;
options.other_variant = true;

let generated = generate_rust_code_with_options(
  r#"person = { name: tstr, ? email: tstr }"#,
  &options,
)?;
# Ok::<(), cddl_codegen_rust::CodegenError>(())
```

All five macro options are available on `CodegenOptions`: `any_type`,
`non_exhaustive`, `other_variant`, `fundamental_aliases`, and `substitutions`.
Create it with `Default::default()` and set the fields you need.
`generate_rust_code_for_rule(source, rule_name, output_name, options)` generates
one rule using its CDDL name (for example, `"person-record"`), with an optional
Rust output name. Referenced user-defined types must be generated separately.

## Transforming or combining schemas

Use the re-exported `cddl_codegen_rust::cddl` parser and AST, then pass the AST to
`generate_rust_code_from_ast`. This ensures the AST matches the engine's
dependency version without adding a separate `cddl` dependency. No procedural
macros or `CARGO_MANIFEST_DIR` are needed by the library.

For example, a tool can combine compatible, non-overlapping fields from two
versions of a map rule before generating Rust:

```rust
use cddl_codegen_rust::cddl::ast::{Rule, Type2};
use cddl_codegen_rust::cddl::pest_bridge::cddl_from_pest_str;
use cddl_codegen_rust::{generate_rust_code_from_ast, CodegenOptions};

let mut first = cddl_from_pest_str(
  "person = {\n; Display name\nname: tstr\n}",
)?;
let second = cddl_from_pest_str(
  "person = {\n; Contact address\n? email: tstr\n}",
)?;

if let (Rule::Type { rule: first_rule, .. }, Rule::Type { rule: second_rule, .. }) =
  (&mut first.rules[0], &second.rules[0])
{
  if let (Type2::Map { group: first_map, .. }, Type2::Map { group: second_map, .. }) =
    (&mut first_rule.value.type_choices[0].type1.type2,
     &second_rule.value.type_choices[0].type1.type2)
  {
    first_map.group_choices[0].group_entries.extend(
      second_map.group_choices[0].group_entries.iter().cloned(),
    );
  }
}

// The nodes now come from different sources: use AST comments only.
let rust = generate_rust_code_from_ast(&first, "", &CodegenOptions::default())?;
assert_eq!(rust.matches("pub struct Person").count(), 1);
assert!(rust.contains("pub name: String,"));
assert!(rust.contains("pub email: Option<String>,"));
assert!(rust.contains("/// Display name"));
assert!(rust.contains("/// Contact address"));
# Ok::<(), Box<dyn std::error::Error>>(())
```

This is an AST transformation example, **not an automatic schema-version
merger**. The caller must decide how to resolve duplicate fields, differing
types, optionality, rule references, and conflicting definitions. Retain the
input strings for as long as the borrowed ASTs are used.

For an unchanged single-document AST, supply the original source to preserve
fallback documentation based on line spans. For an AST assembled from multiple
documents, supply `""` to retain AST-attached comments without incorrectly
borrowing documentation from another document's line numbers.

## Build scripts and generated dependencies

A build script can read a schema, call `generate_rust_code`, and write the
result to `OUT_DIR` for `include!`. It is responsible for file I/O and emitting
`cargo:rerun-if-changed` for each input. See the repository's standalone
`cddl-derive/tests/consumer` fixture for a compiled example.

The crate compiling the **generated Rust** needs `serde` with its `derive`
feature in `[dependencies]`. Depending on the schema, it also needs
`serde_with` with `macros` for byte strings and container encoding adapters,
`ciborium` for tagged prelude types, and `serde_json` for the default `any`
representation. These must not be only build dependencies. Custom type paths
must resolve in the generated code's module.

The library preserves the macros' existing mappings and limitations; it does
not enforce all CDDL constraints or validate that custom Rust type strings
compile. Use the `cddl` validators for data validation and compile the generated
code in its consumer.
