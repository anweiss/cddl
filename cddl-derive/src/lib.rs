//! Proc macros for deriving Rust types from CDDL definitions.
//!
//! This crate provides two macros:
//!
//! - [`cddl`] — an attribute macro applied to a struct to populate its fields
//!   from a matching CDDL rule.
//! - [`cddl_typegen`] — a function-like macro that generates all Rust types
//!   from a CDDL file.
//!
//! # Examples
//!
//! ## Attribute macro on a single struct
//!
//! ```rust,ignore
//! use cddl_derive::cddl;
//!
//! // schema.cddl contains: person = { name: tstr, age: uint }
//! #[cddl(path = "schema.cddl")]
//! struct Person;
//! ```
//!
//! The struct name is converted from PascalCase to CDDL kebab-case for lookup.
//! You can override the rule name with the `rule` attribute:
//!
//! ```rust,ignore
//! #[cddl(path = "schema.cddl", rule = "my-person")]
//! struct Person;
//! ```
//!
//! ## Generate all types from a CDDL file
//!
//! ```rust,ignore
//! use cddl_derive::cddl_typegen;
//!
//! cddl_typegen!("schema.cddl");
//! ```
//!
//! # CDDL comments
//!
//! CDDL comments (`; ...`) are preserved as Rust doc comments on the generated
//! types and fields. A comment (or several consecutive comment lines) directly
//! above a rule or field becomes its leading documentation, and a comment
//! trailing a definition on the same line is appended after it.
//!
//! # Configuration
//!
//! Both macros accept the same optional settings. Defaults match the behaviour
//! that existed before they were introduced, so adding none of them changes
//! nothing. See <https://github.com/anweiss/cddl/issues/641>.
//!
//! | Option | Default | Effect |
//! |---|---|---|
//! | `any_type = "<path>"` | `serde_json::Value` | Rust type generated for CDDL `any`. Set to `ciborium::Value` for a CBOR-first schema. |
//! | `non_exhaustive = true` | `false` | Emit `#[non_exhaustive]` on generated structs and enums, so adding a field or variant later is not a breaking change downstream. |
//! | `other_variant = true` | `false` | Append an `Other(String)` catch-all to generated enums, so a value added to the schema later still deserializes. |
//! | `substitute("k" = "<path>")` | none | Replace generated types with hand-written ones. |
//!
//! A `substitute` key is either a CDDL rule name, which replaces every
//! reference to that rule and suppresses its definition, or a rule-qualified
//! field name, which replaces just that field. Field-level substitution is how
//! you mix representations within one schema — for example, marking one `any`
//! as CBOR while the rest stay JSON:
//!
//! ```rust,ignore
//! cddl_typegen!(
//!     "schema.cddl",
//!     non_exhaustive = true,
//!     other_variant = true,
//!     substitute(
//!         "label" = "crate::Label",
//!         "claim.payload" = "ciborium::Value",
//!     )
//! );
//! ```
//!
//! The same options work on the attribute macro:
//!
//! ```rust,ignore
//! #[cddl(path = "schema.cddl", non_exhaustive = true)]
//! struct Person;
//! ```
//!
//! Substitutions match whole identifiers and apply inside containers, so
//! substituting `label` rewrites `Vec<Label>` but leaves `LabelSet` alone. An
//! explicit substitution replaces the type outright, which also drops any CBOR
//! tag that had been inferred from the original prelude type.
//!
//! # Enums of string literals
//!
//! A rule whose choices are all string literals (`kind = "a" / "b"`) is a
//! closed set of strings on the wire, and is generated with hand-written serde
//! impls that read and write a bare string, plus an `as_str` accessor. serde's
//! untagged representation cannot express this — it deserializes a unit variant
//! from `null` only — so these enums would otherwise fail to round-trip.
//! Choices that mix string literals with other types still use the untagged
//! representation.
//!
//! # Required dependencies of generated code
//!
//! Generated types always depend on `serde`. Some CDDL constructs pull in
//! additional crates, and only when the schema actually uses them:
//!
//! | CDDL construct | Generated as | Extra dependency |
//! |---|---|---|
//! | `bstr` / `bytes` | `Vec<u8>` annotated with `#[serde_as(as = "serde_with::Bytes")]` | `serde_with` (feature `macros`) |
//! | `tdate`, `time`, `uri`, `b64url`, `b64legacy`, `regexp` | the inner type plus a CBOR tag serde helper | `ciborium` |
//! | `any` | `serde_json::Value` | `serde_json` |
//!
//! `bstr` needs `serde_with` because serde encodes a bare `Vec<u8>` as an array
//! of integers rather than as a CBOR byte string (major type 2). See
//! <https://github.com/anweiss/cddl/issues/638>.
//!
//! The tagged prelude types are CBOR tags wrapping a simpler value (RFC 8610
//! Appendix D). The generated struct keeps the inner Rust type and applies the
//! tag through a serde helper. The helper is format-aware: CBOR gets the tag,
//! while human-readable formats such as JSON keep the bare value, so the same
//! type round-trips through both. Deserialization accepts tagged and untagged
//! input alike. See <https://github.com/anweiss/cddl/issues/639>.

extern crate proc_macro;

mod codegen;

use codegen::CodegenOptions;
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::{parenthesized, parse_macro_input, Ident, LitBool, LitStr, Token};

/// Parse the option keys shared by `#[cddl(...)]` and `cddl_typegen!(...)`.
///
/// Returns `false` if the key is not a recognised option, leaving the caller to
/// handle it or report it as unknown.
fn parse_codegen_option(
  key: &Ident,
  input: ParseStream<'_>,
  opts: &mut CodegenOptions,
) -> syn::Result<bool> {
  match key.to_string().as_str() {
    "any_type" => {
      input.parse::<Token![=]>()?;
      opts.any_type = Some(input.parse::<LitStr>()?.value());
    }
    "non_exhaustive" => {
      input.parse::<Token![=]>()?;
      opts.non_exhaustive = input.parse::<LitBool>()?.value();
    }
    "other_variant" => {
      input.parse::<Token![=]>()?;
      opts.other_variant = input.parse::<LitBool>()?.value();
    }
    "substitute" => {
      // substitute("rule" = "path::To::Type", "rule.field" = "OtherType")
      let content;
      parenthesized!(content in input);
      while !content.is_empty() {
        let target: LitStr = content.parse()?;
        content.parse::<Token![=]>()?;
        let replacement: LitStr = content.parse()?;
        opts
          .substitutions
          .insert(target.value(), replacement.value());
        if !content.is_empty() {
          content.parse::<Token![,]>()?;
        }
      }
    }
    _ => return Ok(false),
  }

  Ok(true)
}

/// Parsed arguments for the `#[cddl(...)]` attribute.
struct CddlAttrArgs {
  path: String,
  rule: Option<String>,
  opts: CodegenOptions,
}

impl Parse for CddlAttrArgs {
  fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
    let mut path = None;
    let mut rule = None;
    let mut opts = CodegenOptions::default();

    while !input.is_empty() {
      let key: Ident = input.parse()?;

      match key.to_string().as_str() {
        "path" => {
          input.parse::<Token![=]>()?;
          path = Some(input.parse::<LitStr>()?.value());
        }
        "rule" => {
          input.parse::<Token![=]>()?;
          rule = Some(input.parse::<LitStr>()?.value());
        }
        other => {
          if !parse_codegen_option(&key, input, &mut opts)? {
            return Err(syn::Error::new(
              key.span(),
              format!("unknown attribute `{other}`"),
            ));
          }
        }
      }

      if !input.is_empty() {
        input.parse::<Token![,]>()?;
      }
    }

    let path = path.ok_or_else(|| input.error("missing required `path` attribute"))?;

    Ok(CddlAttrArgs { path, rule, opts })
  }
}

/// Parsed arguments for the `cddl_typegen!` macro.
struct TypegenArgs {
  path: LitStr,
  opts: CodegenOptions,
}

impl Parse for TypegenArgs {
  fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
    let path: LitStr = input.parse()?;
    let mut opts = CodegenOptions::default();

    while !input.is_empty() {
      input.parse::<Token![,]>()?;
      // A trailing comma is allowed.
      if input.is_empty() {
        break;
      }

      let key: Ident = input.parse()?;
      if !parse_codegen_option(&key, input, &mut opts)? {
        let name = key.to_string();
        return Err(syn::Error::new(
          key.span(),
          format!("unknown option `{name}`"),
        ));
      }
    }

    Ok(TypegenArgs { path, opts })
  }
}

/// Attribute macro that populates a struct with fields derived from a CDDL rule.
///
/// The macro reads the CDDL file at compile time, finds the rule matching the
/// struct name (converted from PascalCase to kebab-case), and replaces the
/// struct body with the generated fields. Derive macros for `Clone`, `Debug`,
/// `serde::Serialize`, and `serde::Deserialize` are added automatically.
///
/// CDDL comments on the matched rule and its fields are preserved as Rust doc
/// comments on the generated struct and fields.
///
/// # Attributes
///
/// - `path` (required) — path to the CDDL file, relative to the crate root.
/// - `rule` (optional) — explicit CDDL rule name to use instead of deriving it
///   from the struct name.
/// - `any_type`, `non_exhaustive`, `other_variant`, `substitute` (optional) —
///   see the crate-level documentation.
///
/// # Example
///
/// ```rust,ignore
/// use cddl_derive::cddl;
///
/// #[cddl(path = "schema.cddl")]
/// struct Person;
/// ```
#[proc_macro_attribute]
pub fn cddl(attr: TokenStream, item: TokenStream) -> TokenStream {
  let args = parse_macro_input!(attr as CddlAttrArgs);
  let input: syn::ItemStruct = match syn::parse(item) {
    Ok(s) => s,
    Err(e) => return e.to_compile_error().into(),
  };

  let struct_name = input.ident.to_string();

  // Determine which CDDL rule to look up
  let rule_name = args
    .rule
    .unwrap_or_else(|| codegen::pascal_to_cddl_name(&struct_name));
  let pascal_rule = codegen::to_pascal_case(&rule_name);

  // Resolve the CDDL file path relative to CARGO_MANIFEST_DIR
  let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
  let cddl_path = std::path::Path::new(&manifest_dir).join(&args.path);
  let cddl_content = match std::fs::read_to_string(&cddl_path) {
    Ok(s) => s,
    Err(e) => {
      let msg = format!("failed to read CDDL file `{}`: {}", cddl_path.display(), e);
      return syn::Error::new(proc_macro2::Span::call_site(), msg)
        .to_compile_error()
        .into();
    }
  };

  // Parse the CDDL
  let cddl_ast = match cddl::parser::cddl_from_str(&cddl_content, true) {
    Ok(ast) => ast,
    Err(e) => {
      let msg = format!("failed to parse CDDL: {}", e);
      return syn::Error::new(proc_macro2::Span::call_site(), msg)
        .to_compile_error()
        .into();
    }
  };

  // Generate code for the single rule, using the user's struct name
  let generated = match codegen::generate_single_type(
    &cddl_ast,
    &pascal_rule,
    Some(&struct_name),
    &cddl_content,
    &args.opts,
  ) {
    Ok(code) => code,
    Err(e) => {
      let msg = format!("codegen error: {}", e);
      return syn::Error::new(proc_macro2::Span::call_site(), msg)
        .to_compile_error()
        .into();
    }
  };

  // Parse the generated code as a token stream
  let tokens: proc_macro2::TokenStream = match generated.parse() {
    Ok(ts) => ts,
    Err(e) => {
      let msg = format!("failed to parse generated code: {}", e);
      return syn::Error::new(proc_macro2::Span::call_site(), msg)
        .to_compile_error()
        .into();
    }
  };

  // Emit an include_str! so Cargo tracks the CDDL file for changes
  let path_str = args.path.clone();
  let tracking: proc_macro2::TokenStream = format!(
    "const _: &str = include_str!(concat!(env!(\"CARGO_MANIFEST_DIR\"), \"/{}\"));",
    path_str
  )
  .parse()
  .expect("failed to generate tracking include");

  let combined = quote::quote! {
    #tracking
    #tokens
  };

  combined.into()
}

/// Generate all Rust types from a CDDL file.
///
/// This macro reads the specified CDDL file at compile time and generates
/// corresponding Rust structs, enums, and type aliases for every rule. CDDL
/// comments are preserved as Rust doc comments on the generated types and
/// fields.
///
/// # Example
///
/// ```rust,ignore
/// use cddl_derive::cddl_typegen;
///
/// cddl_typegen!("schema.cddl");
/// // Generates: pub struct Person { ... }, pub struct Address { ... }, etc.
/// ```
#[proc_macro]
pub fn cddl_typegen(input: TokenStream) -> TokenStream {
  let args = parse_macro_input!(input as TypegenArgs);
  let path_lit = args.path;
  let path_str = path_lit.value();

  // Resolve relative to CARGO_MANIFEST_DIR
  let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
  let cddl_path = std::path::Path::new(&manifest_dir).join(&path_str);
  let cddl_content = match std::fs::read_to_string(&cddl_path) {
    Ok(s) => s,
    Err(e) => {
      let msg = format!("failed to read CDDL file `{}`: {}", cddl_path.display(), e);
      return syn::Error::new(path_lit.span(), msg)
        .to_compile_error()
        .into();
    }
  };

  // Parse the CDDL
  let cddl_ast = match cddl::parser::cddl_from_str(&cddl_content, true) {
    Ok(ast) => ast,
    Err(e) => {
      let msg = format!("failed to parse CDDL: {}", e);
      return syn::Error::new(path_lit.span(), msg)
        .to_compile_error()
        .into();
    }
  };

  // Generate all types
  let generated = match codegen::generate_all_types(&cddl_ast, &cddl_content, &args.opts) {
    Ok(code) => code,
    Err(e) => {
      let msg = format!("codegen error: {}", e);
      return syn::Error::new(path_lit.span(), msg)
        .to_compile_error()
        .into();
    }
  };

  // Parse the generated code as a token stream
  let tokens: proc_macro2::TokenStream = match generated.parse() {
    Ok(ts) => ts,
    Err(e) => {
      let msg = format!("failed to parse generated code: {}", e);
      return syn::Error::new(path_lit.span(), msg)
        .to_compile_error()
        .into();
    }
  };

  // Emit an include_str! so Cargo tracks the CDDL file for changes
  let tracking: proc_macro2::TokenStream = format!(
    "const _: &str = include_str!(concat!(env!(\"CARGO_MANIFEST_DIR\"), \"/{}\"));",
    path_str
  )
  .parse()
  .expect("failed to generate tracking include");

  let combined = quote::quote! {
    #tracking
    #tokens
  };

  combined.into()
}
