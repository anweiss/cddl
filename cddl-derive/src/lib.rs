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

extern crate proc_macro;

mod codegen;

use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Ident, LitStr, Token};

/// Parsed arguments for the `#[cddl(...)]` attribute.
struct CddlAttrArgs {
  path: String,
  rule: Option<String>,
}

impl Parse for CddlAttrArgs {
  fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
    let mut path = None;
    let mut rule = None;

    while !input.is_empty() {
      let key: Ident = input.parse()?;
      input.parse::<Token![=]>()?;

      match key.to_string().as_str() {
        "path" => {
          let value: LitStr = input.parse()?;
          path = Some(value.value());
        }
        "rule" => {
          let value: LitStr = input.parse()?;
          rule = Some(value.value());
        }
        other => {
          return Err(syn::Error::new(
            key.span(),
            format!("unknown attribute `{other}`"),
          ));
        }
      }

      if !input.is_empty() {
        input.parse::<Token![,]>()?;
      }
    }

    let path = path.ok_or_else(|| input.error("missing required `path` attribute"))?;

    Ok(CddlAttrArgs { path, rule })
  }
}

/// Attribute macro that populates a struct with fields derived from a CDDL rule.
///
/// The macro reads the CDDL file at compile time, finds the rule matching the
/// struct name (converted from PascalCase to kebab-case), and replaces the
/// struct body with the generated fields. Derive macros for `Clone`, `Debug`,
/// `serde::Serialize`, and `serde::Deserialize` are added automatically.
///
/// # Attributes
///
/// - `path` (required) — path to the CDDL file, relative to the crate root.
/// - `rule` (optional) — explicit CDDL rule name to use instead of deriving it
///   from the struct name.
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
  let generated = match codegen::generate_single_type(&cddl_ast, &pascal_rule, Some(&struct_name)) {
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
  let path_str = args.path;
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
/// corresponding Rust structs, enums, and type aliases for every rule.
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
  let path_lit = parse_macro_input!(input as LitStr);
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
  let generated = match codegen::generate_all_types(&cddl_ast) {
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
