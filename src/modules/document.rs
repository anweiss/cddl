//! Owning resolved documents and the browser-facing resolution API.

#[cfg(not(feature = "std"))]
use alloc::string::String;

use super::{resolve_modules, ModuleError, ModuleSource, ResolveOptions};

/// An owned basic CDDL document produced by module resolution.
///
/// The parsed AST borrows this document, not the original input or module
/// source. Keep the document alive while using the AST. Resolution and parsing
/// are separate operations: [`Self::resolve`] reports module errors, while
/// [`Self::parse`] also checks the root document's basic CDDL syntax.
///
/// ```
/// # #[cfg(feature = "modules")]
/// # fn main() {
/// use cddl::modules::{MemoryModuleSource, ResolvedCddl, ResolveOptions};
///
/// let mut source = MemoryModuleSource::new();
/// source.insert("numbers", "value = uint\n");
/// let document = ResolvedCddl::resolve(
///     "start = n.value\n;# import numbers as n\n",
///     &source,
///     &ResolveOptions::default(),
/// ).unwrap();
/// let ast = document.parse().unwrap();
/// assert_eq!(ast.rules.len(), 2);
/// # }
/// # #[cfg(not(feature = "modules"))]
/// # fn main() {}
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedCddl {
  text: String,
}

impl ResolvedCddl {
  /// Resolve module directives into an owned basic CDDL document.
  pub fn resolve(
    input: &str,
    source: &dyn ModuleSource,
    options: &ResolveOptions,
  ) -> Result<Self, ModuleError> {
    resolve_modules(input, source, options).map(|text| Self { text })
  }

  /// The resolved basic CDDL text, with module directives removed.
  pub fn text(&self) -> &str {
    &self.text
  }

  /// Parse an AST borrowing the owned text, on native and WASM targets alike.
  ///
  /// This checks basic CDDL syntax, not whether all rule references are defined.
  pub fn parse(&self) -> crate::parser::Result<crate::ast::CDDL<'_>> {
    crate::pest_bridge::cddl_from_pest_str(&self.text)
  }

  /// Validate JSON against the resolved document using the existing validator.
  ///
  /// Parsing errors are returned without writing diagnostics to stderr.
  #[cfg(all(feature = "json", not(feature = "lsp"), not(target_arch = "wasm32")))]
  pub fn validate_json(
    &self,
    json: &str,
    #[cfg(feature = "additional-controls")] enabled_features: Option<&[&str]>,
  ) -> crate::validator::json::Result {
    use crate::validator::{json, Validator};

    let cddl = self
      .parse()
      .map_err(|error| json::Error::CDDLParsing(error.to_string()))?;
    let json = serde_json::from_str(json).map_err(json::Error::JSONParsing)?;
    json::JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      enabled_features,
    )
    .validate()
  }

  /// Validate CBOR against the resolved document using the existing validator.
  ///
  /// Parsing errors are returned without writing diagnostics to stderr.
  /// Invalid schemas produce `CDDLParsing`; malformed CBOR produces `CBORParsing`.
  #[cfg(all(feature = "cbor", not(feature = "lsp"), not(target_arch = "wasm32")))]
  pub fn validate_cbor(
    &self,
    cbor: &[u8],
    #[cfg(feature = "additional-controls")] enabled_features: Option<&[&str]>,
  ) -> crate::validator::cbor::Result<std::io::Error> {
    use crate::validator::{
      cbor,
      cbor_value::{decode_cbor, DecodeError},
      Validator,
    };

    let cddl = self
      .parse()
      .map_err(|error| cbor::Error::CDDLParsing(error.to_string()))?;
    let cbor = decode_cbor(cbor).map_err(|error| {
      cbor::Error::CBORParsing(match error {
        DecodeError::Io(error) => ciborium::de::Error::Io(error),
        DecodeError::Syntax(offset) => ciborium::de::Error::Syntax(offset),
        DecodeError::UnexpectedEof => ciborium::de::Error::Io(std::io::Error::new(
          std::io::ErrorKind::UnexpectedEof,
          error,
        )),
        error => ciborium::de::Error::Semantic(None, error.to_string()),
      })
    })?;
    cbor::CBORValidator::new(
      &cddl,
      cbor,
      #[cfg(feature = "additional-controls")]
      enabled_features,
    )
    .validate()
  }
}

/// Resolve modules supplied as a JavaScript object mapping module names to CDDL
/// text. Returns basic CDDL suitable for the parser and validation WASM APIs.
///
/// No filesystem or network access is performed. `start_rule` has the same
/// meaning as [`ResolveOptions::start_rule`].
#[cfg(all(target_arch = "wasm32", feature = "std"))]
#[wasm_bindgen::prelude::wasm_bindgen]
pub fn resolve_cddl_modules(
  input: &str,
  modules: wasm_bindgen::JsValue,
  start_rule: Option<String>,
) -> Result<String, wasm_bindgen::JsValue> {
  let modules: std::collections::BTreeMap<String, String> = serde_wasm_bindgen::from_value(modules)
    .map_err(|_| wasm_bindgen::JsValue::from_str("modules must map names to CDDL strings"))?;
  let mut source = super::MemoryModuleSource::new();
  for (name, text) in modules {
    source.insert(name, text);
  }
  let options = ResolveOptions {
    start_rule,
    ..ResolveOptions::default()
  };
  ResolvedCddl::resolve(input, &source, &options)
    .map(|document| document.text)
    .map_err(|error| wasm_bindgen::JsValue::from_str(&error.to_string()))
}
