//! CDDL module structure as specified in
//! [draft-ietf-cbor-cddl-modules-06](https://datatracker.ietf.org/doc/draft-ietf-cbor-cddl-modules/06/).
//!
//! The module structure is a *super-syntax* carried in comments: lines that
//! begin with `;#` are parsed as directives by a module-aware processor while
//! remaining ordinary comments to a basic (RFC 8610) CDDL parser. Processing
//! the directives of a source file turns it into a *module*.
//!
//! Resolution is a source-to-source transformation: a module-structured CDDL
//! document is resolved into an equivalent basic CDDL document, which can then
//! be handed to [`crate::parser::cddl_from_str`] like any other input.
//!
//! ```no_run
//! # #[cfg(all(feature = "modules", feature = "std", not(target_arch = "wasm32")))]
//! # fn main() -> Result<(), cddl::modules::ModuleError> {
//! use cddl::modules::{resolve_modules, ResolveOptions};
//!
//! let input = "start = COSE_Key\n;# import rfc9052 as cose\n";
//! let source = cddl::modules::FsModuleSource::from_env();
//! let basic_cddl = resolve_modules(input, &source, &ResolveOptions::default())?;
//! let _ast = cddl::parser::cddl_from_str(&basic_cddl, false);
//! # Ok(())
//! # }
//! # #[cfg(not(all(feature = "modules", feature = "std", not(target_arch = "wasm32"))))]
//! # fn main() {}
//! ```

mod directive;
mod resolver;
mod scan;
mod source;

#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};

use core::fmt;

pub use directive::{parse_directives, Directive, DirectiveKind, NameSelector};
pub use resolver::{resolve_modules, ResolveOptions};
pub use source::{MemoryModuleSource, ModuleSource};

#[cfg(all(feature = "std", not(target_arch = "wasm32")))]
pub use source::{FsModuleSource, DEFAULT_INCLUDE_PATH, INCLUDE_PATH_VAR};

/// Errors arising from parsing or resolving CDDL module directives.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleError {
  /// A `;#` directive could not be parsed against the grammar in Appendix A
  /// of the specification. Carries the 1-based line number and a description.
  Directive {
    /// 1-based line number of the offending directive.
    line: usize,
    /// Human-readable description of the syntax error.
    message: String,
  },
  /// A referenced module could not be located in any configured source.
  ModuleNotFound {
    /// The module name as written in the directive.
    name: String,
    /// 1-based line number of the directive that referenced it.
    line: usize,
  },
  /// A module was located but could not be read.
  ModuleUnreadable {
    /// The module name as written in the directive.
    name: String,
    /// Underlying reason.
    message: String,
  },
  /// A module could not be parsed as basic CDDL.
  ModuleParse {
    /// The module name as written in the directive.
    name: String,
    /// Parser diagnostics.
    message: String,
  },
  /// A rule named in a `from` clause does not exist in the referenced module.
  RuleNotFound {
    /// The rule name as written in the directive.
    rule: String,
    /// The module the rule was expected in.
    module: String,
    /// 1-based line number of the directive.
    line: usize,
  },
  /// A cycle was detected while following module references.
  CircularReference {
    /// The chain of module names, in the order they were entered.
    chain: Vec<String>,
  },
}

impl fmt::Display for ModuleError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ModuleError::Directive { line, message } => {
        write!(f, "invalid directive on line {}: {}", line, message)
      }
      ModuleError::ModuleNotFound { name, line } => write!(
        f,
        "module \"{}\" referenced on line {} was not found in any module source",
        name, line
      ),
      ModuleError::ModuleUnreadable { name, message } => {
        write!(f, "module \"{}\" could not be read: {}", name, message)
      }
      ModuleError::ModuleParse { name, message } => {
        write!(f, "module \"{}\" is not valid CDDL: {}", name, message)
      }
      ModuleError::RuleNotFound {
        rule,
        module,
        line,
      } => write!(
        f,
        "rule \"{}\" named on line {} is not defined in module \"{}\"",
        rule, line, module
      ),
      ModuleError::CircularReference { chain } => {
        write!(f, "circular module reference: {}", chain.join(" -> "))
      }
    }
  }
}

#[cfg(feature = "std")]
impl std::error::Error for ModuleError {}
