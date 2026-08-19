//! Locating module sources.

#[cfg(not(feature = "std"))]
use alloc::{collections::BTreeMap, string::String};

#[cfg(feature = "std")]
use std::collections::BTreeMap;

use super::ModuleError;

/// A provider of module sources, keyed by the filename written in a directive.
///
/// This is the seam that keeps module resolution usable on targets without a
/// filesystem (notably `wasm32-unknown-unknown`), where the embedder supplies
/// module text directly.
pub trait ModuleSource {
  /// Return the CDDL text of the named module, or `None` if this source does
  /// not provide it.
  fn load(&self, name: &str) -> Result<Option<String>, ModuleError>;
}

/// An in-memory [`ModuleSource`], usable on every target.
#[derive(Debug, Clone, Default)]
pub struct MemoryModuleSource {
  modules: BTreeMap<String, String>,
}

impl MemoryModuleSource {
  /// Create an empty source.
  pub fn new() -> Self {
    Self::default()
  }

  /// Register the text of a module under `name`.
  pub fn insert(&mut self, name: impl Into<String>, text: impl Into<String>) -> &mut Self {
    self.modules.insert(name.into(), text.into());
    self
  }
}

impl ModuleSource for MemoryModuleSource {
  fn load(&self, name: &str) -> Result<Option<String>, ModuleError> {
    Ok(self.modules.get(name).cloned())
  }
}

/// The environment variable naming the module search path.
#[cfg(all(feature = "std", not(target_arch = "wasm32")))]
pub const INCLUDE_PATH_VAR: &str = "CDDL_INCLUDE_PATH";

/// The search path used when [`INCLUDE_PATH_VAR`] is unset: the current
/// directory, followed by the processor's own collection.
#[cfg(all(feature = "std", not(target_arch = "wasm32")))]
pub const DEFAULT_INCLUDE_PATH: &str = ".:";

/// A [`ModuleSource`] backed by a colon-separated search path of directories,
/// per §2.4 of the specification.
///
/// An empty path element denotes the processor's own bundled collection of
/// modules extracted from published RFCs. This implementation ships no such
/// collection, so empty elements are accepted and skipped rather than being
/// resolved against the root directory.
#[cfg(all(feature = "std", not(target_arch = "wasm32")))]
#[derive(Debug, Clone, Default)]
pub struct FsModuleSource {
  directories: Vec<std::path::PathBuf>,
}

#[cfg(all(feature = "std", not(target_arch = "wasm32")))]
impl FsModuleSource {
  /// Build a source from a colon-separated search path.
  pub fn from_include_path(path: &str) -> Self {
    FsModuleSource {
      directories: path
        .split(':')
        .filter(|element| !element.is_empty())
        .map(std::path::PathBuf::from)
        .collect(),
    }
  }

  /// Build a source from `CDDL_INCLUDE_PATH`, falling back to
  /// [`DEFAULT_INCLUDE_PATH`].
  pub fn from_env() -> Self {
    match std::env::var(INCLUDE_PATH_VAR) {
      Ok(path) if !path.is_empty() => Self::from_include_path(&path),
      _ => Self::from_include_path(DEFAULT_INCLUDE_PATH),
    }
  }

  /// The directories searched, in order.
  pub fn directories(&self) -> &[std::path::PathBuf] {
    &self.directories
  }
}

#[cfg(all(feature = "std", not(target_arch = "wasm32")))]
impl ModuleSource for FsModuleSource {
  fn load(&self, name: &str) -> Result<Option<String>, ModuleError> {
    for directory in &self.directories {
      // A module name matches the `filename` production, which admits no path
      // separator, so this can only ever name a direct child.
      for candidate in [directory.join(name), directory.join(format!("{}.cddl", name))] {
        if !candidate.is_file() {
          continue;
        }

        return std::fs::read_to_string(&candidate)
          .map(Some)
          .map_err(|e| ModuleError::ModuleUnreadable {
            name: name.to_string(),
            message: e.to_string(),
          });
      }
    }

    Ok(None)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn memory_source_round_trips() {
    let mut source = MemoryModuleSource::new();
    source.insert("rfc9052", "label = int / tstr\n");

    assert_eq!(
      source.load("rfc9052").unwrap().as_deref(),
      Some("label = int / tstr\n")
    );
    assert_eq!(source.load("missing").unwrap(), None);
  }

  #[cfg(all(feature = "std", not(target_arch = "wasm32")))]
  #[test]
  fn empty_include_path_elements_carry_no_bundled_collection() {
    // §2.4 gives an empty element the meaning "the processor's own collection".
    // This implementation ships no such collection, so the element resolves to
    // nothing rather than to the root directory.
    let source = FsModuleSource::from_include_path(".:");
    assert_eq!(source.directories().len(), 1);
    assert_eq!(source.load("rfc9052").unwrap(), None);
  }
}

