//! Resolution of module-structured CDDL into basic (RFC 8610) CDDL.
//!
//! Resolution is a source-to-source transformation, matching the behavior of
//! the `cddlc -2tcddl` tool described in Appendix B of the specification: the
//! directives of the input are processed, the rules they name are pulled in
//! from the referenced modules (prefixed into a namespace where an `as` clause
//! asks for one), and the result is a document containing no directives that
//! any RFC 8610 parser can read.

#[cfg(not(feature = "std"))]
use alloc::{
  collections::{BTreeMap, BTreeSet},
  format,
  string::{String, ToString},
  vec::Vec,
};

#[cfg(feature = "std")]
use std::collections::{BTreeMap, BTreeSet};

use super::{
  directive::{parse_directives, Directive, DirectiveKind, NameSelector},
  scan::{scan, IdentRole},
  ModuleError, ModuleSource,
};

/// The names of the standard prelude (Appendix D of RFC 8610). Prelude names
/// are never namespaced when a module is imported under an `as` clause, and
/// never count as references into a module.
const PRELUDE: &[&str] = &[
  "any",
  "uint",
  "nint",
  "int",
  "bstr",
  "bytes",
  "tstr",
  "text",
  "tdate",
  "time",
  "number",
  "biguint",
  "bignint",
  "bigint",
  "integer",
  "unsigned",
  "decfrac",
  "bigfloat",
  "eb64url",
  "eb64legacy",
  "eb16",
  "encoded-cbor",
  "uri",
  "b64url",
  "b64legacy",
  "regexp",
  "mime-message",
  "cbor-any",
  "float16",
  "float32",
  "float64",
  "float16-32",
  "float32-64",
  "float",
  "false",
  "true",
  "bool",
  "nil",
  "null",
  "undefined",
];

/// Options controlling module resolution.
#[derive(Debug, Clone, Default)]
pub struct ResolveOptions {
  /// A start (root) rule, emitted as `$.start.$ = <rule>` per §2.7 of the
  /// specification. Corresponds to the tool's `-s` flag.
  pub start_rule: Option<String>,
  /// Directives synthesized from the command line, applied before any
  /// directives found in the input, as `(namespace, module)` pairs. Corresponds
  /// to the tool's `-i` flag, where `-icose=rfc9052` is a shortcut for
  /// `;# import rfc9052 as cose`.
  pub command_line_imports: Vec<(String, String)>,
}

/// A rule definition recovered from a module, with its original text intact.
#[derive(Debug, Clone)]
struct ModuleRule {
  name: String,
  text: String,
  deps: Vec<String>,
}

/// Resolve the module directives in `input` against `source`, returning an
/// equivalent basic CDDL document.
///
/// # Errors
///
/// Returns a [`ModuleError`] if a directive is malformed, a referenced module
/// cannot be found or read, a rule named in a `from` clause does not exist, or
/// the modules reference each other cyclically.
pub fn resolve_modules(
  input: &str,
  source: &dyn ModuleSource,
  options: &ResolveOptions,
) -> Result<String, ModuleError> {
  let mut chain = Vec::new();
  resolve(input, source, options, &mut chain)
}

fn resolve(
  input: &str,
  source: &dyn ModuleSource,
  options: &ResolveOptions,
  chain: &mut Vec<String>,
) -> Result<String, ModuleError> {
  let mut directives: Vec<Directive> = options
    .command_line_imports
    .iter()
    .map(|(namespace, module)| Directive {
      kind: DirectiveKind::Import,
      names: None,
      filename: module.clone(),
      alias: Some(namespace.clone()),
      line: 0,
    })
    .collect();

  directives.extend(parse_directives(input)?);

  let body = strip_directives(input);
  let local = rules_of(&body);

  let mut emitted: BTreeSet<String> = local.iter().map(|rule| rule.name.clone()).collect();
  let mut references = referenced_names(&body);

  if let Some(start) = &options.start_rule {
    references.insert(start.clone());
  }

  let mut pulled: Vec<String> = Vec::new();

  for directive in &directives {
    let text = load_module(&directive.filename, directive.line, source, chain)?;

    let defined: BTreeSet<String> = scan(&text)
      .rules
      .into_iter()
      .map(|rule| rule.name)
      .collect();

    let text = match &directive.alias {
      Some(alias) => apply_namespace(&text, &defined, alias),
      None => text,
    };

    let rules = rules_of(&text);
    let index: BTreeMap<&str, &ModuleRule> =
      rules.iter().map(|rule| (rule.name.as_str(), rule)).collect();

    let mut aliases: Vec<String> = Vec::new();

    let selected = match (&directive.kind, &directive.names) {
      // `include <module>`: every rule of the module, verbatim.
      (DirectiveKind::Include, None) => rules.iter().map(|rule| rule.name.clone()).collect(),

      // `include <names> from <module>`: exactly the rules named.
      (DirectiveKind::Include, Some(selectors)) => {
        let mut names = Vec::new();

        for selector in selectors {
          match selector {
            NameSelector::All => {
              for rule in &rules {
                push_unique(&mut names, rule.name.clone());
              }
            }
            NameSelector::Name(written) => {
              let resolved = qualify(written, directive.alias.as_deref());

              if !index.contains_key(resolved.as_str()) {
                return Err(ModuleError::RuleNotFound {
                  rule: written.clone(),
                  module: directive.filename.clone(),
                  line: directive.line,
                });
              }

              push_unique(&mut names, resolved);
            }
          }
        }

        names
      }

      // `import <module>`: the rules this module references, transitively.
      (DirectiveKind::Import, None) => {
        let seeds: Vec<String> = rules
          .iter()
          .map(|rule| rule.name.clone())
          .filter(|name| references.contains(name))
          .collect();

        closure(&seeds, &index)
      }

      // `import <names> from <module>`: the rules named, transitively.
      (DirectiveKind::Import, Some(selectors)) => {
        let mut seeds = Vec::new();

        for selector in selectors {
          match selector {
            NameSelector::All => {
              for rule in &rules {
                push_unique(&mut seeds, rule.name.clone());
              }
            }
            NameSelector::Name(written) => {
              let resolved = qualify(written, directive.alias.as_deref());

              if !index.contains_key(resolved.as_str()) {
                return Err(ModuleError::RuleNotFound {
                  rule: written.clone(),
                  module: directive.filename.clone(),
                  line: directive.line,
                });
              }

              // §2.6: importing a name that was written without the namespace
              // prefix also defines an unprefixed alias for it.
              if resolved != *written && !emitted.contains(written) {
                aliases.push(format!("{} = {}", written, resolved));
              }

              push_unique(&mut seeds, resolved);
            }
          }
        }

        closure(&seeds, &index)
      }
    };

    for alias in aliases {
      pulled.push(alias);
    }

    for name in selected {
      if !emitted.insert(name.clone()) {
        continue;
      }

      if let Some(rule) = index.get(name.as_str()) {
        // A rule pulled in here may reference names that a later directive is
        // responsible for supplying, so the reference set has to grow as rules
        // are emitted rather than being fixed at the original body.
        references.extend(referenced_names(&rule.text));
        pulled.push(rule.text.clone());
      }
    }
  }

  let mut output = String::new();

  if let Some(start) = &options.start_rule {
    output.push_str(&format!("$.start.$ = {}\n", start));
  }

  let body = body.trim();
  if !body.is_empty() {
    output.push_str(body);
    output.push('\n');
  }

  for rule in pulled {
    output.push_str(rule.trim_end());
    output.push('\n');
  }

  Ok(output)
}

/// Load a module and resolve its own directives, so that what a caller sees is
/// always basic CDDL.
fn load_module(
  name: &str,
  line: usize,
  source: &dyn ModuleSource,
  chain: &mut Vec<String>,
) -> Result<String, ModuleError> {
  if chain.iter().any(|entry| entry == name) {
    let mut cycle = chain.clone();
    cycle.push(name.to_string());

    return Err(ModuleError::CircularReference { chain: cycle });
  }

  let text = source
    .load(name)?
    .ok_or_else(|| ModuleError::ModuleNotFound {
      name: name.to_string(),
      line,
    })?;

  chain.push(name.to_string());
  let resolved = resolve(&text, source, &ResolveOptions::default(), chain);
  chain.pop();

  resolved
}

/// Resolve a name written in a directive against the directive's namespace.
fn qualify(written: &str, alias: Option<&str>) -> String {
  match alias {
    Some(alias) if !written.starts_with(&format!("{}.", alias)) => format!("{}.{}", alias, written),
    _ => written.to_string(),
  }
}

fn push_unique(names: &mut Vec<String>, name: String) {
  if !names.contains(&name) {
    names.push(name);
  }
}

/// Breadth-first transitive closure over rule dependencies, in discovery order.
fn closure(seeds: &[String], index: &BTreeMap<&str, &ModuleRule>) -> Vec<String> {
  let mut ordered: Vec<String> = Vec::new();
  let mut queue: Vec<String> = seeds.to_vec();
  let mut head = 0usize;

  while head < queue.len() {
    let name = queue[head].clone();
    head += 1;

    if ordered.contains(&name) {
      continue;
    }

    ordered.push(name.clone());

    if let Some(rule) = index.get(name.as_str()) {
      for dep in &rule.deps {
        if !ordered.contains(dep) {
          queue.push(dep.clone());
        }
      }
    }
  }

  ordered
}

/// Remove directive lines, leaving the rest of the document otherwise intact.
fn strip_directives(input: &str) -> String {
  let mut output = String::with_capacity(input.len());

  for line in input.lines() {
    if line.trim_end_matches('\r').starts_with(";#") {
      continue;
    }

    output.push_str(line);
    output.push('\n');
  }

  output
}

/// Split a document into its rules, recording each rule's dependencies on
/// other rules of the same document.
///
/// A name may be defined more than once — RFC 8610 allows a rule to be extended
/// with `/=` and `//=`, and those extensions need not be adjacent to the
/// original definition. All occurrences of a name are merged into a single
/// entry so that selecting a rule always carries its extensions with it.
fn rules_of(src: &str) -> Vec<ModuleRule> {
  let scanned = scan(src);
  let defined: BTreeSet<&str> = scanned
    .rules
    .iter()
    .map(|rule| rule.name.as_str())
    .collect();

  let mut merged: Vec<ModuleRule> = Vec::new();

  for rule in &scanned.rules {
    let mut deps: Vec<String> = Vec::new();

    for ident in &scanned.idents {
      if ident.role != IdentRole::Reference
        || ident.start < rule.start
        || ident.end > rule.end
        || ident.text == rule.name
        || PRELUDE.contains(&ident.text.as_str())
        || !defined.contains(ident.text.as_str())
      {
        continue;
      }

      push_unique(&mut deps, ident.text.clone());
    }

    let text = src[rule.start..rule.end].to_string();

    match merged.iter_mut().find(|existing| existing.name == rule.name) {
      Some(existing) => {
        existing.text.push('\n');
        existing.text.push_str(&text);

        for dep in deps {
          push_unique(&mut existing.deps, dep);
        }
      }
      None => merged.push(ModuleRule {
        name: rule.name.clone(),
        text,
        deps,
      }),
    }
  }

  merged
}

/// Every name referenced — as opposed to defined — by a document.
fn referenced_names(src: &str) -> BTreeSet<String> {
  scan(src)
    .idents
    .into_iter()
    .filter(|ident| ident.role == IdentRole::Reference)
    .map(|ident| ident.text)
    .collect()
}

/// Rewrite a module so that every name it defines, and every reference to one,
/// carries the given namespace prefix. Prelude names are left alone.
fn apply_namespace(src: &str, defined: &BTreeSet<String>, namespace: &str) -> String {
  let scanned = scan(src);
  let mut output = String::with_capacity(src.len());
  let mut cursor = 0usize;

  for ident in &scanned.idents {
    if !matches!(ident.role, IdentRole::RuleHead | IdentRole::Reference)
      || PRELUDE.contains(&ident.text.as_str())
      || !defined.contains(&ident.text)
    {
      continue;
    }

    output.push_str(&src[cursor..ident.start]);
    output.push_str(namespace);
    output.push('.');
    output.push_str(&ident.text);
    cursor = ident.end;
  }

  output.push_str(&src[cursor..]);
  output
}
