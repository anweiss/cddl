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
  vec,
  vec::Vec,
};

#[cfg(feature = "std")]
use std::collections::{BTreeMap, BTreeSet};

use super::{
  directive::{is_filename, is_id, parse_directives, Directive, DirectiveKind, NameSelector},
  scan::{directive_ranges, scan, IdentRole},
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
/// All occurrences of a name — RFC 8610 allows a rule to be extended with `/=`
/// and `//=`, and those extensions need not be adjacent to the original
/// definition — are collected into one entry so that selecting a rule always
/// carries its extensions with it.
#[derive(Debug, Clone)]
struct ModuleRule {
  name: String,
  /// The generic parameters declared by the base definition, if any.
  params: Vec<String>,
  /// Every occurrence, in source order.
  parts: Vec<RulePart>,
}

/// One occurrence of a rule: its base definition or one extension.
#[derive(Debug, Clone)]
struct RulePart {
  text: String,
  deps: Vec<String>,
  /// Whether this part extends an existing definition (`/=` or `//=`).
  extension: bool,
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
  resolve(input, source, options, &mut Loader::default())
}

/// State shared by every module load within one top-level resolution.
#[derive(Default)]
struct Loader {
  /// Modules currently being resolved, outermost first, for cycle detection.
  chain: Vec<String>,
  /// Modules already resolved and validated. A module imported along many
  /// paths would otherwise be reloaded once per path, which grows
  /// exponentially with the depth of a shared dependency graph.
  resolved: BTreeMap<String, String>,
}

fn resolve(
  input: &str,
  source: &dyn ModuleSource,
  options: &ResolveOptions,
  loader: &mut Loader,
) -> Result<String, ModuleError> {
  let mut directives: Vec<Directive> = options
    .command_line_imports
    .iter()
    .map(|(namespace, module)| command_line_directive(namespace, module))
    .collect::<Result<_, _>>()?;

  directives.extend(parse_directives(input)?);

  let body = strip_directives(input);
  let local = rules_of(&body);

  let mut emitted: BTreeSet<String> = local.iter().map(|rule| rule.name.clone()).collect();
  // Parts already emitted, so a rule selected by two directives is not
  // duplicated. Extensions deduplicate separately from base definitions: a
  // base suppressed by a local definition must not suppress `/=` or `//=`
  // contributions.
  let mut emitted_parts: BTreeSet<String> = BTreeSet::new();
  let mut references = referenced_names(&body);

  if let Some(start) = &options.start_rule {
    if !is_id(start) {
      return Err(ModuleError::Directive {
        line: 0,
        message: format!("\"{}\" is not a valid start rule", start),
      });
    }
    references.insert(start.clone());
  }

  let mut pulled: Vec<String> = Vec::new();

  for directive in &directives {
    let text = load_module(&directive.filename, directive.line, source, loader)?;

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
    let index: BTreeMap<&str, &ModuleRule> = rules
      .iter()
      .map(|rule| (rule.name.as_str(), rule))
      .collect();

    let mut aliases: Vec<String> = Vec::new();

    match (&directive.kind, &directive.names) {
      // `include <module>`: every rule of the module, verbatim.
      (DirectiveKind::Include, None) => {
        for rule in &rules {
          emit_rule(
            rule,
            &mut emitted,
            &mut emitted_parts,
            &mut references,
            &mut pulled,
          );
        }
      }

      // `include <names> from <module>`: exactly the rules named.
      (DirectiveKind::Include, Some(selectors)) => {
        for name in select_named(directive, selectors, &rules, &index, None)? {
          if let Some(rule) = index.get(name.as_str()) {
            emit_rule(
              rule,
              &mut emitted,
              &mut emitted_parts,
              &mut references,
              &mut pulled,
            );
          }
        }
      }

      // `import <module>`: the rules this module references, transitively.
      (DirectiveKind::Import, None) => {
        let seeds: Vec<String> = rules
          .iter()
          .map(|rule| rule.name.clone())
          .filter(|name| references.contains(name))
          .collect();

        for (name, part) in closure(&seeds, &index, &emitted) {
          emit_part(
            name,
            part,
            &mut emitted,
            &mut emitted_parts,
            &mut references,
            &mut pulled,
          );
        }
      }

      // `import <names> from <module>`: the rules named, transitively.
      (DirectiveKind::Import, Some(selectors)) => {
        let seeds = select_named(
          directive,
          selectors,
          &rules,
          &index,
          Some((&mut emitted, &mut aliases)),
        )?;
        for alias in aliases.drain(..) {
          pulled.push(alias);
        }
        for (name, part) in closure(&seeds, &index, &emitted) {
          emit_part(
            name,
            part,
            &mut emitted,
            &mut emitted_parts,
            &mut references,
            &mut pulled,
          );
        }
      }
    }
  }

  Ok(render(options.start_rule.as_deref(), &body, pulled))
}

fn render(start_rule: Option<&str>, body: &str, pulled: Vec<String>) -> String {
  let mut output = String::new();

  if let Some(start) = start_rule {
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

  output
}

/// Emit every part of `rule` that is not already supplied: a base definition
/// whose name is already present (locally or from an earlier directive) is
/// skipped, but `/=` and `//=` extensions always travel with the selection.
fn emit_rule(
  rule: &ModuleRule,
  emitted: &mut BTreeSet<String>,
  emitted_parts: &mut BTreeSet<String>,
  references: &mut BTreeSet<String>,
  pulled: &mut Vec<String>,
) {
  // Base definitions go first: emitting an extension marks the name as
  // defined (see emit_part), and a module that carries both must not lose
  // its own base to that marker.
  for part in rule
    .parts
    .iter()
    .filter(|part| !part.extension)
    .chain(rule.parts.iter().filter(|part| part.extension))
  {
    if part.extension || !emitted.contains(&rule.name) {
      emit_part(&rule.name, part, emitted, emitted_parts, references, pulled);
    }
  }
}

fn emit_part(
  name: &str,
  part: &RulePart,
  emitted: &mut BTreeSet<String>,
  emitted_parts: &mut BTreeSet<String>,
  references: &mut BTreeSet<String>,
  pulled: &mut Vec<String>,
) {
  if !emitted_parts.insert(format!("{}\u{0}{}", name, part.text)) {
    return;
  }

  // A bare extension introduces the name as surely as a base definition
  // does, so a later module's base definition for the same name must be
  // suppressed rather than emitted as a duplicate.
  emitted.insert(name.to_string());

  // A rule pulled in here may reference names that a later directive is
  // responsible for supplying, so the reference set has to grow as rules
  // are emitted rather than being fixed at the original body.
  references.extend(referenced_names(&part.text));
  pulled.push(part.text.clone());
}

fn command_line_directive(namespace: &str, module: &str) -> Result<Directive, ModuleError> {
  if !is_filename(module) {
    return Err(ModuleError::Directive {
      line: 0,
      message: format!("\"{}\" is not a valid module name", module),
    });
  }
  if !is_id(namespace) {
    return Err(ModuleError::Directive {
      line: 0,
      message: format!("\"{}\" is not a valid namespace", namespace),
    });
  }
  Ok(Directive {
    kind: DirectiveKind::Import,
    names: None,
    filename: module.to_string(),
    alias: Some(namespace.to_string()),
    line: 0,
  })
}

/// Resolve the names of a `from` clause against a module. When `alias_sink` is
/// supplied (for `import`), names written without the namespace prefix also
/// get an unprefixed alias rule, per §2.6.
fn select_named(
  directive: &Directive,
  selectors: &[NameSelector],
  rules: &[ModuleRule],
  index: &BTreeMap<&str, &ModuleRule>,
  mut alias_sink: Option<(&mut BTreeSet<String>, &mut Vec<String>)>,
) -> Result<Vec<String>, ModuleError> {
  let mut names = Vec::new();

  for selector in selectors {
    let written = match selector {
      NameSelector::All => {
        for rule in rules {
          push_unique(&mut names, rule.name.clone());
        }
        continue;
      }
      NameSelector::Name(written) => written,
    };

    let resolved = qualify(written, directive.alias.as_deref());
    if !index.contains_key(resolved.as_str()) {
      return Err(ModuleError::RuleNotFound {
        rule: written.clone(),
        module: directive.filename.clone(),
        line: directive.line,
      });
    }

    if let Some((emitted, aliases)) = alias_sink.as_mut() {
      if resolved != *written && emitted.insert(written.clone()) {
        // A generic rule needs a generic alias that forwards its parameters.
        let params = &index[resolved.as_str()].params;
        aliases.push(if params.is_empty() {
          format!("{} = {}", written, resolved)
        } else {
          let params = params.join(", ");
          format!("{0}<{1}> = {2}<{1}>", written, params, resolved)
        });
      }
    }

    push_unique(&mut names, resolved);
  }

  Ok(names)
}

/// Load a module and resolve its own directives, so that what a caller sees is
/// always basic CDDL.
fn load_module(
  name: &str,
  line: usize,
  source: &dyn ModuleSource,
  loader: &mut Loader,
) -> Result<String, ModuleError> {
  if loader.chain.iter().any(|entry| entry == name) {
    let mut cycle = loader.chain.clone();
    cycle.push(name.to_string());

    return Err(ModuleError::CircularReference { chain: cycle });
  }

  if let Some(resolved) = loader.resolved.get(name) {
    return Ok(resolved.clone());
  }

  let text = source
    .load(name)?
    .ok_or_else(|| ModuleError::ModuleNotFound {
      name: name.to_string(),
      line,
    })?;

  loader.chain.push(name.to_string());
  let resolved = resolve(&text, source, &ResolveOptions::default(), loader);
  loader.chain.pop();

  let resolved = resolved?;
  validate_module(name, &resolved)?;

  loader.resolved.insert(name.to_string(), resolved.clone());
  Ok(resolved)
}

/// Check that a resolved module is basic CDDL, so that a malformed module is
/// reported against the module that contains it rather than surfacing later as
/// a parse failure of the resolved document.
fn validate_module(name: &str, resolved: &str) -> Result<(), ModuleError> {
  crate::pest_bridge::cddl_from_pest_str(resolved)
    .map(|_| ())
    .map_err(|message| ModuleError::ModuleParse {
      name: name.to_string(),
      message: message.to_string(),
    })
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

/// Breadth-first transitive closure over rule dependencies, in discovery
/// order. Only parts that will actually be retained are returned, and only
/// their dependencies are followed: a base definition suppressed by a local
/// definition or an earlier directive is satisfied by that existing
/// definition, so its module-side dependencies are not pulled in.
fn closure<'a>(
  seeds: &[String],
  index: &BTreeMap<&str, &'a ModuleRule>,
  emitted: &BTreeSet<String>,
) -> Vec<(&'a str, &'a RulePart)> {
  let mut retained: Vec<(&'a str, &'a RulePart)> = Vec::new();
  let mut queue: Vec<String> = seeds.to_vec();
  let mut seen: BTreeSet<String> = BTreeSet::new();
  let mut head = 0usize;

  while head < queue.len() {
    let name = queue[head].clone();
    head += 1;

    if !seen.insert(name.clone()) {
      continue;
    }

    if let Some(rule) = index.get(name.as_str()) {
      for part in &rule.parts {
        if !part.extension && emitted.contains(&rule.name) {
          continue;
        }

        retained.push((rule.name.as_str(), part));

        for dep in &part.deps {
          if !seen.contains(dep) {
            queue.push(dep.clone());
          }
        }
      }
    }
  }

  retained
}

/// Remove directive lines, leaving the rest of the document otherwise intact.
fn strip_directives(input: &str) -> String {
  let mut output = String::with_capacity(input.len());

  let mut cursor = 0;
  for range in directive_ranges(input) {
    output.push_str(&input[cursor..range.start]);
    cursor = range.end;
  }
  output.push_str(&input[cursor..]);
  output
}

/// Split a document into its rules, recording each occurrence's dependencies
/// on other rules of the same document. Occurrences of one name are grouped
/// into a single [`ModuleRule`] but kept as separate parts, so that a base
/// definition and its `/=` or `//=` extensions can be deduplicated
/// independently during emission.
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

    let part = RulePart {
      text: src[rule.start..rule.end].to_string(),
      deps,
      extension: rule.extension,
    };

    match merged
      .iter_mut()
      .find(|existing| existing.name == rule.name)
    {
      Some(existing) => existing.parts.push(part),
      None => merged.push(ModuleRule {
        name: rule.name.clone(),
        params: rule.params.clone(),
        parts: vec![part],
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
  let renamed = rename_capturing_params(&scanned.idents, namespace);
  let mut output = String::with_capacity(src.len());
  let mut cursor = 0usize;

  for ident in &scanned.idents {
    let replacement = match ident.role {
      IdentRole::GenericParam => match renamed.get(&ident.text) {
        Some(fresh) => fresh.clone(),
        None => continue,
      },
      IdentRole::RuleHead | IdentRole::Reference
        if !PRELUDE.contains(&ident.text.as_str()) && defined.contains(&ident.text) =>
      {
        format!("{}.{}", namespace, ident.text)
      }
      _ => continue,
    };

    output.push_str(&src[cursor..ident.start]);
    output.push_str(&replacement);
    cursor = ident.end;
  }

  output.push_str(&src[cursor..]);
  output
}

/// Generic parameters already spelled with the namespace prefix would capture
/// the module's own names once those are prefixed (`wrapper<ns.item> = [item]`
/// would turn `item` into the parameter). Map each such parameter to a fresh
/// name that occurs nowhere in the module and cannot take the prefix.
fn rename_capturing_params(
  idents: &[super::scan::IdentToken],
  namespace: &str,
) -> BTreeMap<String, String> {
  let prefix = format!("{}.", namespace);
  let mut taken: BTreeSet<String> = idents.iter().map(|ident| ident.text.clone()).collect();
  let mut renamed = BTreeMap::new();

  for ident in idents {
    if ident.role != IdentRole::GenericParam
      || !ident.text.starts_with(&prefix)
      || renamed.contains_key(&ident.text)
    {
      continue;
    }

    let base = ident.text.replace('.', "-");
    let mut fresh = base.clone();
    let mut n = 1usize;
    while taken.contains(&fresh) {
      fresh = format!("{}-{}", base, n);
      n += 1;
    }
    taken.insert(fresh.clone());
    renamed.insert(ident.text.clone(), fresh);
  }

  renamed
}
