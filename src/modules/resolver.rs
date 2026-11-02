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
  /// specification. Corresponds to the tool's `-s` flag. Resolution fails if
  /// the input or selected module rules already define `$.start.$`.
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
  /// Whether the parser classifies the rule as a group rule. Only computed
  /// where an alias is generated, since it needs a full parse.
  group: bool,
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

    let rules = module_rules(&text, directive);
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

  render(options.start_rule.as_deref(), &body, pulled, &emitted)
}

fn render(
  start_rule: Option<&str>,
  body: &str,
  pulled: Vec<String>,
  emitted: &BTreeSet<String>,
) -> Result<String, ModuleError> {
  if start_rule.is_some() && emitted.contains("$.start.$") {
    return Err(ModuleError::Directive {
      line: 0,
      message: "the synthetic start rule conflicts with an existing definition".to_string(),
    });
  }

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

  Ok(output)
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
  let mut selected: BTreeSet<String> = BTreeSet::new();

  for selector in selectors {
    let written = match selector {
      NameSelector::All => {
        for rule in rules {
          if selected.insert(rule.name.clone()) {
            names.push(rule.name.clone());
          }
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
        // A generic rule needs a generic alias that forwards its parameters,
        // and a group rule needs an alias the parser also reads as a group:
        // `options = ns.options` and `options = (ns.options)` are both type
        // rules, while `options = (ns.options,)` is a group rule.
        let target = &index[resolved.as_str()];
        let (head, reference) = if target.params.is_empty() {
          (written.clone(), resolved.clone())
        } else {
          let params = target.params.join(", ");
          (
            format!("{}<{}>", written, params),
            format!("{}<{}>", resolved, params),
          )
        };
        aliases.push(if target.group {
          format!("{} = ({},)", head, reference)
        } else {
          format!("{} = {}", head, reference)
        });
      }
    }

    if selected.insert(resolved.clone()) {
      names.push(resolved);
    }
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
    Some(alias)
      if !PRELUDE.contains(&written)
        && !written[socket_sigils(written)..].starts_with(&format!("{}.", alias)) =>
    {
      namespaced(written, alias)
    }
    _ => written.to_string(),
  }
}

/// The number of leading `$` characters that make `name` a socket: one for a
/// type socket, two for a group socket. `$.start.$` is a plain identifier.
fn socket_sigils(name: &str) -> usize {
  let sigils = name.bytes().take(2).take_while(|b| *b == b'$').count();
  // Only a character that can start an identifier makes the `$` a sigil:
  // `$-value`, `$1` and `$.start.$` are plain identifiers.
  match name.as_bytes().get(sigils) {
    Some(b) if b.is_ascii_alphabetic() || matches!(b, b'@' | b'_' | b'$') => sigils,
    _ => 0,
  }
}

/// Prefix a name with a namespace after any socket sigils, so `$$options`
/// becomes `$$ns.options` and stays a group socket rather than turning into
/// the type name `ns.$$options`.
fn namespaced(name: &str, namespace: &str) -> String {
  let sigils = socket_sigils(name);
  format!("{}{}.{}", &name[..sigils], namespace, &name[sigils..])
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
  let mut slots: BTreeMap<&str, usize> = BTreeMap::new();
  // Rules and identifiers are both in source order and rules do not overlap,
  // so one cursor visits each identifier once.
  let mut cursor = 0usize;

  for rule in &scanned.rules {
    let mut deps: Vec<String> = Vec::new();
    let mut seen: BTreeSet<&str> = BTreeSet::new();

    while cursor < scanned.idents.len() && scanned.idents[cursor].start < rule.start {
      cursor += 1;
    }
    while cursor < scanned.idents.len() && scanned.idents[cursor].end <= rule.end {
      let ident = &scanned.idents[cursor];
      cursor += 1;
      // A prelude name is a dependency only when this document defines or
      // extends it; otherwise `defined` excludes it as a built-in.
      if ident.role == IdentRole::Reference
        && ident.text != rule.name
        && defined.contains(ident.text.as_str())
        && seen.insert(ident.text.as_str())
      {
        deps.push(ident.text.clone());
      }
    }

    let part = RulePart {
      text: src[rule.start..rule.end].to_string(),
      deps,
      extension: rule.extension,
    };

    match slots.get(rule.name.as_str()) {
      Some(slot) => merged[*slot].parts.push(part),
      None => {
        slots.insert(rule.name.as_str(), merged.len());
        merged.push(ModuleRule {
          name: rule.name.clone(),
          params: rule.params.clone(),
          parts: vec![part],
          group: false,
        });
      }
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
        namespaced(&ident.text, namespace)
      }
      _ => continue,
    };

    output.push_str(&src[cursor..ident.start]);
    // A new prefix can turn an adjacent numeric token into a different one:
    // `1item` -> `1e3.item`, or `0xffitem` -> `0xffa.item`. Separate the
    // tokens before rewriting, including renamed generic parameters.
    if ident.start > 0 && src.as_bytes()[ident.start - 1].is_ascii_alphanumeric() {
      output.push(' ');
    }
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

/// The rules of a (namespaced) module. For `import <names> from`, which emits
/// alias rules, group rules are also identified so their aliases stay groups.
fn module_rules(text: &str, directive: &Directive) -> Vec<ModuleRule> {
  let mut rules = rules_of(text);
  if directive.kind != DirectiveKind::Import || directive.names.is_none() {
    return rules;
  }
  // The module was validated when it was loaded, and namespacing only renames
  // identifiers, so this parse is expected to succeed.
  if let Ok(cddl) = crate::pest_bridge::cddl_from_pest_str(text) {
    let groups: BTreeSet<String> = cddl
      .rules
      .iter()
      .filter_map(|parsed| match parsed {
        crate::ast::Rule::Group { rule, .. } => Some(rule.name.to_string()),
        _ => None,
      })
      .collect();
    for rule in &mut rules {
      rule.group = groups.contains(&rule.name);
    }
  }
  rules
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn rules_of_handles_many_rules_and_extensions() {
    let mut src = String::new();
    for n in 0..10_000 {
      src.push_str(&format!(
        "rule{n} = [int, next{n}]\nnext{n} = tstr\n",
        n = n
      ));
    }
    for n in 0..10_000 {
      src.push_str(&format!("rule{n} /= uint\n", n = n));
    }
    let rules = rules_of(&src);
    assert_eq!(rules.len(), 20_000);
    let rule = rules.iter().find(|rule| rule.name == "rule9999").unwrap();
    assert_eq!(rule.parts.len(), 2);
    assert_eq!(rule.parts[0].deps, ["next9999"]);
    assert!(rule.parts[1].extension);
    assert!(rule.parts[1].deps.is_empty());
  }

  #[test]
  fn wildcard_selection_lists_each_rule_once_in_source_order() {
    let mut src = String::new();
    for n in 0..10_000 {
      src.push_str(&format!("rule{n} = int\nrule{n} /= tstr\n", n = n));
    }
    let rules = rules_of(&src);
    let index: BTreeMap<&str, &ModuleRule> = rules
      .iter()
      .map(|rule| (rule.name.as_str(), rule))
      .collect();
    let directive = Directive {
      kind: DirectiveKind::Include,
      names: Some(vec![
        NameSelector::All,
        NameSelector::Name("rule7".to_string()),
      ]),
      filename: "m".to_string(),
      alias: None,
      line: 1,
    };
    let selectors = directive.names.clone().unwrap();
    let names = select_named(&directive, &selectors, &rules, &index, None).unwrap();
    assert_eq!(names.len(), 10_000);
    assert_eq!(names[0], "rule0");
    assert_eq!(names[9_999], "rule9999");
  }

  #[test]
  fn only_identifier_starts_make_dollar_signs_sockets() {
    assert_eq!(namespaced("$$options", "ns"), "$$ns.options");
    assert_eq!(namespaced("$kind", "ns"), "$ns.kind");
    assert_eq!(namespaced("$$$x", "ns"), "$$ns.$x");
    assert_eq!(namespaced("$-value", "ns"), "ns.$-value");
    assert_eq!(namespaced("$1", "ns"), "ns.$1");
    assert_eq!(namespaced("$.start.$", "ns"), "ns.$.start.$");
  }
}
