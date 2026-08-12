//! Internal code generation logic for converting CDDL AST to Rust source code.

use cddl::ast::{
  Group, GroupChoice, GroupEntry, MemberKey, Occur, Rule, Type, Type1, Type2, TypeChoice, TypeRule,
  ValueMemberKeyEntry, CDDL,
};
use std::collections::BTreeMap;
use std::fmt::Write;

/// Errors that can occur during code generation.
#[derive(Debug)]
pub(crate) enum CodegenError {
  /// CDDL parsing failed.
  ParseError(String),
  /// Formatting error.
  FmtError(std::fmt::Error),
}

impl std::fmt::Display for CodegenError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      CodegenError::ParseError(msg) => write!(f, "CDDL parse error: {}", msg),
      CodegenError::FmtError(e) => write!(f, "formatting error: {}", e),
    }
  }
}

impl From<std::fmt::Error> for CodegenError {
  fn from(e: std::fmt::Error) -> Self {
    CodegenError::FmtError(e)
  }
}

/// Caller-supplied configuration for code generation.
///
/// Every field is opt-in; the default is the behaviour that existed before
/// these options were introduced. See
/// <https://github.com/anweiss/cddl/issues/641>.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct CodegenOptions {
  /// Rust type to generate for CDDL `any`. Defaults to `serde_json::Value`.
  ///
  /// Set this to `ciborium::Value` for a CBOR-first schema. Individual fields
  /// can still be overridden through `substitutions`.
  pub any_type: Option<String>,
  /// Emit `#[non_exhaustive]` on generated structs and enums, so that adding a
  /// field or variant later is not a breaking change for downstream crates.
  pub non_exhaustive: bool,
  /// Append an `Other(String)` catch-all variant to generated enums, so that a
  /// value added to the schema later deserializes instead of failing.
  pub other_variant: bool,
  /// Replace generated types with hand-written Rust types.
  ///
  /// A key is either a CDDL rule name (`"label"`), which replaces every
  /// reference to that rule and suppresses its definition, or a
  /// rule-qualified field name (`"manifest.data"`), which replaces just that
  /// field.
  pub substitutions: BTreeMap<String, String>,
}

impl CodegenOptions {
  /// The Rust type to use for CDDL `any`.
  fn any_type(&self) -> &str {
    self.any_type.as_deref().unwrap_or(DEFAULT_ANY_TYPE)
  }

  /// Whether any option would change the generated output.
  fn is_default(&self) -> bool {
    *self == Self::default()
  }
}

/// The Rust type CDDL `any` maps to unless overridden.
pub(crate) const DEFAULT_ANY_TYPE: &str = "serde_json::Value";

/// A generated Rust type definition.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RustTypeDef {
  Struct {
    name: String,
    fields: Vec<RustField>,
    doc: Vec<String>,
  },
  TypeAlias {
    name: String,
    target: String,
    doc: Vec<String>,
  },
  Enum {
    name: String,
    variants: Vec<RustEnumVariant>,
    doc: Vec<String>,
  },
}

/// A field within a generated Rust struct.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct RustField {
  pub name: String,
  pub original_name: String,
  pub rust_type: String,
  pub is_optional: bool,
  pub doc: Vec<String>,
  pub is_boxed: bool,
  /// Whether this field is generated for a CDDL construct that has no field key.
  pub is_synthetic: bool,
  /// The CBOR tag this field's CDDL type carries, if any (see
  /// https://github.com/anweiss/cddl/issues/639).
  pub tag: Option<TaggedPrelude>,
}

/// A CDDL prelude type that is defined as a CBOR tag wrapping a simpler value.
///
/// RFC 8610 Appendix D defines these as `#6.N(...)`. The generated Rust type is
/// the *inner* type, with a serde helper that applies the tag on the wire.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TaggedPrelude {
  /// The CDDL identifier, e.g. `tdate`.
  pub ident: &'static str,
  /// The CBOR tag number, e.g. 0 for `tdate`.
  pub tag: u64,
  /// The Rust type the tag wraps, e.g. `String`.
  pub inner: &'static str,
}

/// The prelude types that are CBOR tags, and the tag each one carries.
///
/// | CDDL | Definition | Tag |
/// |---|---|---|
/// | `tdate` | `#6.0(tstr)` | 0 |
/// | `time` | `#6.1(number)` | 1 |
/// | `uri` | `#6.32(tstr)` | 32 |
/// | `b64url` | `#6.33(tstr)` | 33 |
/// | `b64legacy` | `#6.34(tstr)` | 34 |
/// | `regexp` | `#6.35(tstr)` | 35 |
const TAGGED_PRELUDE_TYPES: &[TaggedPrelude] = &[
  TaggedPrelude {
    ident: "tdate",
    tag: 0,
    inner: "String",
  },
  TaggedPrelude {
    ident: "time",
    tag: 1,
    inner: "i64",
  },
  TaggedPrelude {
    ident: "uri",
    tag: 32,
    inner: "String",
  },
  TaggedPrelude {
    ident: "b64url",
    tag: 33,
    inner: "String",
  },
  TaggedPrelude {
    ident: "b64legacy",
    tag: 34,
    inner: "String",
  },
  TaggedPrelude {
    ident: "regexp",
    tag: 35,
    inner: "String",
  },
];

/// Look up the CBOR tag for a CDDL prelude identifier.
pub(crate) fn tagged_prelude(ident: &str) -> Option<TaggedPrelude> {
  TAGGED_PRELUDE_TYPES
    .iter()
    .copied()
    .find(|t| t.ident == ident)
}

/// A variant within a generated Rust enum.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct RustEnumVariant {
  pub name: String,
  pub inner_type: Option<String>,
  pub doc: Vec<String>,
  /// The CDDL string literal this variant corresponds to, used to emit a
  /// `#[serde(rename = ...)]` attribute when it differs from the variant name.
  pub rename: Option<String>,
}

/// Clean a slice of raw comment texts (each the substring following `;`) into
/// trimmed documentation lines, dropping bare newline markers.
fn clean_comment_lines(raw: &[&str]) -> Vec<String> {
  raw
    .iter()
    .filter(|c| **c != "\n")
    .map(|c| c.trim().to_string())
    .filter(|c| !c.is_empty())
    .collect()
}

/// Doc comment lines attached directly to a rule in the AST (the contiguous
/// `; ...` block immediately above it), as populated by the parser.
fn ast_rule_doc(rule: &Rule<'_>) -> Vec<String> {
  rule
    .comments_before_rule()
    .map(|c| clean_comment_lines(&c.0))
    .unwrap_or_default()
}

/// Doc comment lines attached directly to a group entry in the AST: its leading
/// comment block followed by any trailing same-line comment.
fn ast_entry_doc(entry: &GroupEntry<'_>) -> Vec<String> {
  let (leading, trailing) = match entry {
    GroupEntry::ValueMemberKey {
      leading_comments,
      trailing_comments,
      ..
    }
    | GroupEntry::TypeGroupname {
      leading_comments,
      trailing_comments,
      ..
    } => (leading_comments, trailing_comments),
    GroupEntry::InlineGroup { .. } => return Vec::new(),
  };

  let mut docs = Vec::new();
  if let Some(c) = leading {
    docs.extend(clean_comment_lines(&c.0));
  }
  if let Some(c) = trailing {
    docs.extend(clean_comment_lines(&c.0));
  }
  docs
}

/// Prefer the rule's AST-attached doc comments; fall back to recovering them
/// from the raw source via the line-indexed `CommentMap` when the AST carries
/// none (e.g. a build configuration without `ast-comments`).
fn ast_or_map_rule_doc(rule: &Rule<'_>, line: usize, comments: &CommentMap) -> Vec<String> {
  let ast = ast_rule_doc(rule);
  if ast.is_empty() {
    comments.docs_for(line)
  } else {
    ast
  }
}

/// Doc comments extracted from CDDL source, indexed by 1-based source line.
///
/// CDDL comments (`; ...`) that appear on their own line immediately above a
/// rule or field become "leading" documentation, and a comment trailing a
/// definition on the same line is appended after it. Together they are emitted
/// as Rust doc comments on the generated types and fields.
#[derive(Debug, Default)]
pub(crate) struct CommentMap {
  /// For each 1-based line, the cleaned text of a stand-alone comment line.
  pure: Vec<Option<String>>,
  /// For each 1-based line, the cleaned text of a trailing comment.
  trailing: Vec<Option<String>>,
}

impl CommentMap {
  /// Build a comment map from raw CDDL source text.
  pub(crate) fn new(source: &str) -> Self {
    // Index 0 is a placeholder so that lines are addressable 1-based.
    let mut pure = vec![None];
    let mut trailing = vec![None];

    for line in source.lines() {
      let (code, comment) = split_comment(line);
      match comment {
        Some(c) => {
          let cleaned = c.trim().to_string();
          if code.trim().is_empty() {
            pure.push(Some(cleaned));
            trailing.push(None);
          } else {
            pure.push(None);
            trailing.push(Some(cleaned));
          }
        }
        None => {
          pure.push(None);
          trailing.push(None);
        }
      }
    }

    CommentMap { pure, trailing }
  }

  /// Collect the doc comment lines associated with the given 1-based source
  /// line: any contiguous stand-alone comment lines directly above it, followed
  /// by a trailing comment on the line itself.
  pub(crate) fn docs_for(&self, line: usize) -> Vec<String> {
    let mut docs = Vec::new();

    // Walk upward over contiguous stand-alone comment lines. The range is empty
    // when `line <= 1`, so there is no risk of unsigned underflow.
    for l in (1..line).rev() {
      match self.pure.get(l).and_then(|c| c.as_ref()) {
        Some(c) => docs.push(c.clone()),
        None => break,
      }
    }
    docs.reverse();

    if let Some(Some(c)) = self.trailing.get(line) {
      docs.push(c.clone());
    }

    docs
  }
}

/// Split a CDDL source line into its code portion and an optional trailing
/// comment (the text following `;`, excluding the `;` itself). Semicolons
/// inside text strings (`"..."`) or byte strings (`'...'`) are ignored.
fn split_comment(line: &str) -> (&str, Option<&str>) {
  let bytes = line.as_bytes();
  let mut in_dquote = false;
  let mut in_squote = false;
  let mut i = 0;
  while i < bytes.len() {
    match bytes[i] {
      b'\\' if in_dquote || in_squote => {
        // Skip escaped character.
        i += 1;
      }
      b'"' if !in_squote => in_dquote = !in_dquote,
      b'\'' if !in_dquote => in_squote = !in_squote,
      b';' if !in_dquote && !in_squote => {
        return (&line[..i], Some(&line[i + 1..]));
      }
      _ => {}
    }
    i += 1;
  }
  (line, None)
}

/// Apply caller-supplied options to the collected type definitions.
///
/// This runs after the CDDL has been lowered to `RustTypeDef`s so that the
/// conversion itself stays option-free: everything here is a rewrite of the
/// already-generated Rust type strings.
fn apply_options(defs: &mut Vec<RustTypeDef>, opts: &CodegenOptions) {
  if opts.is_default() {
    return;
  }

  // Rule-level substitutions replace every reference to a rule, so the rule's
  // own definition is dropped.
  let rule_subs: BTreeMap<String, String> = opts
    .substitutions
    .iter()
    .filter(|(k, _)| !k.contains('.'))
    .map(|(k, v)| (to_pascal_case(k), v.clone()))
    .collect();

  if !rule_subs.is_empty() {
    defs.retain(|d| {
      let name = match d {
        RustTypeDef::Struct { name, .. }
        | RustTypeDef::TypeAlias { name, .. }
        | RustTypeDef::Enum { name, .. } => name,
      };
      !rule_subs.contains_key(name)
    });
  }

  let any_type = opts.any_type();

  for def in defs.iter_mut() {
    match def {
      RustTypeDef::Struct { name, fields, .. } => {
        for field in fields.iter_mut() {
          let key = format!("{}.{}", to_cddl_lookup_name(name), field.original_name);
          if let Some(sub) = opts.substitutions.get(&key) {
            // An explicit substitution replaces the type outright, so any CBOR
            // tag inferred from the original prelude type no longer applies.
            field.rust_type.clone_from(sub);
            field.tag = None;
            continue;
          }

          rewrite_type(&mut field.rust_type, &rule_subs, any_type);
        }
      }
      RustTypeDef::TypeAlias { target, .. } => {
        rewrite_type(target, &rule_subs, any_type);
      }
      RustTypeDef::Enum { variants, .. } => {
        for variant in variants.iter_mut() {
          if let Some(inner) = variant.inner_type.as_mut() {
            rewrite_type(inner, &rule_subs, any_type);
          }
        }
      }
    }
  }
}

/// Rewrite a generated Rust type string in place, applying rule substitutions
/// and the configured `any` type.
///
/// Types are matched on identifier boundaries so that a substitution for rule
/// `Label` does not also rewrite `LabelSet`, and so that substitutions apply
/// inside containers such as `Vec<Label>` and `Option<Label>`.
fn rewrite_type(ty: &mut String, rule_subs: &BTreeMap<String, String>, any_type: &str) {
  if rule_subs.is_empty() && any_type == DEFAULT_ANY_TYPE {
    return;
  }

  let mut out = String::with_capacity(ty.len());
  let mut token = String::new();

  let flush = |token: &mut String, out: &mut String| {
    if token.is_empty() {
      return;
    }
    if token == DEFAULT_ANY_TYPE {
      out.push_str(any_type);
    } else if let Some(sub) = rule_subs.get(token.as_str()) {
      out.push_str(sub);
    } else {
      out.push_str(token);
    }
    token.clear();
  };

  for c in ty.chars() {
    if c.is_alphanumeric() || c == '_' || c == ':' {
      token.push(c);
    } else {
      flush(&mut token, &mut out);
      out.push(c);
    }
  }
  flush(&mut token, &mut out);

  *ty = out;
}

/// Convert a generated Rust type name back to the CDDL-style name used as a
/// substitution key, so that callers can write `"manifest.data"` rather than
/// having to know the PascalCase name codegen chose.
fn to_cddl_lookup_name(rust_name: &str) -> String {
  pascal_to_cddl_name(rust_name)
}

/// Generate Rust source code for all rules in a parsed CDDL AST.
pub(crate) fn generate_all_types(
  cddl: &CDDL<'_>,
  source: &str,
  opts: &CodegenOptions,
) -> Result<String, CodegenError> {
  let comments = CommentMap::new(source);
  let mut type_defs = collect_type_defs(cddl, &comments)?;
  apply_options(&mut type_defs, opts);
  render_type_defs(&type_defs, opts)
}

/// Generate Rust source code for a single named rule in a parsed CDDL AST.
///
/// If `output_name` is provided, the generated type will use that name instead
/// of the name derived from the CDDL rule. This allows the `#[cddl]` attribute
/// macro to preserve the user's chosen struct name.
pub(crate) fn generate_single_type(
  cddl: &CDDL<'_>,
  rule_name: &str,
  output_name: Option<&str>,
  source: &str,
  opts: &CodegenOptions,
) -> Result<String, CodegenError> {
  let comments = CommentMap::new(source);
  let mut type_defs = collect_type_defs(cddl, &comments)?;
  apply_options(&mut type_defs, opts);
  let matching = type_defs
    .into_iter()
    .find(|d| match d {
      RustTypeDef::Struct { name, .. }
      | RustTypeDef::TypeAlias { name, .. }
      | RustTypeDef::Enum { name, .. } => name == rule_name,
    })
    .ok_or_else(|| {
      CodegenError::ParseError(format!(
        "no rule matching '{}' found in CDDL definition",
        rule_name
      ))
    })?;

  let mut output = String::new();
  match &matching {
    RustTypeDef::Struct { name, fields, doc } => {
      let emit_name = output_name.unwrap_or(name);
      // Scope the helper module to this type, so several single-type macro
      // invocations can coexist in one file without colliding.
      let tag_mod = format!("{}_{}", TAG_HELPER_MOD, to_snake_case(emit_name));
      let tags = collect_tags(std::slice::from_ref(&matching));
      render_tag_helpers(&mut output, &tag_mod, &tags)?;
      render_struct(&mut output, emit_name, fields, doc, &tag_mod, opts)?;
    }
    RustTypeDef::TypeAlias { name, target, doc } => {
      let emit_name = output_name.unwrap_or(name);
      render_type_alias(&mut output, emit_name, target, doc)?;
    }
    RustTypeDef::Enum {
      name,
      variants,
      doc,
    } => {
      let emit_name = output_name.unwrap_or(name);
      render_enum(&mut output, emit_name, variants, doc, opts)?;
    }
  }
  Ok(output)
}

/// Convert a PascalCase struct name to a CDDL-style kebab-case identifier.
pub(crate) fn pascal_to_cddl_name(pascal: &str) -> String {
  let mut result = String::with_capacity(pascal.len() + 4);
  for (i, c) in pascal.chars().enumerate() {
    if c.is_uppercase() {
      if i > 0 {
        result.push('-');
      }
      result.push(c.to_lowercase().next().unwrap());
    } else {
      result.push(c);
    }
  }
  result
}

// --- Internal helpers (unchanged from original codegen) ---

fn collect_type_defs(
  cddl: &CDDL<'_>,
  comments: &CommentMap,
) -> Result<Vec<RustTypeDef>, CodegenError> {
  // Group type rules by their generated Rust name in a single pass so that
  // socket/plug alternates (e.g. `$foo /= int` and `$foo /= tstr`) can be
  // merged into a single enum instead of producing duplicate definitions.
  let mut type_rule_alternates: std::collections::HashMap<String, Vec<&TypeRule<'_>>> =
    std::collections::HashMap::new();
  for rule in &cddl.rules {
    if let Rule::Type {
      rule: type_rule, ..
    } = rule
    {
      type_rule_alternates
        .entry(to_pascal_case(type_rule.name.ident))
        .or_default()
        .push(type_rule);
    }
  }

  let mut defs = Vec::new();
  let mut merged: std::collections::HashSet<String> = std::collections::HashSet::new();
  for rule in &cddl.rules {
    match rule {
      Rule::Type {
        rule: type_rule, ..
      } => {
        let name = to_pascal_case(type_rule.name.ident);
        let alternates = type_rule_alternates.get(&name);
        if alternates.map(|a| a.len()).unwrap_or(0) > 1 {
          // Multiple type rules share this name (socket/plug alternates).
          // Merge all of their type choices into a single enum, emitting it
          // once at the position of the first alternate.
          if merged.insert(name.clone()) {
            let doc = ast_or_map_rule_doc(rule, type_rule.name.span.2, comments);
            defs.push(merge_type_rules_to_enum(
              &name,
              alternates.unwrap(),
              comments,
              doc,
            )?);
          }
        } else {
          let doc = ast_or_map_rule_doc(rule, type_rule.name.span.2, comments);
          if let Some(def) = type_rule_to_rust_def(type_rule, comments, doc)? {
            defs.push(def);
          }
        }
      }
      Rule::Group {
        rule: group_rule, ..
      } => {
        let name = to_pascal_case(group_rule.name.ident);
        let doc = ast_or_map_rule_doc(rule, group_rule.name.span.2, comments);
        if let Some(fields) = group_entry_to_fields(&group_rule.entry, comments)? {
          defs.push(RustTypeDef::Struct { name, fields, doc });
        }
      }
    }
  }
  apply_recursive_boxing(&mut defs);
  Ok(defs)
}

/// Merge the type choices of several socket/plug type rule alternates into a
/// single enum definition.
fn merge_type_rules_to_enum(
  name: &str,
  rules: &[&TypeRule<'_>],
  comments: &CommentMap,
  doc: Vec<String>,
) -> Result<RustTypeDef, CodegenError> {
  let mut variants = Vec::new();
  for rule in rules {
    for tc in &rule.value.type_choices {
      variants.push(type_choice_to_variant(tc, comments)?);
    }
  }
  Ok(RustTypeDef::Enum {
    name: name.to_string(),
    variants,
    doc,
  })
}

fn apply_recursive_boxing(defs: &mut [RustTypeDef]) {
  let mut names = Vec::new();
  for def in defs.iter() {
    match def {
      RustTypeDef::Struct { name, .. }
      | RustTypeDef::TypeAlias { name, .. }
      | RustTypeDef::Enum { name, .. } => names.push(name.clone()),
    }
  }

  let mut index_by_name = std::collections::HashMap::new();
  for (idx, name) in names.iter().enumerate() {
    index_by_name.insert(name.clone(), idx);
  }

  let mut edges: Vec<Vec<usize>> = vec![Vec::new(); names.len()];
  for def in defs.iter() {
    match def {
      RustTypeDef::Struct { name, fields, .. } => {
        let Some(&src_idx) = index_by_name.get(name) else {
          continue;
        };
        for field in fields {
          if let Some(&dst_idx) = index_by_name.get(&field.rust_type) {
            edges[src_idx].push(dst_idx);
          }
        }
      }
      RustTypeDef::TypeAlias { name, target, .. } => {
        let Some(&src_idx) = index_by_name.get(name) else {
          continue;
        };
        if let Some(&dst_idx) = index_by_name.get(target) {
          edges[src_idx].push(dst_idx);
        }
      }
      RustTypeDef::Enum { name, variants, .. } => {
        let Some(&src_idx) = index_by_name.get(name) else {
          continue;
        };
        for variant in variants {
          if let Some(inner) = &variant.inner_type {
            if let Some(&dst_idx) = index_by_name.get(inner) {
              edges[src_idx].push(dst_idx);
            }
          }
        }
      }
    }
  }

  let scc_ids = compute_scc_ids(&edges);
  let mut scc_sizes: std::collections::HashMap<usize, usize> = std::collections::HashMap::new();
  for &scc_id in &scc_ids {
    *scc_sizes.entry(scc_id).or_insert(0) += 1;
  }
  let mut has_self_loop = vec![false; names.len()];
  for (src, targets) in edges.iter().enumerate() {
    for &dst in targets {
      if src == dst {
        has_self_loop[src] = true;
        break;
      }
    }
  }

  for def in defs.iter_mut() {
    let RustTypeDef::Struct { name, fields, .. } = def else {
      continue;
    };
    let Some(&src_idx) = index_by_name.get(name) else {
      continue;
    };
    let src_scc = scc_ids[src_idx];
    let is_cyclic_scc = scc_sizes
      .get(&src_scc)
      .copied()
      .expect("SCC size must exist for every computed SCC id")
      > 1
      || has_self_loop[src_idx];
    if !is_cyclic_scc {
      continue;
    }

    for field in fields.iter_mut() {
      let Some(&dst_idx) = index_by_name.get(&field.rust_type) else {
        continue;
      };
      if scc_ids[dst_idx] != src_scc {
        continue;
      }
      // Box descending edges in cyclic SCCs so cycle breaking is deterministic.
      if name.as_str() >= field.rust_type.as_str() {
        field.is_boxed = true;
      }
    }
  }
}

fn compute_scc_ids(edges: &[Vec<usize>]) -> Vec<usize> {
  const UNVISITED: usize = usize::MAX;

  fn dfs(node: usize, edges: &[Vec<usize>], visited: &mut [bool], order: &mut Vec<usize>) {
    if visited[node] {
      return;
    }
    visited[node] = true;
    for &next in &edges[node] {
      dfs(next, edges, visited, order);
    }
    order.push(node);
  }

  fn reverse_dfs(node: usize, rev_edges: &[Vec<usize>], scc_id: usize, scc_ids: &mut [usize]) {
    if scc_ids[node] != UNVISITED {
      return;
    }
    scc_ids[node] = scc_id;
    for &next in &rev_edges[node] {
      reverse_dfs(next, rev_edges, scc_id, scc_ids);
    }
  }

  let mut order = Vec::with_capacity(edges.len());
  let mut visited = vec![false; edges.len()];
  for node in 0..edges.len() {
    dfs(node, edges, &mut visited, &mut order);
  }

  let mut rev_edges = vec![Vec::new(); edges.len()];
  for (src, targets) in edges.iter().enumerate() {
    for &dst in targets {
      rev_edges[dst].push(src);
    }
  }

  let mut scc_ids = vec![UNVISITED; edges.len()];
  let mut next_scc = 0;
  while let Some(node) = order.pop() {
    if scc_ids[node] == UNVISITED {
      reverse_dfs(node, &rev_edges, next_scc, &mut scc_ids);
      next_scc += 1;
    }
  }

  scc_ids
}

fn type_rule_to_rust_def(
  rule: &TypeRule<'_>,
  comments: &CommentMap,
  doc: Vec<String>,
) -> Result<Option<RustTypeDef>, CodegenError> {
  let name = to_pascal_case(rule.name.ident);
  let ty = &rule.value;

  if ty.type_choices.len() > 1 {
    return Ok(Some(type_choices_to_enum(&name, ty, comments, doc)?));
  }

  if let Some(tc) = ty.type_choices.first() {
    let type1 = &tc.type1;
    match &type1.type2 {
      Type2::Map { group, .. } => {
        let fields = group_to_fields(group, comments)?;
        Ok(Some(RustTypeDef::Struct { name, fields, doc }))
      }
      Type2::Array { group, .. } => {
        let rust_type = array_group_to_type(group)?;
        Ok(Some(RustTypeDef::TypeAlias {
          name,
          target: rust_type,
          doc,
        }))
      }
      Type2::Typename { ident, .. } => {
        let target = cddl_ident_to_rust_type(ident.ident);
        Ok(Some(RustTypeDef::TypeAlias { name, target, doc }))
      }
      Type2::ParenthesizedType { pt, .. } => {
        if pt.type_choices.len() > 1 {
          return Ok(Some(type_choices_to_enum(&name, pt, comments, doc)?));
        }
        let target = type_to_rust_string(pt)?;
        Ok(Some(RustTypeDef::TypeAlias { name, target, doc }))
      }
      Type2::Unwrap { ident, .. } => {
        let target = to_pascal_case(ident.ident);
        Ok(Some(RustTypeDef::TypeAlias { name, target, doc }))
      }
      Type2::IntValue { .. }
      | Type2::UintValue { .. }
      | Type2::FloatValue { .. }
      | Type2::TextValue { .. } => {
        // Literal value rules (e.g. `color = "red"`) and numeric range rules
        // (e.g. `scale = 1..10`, whose lower bound is parsed as a value) are
        // emitted as type aliases to the underlying Rust type so that other
        // rules referencing them resolve to a defined type.
        let target = type1_to_rust_string(type1)?;
        Ok(Some(RustTypeDef::TypeAlias { name, target, doc }))
      }
      Type2::ChoiceFromInlineGroup { group, .. } => {
        let variants = group_to_enum_variants(group, comments)?;
        Ok(Some(RustTypeDef::Enum {
          name,
          variants,
          doc,
        }))
      }
      Type2::ChoiceFromGroup { ident, .. } => {
        let target = to_pascal_case(ident.ident);
        Ok(Some(RustTypeDef::TypeAlias { name, target, doc }))
      }
      Type2::TaggedData { t, .. } => {
        let target = type_to_rust_string(t)?;
        Ok(Some(RustTypeDef::TypeAlias { name, target, doc }))
      }
      _ => Ok(None),
    }
  } else {
    Ok(None)
  }
}

fn type_choices_to_enum(
  name: &str,
  ty: &Type<'_>,
  comments: &CommentMap,
  doc: Vec<String>,
) -> Result<RustTypeDef, CodegenError> {
  let mut variants = Vec::new();
  for tc in &ty.type_choices {
    let variant = type_choice_to_variant(tc, comments)?;
    variants.push(variant);
  }
  Ok(RustTypeDef::Enum {
    name: name.to_string(),
    variants,
    doc,
  })
}

fn type_choice_to_variant(
  tc: &TypeChoice<'_>,
  comments: &CommentMap,
) -> Result<RustEnumVariant, CodegenError> {
  let type1 = &tc.type1;
  let doc = comments.docs_for(type1.span.2);
  match &type1.type2 {
    Type2::Typename { ident, .. } => {
      let ident_str = ident.ident;
      let variant_name = to_pascal_case(ident_str);
      let inner = if is_prelude_type(ident_str) {
        Some(cddl_ident_to_rust_type(ident_str))
      } else {
        Some(variant_name.clone())
      };
      Ok(RustEnumVariant {
        name: variant_name,
        inner_type: inner,
        doc,
        rename: None,
      })
    }
    Type2::TextValue { value, .. } => Ok(RustEnumVariant {
      name: to_pascal_case(value),
      inner_type: None,
      doc,
      rename: Some(value.to_string()),
    }),
    Type2::IntValue { value, .. } => {
      let variant_name = if *value < 0 {
        format!("Neg{}", value.unsigned_abs())
      } else {
        format!("N{}", value)
      };
      Ok(RustEnumVariant {
        name: variant_name,
        inner_type: None,
        doc,
        rename: None,
      })
    }
    Type2::UintValue { value, .. } => Ok(RustEnumVariant {
      name: format!("N{}", value),
      inner_type: None,
      doc,
      rename: None,
    }),
    Type2::FloatValue { value, .. } => Ok(RustEnumVariant {
      name: format!("F{}", value.to_string().replace(['.', '-'], "_")),
      inner_type: None,
      doc,
      rename: None,
    }),
    Type2::Map { group, .. } => {
      let fields = group_to_fields(group, comments)?;
      let variant_name = if fields.is_empty() {
        "Empty".to_string()
      } else {
        fields
          .iter()
          .map(|f| to_pascal_case(&f.original_name))
          .collect::<Vec<_>>()
          .join("")
      };
      Ok(RustEnumVariant {
        name: variant_name,
        inner_type: None,
        doc,
        rename: None,
      })
    }
    Type2::Array { .. } => Ok(RustEnumVariant {
      name: "Array".to_string(),
      inner_type: Some("Vec<()>".to_string()),
      doc,
      rename: None,
    }),
    Type2::ParenthesizedType { pt, .. } => {
      let rust_type = type_to_rust_string(pt)?;
      let variant_name = to_pascal_case(&rust_type);
      Ok(RustEnumVariant {
        name: variant_name,
        inner_type: Some(rust_type),
        doc,
        rename: None,
      })
    }
    _ => Ok(RustEnumVariant {
      name: "Unknown".to_string(),
      inner_type: None,
      doc,
      rename: None,
    }),
  }
}

fn group_to_fields(
  group: &Group<'_>,
  comments: &CommentMap,
) -> Result<Vec<RustField>, CodegenError> {
  let mut fields = Vec::new();
  for gc in &group.group_choices {
    let gc_fields = group_choice_to_fields(gc, comments)?;
    fields.extend(gc_fields);
  }
  deduplicate_field_names(&mut fields);
  Ok(fields)
}

/// Give every field in a struct a unique name.
///
/// Wildcard entries (`* tstr => int`) have no key to name themselves after, so
/// they are all emitted as `entries`. A map may legitimately contain more than
/// one of them (`{ * tstr => int, * int => tstr }`), which would otherwise emit
/// the same field twice and fail to compile with "field `entries` specified
/// more than once". Repeats get a `_1`, `_2`, ... suffix.
///
/// Synthetic fields have no corresponding CDDL key, so their `original_name` is
/// renamed alongside the Rust field name. This avoids emitting a misleading
/// `#[serde(rename = "entries")]` for the second and subsequent wildcard fields.
///
/// See https://github.com/anweiss/cddl/issues/640
fn deduplicate_field_names(fields: &mut [RustField]) {
  let mut seen: std::collections::HashMap<String, usize> = std::collections::HashMap::new();

  for field in fields.iter_mut() {
    let base = field.name.clone();
    let count = seen.entry(base.clone()).or_insert(0);
    *count += 1;

    if *count > 1 {
      let unique = format!("{}_{}", base, *count - 1);
      if field.is_synthetic {
        field.original_name = unique.clone();
      }
      field.name = unique;
    }
  }
}

fn group_choice_to_fields(
  gc: &GroupChoice<'_>,
  comments: &CommentMap,
) -> Result<Vec<RustField>, CodegenError> {
  let mut fields = Vec::new();
  for (entry, _optional_comma) in &gc.group_entries {
    if let Some(mut entry_fields) = group_entry_to_fields(entry, comments)? {
      fields.append(&mut entry_fields);
    }
  }
  Ok(fields)
}

fn group_entry_to_fields(
  entry: &GroupEntry<'_>,
  comments: &CommentMap,
) -> Result<Option<Vec<RustField>>, CodegenError> {
  match entry {
    GroupEntry::ValueMemberKey { ge, .. } => {
      if let Some(mut field) = value_member_key_to_field(ge, comments)? {
        let ast_doc = ast_entry_doc(entry);
        if !ast_doc.is_empty() {
          field.doc = ast_doc;
        }
        Ok(Some(vec![field]))
      } else {
        Ok(None)
      }
    }
    GroupEntry::TypeGroupname { ge, .. } => {
      let ident = ge.name.ident;
      let rust_type = cddl_ident_to_rust_type(ident);
      let field_name = to_snake_case(ident);
      let is_optional = ge
        .occur
        .as_ref()
        .map(|o| matches!(o.occur, Occur::Optional { .. }))
        .unwrap_or(false);
      let ast_doc = ast_entry_doc(entry);
      let doc = if ast_doc.is_empty() {
        comments.docs_for(ge.name.span.2)
      } else {
        ast_doc
      };
      Ok(Some(vec![RustField {
        name: field_name,
        original_name: ident.to_string(),
        rust_type,
        is_optional,
        doc,
        is_boxed: false,
        is_synthetic: false,
        tag: tagged_prelude(ident),
      }]))
    }
    GroupEntry::InlineGroup { group, occur, .. } => {
      let is_optional = occur
        .as_ref()
        .map(|o| matches!(o.occur, Occur::Optional { .. }))
        .unwrap_or(false);
      let mut fields = group_to_fields(group, comments)?;
      if is_optional {
        for f in &mut fields {
          f.is_optional = true;
        }
      }
      Ok(Some(fields))
    }
  }
}

fn value_member_key_to_field(
  vmke: &ValueMemberKeyEntry<'_>,
  comments: &CommentMap,
) -> Result<Option<RustField>, CodegenError> {
  let doc = comments.docs_for(vmke_line(vmke));
  let (field_name, original_name) = match &vmke.member_key {
    Some(MemberKey::Bareword { ident, .. }) => {
      (to_snake_case(ident.ident), ident.ident.to_string())
    }
    Some(MemberKey::Value { value, .. }) => {
      let s = value.to_string();
      let s = s.trim_matches('"');
      (to_snake_case(s), s.to_string())
    }
    Some(MemberKey::Type1 { t1, .. }) => {
      let key_type = type1_to_rust_string(t1)?;
      let value_type = type_to_rust_string(&vmke.entry_type)?;
      let rust_type = format!("std::collections::HashMap<{}, {}>", key_type, value_type);
      return Ok(Some(RustField {
        name: "entries".to_string(),
        original_name: "entries".to_string(),
        rust_type,
        is_optional: false,
        doc,
        is_boxed: false,
        is_synthetic: true,
        tag: None,
      }));
    }
    None => {
      let rust_type = type_to_rust_string(&vmke.entry_type)?;
      return Ok(Some(RustField {
        name: "value".to_string(),
        original_name: "value".to_string(),
        rust_type,
        is_optional: false,
        doc,
        is_boxed: false,
        is_synthetic: true,
        tag: type_tagged_prelude(&vmke.entry_type),
      }));
    }
    _ => return Ok(None),
  };

  let is_optional = vmke
    .occur
    .as_ref()
    .map(|o| matches!(o.occur, Occur::Optional { .. }))
    .unwrap_or(false);

  let is_vec = vmke
    .occur
    .as_ref()
    .map(|o| is_vec_occurrence(&o.occur))
    .unwrap_or(false);

  let rust_type = type_to_rust_string(&vmke.entry_type)?;

  let final_type = if is_vec {
    format!("Vec<{}>", rust_type)
  } else {
    rust_type
  };

  Ok(Some(RustField {
    name: field_name,
    original_name,
    rust_type: final_type,
    is_optional,
    doc,
    is_boxed: false,
    is_synthetic: false,
    // Only an unwrapped occurrence carries the tag directly; `* tdate` becomes
    // Vec<String> and is handled as a follow-up.
    tag: if is_vec {
      None
    } else {
      type_tagged_prelude(&vmke.entry_type)
    },
  }))
}

/// If a CDDL type is exactly one of the tagged prelude identifiers, return it.
///
/// Only a bare, single-choice reference is matched. Type choices and types
/// nested inside containers keep their untagged representation.
fn type_tagged_prelude(t: &Type<'_>) -> Option<TaggedPrelude> {
  if t.type_choices.len() != 1 {
    return None;
  }

  match &t.type_choices[0].type1.type2 {
    Type2::Typename { ident, .. } => tagged_prelude(ident.ident),
    _ => None,
  }
}

/// Determine the 1-based source line for a value member key entry, preferring
/// the member key identifier/value and falling back to the entry type.
fn vmke_line(vmke: &ValueMemberKeyEntry<'_>) -> usize {
  match &vmke.member_key {
    Some(MemberKey::Bareword { ident, .. }) => ident.span.2,
    Some(MemberKey::Value { span, .. }) => span.2,
    Some(MemberKey::Type1 { span, .. }) => span.2,
    _ => vmke.entry_type.span.2,
  }
}

fn is_vec_occurrence(occur: &Occur) -> bool {
  match occur {
    Occur::ZeroOrMore { .. } | Occur::OneOrMore { .. } => true,
    Occur::Exact { upper: Some(u), .. } => *u > 1,
    _ => false,
  }
}

fn array_group_to_type(group: &Group<'_>) -> Result<String, CodegenError> {
  if group.group_choices.len() == 1 {
    let gc = &group.group_choices[0];
    if gc.group_entries.len() == 1 {
      let (entry, _) = &gc.group_entries[0];
      match entry {
        GroupEntry::ValueMemberKey { ge, .. } => {
          let element_type = type_to_rust_string(&ge.entry_type)?;
          return Ok(format!("Vec<{}>", element_type));
        }
        GroupEntry::TypeGroupname { ge, .. } => {
          let element_type = cddl_ident_to_rust_type(ge.name.ident);
          return Ok(format!("Vec<{}>", element_type));
        }
        _ => {}
      }
    }
    if !gc.group_entries.is_empty() {
      let mut types = Vec::new();
      for (entry, _) in &gc.group_entries {
        match entry {
          GroupEntry::ValueMemberKey { ge, .. } => {
            types.push(type_to_rust_string(&ge.entry_type)?);
          }
          GroupEntry::TypeGroupname { ge, .. } => {
            types.push(cddl_ident_to_rust_type(ge.name.ident));
          }
          _ => types.push("()".to_string()),
        }
      }
      if types.len() == 1 {
        return Ok(format!("Vec<{}>", types[0]));
      }
      return Ok(format!("({})", types.join(", ")));
    }
  }
  Ok("Vec<()>".to_string())
}

fn type_to_rust_string(ty: &Type<'_>) -> Result<String, CodegenError> {
  if ty.type_choices.len() == 1 {
    return type1_to_rust_string(&ty.type_choices[0].type1);
  }
  if ty.type_choices.len() == 2 {
    let (a, b) = (&ty.type_choices[0].type1, &ty.type_choices[1].type1);
    if is_null_type(&b.type2) {
      let inner = type1_to_rust_string(a)?;
      return Ok(format!("Option<{}>", inner));
    }
    if is_null_type(&a.type2) {
      let inner = type1_to_rust_string(b)?;
      return Ok(format!("Option<{}>", inner));
    }
  }
  Ok("serde_json::Value".to_string())
}

fn type1_to_rust_string(type1: &Type1<'_>) -> Result<String, CodegenError> {
  type2_to_rust_string(&type1.type2)
}

fn type2_to_rust_string(type2: &Type2<'_>) -> Result<String, CodegenError> {
  match type2 {
    Type2::Typename { ident, .. } => Ok(cddl_ident_to_rust_type(ident.ident)),
    Type2::Map { group, .. } => {
      if let Some(field) = detect_table_type(group)? {
        Ok(field)
      } else {
        Ok("serde_json::Value".to_string())
      }
    }
    Type2::Array { group, .. } => array_group_to_type(group),
    Type2::TextValue { .. } => Ok("String".to_string()),
    Type2::IntValue { .. } => Ok("i64".to_string()),
    Type2::UintValue { .. } => Ok("u64".to_string()),
    Type2::FloatValue { .. } => Ok("f64".to_string()),
    Type2::UTF8ByteString { .. } | Type2::B16ByteString { .. } | Type2::B64ByteString { .. } => {
      Ok("Vec<u8>".to_string())
    }
    Type2::ParenthesizedType { pt, .. } => type_to_rust_string(pt),
    Type2::Unwrap { ident, .. } => Ok(to_pascal_case(ident.ident)),
    Type2::TaggedData { t, .. } => type_to_rust_string(t),
    Type2::Any { .. } => Ok("serde_json::Value".to_string()),
    Type2::ChoiceFromInlineGroup { .. } | Type2::ChoiceFromGroup { .. } => {
      Ok("serde_json::Value".to_string())
    }
    Type2::DataMajorType { mt, .. } => Ok(major_type_to_rust(*mt)),
  }
}

fn detect_table_type(group: &Group<'_>) -> Result<Option<String>, CodegenError> {
  if group.group_choices.len() != 1 {
    return Ok(None);
  }
  let gc = &group.group_choices[0];
  if gc.group_entries.len() != 1 {
    return Ok(None);
  }
  let (entry, _) = &gc.group_entries[0];
  if let GroupEntry::ValueMemberKey { ge, .. } = entry {
    if let Some(MemberKey::Type1 { t1, .. }) = &ge.member_key {
      let key_type = type1_to_rust_string(t1)?;
      let value_type = type_to_rust_string(&ge.entry_type)?;
      return Ok(Some(format!(
        "std::collections::HashMap<{}, {}>",
        key_type, value_type
      )));
    }
  }
  Ok(None)
}

fn is_null_type(type2: &Type2<'_>) -> bool {
  matches!(type2, Type2::Typename { ident, .. } if ident.ident == "null" || ident.ident == "nil")
}

fn group_to_enum_variants(
  group: &Group<'_>,
  comments: &CommentMap,
) -> Result<Vec<RustEnumVariant>, CodegenError> {
  let mut variants = Vec::new();
  for gc in &group.group_choices {
    for (entry, _) in &gc.group_entries {
      let variant = group_entry_to_variant(entry, comments)?;
      variants.push(variant);
    }
  }
  Ok(variants)
}

fn group_entry_to_variant(
  entry: &GroupEntry<'_>,
  comments: &CommentMap,
) -> Result<RustEnumVariant, CodegenError> {
  match entry {
    GroupEntry::ValueMemberKey { ge, .. } => {
      let doc = comments.docs_for(vmke_line(ge));
      let variant_name = match &ge.member_key {
        Some(MemberKey::Bareword { ident, .. }) => to_pascal_case(ident.ident),
        Some(MemberKey::Value { value, .. }) => {
          let s = value.to_string();
          to_pascal_case(s.trim_matches('"'))
        }
        _ => "Variant".to_string(),
      };
      let inner = type_to_rust_string(&ge.entry_type)?;
      Ok(RustEnumVariant {
        name: variant_name,
        inner_type: Some(inner),
        doc,
        rename: None,
      })
    }
    GroupEntry::TypeGroupname { ge, .. } => {
      let doc = comments.docs_for(ge.name.span.2);
      let variant_name = to_pascal_case(ge.name.ident);
      let inner = cddl_ident_to_rust_type(ge.name.ident);
      Ok(RustEnumVariant {
        name: variant_name.clone(),
        inner_type: if is_prelude_type(ge.name.ident) {
          Some(inner)
        } else {
          Some(variant_name)
        },
        doc,
        rename: None,
      })
    }
    GroupEntry::InlineGroup { .. } => Ok(RustEnumVariant {
      name: "Group".to_string(),
      inner_type: None,
      doc: Vec::new(),
      rename: None,
    }),
  }
}

fn cddl_ident_to_rust_type(ident: &str) -> String {
  match ident {
    "bool" | "true" | "false" => "bool".to_string(),
    "uint" | "unsigned" => "u64".to_string(),
    "nint" => "i64".to_string(),
    "int" | "integer" => "i64".to_string(),
    "float16" | "float32" | "float64" | "float16-32" | "float32-64" | "float" => "f64".to_string(),
    "number" => "f64".to_string(),
    "tstr" | "text" => "String".to_string(),
    "bstr" | "bytes" => "Vec<u8>".to_string(),
    "null" | "nil" => "()".to_string(),
    "any" => "serde_json::Value".to_string(),
    "undefined" => "()".to_string(),
    "tdate" => "String".to_string(),
    "time" => "i64".to_string(),
    "uri" => "String".to_string(),
    "b64url" | "b64legacy" => "String".to_string(),
    "regexp" => "String".to_string(),
    "biguint" | "bignint" | "bigint" => "Vec<u8>".to_string(),
    _ => to_pascal_case(ident),
  }
}

fn major_type_to_rust(mt: u8) -> String {
  match mt {
    0 => "u64".to_string(),
    1 => "i64".to_string(),
    2 => "Vec<u8>".to_string(),
    3 => "String".to_string(),
    4 => "Vec<serde_json::Value>".to_string(),
    5 => "std::collections::HashMap<String, serde_json::Value>".to_string(),
    7 => "bool".to_string(),
    _ => "serde_json::Value".to_string(),
  }
}

fn is_prelude_type(ident: &str) -> bool {
  matches!(
    ident,
    "bool"
      | "true"
      | "false"
      | "uint"
      | "unsigned"
      | "nint"
      | "int"
      | "integer"
      | "float16"
      | "float32"
      | "float64"
      | "float16-32"
      | "float32-64"
      | "float"
      | "number"
      | "tstr"
      | "text"
      | "bstr"
      | "bytes"
      | "null"
      | "nil"
      | "any"
      | "undefined"
      | "tdate"
      | "time"
      | "uri"
      | "b64url"
      | "b64legacy"
      | "regexp"
      | "biguint"
      | "bignint"
      | "bigint"
  )
}

/// Name of the module holding the CBOR tag serde helpers for a whole file.
const TAG_HELPER_MOD: &str = "__cddl_tag_helpers";

/// Emit serde helper modules for each CBOR tag used by the generated types.
///
/// CDDL prelude types such as `tdate` and `uri` are CBOR tags wrapping a
/// simpler value (RFC 8610 Appendix D). The generated struct keeps the inner
/// Rust type (`String`), and these helpers apply the tag on the wire.
///
/// The helpers are format-aware: CBOR (and any other non-human-readable
/// format) gets the tag, while human-readable formats such as JSON keep the
/// bare value, so the same type round-trips through both. Deserialization
/// accepts an untagged value as well as a tagged one.
///
/// See https://github.com/anweiss/cddl/issues/639
fn render_tag_helpers(
  output: &mut String,
  mod_name: &str,
  tags: &[TaggedPrelude],
) -> Result<(), CodegenError> {
  if tags.is_empty() {
    return Ok(());
  }

  writeln!(output, "#[doc(hidden)]")?;
  writeln!(output, "#[allow(non_snake_case, unused_imports)]")?;
  writeln!(output, "pub mod {} {{", mod_name)?;

  for t in tags {
    let TaggedPrelude { ident, tag, inner } = *t;
    let module = ident.replace('-', "_");

    writeln!(
      output,
      "    /// serde helper for CDDL `{}` (CBOR tag {}).",
      ident, tag
    )?;
    writeln!(output, "    pub mod {} {{", module)?;
    writeln!(
      output,
      "        use serde::{{Deserialize, Deserializer, Serialize, Serializer}};"
    )?;
    writeln!(output)?;
    writeln!(
      output,
      "        pub fn serialize<S: Serializer>(v: &{}, s: S) -> Result<S::Ok, S::Error> {{",
      inner
    )?;
    writeln!(output, "            if s.is_human_readable() {{")?;
    writeln!(output, "                v.serialize(s)")?;
    writeln!(output, "            }} else {{")?;
    writeln!(
      output,
      "                ciborium::tag::Required::<&{}, {}>(v).serialize(s)",
      inner, tag
    )?;
    writeln!(output, "            }}")?;
    writeln!(output, "        }}")?;
    writeln!(output)?;
    writeln!(
      output,
      "        pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<{}, D::Error> {{",
      inner
    )?;
    writeln!(output, "            if d.is_human_readable() {{")?;
    writeln!(output, "                {}::deserialize(d)", inner)?;
    writeln!(output, "            }} else {{")?;
    writeln!(
      output,
      "                Ok(ciborium::tag::Accepted::<{}, {}>::deserialize(d)?.0)",
      inner, tag
    )?;
    writeln!(output, "            }}")?;
    writeln!(output, "        }}")?;
    writeln!(output, "    }}")?;
    writeln!(output)?;

    writeln!(
      output,
      "    /// serde helper for an optional CDDL `{}`.",
      ident
    )?;
    writeln!(output, "    pub mod {}_opt {{", module)?;
    writeln!(
      output,
      "        use serde::{{Deserialize, Deserializer, Serializer}};"
    )?;
    writeln!(output)?;
    writeln!(
      output,
      "        pub fn serialize<S: Serializer>(v: &Option<{}>, s: S) -> Result<S::Ok, S::Error> {{",
      inner
    )?;
    writeln!(output, "            match v {{")?;
    writeln!(output, "                Some(v) => {{")?;
    writeln!(output, "                    if s.is_human_readable() {{")?;
    writeln!(output, "                        s.serialize_some(v)")?;
    writeln!(output, "                    }} else {{")?;
    writeln!(
      output,
      "                        s.serialize_some(&ciborium::tag::Required::<&{}, {}>(v))",
      inner, tag
    )?;
    writeln!(output, "                    }}")?;
    writeln!(output, "                }}")?;
    writeln!(output, "                None => s.serialize_none(),")?;
    writeln!(output, "            }}")?;
    writeln!(output, "        }}")?;
    writeln!(output)?;
    writeln!(output, "        pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Option<{}>, D::Error> {{", inner)?;
    writeln!(output, "            if d.is_human_readable() {{")?;
    writeln!(
      output,
      "                Option::<{}>::deserialize(d)",
      inner
    )?;
    writeln!(output, "            }} else {{")?;
    writeln!(
      output,
      "                Ok(Option::<ciborium::tag::Accepted<{}, {}>>::deserialize(d)?.map(|a| a.0))",
      inner, tag
    )?;
    writeln!(output, "            }}")?;
    writeln!(output, "        }}")?;
    writeln!(output, "    }}")?;
  }

  writeln!(output, "}}")?;
  writeln!(output)?;
  Ok(())
}

/// Collect the distinct CBOR tags referenced by a set of generated types.
fn collect_tags(defs: &[RustTypeDef]) -> Vec<TaggedPrelude> {
  let mut tags: Vec<TaggedPrelude> = Vec::new();
  for def in defs {
    if let RustTypeDef::Struct { fields, .. } = def {
      for field in fields {
        if let Some(t) = field.tag {
          if !tags.iter().any(|existing| existing.ident == t.ident) {
            tags.push(t);
          }
        }
      }
    }
  }
  tags
}

fn render_type_defs(defs: &[RustTypeDef], opts: &CodegenOptions) -> Result<String, CodegenError> {
  let mut output = String::new();

  let tags = collect_tags(defs);
  render_tag_helpers(&mut output, TAG_HELPER_MOD, &tags)?;

  for (idx, def) in defs.iter().enumerate() {
    if idx > 0 {
      output.push('\n');
    }
    match def {
      RustTypeDef::Struct { name, fields, doc } => {
        render_struct(&mut output, name, fields, doc, TAG_HELPER_MOD, opts)?;
      }
      RustTypeDef::TypeAlias { name, target, doc } => {
        render_type_alias(&mut output, name, target, doc)?;
      }
      RustTypeDef::Enum {
        name,
        variants,
        doc,
      } => {
        render_enum(&mut output, name, variants, doc, opts)?;
      }
    }
  }

  Ok(output)
}

/// Write doc comment lines (each rendered as a Rust `///` line) with the given
/// indentation prefix.
fn render_doc(output: &mut String, doc: &[String], indent: &str) -> Result<(), CodegenError> {
  for line in doc {
    if line.is_empty() {
      writeln!(output, "{}///", indent)?;
    } else {
      writeln!(output, "{}/// {}", indent, line)?;
    }
  }
  Ok(())
}

/// Build a `serde_with` conversion spec for a generated field type, or `None`
/// when the type contains no byte strings.
///
/// CDDL `bstr` / `bytes` map to `Vec<u8>`, which serde encodes as an *array of
/// integers* rather than a CBOR byte string (major type 2). Annotating the
/// field with `#[serde_as(as = "serde_with::Bytes")]` restores the correct
/// encoding without changing the public type of the field.
///
/// The spec mirrors the shape of the Rust type, replacing each `Vec<u8>` with
/// `serde_with::Bytes` and every other position with `_`, so byte strings
/// nested inside containers are handled too:
///
/// | Rust type | Spec |
/// |---|---|
/// | `Vec<u8>` | `serde_with::Bytes` |
/// | `Vec<Vec<u8>>` | `Vec<serde_with::Bytes>` |
/// | `std::collections::HashMap<String, Vec<u8>>` | `std::collections::HashMap<_, serde_with::Bytes>` |
/// | `String` | `None` |
///
/// See https://github.com/anweiss/cddl/issues/638
fn serde_as_spec(rust_type: &str) -> Option<String> {
  let rust_type = rust_type.trim();

  if rust_type == "Vec<u8>" {
    return Some("serde_with::Bytes".to_string());
  }

  let open = rust_type.find('<')?;
  if !rust_type.ends_with('>') {
    return None;
  }

  let base = &rust_type[..open];
  let args = split_generic_args(&rust_type[open + 1..rust_type.len() - 1]);

  let specs = args
    .iter()
    .map(|arg| serde_as_spec(arg))
    .collect::<Vec<_>>();

  if specs.iter().all(Option::is_none) {
    return None;
  }

  let rendered = specs
    .into_iter()
    .map(|spec| spec.unwrap_or_else(|| "_".to_string()))
    .collect::<Vec<_>>()
    .join(", ");

  Some(format!("{}<{}>", base, rendered))
}

/// Split the inside of a generic argument list on top-level commas, ignoring
/// commas nested inside further generic arguments.
fn split_generic_args(args: &str) -> Vec<String> {
  let mut out = Vec::new();
  let mut depth = 0usize;
  let mut current = String::new();

  for c in args.chars() {
    match c {
      '<' => {
        depth += 1;
        current.push(c);
      }
      '>' => {
        depth = depth.saturating_sub(1);
        current.push(c);
      }
      ',' if depth == 0 => {
        out.push(current.trim().to_string());
        current.clear();
      }
      _ => current.push(c),
    }
  }

  if !current.trim().is_empty() {
    out.push(current.trim().to_string());
  }

  out
}

fn render_struct(
  output: &mut String,
  name: &str,
  fields: &[RustField],
  doc: &[String],
  tag_mod: &str,
  opts: &CodegenOptions,
) -> Result<(), CodegenError> {
  render_doc(output, doc, "")?;

  // Byte-string fields need serde_with to encode as CBOR major type 2 rather
  // than as an array of integers. The attribute must precede the derive.
  let field_specs = fields
    .iter()
    .map(|field| serde_as_spec(&field.rust_type))
    .collect::<Vec<_>>();

  if field_specs.iter().any(Option::is_some) {
    writeln!(output, "#[serde_with::serde_as]")?;
  }

  writeln!(
    output,
    "#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]"
  )?;
  if opts.non_exhaustive {
    writeln!(output, "#[non_exhaustive]")?;
  }
  writeln!(output, "pub struct {} {{", name)?;
  for (field, spec) in fields.iter().zip(field_specs) {
    render_doc(output, &field.doc, "    ")?;
    if let Some(spec) = spec {
      let spec = if field.is_boxed {
        format!("Box<{}>", spec)
      } else {
        spec
      };
      let spec = if field.is_optional {
        format!("Option<{}>", spec)
      } else {
        spec
      };
      writeln!(output, "    #[serde_as(as = \"{}\")]", spec)?;
    }
    if let Some(t) = field.tag {
      let module = t.ident.replace('-', "_");
      if field.is_optional {
        // serde drops the implicit Option default once a custom
        // deserializer is attached, so an absent key would otherwise fail.
        writeln!(
          output,
          "    #[serde(default, with = \"{}::{}_opt\")]",
          tag_mod, module
        )?;
      } else {
        writeln!(output, "    #[serde(with = \"{}::{}\")]", tag_mod, module)?;
      }
    }
    if field.name != field.original_name {
      writeln!(output, "    #[serde(rename = \"{}\")]", field.original_name)?;
    }
    if field.is_optional {
      writeln!(
        output,
        "    #[serde(skip_serializing_if = \"Option::is_none\")]"
      )?;
      let optional_inner_type = if field.is_boxed {
        format!("Box<{}>", field.rust_type)
      } else {
        field.rust_type.clone()
      };
      writeln!(
        output,
        "    pub {}: Option<{}>,",
        field.name, optional_inner_type
      )?;
    } else {
      let field_type = if field.is_boxed {
        format!("Box<{}>", field.rust_type)
      } else {
        field.rust_type.clone()
      };
      writeln!(output, "    pub {}: {},", field.name, field_type)?;
    }
  }
  writeln!(output, "}}")?;
  Ok(())
}

fn render_type_alias(
  output: &mut String,
  name: &str,
  target: &str,
  doc: &[String],
) -> Result<(), CodegenError> {
  render_doc(output, doc, "")?;
  writeln!(output, "pub type {} = {};", name, target)?;
  Ok(())
}

fn render_enum(
  output: &mut String,
  name: &str,
  variants: &[RustEnumVariant],
  doc: &[String],
  opts: &CodegenOptions,
) -> Result<(), CodegenError> {
  // An enum whose variants are all string literals (`a = "x" / "y"`) is a
  // closed set of strings on the wire. serde's untagged representation cannot
  // express that -- it deserializes a unit variant from `null` only -- so such
  // enums get a string-based representation instead.
  let string_enum = !variants.is_empty()
    && variants
      .iter()
      .all(|v| v.inner_type.is_none() && v.rename.is_some());

  if string_enum {
    return render_string_enum(output, name, variants, doc, opts);
  }

  render_doc(output, doc, "")?;
  writeln!(
    output,
    "#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]"
  )?;
  writeln!(output, "#[serde(untagged)]")?;
  if opts.non_exhaustive {
    writeln!(output, "#[non_exhaustive]")?;
  }
  writeln!(output, "pub enum {} {{", name)?;
  for variant in variants {
    render_doc(output, &variant.doc, "    ")?;
    if let Some(rename) = &variant.rename {
      writeln!(output, "    #[serde(rename = \"{}\")]", rename)?;
    }
    if let Some(inner) = &variant.inner_type {
      writeln!(output, "    {}({}),", variant.name, inner)?;
    } else {
      writeln!(output, "    {},", variant.name)?;
    }
  }
  if opts.other_variant && !variants.iter().any(|v| v.name == "Other") {
    render_other_variant_doc(output)?;
    writeln!(output, "    Other(String),")?;
  }
  writeln!(output, "}}")?;
  Ok(())
}

/// Render an enum whose variants are all string literals.
///
/// serde is implemented by hand rather than derived, because neither the
/// default nor the untagged representation round-trips a bare string through a
/// unit variant, and because `other_variant` needs a catch-all that serde has
/// no attribute for outside internally-tagged enums.
fn render_string_enum(
  output: &mut String,
  name: &str,
  variants: &[RustEnumVariant],
  doc: &[String],
  opts: &CodegenOptions,
) -> Result<(), CodegenError> {
  render_doc(output, doc, "")?;
  writeln!(output, "#[derive(Clone, Debug, PartialEq, Eq)]")?;
  if opts.non_exhaustive {
    writeln!(output, "#[non_exhaustive]")?;
  }
  writeln!(output, "pub enum {} {{", name)?;
  for variant in variants {
    render_doc(output, &variant.doc, "    ")?;
    writeln!(output, "    {},", variant.name)?;
  }
  if opts.other_variant {
    render_other_variant_doc(output)?;
    writeln!(output, "    Other(String),")?;
  }
  writeln!(output, "}}")?;
  writeln!(output)?;

  writeln!(output, "impl {} {{", name)?;
  writeln!(output, "    /// The string this value is represented by.")?;
  writeln!(output, "    pub fn as_str(&self) -> &str {{")?;
  writeln!(output, "        match self {{")?;
  for variant in variants {
    let text = variant.rename.as_deref().unwrap_or(&variant.name);
    writeln!(output, "            Self::{} => {:?},", variant.name, text)?;
  }
  if opts.other_variant {
    writeln!(output, "            Self::Other(v) => v.as_str(),")?;
  }
  writeln!(output, "        }}")?;
  writeln!(output, "    }}")?;
  writeln!(output, "}}")?;
  writeln!(output)?;

  writeln!(output, "impl serde::Serialize for {} {{", name)?;
  writeln!(
    output,
    "    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {{"
  )?;
  writeln!(output, "        s.serialize_str(self.as_str())")?;
  writeln!(output, "    }}")?;
  writeln!(output, "}}")?;
  writeln!(output)?;

  writeln!(output, "impl<'de> serde::Deserialize<'de> for {} {{", name)?;
  writeln!(
    output,
    "    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {{"
  )?;
  writeln!(
    output,
    "        let s = <String as serde::Deserialize>::deserialize(d)?;"
  )?;
  writeln!(output, "        Ok(match s.as_str() {{")?;
  for variant in variants {
    let text = variant.rename.as_deref().unwrap_or(&variant.name);
    writeln!(output, "            {:?} => Self::{},", text, variant.name)?;
  }
  if opts.other_variant {
    writeln!(output, "            _ => Self::Other(s),")?;
  } else {
    let expected = variants
      .iter()
      .map(|v| v.rename.as_deref().unwrap_or(&v.name))
      .collect::<Vec<_>>()
      .join(", ");
    writeln!(output, "            other => {{")?;
    writeln!(
      output,
      "                return Err(serde::de::Error::custom(format!("
    )?;
    writeln!(
      output,
      "                    \"unknown {} {{:?}}, expected one of: {}\", other",
      name, expected
    )?;
    writeln!(output, "                )));")?;
    writeln!(output, "            }}")?;
  }
  writeln!(output, "        }})")?;
  writeln!(output, "    }}")?;
  writeln!(output, "}}")?;
  Ok(())
}

/// Documentation for the generated `Other` catch-all variant.
fn render_other_variant_doc(output: &mut String) -> Result<(), CodegenError> {
  writeln!(
    output,
    "    /// A value not present in the CDDL definition."
  )?;
  writeln!(output, "    ///")?;
  writeln!(
    output,
    "    /// Lets a value added to the schema later deserialize instead of"
  )?;
  writeln!(output, "    /// failing.")?;
  Ok(())
}

pub(crate) fn to_pascal_case(s: &str) -> String {
  let mut result = String::with_capacity(s.len());
  for segment in s
    .split(|c: char| !c.is_alphanumeric())
    .filter(|segment| !segment.is_empty())
  {
    let mut chars = segment.chars();
    let Some(first) = chars.next() else {
      continue;
    };

    result.extend(first.to_uppercase());

    let rest: String = chars.collect();
    let mut has_alpha = false;
    let segment_is_all_caps = segment.chars().all(|c| {
      if c.is_alphabetic() {
        has_alpha = true;
        c.is_uppercase()
      } else {
        true
      }
    }) && has_alpha;
    if segment_is_all_caps {
      for c in rest.chars() {
        result.extend(c.to_lowercase());
      }
    } else {
      result.push_str(&rest);
    }
  }
  if result.is_empty() {
    return "Unknown".to_string();
  }
  result
}

fn to_snake_case(s: &str) -> String {
  let mut result = String::with_capacity(s.len() + 4);
  let mut prev_was_upper = false;
  let mut prev_was_separator = false;
  for (i, c) in s.chars().enumerate() {
    if c.is_uppercase() {
      if i > 0 && !prev_was_upper && !prev_was_separator {
        result.push('_');
      }
      result.push(c.to_lowercase().next().unwrap());
      prev_was_upper = true;
      prev_was_separator = false;
    } else if c.is_lowercase() || c.is_ascii_digit() {
      result.push(c);
      prev_was_upper = false;
      prev_was_separator = false;
    } else {
      if !result.is_empty() && !result.ends_with('_') {
        result.push('_');
      }
      prev_was_separator = true;
      prev_was_upper = false;
    }
  }
  while result.ends_with('_') {
    result.pop();
  }
  if result.is_empty() {
    return "value".to_string();
  }
  if result.chars().next().unwrap().is_ascii_digit() {
    result.insert(0, '_');
  }
  if is_rust_keyword(&result) {
    result.push('_');
  }
  result
}

fn is_rust_keyword(s: &str) -> bool {
  matches!(
    s,
    "as"
      | "async"
      | "await"
      | "break"
      | "const"
      | "continue"
      | "crate"
      | "dyn"
      | "else"
      | "enum"
      | "extern"
      | "false"
      | "fn"
      | "for"
      | "if"
      | "impl"
      | "in"
      | "let"
      | "loop"
      | "match"
      | "mod"
      | "move"
      | "mut"
      | "pub"
      | "ref"
      | "return"
      | "self"
      | "Self"
      | "static"
      | "struct"
      | "super"
      | "trait"
      | "true"
      | "type"
      | "unsafe"
      | "use"
      | "where"
      | "while"
      | "yield"
      | "box"
  )
}

#[cfg(test)]
mod tests {
  use super::*;
  use cddl::parser::cddl_from_str;

  fn gen(input: &str) -> String {
    let cddl = cddl_from_str(input, true).unwrap();
    generate_all_types(&cddl, input, &CodegenOptions::default()).unwrap()
  }

  #[test]
  fn test_to_pascal_case() {
    assert_eq!(to_pascal_case("my-type"), "MyType");
    assert_eq!(to_pascal_case("my_type"), "MyType");
    assert_eq!(to_pascal_case("person"), "Person");
    assert_eq!(to_pascal_case("http-request"), "HttpRequest");
    assert_eq!(to_pascal_case("EXCLUSION_RANGE-map"), "ExclusionRangeMap");
  }

  #[test]
  fn test_to_snake_case() {
    assert_eq!(to_snake_case("myType"), "my_type");
    assert_eq!(to_snake_case("my-type"), "my_type");
    assert_eq!(to_snake_case("type"), "type_");
    assert_eq!(to_snake_case("self"), "self_");
    assert_eq!(to_snake_case("dc:format"), "dc_format");
    assert_eq!(to_snake_case("informational_URI"), "informational_uri");
  }

  #[test]
  fn test_pascal_to_cddl_name() {
    assert_eq!(pascal_to_cddl_name("Person"), "person");
    assert_eq!(pascal_to_cddl_name("MyType"), "my-type");
    assert_eq!(pascal_to_cddl_name("HttpRequest"), "http-request");
  }

  #[test]
  fn test_simple_struct() {
    let result = gen(
      r#"
      person = {
        name: tstr,
        age: uint,
      }
    "#,
    );
    assert!(result.contains("pub struct Person"));
    assert!(result.contains("pub name: String,"));
    assert!(result.contains("pub age: u64,"));
  }

  #[test]
  fn test_optional_fields() {
    let result = gen(
      r#"
      person = {
        name: tstr,
        ? nickname: tstr,
      }
    "#,
    );
    assert!(result.contains("pub nickname: Option<String>,"));
    assert!(result.contains("skip_serializing_if"));
  }

  #[test]
  fn test_type_choices_enum() {
    let result = gen(r#"value = int / tstr / bool"#);
    assert!(result.contains("pub enum Value"));
    assert!(result.contains("Int(i64)"));
    assert!(result.contains("Tstr(String)"));
  }

  /// An enum of string literals is a closed set of strings on the wire.
  ///
  /// serde's untagged representation cannot express that -- it deserializes a
  /// unit variant from `null` only -- so these enums get hand-written impls
  /// that read and write a bare string.
  #[test]
  fn test_string_literal_choices_serialize_as_strings() {
    let result = gen(r#"action = "created" / "updated" / "deleted""#);
    assert!(result.contains("pub enum Action"));
    assert!(result.contains("Created,"));
    assert!(result.contains("Updated,"));
    assert!(result.contains("Deleted,"));

    assert!(!result.contains("#[serde(untagged)]"));
    assert!(result.contains("impl serde::Serialize for Action"));
    assert!(result.contains("impl<'de> serde::Deserialize<'de> for Action"));
    assert!(result.contains("Self::Created => \"created\","));
    assert!(result.contains("\"created\" => Self::Created,"));
  }

  /// A choice that mixes string literals with other types still needs the
  /// untagged representation.
  #[test]
  fn test_mixed_choices_stay_untagged() {
    let result = gen(r#"action = "created" / int"#);
    assert!(result.contains("#[serde(untagged)]"));
    assert!(result.contains("#[serde(rename = \"created\")]"));
  }

  #[test]
  fn test_non_exhaustive_option() {
    let cddl = cddl_from_str(r#"person = { name: tstr }"#, true).unwrap();
    let opts = CodegenOptions {
      non_exhaustive: true,
      ..Default::default()
    };
    let result = generate_all_types(&cddl, r#"person = { name: tstr }"#, &opts).unwrap();
    assert!(result.contains("#[non_exhaustive]"));
    assert!(result.contains("pub struct Person"));
  }

  #[test]
  fn test_any_type_option() {
    let src = r#"claim = { payload: any }"#;
    let cddl = cddl_from_str(src, true).unwrap();
    let opts = CodegenOptions {
      any_type: Some("ciborium::Value".to_string()),
      ..Default::default()
    };
    let result = generate_all_types(&cddl, src, &opts).unwrap();
    assert!(result.contains("ciborium::Value"));
    assert!(!result.contains("serde_json::Value"));
  }

  /// A rule-level substitution replaces references to the rule and drops its
  /// own definition.
  #[test]
  fn test_rule_substitution() {
    let src = r#"
      label = tstr
      claim = { name: label }
    "#;
    let cddl = cddl_from_str(src, true).unwrap();
    let mut substitutions = BTreeMap::new();
    substitutions.insert("label".to_string(), "crate::Label".to_string());
    let opts = CodegenOptions {
      substitutions,
      ..Default::default()
    };
    let result = generate_all_types(&cddl, src, &opts).unwrap();
    assert!(result.contains("pub name: crate::Label"));
    assert!(!result.contains("pub type Label"));
  }

  /// A field-level substitution names the rule and the field, and leaves other
  /// fields of the same type alone.
  #[test]
  fn test_field_substitution() {
    let src = r#"claim = { a: tstr, b: tstr }"#;
    let cddl = cddl_from_str(src, true).unwrap();
    let mut substitutions = BTreeMap::new();
    substitutions.insert("claim.a".to_string(), "crate::Special".to_string());
    let opts = CodegenOptions {
      substitutions,
      ..Default::default()
    };
    let result = generate_all_types(&cddl, src, &opts).unwrap();
    assert!(result.contains("pub a: crate::Special"));
    assert!(result.contains("pub b: String"));
  }

  /// Substitution matches whole identifiers, so a rule named `Label` does not
  /// also rewrite `LabelSet`.
  #[test]
  fn test_substitution_matches_whole_identifiers() {
    let src = r#"
      label = tstr
      label-set = [* label]
      claim = { a: label, b: label-set }
    "#;
    let cddl = cddl_from_str(src, true).unwrap();
    let mut substitutions = BTreeMap::new();
    substitutions.insert("label".to_string(), "crate::Label".to_string());
    let opts = CodegenOptions {
      substitutions,
      ..Default::default()
    };
    let result = generate_all_types(&cddl, src, &opts).unwrap();
    assert!(result.contains("pub a: crate::Label"));
    assert!(result.contains("pub b: LabelSet"));
    assert!(result.contains("Vec<crate::Label>"));
  }

  #[test]
  fn test_other_variant_option() {
    let src = r#"action = "created" / "updated""#;
    let cddl = cddl_from_str(src, true).unwrap();
    let opts = CodegenOptions {
      other_variant: true,
      ..Default::default()
    };
    let result = generate_all_types(&cddl, src, &opts).unwrap();
    assert!(result.contains("Other(String),"));
    assert!(result.contains("_ => Self::Other(s),"));
  }

  #[test]
  fn test_type_alias() {
    let result = gen(r#"name = tstr"#);
    assert!(result.contains("pub type Name = String;"));
  }

  #[test]
  fn test_socket_plug_alternates_merge_into_enum() {
    let result = gen(
      r#"
      $foo /= int
      $foo /= tstr
    "#,
    );
    // The socket/plug alternates should be merged into a single enum instead
    // of producing duplicate `pub type Foo = ...;` definitions.
    assert_eq!(result.matches("pub enum Foo").count(), 1);
    assert!(!result.contains("pub type Foo"));
    assert!(result.contains("#[serde(untagged)]"));
    assert!(result.contains("Int(i64)"));
    assert!(result.contains("Tstr(String)"));
  }

  #[test]
  fn test_single_type_generation() {
    let cddl = cddl_from_str(
      r#"
      address = { street: tstr, city: tstr }
      person = { name: tstr, home: address }
    "#,
      true,
    )
    .unwrap();
    let result = generate_single_type(
      &cddl,
      "Person",
      None,
      r#"
      address = { street: tstr, city: tstr }
      person = { name: tstr, home: address }
    "#,
      &CodegenOptions::default(),
    )
    .unwrap();
    assert!(result.contains("pub struct Person"));
    assert!(!result.contains("pub struct Address"));
  }

  #[test]
  fn test_single_type_with_output_name_override() {
    let cddl = cddl_from_str(
      r#"
      address = { street: tstr, city: tstr }
    "#,
      true,
    )
    .unwrap();
    let result = generate_single_type(
      &cddl,
      "Address",
      Some("Addr"),
      r#"
      address = { street: tstr, city: tstr }
    "#,
      &CodegenOptions::default(),
    )
    .unwrap();
    assert!(result.contains("pub struct Addr"));
    assert!(!result.contains("pub struct Address"));
  }

  #[test]
  fn test_hyphenated_names_serde_rename() {
    let result = gen(
      r#"
      my-record = {
        first-name: tstr,
      }
    "#,
    );
    assert!(result.contains("#[serde(rename = \"first-name\")]"));
    assert!(result.contains("pub first_name: String,"));
  }

  #[test]
  fn test_repeated_wildcard_fields_get_unique_synthetic_names() {
    let result = gen(
      r#"
      mixed = {
        * tstr => int,
        * int => tstr,
      }
    "#,
    );

    assert!(result.contains("pub entries: std::collections::HashMap<String, i64>,"));
    assert!(result.contains("pub entries_1: std::collections::HashMap<i64, String>,"));
    assert!(!result.contains("#[serde(rename = \"entries\")]"));
  }

  #[test]
  fn test_real_duplicate_field_preserves_original_name() {
    let result = gen(
      r#"
      record = {
        entries: tstr,
        "entries": int,
      }
    "#,
    );

    assert!(result.contains("pub entries: String,"));
    assert!(result.contains("#[serde(rename = \"entries\")]"));
    assert!(result.contains("pub entries_1: i64,"));
  }

  #[test]
  fn test_nullable_type() {
    let result = gen(
      r#"
      record = {
        value: tstr / null,
      }
    "#,
    );
    assert!(result.contains("pub value: Option<String>,"));
  }

  #[test]
  fn test_keyword_escaping() {
    let result = gen(
      r#"
      my-record = {
        type: tstr,
      }
    "#,
    );
    assert!(result.contains("pub type_: String,"));
    assert!(result.contains("#[serde(rename = \"type\")]"));
  }

  #[test]
  fn test_symbolic_field_name_sanitization() {
    let result = gen(
      r#"
      record = {
        "dc:format": tstr,
      }
    "#,
    );
    assert!(result.contains("pub dc_format: String,"));
    assert!(result.contains("#[serde(rename = \"dc:format\")]"));
  }

  #[test]
  fn test_upper_snake_field_name_sanitization() {
    let result = gen(
      r#"
      record = {
        "informational_URI": tstr,
      }
    "#,
    );
    assert!(result.contains("pub informational_uri: String,"));
    assert!(result.contains("#[serde(rename = \"informational_URI\")]"));
  }

  #[test]
  fn test_upper_snake_rule_name_to_pascal() {
    let result = gen(
      r#"
      EXCLUSION_RANGE-map = {
        start: uint,
        end: uint,
      }
    "#,
    );
    assert!(result.contains("pub struct ExclusionRangeMap"));
  }

  #[test]
  fn test_leading_comment_becomes_struct_doc() {
    let result = gen(
      r#"; A person record.
person = {
  name: tstr,
}
"#,
    );
    assert!(result.contains("/// A person record."));
    // The doc comment precedes the derive attribute and struct.
    let doc_idx = result.find("/// A person record.").unwrap();
    let struct_idx = result.find("pub struct Person").unwrap();
    assert!(doc_idx < struct_idx);
  }

  #[test]
  fn test_multiline_leading_comments_preserved() {
    let result = gen(
      r#"; First line.
; Second line.
person = {
  name: tstr,
}
"#,
    );
    assert!(result.contains("/// First line."));
    assert!(result.contains("/// Second line."));
  }

  #[test]
  fn test_field_leading_and_trailing_comments() {
    let result = gen(
      r#"person = {
  ; The person's name.
  name: tstr,
  age: uint, ; Age in years.
}
"#,
    );
    assert!(result.contains("/// The person's name."));
    assert!(result.contains("/// Age in years."));
  }

  #[test]
  fn test_nested_struct_field_comments_from_ast() {
    // Comments nested inside CDDL structures are preserved via the AST, not by
    // scanning raw source lines.
    let result = gen(
      r#"person = {
  ; Mailing address.
  address: address,
}

address = {
  ; Street line.
  street: tstr,
  city: tstr, ; City name.
}
"#,
    );
    assert!(result.contains("/// Mailing address."));
    assert!(result.contains("/// Street line."));
    assert!(result.contains("/// City name."));
  }

  #[test]
  fn test_type_alias_doc_comment() {
    let result = gen(
      r#"; A numeric score.
score = uint
"#,
    );
    assert!(result.contains("/// A numeric score."));
    let doc_idx = result.find("/// A numeric score.").unwrap();
    let alias_idx = result.find("pub type Score").unwrap();
    assert!(doc_idx < alias_idx);
  }

  #[test]
  fn test_enum_doc_comment() {
    let result = gen(
      r#"; Status of an entity.
status = "active" / "inactive"
"#,
    );
    assert!(result.contains("/// Status of an entity."));
    let doc_idx = result.find("/// Status of an entity.").unwrap();
    let enum_idx = result.find("pub enum Status").unwrap();
    assert!(doc_idx < enum_idx);
  }

  #[test]
  fn test_semicolon_in_string_not_treated_as_comment() {
    let result = gen(
      r#"record = {
  sep: "a;b",
}
"#,
    );
    // No spurious doc comment should be generated from the ';' inside the string.
    assert!(!result.contains("///"));
    assert!(result.contains("pub sep: String,"));
  }

  #[test]
  fn test_trailing_rule_comment_becomes_doc() {
    let result = gen(
      r#"score = uint ; The score value.
"#,
    );
    assert!(result.contains("/// The score value."));
  }

  #[test]
  fn test_mutually_recursive_structs_are_boxed_deterministically() {
    let result = gen(
      r#"
      node = {
        ? "name": tstr,
        ? "child": child,
      }

      child = {
        ? "value": int,
        ? "parent": node,
      }
    "#,
    );
    assert!(result.contains("pub child: Option<Box<Child>>,"));
    assert!(result.contains("pub parent: Option<Node>,"));
  }

  #[test]
  fn test_self_recursive_struct_is_boxed() {
    let result = gen(
      r#"
      node = {
        ? "child": node,
      }
    "#,
    );
    assert!(result.contains("pub child: Option<Box<Node>>,"));
  }
}
