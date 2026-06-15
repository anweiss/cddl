//! Internal code generation logic for converting CDDL AST to Rust source code.

use cddl::ast::{
  Group, GroupChoice, GroupEntry, MemberKey, Occur, Rule, Type, Type1, Type2, TypeChoice, TypeRule,
  ValueMemberKeyEntry, CDDL,
};
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
}

/// A variant within a generated Rust enum.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct RustEnumVariant {
  pub name: String,
  pub inner_type: Option<String>,
  pub doc: Vec<String>,
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

    if line >= 2 {
      let mut l = line - 1;
      while l >= 1 {
        match self.pure.get(l).and_then(|c| c.as_ref()) {
          Some(c) => {
            docs.push(c.clone());
            l -= 1;
          }
          None => break,
        }
      }
      docs.reverse();
    }

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

/// Generate Rust source code for all rules in a parsed CDDL AST.
pub(crate) fn generate_all_types(cddl: &CDDL<'_>, source: &str) -> Result<String, CodegenError> {
  let comments = CommentMap::new(source);
  let type_defs = collect_type_defs(cddl, &comments)?;
  render_type_defs(&type_defs)
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
) -> Result<String, CodegenError> {
  let comments = CommentMap::new(source);
  let type_defs = collect_type_defs(cddl, &comments)?;
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
      render_struct(&mut output, emit_name, fields, doc)?;
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
      render_enum(&mut output, emit_name, variants, doc)?;
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
  let mut defs = Vec::new();
  for rule in &cddl.rules {
    match rule {
      Rule::Type {
        rule: type_rule, ..
      } => {
        let doc = comments.docs_for(type_rule.name.span.2);
        if let Some(def) = type_rule_to_rust_def(type_rule, comments, doc)? {
          defs.push(def);
        }
      }
      Rule::Group {
        rule: group_rule, ..
      } => {
        let name = to_pascal_case(group_rule.name.ident);
        let doc = comments.docs_for(group_rule.name.span.2);
        if let Some(fields) = group_entry_to_fields(&group_rule.entry, comments)? {
          defs.push(RustTypeDef::Struct { name, fields, doc });
        }
      }
    }
  }
  Ok(defs)
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
      | Type2::TextValue { .. } => Ok(None),
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
      })
    }
    Type2::TextValue { value, .. } => Ok(RustEnumVariant {
      name: to_pascal_case(value),
      inner_type: None,
      doc,
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
      })
    }
    Type2::UintValue { value, .. } => Ok(RustEnumVariant {
      name: format!("N{}", value),
      inner_type: None,
      doc,
    }),
    Type2::FloatValue { value, .. } => Ok(RustEnumVariant {
      name: format!("F{}", value.to_string().replace(['.', '-'], "_")),
      inner_type: None,
      doc,
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
      })
    }
    Type2::Array { .. } => Ok(RustEnumVariant {
      name: "Array".to_string(),
      inner_type: Some("Vec<()>".to_string()),
      doc,
    }),
    Type2::ParenthesizedType { pt, .. } => {
      let rust_type = type_to_rust_string(pt)?;
      let variant_name = to_pascal_case(&rust_type);
      Ok(RustEnumVariant {
        name: variant_name,
        inner_type: Some(rust_type),
        doc,
      })
    }
    _ => Ok(RustEnumVariant {
      name: "Unknown".to_string(),
      inner_type: None,
      doc,
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
  Ok(fields)
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
      if let Some(field) = value_member_key_to_field(ge, comments)? {
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
      let doc = comments.docs_for(ge.name.span.2);
      Ok(Some(vec![RustField {
        name: field_name,
        original_name: ident.to_string(),
        rust_type,
        is_optional,
        doc,
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
  }))
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
      })
    }
    GroupEntry::InlineGroup { .. } => Ok(RustEnumVariant {
      name: "Group".to_string(),
      inner_type: None,
      doc: Vec::new(),
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

fn render_type_defs(defs: &[RustTypeDef]) -> Result<String, CodegenError> {
  let mut output = String::new();

  for (idx, def) in defs.iter().enumerate() {
    if idx > 0 {
      output.push('\n');
    }
    match def {
      RustTypeDef::Struct { name, fields, doc } => {
        render_struct(&mut output, name, fields, doc)?;
      }
      RustTypeDef::TypeAlias { name, target, doc } => {
        render_type_alias(&mut output, name, target, doc)?;
      }
      RustTypeDef::Enum {
        name,
        variants,
        doc,
      } => {
        render_enum(&mut output, name, variants, doc)?;
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

fn render_struct(
  output: &mut String,
  name: &str,
  fields: &[RustField],
  doc: &[String],
) -> Result<(), CodegenError> {
  render_doc(output, doc, "")?;
  writeln!(
    output,
    "#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]"
  )?;
  writeln!(output, "pub struct {} {{", name)?;
  for field in fields {
    render_doc(output, &field.doc, "    ")?;
    if field.name != field.original_name {
      writeln!(output, "    #[serde(rename = \"{}\")]", field.original_name)?;
    }
    if field.is_optional {
      writeln!(
        output,
        "    #[serde(skip_serializing_if = \"Option::is_none\")]"
      )?;
      writeln!(
        output,
        "    pub {}: Option<{}>,",
        field.name, field.rust_type
      )?;
    } else {
      writeln!(output, "    pub {}: {},", field.name, field.rust_type)?;
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
) -> Result<(), CodegenError> {
  render_doc(output, doc, "")?;
  writeln!(
    output,
    "#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]"
  )?;
  writeln!(output, "#[serde(untagged)]")?;
  writeln!(output, "pub enum {} {{", name)?;
  for variant in variants {
    render_doc(output, &variant.doc, "    ")?;
    if let Some(inner) = &variant.inner_type {
      writeln!(output, "    {}({}),", variant.name, inner)?;
    } else {
      writeln!(output, "    {},", variant.name)?;
    }
  }
  writeln!(output, "}}")?;
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
    generate_all_types(&cddl, input).unwrap()
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

  #[test]
  fn test_type_alias() {
    let result = gen(r#"name = tstr"#);
    assert!(result.contains("pub type Name = String;"));
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
}
