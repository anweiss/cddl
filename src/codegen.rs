//! Code generation module for deriving Rust types from CDDL definitions.
//!
//! This module provides functionality to generate Rust source code (struct and
//! enum definitions) from CDDL type definitions. The generated code can be used
//! together with Serde's `Serialize` and `Deserialize` derives for automatic
//! encoding/decoding of CBOR and JSON data structures.
//!
//! # Example
//!
//! ```rust
//! use cddl::codegen::generate_rust_code;
//!
//! let cddl_input = r#"
//!   person = {
//!     name: tstr,
//!     age: uint,
//!     address: tstr,
//!   }
//! "#;
//!
//! let rust_code = generate_rust_code(cddl_input).unwrap();
//! assert!(rust_code.contains("pub struct Person"));
//! assert!(rust_code.contains("pub name: String"));
//! assert!(rust_code.contains("pub age: u64"));
//! assert!(rust_code.contains("pub address: String"));
//! ```

use crate::ast::{
  Group, GroupChoice, GroupEntry, MemberKey, Occur, Rule, Type, Type1, Type2, TypeChoice, TypeRule,
  ValueMemberKeyEntry, CDDL,
};
use crate::parser::cddl_from_str;
use std::fmt::Write;

/// Errors that can occur during code generation.
#[derive(Debug)]
pub enum CodegenError {
  /// CDDL parsing failed.
  ParseError(String),
  /// Code generation encountered an unsupported CDDL construct.
  UnsupportedConstruct(String),
  /// Formatting error.
  FmtError(std::fmt::Error),
}

impl std::fmt::Display for CodegenError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      CodegenError::ParseError(msg) => write!(f, "CDDL parse error: {}", msg),
      CodegenError::UnsupportedConstruct(msg) => {
        write!(f, "unsupported CDDL construct: {}", msg)
      }
      CodegenError::FmtError(e) => write!(f, "formatting error: {}", e),
    }
  }
}

impl std::error::Error for CodegenError {}

impl From<std::fmt::Error> for CodegenError {
  fn from(e: std::fmt::Error) -> Self {
    CodegenError::FmtError(e)
  }
}

/// A generated Rust type definition.
#[derive(Debug, Clone, PartialEq)]
enum RustTypeDef {
  /// A struct with named fields.
  Struct {
    name: String,
    fields: Vec<RustField>,
  },
  /// A type alias (`type Foo = Bar;`).
  TypeAlias { name: String, target: String },
  /// An enum with variant choices.
  Enum {
    name: String,
    variants: Vec<RustEnumVariant>,
  },
}

/// A field within a generated Rust struct.
#[derive(Debug, Clone, PartialEq)]
struct RustField {
  /// Snake-case field name
  name: String,
  /// Original CDDL key name (for serde rename)
  original_name: String,
  /// Rust type string
  rust_type: String,
  /// Whether the field is optional
  is_optional: bool,
}

/// A variant within a generated Rust enum.
#[derive(Debug, Clone, PartialEq)]
struct RustEnumVariant {
  /// PascalCase variant name
  name: String,
  /// Optional inner type (for newtype variants)
  inner_type: Option<String>,
}

/// Generate Rust source code from a CDDL input string.
///
/// Parses the CDDL input and generates Rust struct and enum definitions
/// for each rule. Type rules whose values are map types (`{ ... }`) become
/// structs with named fields. Type rules with multiple type choices become
/// enums. Simple type aliases are also generated.
///
/// # Arguments
///
/// * `cddl_input` - A string slice containing valid CDDL definitions.
///
/// # Returns
///
/// A `Result` containing the generated Rust source code as a `String`, or
/// a `CodegenError` if parsing or code generation fails.
///
/// # Example
///
/// ```rust
/// use cddl::codegen::generate_rust_code;
///
/// let cddl_input = r#"
///   message = {
///     id: uint,
///     body: tstr,
///     ? timestamp: float,
///   }
/// "#;
///
/// let rust_code = generate_rust_code(cddl_input).unwrap();
/// assert!(rust_code.contains("pub struct Message"));
/// ```
pub fn generate_rust_code(cddl_input: &str) -> Result<String, CodegenError> {
  let cddl =
    cddl_from_str(cddl_input, true).map_err(|e| CodegenError::ParseError(e.to_string()))?;
  let type_defs = collect_type_defs(&cddl)?;
  render_type_defs(&type_defs)
}

/// Collect all type definitions from the parsed CDDL AST.
fn collect_type_defs(cddl: &CDDL<'_>) -> Result<Vec<RustTypeDef>, CodegenError> {
  let mut defs = Vec::new();

  for rule in &cddl.rules {
    match rule {
      Rule::Type {
        rule: type_rule, ..
      } => {
        if let Some(def) = type_rule_to_rust_def(type_rule)? {
          defs.push(def);
        }
      }
      Rule::Group {
        rule: group_rule, ..
      } => {
        // Group rules define reusable groups; generate a struct from the entry
        let name = to_pascal_case(group_rule.name.ident);
        if let Some(fields) = group_entry_to_fields(&group_rule.entry)? {
          defs.push(RustTypeDef::Struct { name, fields });
        }
      }
    }
  }

  Ok(defs)
}

/// Convert a type rule to a Rust type definition.
fn type_rule_to_rust_def(rule: &TypeRule<'_>) -> Result<Option<RustTypeDef>, CodegenError> {
  let name = to_pascal_case(rule.name.ident);
  let ty = &rule.value;

  // Multiple type choices → enum
  if ty.type_choices.len() > 1 {
    return Ok(Some(type_choices_to_enum(&name, ty)?));
  }

  // Single type choice
  if let Some(tc) = ty.type_choices.first() {
    let type1 = &tc.type1;
    match &type1.type2 {
      // Map → struct
      Type2::Map { group, .. } => {
        let fields = group_to_fields(group)?;
        Ok(Some(RustTypeDef::Struct { name, fields }))
      }
      // Array → type alias to Vec<T>
      Type2::Array { group, .. } => {
        let rust_type = array_group_to_type(group)?;
        Ok(Some(RustTypeDef::TypeAlias {
          name,
          target: rust_type,
        }))
      }
      // Typename reference → type alias
      Type2::Typename { ident, .. } => {
        let target = cddl_ident_to_rust_type(ident.ident);
        Ok(Some(RustTypeDef::TypeAlias { name, target }))
      }
      // Parenthesized type → recurse
      Type2::ParenthesizedType { pt, .. } => {
        if pt.type_choices.len() > 1 {
          return Ok(Some(type_choices_to_enum(&name, pt)?));
        }
        let target = type_to_rust_string(pt)?;
        Ok(Some(RustTypeDef::TypeAlias { name, target }))
      }
      // Unwrap → type alias to the unwrapped type
      Type2::Unwrap { ident, .. } => {
        let target = to_pascal_case(ident.ident);
        Ok(Some(RustTypeDef::TypeAlias { name, target }))
      }
      // Literal values – skip, as they don't produce useful Rust types
      Type2::IntValue { .. }
      | Type2::UintValue { .. }
      | Type2::FloatValue { .. }
      | Type2::TextValue { .. } => Ok(None),
      // Choice from group → enum
      Type2::ChoiceFromInlineGroup { group, .. } => {
        let variants = group_to_enum_variants(group)?;
        Ok(Some(RustTypeDef::Enum { name, variants }))
      }
      Type2::ChoiceFromGroup { ident, .. } => {
        let target = to_pascal_case(ident.ident);
        Ok(Some(RustTypeDef::TypeAlias { name, target }))
      }
      // Tagged data → type alias to inner type
      Type2::TaggedData { t, .. } => {
        let target = type_to_rust_string(t)?;
        Ok(Some(RustTypeDef::TypeAlias { name, target }))
      }
      _ => Ok(None),
    }
  } else {
    Ok(None)
  }
}

/// Convert multiple type choices into an enum.
fn type_choices_to_enum(name: &str, ty: &Type<'_>) -> Result<RustTypeDef, CodegenError> {
  let mut variants = Vec::new();
  for tc in &ty.type_choices {
    let variant = type_choice_to_variant(tc)?;
    variants.push(variant);
  }
  Ok(RustTypeDef::Enum {
    name: name.to_string(),
    variants,
  })
}

/// Convert a single type choice into an enum variant.
fn type_choice_to_variant(tc: &TypeChoice<'_>) -> Result<RustEnumVariant, CodegenError> {
  let type1 = &tc.type1;
  match &type1.type2 {
    Type2::Typename { ident, .. } => {
      let ident_str = ident.ident;
      let variant_name = to_pascal_case(ident_str);
      // For prelude types, include them as newtype variants
      let inner = if is_prelude_type(ident_str) {
        Some(cddl_ident_to_rust_type(ident_str))
      } else {
        Some(variant_name.clone())
      };
      Ok(RustEnumVariant {
        name: variant_name,
        inner_type: inner,
      })
    }
    Type2::TextValue { value, .. } => {
      let variant_name = to_pascal_case(value);
      Ok(RustEnumVariant {
        name: variant_name,
        inner_type: None,
      })
    }
    Type2::IntValue { value, .. } => {
      let variant_name = if *value < 0 {
        format!("Neg{}", value.unsigned_abs())
      } else {
        format!("N{}", value)
      };
      Ok(RustEnumVariant {
        name: variant_name,
        inner_type: None,
      })
    }
    Type2::UintValue { value, .. } => Ok(RustEnumVariant {
      name: format!("N{}", value),
      inner_type: None,
    }),
    Type2::FloatValue { value, .. } => Ok(RustEnumVariant {
      name: format!("F{}", value.to_string().replace(['.', '-'], "_")),
      inner_type: None,
    }),
    Type2::Map { group, .. } => {
      // Inline map in a choice — generate a variant name from index
      let fields = group_to_fields(group)?;
      // Use field names to create a descriptive variant name
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
      })
    }
    Type2::Array { .. } => Ok(RustEnumVariant {
      name: "Array".to_string(),
      inner_type: Some("Vec<()>".to_string()),
    }),
    Type2::ParenthesizedType { pt, .. } => {
      let rust_type = type_to_rust_string(pt)?;
      let variant_name = to_pascal_case(&rust_type);
      Ok(RustEnumVariant {
        name: variant_name,
        inner_type: Some(rust_type),
      })
    }
    _ => Ok(RustEnumVariant {
      name: "Unknown".to_string(),
      inner_type: None,
    }),
  }
}

/// Convert a group to struct fields.
fn group_to_fields(group: &Group<'_>) -> Result<Vec<RustField>, CodegenError> {
  let mut fields = Vec::new();
  for gc in &group.group_choices {
    let gc_fields = group_choice_to_fields(gc)?;
    fields.extend(gc_fields);
  }
  Ok(fields)
}

/// Convert a group choice to struct fields.
fn group_choice_to_fields(gc: &GroupChoice<'_>) -> Result<Vec<RustField>, CodegenError> {
  let mut fields = Vec::new();
  for (entry, _optional_comma) in &gc.group_entries {
    if let Some(mut entry_fields) = group_entry_to_fields(entry)? {
      fields.append(&mut entry_fields);
    }
  }
  Ok(fields)
}

/// Convert a group entry to struct fields.
fn group_entry_to_fields(entry: &GroupEntry<'_>) -> Result<Option<Vec<RustField>>, CodegenError> {
  match entry {
    GroupEntry::ValueMemberKey { ge, .. } => {
      if let Some(field) = value_member_key_to_field(ge)? {
        Ok(Some(vec![field]))
      } else {
        Ok(None)
      }
    }
    GroupEntry::TypeGroupname { ge, .. } => {
      // A type/group name reference used as a group entry (flattening)
      let ident = ge.name.ident;
      let rust_type = cddl_ident_to_rust_type(ident);
      let field_name = to_snake_case(ident);
      let is_optional = ge
        .occur
        .as_ref()
        .map(|o| matches!(o.occur, Occur::Optional { .. }))
        .unwrap_or(false);
      Ok(Some(vec![RustField {
        name: field_name,
        original_name: ident.to_string(),
        rust_type,
        is_optional,
      }]))
    }
    GroupEntry::InlineGroup { group, occur, .. } => {
      // Inline group — flatten its fields into the parent
      let is_optional = occur
        .as_ref()
        .map(|o| matches!(o.occur, Occur::Optional { .. }))
        .unwrap_or(false);
      let mut fields = group_to_fields(group)?;
      if is_optional {
        for f in &mut fields {
          f.is_optional = true;
        }
      }
      Ok(Some(fields))
    }
  }
}

/// Convert a ValueMemberKeyEntry to a RustField.
fn value_member_key_to_field(
  vmke: &ValueMemberKeyEntry<'_>,
) -> Result<Option<RustField>, CodegenError> {
  let (field_name, original_name) = match &vmke.member_key {
    Some(MemberKey::Bareword { ident, .. }) => {
      (to_snake_case(ident.ident), ident.ident.to_string())
    }
    Some(MemberKey::Value { value, .. }) => {
      let s = value.to_string();
      // Strip surrounding quotes
      let s = s.trim_matches('"');
      (to_snake_case(s), s.to_string())
    }
    Some(MemberKey::Type1 { t1, .. }) => {
      // Type1 member key (e.g., `tstr => ...`) — table-style entry
      let key_type = type1_to_rust_string(t1)?;
      let value_type = type_to_rust_string(&vmke.entry_type)?;
      let rust_type = format!("std::collections::HashMap<{}, {}>", key_type, value_type);
      return Ok(Some(RustField {
        name: "entries".to_string(),
        original_name: "entries".to_string(),
        rust_type,
        is_optional: false,
      }));
    }
    None => {
      // Positional entry without a key — use auto-generated field name
      let rust_type = type_to_rust_string(&vmke.entry_type)?;
      return Ok(Some(RustField {
        name: "value".to_string(),
        original_name: "value".to_string(),
        rust_type,
        is_optional: false,
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
  }))
}

/// Check if an occurrence indicator means "zero or more" or "one or more".
fn is_vec_occurrence(occur: &Occur) -> bool {
  match occur {
    Occur::ZeroOrMore { .. } | Occur::OneOrMore { .. } => true,
    Occur::Exact { upper: Some(u), .. } => *u > 1,
    _ => false,
  }
}

/// Convert an array group to a Rust type string.
fn array_group_to_type(group: &Group<'_>) -> Result<String, CodegenError> {
  // For simple arrays like `[* tstr]` → `Vec<String>`
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
    // Multiple entries → tuple type
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

/// Convert a Type to its Rust representation string.
fn type_to_rust_string(ty: &Type<'_>) -> Result<String, CodegenError> {
  if ty.type_choices.len() == 1 {
    return type1_to_rust_string(&ty.type_choices[0].type1);
  }

  // Multiple choices: check for Option pattern (T / null)
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

  // Fallback: generate a serde_json::Value for complex choices within types
  // (The top-level type definition handler creates proper enums)
  Ok("serde_json::Value".to_string())
}

/// Convert Type1 to Rust type string.
fn type1_to_rust_string(type1: &Type1<'_>) -> Result<String, CodegenError> {
  type2_to_rust_string(&type1.type2)
}

/// Convert Type2 to Rust type string.
fn type2_to_rust_string(type2: &Type2<'_>) -> Result<String, CodegenError> {
  match type2 {
    Type2::Typename { ident, .. } => Ok(cddl_ident_to_rust_type(ident.ident)),
    Type2::Map { group, .. } => {
      // Inline maps become HashMap
      // Check if this looks like a table type (tstr => ...)
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

/// Detect table-style map types like `{ * tstr => int }`.
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

/// Check if a Type2 represents null/nil.
fn is_null_type(type2: &Type2<'_>) -> bool {
  matches!(type2, Type2::Typename { ident, .. } if ident.ident == "null" || ident.ident == "nil")
}

/// Convert a group to enum variants (for choice-from-group).
fn group_to_enum_variants(group: &Group<'_>) -> Result<Vec<RustEnumVariant>, CodegenError> {
  let mut variants = Vec::new();
  for gc in &group.group_choices {
    for (entry, _) in &gc.group_entries {
      let variant = group_entry_to_variant(entry)?;
      variants.push(variant);
    }
  }
  Ok(variants)
}

/// Convert a group entry to an enum variant.
fn group_entry_to_variant(entry: &GroupEntry<'_>) -> Result<RustEnumVariant, CodegenError> {
  match entry {
    GroupEntry::ValueMemberKey { ge, .. } => {
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
      })
    }
    GroupEntry::TypeGroupname { ge, .. } => {
      let variant_name = to_pascal_case(ge.name.ident);
      let inner = cddl_ident_to_rust_type(ge.name.ident);
      Ok(RustEnumVariant {
        name: variant_name.clone(),
        inner_type: if is_prelude_type(ge.name.ident) {
          Some(inner)
        } else {
          Some(variant_name)
        },
      })
    }
    GroupEntry::InlineGroup { .. } => Ok(RustEnumVariant {
      name: "Group".to_string(),
      inner_type: None,
    }),
  }
}

/// Map a CDDL identifier to a Rust type string.
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

/// Map a CBOR major type number to a Rust type.
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

/// Check if an identifier is a CDDL standard prelude type.
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

/// Render all collected type definitions as Rust source code.
fn render_type_defs(defs: &[RustTypeDef]) -> Result<String, CodegenError> {
  let mut output = String::new();
  output.push_str("// Auto-generated from CDDL definition\n");
  output.push_str("// Do not edit manually\n\n");
  output.push_str("use serde::{Deserialize, Serialize};\n\n");

  for (idx, def) in defs.iter().enumerate() {
    if idx > 0 {
      output.push('\n');
    }
    match def {
      RustTypeDef::Struct { name, fields } => {
        render_struct(&mut output, name, fields)?;
      }
      RustTypeDef::TypeAlias { name, target } => {
        render_type_alias(&mut output, name, target)?;
      }
      RustTypeDef::Enum { name, variants } => {
        render_enum(&mut output, name, variants)?;
      }
    }
  }

  Ok(output)
}

/// Render a single struct definition.
fn render_struct(
  output: &mut String,
  name: &str,
  fields: &[RustField],
) -> Result<(), CodegenError> {
  writeln!(output, "#[derive(Clone, Debug, Deserialize, Serialize)]")?;
  writeln!(output, "pub struct {} {{", name)?;
  for field in fields {
    // Add serde rename if the field name differs from the original CDDL key
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

/// Render a type alias.
fn render_type_alias(output: &mut String, name: &str, target: &str) -> Result<(), CodegenError> {
  writeln!(output, "pub type {} = {};", name, target)?;
  Ok(())
}

/// Render an enum definition.
fn render_enum(
  output: &mut String,
  name: &str,
  variants: &[RustEnumVariant],
) -> Result<(), CodegenError> {
  writeln!(output, "#[derive(Clone, Debug, Deserialize, Serialize)]")?;
  writeln!(output, "#[serde(untagged)]")?;
  writeln!(output, "pub enum {} {{", name)?;
  for variant in variants {
    if let Some(inner) = &variant.inner_type {
      writeln!(output, "    {}({}),", variant.name, inner)?;
    } else {
      writeln!(output, "    {},", variant.name)?;
    }
  }
  writeln!(output, "}}")?;
  Ok(())
}

/// Convert a string to PascalCase.
fn to_pascal_case(s: &str) -> String {
  let mut result = String::with_capacity(s.len());
  let mut capitalize_next = true;

  for c in s.chars() {
    if c == '-' || c == '_' || c == '.' || c == ' ' {
      capitalize_next = true;
    } else if capitalize_next {
      result.extend(c.to_uppercase());
      capitalize_next = false;
    } else {
      result.push(c);
    }
  }

  if result.is_empty() {
    return "Unknown".to_string();
  }

  result
}

/// Convert a string to snake_case.
fn to_snake_case(s: &str) -> String {
  let mut result = String::with_capacity(s.len() + 4);
  let mut prev_was_upper = false;
  let mut prev_was_separator = false;

  for (i, c) in s.chars().enumerate() {
    if c == '-' || c == '.' || c == ' ' {
      if !result.is_empty() {
        result.push('_');
      }
      prev_was_separator = true;
      prev_was_upper = false;
    } else if c.is_uppercase() {
      if i > 0 && !prev_was_upper && !prev_was_separator {
        result.push('_');
      }
      result.push(c.to_lowercase().next().unwrap());
      prev_was_upper = true;
      prev_was_separator = false;
    } else {
      result.push(c);
      prev_was_upper = false;
      prev_was_separator = false;
    }
  }

  // Handle Rust keywords
  if is_rust_keyword(&result) {
    result.push('_');
  }

  if result.is_empty() {
    return "value".to_string();
  }

  result
}

/// Check if a string is a Rust keyword.
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

  #[test]
  fn test_to_pascal_case() {
    assert_eq!(to_pascal_case("my-type"), "MyType");
    assert_eq!(to_pascal_case("my_type"), "MyType");
    assert_eq!(to_pascal_case("my.type"), "MyType");
    assert_eq!(to_pascal_case("mytype"), "Mytype");
    assert_eq!(to_pascal_case("MyType"), "MyType");
    assert_eq!(to_pascal_case("person"), "Person");
    assert_eq!(to_pascal_case("http-request"), "HttpRequest");
  }

  #[test]
  fn test_to_snake_case() {
    assert_eq!(to_snake_case("myType"), "my_type");
    assert_eq!(to_snake_case("my-type"), "my_type");
    assert_eq!(to_snake_case("my_type"), "my_type");
    assert_eq!(to_snake_case("MyType"), "my_type");
    assert_eq!(to_snake_case("type"), "type_");
    assert_eq!(to_snake_case("self"), "self_");
  }

  #[test]
  fn test_cddl_ident_to_rust_type() {
    assert_eq!(cddl_ident_to_rust_type("tstr"), "String");
    assert_eq!(cddl_ident_to_rust_type("text"), "String");
    assert_eq!(cddl_ident_to_rust_type("uint"), "u64");
    assert_eq!(cddl_ident_to_rust_type("int"), "i64");
    assert_eq!(cddl_ident_to_rust_type("float"), "f64");
    assert_eq!(cddl_ident_to_rust_type("bool"), "bool");
    assert_eq!(cddl_ident_to_rust_type("bstr"), "Vec<u8>");
    assert_eq!(cddl_ident_to_rust_type("null"), "()");
    assert_eq!(cddl_ident_to_rust_type("any"), "serde_json::Value");
    assert_eq!(cddl_ident_to_rust_type("my-custom-type"), "MyCustomType");
  }

  #[test]
  fn test_simple_struct() {
    let cddl_input = r#"
      person = {
        name: tstr,
        age: uint,
      }
    "#;
    let result = generate_rust_code(cddl_input).unwrap();
    assert!(result.contains("pub struct Person"));
    assert!(result.contains("pub name: String,"));
    assert!(result.contains("pub age: u64,"));
  }

  #[test]
  fn test_optional_fields() {
    let cddl_input = r#"
      person = {
        name: tstr,
        ? nickname: tstr,
      }
    "#;
    let result = generate_rust_code(cddl_input).unwrap();
    assert!(result.contains("pub name: String,"));
    assert!(result.contains("pub nickname: Option<String>,"));
    assert!(result.contains("skip_serializing_if"));
  }

  #[test]
  fn test_type_alias() {
    let cddl_input = r#"
      name = tstr
    "#;
    let result = generate_rust_code(cddl_input).unwrap();
    assert!(result.contains("pub type Name = String;"));
  }

  #[test]
  fn test_type_choices_enum() {
    let cddl_input = r#"
      value = int / tstr / bool
    "#;
    let result = generate_rust_code(cddl_input).unwrap();
    assert!(result.contains("pub enum Value"));
    assert!(result.contains("Int(i64)"));
    assert!(result.contains("Tstr(String)"));
    assert!(result.contains("Bool(bool)"));
  }

  #[test]
  fn test_nested_struct_reference() {
    let cddl_input = r#"
      address = {
        street: tstr,
        city: tstr,
      }

      person = {
        name: tstr,
        home: address,
      }
    "#;
    let result = generate_rust_code(cddl_input).unwrap();
    assert!(result.contains("pub struct Address"));
    assert!(result.contains("pub struct Person"));
    assert!(result.contains("pub home: Address,"));
  }

  #[test]
  fn test_array_type() {
    let cddl_input = r#"
      names = [* tstr]
    "#;
    let result = generate_rust_code(cddl_input).unwrap();
    assert!(result.contains("pub type Names = Vec<String>;"));
  }

  #[test]
  fn test_hyphenated_field_names() {
    let cddl_input = r#"
      my-record = {
        first-name: tstr,
        last-name: tstr,
      }
    "#;
    let result = generate_rust_code(cddl_input).unwrap();
    assert!(result.contains("pub struct MyRecord"));
    assert!(result.contains("#[serde(rename = \"first-name\")]"));
    assert!(result.contains("pub first_name: String,"));
    assert!(result.contains("#[serde(rename = \"last-name\")]"));
    assert!(result.contains("pub last_name: String,"));
  }

  #[test]
  fn test_nullable_type() {
    let cddl_input = r#"
      record = {
        value: tstr / null,
      }
    "#;
    let result = generate_rust_code(cddl_input).unwrap();
    assert!(result.contains("pub value: Option<String>,"));
  }

  #[test]
  fn test_rust_keyword_field_name() {
    let cddl_input = r#"
      my-record = {
        type: tstr,
        match: uint,
      }
    "#;
    let result = generate_rust_code(cddl_input).unwrap();
    assert!(result.contains("pub type_: String,"));
    assert!(result.contains("#[serde(rename = \"type\")]"));
    assert!(result.contains("pub match_: u64,"));
    assert!(result.contains("#[serde(rename = \"match\")]"));
  }

  #[test]
  fn test_byte_string_types() {
    let cddl_input = r#"
      record = {
        data: bstr,
        payload: bytes,
      }
    "#;
    let result = generate_rust_code(cddl_input).unwrap();
    assert!(result.contains("pub data: Vec<u8>,"));
    assert!(result.contains("pub payload: Vec<u8>,"));
  }

  #[test]
  fn test_float_types() {
    let cddl_input = r#"
      measurement = {
        value: float64,
        precision: float,
      }
    "#;
    let result = generate_rust_code(cddl_input).unwrap();
    assert!(result.contains("pub value: f64,"));
    assert!(result.contains("pub precision: f64,"));
  }
}
