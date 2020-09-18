/// CBOR validation implementation
pub mod cbor;

/// JSON validation implementation
pub mod json;

use json::JSONValidator;
use serde::de::Deserialize;
use serde_json::Value;

use crate::{
  ast::{
    GroupChoice, GroupEntry, GroupRule, Identifier, Occur, Rule, Type, Type2, TypeChoice, CDDL,
  },
  cddl_from_str, lexer_from_str,
};

/// Validate JSON string from a given CDDL document string
pub fn validate_json_from_str(cddl: &str, json: &str) -> json::Result {
  let mut lexer = lexer_from_str(cddl);
  let cddl = cddl_from_str(&mut lexer, cddl, true).map_err(json::Error::CDDLParsing)?;
  let json = serde_json::from_str::<Value>(json).map_err(json::Error::JSONParsing)?;

  let mut jv = JSONValidator::new(&cddl, json);
  jv.validate()
}

/// Validate CBOR slice from a given CDDL document string
pub fn validate_cbor_from_slice(cddl: &str, cbor: &[u8]) -> cbor::Result {
  todo!()
}

/// Find non-choice alternate rule from a given identifier
pub fn rule_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Option<&'a Rule<'a>> {
  cddl.rules.iter().find_map(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident && !rule.is_type_choice_alternate => Some(r),
    Rule::Group { rule, .. } if rule.name == *ident && !rule.is_group_choice_alternate => Some(r),
    _ => None,
  })
}

/// Find non-group choice alternate rule from a given identifier
pub fn group_rule_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Option<&'a GroupRule<'a>> {
  cddl.rules.iter().find_map(|r| match r {
    Rule::Group { rule, .. } if rule.name == *ident && !rule.is_group_choice_alternate => {
      Some(rule.as_ref())
    }
    _ => None,
  })
}

/// Retrieve the list of generic parameters for a given rule
pub fn generic_params_from_rule<'a>(rule: &Rule<'a>) -> Option<Vec<&'a str>> {
  match rule {
    Rule::Type { rule, .. } => rule
      .generic_params
      .as_ref()
      .map(|gp| gp.params.iter().map(|gp| gp.param.ident).collect()),
    Rule::Group { rule, .. } => rule
      .generic_params
      .as_ref()
      .map(|gp| gp.params.iter().map(|gp| gp.param.ident).collect()),
  }
}

/// Find all type choice alternate rules from a given identifier
pub fn type_choice_alternates_from_ident<'a>(
  cddl: &'a CDDL,
  ident: &Identifier,
) -> Vec<&'a Type<'a>> {
  cddl
    .rules
    .iter()
    .filter_map(|r| match r {
      Rule::Type { rule, .. } if &rule.name == ident => Some(&rule.value),
      _ => None,
    })
    .collect::<Vec<_>>()
}

/// Find all group choice alternate rules from a given identifier
pub fn group_choice_alternates_from_ident<'a>(
  cddl: &'a CDDL,
  ident: &Identifier,
) -> Vec<&'a GroupEntry<'a>> {
  cddl
    .rules
    .iter()
    .filter_map(|r| match r {
      Rule::Group { rule, .. } if &rule.name == ident => Some(&rule.entry),
      _ => None,
    })
    .collect::<Vec<_>>()
}

/// Convert a given group choice to a list of type choices
pub fn type_choices_from_group_choice<'a>(
  cddl: &'a CDDL,
  grpchoice: &GroupChoice<'a>,
) -> Vec<TypeChoice<'a>> {
  let mut type_choices = Vec::new();
  for ge in grpchoice.group_entries.iter() {
    match &ge.0 {
      GroupEntry::ValueMemberKey { ge, .. } => {
        type_choices.append(&mut ge.entry_type.type_choices.clone());
      }
      GroupEntry::TypeGroupname { ge, .. } => {
        // TODO: parse generic args
        if let Some(r) = rule_from_ident(cddl, &ge.name) {
          match r {
            Rule::Type { rule, .. } => type_choices.append(&mut rule.value.type_choices.clone()),
            Rule::Group { rule, .. } => type_choices.append(&mut type_choices_from_group_choice(
              cddl,
              &GroupChoice::new(vec![rule.entry.clone()]),
            )),
          }
        }
      }
      GroupEntry::InlineGroup { group, .. } => {
        for gc in group.group_choices.iter() {
          type_choices.append(&mut type_choices_from_group_choice(cddl, gc));
        }
      }
    }
  }

  type_choices
}

/// Is the given identifier associated with a null data type
pub fn is_ident_null_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if ident.ident == "null" || ident.ident == "nil" {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_null_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a boolean data type
pub fn is_ident_bool_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let "bool" | "true" | "false" = ident.ident {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_bool_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a numeric data type
pub fn is_ident_numeric_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let "uint" | "nint" | "integer" | "int" | "number" | "float" | "float16" | "float32"
  | "float64" | "float16-32" | "float32-64" | "unsigned" = ident.ident
  {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_numeric_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a uint data type
pub fn is_ident_uint_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let "uint" = ident.ident {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_uint_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Is the given identifier associated with a string data type
pub fn is_ident_string_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if ident.ident == "text" || ident.ident == "tstr" {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_string_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

/// Validate an array based on an occurrence indicator. The first boolean in the
/// returned tuple indicates whether to validate the array homogenously or
/// non-homogenously (based on the index of the entry). The second boolean in
/// the returned tuple indicates whether or not validation errors should be
/// ignored.
pub fn validate_array_occurrence<'de, T: Deserialize<'de>>(
  occurence: Option<&Occur>,
  values: &[T],
) -> std::result::Result<bool, String> {
  match occurence {
    Some(Occur::ZeroOrMore(_)) | Some(Occur::Optional(_)) => Ok(true),
    Some(Occur::OneOrMore(_)) => {
      if values.is_empty() {
        Err("array must have at least one item".to_string())
      } else {
        Ok(true)
      }
    }
    Some(Occur::Exact { lower, upper, .. }) => {
      if let Some(lower) = lower {
        if let Some(upper) = upper {
          if lower == upper && values.len() != *lower {
            return Err(format!("array must have exactly {} items", lower));
          }
          if values.len() < *lower || values.len() > *upper {
            return Err(format!(
              "array must have between {} and {} items",
              lower, upper
            ));
          }
        } else if values.len() < *lower {
          return Err(format!("array must have at least {} items", lower));
        }
      } else if let Some(upper) = upper {
        if values.len() > *upper {
          return Err(format!("array must have not more than {} items", upper));
        }
      }

      Ok(true)
    }
    None => {
      if values.is_empty() {
        Err("array must have exactly one item".to_string())
      } else {
        Ok(false)
      }
    }
  }
}
