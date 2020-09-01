use crate::{
  ast::*,
  token,
  visitor::{self, *},
};
use serde_json::Value;
use std::fmt;

pub type Result = std::result::Result<(), Error>;

#[derive(Debug)]
pub struct Error {
  pub reason: String,
  pub cddl_location: String,
  pub json_location: String,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "Error {} at cddl location {} and JSON location {}",
      self.reason, self.cddl_location, self.json_location
    )
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

pub struct JSONValidator<'a> {
  cddl: &'a CDDL<'a>,
  json: Value,
  errors: Vec<Error>,
  cddl_location: String,
  json_location: String,
  match_member_key: bool,
  object_value: Option<Value>,
}

impl<'a> JSONValidator<'a> {
  pub fn new(cddl: &'a CDDL<'a>, json: Value) -> Self {
    JSONValidator {
      cddl,
      json,
      errors: Vec::default(),
      cddl_location: String::new(),
      json_location: String::new(),
      match_member_key: false,
      object_value: None,
    }
  }

  pub fn validate(&mut self) -> Result {
    for r in self.cddl.rules.iter() {
      // First type rule is root
      if let Rule::Type { rule, .. } = r {
        return self.visit_type_rule(rule);
      }
    }

    Ok(())
  }

  pub fn add_error(&mut self, reason: String) {
    self.errors.push(Error {
      reason,
      cddl_location: self.cddl_location.clone(),
      json_location: self.json_location.clone(),
    });
  }
}

impl<'a> Visitor<Error> for JSONValidator<'a> {
  fn visit_type(&mut self, t: &Type) -> visitor::Result<Error> {
    // Find the first type choice that validates to true
    let find_type_choice = |tc: &TypeChoice| match self.visit_type1(&tc.type1) {
      Ok(()) => true,
      Err(e) => {
        self.errors.push(e);
        false
      }
    };

    if t.type_choices.iter().any(find_type_choice) {
      return Ok(());
    }

    Ok(())
  }

  fn visit_type2(&mut self, t2: &Type2) -> visitor::Result<Error> {
    match t2 {
      Type2::TextValue { value, .. } => match &self.json {
        Value::String(s) if value == s => Ok(()),
        _ => todo!(),
      },
      Type2::Map { group, .. } => match &self.json {
        Value::Object(_) => self.visit_group(group),
        _ => todo!(),
      },
      Type2::Typename { ident, .. } => self.visit_identifier(ident),
      _ => todo!(),
    }
  }

  fn visit_identifier(&mut self, ident: &Identifier) -> visitor::Result<Error> {
    if let Some(tr) = type_rule_from_ident(self.cddl, ident) {
      return self.visit_type_rule(tr);
    }

    match &self.json {
      Value::Null if is_ident_null_data_type(self.cddl, ident) => Ok(()),
      Value::Bool(_) if is_ident_bool_data_type(self.cddl, ident) => Ok(()),
      Value::Number(_) if is_ident_numeric_data_type(self.cddl, ident) => Ok(()),
      Value::String(_) if is_ident_string_data_type(self.cddl, ident) => Ok(()),
      Value::Object(o) => {
        if self.match_member_key {
          if is_ident_string_data_type(self.cddl, ident) {
            return Ok(());
          }

          if let Some(v) = o.get(ident.ident) {
            self.object_value = Some(v.clone());
            self.json_location.push_str(&format!(".{}", ident.ident));

            return Ok(());
          }

          self.add_error(format!("object missing key {}", ident));
        }

        Ok(())
      }
      _ => {
        self.add_error(format!("expected type {}", ident));
        Ok(())
      }
    }
  }

  fn visit_value_member_key_entry(
    &mut self,
    entry: &ValueMemberKeyEntry,
  ) -> visitor::Result<Error> {
    if let Some(occur) = &entry.occur {
      self.visit_occurrence(occur)?;
    }

    if let Some(mk) = &entry.member_key {
      self.match_member_key = true;

      self.visit_memberkey(mk)?;

      self.match_member_key = false;
    }

    if let Some(v) = self.object_value.take() {
      let mut jv = JSONValidator::new(self.cddl, v);
      jv.json_location.push_str(&self.json_location);
      jv.visit_type(&entry.entry_type)?;
      self.json_location = jv.json_location;
      self.errors.append(&mut jv.errors);

      Ok(())
    } else {
      self.visit_type(&entry.entry_type)
    }
  }

  fn visit_value(&mut self, value: &token::Value) -> visitor::Result<Error> {
    let error: Option<String> = match &self.json {
      Value::Number(n) => {
        if let Some(i) = n.as_i64() {
          if let token::Value::INT(v) = value {
            if i != *v as i64 {
              Some(format!("expected {}, got {}", value, n))
            } else {
              None
            }
          } else {
            None
          }
        } else {
          None
        }
      }
      Value::Null => todo!(),
      Value::Bool(_) => todo!(),
      Value::String(_) => todo!(),
      Value::Array(_) => todo!(),
      Value::Object(_) => todo!(),
    };

    if let Some(e) = error {
      self.add_error(e);
    }

    Ok(())
  }

  fn visit_occurrence(&mut self, o: &Occurrence) -> visitor::Result<Error> {
    todo!()
  }
}

fn type_rule_from_ident<'a>(cddl: &'a CDDL, ident: &'a Identifier) -> Option<&'a TypeRule<'a>> {
  cddl.rules.iter().find_map(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => Some(rule),
    _ => None,
  })
}

fn is_ident_null_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
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

fn is_ident_bool_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if ident.ident == "bool" {
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

fn is_ident_numeric_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let "uint" | "nint" | "int" | "number" | "float" | "float16" | "float32" | "float64"
  | "float16-32" | "float32-64" = ident.ident
  {
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

fn is_ident_string_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if ident.ident == "text" || ident.ident == "tstr" {
    return true;
  }

  cddl.rules.iter().any(|r| match r {
    Rule::Type { rule, .. } if &rule.name == ident => rule.value.type_choices.iter().any(|tc| {
      if let Type2::Typename { ident, .. } = &tc.type1.type2 {
        is_ident_string_data_type(cddl, ident)
      } else {
        false
      }
    }),
    _ => false,
  })
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{cddl_from_str, lexer_from_str};

  #[test]
  fn validate() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl_str = r#"obj = {
      key1: {
        key2: {
          key3: tstr,
        },
      },
    }"#;
    let json = r#"{
      "key1": {
        "key2": {
          "key3": "value"
        }
      }
    }"#;

    let mut lexer = lexer_from_str(cddl_str);
    let cddl = cddl_from_str(&mut lexer, cddl_str, true)?;
    let json = serde_json::from_str::<Value>(json)?;

    let mut jv = JSONValidator::new(&cddl, json);
    jv.validate()?;

    println!("errors: {:?}", jv.errors);

    Ok(())
  }
}
