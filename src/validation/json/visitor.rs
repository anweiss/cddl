use crate::{
  ast::*,
  visitor::{self, Visitor},
};
use serde_json::Value;
use std::fmt;

pub type Result = std::result::Result<(), Error>;

#[derive(Debug)]
pub struct Error {
  pub reason: String,
  pub location: Value,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Error {} at location {}", self.reason, self.location)
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

pub struct JSONValidator<'a> {
  cddl: &'a CDDL<'a>,
  json: &'a Value,
  errors: Vec<Error>,
}

impl<'a> JSONValidator<'a> {
  pub fn new(cddl: &'a CDDL<'a>, json: &'a Value) -> Self {
    JSONValidator {
      cddl,
      json,
      errors: Vec::default(),
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
}

impl<'a> Visitor<Error> for JSONValidator<'a> {
  fn visit_type_rule(&mut self, tr: &TypeRule) -> visitor::Result<Error> {
    self.visit_type(&tr.value)
  }

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

  fn visit_type1(&mut self, t1: &Type1) -> visitor::Result<Error> {
    if let Some(o) = &t1.operator {
      return self.visit_operator(&t1, o);
    }

    self.visit_type2(&t1.type2)
  }

  fn visit_operator(&mut self, target: &Type1, o: &Operator) -> visitor::Result<Error> {
    match &o.operator {
      RangeCtlOp::RangeOp { is_inclusive, .. } => {
        return self.visit_range(&target.type2, &o.type2, *is_inclusive)
      }
      RangeCtlOp::CtlOp { ctrl, .. } => {
        return self.visit_control_operator(&target.type2, ctrl, &o.type2)
      }
    }
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

  fn visit_group(&mut self, g: &Group) -> visitor::Result<Error> {
    todo!()
  }

  fn visit_identifier(&mut self, ident: &Identifier) -> visitor::Result<Error> {
    match &self.json {
      Value::Null if is_ident_null_data_type(self.cddl, ident) => Ok(()),
      Value::Bool(_) if is_ident_bool_data_type(self.cddl, ident) => Ok(()),
      Value::Number(_) if is_ident_numeric_data_type(self.cddl, ident) => Ok(()),
      Value::String(_) if is_ident_string_data_type(self.cddl, ident) => Ok(()),
      Value::Object(_) => {
        if let Some(tr) = type_rule_from_ident(self.cddl, ident) {
          self.visit_type_rule(tr)
        } else {
          self.errors.push(Error {
            reason: format!("rule for ident {} not found", ident),
            location: self.json.clone(),
          });

          Ok(())
        }
      }
      _ => {
        self.errors.push(Error {
          reason: format!("expected type {}", ident),
          location: self.json.clone(),
        });

        Ok(())
      }
    }
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
    let cddl_str = r#"mystring = "value""#;
    let json = r#""value""#;

    let mut lexer = lexer_from_str(cddl_str);
    let cddl = cddl_from_str(&mut lexer, cddl_str, true)?;
    let json = serde_json::from_str::<Value>(json)?;

    let mut jv = JSONValidator::new(&cddl, &json);
    jv.validate()?;

    Ok(())
  }
}
