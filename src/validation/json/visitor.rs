use crate::{
  ast::*,
  token,
  visitor::{self, *},
};
use serde_json::Value;
use std::{collections::BTreeMap, fmt};

pub type Result = std::result::Result<(), Error>;

#[derive(Debug)]
pub struct Error(pub Vec<ValidationError>);

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "errors: {:?}", self.0)
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

#[derive(Clone, Debug)]
pub struct ValidationError {
  pub reason: String,
  pub cddl_location: String,
  pub json_location: String,
}

impl fmt::Display for ValidationError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "error at cddl location {} and JSON location {}: {}",
      self.cddl_location, self.json_location, self.reason
    )
  }
}

impl std::error::Error for ValidationError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

pub struct JSONValidator<'a> {
  cddl: &'a CDDL<'a>,
  json: Value,
  errors: Vec<ValidationError>,
  cddl_location: String,
  json_location: String,
  occurence: Option<Occur>,
  group_entry_idx: Option<usize>,
  object_value: Option<Value>,
  is_member_key: bool,
  generic_rules: BTreeMap<String, Option<BTreeMap<String, Option<Type1<'a>>>>>,
}

impl<'a> JSONValidator<'a> {
  pub fn new(cddl: &'a CDDL<'a>, json: Value) -> Self {
    JSONValidator {
      cddl,
      json,
      errors: Vec::default(),
      cddl_location: String::new(),
      json_location: String::new(),
      occurence: None,
      group_entry_idx: None,
      object_value: None,
      is_member_key: false,
      generic_rules: BTreeMap::new(),
    }
  }

  pub fn validate(&mut self) -> std::result::Result<(), Error> {
    for r in self.cddl.rules.iter() {
      // First type rule is root
      if let Rule::Type { rule, .. } = r {
        self.visit_type_rule(rule).map_err(|e| Error(vec![e]))?;
        break;
      }
    }

    if !self.errors.is_empty() {
      return Err(Error(self.errors.clone()));
    }

    Ok(())
  }

  pub fn add_error(&mut self, reason: String) {
    self.errors.push(ValidationError {
      reason,
      cddl_location: self.cddl_location.clone(),
      json_location: self.json_location.clone(),
    });
  }
}

pub fn validate_array_occurrence(
  occurence: Option<&Occur>,
  values: &[Value],
) -> std::result::Result<(bool, bool), String> {
  match occurence {
    Some(Occur::ZeroOrMore(_)) | Some(Occur::Optional(_)) => Ok((true, true)),
    Some(Occur::OneOrMore(_)) => {
      if values.is_empty() {
        Err("array must have at least one item".to_string())
      } else {
        Ok((true, false))
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

      Ok((true, false))
    }
    None => {
      if values.is_empty() {
        Err("array must have exactly one item".to_string())
      } else {
        Ok((false, false))
      }
    }
  }
}

impl<'a> Visitor<ValidationError> for JSONValidator<'a> {
  fn visit_type_rule(&mut self, tr: &TypeRule) -> visitor::Result<ValidationError> {
    self.generic_rules.insert(
      tr.name.to_string(),
      tr.generic_params.as_ref().take().map(|gp| {
        gp.params
          .iter()
          .map(|p| (p.param.ident.to_string(), None))
          .collect()
      }),
    );

    walk_type_rule(self, tr)
  }

  fn visit_group_rule(&mut self, gr: &GroupRule) -> visitor::Result<ValidationError> {
    self.generic_rules.insert(
      gr.name.to_string(),
      gr.generic_params.as_ref().take().map(|gp| {
        gp.params
          .iter()
          .map(|p| (p.param.ident.to_string(), None))
          .collect()
      }),
    );

    walk_group_rule(self, gr)
  }

  fn visit_type(&mut self, t: &Type) -> visitor::Result<ValidationError> {
    let initial_error_count = self.errors.len();
    for type_choice in t.type_choices.iter() {
      let error_count = self.errors.len();
      self.visit_type_choice(type_choice)?;
      if self.errors.len() == error_count {
        // Disregard invalid type choice validation errors if one of the
        // choices validates successfully
        let type_choice_error_count = self.errors.len() - initial_error_count;
        if type_choice_error_count > 0 {
          for i in 0..type_choice_error_count {
            self.errors.pop();
          }
        }

        return Ok(());
      }
    }

    Ok(())
  }

  fn visit_group(&mut self, g: &Group) -> visitor::Result<ValidationError> {
    let initial_error_count = self.errors.len();
    for group_choice in g.group_choices.iter() {
      let error_count = self.errors.len();
      self.visit_group_choice(group_choice)?;
      if self.errors.len() == error_count {
        // Disregard invalid group choice validation errors if one of the
        // choices validates successfully
        let group_choice_error_count = self.errors.len() - initial_error_count;
        if group_choice_error_count > 0 {
          for i in 0..group_choice_error_count {
            self.errors.pop();
          }
        }

        return Ok(());
      }
    }

    Ok(())
  }

  fn visit_group_choice(&mut self, gc: &GroupChoice) -> visitor::Result<ValidationError> {
    for (idx, ge) in gc.group_entries.iter().enumerate() {
      self.group_entry_idx = Some(idx);
      self.visit_group_entry(&ge.0)?;
    }

    Ok(())
  }

  fn visit_range(
    &mut self,
    lower: &Type2,
    upper: &Type2,
    is_inclusive: bool,
  ) -> visitor::Result<ValidationError> {
    if let Value::Array(a) = &self.json {
      #[allow(unused_assignments)]
      let mut iter_items = false;
      #[allow(unused_assignments)]
      let mut allow_errors = false;
      match validate_array_occurrence(self.occurence.as_ref().take(), a) {
        Ok(r) => {
          iter_items = r.0;
          allow_errors = r.1;
        }
        Err(e) => {
          self.add_error(e);
          return Ok(());
        }
      }

      if iter_items {
        for (idx, v) in a.iter().enumerate() {
          let mut jv = JSONValidator::new(self.cddl, v.clone());
          jv.json_location
            .push_str(&format!("{}/{}", self.json_location, idx));

          jv.visit_range(lower, upper, is_inclusive)?;

          // If an array item is invalid, but a '?' or '*' occurrence indicator
          // is present, the ambiguity results in the error being disregarded
          if !allow_errors {
            self.errors.append(&mut jv.errors);
          }
        }
      } else if let Some(idx) = self.group_entry_idx.take() {
        if let Some(v) = a.get(idx) {
          let mut jv = JSONValidator::new(self.cddl, v.clone());
          jv.json_location
            .push_str(&format!("{}/{}", self.json_location, idx));

          jv.visit_range(lower, upper, is_inclusive)?;

          // If an array item is invalid, but a '?' or '*' occurrence indicator
          // is present, the ambiguity results in the error being disregarded
          if !allow_errors {
            self.errors.append(&mut jv.errors);
          }
        } else {
          self.add_error(format!("expecting array item at index {}", idx));
        }
      }

      return Ok(());
    }

    match lower {
      Type2::IntValue { value: l, .. } => match upper {
        Type2::IntValue { value: u, .. } => {
          let error_str = if is_inclusive {
            format!(
              "expected integer to be in range {} <= value <= {}, got {}",
              l, u, self.json
            )
          } else {
            format!(
              "expected integer to be in range {} < value < {}, got {}",
              l, u, self.json
            )
          };

          match &self.json {
            Value::Number(n) => {
              if let Some(i) = n.as_i64() {
                if is_inclusive {
                  if i < *l as i64 || i > *u as i64 {
                    self.add_error(error_str);
                  } else {
                    return Ok(());
                  }
                } else if i <= *l as i64 || i >= *u as i64 {
                  self.add_error(error_str);
                  return Ok(());
                } else {
                  return Ok(());
                }
              } else {
                self.add_error(error_str);
                return Ok(());
              }
            }
            _ => {
              self.add_error(error_str);
              return Ok(());
            }
          }
        }
        Type2::UintValue { value: u, .. } => {
          let error_str = if is_inclusive {
            format!(
              "expected integer to be in range {} <= value <= {}, got {}",
              l, u, self.json
            )
          } else {
            format!(
              "expected integer to be in range {} < value < {}, got {}",
              l, u, self.json
            )
          };

          match &self.json {
            Value::Number(n) => {
              if let Some(i) = n.as_i64() {
                if is_inclusive {
                  if i < *l as i64 || i > *u as i64 {
                    self.add_error(error_str);
                  } else {
                    return Ok(());
                  }
                } else if i <= *l as i64 || i >= *u as i64 {
                  self.add_error(error_str);
                  return Ok(());
                } else {
                  return Ok(());
                }
              } else {
                self.add_error(error_str);
                return Ok(());
              }
            }
            _ => {
              self.add_error(error_str);
              return Ok(());
            }
          }
        }
        _ => {
          self.add_error(format!(
            "invalid cddl range. upper value must be an integer type. got {}",
            upper
          ));
          return Ok(());
        }
      },
      Type2::UintValue { value: l, .. } => match upper {
        Type2::UintValue { value: u, .. } => {
          let error_str = if is_inclusive {
            format!(
              "expected uint to be in range {} <= value <= {}, got {}",
              l, u, self.json
            )
          } else {
            format!(
              "expected uint to be in range {} < value < {}, got {}",
              l, u, self.json
            )
          };

          match &self.json {
            Value::Number(n) => {
              if let Some(i) = n.as_u64() {
                if is_inclusive {
                  if i < *l as u64 || i > *u as u64 {
                    self.add_error(error_str);
                  } else {
                    return Ok(());
                  }
                } else if i <= *l as u64 || i >= *u as u64 {
                  self.add_error(error_str);
                  return Ok(());
                } else {
                  return Ok(());
                }
              } else {
                self.add_error(error_str);
                return Ok(());
              }
            }
            _ => {
              self.add_error(error_str);
              return Ok(());
            }
          }
        }
        _ => {
          self.add_error(format!(
            "invalid cddl range. upper value must be a uint type. got {}",
            upper
          ));
          return Ok(());
        }
      },
      Type2::FloatValue { value: l, .. } => match upper {
        Type2::FloatValue { value: u, .. } => {
          let error_str = if is_inclusive {
            format!(
              "expected float to be in range {} <= value <= {}, got {}",
              l, u, self.json
            )
          } else {
            format!(
              "expected float to be in range {} < value < {}, got {}",
              l, u, self.json
            )
          };

          match &self.json {
            Value::Number(n) => {
              if let Some(f) = n.as_f64() {
                if is_inclusive {
                  if f < *l as f64 || f > *u as f64 {
                    self.add_error(error_str);
                  } else {
                    return Ok(());
                  }
                } else if f <= *l as f64 || f >= *u as f64 {
                  self.add_error(error_str);
                  return Ok(());
                } else {
                  return Ok(());
                }
              } else {
                self.add_error(error_str);
                return Ok(());
              }
            }
            _ => {
              self.add_error(error_str);
              return Ok(());
            }
          }
        }
        _ => {
          self.add_error(format!(
            "invalid cddl range. upper value must be a float type. got {}",
            upper
          ));
          return Ok(());
        }
      },
      _ => {
        self.add_error(
          "invalid cddl range. upper and lower values must be either integers or floats"
            .to_string(),
        );

        return Ok(());
      }
    }

    Ok(())
  }

  fn visit_type2(&mut self, t2: &Type2) -> visitor::Result<ValidationError> {
    match t2 {
      Type2::TextValue { value, .. } => self.visit_value(&token::Value::TEXT(value)),
      Type2::Map { group, .. } => match &self.json {
        Value::Object(_) => self.visit_group(group),
        Value::Array(a) => {
          // Member keys are annotation only in an array context
          if self.is_member_key {
            return Ok(());
          }

          #[allow(unused_assignments)]
          let mut iter_items = false;
          #[allow(unused_assignments)]
          let mut allow_errors = false;
          match validate_array_occurrence(self.occurence.as_ref().take(), a) {
            Ok(r) => {
              iter_items = r.0;
              allow_errors = r.1;
            }
            Err(e) => {
              self.add_error(e);
              return Ok(());
            }
          }

          if iter_items {
            for (idx, v) in a.iter().enumerate() {
              let mut jv = JSONValidator::new(self.cddl, v.clone());
              jv.json_location
                .push_str(&format!("{}/{}", self.json_location, idx));

              jv.visit_group(group)?;

              // If an array item is invalid, but a '?' or '*' occurrence indicator
              // is present, the ambiguity results in the error being disregarded
              if !allow_errors {
                self.errors.append(&mut jv.errors);
              }
            }
          } else if let Some(idx) = self.group_entry_idx.take() {
            if let Some(v) = a.get(idx) {
              let mut jv = JSONValidator::new(self.cddl, v.clone());
              jv.json_location
                .push_str(&format!("{}/{}", self.json_location, idx));

              jv.visit_group(group)?;

              // If an array item is invalid, but a '?' or '*' occurrence indicator
              // is present, the ambiguity results in the error being disregarded
              if !allow_errors {
                self.errors.append(&mut jv.errors);
              }
            } else {
              self.add_error(format!("expecting map object {} at index {}", group, idx));
            }
          }

          Ok(())
        }
        _ => {
          self.add_error(format!("expecting map object {}, got {}", t2, self.json));
          Ok(())
        }
      },
      Type2::Array { group, .. } => match &self.json {
        Value::Array(_) => self.visit_group(group),
        _ => todo!(),
      },
      Type2::Typename {
        ident,
        generic_args,
        ..
      } => {
        if let Some(ga) = generic_args {
          todo!()
        }
        self.visit_identifier(ident)
      }
      Type2::IntValue { value, .. } => self.visit_value(&token::Value::INT(*value)),
      Type2::UintValue { value, .. } => self.visit_value(&token::Value::UINT(*value)),
      Type2::FloatValue { value, .. } => self.visit_value(&token::Value::FLOAT(*value)),
      Type2::Any(_) => Ok(()),
      _ => todo!(),
    }
  }

  fn visit_identifier(&mut self, ident: &Identifier) -> visitor::Result<ValidationError> {
    if let Some(r) = rule_from_ident(self.cddl, ident) {
      return self.visit_rule(r);
    }

    match &self.json {
      Value::Null if is_ident_null_data_type(self.cddl, ident) => Ok(()),
      Value::Bool(_) if is_ident_bool_data_type(self.cddl, ident) => Ok(()),
      Value::Number(_) if is_ident_numeric_data_type(self.cddl, ident) => Ok(()),
      Value::String(_) if is_ident_string_data_type(self.cddl, ident) => Ok(()),
      Value::Array(a) => {
        // Member keys are annotation only in an array context
        if self.is_member_key {
          return Ok(());
        }

        #[allow(unused_assignments)]
        let mut iter_items = false;
        #[allow(unused_assignments)]
        let mut allow_errors = false;
        match validate_array_occurrence(self.occurence.as_ref().take(), a) {
          Ok(r) => {
            iter_items = r.0;
            allow_errors = r.1;
          }
          Err(e) => {
            self.add_error(e);
            return Ok(());
          }
        }

        if iter_items {
          for (idx, v) in a.iter().enumerate() {
            let mut jv = JSONValidator::new(self.cddl, v.clone());
            jv.json_location
              .push_str(&format!("{}/{}", self.json_location, idx));

            jv.visit_identifier(ident)?;

            // If an array item is invalid, but a '?' or '*' occurrence indicator
            // is present, the ambiguity results in the error being disregarded
            if !allow_errors {
              self.errors.append(&mut jv.errors);
            }
          }
        } else if let Some(idx) = self.group_entry_idx.take() {
          if let Some(v) = a.get(idx) {
            let mut jv = JSONValidator::new(self.cddl, v.clone());
            jv.json_location
              .push_str(&format!("{}/{}", self.json_location, idx));

            jv.visit_identifier(ident)?;

            // If an array item is invalid, but a '?' or '*' occurrence indicator
            // is present, the ambiguity results in the error being disregarded
            if !allow_errors {
              self.errors.append(&mut jv.errors);
            }
          } else {
            self.add_error(format!("expecting type {} at index {}", ident, idx));
          }
        }

        Ok(())
      }
      Value::Object(o) => {
        if is_ident_string_data_type(self.cddl, ident) {
          return Ok(());
        }

        self.visit_value(&token::Value::TEXT(ident.ident))
      }
      _ => {
        self.add_error(format!("expected type {}, got {}", ident, self.json));
        Ok(())
      }
    }
  }

  fn visit_value_member_key_entry(
    &mut self,
    entry: &ValueMemberKeyEntry,
  ) -> visitor::Result<ValidationError> {
    if let Some(occur) = &entry.occur {
      self.visit_occurrence(occur)?;
    }

    let current_location = self.json_location.clone();

    if let Some(mk) = &entry.member_key {
      self.is_member_key = true;
      self.visit_memberkey(mk)?;
      self.is_member_key = false;
    }

    if let Some(v) = self.object_value.take() {
      let mut jv = JSONValidator::new(self.cddl, v);
      jv.json_location.push_str(&self.json_location);
      jv.visit_type(&entry.entry_type)?;

      self.json_location = current_location;
      self.errors.append(&mut jv.errors);

      Ok(())
    } else {
      self.visit_type(&entry.entry_type)
    }
  }

  fn visit_value(&mut self, value: &token::Value) -> visitor::Result<ValidationError> {
    let error: Option<String> = match &self.json {
      Value::Number(n) => match value {
        token::Value::INT(v) => match n.as_i64() {
          Some(i) if i == *v as i64 => None,
          None => Some(format!("{} cannot be represented as an i64", n)),
          _ => Some(format!("expected {}, got {}", v, n)),
        },
        token::Value::UINT(v) => match n.as_u64() {
          Some(i) if i == *v as u64 => None,
          None => Some(format!("{} cannot be represented as a u64", n)),
          _ => Some(format!("expected {}, got {}", v, n)),
        },
        token::Value::FLOAT(v) => match n.as_f64() {
          Some(f) if (f - *v).abs() < std::f64::EPSILON => None,
          None => Some(format!("{} cannot be represented as an f64", n)),
          _ => Some(format!("expected {}, got {}", v, n)),
        },
        _ => Some(format!("expected {}, got {}", value, n)),
      },
      Value::String(s) => match value {
        token::Value::TEXT(t) if s == t => None,
        token::Value::BYTE(token::ByteValue::UTF8(b)) if s.as_bytes() == b.as_ref() => None,
        token::Value::BYTE(token::ByteValue::B16(b)) if s.as_bytes() == b.as_ref() => None,
        token::Value::BYTE(token::ByteValue::B64(b)) if s.as_bytes() == b.as_ref() => None,
        _ => Some(format!("expected {}, got \"{}\"", value, s)),
      },
      Value::Array(a) => {
        // Member keys are annotation only in an array context
        if self.is_member_key {
          return Ok(());
        }

        #[allow(unused_assignments)]
        let mut iter_items = false;
        #[allow(unused_assignments)]
        let mut allow_errors = false;
        match validate_array_occurrence(self.occurence.as_ref().take(), a) {
          Ok(r) => {
            iter_items = r.0;
            allow_errors = r.1;
          }
          Err(e) => {
            self.add_error(e);
            return Ok(());
          }
        }

        if iter_items {
          for (idx, v) in a.iter().enumerate() {
            let mut jv = JSONValidator::new(self.cddl, v.clone());
            jv.json_location
              .push_str(&format!("{}/{}", self.json_location, idx));

            jv.visit_value(value)?;

            // If an array item is invalid, but a '?' or '*' occurrence indicator
            // is present, the ambiguity results in the error being disregarded
            if !allow_errors {
              self.errors.append(&mut jv.errors);
            }
          }
        } else if let Some(idx) = self.group_entry_idx.take() {
          if let Some(v) = a.get(idx) {
            let mut jv = JSONValidator::new(self.cddl, v.clone());
            jv.json_location
              .push_str(&format!("{}/{}", self.json_location, idx));

            jv.visit_value(value)?;

            // If an array item is invalid, but a '?' or '*' occurrence indicator
            // is present, the ambiguity results in the error being disregarded
            if !allow_errors {
              self.errors.append(&mut jv.errors);
            }
          } else {
            self.add_error(format!("expecting value {} at index {}", value, idx));
          }
        }

        None
      }
      Value::Object(o) => {
        if let token::Value::TEXT(t) = value {
          if let Some(v) = o.get(*t) {
            self.object_value = Some(v.clone());
            self.json_location.push_str(&format!("/{}", t));

            None
          } else if let Some(occur) = &self.occurence {
            if let Occur::Optional(_) | Occur::ZeroOrMore(_) = occur {
              None
            } else {
              Some(format!("object missing key: {}", t))
            }
          } else {
            Some(format!("object missing key: {}", t))
          }
        } else {
          Some(format!(
            "CDDL member key must be string data type. got {}",
            value
          ))
        }
      }
      _ => Some(format!("expected {}, got {}", value, self.json)),
    };

    if let Some(e) = error {
      self.add_error(e);
    }

    Ok(())
  }

  fn visit_occurrence(&mut self, o: &Occurrence) -> visitor::Result<ValidationError> {
    self.occurence = Some(o.occur.clone());

    Ok(())
  }
}

fn rule_from_ident<'a>(cddl: &'a CDDL, ident: &'a Identifier) -> Option<&'a Rule<'a>> {
  cddl.rules.iter().find_map(|r| match r {
    Rule::Type { rule, .. } if rule.name.ident == ident.ident => Some(r),
    Rule::Group { rule, .. } if rule.name.ident == ident.ident => Some(r),
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

fn is_ident_numeric_data_type(cddl: &CDDL, ident: &Identifier) -> bool {
  if let "uint" | "nint" | "integer" | "int" | "number" | "float" | "float16" | "float32"
  | "float64" | "float16-32" | "float32-64" | "unsigned" = ident.ident
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
    let cddl_str = r#"rule = [ + -10.0..-3.0 ]"#;
    let json = r#"[
      -10.0
    ]"#;

    let mut lexer = lexer_from_str(cddl_str);
    let cddl = cddl_from_str(&mut lexer, cddl_str, true)?;
    let json = serde_json::from_str::<Value>(json)?;

    let mut jv = JSONValidator::new(&cddl, json);
    jv.validate()?;

    Ok(())
  }
}
