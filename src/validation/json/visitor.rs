#![cfg(feature = "std")]

use crate::{
  ast::*,
  token::{self, Token},
  visitor::{self, *},
};
use serde_json::Value;
use std::fmt;

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
  pub is_multi_type_choice: bool,
  pub is_group_to_choice_enum: bool,
}

impl fmt::Display for ValidationError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut error_str = String::from("error validating");
    if self.is_multi_type_choice {
      error_str.push_str(" type choice");
    }
    if self.is_group_to_choice_enum {
      error_str.push_str(" type choice in group to choice enumeration");
    }

    write!(
      f,
      "{} at cddl location {} and JSON location {}: {}",
      error_str, self.cddl_location, self.json_location, self.reason
    )
  }
}

impl std::error::Error for ValidationError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

impl ValidationError {
  fn from_validator(jv: &JSONValidator, reason: String) -> Self {
    ValidationError {
      cddl_location: jv.cddl_location.clone(),
      json_location: jv.json_location.clone(),
      reason,
      is_multi_type_choice: jv.is_multi_type_choice,
      is_group_to_choice_enum: jv.is_group_to_choice_enum,
    }
  }
}

pub struct JSONValidator<'a> {
  cddl: &'a CDDL<'a>,
  json: Value,
  errors: Vec<ValidationError>,
  cddl_location: String,
  json_location: String,
  // Occurrence indicator detected in current state of AST evaluation
  occurence: Option<Occur>,
  // Current group entry index detected in current state of AST evaluation
  group_entry_idx: Option<usize>,
  // JSON object value hoisted from previous state of AST evaluation
  object_value: Option<Value>,
  // Is member key detected in current state of AST evaluation
  is_member_key: bool,
  // Is a cut detected in current state of AST evaluation
  is_cut_present: bool,
  // Str value of cut detected in current state of AST evaluation
  cut_value: Option<&'a str>,
  // Validate the generic rule given by str ident in current state of AST
  // evaluation
  eval_generic_rule: Option<&'a str>,
  // Aggregation of generic rules
  generic_rules: Vec<GenericRule<'a>>,
  // Control operator token detected in current state of AST evaluation
  ctrl: Option<token::Token<'a>>,
  // Is a group to choice enumeration detected in current state of AST
  // evaluation
  is_group_to_choice_enum: bool,
  // Are 2 or more type choices detected in current state of AST evaluation
  is_multi_type_choice: bool,
}

#[derive(Clone, Debug)]
pub struct GenericRule<'a> {
  name: &'a str,
  params: Vec<&'a str>,
  args: Vec<Type1<'a>>,
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
      is_cut_present: false,
      cut_value: None,
      eval_generic_rule: None,
      generic_rules: Vec::new(),
      ctrl: None,
      is_group_to_choice_enum: false,
      is_multi_type_choice: false,
    }
  }

  pub fn validate(&mut self) -> std::result::Result<(), Error> {
    for r in self.cddl.rules.iter() {
      // First type rule is root
      if let Rule::Type { rule, .. } = r {
        if rule.generic_params.is_none() {
          self.visit_type_rule(rule).map_err(|e| Error(vec![e]))?;
          break;
        }
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
      is_multi_type_choice: self.is_multi_type_choice,
      is_group_to_choice_enum: self.is_group_to_choice_enum,
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

impl<'a> Visitor<'a, ValidationError> for JSONValidator<'a> {
  fn visit_type_rule(&mut self, tr: &TypeRule<'a>) -> visitor::Result<ValidationError> {
    if let Some(gp) = &tr.generic_params {
      if let Some(gr) = self
        .generic_rules
        .iter_mut()
        .find(|r| r.name == tr.name.ident)
      {
        gr.params = gp.params.iter().map(|p| p.param.ident).collect();
      } else {
        self.generic_rules.push(GenericRule {
          name: tr.name.ident,
          params: gp.params.iter().map(|p| p.param.ident).collect(),
          args: vec![],
        });
      }
    }

    let error_count = self.errors.len();

    for t in type_choice_alternates_from_ident(self.cddl, &tr.name) {
      let cur_errors = self.errors.len();
      self.visit_type(t)?;
      if self.errors.len() == cur_errors {
        for _ in 0..self.errors.len() - error_count {
          self.errors.pop();
        }

        return Ok(());
      }
    }

    Ok(())
  }

  fn visit_group_rule(&mut self, gr: &GroupRule<'a>) -> visitor::Result<ValidationError> {
    if let Some(gp) = &gr.generic_params {
      if let Some(gr) = self
        .generic_rules
        .iter_mut()
        .find(|r| r.name == gr.name.ident)
      {
        gr.params = gp.params.iter().map(|p| p.param.ident).collect();
      } else {
        self.generic_rules.push(GenericRule {
          name: gr.name.ident,
          params: gp.params.iter().map(|p| p.param.ident).collect(),
          args: vec![],
        });
      }
    }

    let error_count = self.errors.len();

    for t in group_choice_alternates_from_ident(self.cddl, &gr.name) {
      let cur_errors = self.errors.len();
      self.visit_group_entry(&gr.entry)?;
      if self.errors.len() == cur_errors {
        for _ in 0..self.errors.len() - error_count {
          self.errors.pop();
        }

        return Ok(());
      }
    }

    Ok(())
  }

  fn visit_type(&mut self, t: &Type<'a>) -> visitor::Result<ValidationError> {
    if t.type_choices.len() > 1 {
      self.is_multi_type_choice = true;
    }

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

  fn visit_group(&mut self, g: &Group<'a>) -> visitor::Result<ValidationError> {
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

  fn visit_group_choice(&mut self, gc: &GroupChoice<'a>) -> visitor::Result<ValidationError> {
    if self.is_group_to_choice_enum {
      let initial_error_count = self.errors.len();
      for tc in type_choices_from_group_choice(self.cddl, gc).iter() {
        let error_count = self.errors.len();
        self.visit_type_choice(tc)?;
        if self.errors.len() == error_count {
          let type_choice_error_count = self.errors.len() - initial_error_count;
          if type_choice_error_count > 0 {
            for i in 0..type_choice_error_count {
              self.errors.pop();
            }
          }
          return Ok(());
        }
      }

      return Ok(());
    }

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
          jv.generic_rules = self.generic_rules.clone();
          jv.eval_generic_rule = self.eval_generic_rule;
          jv.is_multi_type_choice = self.is_multi_type_choice;
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
          jv.generic_rules = self.generic_rules.clone();
          jv.eval_generic_rule = self.eval_generic_rule;
          jv.is_multi_type_choice = self.is_multi_type_choice;
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
            Value::String(s) => match self.ctrl {
              Some(Token::SIZE) => {
                let len = s.len();
                let s = s.clone();
                if is_inclusive {
                  if s.len() < *l || s.len() > *u {
                    self.add_error(format!(
                      "expected \"{}\" string length to be in the range {} <= value <= {}, got {}",
                      s, l, u, len
                    ));
                    return Ok(());
                  } else {
                    return Ok(());
                  }
                } else if s.len() <= *l || s.len() >= *u {
                  self.add_error(format!(
                    "expected \"{}\" string length to be in the range {} < value < {}, got {}",
                    s, l, u, len
                  ));
                  return Ok(());
                }
              }
              _ => {
                self.add_error("string value cannot be validated against a range without the .size control operator".to_string());
                return Ok(());
              }
            },
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

  fn visit_control_operator(
    &mut self,
    target: &Type2<'a>,
    ctrl: &str,
    controller: &Type2<'a>,
  ) -> visitor::Result<ValidationError> {
    match token::lookup_control_from_str(ctrl) {
      Some(Token::EQ) => {
        match target {
          Type2::Typename { ident, .. } => {
            if is_ident_string_data_type(self.cddl, ident)
              || is_ident_numeric_data_type(self.cddl, ident)
            {
              return self.visit_type2(controller);
            }
          }
          Type2::Array { .. } => {
            if let Value::Array(_) = &self.json {
              return self.visit_type2(controller);
            }
          }
          _ => self.add_error(format!(
            "target for .eq operator must be a string, numerical or array data type, got {}",
            target
          )),
        }
        Ok(())
      }
      t @ Some(Token::NE) => {
        match target {
          Type2::Typename { ident, .. } => {
            if is_ident_string_data_type(self.cddl, ident)
              || is_ident_numeric_data_type(self.cddl, ident)
            {
              self.ctrl = t;
              self.visit_type2(controller)?;
              self.ctrl = None;
              return Ok(());
            }
          }
          Type2::Array { .. } => {
            if let Value::Array(_) = &self.json {
              self.ctrl = t;
              self.visit_type2(controller)?;
              self.ctrl = None;
              return Ok(());
            }
          }
          _ => self.add_error(format!(
            "target for .eq operator must be a string, numerical or array data type, got {}",
            target
          )),
        }
        Ok(())
      }
      t @ Some(Token::LT) | t @ Some(Token::GT) | t @ Some(Token::GE) | t @ Some(Token::LE) => {
        match target {
          Type2::Typename { ident, .. } if is_ident_numeric_data_type(self.cddl, ident) => {
            self.ctrl = t;
            self.visit_type2(controller)?;
            self.ctrl = None;
            Ok(())
          }
          _ => {
            self.add_error(format!(
              "target for .lt, .gt, .ge or .le operator must be a numerical data type, got {}",
              target
            ));
            Ok(())
          }
        }
      }
      t @ Some(Token::SIZE) => match target {
        Type2::Typename { ident, .. }
          if is_ident_string_data_type(self.cddl, ident)
            || is_ident_uint_data_type(self.cddl, ident) =>
        {
          self.ctrl = t;
          self.visit_type2(controller)?;
          self.ctrl = None;
          Ok(())
        }
        _ => {
          self.add_error(format!(
            "target for .size must a string or uint data type, got {}",
            target
          ));
          Ok(())
        }
      },
      t @ Some(Token::AND) => {
        self.ctrl = t;
        self.visit_type2(target)?;
        self.visit_type2(controller)?;
        self.ctrl = None;
        Ok(())
      }
      t @ Some(Token::WITHIN) => {
        self.ctrl = t;
        let error_count = self.errors.len();
        self.visit_type2(target)?;
        let no_errors = self.errors.len() == error_count;
        self.visit_type2(controller)?;
        if no_errors && self.errors.len() > error_count {
          for _ in 0..self.errors.len() - error_count {
            self.errors.pop();
          }

          self.add_error(format!(
            "expected type {} .within type {}, got {}",
            target, controller, self.json,
          ));
        }

        self.ctrl = None;

        Ok(())
      }
      t @ Some(Token::DEFAULT) => {
        self.ctrl = t;
        let error_count = self.errors.len();
        self.visit_type2(target)?;
        if self.errors.len() != error_count {
          if let Some(Occur::Optional(_)) = self.occurence {
            self.add_error(format!(
              "expecting default value {}, got {}",
              controller, self.json
            ));
          }
        }
        self.ctrl = None;
        Ok(())
      }
      t @ Some(Token::REGEXP) | t @ Some(Token::PCRE) => {
        self.ctrl = t;
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.cddl, ident) => {
            match self.json {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".regexp/.pcre control can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".regexp/.pcre contro9l can only be matched against string data type, got {}",
            target
          )),
        }
        self.ctrl = None;

        Ok(())
      }
      _ => {
        self.add_error(format!("unsupported control operator {}", ctrl));
        Ok(())
      }
    }
  }

  fn visit_type2(&mut self, t2: &Type2<'a>) -> visitor::Result<ValidationError> {
    match t2 {
      Type2::TextValue { value, .. } => self.visit_value(&token::Value::TEXT(value)),
      Type2::Map { group, .. } => match &self.json {
        Value::Object(_) => {
          self.visit_group(group)?;
          self.is_cut_present = false;
          self.cut_value = None;
          Ok(())
        }
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
              jv.generic_rules = self.generic_rules.clone();
              jv.eval_generic_rule = self.eval_generic_rule;
              jv.is_multi_type_choice = self.is_multi_type_choice;
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
              jv.generic_rules = self.generic_rules.clone();
              jv.eval_generic_rule = self.eval_generic_rule;
              jv.is_multi_type_choice = self.is_multi_type_choice;
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
        _ => {
          self.add_error(format!("expected array type, got {}", self.json));
          Ok(())
        }
      },
      Type2::ChoiceFromGroup {
        ident,
        generic_args,
        ..
      } => {
        if let Some(ga) = generic_args {
          if let Some(rule) = rule_from_ident(self.cddl, ident) {
            if let Some(gr) = self
              .generic_rules
              .iter_mut()
              .find(|gr| gr.name == ident.ident)
            {
              for arg in ga.args.iter() {
                gr.args.push((*arg.arg).clone());
              }
            } else if let Some(params) = generic_params_from_rule(self.cddl, rule) {
              self.generic_rules.push(GenericRule {
                name: ident.ident,
                params,
                args: ga.args.iter().cloned().map(|arg| *arg.arg).collect(),
              });
            }

            let mut jv = JSONValidator::new(self.cddl, self.json.clone());
            jv.generic_rules = self.generic_rules.clone();
            jv.eval_generic_rule = Some(ident.ident);
            jv.is_group_to_choice_enum = true;
            jv.is_multi_type_choice = self.is_multi_type_choice;
            jv.visit_rule(rule)?;

            self.errors.append(&mut jv.errors);

            return Ok(());
          }
        }

        if group_rule_from_ident(self.cddl, ident).is_none() {
          self.add_error(format!(
            "rule {} must be a group rule to turn it into a choice",
            ident
          ));
          return Ok(());
        }

        self.is_group_to_choice_enum = true;
        self.visit_identifier(ident)?;
        self.is_group_to_choice_enum = false;

        Ok(())
      }
      Type2::ChoiceFromInlineGroup { group, .. } => {
        self.is_group_to_choice_enum = true;
        self.visit_group(group)?;
        self.is_group_to_choice_enum = false;
        Ok(())
      }
      Type2::Typename {
        ident,
        generic_args,
        ..
      } => {
        if let Some(ga) = generic_args {
          if let Some(rule) = rule_from_ident(self.cddl, ident) {
            if let Some(gr) = self
              .generic_rules
              .iter_mut()
              .find(|gr| gr.name == ident.ident)
            {
              for arg in ga.args.iter() {
                gr.args.push((*arg.arg).clone());
              }
            } else if let Some(params) = generic_params_from_rule(self.cddl, rule) {
              self.generic_rules.push(GenericRule {
                name: ident.ident,
                params,
                args: ga.args.iter().cloned().map(|arg| *arg.arg).collect(),
              });
            }

            let mut jv = JSONValidator::new(self.cddl, self.json.clone());
            jv.generic_rules = self.generic_rules.clone();
            jv.eval_generic_rule = Some(ident.ident);
            jv.is_multi_type_choice = self.is_multi_type_choice;
            jv.visit_rule(rule)?;

            self.errors.append(&mut jv.errors);

            return Ok(());
          }
        }

        self.visit_identifier(ident)
      }
      Type2::IntValue { value, .. } => self.visit_value(&token::Value::INT(*value)),
      Type2::UintValue { value, .. } => self.visit_value(&token::Value::UINT(*value)),
      Type2::FloatValue { value, .. } => self.visit_value(&token::Value::FLOAT(*value)),
      Type2::ParenthesizedType { pt, .. } => self.visit_type(pt),
      Type2::Any(_) => Ok(()),
      _ => {
        self.add_error(format!(
          "unsupported data type for validating JSON, got {}",
          t2
        ));
        Ok(())
      }
    }
  }

  fn visit_identifier(&mut self, ident: &Identifier<'a>) -> visitor::Result<ValidationError> {
    if let Some(name) = self.eval_generic_rule {
      if let Some(gr) = self
        .generic_rules
        .iter()
        .cloned()
        .find(|gr| gr.name == name)
      {
        for (idx, gp) in gr.params.iter().enumerate() {
          if *gp == ident.ident {
            if let Some(arg) = gr.args.get(idx) {
              return self.visit_type1(arg);
            }
          }
        }
      }
    }

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
            jv.generic_rules = self.generic_rules.clone();
            jv.eval_generic_rule = self.eval_generic_rule;
            jv.is_multi_type_choice = self.is_multi_type_choice;
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
            jv.generic_rules = self.generic_rules.clone();
            jv.eval_generic_rule = self.eval_generic_rule;
            jv.is_multi_type_choice = self.is_multi_type_choice;
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
        if let Some(cut_value) = self.cut_value.take() {
          self.add_error(format!(
            "cut present for member key {}. expected type {}, got {}",
            cut_value, ident, self.json
          ));
        } else {
          self.add_error(format!("expected type {}, got {}", ident, self.json));
        }
        Ok(())
      }
    }
  }

  fn visit_value_member_key_entry(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
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
      jv.generic_rules = self.generic_rules.clone();
      jv.eval_generic_rule = self.eval_generic_rule;
      jv.is_multi_type_choice = self.is_multi_type_choice;
      jv.json_location.push_str(&self.json_location);
      jv.visit_type(&entry.entry_type)?;

      self.json_location = current_location;
      if !jv.errors.is_empty() {
        if let Some(occur) = &self.occurence {
          if let Occur::Optional(_) | Occur::ZeroOrMore(_) = occur {
            if !self.is_cut_present {
              return Ok(());
            }
          }
        }
      }

      self.errors.append(&mut jv.errors);

      Ok(())
    } else {
      self.visit_type(&entry.entry_type)
    }
  }

  fn visit_memberkey(&mut self, mk: &MemberKey<'a>) -> visitor::Result<ValidationError> {
    if let MemberKey::Type1 { is_cut, .. } = mk {
      self.is_cut_present = *is_cut;
    }

    walk_memberkey(self, mk)
  }

  fn visit_value(&mut self, value: &token::Value<'a>) -> visitor::Result<ValidationError> {
    let error: Option<String> = match &self.json {
      Value::Number(n) => match value {
        token::Value::INT(v) => match n.as_i64() {
          Some(i) => match &self.ctrl {
            Some(Token::NE) if i != *v as i64 => None,
            Some(Token::LT) if i < *v as i64 => None,
            Some(Token::LE) if i <= *v as i64 => None,
            Some(Token::GT) if i > *v as i64 => None,
            Some(Token::GE) if i >= *v as i64 => None,
            None => {
              if i == *v as i64 {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            _ => Some(format!(
              "expected value {} {}, got {}",
              self.ctrl.clone().unwrap(),
              v,
              n
            )),
          },
          None => Some(format!("{} cannot be represented as an i64", n)),
        },
        token::Value::UINT(v) => match n.as_u64() {
          Some(i) => match &self.ctrl {
            Some(Token::NE) if i != *v as u64 => None,
            Some(Token::LT) if i < *v as u64 => None,
            Some(Token::LE) if i <= *v as u64 => None,
            Some(Token::GT) if i > *v as u64 => None,
            Some(Token::GE) if i >= *v as u64 => None,
            Some(Token::SIZE) if i < 256u64.pow(*v as u32) => None,
            None => {
              if i == *v as u64 {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            _ => Some(format!(
              "expected value {} {}, got {}",
              self.ctrl.clone().unwrap(),
              v,
              n
            )),
          },
          None => Some(format!("{} cannot be represented as a u64", n)),
        },
        token::Value::FLOAT(v) => match n.as_f64() {
          Some(f) => match &self.ctrl {
            Some(Token::NE) if (f - *v).abs() > std::f64::EPSILON => None,
            Some(Token::LT) if f < *v as f64 => None,
            Some(Token::LE) if f <= *v as f64 => None,
            Some(Token::GT) if f > *v as f64 => None,
            Some(Token::GE) if f >= *v as f64 => None,
            None => {
              if (f - *v).abs() < std::f64::EPSILON {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            _ => Some(format!(
              "expected value {} {}, got {}",
              self.ctrl.clone().unwrap(),
              v,
              n
            )),
          },
          None => Some(format!("{} cannot be represented as an i64", n)),
        },
        _ => Some(format!("expected {}, got {}", value, n)),
      },
      Value::String(s) => match value {
        token::Value::TEXT(t) => match &self.ctrl {
          Some(Token::NE) => {
            if s != t {
              None
            } else {
              Some(format!("expected {} .ne to \"{}\"", value, s))
            }
          }
          Some(Token::REGEXP) | Some(Token::PCRE) => {
            let re = regex::Regex::new(
              serde_json::from_str::<Value>(&format!("\"{}\"", t))
                .map_err(|e| ValidationError::from_validator(self, e.to_string()))?
                .as_str()
                .ok_or_else(|| {
                  ValidationError::from_validator(self, "malformed regex".to_string())
                })?,
            )
            .map_err(|e| ValidationError::from_validator(self, e.to_string()))?;

            if re.is_match(s) {
              None
            } else {
              Some(format!("expected \"{}\" to match regex \"{}\"", s, t))
            }
          }
          _ => {
            if s == t {
              None
            } else if let Some(ctrl) = &self.ctrl {
              Some(format!("expected value {} {}, got \"{}\"", ctrl, value, s))
            } else {
              Some(format!("expected value {} got \"{}\"", value, s))
            }
          }
        },
        token::Value::UINT(u) => match &self.ctrl {
          Some(Token::SIZE) => {
            if s.len() == *u {
              None
            } else {
              Some(format!("expected \"{}\" .size {}, got {}", s, u, s.len()))
            }
          }
          _ => Some(format!("expected {}, got {}", u, s)),
        },
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
            jv.generic_rules = self.generic_rules.clone();
            jv.eval_generic_rule = self.eval_generic_rule;
            jv.is_multi_type_choice = self.is_multi_type_choice;
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
            jv.generic_rules = self.generic_rules.clone();
            jv.eval_generic_rule = self.eval_generic_rule;
            jv.is_multi_type_choice = self.is_multi_type_choice;
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
          if self.is_cut_present {
            self.cut_value = Some(t);
          }

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

pub fn rule_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Option<&'a Rule<'a>> {
  cddl.rules.iter().find_map(|r| match r {
    Rule::Type { rule, .. } if rule.name == *ident && !rule.is_type_choice_alternate => Some(r),
    Rule::Group { rule, .. } if rule.name == *ident && !rule.is_group_choice_alternate => Some(r),
    _ => None,
  })
}

pub fn group_rule_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Option<&'a GroupRule<'a>> {
  cddl.rules.iter().find_map(|r| match r {
    Rule::Group { rule, .. } if rule.name == *ident && !rule.is_group_choice_alternate => {
      Some(rule.as_ref())
    }
    _ => None,
  })
}

pub fn generic_params_from_rule<'a>(cdd: &'a CDDL, rule: &Rule<'a>) -> Option<Vec<&'a str>> {
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{cddl_from_str, lexer_from_str};

  #[test]
  fn validate() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let input = r#"terminal-color = &basecolors<0..10>
    basecolors<v> = ( id: v )"#;
    let json = r#"11"#;

    let mut lexer = lexer_from_str(input);
    let cddl = cddl_from_str(&mut lexer, input, true)?;
    let json = serde_json::from_str::<Value>(json)?;

    let mut jv = JSONValidator::new(&cddl, json);
    jv.validate()?;

    Ok(())
  }
}
