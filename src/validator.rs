#![cfg(feature = "std")]

use super::{ast::*, lexer::Lexer, parser::Parser, token};
use serde_json;
use serde_json::Value;
use std::{error::Error, f64, fmt, result};

pub type Result = result::Result<(), ValidationError>;

#[derive(Debug)]
pub enum ValidationError {
  CDDL(String),
  JSON(JSONError),
  Compilation(String),
  Occurrence(String),
  MultiError(Vec<ValidationError>),
}

#[derive(Debug)]
pub struct JSONError {
  expected: String,
  actual: Value,
}

impl fmt::Display for ValidationError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ValidationError::CDDL(ce) => write!(f, "malformed CDDL: {}", ce),
      ValidationError::JSON(je) => write!(
        f,
        "failed to validate JSON against CDDL\n\nexpected: {}\nactual: {}",
        je.expected,
        serde_json::to_string_pretty(&je.actual).map_err(|_| fmt::Error)?,
      ),
      ValidationError::Compilation(ce) => write!(f, "error on compilation: {}", ce),
      ValidationError::Occurrence(oe) => write!(f, "occurrence error: {}", oe),
      ValidationError::MultiError(me) => {
        let mut errors = String::new();

        for e in me.iter() {
          match e {
            // Temporary work around for nested MultiError's
            ValidationError::MultiError(_) => errors.push_str(&format!("{}", e)),
            _ => errors.push_str(&format!("{}\n", e)),
          }
        }

        write!(f, "{}", errors)
      }
    }
  }
}

impl Error for ValidationError {
  fn description(&self) -> &str {
    "ValidationError"
  }

  fn cause(&self) -> Option<&Error> {
    None
  }
}

pub fn validate_json_from_str(cddl: &str, json: &str) -> Result {
  let mut l = Lexer::new(cddl);

  let mut p = Parser::new(&mut l).map_err(|e| ValidationError::Compilation(e.to_string()))?;

  validate_json(
    &p.parse_cddl()
      .map_err(|e| ValidationError::Compilation(e.to_string()))?,
    &serde_json::from_str(json).map_err(|e| ValidationError::Compilation(e.to_string()))?,
  )
}

fn validate_json(cddl: &CDDL, json: &Value) -> Result {
  for rule in cddl.rules.iter() {
    // First type rule is root
    if let Rule::Type(tr) = rule {
      return cddl.validate_type_rule(tr, None, json);
    }
  }

  Ok(())
}

impl<'a> CDDL<'a> {
  // TODO: support socket plug evaluation
  fn validate_rule_for_ident(
    &self,
    ident: &Identifier,
    occur: Option<&Occur>,
    json: &Value,
  ) -> Result {
    for rule in self.rules.iter() {
      match rule {
        Rule::Type(tr) if tr.name == *ident => return self.validate_type_rule(tr, occur, json),
        Rule::Group(gr) if gr.name == *ident => return self.validate_group_rule(gr, occur, json),
        _ => continue,
      }
    }

    Err(ValidationError::CDDL(format!(
      "No rule with name {} defined\n",
      (ident.0).0
    )))
  }

  // TODO: support generic parameter and type choice alternative evaluation
  fn validate_type_rule(&self, tr: &TypeRule, occur: Option<&Occur>, json: &Value) -> Result {
    self.validate_type(&tr.value, occur, json)
  }

  // TODO: support generic parameter and group choice alternative evaluation
  fn validate_group_rule(&self, gr: &GroupRule, occur: Option<&Occur>, json: &Value) -> Result {
    self.validate_group_entry(&gr.entry, occur, json)
  }

  fn validate_type(&self, t: &Type, occur: Option<&Occur>, json: &Value) -> Result {
    let mut validation_errors: Vec<ValidationError> = Vec::new();

    // Find the first type choice that validates to true
    if t
      .0
      .iter()
      .any(|t1| match self.validate_type1(t1, occur, json) {
        Ok(()) => true,
        Err(e) => {
          validation_errors.push(e);
          false
        }
      })
    {
      return Ok(());
    }

    Err(ValidationError::MultiError(validation_errors))
  }

  fn validate_type1(&self, t1: &Type1, occur: Option<&Occur>, json: &Value) -> Result {
    self.validate_type2(&t1.type2, occur, json)
  }

  fn validate_type2(&self, t2: &Type2, occur: Option<&Occur>, json: &Value) -> Result {
    match t2 {
      Type2::Value(v) => match json {
        Value::Number(_) => validate_numeric_value(v, json),
        Value::String(s) => validate_string_value(v, s),
        _ => Err(ValidationError::JSON(JSONError {
          expected: t2.to_string(),
          actual: json.clone(),
        })),
      },
      // TODO: evaluate genericarg
      Type2::Typename((tn, _)) => match json {
        Value::Null => expect_null((tn.0).0),
        Value::Bool(_) => expect_bool((tn.0).0, json),
        Value::String(_) => {
          if (tn.0).0 == "tstr" || (tn.0).0 == "text" {
            Ok(())
          } else {
            self.validate_rule_for_ident(tn, occur, json)
          }
        }
        Value::Number(_) => validate_numeric_data_type((tn.0).0, json),
        Value::Object(_) => self.validate_rule_for_ident(tn, occur, json),
        Value::Array(_) => self.validate_rule_for_ident(tn, occur, json),
      },
      Type2::Array(g) => match json {
        Value::Array(_) => self.validate_group(g, occur, json),
        _ => Err(ValidationError::JSON(JSONError {
          expected: t2.to_string(),
          actual: json.clone(),
        })),
      },
      Type2::Map(g) => match json {
        Value::Object(_) => self.validate_group(g, occur, json),
        _ => Err(ValidationError::JSON(JSONError {
          expected: t2.to_string(),
          actual: json.clone(),
        })),
      },
      _ => Err(ValidationError::CDDL(format!(
        "CDDL type {} can't be used to validate JSON {}",
        t2, json
      ))),
    }
  }

  fn validate_group(&self, g: &Group, occur: Option<&Occur>, json: &Value) -> Result {
    let mut validation_errors: Vec<ValidationError> = Vec::new();

    // Find the first group choice that validates to true
    if g
      .0
      .iter()
      .any(|gc| match self.validate_group_choice(gc, occur, json) {
        Ok(()) => true,
        Err(e) => {
          validation_errors.push(e);
          false
        }
      })
    {
      return Ok(());
    }

    Err(ValidationError::MultiError(validation_errors))
  }

  fn validate_group_choice(&self, gc: &GroupChoice, occur: Option<&Occur>, json: &Value) -> Result {
    'geiter: for ge in gc.0.iter() {
      match json {
        Value::Array(values) => {
          if let GroupEntry::TypeGroupname(tge) = ge {
            if let Some(o) = &tge.occur {
              validate_array_occurrence(o, &tge.name.to_string(), values)?;
            }
          }

          if let GroupEntry::InlineGroup((geo, g)) = ge {
            if let Some(o) = geo {
              validate_array_occurrence(o, &g.to_string(), values)?;
            }
          }

          let mut errors: Vec<ValidationError> = Vec::new();

          for v in values.iter() {
            match self.validate_group_entry(ge, occur, v) {
              Ok(()) => continue 'geiter,
              Err(e) => errors.push(e),
            }
          }

          if !errors.is_empty() {
            return Err(ValidationError::MultiError(errors));
          }
        }
        Value::Object(_) => match self.validate_group_entry(ge, occur, json) {
          Ok(()) => continue,
          Err(e) => return Err(e),
        },
        _ => {
          return Err(ValidationError::JSON(JSONError {
            expected: gc.to_string(),
            actual: json.clone(),
          }))
        }
      }
    }

    Ok(())
  }

  fn validate_group_entry(&self, ge: &GroupEntry, occur: Option<&Occur>, json: &Value) -> Result {
    match ge {
      GroupEntry::ValueMemberKey(vmke) => {
        if let Some(mk) = &vmke.member_key {
          match mk {
            MemberKey::Type1(t1) => match &t1.0.type2 {
              Type2::Value(token::Value::TEXT(t)) => match json {
                // CDDL { "my-key" => tstr, } validates JSON { "my-key": "myvalue" }
                Value::Object(om) => {
                  if !is_type_json_prelude(&vmke.entry_type.to_string()) {
                    if let Some(v) = om.get(*t) {
                      return self.validate_type(&vmke.entry_type, occur, v);
                    }

                    return self.validate_type(&vmke.entry_type, occur, json);
                  }

                  if let Some(v) = om.get(*t) {
                    self.validate_type(&vmke.entry_type, occur, v)
                  } else {
                    Err(ValidationError::JSON(JSONError {
                      expected: ge.to_string(),
                      actual: json.clone(),
                    }))
                  }
                }
                // Otherwise, validate JSON against the type of the entry.
                // Matched when in an array and the key for the group entry is
                // ignored.
                // CDDL [ city: tstr, ] validates JSON [ "city" ]
                _ => self.validate_type(&vmke.entry_type, occur, json),
              },
              // CDDL { * tstr => any } validates { "otherkey1": "anyvalue", "otherkey2": true }
              Type2::Typename((ident, _)) if (ident.0).0 == "tstr" || (ident.0).0 == "text" => {
                Ok(())
              }
              _ => Err(ValidationError::CDDL(
                "CDDL member key must be quoted string or bareword for validating JSON objects"
                  .to_string(),
              )),
            },
            MemberKey::Bareword(ident) => match json {
              Value::Object(om) => {
                if !is_type_json_prelude(&vmke.entry_type.to_string()) {
                  if let Some(v) = om.get((ident.0).0) {
                    return self.validate_type(&vmke.entry_type, vmke.occur.as_ref(), v);
                  }

                  return self.validate_type(&vmke.entry_type, vmke.occur.as_ref(), json);
                }

                if let Some(v) = om.get((ident.0).0) {
                  return self.validate_type(&vmke.entry_type, vmke.occur.as_ref(), v);
                }

                if let Some(o) = occur {
                  match o {
                    // If optional occurence, return Ok
                    Occur::Optional | Occur::ZeroOrMore => return Ok(()),
                    _ => {
                      return Err(ValidationError::JSON(JSONError {
                        expected: ge.to_string(),
                        actual: json.clone(),
                      }))
                    }
                  }
                }

                self.validate_type(&vmke.entry_type, vmke.occur.as_ref(), json)
              }
              _ => self.validate_type(&vmke.entry_type, vmke.occur.as_ref(), json),
            },
            _ => Err(ValidationError::CDDL(
              "CDDL member key must be quoted string or bareword for validating JSON objects"
                .to_string(),
            )),
          }
        } else {
          // TODO: Inline type
          unimplemented!()
        }
      }
      GroupEntry::TypeGroupname(tge) => {
        self.validate_rule_for_ident(&tge.name, tge.occur.as_ref(), json)
      }
      GroupEntry::InlineGroup((igo, g)) => {
        if igo.is_some() {
          self.validate_group(g, igo.as_ref(), json)
        } else {
          self.validate_group(g, occur, json)
        }
      }
    }
  }
}

fn validate_array_occurrence(occur: &Occur, group: &str, values: &[Value]) -> Result {
  match occur {
    Occur::ZeroOrMore | Occur::Optional => Ok(()),
    Occur::OneOrMore => {
      if values.is_empty() {
        Err(ValidationError::Occurrence(format!(
          "Expecting one or more values of group {}",
          group
        )))
      } else {
        Ok(())
      }
    }
    Occur::Exact((l, u)) => {
      if let Some(li) = l {
        if let Some(ui) = u {
          if values.len() < *li || values.len() > *ui {
            if li == ui {
              return Err(ValidationError::Occurrence(format!(
                "Expecting exactly {} values of group {}. Got {} values",
                li,
                group,
                values.len()
              )));
            }

            return Err(ValidationError::Occurrence(format!(
              "Expecting between {} and {} values of group {}. Got {} values",
              li,
              ui,
              group,
              values.len()
            )));
          }
        }

        if values.len() < *li {
          return Err(ValidationError::Occurrence(format!(
            "Expecting at least {} values of group {}. Got {} values",
            li,
            group,
            values.len()
          )));
        }
      }

      if let Some(ui) = u {
        if values.len() > *ui {
          return Err(ValidationError::Occurrence(format!(
            "Expecting no more than {} values of group {}. Got {} values",
            ui,
            group,
            values.len()
          )));
        }
      }

      Ok(())
    }
  }
}

fn expect_null(ident: &str) -> Result {
  match ident {
    "null" | "nil" => Ok(()),
    _ => Err(ValidationError::JSON(JSONError {
      expected: ident.to_string(),
      actual: Value::Null,
    })),
  }
}

fn expect_bool(ident: &str, json: &Value) -> Result {
  match json {
    Value::Bool(b) => {
      if ident == "bool" {
        return Ok(());
      }

      if let Ok(bfs) = ident.parse::<bool>() {
        if bfs == *b {
          return Ok(());
        }

        return Err(ValidationError::JSON(JSONError {
          expected: ident.to_string(),
          actual: json.clone(),
        }));
      }

      Err(ValidationError::JSON(JSONError {
        expected: ident.to_string(),
        actual: json.clone(),
      }))
    }
    _ => Err(ValidationError::JSON(JSONError {
      expected: ident.to_string(),
      actual: json.clone(),
    })),
  }
}

fn validate_numeric_value(v: &token::Value, json: &Value) -> Result {
  match json {
    Value::Number(n) => match *v {
      token::Value::INT(i) => match n.as_i64() {
        Some(n64) if n64 == i as i64 => Ok(()),
        _ => Err(ValidationError::JSON(JSONError {
          expected: v.to_string(),
          actual: json.clone(),
        })),
      },
      token::Value::FLOAT(f) => match n.as_f64() {
        Some(n64) if (n64 - f as f64).abs() < f64::EPSILON => Ok(()),
        _ => Err(ValidationError::JSON(JSONError {
          expected: v.to_string(),
          actual: json.clone(),
        })),
      },
      _ => Ok(()),
    },
    _ => Err(ValidationError::JSON(JSONError {
      expected: v.to_string(),
      actual: json.clone(),
    })),
  }
}

fn validate_numeric_data_type(ident: &str, json: &Value) -> Result {
  match json {
    Value::Number(n) => match ident {
      "uint" => n
        .as_u64()
        .ok_or_else(|| {
          ValidationError::JSON(JSONError {
            expected: ident.to_string(),
            actual: json.clone(),
          })
        })
        .map(|_| ()),
      "nint" => match n.as_i64() {
        Some(n64) if n64 < 0 => Ok(()),
        _ => Err(ValidationError::JSON(JSONError {
          expected: ident.to_string(),
          actual: json.clone(),
        })),
      },
      "int" => n
        .as_i64()
        .ok_or_else(|| {
          ValidationError::JSON(JSONError {
            expected: ident.to_string(),
            actual: json.clone(),
          })
        })
        .map(|_| ()),
      "number" => Ok(()),
      "float16" => match n.as_f64() {
        Some(_) => Ok(()),
        _ => Err(ValidationError::JSON(JSONError {
          expected: ident.to_string(),
          actual: json.clone(),
        })),
      },
      // TODO: Finish rest of numerical data types
      "float32" => match n.as_f64() {
        Some(_) => Ok(()),
        _ => Err(ValidationError::JSON(JSONError {
          expected: ident.to_string(),
          actual: json.clone(),
        })),
      },
      // TODO: Finish rest of numerical data types
      _ => Err(ValidationError::JSON(JSONError {
        expected: ident.to_string(),
        actual: json.clone(),
      })),
    },
    _ => Err(ValidationError::JSON(JSONError {
      expected: ident.to_string(),
      actual: json.clone(),
    })),
  }
}

fn validate_string_value(v: &token::Value, s: &str) -> Result {
  match *v {
    token::Value::TEXT(t) if t == s => Ok(()),
    _ => Err(ValidationError::JSON(JSONError {
      expected: v.to_string(),
      actual: Value::String(s.to_string()),
    })),
  }
}

fn is_type_json_prelude(t: &str) -> bool {
  match t {
    "any" | "uint" | "nint" | "tstr" | "text" | "number" | "float16" | "float32" | "float64"
    | "float16-32" | "float32-64" | "float" | "false" | "true" | "bool" | "nil" | "null" => true,
    _ => false,
  }
}

#[cfg(test)]
mod tests {
  use super::super::{lexer::Lexer, parser::Parser};
  use super::*;
  use serde_json;

  #[test]
  fn validate_json_null() -> Result {
    let json_input = r#"null"#;

    let cddl_input = r#"mynullrule = null"#;

    let mut l = Lexer::new(cddl_input);
    let mut p = Parser::new(&mut l).map_err(|e| ValidationError::Compilation(e.to_string()))?;

    let cddl = p
      .parse_cddl()
      .map_err(|e| ValidationError::Compilation(e.to_string()))?;

    validate_json(
      &cddl,
      &serde_json::from_str(json_input).map_err(|e| ValidationError::Compilation(e.to_string()))?,
    )?;

    Ok(())
  }

  #[test]
  fn validate_json_bool() -> Result {
    let json_input = r#"true"#;

    let cddl_input = r#"myboolrule = true"#;

    let mut l = Lexer::new(cddl_input);
    let mut p = Parser::new(&mut l).map_err(|e| ValidationError::Compilation(e.to_string()))?;

    let cddl = p
      .parse_cddl()
      .map_err(|e| ValidationError::Compilation(e.to_string()))?;

    validate_json(
      &cddl,
      &serde_json::from_str(json_input).map_err(|e| ValidationError::Compilation(e.to_string()))?,
    )?;

    Ok(())
  }

  #[test]
  fn validate_json_number() -> Result {
    let json_inputs = [r#"3"#, r#"1.5"#, r#"10"#];

    let cddl_input = r#"mynumericrule = 3 / 1.5 / 10"#;

    let mut l = Lexer::new(cddl_input);
    let mut p = Parser::new(&mut l).map_err(|e| ValidationError::Compilation(e.to_string()))?;

    let cddl = p
      .parse_cddl()
      .map_err(|e| ValidationError::Compilation(e.to_string()))?;

    for ji in json_inputs.iter() {
      validate_json(
        &cddl,
        &serde_json::from_str(ji).map_err(|e| ValidationError::Compilation(e.to_string()))?,
      )?;
    }

    Ok(())
  }

  #[test]
  fn validate_json_string() -> Result {
    let json_input = r#""mystring""#;

    let cddl_input = r#"mystringrule = "mystring""#;

    let mut l = Lexer::new(cddl_input);
    let mut p = Parser::new(&mut l).map_err(|e| ValidationError::Compilation(e.to_string()))?;

    let cddl = p
      .parse_cddl()
      .map_err(|e| ValidationError::Compilation(e.to_string()))?;

    validate_json(
      &cddl,
      &serde_json::from_str(json_input).map_err(|e| ValidationError::Compilation(e.to_string()))?,
    )?;

    Ok(())
  }

  #[test]
  fn validate_json_object() -> Result {
    let json_input = r#"{
      "mykey": "myvalue",
      "myarray": [
        {
          "myotherkey": "myothervalue"
        }
      ]
    }"#;

    let cddl_input = r#"myobject = {
      mykey: tstr,
      myarray: [* arraytype],
    }
    
    arraytype = {
      myotherkey: tstr,
    }"#;

    let mut l = Lexer::new(cddl_input);
    let mut p = Parser::new(&mut l).map_err(|e| ValidationError::Compilation(e.to_string()))?;

    let cddl = p
      .parse_cddl()
      .map_err(|e| ValidationError::Compilation(e.to_string()))?;

    validate_json(
      &cddl,
      &serde_json::from_str(json_input).map_err(|e| ValidationError::Compilation(e.to_string()))?,
    )?;

    Ok(())
  }

  #[test]
  fn validate_json_array() -> Result {
    let json_input = r#"[
      "item1",
      {
        "longitude": 1234,
        "latitude": 3947
      }
    ]"#;

    let cddl_input = r#"Geography = [
      city           : tstr,
      gpsCoordinates : GpsCoordinates,
    ]

    GpsCoordinates = {
      longitude      : uint,            ; degrees, scaled by 10^7
      latitude       : uint,            ; degrees, scaled by 10^7
    }"#;

    let mut l = Lexer::new(cddl_input);
    let mut p = Parser::new(&mut l).map_err(|e| ValidationError::Compilation(e.to_string()))?;

    let cddl = p
      .parse_cddl()
      .map_err(|e| ValidationError::Compilation(e.to_string()))?;

    validate_json(
      &cddl,
      &serde_json::from_str(json_input).map_err(|e| ValidationError::Compilation(e.to_string()))?,
    )?;

    Ok(())
  }
}
