use super::ast::*;
use super::token;
use serde_json;
use serde_json::Value;
use std::fmt;

pub type Result = std::result::Result<(), ValidationError>;

#[derive(Debug)]
pub enum ValidationError {
  CDDL(String),
  JSON(JSONError),
  Compilation(String),
}

#[derive(Debug)]
pub struct JSONError {
  expected: String,
  actual: String,
}

impl fmt::Display for ValidationError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ValidationError::CDDL(ce) => write!(f, "malformed CDDL: {}", ce),
      ValidationError::JSON(je) => write!(
        f,
        "failed to validate JSON against CDDL\nexpected: {}\nactual: {}",
        je.expected,
        serde_json::to_string_pretty(&je.actual).map_err(|_| std::fmt::Error)?
      ),
      ValidationError::Compilation(ce) => writeln!(f, "error on compilation: {}", ce),
    }
  }
}

pub fn validate_json(cddl: &CDDL, json: &Value) -> Result {
  for rule in cddl.rules.iter() {
    // First type rule is root
    if let Rule::Type(tr) = rule {
      return cddl.validate_type_rule(tr, None, json);
    }
  }

  Ok(())
}

impl<'a> CDDL<'a> {
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

  fn validate_type_rule(&self, tr: &TypeRule, occur: Option<&Occur>, json: &Value) -> Result {
    self.validate_type(&tr.value, occur, json)
  }

  fn validate_group_rule(&self, gr: &GroupRule, occur: Option<&Occur>, json: &Value) -> Result {
    self.validate_group_entry(&gr.entry, occur, json)
  }

  fn validate_type(&self, t: &Type, occur: Option<&Occur>, json: &Value) -> Result {
    // Find the first type choice that validates to true
    if t
      .0
      .iter()
      .any(|t1| self.validate_type1(t1, occur, json).is_ok())
    {
      return Ok(());
    }

    // TODO: need to retrieve the more granular error
    Err(ValidationError::JSON(JSONError {
      expected: t.to_string(),
      actual: json.to_string(),
    }))
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
          actual: json.to_string(),
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
        Value::Array(_) => validate_array_values(self, g, occur, json),
        _ => Err(ValidationError::JSON(JSONError {
          expected: t2.to_string(),
          actual: json.to_string(),
        })),
      },
      Type2::Map(g) => match json {
        Value::Object(_) => self.validate_group(g, occur, json),
        _ => Err(ValidationError::JSON(JSONError {
          expected: t2.to_string(),
          actual: json.to_string(),
        })),
      },
      _ => Err(ValidationError::CDDL(format!(
        "CDDL type {} can't be used to validate JSON {}",
        t2, json
      ))),
    }
  }

  fn validate_group(&self, g: &Group, occur: Option<&Occur>, json: &Value) -> Result {
    // Find the first group choice that validates to true
    if g
      .0
      .iter()
      .any(|gc| self.validate_group_choice(gc, occur, json).is_ok())
    {
      return Ok(());
    }

    // TODO: need to retrieve the more granular error
    Err(ValidationError::JSON(JSONError {
      expected: g.to_string(),
      actual: json.to_string(),
    }))
  }

  fn validate_group_choice(&self, gc: &GroupChoice, occur: Option<&Occur>, json: &Value) -> Result {
    for ge in gc.0.iter() {
      match json {
        Value::Array(values) => {
          if values
            .iter()
            .any(|v| self.validate_group_entry(ge, occur, v).is_ok())
          {
            continue;
          } else {
            return Err(ValidationError::JSON(JSONError {
              expected: gc.to_string(),
              actual: json.to_string(),
            }));
          }
        }
        Value::Object(_) => match self.validate_group_entry(ge, occur, json) {
          Ok(()) => continue,
          Err(e) => return Err(e),
        },
        _ => {
          return Err(ValidationError::JSON(JSONError {
            expected: gc.to_string(),
            actual: json.to_string(),
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
            // TODO: include same type hoisting logic here
            MemberKey::Type1(t1) => match &t1.0.type2 {
              Type2::Value(token::Value::TEXT(t)) => match json {
                // CDDL { "my-key" => tstr, } validates JSON { "my-key": "myvalue" }
                Value::Object(om) => {
                  if let Some(v) = om.get(*t) {
                    self.validate_type(&vmke.entry_type, occur, v)
                  } else {
                    Err(ValidationError::JSON(JSONError {
                      expected: ge.to_string(),
                      actual: json.to_string(),
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
            MemberKey::Bareword(ident) => {
              match json {
                Value::Object(om) => {
                  // TODO: this is a very kludgy way to hoist the group entry
                  if let Some(c) = vmke.entry_type.to_string().chars().next() {
                    if c != '['
                      && c != '{'
                      && c != '('
                      && !is_type_json_prelude(&vmke.entry_type.to_string())
                    {
                      if let Some(v) = om.get((ident.0).0) {
                        return self.validate_type(&vmke.entry_type, occur, v);
                      }

                      return self.validate_type(&vmke.entry_type, occur, json);
                    }
                  }

                  if let Some(v) = om.get((ident.0).0) {
                    self.validate_type(&vmke.entry_type, occur, v)
                  } else if let Some(o) = occur {
                    match o {
                      // If optional occurence, return Ok
                      Occur::Optional => Ok(()),
                      _ => Err(ValidationError::JSON(JSONError {
                        expected: ge.to_string(),
                        actual: json.to_string(),
                      })),
                    }
                  } else {
                    Err(ValidationError::JSON(JSONError {
                      expected: ge.to_string(),
                      actual: json.to_string(),
                    }))
                  }
                }
                _ => self.validate_type(&vmke.entry_type, occur, json),
              }
            }
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
      GroupEntry::InlineGroup((_, g)) => self.validate_group(g, occur, json),
    }
  }
}

fn expect_null(ident: &str) -> Result {
  match ident {
    "null" | "nil" => Ok(()),
    _ => Err(ValidationError::JSON(JSONError {
      expected: ident.to_string(),
      actual: Value::Null.to_string(),
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
          actual: json.to_string(),
        }));
      }

      Err(ValidationError::JSON(JSONError {
        expected: ident.to_string(),
        actual: json.to_string(),
      }))
    }
    _ => Err(ValidationError::JSON(JSONError {
      expected: ident.to_string(),
      actual: json.to_string(),
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
          actual: json.to_string(),
        })),
      },
      token::Value::FLOAT(f) => match n.as_f64() {
        Some(n64) if (n64 - f as f64).abs() < std::f64::EPSILON => Ok(()),
        _ => Err(ValidationError::JSON(JSONError {
          expected: v.to_string(),
          actual: json.to_string(),
        })),
      },
      _ => Ok(()),
    },
    _ => Err(ValidationError::JSON(JSONError {
      expected: v.to_string(),
      actual: json.to_string(),
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
            actual: json.to_string(),
          })
        })
        .map(|_| ()),
      "nint" => match n.as_i64() {
        Some(n64) if n64 < 0 => Ok(()),
        _ => Err(ValidationError::JSON(JSONError {
          expected: ident.to_string(),
          actual: json.to_string(),
        })),
      },
      "int" => n
        .as_i64()
        .ok_or_else(|| {
          ValidationError::JSON(JSONError {
            expected: ident.to_string(),
            actual: json.to_string(),
          })
        })
        .map(|_| ()),
      "number" => Ok(()),
      "float16" => match n.as_f64() {
        Some(_) => Ok(()),
        _ => Err(ValidationError::JSON(JSONError {
          expected: ident.to_string(),
          actual: json.to_string(),
        })),
      },
      // TODO: Finish rest of numerical data types
      "float32" => match n.as_f64() {
        Some(_) => Ok(()),
        _ => Err(ValidationError::JSON(JSONError {
          expected: ident.to_string(),
          actual: json.to_string(),
        })),
      },
      // TODO: Finish rest of numerical data types
      _ => Err(ValidationError::JSON(JSONError {
        expected: ident.to_string(),
        actual: json.to_string(),
      })),
    },
    _ => Err(ValidationError::JSON(JSONError {
      expected: ident.to_string(),
      actual: json.to_string(),
    })),
  }
}

fn validate_string_value(v: &token::Value, s: &str) -> Result {
  match *v {
    token::Value::TEXT(t) if t == s => Ok(()),
    _ => Err(ValidationError::JSON(JSONError {
      expected: v.to_string(),
      actual: Value::String(s.to_string()).to_string(),
    })),
  }
}

fn validate_array_values(cddl: &CDDL, g: &Group, occur: Option<&Occur>, json: &Value) -> Result {
  if let Value::Array(values) = json {
    // TODO: Add occurrence error handling
    if let Some(o) = occur {
      match o {
        Occur::ZeroOrMore => return cddl.validate_group(g, occur, json),
        Occur::Exact((l, u)) => {
          if let Some(li) = l {
            if let Some(ui) = u {
              if values.len() < *li || values.len() > *ui {
                return Err(ValidationError::JSON(JSONError {
                  expected: g.to_string(),
                  actual: json.to_string(),
                }));
              }

              return cddl.validate_group(g, occur, json);
            }

            if values.len() < *li {
              return Err(ValidationError::JSON(JSONError {
                expected: g.to_string(),
                actual: json.to_string(),
              }));
            }

            return cddl.validate_group(g, occur, json);
          }

          if let Some(ui) = u {
            if values.len() > *ui {
              return Err(ValidationError::JSON(JSONError {
                expected: g.to_string(),
                actual: json.to_string(),
              }));
            }

            return cddl.validate_group(g, occur, json);
          }
        }
        Occur::OneOrMore => {
          if values.is_empty() {
            return Err(ValidationError::JSON(JSONError {
              expected: g.to_string(),
              actual: json.to_string(),
            }));
          } else {
            return cddl.validate_group(g, occur, json);
          }
        }
        Occur::Optional => {
          if values.len() > 1 {
            return Err(ValidationError::JSON(JSONError {
              expected: g.to_string(),
              actual: json.to_string(),
            }));
          } else {
            return cddl.validate_group(g, occur, json);
          }
        }
      }
    }

    return cddl.validate_group(g, occur, json);
  }

  Err(ValidationError::JSON(JSONError {
    expected: g.to_string(),
    actual: json.to_string(),
  }))
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
