use super::ast::*;
use super::token;
use serde_json;
use serde_json::{Number, Value};
use std::error::Error;
use half;

pub fn validate_json(cddl: &CDDL, json: &Value) -> Result<String, Box<Error>> {
  for rule in cddl.rules.iter() {
    // First type rule is root
    if let Rule::Type(tr) = rule {
      return cddl.validate_type_rule(tr, json);
    }
  }
  Ok("Success".to_string())
}

impl<'a> CDDL<'a> {
  fn validate_type_rule(&self, tr: &TypeRule, json: &Value) -> Result<String, Box<Error>> {
    self.validate_type(&tr.value, json)
  }

  fn validate_type(&self, t: &Type, json: &Value) -> Result<String, Box<Error>> {
    let mut errors: Vec<Box<Error>> = Vec::new();

    // Find the first type choice that validates to true
    for t1 in t.0.iter() {
      match self.validate_type1(t1, json) {
        Ok(s) => return Ok(s),
        Err(e) => errors.push(e)
      }
    }

    let mut error_chain = String::new();

    for e in errors.iter() {
      error_chain.push_str(&format!("Type rule validation error: {}\n", e))
    }

    Err(error_chain.into())
  }

  fn validate_type1(&self, t1: &Type1, json: &Value) -> Result<String, Box<Error>> {
    self.validate_type2(&t1.type2, json)
  }

  fn validate_type2(&self, t2: &Type2, json: &Value) -> Result<String, Box<Error>> {
    match t2 {
      Type2::Value(v) => match json {
        Value::Number(n) => validate_numeric_value(v, n),
        Value::String(s) => validate_string_value(v, s),
        _ => Err("Bad value".into()),
      },
      // TODO: evaluate genericarg
      Type2::Typename((tn, _)) => match json {
        Value::Null => validate_null_value(tn),
        Value::Bool(b) => validate_bool_value(tn, *b),
        // TODO: Refactor into JSON data type limited prelude fn
        Value::String(s) => if (tn.0).0 != "tstr" && (tn.0).0 != "text" {
          Err(format!("Expecting data type \"{}\". Got JSON string \"{}\"", (tn.0).0, s).into())
        } else {
          Ok("Success".to_string())
        }
        Value::Number(n) => validate_numeric_data_type((tn.0).0, n),
        _ => Err("Bad typename".into()),
      },
      Type2::Array(g) => match json {
        Value::Array(values) => validate_array_values(self, g, values),
        _ => Err("Value not array".into()),
      },
      Type2::Map(g) => match json {
        Value::Object(_) => self.validate_group(g, json),
        _ => Err("Value not object".into()),
      }
      _ => Err("Failed".into()),
    }
  }

  fn validate_group(&self, g: &Group, json: &Value) -> Result<String, Box<Error>> {
    let mut errors: Vec<Box<Error>> = Vec::new();

    match json {
      Value::Object(om) => {
        for gc in g.0.iter() {
          match self.validate_group_choice(gc, om) {
            Ok(s) => return Ok(s),
            Err(e) => errors.push(e),
          }
        }

        let mut error_chain = String::from("No group choices passed validation\n");
    
        for e in errors.iter() {
          error_chain.push_str(&format!("Group choice validation error: {}", e))
        }

        Err(error_chain.into())
      }
      _ => Err("Group validation failed".into()),
    }
  }

  fn validate_group_choice(
    &self,
    g: &GroupChoice,
    om: &serde_json::Map<String, Value>,
  ) -> Result<String, Box<Error>> {
    for ge in g.0.iter() {
      self.validate_group_entry(ge, om)?;
    }

    Ok("Group choice validation success".into())
  }

  fn validate_group_entry(
    &self,
    ge: &GroupEntry,
    om: &serde_json::Map<String, Value>,
  ) -> Result<String, Box<Error>> {
    match ge {
      GroupEntry::ValueMemberKey(vmke) => {
        if let Some(mk) = &vmke.member_key {
          match mk {
            MemberKey::Bareword(Identifier((ident, _))) => {
              if let Some(v) = om.get(*ident) {
                return self.validate_type(&vmke.entry_type, v);
              }

              Err(
                format!("JSON object does not contain key name {}", ident).into(),
              )
            }
            _ => Err("Group entry validation failed".into()),
          }
        } else {
          Err("Group entry validation failed".into())
        }
      }
      _ => Err("Group entry validation failed".into()),
    }
  }
}

fn validate_null_value(ident: &Identifier) -> Result<String, Box<Error>> {
  if (ident.0).0 == "null" {
    return Ok("Success".to_string());
  }

  Err("Ident not null".into())
}

fn validate_bool_value(ident: &Identifier, b: bool) -> Result<String, Box<Error>> {
  if let Ok(bfs) = (ident.0).0.parse::<bool>() {
    if bfs == b {
      return Ok("Success".to_string());
    }

    return Err("Bool not match".into());
  }

  Err("Unable to parse bool".into())
}

fn validate_numeric_value(v: &token::Value, n: &Number) -> Result<String, Box<Error>> {
  match *v {
    token::Value::INT(i) => match n.as_i64() {
      Some(n64) if n64 == i as i64 => Ok("Int value matches".to_string()),
      _ => Err("Int value does not match".into()),
    },
    token::Value::FLOAT(f) => match n.as_f64() {
      Some(n64) if (n64 - f as f64).abs() < std::f64::EPSILON => {
        Ok("Float value matches".to_string())
      }
      _ => Err("Float value does not match".into()),
    },
    _ => Ok("Success".to_string()),
  }
}

fn validate_numeric_data_type(ident: &str, n: &Number) -> Result<String, Box<Error>> {
  match ident {
    "uint" => n.as_u64().ok_or(format!("Number {} is not of type uint", n).into()).map(|i| i.to_string()),
    "nint" => match n.as_i64() {
      Some(n64) if n64 < 0 => Ok("Number is of type nint".to_string()),
      _ => Err(format!("Number {} is not of type nint", n).into()),
    }
    "int" => n.as_i64().ok_or(format!("Number {} is not of type int", n).into()).map(|i| i.to_string()),
    "number" => Ok("Number is of type number".to_string()),
    "float16" => match n.as_f64() {
      Some(n64) => Ok(format!("Number is of type float16. Truncated float16 value: {}", half::f16::from_f64(n64)).to_string()),
      _ => Err(format!("Number {} is not of type float16", n).into()),
    }
    // TODO: Finish rest of numerical data types
    _ => Err("Expected non-numeric data type. Got JSON number instead".into()),
  }
}

fn validate_string_value(v: &token::Value, s: &str) -> Result<String, Box<Error>> {
  match *v {
    token::Value::TEXT(t) if t == s => Ok("Text value matches".to_string()),
    token::Value::TEXT(t) => Err(
      format!(
        "token value text {} does not match json value string {}",
        t, s
      )
      .into(),
    ),
    _ => Err("token value not TEXT".into()),
  }
}

fn validate_array_values(cddl: &CDDL, g: &Group, values: &[Value]) -> Result<String, Box<Error>> {
  for v in values.iter() {
    cddl.validate_group(g, v)?;
  }

  Ok("Success".to_string())
}

#[cfg(test)]
mod tests {
  use super::super::{lexer::Lexer, parser::Parser};
  use super::*;
  use serde_json;

  #[test]
  fn validate_json_null() -> Result<(), Box<Error>> {
    let json_input = r#"null"#;

    let cddl_input = r#"mynullrule = null"#;

    let mut l = Lexer::new(cddl_input);
    let mut p = Parser::new(&mut l)?;

    let cddl = p.parse_cddl()?;

    validate_json(&cddl, &serde_json::from_str(json_input)?)?;

    Ok(())
  }

  #[test]
  fn validate_json_bool() -> Result<(), Box<Error>> {
    let json_input = r#"true"#;

    let cddl_input = r#"myboolrule = true"#;

    let mut l = Lexer::new(cddl_input);
    let mut p = Parser::new(&mut l)?;

    let cddl = p.parse_cddl()?;

    validate_json(&cddl, &serde_json::from_str(json_input)?)?;

    Ok(())
  }

  #[test]
  fn validate_json_number() -> Result<(), Box<Error>> {
    let json_inputs = [r#"3"#, r#"1.5"#, r#"10"#];

    let cddl_input = r#"mynumericrule = 3 / 1.5 / 10"#;

    let mut l = Lexer::new(cddl_input);
    let mut p = Parser::new(&mut l)?;

    let cddl = p.parse_cddl()?;

    for ji in json_inputs.iter() {
      validate_json(&cddl, &serde_json::from_str(ji)?)?;
    }

    Ok(())
  }

  #[test]
  fn validate_json_string() -> Result<(), Box<Error>> {
    let json_input = r#""mystring""#;

    let cddl_input = r#"mystringrule = "mystring""#;

    let mut l = Lexer::new(cddl_input);
    let mut p = Parser::new(&mut l)?;

    let cddl = p.parse_cddl()?;

    validate_json(&cddl, &serde_json::from_str(json_input)?)?;

    Ok(())
  }

  #[test]
  fn validate_json_object() -> Result<(), Box<Error>> {
    let json_input = r#"{
      "mykey": "myvalue"
    }"#;

    let cddl_input = r#"myobject = {
      mykey: tstr,
    }"#;

    let mut l = Lexer::new(cddl_input);
    let mut p = Parser::new(&mut l)?;

    let cddl = p.parse_cddl()?;

    validate_json(&cddl, &serde_json::from_str(json_input)?)?;

    Ok(())
  }
}
