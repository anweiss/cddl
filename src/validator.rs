use super::ast::*;
use super::token;
use serde_json;
use serde_json::{Number, Value};
use std::error::Error;

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
    // Find the first type choice that validates to true
    if tr
      .value
      .0
      .iter()
      .any(|t1| self.validate_type1(t1, json).is_ok())
    {
      return Ok("Success".to_string());
    }

    Err("Type rule validation failed".into())
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
        _ => Err("Bad typename".into()),
      },
      _ => Err("Failed".into()),
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
}
