use crate::{
  ast::*,
  parser,
  validation::{CompilationError, Error, Result, Validator},
};
use serde_cbor::{self, Value};
use std::{f64, fmt};

/// Error type when validating CDDL
#[derive(Debug)]
pub struct CBORError {
  expected_memberkey: Option<String>,
  expected_value: String,
  actual_memberkey: Option<String>,
  actual_value: Value,
}

impl std::error::Error for CBORError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

impl fmt::Display for CBORError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let actual_value = serde_json::to_string_pretty(&self.actual_value).map_err(|_| fmt::Error)?;

    if let Some(emk) = &self.expected_memberkey {
      if let Some(amk) = &self.actual_memberkey {
        return write!(
          f,
          "expected: ( {} {} )\nactual: \"{}\": {}",
          emk, self.expected_value, amk, actual_value
        );
      }

      return write!(
        f,
        "expected: ( {} {} )\nactual: {}",
        emk, self.expected_value, actual_value
      );
    }

    if let Some(amk) = &self.actual_memberkey {
      return write!(
        f,
        "expected: ( {} )\nactual: {}: {}",
        self.expected_value, amk, actual_value
      );
    }

    write!(
      f,
      "expected: ( {} )\nactual: {}\n",
      self.expected_value, actual_value,
    )
  }
}

impl Into<Error> for CBORError {
  fn into(self) -> Error {
    Error::Target(Box::from(self))
  }
}

impl Validator<Value> for CDDL {
  fn validate(&self, value: &Value) -> Result {
    for r in self.rules.iter() {
      if let Rule::Type { rule, .. } = r {
        return self.validate_type_rule(rule, None, None, None, value);
      }
    }

    Ok(())
  }

  fn validate_rule_for_ident(
    &self,
    ident: &Identifier,
    is_enumeration: bool,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    occur: Option<&Occur>,
    value: &Value,
  ) -> Result {
    for rule in self.rules.iter() {
      match rule {
        Rule::Type { rule, .. } if rule.name.ident == ident.ident => {
          return self.validate_type_rule(&rule, expected_memberkey, actual_memberkey, occur, value)
        }
        Rule::Group { rule, .. } if rule.name.ident == ident.ident => {
          return self.validate_group_rule(&rule, is_enumeration, occur, value)
        }
        _ => continue,
      }
    }

    Err(Error::Syntax(format!(
      "No rule with name {} defined\n",
      ident.ident,
    )))
  }

  fn validate_type_rule(
    &self,
    tr: &TypeRule,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    occur: Option<&Occur>,
    value: &Value,
  ) -> Result {
    self.validate_type(
      &tr.value,
      expected_memberkey,
      actual_memberkey,
      occur,
      value,
    )
  }

  fn validate_group_rule(
    &self,
    gr: &GroupRule,
    is_enumeration: bool,
    occur: Option<&Occur>,
    value: &Value,
  ) -> Result {
    self.validate_group_entry(&gr.entry, is_enumeration, None, occur, value)
  }

  fn validate_type(
    &self,
    t: &Type,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    occur: Option<&Occur>,
    value: &Value,
  ) -> Result {
    let mut validation_errors: Vec<Error> = Vec::new();

    // Find the first type choice that validates to true
    let find_type_choice = |t1| match self.validate_type1(
      t1,
      expected_memberkey.clone(),
      actual_memberkey.clone(),
      occur,
      value,
    ) {
      Ok(()) => true,
      Err(e) => {
        validation_errors.push(e);
        false
      }
    };

    if t.type_choices.iter().any(find_type_choice) {
      return Ok(());
    }

    Err(Error::MultiError(validation_errors))
  }

  fn validate_type1(
    &self,
    t1: &Type1,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    occur: Option<&Occur>,
    value: &Value,
  ) -> Result {
    self.validate_type2(
      &t1.type2,
      expected_memberkey,
      actual_memberkey,
      occur,
      value,
    )
  }

  fn validate_range(
    &self,
    _lower: &Type2,
    _upper: &Type2,
    _is_inclusive: bool,
    _value: &Value,
  ) -> Result {
    unimplemented!()
  }

  fn validate_control_operator(
    &self,
    _target: &Type2,
    _operator: &'static str,
    _controller: &Type2,
    _value: &Value,
  ) -> Result {
    unimplemented!()
  }

  fn validate_type2(
    &self,
    t2: &Type2,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    occur: Option<&Occur>,
    value: &Value,
  ) -> Result {
    match t2 {
      Type2::TextValue { value: t, .. } => match value {
        Value::Text(s) if t == s => Ok(()),
        _ => Err(
          CBORError {
            expected_memberkey,
            expected_value: t2.to_string(),
            actual_memberkey,
            actual_value: value.clone(),
          }
          .into(),
        ),
      },
      Type2::IntValue { value: iv, .. } => match value {
        Value::Integer(i) if *iv as i128 == *i => Ok(()),
        _ => Err(
          CBORError {
            expected_memberkey,
            expected_value: t2.to_string(),
            actual_memberkey,
            actual_value: value.clone(),
          }
          .into(),
        ),
      },
      Type2::UintValue { value: uiv, .. } => match value {
        Value::Integer(i) if *uiv as u128 == *i as u128 => Ok(()),
        _ => Err(
          CBORError {
            expected_memberkey,
            expected_value: t2.to_string(),
            actual_memberkey,
            actual_value: value.clone(),
          }
          .into(),
        ),
      },
      Type2::FloatValue { value: fv, .. } => match value {
        Value::Float(f) if (fv - f).abs() < f64::EPSILON => Ok(()),
        _ => Err(
          CBORError {
            expected_memberkey,
            expected_value: t2.to_string(),
            actual_memberkey,
            actual_value: value.clone(),
          }
          .into(),
        ),
      },
      Type2::B16ByteString { value: bs, .. } => match value {
        Value::Bytes(b) if b == bs => Ok(()),
        _ => Err(
          CBORError {
            expected_memberkey,
            expected_value: t2.to_string(),
            actual_memberkey,
            actual_value: value.clone(),
          }
          .into(),
        ),
      },
      Type2::B64ByteString { value: bs, .. } => match value {
        Value::Bytes(b) if b == bs => Ok(()),
        _ => Err(
          CBORError {
            expected_memberkey,
            expected_value: t2.to_string(),
            actual_memberkey,
            actual_value: value.clone(),
          }
          .into(),
        ),
      },
      // TODO: evaluate genericarg
      Type2::Typename { ident, .. } => match value {
        Value::Null => expect_null(&ident.ident),
        Value::Bool(_) => self.expect_bool(&ident.ident, value),
        Value::Text(_) => {
          if ident.ident == "tstr" || ident.ident == "text" {
            Ok(())
          } else if is_type_prelude(&ident.ident) {
            // Expecting non-text type but got text
            Err(
              CBORError {
                expected_memberkey,
                expected_value: ident.ident.to_string(),
                actual_memberkey,
                actual_value: value.clone(),
              }
              .into(),
            )
          } else {
            self.validate_rule_for_ident(
              ident,
              false,
              expected_memberkey,
              actual_memberkey,
              occur,
              value,
            )
          }
        }
        Value::Integer(_) | Value::Float(_) => {
          self.validate_numeric_data_type(expected_memberkey, actual_memberkey, &ident.ident, value)
        }
        Value::Map(_) => self.validate_rule_for_ident(
          ident,
          false,
          expected_memberkey,
          actual_memberkey,
          occur,
          value,
        ),
        Value::Array(_) => self.validate_rule_for_ident(
          ident,
          false,
          expected_memberkey,
          actual_memberkey,
          occur,
          value,
        ),
        Value::Bytes(_) => {
          if ident.ident == "bstr" || ident.ident == "bytes" {
            Ok(())
          } else if is_type_prelude(&ident.ident) {
            // Expecting non-bytes type but got bytes
            Err(
              CBORError {
                expected_memberkey,
                expected_value: ident.ident.to_string(),
                actual_memberkey,
                actual_value: value.clone(),
              }
              .into(),
            )
          } else {
            self.validate_rule_for_ident(
              ident,
              false,
              expected_memberkey,
              actual_memberkey,
              occur,
              value,
            )
          }
        }
        _ => unimplemented!(),
      },
      Type2::Array { group, .. } => match value {
        Value::Array(_) => self.validate_group(group, occur, value),
        _ => Err(
          CBORError {
            expected_memberkey,
            expected_value: t2.to_string(),
            actual_memberkey,
            actual_value: value.clone(),
          }
          .into(),
        ),
      },
      Type2::Map { group, .. } => match value {
        Value::Map(_) => self.validate_group(group, occur, value),
        _ => Err(
          CBORError {
            expected_memberkey,
            expected_value: t2.to_string(),
            actual_memberkey,
            actual_value: value.clone(),
          }
          .into(),
        ),
      },
      Type2::ChoiceFromInlineGroup { group, .. } => {
        self.validate_group_to_choice_enum(group, occur, value)
      }
      Type2::ChoiceFromGroup { ident, .. } => self.validate_rule_for_ident(
        ident,
        true,
        expected_memberkey,
        actual_memberkey,
        occur,
        value,
      ),
      _ => Err(Error::Syntax(format!(
        "CDDL type {} can't be used to validate CBOR {:?}",
        t2, value
      ))),
    }
  }

  fn validate_group(&self, g: &Group, occur: Option<&Occur>, value: &Value) -> Result {
    let mut validation_errors: Vec<Error> = Vec::new();

    // Find the first group choice that validates to true
    if g
      .group_choices
      .iter()
      .any(|gc| match self.validate_group_choice(gc, occur, value) {
        Ok(()) => true,
        Err(e) => {
          validation_errors.push(e);
          false
        }
      })
    {
      return Ok(());
    }

    Err(Error::MultiError(validation_errors))
  }

  fn validate_group_to_choice_enum(
    &self,
    _g: &Group,
    _occur: Option<&Occur>,
    _value: &Value,
  ) -> Result {
    unimplemented!()
  }

  fn validate_group_choice(
    &self,
    gc: &GroupChoice,
    occur: Option<&Occur>,
    value: &Value,
  ) -> Result {
    let mut errors: Vec<Error> = Vec::new();

    for (ge_index, ge) in gc.group_entries.iter().enumerate() {
      match value {
        Value::Array(values) => {
          if let GroupEntry::TypeGroupname { ge: tge, .. } = &ge.0 {
            if let Some(o) = &tge.occur {
              if gc.group_entries.len() != 1 {
                // Arrays with multiple occurrences are too hard to parse
                // correctly.  For now, just return an error instead.
                return Err(
                  CBORError {
                    expected_memberkey: None,
                    expected_value: gc.to_string(),
                    actual_memberkey: None,
                    actual_value: value.clone(),
                  }
                  .into(),
                );
              }

              // Validate that the number of values is correct according to the
              // occurrence.
              self.validate_array_occurrence(o, &tge.name.to_string(), values)?;

              // Validate that each member of the value array matches
              // the groupentry.
              for value in values {
                self.validate_group_entry(&ge.0, false, None, occur, value)?;
              }
              return Ok(());
            }
          }

          if let GroupEntry::InlineGroup {
            occur: geo,
            group: g,
            ..
          } = &ge.0
          {
            if let Some(o) = geo {
              self.validate_array_occurrence(&o, &g.to_string(), values)?;
            }
          }

          let validate_all_entries =
            |v: &Value| match self.validate_group_entry(&ge.0, false, None, occur, v) {
              Ok(()) => true,
              Err(e) => {
                errors.push(e);

                false
              }
            };

          if let GroupEntry::TypeGroupname { ge: tge, .. } = &ge.0 {
            if self.rules.iter().any(|r| match r {
              Rule::Type { rule, .. } if rule.name.ident == tge.name.ident => true,
              _ => false,
            }) && values.iter().all(validate_all_entries)
            {
              return Ok(());
            }
          }

          // Match array element 1-on-1
          // first verify that the array lengths match.
          if values.len() != gc.group_entries.len() {
            return Err(
              CBORError {
                expected_memberkey: None,
                expected_value: gc.to_string(),
                actual_memberkey: None,
                actual_value: value.clone(),
              }
              .into(),
            );
          }
          let value_at_index = values.get(ge_index).unwrap();
          self.validate_group_entry(&ge.0, false, None, occur, value_at_index)?;
        }
        Value::Map(_) => {
          // Validate the object key/value pairs against each group entry,
          // collecting errors along the way
          match self.validate_group_entry(&ge.0, false, None, occur, value) {
            Ok(()) => continue,
            Err(e) => errors.push(e),
          }
        }
        _ => {
          return Err(
            CBORError {
              expected_memberkey: None,
              expected_value: gc.to_string(),
              actual_memberkey: None,
              actual_value: value.clone(),
            }
            .into(),
          );
        }
      }
    }

    if !errors.is_empty() {
      return Err(Error::MultiError(errors));
    }

    Ok(())
  }

  fn validate_group_entry(
    &self,
    ge: &GroupEntry,
    is_enumeration: bool,
    _wildcard_entry: Option<&Type>,
    occur: Option<&Occur>,
    value: &Value,
  ) -> Result {
    match ge {
      GroupEntry::ValueMemberKey { ge: vmke, .. } => {
        if let Some(mk) = &vmke.member_key {
          match mk {
            MemberKey::Type1 { t1, .. } => match &t1.type2 {
              Type2::TextValue { value: t, .. } => match value {
                // CDDL { "my-key" => tstr, } validates JSON { "my-key": "myvalue" }
                Value::Map(om) => {
                  if !is_type_prelude(&vmke.entry_type.to_string()) {
                    if let Some(v) = om.get(&Value::Text(t.to_string())) {
                      return self.validate_type(
                        &vmke.entry_type,
                        Some(mk.to_string()),
                        Some(t.to_string()),
                        occur,
                        v,
                      );
                    }

                    return self.validate_type(
                      &vmke.entry_type,
                      Some(mk.to_string()),
                      None,
                      occur,
                      value,
                    );
                  }

                  if let Some(v) = om.get(&Value::Text(t.to_string())) {
                    self.validate_type(
                      &vmke.entry_type,
                      Some(mk.to_string()),
                      Some(t.to_string()),
                      occur,
                      v,
                    )
                  } else {
                    Err(
                      CBORError {
                        expected_memberkey: Some(mk.to_string()),
                        expected_value: ge.to_string(),
                        actual_memberkey: None,
                        actual_value: value.clone(),
                      }
                      .into(),
                    )
                  }
                }
                // Otherwise, validate JSON against the type of the entry.
                // Matched when in an array and the key for the group entry is
                // ignored.
                // CDDL [ city: tstr, ] validates JSON [ "city" ]
                _ => self.validate_type(&vmke.entry_type, Some(mk.to_string()), None, occur, value),
              },
              // CDDL { * tstr => any } validates { "otherkey1": "anyvalue", "otherkey2": true }
              Type2::Typename { ident, .. } if ident.ident == "tstr" || ident.ident == "text" => {
                Ok(())
              }
              _ => Err(Error::Syntax(
                "CDDL member key must be quoted string or bareword for validating JSON objects"
                  .to_string(),
              )),
            },
            MemberKey::Bareword { ident, .. } => match value {
              Value::Map(om) => {
                if !is_type_prelude(&vmke.entry_type.to_string()) {
                  if let Some(v) = om.get(&Value::Text(ident.ident.to_string())) {
                    return self.validate_type(
                      &vmke.entry_type,
                      Some(mk.to_string()),
                      Some(ident.ident.to_string()),
                      vmke.occur.as_ref(),
                      v,
                    );
                  } // REVIEW NEEDED: should there be an "else" path that handles optional
                    // keys (vmke.occur) ?
                    // I don't understand why there is different behavior based on
                    // is_type_prelude().

                  return self.validate_type(
                    &vmke.entry_type,
                    Some(mk.to_string()),
                    None,
                    vmke.occur.as_ref(),
                    value,
                  );
                }

                match om.get(&Value::Text(ident.ident.to_string())) {
                  Some(v) => self.validate_type(
                    &vmke.entry_type,
                    Some(mk.to_string()),
                    Some(ident.ident.to_string()),
                    vmke.occur.as_ref(),
                    v,
                  ),
                  None => match &vmke.occur {
                    // We failed to find a matching key-value pair in the
                    // value map.  This is OK if the occurrence would allow
                    // it.
                    // Occurrences in a map have quirky behavior; see
                    // rfc8610 section 3.2.
                    // "? acts as expected: this key is optional
                    // "*" acts just like ?
                    // "+" has no effect; this key is required
                    Some(o) => match o {
                      Occur::Optional(_) | Occur::ZeroOrMore(_) => Ok(()),
                      _ => Err(
                        CBORError {
                          expected_memberkey: Some(mk.to_string()),
                          expected_value: format!("{} {}", mk, vmke.entry_type),
                          actual_memberkey: None,
                          actual_value: value.clone(),
                        }
                        .into(),
                      ),
                    },
                    None => Err(
                      CBORError {
                        expected_memberkey: Some(mk.to_string()),
                        expected_value: format!("{} {}", mk, vmke.entry_type),
                        actual_memberkey: None,
                        actual_value: value.clone(),
                      }
                      .into(),
                    ),
                  },
                }
              }
              _ => self.validate_type(
                &vmke.entry_type,
                Some(mk.to_string()),
                None,
                vmke.occur.as_ref(),
                value,
              ),
            },
            _ => Err(Error::Syntax(
              "CDDL member key must be quoted string or bareword for validating JSON objects"
                .to_string(),
            )),
          }
        } else {
          // TODO: Inline type
          unimplemented!()
        }
      }
      GroupEntry::TypeGroupname { ge: tge, span } => {
        if is_type_prelude(&tge.name.ident) {
          // Substitute a new AST node for the groupentry validation.
          // FIXME: this seems like an awkward thing to do.
          self.validate_type2(
            &Type2::Typename {
              ident: tge.name.clone(),
              generic_arg: tge.generic_arg.clone(),
              span: span.clone(),
            },
            None,
            None,
            None,
            value,
          )
        } else {
          self.validate_rule_for_ident(
            &tge.name,
            is_enumeration,
            None,
            None,
            tge.occur.as_ref(),
            value,
          )
        }
      }
      GroupEntry::InlineGroup {
        occur: igo,
        group: g,
        ..
      } => {
        if igo.is_some() {
          if is_enumeration {
            return self.validate_group_to_choice_enum(g, igo.as_ref(), value);
          }
          self.validate_group(g, igo.as_ref(), value)
        } else {
          if is_enumeration {
            return self.validate_group_to_choice_enum(g, occur, value);
          }
          self.validate_group(g, occur, value)
        }
      }
    }
  }

  fn validate_array_occurrence(&self, occur: &Occur, group: &str, values: &[Value]) -> Result {
    match occur {
      Occur::ZeroOrMore(_) | Occur::Optional(_) => Ok(()),
      Occur::OneOrMore(_) => {
        if values.is_empty() {
          Err(Error::Occurrence(format!(
            "Expecting one or more values of group {}",
            group
          )))
        } else {
          Ok(())
        }
      }
      Occur::Exact { lower, upper, .. } => {
        if let Some(li) = lower {
          if let Some(ui) = upper {
            if values.len() < *li || values.len() > *ui {
              if li == ui {
                return Err(Error::Occurrence(format!(
                  "Expecting exactly {} values of group {}. Got {} values",
                  li,
                  group,
                  values.len()
                )));
              }

              return Err(Error::Occurrence(format!(
                "Expecting between {} and {} values of group {}. Got {} values",
                li,
                ui,
                group,
                values.len()
              )));
            }
          }

          if values.len() < *li {
            return Err(Error::Occurrence(format!(
              "Expecting at least {} values of group {}. Got {} values",
              li,
              group,
              values.len()
            )));
          }
        }

        if let Some(ui) = upper {
          if values.len() > *ui {
            return Err(Error::Occurrence(format!(
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

  fn expect_bool(&self, ident: &str, value: &Value) -> Result {
    match value {
      Value::Bool(b) => {
        if ident == "bool" {
          return Ok(());
        }

        if let Ok(bfs) = ident.parse::<bool>() {
          if bfs == *b {
            return Ok(());
          }

          return Err(
            CBORError {
              expected_memberkey: None,
              expected_value: ident.to_string(),
              actual_memberkey: None,
              actual_value: value.clone(),
            }
            .into(),
          );
        }

        Err(
          CBORError {
            expected_memberkey: None,
            expected_value: ident.to_string(),
            actual_memberkey: None,
            actual_value: value.clone(),
          }
          .into(),
        )
      }
      _ => Err(
        CBORError {
          expected_memberkey: None,
          expected_value: ident.to_string(),
          actual_memberkey: None,
          actual_value: value.clone(),
        }
        .into(),
      ),
    }
  }

  fn validate_numeric_data_type(
    &self,
    expected_memberkey: Option<String>,
    actual_memberkey: Option<String>,
    ident: &str,
    value: &Value,
  ) -> Result {
    match value {
      Value::Integer(n) => match ident {
        "uint" => {
          if *n >= 0 {
            Ok(())
          } else {
            Err(
              CBORError {
                expected_memberkey,
                expected_value: ident.to_string(),
                actual_memberkey,
                actual_value: value.clone(),
              }
              .into(),
            )
          }
        }
        "nint" => {
          if *n < 0 {
            Ok(())
          } else {
            Err(
              CBORError {
                expected_memberkey,
                expected_value: ident.to_string(),
                actual_memberkey,
                actual_value: value.clone(),
              }
              .into(),
            )
          }
        }
        "int" | "number" => Ok(()),
        // TODO: Finish rest of numerical data types
        _ => Err(
          CBORError {
            expected_memberkey,
            expected_value: ident.to_string(),
            actual_memberkey,
            actual_value: value.clone(),
          }
          .into(),
        ),
      },
      Value::Float(_n) => match ident {
        "number" | "float" | "float16" | "float32" | "float64" => Ok(()),
        // TODO: Finish rest of numerical data types
        _ => Err(
          CBORError {
            expected_memberkey,
            expected_value: ident.to_string(),
            actual_memberkey,
            actual_value: value.clone(),
          }
          .into(),
        ),
      },
      _ => Err(
        CBORError {
          expected_memberkey,
          expected_value: ident.to_string(),
          actual_memberkey,
          actual_value: value.clone(),
        }
        .into(),
      ),
    }
  }
}

fn expect_null(ident: &str) -> Result {
  match ident {
    "null" | "nil" => Ok(()),
    _ => Err(
      CBORError {
        expected_memberkey: None,
        expected_value: ident.to_string(),
        actual_memberkey: None,
        actual_value: Value::Null,
      }
      .into(),
    ),
  }
}

fn is_type_prelude(t: &str) -> bool {
  match t {
    "any" | "uint" | "nint" | "int" | "bstr" | "bytes" | "tstr" | "text" | "tdate" | "time"
    | "number" | "biguint" | "bignint" | "bigint" | "integer" | "unsigned" | "decfrac"
    | "bigfloat" | "eb64url" | "eb64legacy" | "eb16" | "encoded-cbor" | "uri" | "b64url"
    | "b64legacy" | "regexp" | "mime-mesage" | "cbor-any" | "float16" | "float32" | "float64"
    | "float16-32" | "float32-64" | "float" | "false" | "true" | "bool" | "nil" | "null"
    | "undefined" => true,
    _ => false,
  }
}

/// Validates CBOR input against given CDDL input
pub fn validate_cbor_from_slice(cddl_input: &str, cbor_input: &[u8]) -> Result {
  validate_cbor(
    &parser::cddl_from_str(cddl_input)
      .map_err(|e| Error::Compilation(CompilationError::CDDL(e)))?,
    &serde_cbor::from_slice(cbor_input).map_err(|e| Error::Target(e.into()))?,
  )
}

fn validate_cbor<V: Validator<Value>>(cddl: &V, cbor: &Value) -> Result {
  cddl.validate(cbor)
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_cbor;

  #[test]
  fn validate_cbor_null() -> Result {
    let cbor_value = Value::Null;
    let cddl_input = r#"mynullrule = null"#;

    validate_cbor_from_slice(cddl_input, &serde_cbor::to_vec(&cbor_value).unwrap())
  }

  #[test]
  fn validate_cbor_bytes_value() -> Result {
    let cbor_value = Value::Bytes(b"68656c6c6f20776f726c64".to_vec());
    let cddl_input = r#"mybytesstring = h'68656c6c6f20776f726c64'"#;

    validate_cbor_from_slice(cddl_input, &serde_cbor::to_vec(&cbor_value).unwrap())
  }
}
