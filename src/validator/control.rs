use crate::{
  ast::{Identifier, Operator, RangeCtlOp, Rule, Type2, CDDL},
  token::ByteValue,
};

/// Retrieve all text strings and byte string literals from a given rule
/// identifier. Used for proposed .cat control operator.
pub fn string_literals_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Vec<&'a Type2<'a>> {
  let mut literals = Vec::new();
  for r in cddl.rules.iter() {
    if let Rule::Type { rule, .. } = r {
      if rule.name == *ident {
        for tc in rule.value.type_choices.iter() {
          match &tc.type1.type2 {
            t @ Type2::TextValue { .. }
            | t @ Type2::UTF8ByteString { .. }
            | t @ Type2::B16ByteString { .. }
            | t @ Type2::B64ByteString { .. } => literals.push(t),
            Type2::Typename { ident, .. } => {
              literals.append(&mut string_literals_from_ident(cddl, ident))
            }
            _ => continue,
          }
        }
      }
    }
  }

  literals
}

/// Retrieve all numeric values from a given rule identifier. Used for
/// proposed .cat control operator.
pub fn numeric_values_from_ident<'a>(cddl: &'a CDDL, ident: &Identifier) -> Vec<&'a Type2<'a>> {
  let mut literals = Vec::new();
  for r in cddl.rules.iter() {
    if let Rule::Type { rule, .. } = r {
      if rule.name == *ident {
        for tc in rule.value.type_choices.iter() {
          match &tc.type1.type2 {
            t @ Type2::IntValue { .. }
            | t @ Type2::UintValue { .. }
            | t @ Type2::FloatValue { .. } => literals.push(t),
            Type2::Typename { ident, .. } => {
              literals.append(&mut numeric_values_from_ident(cddl, ident))
            }
            _ => continue,
          }
        }
      }
    }
  }

  literals
}

/// Concatenate target and controller
pub fn cat_operation<'a>(
  cddl: &CDDL,
  target: &Type2,
  controller: &Type2,
) -> Result<Vec<Type2<'a>>, String> {
  let mut literals = Vec::new();
  match target {
    Type2::TextValue { value, .. } => match controller {
      // "testing" .cat "123"
      Type2::TextValue {
        value: controller, ..
      } => literals.push(format!("{}{}", value, controller).into()),
      // "testing" .cat a
      Type2::Typename { ident, .. } => {
        let sl = string_literals_from_ident(cddl, ident);
        if sl.is_empty() {
          return Err(format!(
            "controller of type rule {} is not a string literal",
            ident
          ));
        }
        for controller in sl.iter() {
          literals.append(&mut cat_operation(cddl, target, controller)?)
        }
      }
      // "testing" .cat '123'
      Type2::UTF8ByteString {
        value: controller, ..
      } => match std::str::from_utf8(controller) {
        Ok(controller) => literals.push(format!("{}{}", value, controller).into()),
        Err(e) => return Err(format!("error parsing byte string: {}", e)),
      },
      // "testing" .cat h'313233'
      Type2::B16ByteString {
        value: controller, ..
      } => match base16::decode(controller) {
        Ok(controller) => match String::from_utf8(controller) {
          Ok(controller) => literals.push(format!("{}{}", value, controller).into()),
          Err(e) => return Err(format!("error decoding utf-8: {}", e)),
        },
        Err(e) => return Err(format!("error decoding base16 byte string literal: {}", e)),
      },
      // "testing" .cat b64'MTIz'
      Type2::B64ByteString {
        value: controller, ..
      } => match base64::decode_config(controller, base64::URL_SAFE) {
        Ok(controller) => match String::from_utf8(controller) {
          Ok(controller) => literals.push(format!("{}{}", value, controller).into()),
          Err(e) => return Err(format!("error decoding utf-8: {}", e)),
        },
        Err(e) => {
          return Err(format!(
            "error decoding base64 encoded byte string literal: {}",
            e
          ))
        }
      },
      // "testing" .cat ( "123" / "1234" )
      Type2::ParenthesizedType { pt: controller, .. } => {
        for controller in controller.type_choices.iter() {
          if controller.type1.operator.is_none() {
            literals.append(&mut cat_operation(cddl, target, &controller.type1.type2)?);
          }
        }
      }
      _ => return Err("invalid controller used for .cat operation".to_string()),
    },
    // a .cat "123"
    Type2::Typename { ident, .. } => {
      // Only grab the first type choice literal from the target per
      // https://github.com/cbor-wg/cddl-control/issues/2#issuecomment-729253368
      if let Some(value) = string_literals_from_ident(cddl, ident).first() {
        literals.append(&mut cat_operation(cddl, value, controller)?);
      } else {
        return Err("invalid controller used for .cat operation".to_string());
      }
    }
    // ( "test" / "testing" ) .cat "123"
    Type2::ParenthesizedType { pt: target, .. } => {
      // Only grab the first type choice literal from the target per
      // https://github.com/cbor-wg/cddl-control/issues/2#issuecomment-729253368
      if let Some(tc) = target.type_choices.first() {
        // Ignore nested operator
        if tc.type1.operator.is_none() {
          literals.append(&mut cat_operation(cddl, &tc.type1.type2, controller)?);
        }
      }

      return Err("invalid target type in .cat control operator".to_string());
    }
    Type2::UTF8ByteString { value, .. } => match std::str::from_utf8(value) {
      Ok(value) => match controller {
        // 'testing' .cat "123"
        Type2::TextValue {
          value: controller, ..
        } => literals.push(format!("{}{}", value, controller).into()),
        Type2::Typename { ident, .. } => {
          let sl = string_literals_from_ident(cddl, ident);
          if sl.is_empty() {
            return Err(format!(
              "controller of type rule {} is not a string literal",
              ident
            ));
          }
          for controller in sl.iter() {
            literals.append(&mut cat_operation(cddl, target, controller)?)
          }
        }
        // 'testing' .cat '123
        Type2::UTF8ByteString {
          value: controller, ..
        } => match std::str::from_utf8(controller) {
          Ok(controller) => literals.push(format!("{}{}", value, controller).into()),
          Err(e) => return Err(format!("error parsing byte string: {}", e)),
        },
        // 'testing' .cat h'313233'
        Type2::B16ByteString {
          value: controller, ..
        } => match base16::decode(controller) {
          Ok(controller) => match String::from_utf8(controller) {
            Ok(controller) => literals.push(format!("{}{}", value, controller).into()),
            Err(e) => return Err(format!("error decoding utf-8: {}", e)),
          },
          Err(e) => return Err(format!("error decoding base16 byte string literal: {}", e)),
        },
        // 'testing' .cat b64'MTIz'
        Type2::B64ByteString {
          value: controller, ..
        } => match base64::decode_config(controller, base64::URL_SAFE) {
          Ok(controller) => match String::from_utf8(controller) {
            Ok(controller) => literals.push(format!("{}{}", value, controller).into()),
            Err(e) => return Err(format!("error decoding utf-8: {}", e)),
          },
          Err(e) => {
            return Err(format!(
              "error decoding base64 encoded byte string literal: {}",
              e
            ))
          }
        },
        // 'testing' .cat ( "123" / "1234" )
        Type2::ParenthesizedType { pt: controller, .. } => {
          for controller in controller.type_choices.iter() {
            if controller.type1.operator.is_none() {
              literals.append(&mut cat_operation(cddl, target, &controller.type1.type2)?);
            }
          }
        }
        _ => return Err("invalid controller used for .cat operation".to_string()),
      },
      Err(e) => return Err(format!("error parsing byte string: {}", e)),
    },
    Type2::B16ByteString { value, .. } => match controller {
      // h'74657374696E67' .cat "123"
      Type2::TextValue {
        value: controller, ..
      } => {
        let controller = base16::encode_lower(&controller.as_bytes()[..]);
        let concat = [&value[..], &controller.as_bytes()[..]].concat();
        match base16::decode(&concat) {
          // Ignore the decoded value
          Ok(_) => literals.push(ByteValue::B16(concat.into()).into()),
          Err(e) => return Err(format!("concatenated value is invalid base16: {}", e)),
        }
      }
      // h'74657374696E67' .cat b
      Type2::Typename { ident, .. } => {
        let sl = string_literals_from_ident(cddl, ident);
        if sl.is_empty() {
          return Err(format!(
            "controller of type rule {} is not a string literal",
            ident
          ));
        }
        for controller in sl.iter() {
          literals.append(&mut cat_operation(cddl, target, controller)?)
        }
      }
      // h'74657374696E67' .cat '123'
      Type2::UTF8ByteString {
        value: controller, ..
      } => {
        let controller = base16::encode_lower(&controller[..]);
        let concat = [&value[..], &controller.as_bytes()[..]].concat();
        match base16::decode(&concat) {
          // Ignore the decoded value
          Ok(_) => literals.push(ByteValue::B16(concat.into()).into()),
          Err(e) => return Err(format!("concatenated value is invalid base16: {}", e)),
        }
      }
      // h'74657374696E67' .cat h'313233'
      Type2::B16ByteString {
        value: controller, ..
      } => {
        let concat = [&value[..], &controller[..]].concat();
        match base16::decode(&concat) {
          // Ignore the decoded value
          Ok(_) => literals.push(ByteValue::B16(concat.into()).into()),
          Err(e) => return Err(format!("concatenated value is invalid base16: {}", e)),
        }
      }
      // h'74657374696E67' .cat b64'MTIz'
      Type2::B64ByteString {
        value: controller, ..
      } => match base64::decode_config(controller, base64::URL_SAFE) {
        Ok(controller) => {
          let controller = base16::encode_lower(&controller);
          let concat = [&value[..], &controller.as_bytes()[..]].concat();
          match base16::decode(&concat) {
            // Ignore the decoded value
            Ok(_) => literals.push(ByteValue::B16(concat.into()).into()),
            Err(e) => return Err(format!("concatenated value is invalid base16: {}", e)),
          }
        }
        Err(e) => return Err(format!("controller is invalid base64: {}", e)),
      },
      // h'74657374696E67' .cat ( "123" / "1234" )
      Type2::ParenthesizedType { pt: controller, .. } => {
        for controller in controller.type_choices.iter() {
          if controller.type1.operator.is_none() {
            literals.append(&mut cat_operation(cddl, target, &controller.type1.type2)?);
          }
        }
      }
      _ => return Err("invalid controller used for .cat operation".to_string()),
    },
    Type2::B64ByteString { value, .. } => match controller {
      // b64'dGVzdGluZw==' .cat "123"
      Type2::TextValue {
        value: controller, ..
      } => match base64::decode_config(value, base64::URL_SAFE) {
        Ok(value) => {
          let concat = [&value[..], &controller.as_bytes()[..]].concat();
          literals.push(
            ByteValue::B64(
              base64::encode_config(&concat, base64::URL_SAFE)
                .into_bytes()
                .into(),
            )
            .into(),
          )
        }
        Err(e) => return Err(format!("target is invalid base64: {}", e)),
      },
      // b64'dGVzdGluZw==' .cat b
      Type2::Typename { ident, .. } => {
        let sl = string_literals_from_ident(cddl, ident);
        if sl.is_empty() {
          return Err(format!(
            "controller of type rule {} is not a string literal",
            ident
          ));
        }
        for controller in sl.iter() {
          literals.append(&mut cat_operation(cddl, target, controller)?)
        }
      }
      // b64'dGVzdGluZw==' .cat '123'
      Type2::UTF8ByteString {
        value: controller, ..
      } => match base64::decode_config(value, base64::URL_SAFE) {
        Ok(value) => {
          let concat = [&value[..], &controller[..]].concat();
          literals.push(
            ByteValue::B64(
              base64::encode_config(&concat, base64::URL_SAFE)
                .into_bytes()
                .into(),
            )
            .into(),
          )
        }
        Err(e) => return Err(format!("target is invalid base64: {}", e)),
      },
      // b64'dGVzdGluZw==' .cat h'313233'
      Type2::B16ByteString {
        value: controller, ..
      } => match base64::decode_config(value, base64::URL_SAFE) {
        Ok(value) => match base16::decode(controller) {
          Ok(controller) => {
            let concat = [&value[..], &controller[..]].concat();
            literals.push(
              ByteValue::B64(
                base64::encode_config(&concat, base64::URL_SAFE)
                  .into_bytes()
                  .into(),
              )
              .into(),
            )
          }
          Err(e) => return Err(format!("controller is invalid base16: {}", e)),
        },
        Err(e) => return Err(format!("target is invalid base64: {}", e)),
      },
      // b64'dGVzdGluZw==' .cat b64'MTIz'
      Type2::B64ByteString {
        value: controller, ..
      } => match base64::decode_config(value, base64::URL_SAFE) {
        Ok(value) => match base64::decode_config(controller, base64::URL_SAFE) {
          Ok(controller) => {
            let concat = [&value[..], &controller[..]].concat();
            literals.push(
              ByteValue::B64(
                base64::encode_config(&concat, base64::URL_SAFE)
                  .into_bytes()
                  .into(),
              )
              .into(),
            )
          }
          Err(e) => return Err(format!("controller is invalid base64: {}", e)),
        },
        Err(e) => return Err(format!("target is invalid base64: {}", e)),
      },
      // b64'dGVzdGluZw==' .cat ( "123" / "1234" )
      Type2::ParenthesizedType { pt: controller, .. } => {
        for controller in controller.type_choices.iter() {
          if controller.type1.operator.is_none() {
            literals.append(&mut cat_operation(cddl, target, &controller.type1.type2)?);
          }
        }
      }
      _ => return Err("invalid controller used for .cat operation".to_string()),
    },
    _ => {
      return Err(format!(
        "invalid target used for .cat operation, got {}",
        target
      ))
    }
  }

  Ok(literals)
}

/// Numeric addition of target and controller
pub fn plus_operation<'a>(
  cddl: &CDDL,
  target: &Type2,
  controller: &Type2,
) -> Result<Vec<Type2<'a>>, String> {
  let mut values = Vec::new();
  match target {
    Type2::UintValue { value, .. } => match controller {
      Type2::UintValue {
        value: controller, ..
      } => values.push((value + controller).into()),
      Type2::IntValue {
        value: controller, ..
      } => values.push(((*value as isize + controller) as usize).into()),
      Type2::FloatValue {
        value: controller, ..
      } => values.push(((*value as isize + *controller as isize) as usize).into()),
      Type2::Typename { ident, .. } => {
        let nv = numeric_values_from_ident(cddl, ident);
        if nv.is_empty() {
          return Err(format!(
            "controller of type rule {} is not a numeric value",
            ident
          ));
        }
        for controller in nv.iter() {
          println!("controller: {}", controller);
          values.append(&mut plus_operation(cddl, target, controller)?)
        }
      }
      Type2::ParenthesizedType { pt: controller, .. } => {
        for controller in controller.type_choices.iter() {
          match &controller.type1.operator {
            Some(Operator {
              operator: RangeCtlOp::CtlOp { ctrl: ".plus", .. },
              type2: nested_controller,
              ..
            }) => {
              for v in plus_operation(cddl, &controller.type1.type2, &nested_controller)?.iter() {
                values.append(&mut plus_operation(cddl, target, v)?);
              }
            }
            None => values.append(&mut plus_operation(cddl, target, &controller.type1.type2)?),
            _ => return Err("nested operator must be .plus".to_string()),
          }
        }
      }
      _ => return Err("invalid controller used for .plus operation".to_string()),
    },
    Type2::IntValue { value, .. } => match controller {
      Type2::IntValue {
        value: controller, ..
      } => values.push((value + controller).into()),
      Type2::UintValue {
        value: controller, ..
      } => values.push((value + *controller as isize).into()),
      Type2::FloatValue {
        value: controller, ..
      } => values.push((value + *controller as isize).into()),
      Type2::Typename { ident, .. } => {
        let nv = numeric_values_from_ident(cddl, ident);
        if nv.is_empty() {
          return Err(format!(
            "controller of type rule {} is not a numeric value",
            ident
          ));
        }
        for controller in nv.iter() {
          println!("controller: {}", controller);
          values.append(&mut plus_operation(cddl, target, controller)?)
        }
      }
      Type2::ParenthesizedType { pt: controller, .. } => {
        for controller in controller.type_choices.iter() {
          match &controller.type1.operator {
            Some(Operator {
              operator: RangeCtlOp::CtlOp { ctrl: ".plus", .. },
              type2: nested_controller,
              ..
            }) => {
              for v in plus_operation(cddl, &controller.type1.type2, &nested_controller)?.iter() {
                values.append(&mut plus_operation(cddl, target, v)?);
              }
            }
            None => values.append(&mut plus_operation(cddl, target, &controller.type1.type2)?),
            _ => return Err("nested operator must be .plus".to_string()),
          }
        }
      }
      _ => return Err("invalid controller used for .plus operation".to_string()),
    },
    Type2::FloatValue { value, .. } => match controller {
      Type2::IntValue {
        value: controller, ..
      } => values.push((value + *controller as f64).into()),
      Type2::FloatValue {
        value: controller, ..
      } => values.push((value + controller).into()),
      Type2::Typename { ident, .. } => {
        let nv = numeric_values_from_ident(cddl, ident);
        if nv.is_empty() {
          return Err(format!(
            "controller of type rule {} is not a numeric value",
            ident
          ));
        }
        for controller in nv.iter() {
          println!("controller: {}", controller);
          values.append(&mut plus_operation(cddl, target, controller)?)
        }
      }
      Type2::ParenthesizedType { pt: controller, .. } => {
        for controller in controller.type_choices.iter() {
          match &controller.type1.operator {
            Some(Operator {
              operator: RangeCtlOp::CtlOp { ctrl: ".plus", .. },
              type2: nested_controller,
              ..
            }) => {
              for v in plus_operation(cddl, &controller.type1.type2, &nested_controller)?.iter() {
                values.append(&mut plus_operation(cddl, target, v)?);
              }
            }
            None => values.append(&mut plus_operation(cddl, target, &controller.type1.type2)?),
            _ => return Err("nested operator must be .plus".to_string()),
          }
        }
      }
      _ => return Err("invalid controller used for .plus operation".to_string()),
    },
    Type2::Typename { ident, .. } => {
      // Only grab the first type choice value from the target per
      // https://github.com/cbor-wg/cddl-control/issues/2#issuecomment-729253368
      if let Some(value) = numeric_values_from_ident(cddl, ident).first() {
        values.append(&mut plus_operation(cddl, value, controller)?);
      } else {
        return Err("invalid controller used for .plus operation".to_string());
      }
    }
    Type2::ParenthesizedType { pt: target, .. } => {
      // Only grab the first type choice value from the target per
      // https://github.com/cbor-wg/cddl-control/issues/2#issuecomment-729253368
      if let Some(tc) = target.type_choices.first() {
        match &tc.type1.operator {
          Some(Operator {
            operator: RangeCtlOp::CtlOp { ctrl: ".plus", .. },
            type2: nested_controller,
            ..
          }) => {
            for v in plus_operation(cddl, &tc.type1.type2, &nested_controller)?.iter() {
              values.append(&mut plus_operation(cddl, v, controller)?);
            }
          }
          None => values.append(&mut plus_operation(cddl, &tc.type1.type2, controller)?),
          _ => return Err("nested operator must be .plus".to_string()),
        }
      } else {
        return Err("invalid target type in .plus control operator".to_string());
      }
    }
    _ => {
      return Err(format!(
        "invalid target type in .plus control operator, got {}",
        target
      ))
    }
  }

  Ok(values)
}
