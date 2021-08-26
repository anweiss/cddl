use itertools::Itertools;
use pest_meta;

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
  is_dedent: bool,
) -> Result<Vec<Type2<'a>>, String> {
  let mut literals = Vec::new();
  match target {
    Type2::TextValue { value, .. } => match controller {
      // "testing" .cat "123"
      Type2::TextValue {
        value: controller, ..
      } => {
        if is_dedent {
          literals.push(format!("{}{}", dedent_str(value), dedent_str(controller)).into())
        } else {
          literals.push(format!("{}{}", value, controller).into())
        }
      }
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
          literals.append(&mut cat_operation(cddl, target, controller, is_dedent)?)
        }
      }
      // "testing" .cat '123'
      Type2::UTF8ByteString {
        value: controller, ..
      } => match std::str::from_utf8(controller) {
        Ok(controller) => {
          let controller = controller.trim_start_matches('\'').trim_end_matches('\'');

          if is_dedent {
            literals.push(format!("{}{}", dedent_str(value), dedent_str(controller)).into())
          } else {
            literals.push(format!("{}{}", value, controller).into())
          }
        }
        Err(e) => return Err(format!("error parsing byte string: {}", e)),
      },
      // "testing" .cat h'313233'
      Type2::B16ByteString {
        value: controller, ..
      } => match base16::decode(controller) {
        Ok(controller) => match String::from_utf8(controller) {
          Ok(controller) => {
            if is_dedent {
              literals.push(format!("{}{}", dedent_str(value), dedent_str(&controller)).into())
            } else {
              literals.push(format!("{}{}", value, controller).into())
            }
          }
          Err(e) => return Err(format!("error decoding utf-8: {}", e)),
        },
        Err(e) => return Err(format!("error decoding base16 byte string literal: {}", e)),
      },
      // "testing" .cat b64'MTIz'
      Type2::B64ByteString {
        value: controller, ..
      } => match base64::decode_config(controller, base64::URL_SAFE) {
        Ok(controller) => match String::from_utf8(controller) {
          Ok(controller) => {
            if is_dedent {
              literals.push(format!("{}{}", dedent_str(value), dedent_str(&controller)).into())
            } else {
              literals.push(format!("{}{}", value, controller).into())
            }
          }
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
            literals.append(&mut cat_operation(
              cddl,
              target,
              &controller.type1.type2,
              is_dedent,
            )?);
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
        literals.append(&mut cat_operation(cddl, value, controller, is_dedent)?);
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
          literals.append(&mut cat_operation(
            cddl,
            &tc.type1.type2,
            controller,
            is_dedent,
          )?);
        }
      }

      return Err("invalid target type in .cat control operator".to_string());
    }
    Type2::UTF8ByteString { value, .. } => match std::str::from_utf8(value) {
      Ok(value) => match controller {
        // 'testing' .cat "123"
        Type2::TextValue {
          value: controller, ..
        } => {
          let value = value.trim_start_matches('\'').trim_end_matches('\'');

          if is_dedent {
            literals.push(format!("{}{}", dedent_str(value), dedent_str(controller)).into())
          } else {
            literals.push(format!("{}{}", value, controller).into())
          }
        }
        Type2::Typename { ident, .. } => {
          let sl = string_literals_from_ident(cddl, ident);
          if sl.is_empty() {
            return Err(format!(
              "controller of type rule {} is not a string literal",
              ident
            ));
          }
          for controller in sl.iter() {
            literals.append(&mut cat_operation(cddl, target, controller, is_dedent)?)
          }
        }
        // 'testing' .cat '123
        Type2::UTF8ByteString {
          value: controller, ..
        } => match std::str::from_utf8(controller) {
          Ok(controller) => {
            let value = value.trim_start_matches('\'').trim_end_matches('\'');
            let controller = controller.trim_start_matches('\'').trim_end_matches('\'');

            if is_dedent {
              literals.push(format!("{}{}", dedent_str(value), dedent_str(controller)).into())
            } else {
              literals.push(format!("{}{}", value, controller).into())
            }
          }
          Err(e) => return Err(format!("error parsing byte string: {}", e)),
        },
        // 'testing' .cat h'313233'
        Type2::B16ByteString {
          value: controller, ..
        } => match base16::decode(controller) {
          Ok(controller) => match String::from_utf8(controller) {
            Ok(controller) => {
              let value = value.trim_start_matches('\'').trim_end_matches('\'');

              if is_dedent {
                literals.push(format!("{}{}", dedent_str(value), dedent_str(&controller)).into())
              } else {
                literals.push(format!("{}{}", value, controller).into())
              }
            }
            Err(e) => return Err(format!("error decoding utf-8: {}", e)),
          },
          Err(e) => return Err(format!("error decoding base16 byte string literal: {}", e)),
        },
        // 'testing' .cat b64'MTIz'
        Type2::B64ByteString {
          value: controller, ..
        } => match base64::decode_config(controller, base64::URL_SAFE) {
          Ok(controller) => match String::from_utf8(controller) {
            Ok(controller) => {
              let value = value.trim_start_matches('\'').trim_end_matches('\'');

              if is_dedent {
                literals.push(format!("{}{}", dedent_str(value), dedent_str(&controller)).into())
              } else {
                literals.push(format!("{}{}", value, controller).into())
              }
            }
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
              literals.append(&mut cat_operation(
                cddl,
                target,
                &controller.type1.type2,
                is_dedent,
              )?);
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
        let controller = if is_dedent {
          base16::encode_lower(dedent_str(controller).as_bytes())
        } else {
          base16::encode_lower(controller.as_bytes())
        };

        let concat = if is_dedent {
          [&dedent_bytes(value, false)?[..], controller.as_bytes()].concat()
        } else {
          [&value[..], controller.as_bytes()].concat()
        };
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
          literals.append(&mut cat_operation(cddl, target, controller, is_dedent)?)
        }
      }
      // h'74657374696E67' .cat '123'
      Type2::UTF8ByteString {
        value: controller, ..
      } => {
        let controller = if is_dedent {
          base16::encode_lower(&dedent_bytes(controller, true)?)
        } else {
          base16::encode_lower(&controller[..])
        };

        let concat = if is_dedent {
          [&dedent_bytes(value, false)?[..], controller.as_bytes()].concat()
        } else {
          [&value[..], controller.as_bytes()].concat()
        };

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
        let concat = if is_dedent {
          [
            &dedent_bytes(value, false)?[..],
            &dedent_bytes(controller, false)?[..],
          ]
          .concat()
        } else {
          [&value[..], &controller[..]].concat()
        };
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
          let concat = if is_dedent {
            [
              &dedent_bytes(value, false)?[..],
              dedent_str(&controller).as_bytes(),
            ]
            .concat()
          } else {
            [&value[..], controller.as_bytes()].concat()
          };
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
            literals.append(&mut cat_operation(
              cddl,
              target,
              &controller.type1.type2,
              is_dedent,
            )?);
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
          let concat = if is_dedent {
            [
              &dedent_bytes(&value, false)?[..],
              dedent_str(controller).as_bytes(),
            ]
            .concat()
          } else {
            [&value[..], controller.as_bytes()].concat()
          };

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
          literals.append(&mut cat_operation(cddl, target, controller, is_dedent)?)
        }
      }
      // b64'dGVzdGluZw==' .cat '123'
      Type2::UTF8ByteString {
        value: controller, ..
      } => match base64::decode_config(value, base64::URL_SAFE) {
        Ok(value) => {
          let concat = if is_dedent {
            [
              &dedent_bytes(&value, false)?[..],
              &dedent_bytes(controller, true)?[..],
            ]
            .concat()
          } else {
            [&value[..], &controller[..]].concat()
          };

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
            let concat = if is_dedent {
              [
                &dedent_bytes(&value, false)?[..],
                &dedent_bytes(&controller, false)?[..],
              ]
              .concat()
            } else {
              [&value[..], &controller[..]].concat()
            };
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
            let concat = if is_dedent {
              [
                &dedent_bytes(&value, false)?[..],
                &dedent_bytes(&controller, false)?[..],
              ]
              .concat()
            } else {
              [&value[..], &controller[..]].concat()
            };
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
            literals.append(&mut cat_operation(
              cddl,
              target,
              &controller.type1.type2,
              is_dedent,
            )?);
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

fn dedent_str(source: &str) -> String {
  // #[cfg(windows)]
  // let line_ending = "\r\n";
  // #[cfg(not(windows))]
  let line_ending = "\n";

  source
    .split(line_ending)
    .map(|l| l.trim_start())
    .join(line_ending)
}

fn dedent_bytes(source: &[u8], is_utf8_byte_string: bool) -> Result<Vec<u8>, String> {
  #[cfg(windows)]
  let line_ending = "\r\n";
  #[cfg(not(windows))]
  let line_ending = "\n";

  if is_utf8_byte_string {
    return Ok(
      std::str::from_utf8(source)
        .map_err(|e| e.to_string())?
        .trim_start_matches('\'')
        .trim_end_matches('\'')
        .split(line_ending)
        .map(|l| l.trim_start())
        .join(line_ending)
        .into_bytes(),
    );
  }

  Ok(
    std::str::from_utf8(source)
      .map_err(|e| e.to_string())?
      .split(line_ending)
      .map(|l| l.trim_start())
      .join(line_ending)
      .into_bytes(),
  )
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
              for v in plus_operation(cddl, &controller.type1.type2, nested_controller)?.iter() {
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
              for v in plus_operation(cddl, &controller.type1.type2, nested_controller)?.iter() {
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
              for v in plus_operation(cddl, &controller.type1.type2, nested_controller)?.iter() {
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
            for v in plus_operation(cddl, &tc.type1.type2, nested_controller)?.iter() {
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

pub fn validate_abnf(abnf: &str, target: &str) -> Result<(), String> {
  if let Some(idx) = abnf.find('\n') {
    let (rule, abnf) = abnf.split_at(idx);

    let rules = abnf_to_pest::parse_abnf(abnf).map_err(|e| e.to_string())?;
    let mut w = Vec::new();
    abnf_to_pest::render_rules_to_pest(rules)
      .render(0, &mut w)
      .unwrap();
    let pest = String::from_utf8(w).unwrap();

    let pairs = pest_meta::parser::parse(pest_meta::parser::Rule::grammar_rules, &pest)
      .map_err(|e| e.to_string())?;

    let ast = pest_meta::parser::consume_rules(pairs).unwrap();

    let vm = pest_vm::Vm::new(pest_meta::optimizer::optimize(ast));

    let rule = rule.replace("-", "_");
    let _ = vm.parse(&rule, target).map_err(|e| e.to_string())?;
  }

  Ok(())
}

#[cfg(test)]
mod tests {
  use crate::{ast::Span, cddl_from_str, lexer_from_str};

  use super::*;
  use indoc::indoc;

  #[test]
  fn test_cat() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl_str = indoc!(
      r#"
        a = "foo" .cat '
          bar
          baz
        '
      "#
    );

    let mut l = lexer_from_str(cddl_str);
    let cddl = cddl_from_str(&mut l, cddl_str, true)?;

    assert_eq!(
      cat_operation(
        &cddl,
        &Type2::from("foo".to_string()),
        &Type2::from(indoc!(
          r#"'
            bar
            baz
          '"#,
        )),
        false,
      )?,
      vec![Type2::TextValue {
        value: "foo\n  bar\n  baz\n".into(),
        span: Span::default(),
      }],
    );

    Ok(())
  }

  #[test]
  fn test_cat_with_dedent() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl_str = indoc!(
      r#"
        a = "foo" .det b
        b = '
          bar
          baz
        '
      "#
    );

    let mut l = lexer_from_str(cddl_str);
    let cddl = cddl_from_str(&mut l, cddl_str, true)?;

    assert_eq!(
      cat_operation(
        &cddl,
        &Type2::from("foo".to_string()),
        &Type2::Typename {
          ident: "b".into(),
          generic_args: None,
          span: Span::default(),
        },
        true,
      )?,
      vec![Type2::TextValue {
        value: "foo\nbar\nbaz\n".into(),
        span: Span::default(),
      }]
    );

    Ok(())
  }

  #[test]
  fn test_abnf() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let abnf_str = indoc!(
      r#"
        date-fullyear
        date-fullyear   = 4DIGIT
        date-month      = 2DIGIT  ; 01-12
        date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on
                                  ; month/year
        time-hour       = 2DIGIT  ; 00-23
        time-minute     = 2DIGIT  ; 00-59
        time-second     = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap sec
                                  ; rules
        time-secfrac    = "." 1*DIGIT
        time-numoffset  = ("+" / "-") time-hour ":" time-minute
        time-offset     = "Z" / time-numoffset

        partial-time    = time-hour ":" time-minute ":" time-second
                          [time-secfrac]
        full-date       = date-fullyear "-" date-month "-" date-mday
        full-time       = partial-time time-offset

        date-time       = full-date "T" full-time

        DIGIT          =  %x30-39 ; 0-9
        ; abbreviated here
      "#
    );

    validate_abnf(abnf_str, "2009")?;

    Ok(())
  }
}
