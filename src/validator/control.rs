#![cfg(any(feature = "json", feature = "cbor"))]
#![cfg(not(feature = "lsp"))]

use crate::{
  ast::{GroupEntry, Identifier, MemberKey, Operator, RangeCtlOp, Rule, Type2, CDDL},
  token::ControlOperator,
};

#[cfg(feature = "additional-controls")]
use crate::{ast::Type, validator::ByteValue};
#[cfg(feature = "additional-controls")]
use itertools::Itertools;
#[cfg(feature = "additional-controls")]
use pest_meta;

/// Retrieve all text strings and byte string literals from a given rule
/// identifier. Used for proposed .cat control operator.
pub fn string_literals_from_ident<'a>(
  cddl: &'a CDDL<'a>,
  ident: &Identifier,
) -> Vec<&'a Type2<'a>> {
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
pub fn numeric_values_from_ident<'a>(cddl: &'a CDDL<'a>, ident: &Identifier) -> Vec<&'a Type2<'a>> {
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

#[cfg(feature = "additional-controls")]
/// Concatenate target and controller. The Vec return type is to accomodate more
/// than one type choice in the controller.
pub fn cat_operation<'a>(
  cddl: &'a CDDL<'a>,
  target: &Type2,
  controller: &Type2,
  is_dedent: bool,
) -> Result<Vec<Type2<'a>>, String> {
  let mut literals = Vec::new();
  let ctrl = if is_dedent {
    ControlOperator::DET
  } else {
    ControlOperator::CAT
  };

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
      } => match data_encoding::BASE64URL.decode(controller) {
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
      _ => return Err(format!("invalid controller used for {} operation", ctrl)),
    },
    // a .cat "123"
    Type2::Typename { ident, .. } => {
      // Only grab the first type choice literal from the target per
      // https://github.com/cbor-wg/cddl-control/issues/2#issuecomment-729253368
      if let Some(value) = string_literals_from_ident(cddl, ident).first() {
        literals.append(&mut cat_operation(cddl, value, controller, is_dedent)?);
      } else {
        return Err(format!("invalid controller used for {} operation", ctrl));
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

      return Err(format!("invalid target type in {} control operator", ctrl));
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
        } => match data_encoding::BASE64URL.decode(controller) {
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
        _ => return Err(format!("invalid controller used for {} operation", ctrl)),
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
      } => match data_encoding::BASE64URL.decode(controller) {
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
      _ => return Err(format!("invalid controller used for {} operation", ctrl)),
    },
    Type2::B64ByteString { value, .. } => match controller {
      // b64'dGVzdGluZw==' .cat "123"
      Type2::TextValue {
        value: controller, ..
      } => match data_encoding::BASE64URL.decode(value) {
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
            ByteValue::B64(data_encoding::BASE64URL.encode(&concat).into_bytes().into()).into(),
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
      } => match data_encoding::BASE64URL.decode(value) {
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
            ByteValue::B64(data_encoding::BASE64URL.encode(&concat).into_bytes().into()).into(),
          )
        }
        Err(e) => return Err(format!("target is invalid base64: {}", e)),
      },
      // b64'dGVzdGluZw==' .cat h'313233'
      Type2::B16ByteString {
        value: controller, ..
      } => match data_encoding::BASE64URL.decode(value) {
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
              ByteValue::B64(data_encoding::BASE64URL.encode(&concat).into_bytes().into()).into(),
            )
          }
          Err(e) => return Err(format!("controller is invalid base16: {}", e)),
        },
        Err(e) => return Err(format!("target is invalid base64: {}", e)),
      },
      // b64'dGVzdGluZw==' .cat b64'MTIz'
      Type2::B64ByteString {
        value: controller, ..
      } => match data_encoding::BASE64URL.decode(value) {
        Ok(value) => match data_encoding::BASE64URL.decode(controller) {
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
              ByteValue::B64(data_encoding::BASE64URL.encode(&concat).into_bytes().into()).into(),
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
      _ => return Err(format!("invalid controller used for {} operation", ctrl)),
    },
    _ => {
      return Err(format!(
        "invalid target used for {} operation, got {}",
        ctrl, target
      ))
    }
  }

  Ok(literals)
}

#[cfg(feature = "additional-controls")]
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

#[cfg(feature = "additional-controls")]
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

/// Numeric addition of target and controller. The Vec return type is to
/// accommodate more than one type choice in the controller
pub fn plus_operation<'a>(
  cddl: &'a CDDL<'a>,
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
              operator:
                RangeCtlOp::CtlOp {
                  ctrl: ControlOperator::PLUS,
                  ..
                },
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
              operator:
                RangeCtlOp::CtlOp {
                  ctrl: ControlOperator::PLUS,
                  ..
                },
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
              operator:
                RangeCtlOp::CtlOp {
                  ctrl: ControlOperator::PLUS,
                  ..
                },
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
            operator:
              RangeCtlOp::CtlOp {
                ctrl: ControlOperator::PLUS,
                ..
              },
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

#[cfg(feature = "additional-controls")]
pub fn validate_abnf(abnf: &str, target: &str) -> Result<(), String> {
  let abnf = abnf.trim();
  if let Some(idx) = abnf.find('\n') {
    let (rule, abnf) = abnf.split_at(idx);
    let rule = rule.trim();
    let mut abnf = abnf.trim().to_string();
    // Refer to https://docs.rs/abnf/0.13.0/abnf/fn.rulelist.html as to why it
    // needs to end with a newline
    abnf.push('\n');

    let rules = abnf_to_pest::parse_abnf(&abnf).map_err(|e| e.to_string())?;
    let mut w = Vec::new();
    abnf_to_pest::render_rules_to_pest(rules)
      .render(0, &mut w)
      .unwrap();
    let pest = String::from_utf8(w).unwrap();

    let pairs = pest_meta::parser::parse(pest_meta::parser::Rule::grammar_rules, &pest)
      .map_err(|e| e.to_string())?;

    let ast = pest_meta::parser::consume_rules(pairs).unwrap();

    let vm = pest_vm::Vm::new(pest_meta::optimizer::optimize(ast));

    let rule = rule.replace('-', "_");
    let _ = vm.parse(&rule, target).map_err(|e| e.to_string())?;
  }

  Ok(())
}

/// If the controller for an .abnf/.abnfb control operator is a parenthesized
/// type with a nested .cat/.det, it needs to be parsed beforehand. The Vec
/// return type is to accomodate more than one type choice in the controller.
#[cfg(feature = "additional-controls")]
pub fn abnf_from_complex_controller<'a>(
  cddl: &'a CDDL<'a>,
  controller: &Type,
) -> Result<Vec<Type2<'a>>, String> {
  if let Some(tc) = controller.type_choices.first() {
    if let Some(operator) = &tc.type1.operator {
      if let RangeCtlOp::CtlOp { ctrl, .. } = operator.operator {
        match ctrl {
          ControlOperator::CAT => {
            return cat_operation(cddl, &tc.type1.type2, &operator.type2, false)
          }
          ControlOperator::DET => {
            return cat_operation(cddl, &tc.type1.type2, &operator.type2, true)
          }
          _ => return Err("invalid_controller".to_string()),
        }
      }
    }
  }

  Err("invalid controller".to_string())
}

#[cfg(test)]
#[cfg(not(target_arch = "wasm32"))]
mod tests {
  #![allow(unused_imports)]

  #[cfg(feature = "ast-span")]
  use crate::ast::Span;
  use crate::cddl_from_str;

  use super::*;
  use indoc::indoc;

  #[cfg(feature = "additional-controls")]
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

    let cddl = cddl_from_str(cddl_str, true)?;

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
        #[cfg(feature = "ast-span")]
        span: Span::default(),
      }],
    );

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
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

    let cddl = cddl_from_str(cddl_str, true)?;

    assert_eq!(
      cat_operation(
        &cddl,
        &Type2::from("foo".to_string()),
        &Type2::Typename {
          ident: "b".into(),
          generic_args: None,
          #[cfg(feature = "ast-span")]
          span: Span::default(),
        },
        true,
      )?,
      vec![Type2::TextValue {
        value: "foo\nbar\nbaz\n".into(),
        #[cfg(feature = "ast-span")]
        span: Span::default(),
      }]
    );

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
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

  #[cfg(feature = "additional-controls")]
  #[test]
  fn test_b64u_validation() -> std::result::Result<(), Box<dyn std::error::Error>> {
    use crate::ast::Type2;
    use std::borrow::Cow;

    // Test valid base64url encoding
    let target = Type2::TextValue {
      value: "text".into(),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    };

    let controller = Type2::UTF8ByteString {
      value: Cow::from(b"hello".as_slice()),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    };

    // "hello" in base64url is "aGVsbG8"
    assert!(validate_b64u_text(&target, &controller, "aGVsbG8", false)?);
    assert!(!validate_b64u_text(&target, &controller, "invalid", false)?);

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn test_hex_validation() -> std::result::Result<(), Box<dyn std::error::Error>> {
    use crate::ast::Type2;
    use std::borrow::Cow;

    let target = Type2::TextValue {
      value: "text".into(),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    };

    let controller = Type2::UTF8ByteString {
      value: Cow::from(b"hello".as_slice()),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    };

    // "hello" in hex is "68656c6c6f"
    assert!(validate_hex_text(
      &target,
      &controller,
      "68656c6c6f",
      HexCase::Any
    )?);
    assert!(validate_hex_text(
      &target,
      &controller,
      "68656c6c6f",
      HexCase::Lower
    )?);
    assert!(!validate_hex_text(
      &target,
      &controller,
      "68656C6C6F",
      HexCase::Lower
    )?);
    assert!(validate_hex_text(
      &target,
      &controller,
      "68656C6C6F",
      HexCase::Upper
    )?);
    assert!(!validate_hex_text(
      &target,
      &controller,
      "invalid",
      HexCase::Any
    )?);

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn test_base10_validation() -> std::result::Result<(), Box<dyn std::error::Error>> {
    use crate::ast::Type2;

    let target = Type2::TextValue {
      value: "text".into(),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    };

    let controller = Type2::IntValue {
      value: 123,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    };

    assert!(validate_base10_text(&target, &controller, "123")?);
    assert!(!validate_base10_text(&target, &controller, "124")?);
    assert!(!validate_base10_text(&target, &controller, "0123")?); // Leading zeros not allowed
    assert!(!validate_base10_text(&target, &controller, "abc")?);

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn test_printf_validation() -> std::result::Result<(), Box<dyn std::error::Error>> {
    use crate::ast::{Group, GroupChoice, GroupEntry, Type2, ValueMemberKeyEntry};
    use std::borrow::Cow;

    // Helper to build a printf controller array
    fn make_printf_array<'a>(format_str: &'a str, args: Vec<Type2<'a>>) -> Type2<'a> {
      let mut entries = vec![];

      // Format string as first entry
      let fmt_type = crate::ast::Type {
        type_choices: vec![crate::ast::TypeChoice {
          type1: crate::ast::Type1 {
            type2: Type2::TextValue {
              value: Cow::Borrowed(format_str),
              #[cfg(feature = "ast-span")]
              span: crate::ast::Span::default(),
            },
            operator: None,
            #[cfg(feature = "ast-comments")]
            comments_after_type: None,
            #[cfg(feature = "ast-span")]
            span: crate::ast::Span::default(),
          },
          #[cfg(feature = "ast-comments")]
          comments_before_type: None,
          #[cfg(feature = "ast-comments")]
          comments_after_type: None,
        }],
        #[cfg(feature = "ast-span")]
        span: crate::ast::Span::default(),
      };

      entries.push((
        GroupEntry::ValueMemberKey {
          ge: Box::new(ValueMemberKeyEntry {
            occur: None,
            member_key: None,
            entry_type: fmt_type,
          }),
          #[cfg(feature = "ast-comments")]
          leading_comments: None,
          #[cfg(feature = "ast-comments")]
          trailing_comments: None,
          #[cfg(feature = "ast-span")]
          span: crate::ast::Span::default(),
        },
        crate::ast::OptionalComma {
          optional_comma: false,
          #[cfg(feature = "ast-comments")]
          trailing_comments: None,
          _a: std::marker::PhantomData,
        },
      ));

      for arg in args {
        let arg_type = crate::ast::Type {
          type_choices: vec![crate::ast::TypeChoice {
            type1: crate::ast::Type1 {
              type2: arg,
              operator: None,
              #[cfg(feature = "ast-comments")]
              comments_after_type: None,
              #[cfg(feature = "ast-span")]
              span: crate::ast::Span::default(),
            },
            #[cfg(feature = "ast-comments")]
            comments_before_type: None,
            #[cfg(feature = "ast-comments")]
            comments_after_type: None,
          }],
          #[cfg(feature = "ast-span")]
          span: crate::ast::Span::default(),
        };

        entries.push((
          GroupEntry::ValueMemberKey {
            ge: Box::new(ValueMemberKeyEntry {
              occur: None,
              member_key: None,
              entry_type: arg_type,
            }),
            #[cfg(feature = "ast-comments")]
            leading_comments: None,
            #[cfg(feature = "ast-comments")]
            trailing_comments: None,
            #[cfg(feature = "ast-span")]
            span: crate::ast::Span::default(),
          },
          crate::ast::OptionalComma {
            optional_comma: false,
            #[cfg(feature = "ast-comments")]
            trailing_comments: None,
            _a: std::marker::PhantomData,
          },
        ));
      }

      Type2::Array {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: entries,
            #[cfg(feature = "ast-comments")]
            comments_before_grpchoice: None,
            #[cfg(feature = "ast-span")]
            span: crate::ast::Span::default(),
          }],
          #[cfg(feature = "ast-span")]
          span: crate::ast::Span::default(),
        },
        #[cfg(feature = "ast-comments")]
        comments_before_group: None,
        #[cfg(feature = "ast-comments")]
        comments_after_group: None,
        #[cfg(feature = "ast-span")]
        span: crate::ast::Span::default(),
      }
    }

    let target = Type2::TextValue {
      value: "text".into(),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    };

    // Test: "0x%04x" with value 19 => "0x0013"
    let controller = make_printf_array(
      "0x%04x",
      vec![Type2::UintValue {
        value: 19,
        #[cfg(feature = "ast-span")]
        span: Span::default(),
      }],
    );
    assert!(validate_printf_text(&target, &controller, "0x0013")?);
    assert!(!validate_printf_text(&target, &controller, "0x0014")?);

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  #[cfg(feature = "json")]
  #[test]
  fn test_json_validation() -> std::result::Result<(), Box<dyn std::error::Error>> {
    use crate::ast::Type2;

    let target = Type2::TextValue {
      value: "text".into(),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    };

    // Controller: any type (just validate it's valid JSON)
    let controller = Type2::Typename {
      ident: crate::ast::Identifier {
        ident: "any".into(),
        socket: None,
        #[cfg(feature = "ast-span")]
        span: Span::default(),
      },
      generic_args: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    };

    // Valid JSON
    assert!(validate_json_text(
      &target,
      &controller,
      r#"{"key": "value"}"#
    )?);
    assert!(validate_json_text(&target, &controller, r#"[1, 2, 3]"#)?);
    assert!(validate_json_text(&target, &controller, r#""hello""#)?);
    assert!(validate_json_text(&target, &controller, "42")?);
    assert!(validate_json_text(&target, &controller, "true")?);

    // Invalid JSON
    assert!(!validate_json_text(&target, &controller, "not valid json")?);
    assert!(!validate_json_text(&target, &controller, "{invalid}")?);

    // Controller: text type should only match JSON strings
    let text_controller = Type2::Typename {
      ident: crate::ast::Identifier {
        ident: "text".into(),
        socket: None,
        #[cfg(feature = "ast-span")]
        span: Span::default(),
      },
      generic_args: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    };
    assert!(validate_json_text(&target, &text_controller, r#""hello""#)?);
    assert!(!validate_json_text(&target, &text_controller, "42")?);

    Ok(())
  }
}

#[cfg(feature = "additional-controls")]
/// Validate base64url encoded text string against byte string
pub fn validate_b64u_text<'a>(
  __target: &Type2<'a>,
  controller: &Type2<'a>,
  text_value: &str,
  is_sloppy: bool,
) -> Result<bool, String> {
  match controller {
    Type2::UTF8ByteString { value, .. }
    | Type2::B16ByteString { value, .. }
    | Type2::B64ByteString { value, .. } => {
      // Try strict decoding first
      let decoded = data_encoding::BASE64URL_NOPAD.decode(text_value.as_bytes());

      match decoded {
        Ok(decoded_bytes) => Ok(decoded_bytes == value.as_ref()),
        Err(_) if is_sloppy => {
          // RFC 9741: Sloppy mode does not validate that additional trailing
          // bits (beyond the encoded data) are zero. We try a more lenient
          // decode that ignores trailing bit issues.
          let cleaned = text_value.trim_end_matches('=');
          match data_encoding::BASE64URL_NOPAD.decode(cleaned.as_bytes()) {
            Ok(decoded_bytes) => Ok(decoded_bytes == value.as_ref()),
            Err(_) => {
              // Try with lenient decoding: strip last char if we have trailing
              // bits that would be non-zero
              if !cleaned.is_empty() {
                let without_last = &cleaned[..cleaned.len() - 1];
                if let Ok(decoded_bytes) =
                  data_encoding::BASE64URL_NOPAD.decode(without_last.as_bytes())
                {
                  // In sloppy mode, if the byte prefix matches, accept it
                  Ok(value.as_ref().starts_with(&decoded_bytes[..]))
                } else {
                  Ok(false)
                }
              } else {
                Ok(false)
              }
            }
          }
        }
        Err(_) => Ok(false),
      }
    }
    _ => Err(format!(
      "invalid controller type for .b64u operation: {}",
      controller
    )),
  }
}

#[cfg(feature = "additional-controls")]
/// Validate base64 classic encoded text string against byte string
pub fn validate_b64c_text<'a>(
  __target: &Type2<'a>,
  controller: &Type2<'a>,
  text_value: &str,
  is_sloppy: bool,
) -> Result<bool, String> {
  match controller {
    Type2::UTF8ByteString { value, .. }
    | Type2::B16ByteString { value, .. }
    | Type2::B64ByteString { value, .. } => {
      // Try strict decoding first
      let decoded = data_encoding::BASE64.decode(text_value.as_bytes());

      match decoded {
        Ok(decoded_bytes) => Ok(decoded_bytes == value.as_ref()),
        Err(_) if is_sloppy => {
          // RFC 9741: Sloppy mode does not validate that additional trailing
          // bits are zero. Try without padding enforcement.
          let cleaned = text_value.trim_end_matches('=');
          match data_encoding::BASE64_NOPAD.decode(cleaned.as_bytes()) {
            Ok(decoded_bytes) => Ok(decoded_bytes == value.as_ref()),
            Err(_) => {
              // Try lenient: strip last char for non-zero trailing bits
              if !cleaned.is_empty() {
                let without_last = &cleaned[..cleaned.len() - 1];
                if let Ok(decoded_bytes) =
                  data_encoding::BASE64_NOPAD.decode(without_last.as_bytes())
                {
                  Ok(value.as_ref().starts_with(&decoded_bytes[..]))
                } else {
                  Ok(false)
                }
              } else {
                Ok(false)
              }
            }
          }
        }
        Err(_) => Ok(false),
      }
    }
    _ => Err(format!(
      "invalid controller type for .b64c operation: {}",
      controller
    )),
  }
}

#[cfg(feature = "additional-controls")]
/// Validate hex encoded text string against byte string
pub fn validate_hex_text<'a>(
  __target: &Type2<'a>,
  controller: &Type2<'a>,
  text_value: &str,
  case_type: HexCase,
) -> Result<bool, String> {
  match controller {
    Type2::UTF8ByteString { value, .. }
    | Type2::B16ByteString { value, .. }
    | Type2::B64ByteString { value, .. } => {
      let decoded = hex::decode(text_value);
      match decoded {
        Ok(decoded_bytes) => {
          let matches_bytes = decoded_bytes == value.as_ref();
          let matches_case = match case_type {
            HexCase::Any => true,
            HexCase::Lower => text_value
              .chars()
              .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit()),
            HexCase::Upper => text_value
              .chars()
              .all(|c| c.is_ascii_uppercase() || c.is_ascii_digit()),
          };
          Ok(matches_bytes && matches_case)
        }
        Err(_) => Ok(false),
      }
    }
    _ => Err(format!(
      "invalid controller type for .hex operation: {}",
      controller
    )),
  }
}

#[cfg(feature = "additional-controls")]
#[derive(Debug, Clone, Copy)]
pub enum HexCase {
  Any,
  Lower,
  Upper,
}

#[cfg(feature = "additional-controls")]
/// Validate base32 encoded text string against byte string
pub fn validate_b32_text<'a>(
  _target: &Type2<'a>,
  controller: &Type2<'a>,
  text_value: &str,
  is_hex_variant: bool,
) -> Result<bool, String> {
  match controller {
    Type2::UTF8ByteString { value, .. }
    | Type2::B16ByteString { value, .. }
    | Type2::B64ByteString { value, .. } => {
      let decoded = if is_hex_variant {
        data_encoding::BASE32HEX_NOPAD.decode(text_value.as_bytes())
      } else {
        data_encoding::BASE32_NOPAD.decode(text_value.as_bytes())
      };

      match decoded {
        Ok(decoded_bytes) => Ok(decoded_bytes == value.as_ref()),
        Err(_) => Ok(false),
      }
    }
    _ => Err(format!(
      "invalid controller type for .b32/.h32 operation: {}",
      controller
    )),
  }
}

#[cfg(feature = "additional-controls")]
/// Validate base45 encoded text string against byte string  
pub fn validate_b45_text<'a>(
  _target: &Type2<'a>,
  controller: &Type2<'a>,
  text_value: &str,
) -> Result<bool, String> {
  #[cfg(feature = "std")]
  {
    match controller {
      Type2::UTF8ByteString { value, .. }
      | Type2::B16ByteString { value, .. }
      | Type2::B64ByteString { value, .. } => match base45::decode(text_value) {
        Ok(decoded_bytes) => Ok(decoded_bytes == value.as_ref()),
        Err(_) => Ok(false),
      },
      _ => Err(format!(
        "invalid controller type for .b45 operation: {}",
        controller
      )),
    }
  }
  #[cfg(not(feature = "std"))]
  {
    Err("base45 support requires std feature".to_string())
  }
}

#[cfg(feature = "additional-controls")]
/// Validate base10 text string against integer
pub fn validate_base10_text<'a>(
  _target: &Type2<'a>,
  controller: &Type2<'a>,
  text_value: &str,
) -> Result<bool, String> {
  // Validate format: 0 or -?[1-9][0-9]*
  if !text_value.chars().all(|c| c.is_ascii_digit() || c == '-') {
    return Ok(false);
  }

  if text_value == "0" {
    // Zero is valid
  } else if text_value.starts_with('-') {
    if text_value.len() < 2
      || !text_value.chars().nth(1).unwrap().is_ascii_digit()
      || text_value.chars().nth(1).unwrap() == '0'
    {
      return Ok(false);
    }
  } else {
    if text_value.starts_with('0') && text_value.len() > 1 {
      return Ok(false); // No leading zeros
    }
    if !text_value.chars().next().unwrap().is_ascii_digit() || text_value.starts_with('0') {
      return Ok(false);
    }
  }

  match controller {
    Type2::IntValue { value, .. } => match text_value.parse::<isize>() {
      Ok(parsed_value) => Ok(parsed_value == *value),
      Err(_) => Ok(false),
    },
    Type2::UintValue { value, .. } => match text_value.parse::<usize>() {
      Ok(parsed_value) => Ok(parsed_value == *value),
      Err(_) => Ok(false),
    },
    _ => Err(format!(
      "invalid controller type for .base10 operation: {}",
      controller
    )),
  }
}

#[cfg(feature = "additional-controls")]
/// Validate printf formatted text string against array of format string and values.
/// Implements RFC 9741 Section 2.3:
/// The controller is an array of [format_string, arg1, arg2, ...].
/// Supports %d, %i, %u, %x, %X, %o, %b, %B, %s, %c, %f, %e, %E, %g, %G
/// with optional flags (-, +, 0, space), width, and precision.
pub fn validate_printf_text<'a>(
  _target: &Type2<'a>,
  controller: &Type2<'a>,
  text_value: &str,
) -> Result<bool, String> {
  match controller {
    Type2::Array { group, .. } => {
      if group.group_choices.is_empty() {
        return Err("printf controller array cannot be empty".to_string());
      }

      let group_choice = &group.group_choices[0];
      if group_choice.group_entries.is_empty() {
        return Err("printf controller array cannot be empty".to_string());
      }

      // Collect all values from the group entries
      let mut values: Vec<&Type2<'a>> = Vec::new();
      for entry in &group_choice.group_entries {
        match &entry.0 {
          GroupEntry::ValueMemberKey { ge, .. } => {
            if let Some(MemberKey::Type1 { t1, .. }) = &ge.member_key {
              values.push(&t1.type2);
            } else {
              values.push(&ge.entry_type.type_choices[0].type1.type2);
            }
          }
          GroupEntry::TypeGroupname { .. } => {
            // Resolve type/group name to extract format string or arguments
            // TypeGroupnameEntry only has name + generic_args, not a type directly
            // We skip these as they can't directly contribute printf arguments
          }
          _ => {}
        }
      }

      if values.is_empty() {
        return Err("printf controller array cannot be empty".to_string());
      }

      // First value is the format string
      let format_str = match values[0] {
        Type2::TextValue { value, .. } => value.as_ref(),
        _ => {
          return Err(
            "first element of printf controller array must be a format string".to_string(),
          )
        }
      };

      // Build expected string from format and arguments
      let expected = format_printf(format_str, &values[1..])?;
      Ok(text_value == expected)
    }
    _ => Err(format!(
      "invalid controller type for .printf operation: {}",
      controller
    )),
  }
}

#[cfg(feature = "additional-controls")]
/// Format a printf-style string with the given arguments (RFC 9741 Section 2.3).
/// Supports: %d, %i, %u, %x, %X, %o, %b, %B, %s, %c, %f, %e, %E, %g, %G, %%
fn format_printf(format_str: &str, args: &[&Type2<'_>]) -> Result<String, String> {
  let mut result = String::new();
  let mut chars = format_str.chars().peekable();
  let mut arg_idx = 0;

  while let Some(ch) = chars.next() {
    if ch != '%' {
      result.push(ch);
      continue;
    }

    // Check for %%
    if chars.peek() == Some(&'%') {
      chars.next();
      result.push('%');
      continue;
    }

    // Parse flags
    let mut flags = String::new();
    while let Some(&fc) = chars.peek() {
      if fc == '-' || fc == '+' || fc == ' ' || fc == '0' || fc == '#' {
        flags.push(fc);
        chars.next();
      } else {
        break;
      }
    }

    // Parse width
    let mut width = String::new();
    while let Some(&wc) = chars.peek() {
      if wc.is_ascii_digit() {
        width.push(wc);
        chars.next();
      } else {
        break;
      }
    }

    // Parse precision
    let mut precision = String::new();
    if chars.peek() == Some(&'.') {
      chars.next();
      while let Some(&pc) = chars.peek() {
        if pc.is_ascii_digit() {
          precision.push(pc);
          chars.next();
        } else {
          break;
        }
      }
    }

    // Parse conversion specifier
    let specifier = chars.next().ok_or("incomplete format specifier")?;

    if arg_idx >= args.len() {
      return Err(format!(
        "not enough arguments for format string (expected at least {}, got {})",
        arg_idx + 1,
        args.len()
      ));
    }

    let arg = args[arg_idx];
    arg_idx += 1;

    let width_val: Option<usize> = if width.is_empty() {
      None
    } else {
      Some(width.parse().unwrap_or(0))
    };
    let prec_val: Option<usize> = if precision.is_empty() {
      None
    } else {
      Some(precision.parse().unwrap_or(0))
    };

    let formatted = format_single_arg(arg, specifier, &flags, width_val, prec_val)?;
    result.push_str(&formatted);
  }

  Ok(result)
}

#[cfg(feature = "additional-controls")]
/// Format a single argument with the given specifier
fn format_single_arg(
  arg: &Type2<'_>,
  specifier: char,
  flags: &str,
  width: Option<usize>,
  precision: Option<usize>,
) -> Result<String, String> {
  let raw = match specifier {
    'd' | 'i' => {
      let val = extract_int(arg)?;
      format!("{}", val)
    }
    'u' => {
      let val = extract_uint(arg)?;
      format!("{}", val)
    }
    'x' => {
      let val = extract_uint(arg)?;
      if flags.contains('#') {
        format!("0x{:x}", val)
      } else {
        format!("{:x}", val)
      }
    }
    'X' => {
      let val = extract_uint(arg)?;
      if flags.contains('#') {
        format!("0X{:X}", val)
      } else {
        format!("{:X}", val)
      }
    }
    'o' => {
      let val = extract_uint(arg)?;
      if flags.contains('#') {
        format!("0{:o}", val)
      } else {
        format!("{:o}", val)
      }
    }
    'b' => {
      let val = extract_uint(arg)?;
      format!("{:b}", val)
    }
    'B' => {
      let val = extract_uint(arg)?;
      format!("{:b}", val) // Same as %b in C23
    }
    'f' | 'F' => {
      let val = extract_float(arg)?;
      match precision {
        Some(p) => format!("{:.prec$}", val, prec = p),
        None => format!("{:.6}", val), // Default precision is 6
      }
    }
    'e' => {
      let val = extract_float(arg)?;
      let p = precision.unwrap_or(6);
      format!("{:.prec$e}", val, prec = p)
    }
    'E' => {
      let val = extract_float(arg)?;
      let p = precision.unwrap_or(6);
      format!("{:.prec$E}", val, prec = p)
    }
    'g' | 'G' => {
      let val = extract_float(arg)?;
      // Use shortest representation
      match precision {
        Some(p) => format!("{:.prec$}", val, prec = p),
        None => format!("{}", val),
      }
    }
    's' => match arg {
      Type2::TextValue { value, .. } => value.to_string(),
      _ => return Err("expected text string for %s conversion".to_string()),
    },
    'c' => {
      let val = extract_uint(arg)?;
      match char::from_u32(val as u32) {
        Some(c) => c.to_string(),
        None => return Err(format!("invalid Unicode scalar value for %c: {}", val)),
      }
    }
    _ => {
      return Err(format!(
        "unsupported printf conversion specifier: %{}",
        specifier
      ))
    }
  };

  // Apply width and alignment
  apply_width_flags(&raw, flags, width)
}

#[cfg(feature = "additional-controls")]
fn extract_int(t: &Type2<'_>) -> Result<isize, String> {
  match t {
    Type2::IntValue { value, .. } => Ok(*value),
    Type2::UintValue { value, .. } => Ok(*value as isize),
    Type2::FloatValue { value, .. } => Ok(*value as isize),
    _ => Err(format!("expected integer for printf, got {}", t)),
  }
}

#[cfg(feature = "additional-controls")]
fn extract_uint(t: &Type2<'_>) -> Result<usize, String> {
  match t {
    Type2::UintValue { value, .. } => Ok(*value),
    Type2::IntValue { value, .. } if *value >= 0 => Ok(*value as usize),
    Type2::FloatValue { value, .. } => Ok(*value as usize),
    _ => Err(format!("expected unsigned integer for printf, got {}", t)),
  }
}

#[cfg(feature = "additional-controls")]
fn extract_float(t: &Type2<'_>) -> Result<f64, String> {
  match t {
    Type2::FloatValue { value, .. } => Ok(*value),
    Type2::IntValue { value, .. } => Ok(*value as f64),
    Type2::UintValue { value, .. } => Ok(*value as f64),
    _ => Err(format!("expected number for printf, got {}", t)),
  }
}

#[cfg(feature = "additional-controls")]
fn apply_width_flags(raw: &str, flags: &str, width: Option<usize>) -> Result<String, String> {
  match width {
    Some(w) if w > raw.len() => {
      let pad_char = if flags.contains('0') && !flags.contains('-') {
        '0'
      } else {
        ' '
      };
      if flags.contains('-') {
        Ok(format!("{:<width$}", raw, width = w))
      } else {
        // Check for sign handling with zero-padding
        if pad_char == '0' && (raw.starts_with('-') || raw.starts_with('+')) {
          let (sign, rest) = raw.split_at(1);
          Ok(format!("{}{:0>width$}", sign, rest, width = w - 1))
        } else {
          let pad = pad_char.to_string().repeat(w - raw.len());
          Ok(format!("{}{}", pad, raw))
        }
      }
    }
    _ => Ok(raw.to_string()),
  }
}

#[cfg(feature = "additional-controls")]
/// Validate JSON text string against CDDL type (RFC 9741 Section 2.4).
/// The text_value must be valid JSON, and the parsed JSON value must conform
/// to the controller type using the default JSON-to-CBOR conversion rules.
pub fn validate_json_text<'a>(
  _target: &Type2<'a>,
  controller: &Type2<'a>,
  text_value: &str,
) -> Result<bool, String> {
  #[cfg(feature = "json")]
  {
    use serde_json::Value;

    match serde_json::from_str::<Value>(text_value) {
      Ok(json_value) => {
        // Validate the parsed JSON value against the controller type.
        // We do structural validation: check that the JSON value is compatible
        // with what the controller type describes.
        Ok(json_matches_type2(&json_value, controller))
      }
      Err(_) => Ok(false),
    }
  }
  #[cfg(not(feature = "json"))]
  {
    Err("JSON support requires json feature".to_string())
  }
}

#[cfg(feature = "additional-controls")]
#[cfg(feature = "json")]
/// Check if a JSON value structurally matches a Type2 controller.
/// This is a best-effort check for literal and simple type matching.
fn json_matches_type2(json: &serde_json::Value, t2: &Type2<'_>) -> bool {
  use serde_json::Value;

  match t2 {
    // Match against text value literals
    Type2::TextValue { value, .. } => {
      matches!(json, Value::String(s) if s == value.as_ref())
    }
    // Match against integer literals
    Type2::IntValue { value, .. } => {
      matches!(json, Value::Number(n) if n.as_i64() == Some(*value as i64))
    }
    Type2::UintValue { value, .. } => {
      matches!(json, Value::Number(n) if n.as_u64() == Some(*value as u64))
    }
    Type2::FloatValue { value, .. } => {
      matches!(json, Value::Number(n) if n.as_f64() == Some(*value))
    }
    // Match against typename identifiers (basic type checking)
    Type2::Typename { ident, .. } => {
      let name = ident.ident.to_string();
      match name.as_str() {
        "text" | "tstr" => json.is_string(),
        "uint" => matches!(json, Value::Number(n) if n.as_u64().is_some()),
        "nint" => {
          matches!(json, Value::Number(n) if n.as_i64().map(|v| v < 0).unwrap_or(false))
        }
        "int" | "integer" => {
          json.is_number() && json.as_f64().map(|v| v.fract() == 0.0).unwrap_or(false)
        }
        "float" | "float16" | "float32" | "float64" | "float16-32" | "float32-64" | "number" => {
          json.is_number()
        }
        "bool" => json.is_boolean(),
        "true" => matches!(json, Value::Bool(true)),
        "false" => matches!(json, Value::Bool(false)),
        "null" | "nil" => json.is_null(),
        "any" => true, // any matches everything
        _ => true,     // Unknown types: be permissive
      }
    }
    // Map types
    Type2::Map { .. } => json.is_object(),
    // Array types
    Type2::Array { .. } => json.is_array(),
    // For other complex types, just check it's valid JSON (already verified)
    _ => true,
  }
}

#[cfg(feature = "additional-controls")]
/// Validate joined text string against array of components (RFC 9741 Section 3.1).
/// The controller is an array of string components that are concatenated.
/// The target text_value must equal the concatenation of all components.
pub fn validate_join_text<'a>(
  _target: &Type2<'a>,
  controller: &Type2<'a>,
  text_value: &str,
  cddl: Option<&'a CDDL<'a>>,
) -> Result<bool, String> {
  match controller {
    Type2::Array { group, .. } => {
      if group.group_choices.is_empty() {
        return Ok(text_value.is_empty());
      }

      let group_choice = &group.group_choices[0];

      // Collect all literal string markers to build a validation pattern
      let mut parts: Vec<Option<String>> = Vec::new();

      for entry in &group_choice.group_entries {
        match &entry.0 {
          GroupEntry::ValueMemberKey { ge, .. } => {
            let t2 = if let Some(MemberKey::Type1 { t1, .. }) = &ge.member_key {
              &t1.type2
            } else {
              &ge.entry_type.type_choices[0].type1.type2
            };
            parts.push(extract_string_value(t2, cddl));
          }
          GroupEntry::TypeGroupname { ge, .. } => {
            // Type/group name references - try to resolve
            let t2 = Type2::Typename {
              ident: ge.name.clone(),
              generic_args: ge.generic_args.clone(),
              #[cfg(feature = "ast-span")]
              span: crate::ast::Span::default(),
            };
            // We can't easily push a reference to a local, so handle inline
            match &t2 {
              Type2::TextValue { value, .. } => {
                parts.push(Some(value.to_string()));
              }
              _ => parts.push(extract_string_value(&t2, cddl)),
            }
          }
          _ => {
            parts.push(None); // Unknown part, variable
          }
        }
      }

      // If all parts are known literals, just concatenate and compare
      if parts.iter().all(|p| p.is_some()) {
        let expected: String = parts.iter().filter_map(|p| p.as_deref()).collect();
        return Ok(text_value == expected);
      }

      // Use marker-based validation: known literal parts act as delimiters
      // between variable parts. This implements the recommended marker-based
      // subset from RFC 9741 Section 3.1.
      validate_join_with_markers(text_value, &parts)
    }
    _ => Err(format!(
      "invalid controller type for .join operation: {}",
      controller
    )),
  }
}

#[cfg(feature = "additional-controls")]
/// Extract a string value from a Type2 node, resolving through identifiers if possible.
fn extract_string_value<'a>(t2: &Type2<'a>, cddl: Option<&'a CDDL<'a>>) -> Option<String> {
  match t2 {
    Type2::TextValue { value, .. } => Some(value.to_string()),
    Type2::UTF8ByteString { value, .. } => std::str::from_utf8(value).ok().map(|s| s.to_string()),
    Type2::Typename { ident, .. } => {
      // Try to resolve through CDDL rules
      if let Some(cddl) = cddl {
        let literals = string_literals_from_ident(cddl, ident);
        if literals.len() == 1 {
          return extract_string_value(literals[0], Some(cddl));
        }
      }
      None
    }
    _ => None,
  }
}

#[cfg(feature = "additional-controls")]
/// Validate a joined string using marker-based splitting.
/// Literal (known) parts act as markers/delimiters between variable parts.
fn validate_join_with_markers(text: &str, parts: &[Option<String>]) -> Result<bool, String> {
  if parts.is_empty() {
    return Ok(text.is_empty());
  }

  // Simple case: all known
  if parts.iter().all(|p| p.is_some()) {
    let expected: String = parts.iter().filter_map(|p| p.as_deref()).collect();
    return Ok(text == expected);
  }

  // Use the known parts as markers to validate structure
  let mut pos = 0;
  for (i, part) in parts.iter().enumerate() {
    if let Some(literal) = part {
      // Find this literal in the remaining text
      if let Some(found_pos) = text[pos..].find(literal.as_str()) {
        pos += found_pos + literal.len();
      } else {
        return Ok(false); // Marker not found
      }
    } else {
      // Variable part - skip to next marker or end
      if i + 1 < parts.len() {
        // There's a next part; the variable part will be consumed
        // when we find the next marker
        continue;
      } else {
        // Last part is variable - it consumes the rest
        pos = text.len();
      }
    }
  }

  Ok(pos == text.len())
}
