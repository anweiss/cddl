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
      let decoded = if is_sloppy {
        data_encoding::BASE64URL_NOPAD.decode(text_value.as_bytes())
      } else {
        data_encoding::BASE64URL_NOPAD.decode(text_value.as_bytes())
      };

      match decoded {
        Ok(decoded_bytes) => Ok(decoded_bytes == value.as_ref()),
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
      let decoded = if is_sloppy {
        data_encoding::BASE64.decode(text_value.as_bytes())
      } else {
        data_encoding::BASE64.decode(text_value.as_bytes())
      };

      match decoded {
        Ok(decoded_bytes) => Ok(decoded_bytes == value.as_ref()),
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
    if !text_value.chars().next().unwrap().is_ascii_digit()
      || text_value.chars().next().unwrap() == '0'
    {
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
/// Validate printf formatted text string against array of format string and values
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

      // First entry should be the format string
      let format_entry = &group_choice.group_entries[0];
      if let GroupEntry::ValueMemberKey { ge, .. } = &format_entry.0 {
        if let Some(member_key) = &ge.member_key {
          if let MemberKey::Type1 { t1, .. } = member_key {
            if let Type2::TextValue {
              value: format_str, ..
            } = &t1.type2
            {
              // For now, do a basic validation - in a full implementation, we'd need to
              // parse the printf format string and validate against the provided arguments
              // This is a simplified check
              Ok(text_value.contains(&format_str.as_ref().replace("%", "")))
            } else {
              Err("first element of printf controller array must be a format string".to_string())
            }
          } else {
            Err("first element of printf controller array must be a format string".to_string())
          }
        } else {
          Err("first element of printf controller array must be a format string".to_string())
        }
      } else {
        Err("first element of printf controller array must be a format string".to_string())
      }
    }
    _ => Err(format!(
      "invalid controller type for .printf operation: {}",
      controller
    )),
  }
}

#[cfg(feature = "additional-controls")]
/// Validate JSON text string against CDDL type
pub fn validate_json_text<'a>(
  _target: &Type2<'a>,
  _controller: &Type2<'a>,
  text_value: &str,
) -> Result<bool, String> {
  #[cfg(feature = "json")]
  {
    use serde_json::Value;

    match serde_json::from_str::<Value>(text_value) {
      Ok(_json_value) => {
        // For now, return true if it's valid JSON
        // In a full implementation, we'd validate the JSON against the controller type
        Ok(true)
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
/// Validate joined text string against array of components
pub fn validate_join_text<'a>(
  _target: &Type2<'a>,
  controller: &Type2<'a>,
  text_value: &str,
) -> Result<bool, String> {
  match controller {
    Type2::Array { group, .. } => {
      if group.group_choices.is_empty() {
        return Ok(text_value.is_empty());
      }

      let group_choice = &group.group_choices[0];
      let mut expected_string = String::new();

      for entry in &group_choice.group_entries {
        if let GroupEntry::ValueMemberKey { ge, .. } = &entry.0 {
          if let Some(member_key) = &ge.member_key {
            if let MemberKey::Type1 { t1, .. } = member_key {
              match &t1.type2 {
                Type2::TextValue { value, .. } => {
                  expected_string.push_str(&value);
                }
                Type2::UTF8ByteString { value, .. } => match std::str::from_utf8(&value) {
                  Ok(s) => expected_string.push_str(s),
                  Err(_) => return Ok(false),
                },
                _ => {
                  // For other types, we'd need to resolve them
                  // This is a simplified implementation
                  continue;
                }
              }
            }
          }
        }
      }

      Ok(text_value == expected_string)
    }
    _ => Err(format!(
      "invalid controller type for .join operation: {}",
      controller
    )),
  }
}
