use crate::{
  ast::*,
  token::{lookup_ident, Token},
  util::*,
  visitor::{self, *},
};

use std::collections::HashMap;

use displaydoc::Display;
use fake::{faker::name::raw::*, locales::EN, Fake, Faker as FFaker};
use rand::{self, seq::SliceRandom};
use serde_json::{self, Map, Value};

#[cfg(not(target_arch = "wasm32"))]
use crate::{cddl_from_str, lexer_from_str};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

/// Result type from fake data generation
pub type Result<T> = std::result::Result<T, Error>;

/// Fake data generation error type
#[derive(Debug, Display)]
pub enum Error {
  /// Data cannot be generated from a CDDL document that has no type rules
  /// defined
  #[displaydoc("no type rules found in the cddl document")]
  MissingTypeRules,
  /// UTF8 parsing error
  #[displaydoc("{0}")]
  Utf8Error(std::str::Utf8Error),
  /// CDDL parsing error
  #[displaydoc("error parsing cddl: {0}")]
  CDDLParsing(String),
}

#[cfg(feature = "std")]
impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

/// JSON data faker for a given CDDL document
pub struct Faker<'a> {
  cddl: &'a CDDL<'a>,
  faked_json: Option<serde_json::Value>,
  entries: Entries<'a>,
  ctrl: Option<(Identifier<'a>, Token<'a>)>,
  generic_rules: Vec<GenericRule<'a>>,
  eval_generic_rule: Option<&'a str>,
  is_group_to_choice_enum: bool,
}

impl<'a> Faker<'a> {
  /// Construct a new faker from a given CDDL document
  pub fn new(cddl: &'a CDDL) -> Self {
    Faker {
      cddl,
      faked_json: None,
      entries: Entries::None,
      ctrl: None,
      generic_rules: Vec::new(),
      eval_generic_rule: None,
      is_group_to_choice_enum: false,
    }
  }

  fn process_generic_args(
    &mut self,
    ident: &Identifier<'a>,
    generic_args: &GenericArgs<'a>,
  ) -> visitor::Result<Error> {
    if let Some(rule) = rule_from_ident(self.cddl, ident) {
      if let Some(gr) = self
        .generic_rules
        .iter_mut()
        .find(|gr| gr.name == ident.ident)
      {
        for arg in generic_args.args.iter() {
          gr.args.push((*arg.arg).clone());
        }
      } else if let Some(params) = generic_params_from_rule(rule) {
        self.generic_rules.push(GenericRule {
          name: ident.ident,
          params,
          args: generic_args
            .args
            .iter()
            .cloned()
            .map(|arg| *arg.arg)
            .collect(),
        });
      }

      self.eval_generic_rule = Some(ident.ident);

      return self.visit_rule(rule);
    }

    Ok(())
  }
}

enum Entries<'a> {
  None,
  Map(HashMap<String, (Option<Occur>, Type<'a>)>),
  Array(Vec<(Option<Occur>, Type<'a>)>),
}

impl<'a> Visitor<'a, Error> for Faker<'a> {
  fn visit_type_rule(&mut self, tr: &TypeRule<'a>) -> visitor::Result<Error> {
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

    walk_type_rule(self, tr)
  }

  fn visit_group_rule(&mut self, gr: &GroupRule<'a>) -> visitor::Result<Error> {
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

    walk_group_rule(self, gr)
  }

  fn visit_group(&mut self, g: &Group<'a>) -> visitor::Result<Error> {
    if let Some(gc) = g.group_choices.choose(&mut rand::thread_rng()) {
      return self.visit_group_choice(gc);
    }

    walk_group(self, g)
  }

  fn visit_group_choice(&mut self, gc: &GroupChoice<'a>) -> visitor::Result<Error> {
    if self.is_group_to_choice_enum {
      if let Some(tc) =
        type_choices_from_group_choice(self.cddl, gc).choose(&mut rand::thread_rng())
      {
        return self.visit_type_choice(tc);
      }
    }

    walk_group_choice(self, gc)
  }

  fn visit_type(&mut self, t: &Type<'a>) -> visitor::Result<Error> {
    if let Some(tc) = t.type_choices.choose(&mut rand::thread_rng()) {
      return self.visit_type_choice(tc);
    }

    walk_type(self, t)
  }

  fn visit_type_groupname_entry(
    &mut self,
    entry: &TypeGroupnameEntry<'a>,
  ) -> visitor::Result<Error> {
    if let Entries::Array(entries) = &mut self.entries {
      entries.push((
        entry.occur.clone().map(|o| o.occur),
        Type::from(&entry.name),
      ));
    } else if let Entries::Map(_) = self.entries {
      if let Some(rule) = group_rule_from_ident(self.cddl, &entry.name) {
        return self.visit_group_rule(rule);
      }
    }

    Ok(())
  }

  fn visit_control_operator(
    &mut self,
    target: &Type2<'a>,
    ctrl: &Token<'a>,
    controller: &Type2<'a>,
  ) -> visitor::Result<Error> {
    if let Type2::Typename {
      ident: target_ident,
      ..
    } = target
    {
      if let Type2::Typename {
        ident: controller_ident,
        ..
      } = controller
      {
        if let Some(name) = self.eval_generic_rule {
          if let Some(gr) = self
            .generic_rules
            .iter()
            .cloned()
            .find(|gr| gr.name == name)
          {
            for (idx, gp) in gr.params.iter().enumerate() {
              if let Some(arg) = gr.args.get(idx) {
                if *gp == target_ident.ident {
                  let t2 = Type2::from((*arg).clone());

                  if *gp == controller_ident.ident {
                    return self.visit_control_operator(&t2, ctrl, &t2);
                  }

                  return self.visit_control_operator(&arg.type2, ctrl, controller);
                }
              }
            }
          }
        }
      }

      if let Some(name) = self.eval_generic_rule {
        if let Some(gr) = self
          .generic_rules
          .iter()
          .cloned()
          .find(|gr| gr.name == name)
        {
          for (idx, gp) in gr.params.iter().enumerate() {
            if let Some(arg) = gr.args.get(idx) {
              if *gp == target_ident.ident {
                let t2 = Type2::from((*arg).clone());
                return self.visit_control_operator(&t2, ctrl, controller);
              }
            }
          }
        }
      }
    }

    match ctrl {
      Token::SIZE => match target {
        Type2::Typename { ident, .. }
          if is_ident_string_data_type(self.cddl, ident)
            || is_ident_uint_data_type(self.cddl, ident) =>
        {
          self.ctrl = Some((ident.clone(), ctrl.clone()));
          self.visit_type2(controller)?;
          self.ctrl = None;
        }
        _ => return Ok(()),
      },
      _ => return Ok(()),
    }

    Ok(())
  }

  fn visit_range(
    &mut self,
    lower: &Type2<'a>,
    upper: &Type2<'a>,
    is_inclusive: bool,
  ) -> visitor::Result<Error> {
    match lower {
      Type2::IntValue { value: lower_v, .. } => match upper {
        Type2::IntValue { value: upper, .. } => {
          if is_inclusive {
            self.faked_json = Some((*lower_v..=*upper).fake::<isize>().into());
          } else {
            self.faked_json = Some((*lower_v..*upper).fake::<isize>().into());
          }
        }
        Type2::UintValue { value: upper, .. } => {
          if is_inclusive {
            self.faked_json = Some((*lower_v..=*upper as isize).fake::<isize>().into());
          } else {
            self.faked_json = Some((*lower_v..*upper as isize).fake::<isize>().into());
          }
        }
        Type2::Typename {
          ident,
          generic_args,
          ..
        } => {
          if let Some(ga) = generic_args {
            return self.process_generic_args(ident, ga);
          }

          if let Some(t2) = numeric_range_bound_from_ident(self.cddl, ident) {
            return self.visit_range(lower, t2, is_inclusive);
          }
        }
        _ => return Ok(()),
      },
      Type2::UintValue { value: lower_v, .. } => match upper {
        Type2::IntValue { value: upper, .. } => {
          if is_inclusive {
            self.faked_json = Some((*lower_v..=*upper as usize).fake::<usize>().into());
          } else {
            self.faked_json = Some((*lower_v..*upper as usize).fake::<usize>().into());
          }
        }
        Type2::UintValue { value: upper, .. } => {
          if is_inclusive {
            self.faked_json = Some((*lower_v..=*upper).fake::<usize>().into());
          } else {
            self.faked_json = Some((*lower_v..*upper).fake::<usize>().into());
          }
        }
        Type2::Typename {
          ident,
          generic_args,
          ..
        } => {
          if let Some(ga) = generic_args {
            return self.process_generic_args(ident, ga);
          }

          if let Some(t2) = numeric_range_bound_from_ident(self.cddl, ident) {
            return self.visit_range(lower, t2, is_inclusive);
          }
        }
        _ => return Ok(()),
      },
      Type2::FloatValue { value: lower_v, .. } => match upper {
        Type2::FloatValue { value: upper, .. } => {
          if is_inclusive {
            self.faked_json = Some((*lower_v..=*upper).fake::<f64>().into());
          } else {
            self.faked_json = Some((*lower_v..*upper).fake::<f64>().into());
          }
        }
        Type2::Typename {
          ident,
          generic_args,
          ..
        } => {
          if let Some(ga) = generic_args {
            return self.process_generic_args(ident, ga);
          }

          if let Some(t2) = numeric_range_bound_from_ident(self.cddl, ident) {
            return self.visit_range(lower, t2, is_inclusive);
          }
        }
        _ => return Ok(()),
      },
      Type2::Typename {
        ident,
        generic_args,
        ..
      } => {
        if let Some(ga) = generic_args {
          return self.process_generic_args(ident, ga);
        }

        if let Some(t2) = numeric_range_bound_from_ident(self.cddl, ident) {
          return self.visit_range(t2, upper, is_inclusive);
        }
      }
      _ => return Ok(()),
    }

    Ok(())
  }

  fn visit_value_member_key_entry(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
  ) -> visitor::Result<Error> {
    if let Entries::Map(entries) = &mut self.entries {
      let occur = entry.occur.clone().map(|o| o.occur);

      if let Some(mk) = &entry.member_key {
        match mk {
          MemberKey::Bareword { ident, .. } => {
            entries.insert(ident.ident.to_string(), (occur, entry.entry_type.clone()));
          }
          MemberKey::Value { value, .. } => {
            entries.insert(value.to_string(), (occur, entry.entry_type.clone()));
          }
          _ => return Ok(()),
        }
      }
    } else if let Entries::Array(entries) = &mut self.entries {
      entries.push((
        entry.occur.clone().map(|o| o.occur),
        entry.entry_type.clone(),
      ));
    }

    Ok(())
  }

  fn visit_type2(&mut self, t2: &Type2<'a>) -> visitor::Result<Error> {
    match t2 {
      Type2::TextValue { value, .. } => {
        if let Some(Value::Array(array)) = self.faked_json.as_mut() {
          array.push(value.as_ref().into());
        } else {
          self.faked_json = Some(value.as_ref().into());
        }
      }
      Type2::UTF8ByteString { value, .. } => {
        let value = std::str::from_utf8(value.as_ref()).map_err(Error::Utf8Error)?;

        if let Some(Value::Array(array)) = self.faked_json.as_mut() {
          array.push(value.into());
        } else {
          self.faked_json = Some(value.into());
        }
      }
      Type2::UintValue { value, .. } => match &self.ctrl {
        Some((target, Token::SIZE)) => {
          if is_ident_string_data_type(self.cddl, target) {
            self.faked_json = Some(value.fake::<String>().into())
          } else if is_ident_uint_data_type(self.cddl, target) {
            self.faked_json = Some((0..256u64.pow(*value as u32)).fake::<u64>().into());
          }
        }
        _ => self.faked_json = Some((*value).into()),
      },
      Type2::IntValue { value, .. } => self.faked_json = Some((*value).into()),
      Type2::FloatValue { value, .. } => self.faked_json = Some((*value).into()),
      Type2::Array { group, .. } => {
        self.faked_json = Some(Value::Array(Vec::new()));
        self.entries = Entries::Array(Vec::new());
        self.visit_group(group)?;

        if let Entries::Array(array_entries) = &mut self.entries {
          if let Some(Value::Array(array)) = self.faked_json.as_mut() {
            for (occur, entry) in array_entries.iter() {
              if let Some(occur) = occur {
                match occur {
                  #[cfg(feature = "ast-span")]
                  Occur::Optional(_) => {
                    if FFaker.fake::<bool>() {
                      let mut entry_f = Faker::new(self.cddl);
                      entry_f.eval_generic_rule = self.eval_generic_rule;
                      entry_f.generic_rules = self.generic_rules.clone();
                      entry_f.ctrl = self.ctrl.clone();
                      entry_f.visit_type(entry)?;

                      if let Some(value) = entry_f.faked_json {
                        array.push(value);
                      }

                      continue;
                    }
                  }
                  #[cfg(not(feature = "ast-span"))]
                  Occur::Optional => {
                    if FFaker.fake::<bool>() {
                      let mut entry_f = Faker::new(self.cddl);
                      entry_f.eval_generic_rule = self.eval_generic_rule;
                      entry_f.generic_rules = self.generic_rules.clone();
                      entry_f.ctrl = self.ctrl.clone();
                      entry_f.visit_type(entry)?;

                      if let Some(value) = entry_f.faked_json {
                        array.push(value);
                      }

                      continue;
                    }
                  }
                  #[cfg(feature = "ast-span")]
                  Occur::ZeroOrMore(_) => {
                    let lower = (0..2).fake::<usize>();
                    let upper = (0..5).fake::<usize>();

                    // If the random lower >= random upper, the random array
                    // will be empty.
                    for _ in lower..upper {
                      let mut entry_f = Faker::new(self.cddl);
                      entry_f.eval_generic_rule = self.eval_generic_rule;
                      entry_f.generic_rules = self.generic_rules.clone();
                      entry_f.ctrl = self.ctrl.clone();
                      entry_f.visit_type(entry)?;

                      if let Some(value) = entry_f.faked_json {
                        array.push(value);
                      }
                    }

                    // Break due to ambiguity
                    break;
                  }
                  #[cfg(not(feature = "ast-span"))]
                  Occur::ZeroOrMore => {
                    let lower = (0..2).fake::<usize>();
                    let upper = (0..5).fake::<usize>();

                    // If the random lower >= random upper, the random array
                    // will be empty.
                    for _ in lower..upper {
                      let mut entry_f = Faker::new(self.cddl);
                      entry_f.eval_generic_rule = self.eval_generic_rule;
                      entry_f.generic_rules = self.generic_rules.clone();
                      entry_f.ctrl = self.ctrl.clone();
                      entry_f.visit_type(entry)?;

                      if let Some(value) = entry_f.faked_json {
                        array.push(value);
                      }
                    }

                    // Break due to ambiguity
                    break;
                  }
                  #[cfg(feature = "ast-span")]
                  Occur::OneOrMore(_) => {
                    for _ in 0..(1..5).fake::<usize>() {
                      let mut entry_f = Faker::new(self.cddl);
                      entry_f.eval_generic_rule = self.eval_generic_rule;
                      entry_f.generic_rules = self.generic_rules.clone();
                      entry_f.ctrl = self.ctrl.clone();
                      entry_f.visit_type(entry)?;

                      if let Some(value) = entry_f.faked_json {
                        array.push(value);
                      }
                    }

                    // Break due to ambiguity
                    break;
                  }
                  #[cfg(not(feature = "ast-span"))]
                  Occur::OneOrMore => {
                    for _ in 0..(1..5).fake::<usize>() {
                      let mut entry_f = Faker::new(self.cddl);
                      entry_f.eval_generic_rule = self.eval_generic_rule;
                      entry_f.generic_rules = self.generic_rules.clone();
                      entry_f.ctrl = self.ctrl.clone();
                      entry_f.visit_type(entry)?;

                      if let Some(value) = entry_f.faked_json {
                        array.push(value);
                      }
                    }

                    // Break due to ambiguity
                    break;
                  }
                  Occur::Exact { lower, upper, .. } => {
                    if let Some(lower) = lower {
                      if let Some(upper) = upper {
                        for _ in *lower..*upper {
                          let mut entry_f = Faker::new(self.cddl);
                          entry_f.eval_generic_rule = self.eval_generic_rule;
                          entry_f.generic_rules = self.generic_rules.clone();
                          entry_f.ctrl = self.ctrl.clone();
                          entry_f.visit_type(entry)?;

                          if let Some(value) = entry_f.faked_json {
                            array.push(value);
                          }
                        }
                      } else {
                        for _ in *lower..(lower + (0..5).fake::<usize>()) {
                          let mut entry_f = Faker::new(self.cddl);
                          entry_f.eval_generic_rule = self.eval_generic_rule;
                          entry_f.generic_rules = self.generic_rules.clone();
                          entry_f.ctrl = self.ctrl.clone();
                          entry_f.visit_type(entry)?;

                          if let Some(value) = entry_f.faked_json {
                            array.push(value);
                          }
                        }
                      }
                    } else if let Some(upper) = upper {
                      for _ in 0..(upper - (0..=*upper).fake::<usize>()) {
                        let mut entry_f = Faker::new(self.cddl);
                        entry_f.eval_generic_rule = self.eval_generic_rule;
                        entry_f.generic_rules = self.generic_rules.clone();
                        entry_f.ctrl = self.ctrl.clone();
                        entry_f.visit_type(entry)?;

                        if let Some(value) = entry_f.faked_json {
                          array.push(value);
                        }
                      }
                    }
                  }
                }
              } else {
                let mut entry_f = Faker::new(self.cddl);
                entry_f.eval_generic_rule = self.eval_generic_rule;
                entry_f.generic_rules = self.generic_rules.clone();
                entry_f.ctrl = self.ctrl.clone();
                entry_f.visit_type(entry)?;

                if let Some(value) = entry_f.faked_json {
                  array.push(value);
                }
              }
            }
          }
        }

        self.entries = Entries::None;
      }
      Type2::Map { group, .. } => {
        self.faked_json = Some(Value::Object(Map::default()));
        self.entries = Entries::Map(HashMap::new());
        self.visit_group(group)?;

        if let Entries::Map(map_entries) = &mut self.entries {
          if let Some(Value::Object(map)) = self.faked_json.as_mut() {
            for (k, (occur, v)) in map_entries.iter() {
              #[cfg(feature = "ast-span")]
              let generate = if let Some(Occur::Optional(_)) = occur {
                FFaker.fake::<bool>()
              } else {
                true
              };
              #[cfg(not(feature = "ast-span"))]
              let generate = if let Some(Occur::Optional) = occur {
                FFaker.fake::<bool>()
              } else {
                true
              };

              if generate {
                let mut entry_f = Faker::new(self.cddl);
                entry_f.eval_generic_rule = self.eval_generic_rule;
                entry_f.generic_rules = self.generic_rules.clone();
                entry_f.ctrl = self.ctrl.clone();
                entry_f.visit_type(v)?;

                if let Some(value) = entry_f.faked_json {
                  map.insert(k.to_string(), value);
                }
              }
            }
          }
        }

        self.entries = Entries::None;
      }
      Type2::Typename {
        ident,
        generic_args,
        ..
      } => {
        if let Some(ga) = generic_args {
          return self.process_generic_args(ident, ga);
        }

        return self.visit_identifier(ident);
      }
      Type2::ChoiceFromGroup {
        ident,
        generic_args,
        ..
      } => {
        if let Some(ga) = generic_args {
          self.is_group_to_choice_enum = true;

          return self.process_generic_args(ident, ga);
        }

        self.is_group_to_choice_enum = true;
        self.visit_identifier(ident)?;
        self.is_group_to_choice_enum = false;
      }
      _ => walk_type2(self, t2)?,
    }

    Ok(())
  }

  fn visit_identifier(&mut self, ident: &Identifier<'a>) -> visitor::Result<Error> {
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

    if is_ident_string_data_type(self.cddl, ident) {
      self.faked_json = Some(Name(EN).fake::<String>().into());
    } else if let Token::NUMBER = lookup_ident(ident.ident) {
      if FFaker.fake::<bool>() {
        self.faked_json = Some(FFaker.fake::<i16>().into())
      } else {
        self.faked_json = Some(FFaker.fake::<f32>().into())
      }
    } else if is_ident_float_data_type(self.cddl, ident) {
      self.faked_json = Some(FFaker.fake::<f32>().into())
    } else if is_ident_uint_data_type(self.cddl, ident) {
      self.faked_json = Some(FFaker.fake::<u16>().into())
    } else if is_ident_nint_data_type(self.cddl, ident) {
      self.faked_json = Some((..0).fake::<i16>().into())
    } else if is_ident_integer_data_type(self.cddl, ident) {
      self.faked_json = Some(FFaker.fake::<i16>().into())
    } else if is_ident_bool_data_type(self.cddl, ident) {
      self.faked_json = Some(FFaker.fake::<bool>().into())
    } else if let Some(rule) = rule_from_ident(self.cddl, ident) {
      self.visit_rule(rule)?;
    } else {
      self.faked_json = Some(ident.ident.into());
    }

    Ok(())
  }
}

#[cfg(not(target_arch = "wasm32"))]
/// Generate fake JSON from a given CDDL document string
pub fn fake_json_from_cddl_str(cddl_str: &str) -> Result<String> {
  let mut lexer = lexer_from_str(cddl_str);
  let cddl = cddl_from_str(&mut lexer, cddl_str, true).map_err(Error::CDDLParsing)?;
  let mut faker = Faker::new(&cddl);

  for rule in faker.cddl.rules.iter() {
    if let Rule::Type { rule, .. } = rule {
      faker.visit_type_rule(rule)?;
      break;
    }
  }

  if let Some(faked_json) = faker.faked_json {
    return Ok(faked_json.to_string());
  }

  Err(Error::MissingTypeRules)
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
/// Generate fake JSON from a given CDDL document string
pub fn fake_json_from_cddl_str(cddl_str: &str) -> std::result::Result<JsValue, JsValue> {
  use crate::{
    error::ParserError,
    lexer::Lexer,
    parser::{self, Parser},
  };

  let mut l = Lexer::new(cddl_str);
  let mut p = Parser::new((&mut l).iter(), cddl_str).map_err(|e| JsValue::from(e.to_string()))?;
  let c = p.parse_cddl().map_err(|e| JsValue::from(e.to_string()))?;
  if !p.errors.is_empty() {
    return Err(
      JsValue::from_serde(
        &p.errors
          .iter()
          .filter_map(|e| {
            if let parser::Error::PARSER { position, msg } = e {
              Some(ParserError {
                position: *position,
                msg: msg.clone(),
              })
            } else {
              None
            }
          })
          .collect::<Vec<ParserError>>(),
      )
      .map_err(|e| JsValue::from(e.to_string()))?,
    );
  }
  let mut faker = Faker::new(&c);

  for rule in faker.cddl.rules.iter() {
    if let Rule::Type { rule, .. } = rule {
      faker
        .visit_type_rule(rule)
        .map_err(|e| JsValue::from(e.to_string()))?;
      break;
    }
  }

  if let Some(faked_json) = faker.faked_json {
    return Ok(faked_json.to_string().into());
  }

  Err(Error::MissingTypeRules.to_string().into())
}

#[cfg(test)]
#[cfg(not(target_arch = "wasm32"))]
mod tests {
  use super::*;

  use indoc::indoc;

  #[test]
  fn test_faker() -> Result<()> {
    let cddl = indoc!(
      r#"
        messages = message<"reboot", "now"> / message<"sleep", a .. 5>
        message<t, v> = {type: t, value: v}
        a = 1 / 2 / 3
      "#
    );

    println!("{}", fake_json_from_cddl_str(&cddl)?);

    Ok(())
  }
}
