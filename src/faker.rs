use crate::{
  ast::*,
  cddl_from_str, lexer_from_str,
  token::{lookup_ident, Token},
  validator::{group_rule_from_ident, rule_from_ident},
  visitor::{self, walk_group, walk_memberkey, walk_type2, walk_value_member_key_entry, Visitor},
};

use std::{collections::HashMap, fmt};

use displaydoc::Display;
use fake::{Dummy, Fake, Faker as FFaker};
use rand::{rngs::StdRng, SeedableRng};
use serde::Deserialize;
use serde_json::{self, Map, Value};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Display)]
pub enum Error {
  #[displaydoc("no type rules found in the cddl document")]
  MissingTypeRules,
  #[displaydoc("{0}")]
  Utf8Error(std::str::Utf8Error),
  #[displaydoc("error parsing cddl: {0}")]
  CDDLParsing(String),
}

#[cfg(feature = "std")]
impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

pub struct Faker<'a> {
  pub cddl: &'a CDDL<'a>,
  pub faked_json: Option<serde_json::Value>,
  pub map_entries: Option<HashMap<String, (Option<Occur>, Type<'a>)>>,
}

impl<'a> Faker<'a> {
  pub fn new(cddl: &'a CDDL) -> Self {
    Faker {
      cddl,
      faked_json: None,
      map_entries: None,
    }
  }
}

impl<'a> Visitor<'a, Error> for Faker<'a> {
  fn visit_value_member_key_entry(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
  ) -> visitor::Result<Error> {
    let occur = entry.occur.clone().map(|o| o.occur);

    if let Some(mk) = &entry.member_key {
      match mk {
        MemberKey::Bareword { ident, .. } => {
          let mut map_entries = HashMap::new();
          map_entries.insert(
            ident.ident.to_string(),
            (occur.clone(), entry.entry_type.clone()),
          );

          self
            .map_entries
            .get_or_insert(map_entries)
            .insert(ident.ident.to_string(), (occur, entry.entry_type.clone()));
        }
        MemberKey::Value { value, .. } => {
          let mut map_entries = HashMap::new();
          map_entries.insert(value.to_string(), (occur.clone(), entry.entry_type.clone()));

          self
            .map_entries
            .get_or_insert(map_entries)
            .insert(value.to_string(), (occur, entry.entry_type.clone()));
        }
        _ => return Ok(()),
      }
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
      Type2::Array { group, .. } => {
        let mut f = Faker::new(self.cddl);
        f.faked_json = Some(Value::Array(Vec::new()));
        f.visit_group(group)?;

        self.faked_json = f.faked_json;
      }
      Type2::Map { group, .. } => {
        let mut f = Faker::new(self.cddl);
        f.faked_json = Some(Value::Object(Map::new()));
        f.visit_group(group)?;

        if let Some(map_entries) = f.map_entries {
          let mut entries = Map::new();
          for (k, (occur, v)) in map_entries.iter() {
            let generate = if let Some(Occur::Optional(_)) = occur {
              FFaker.fake::<bool>()
            } else {
              true
            };

            if generate {
              let mut entry_f = Faker::new(self.cddl);
              entry_f.visit_type(v)?;

              if let Some(value) = entry_f.faked_json {
                entries.insert(k.to_string(), value);
              }
            }
          }
          self.faked_json = Some(Value::Object(entries));
        }

        self.map_entries = None;
      }
      _ => walk_type2(self, t2)?,
    }

    Ok(())
  }

  fn visit_identifier(&mut self, ident: &Identifier<'a>) -> visitor::Result<Error> {
    match lookup_ident(ident.ident) {
      Token::TSTR => {
        let value = FFaker.fake::<String>();

        if let Some(Value::Array(array)) = self.faked_json.as_mut() {
          array.push(value.into());
        } else {
          self.faked_json = Some(value.into());
        }
      }
      _ => {
        self.faked_json = Some(ident.ident.into());
      }
    }

    Ok(())
  }
}

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

#[cfg(test)]
mod tests {
  use super::*;

  use indoc::indoc;

  #[test]
  fn test_faker() -> Result<()> {
    let cddl = indoc!(
      r#"
        a = { 1: { a: { a: [ + tstr ] } } }
      "#
    );

    println!("{}", fake_json_from_cddl_str(&cddl)?);

    Ok(())
  }
}
