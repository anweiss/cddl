use crate::{
    ast::*,
    token::{self, Token},
    visitor::{self, *}, validator::group_choice_alternates_from_ident,
  };
  
  use std::{borrow::Cow, collections::HashMap, convert::TryFrom, fmt};
  
  /// validation Result
  pub type Result = std::result::Result<(), Error>;
  
  /// validation error
  #[derive(Debug)]
  pub enum Error {
    /// Zero or more validation errors
    Validation(Vec<ValidationError>),
    /// CDDL parsing error
    CDDLParsing(String),
    /// UTF8 parsing error,
    UTF8Parsing(std::str::Utf8Error),
    /// Disabled feature
    DisabledFeature(String),
  }
  
  impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      match self {
        Error::Validation(errors) => {
          let mut error_str = String::new();
          for e in errors.iter() {
            error_str.push_str(&format!("{}\n", e));
          }
          write!(f, "{}", error_str)
        }
        Error::CDDLParsing(error) => write!(f, "error parsing CDDL: {}", error),
        Error::UTF8Parsing(error) => write!(f, "error pasing utf8: {}", error),
        Error::DisabledFeature(feature) => write!(f, "feature {} is not enabled", feature),
      }
    }
  }
  
  impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
      match self {
        _ => None,
      }
    }
  }
  
  impl Error {
    fn from_validator(pv: &ParentValidator, reason: String) -> Self {
      Error::Validation(vec![ValidationError {
        cddl_location: pv.cddl_location.clone(),
        reason,
      }])
    }
  }
  
  /// JSON validation error
  #[derive(Clone, Debug)]
  pub struct ValidationError {
    /// Error message
    pub reason: String,
    /// Location in CDDL where error occurred
    pub cddl_location: String,
  }
  
  impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      let mut error_str = String::from("error validating");
      write!(
        f,
        "{} at location {}: {}",
        error_str, self.cddl_location, self.reason
      )
    }
  }
  
  impl std::error::Error for ValidationError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
      None
    }
  }
  
  impl ValidationError {
    fn from_validator(pv: &ParentValidator, reason: String) -> Self {
      ValidationError {
        cddl_location: pv.cddl_location.clone(),
        reason,
      }
    }
  }
  
  type Rule = u16; // TODO
  
  /// validator type
  #[derive(Clone)]
  pub struct ParentValidator<'a> {
    cddl: &'a CDDL<'a>,
    stack: Vec<Rule>,
    errors: Vec<ValidationError>,
    cddl_location: String,
  }
  
  impl<'a> ParentValidator<'a> {
    pub fn new(cddl: &'a CDDL<'a>) -> Self {
      ParentValidator {
        cddl,
        stack: Vec::default(),
        errors: Vec::default(),
        cddl_location: String::new(),
      }
    }
  }
  
  impl<'a> Visitor<'a, Error> for ParentValidator<'a> {
    fn visit_type_rule(&mut self, tr: &TypeRule<'a>) -> visitor::Result<Error> {
      self.visit_gener
      Ok(())
    }
  
    fn visit_group_rule(&mut self, gr: &GroupRule<'a>) -> visitor::Result<Error> {
  
  
      Ok(())
    }
  
    fn visit_type(&mut self, t: &Type<'a>) -> visitor::Result<Error> {
      Ok(())
    }
  
    fn visit_group(&mut self, g: &Group<'a>) -> visitor::Result<Error> {
  
      Ok(())
    }
  
    fn visit_group_choice(&mut self, gc: &GroupChoice<'a>) -> visitor::Result<Error> {
      
  
      Ok(())
    }
  
    fn visit_range(
      &mut self,
      lower: &Type2,
      upper: &Type2,
      is_inclusive: bool,
    ) -> visitor::Result<Error> {
      // note: this is a case of type1
      Ok(())
    }
  
    fn visit_control_operator(
      &mut self,
      target: &Type2<'a>,
      ctrl: &str,
      controller: &Type2<'a>,
    ) -> visitor::Result<Error> {
      // note: this is a case of type1
  
      Ok(())
    }
  
    fn visit_type2(&mut self, t2: &Type2<'a>) -> visitor::Result<Error> {
      Ok(())
    }
  
    fn visit_identifier(&mut self, ident: &Identifier<'a>) -> visitor::Result<Error> {
      Ok(())
    }
  
    fn visit_value_member_key_entry(
      &mut self,
      entry: &ValueMemberKeyEntry<'a>,
    ) -> visitor::Result<Error> {
      // note: this is a case of grpent
      Ok(())
    }
  
    fn visit_type_groupname_entry(
      &mut self,
      entry: &TypeGroupnameEntry<'a>,
    ) -> visitor::Result<Error> {
      // note: this is a case of grpent
      Ok(())
    }
  
    fn visit_memberkey(&mut self, mk: &MemberKey<'a>) -> visitor::Result<Error> {
  
      Ok(())
    }
  
    fn visit_value(&mut self, value: &token::Value<'a>) -> visitor::Result<Error> {
  
      Ok(())
    }
  
    fn visit_occurrence(&mut self, o: &Occurrence) -> visitor::Result<Error> {
  
      Ok(())
    }
  }
  
  #[cfg(test)]
  #[cfg(not(target_arch = "wasm32"))]
  mod tests {
    #![allow(unused_imports)]
  
    use super::*;
  
    
  }
  