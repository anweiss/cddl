#[cfg(target_arch = "wasm32")]
extern crate console_error_panic_hook;

use super::token::{ByteValue, RangeValue, SocketPlug, Token, Value};
use std::{fmt, marker::PhantomData};

#[cfg(feature = "std")]
use std::borrow::Cow;

#[cfg(target_arch = "wasm32")]
use serde::{self, Serialize};

#[cfg(not(feature = "std"))]
use alloc::{
  borrow::Cow,
  boxed::Box,
  string::{String, ToString},
  vec::Vec,
};

/// Starting index, ending index and line number
#[cfg(feature = "ast-span")]
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
pub struct Span(pub usize, pub usize, pub usize);

#[cfg(feature = "ast-comments")]
#[derive(Default, Debug, PartialEq, Clone)]
#[doc(hidden)]
pub struct Comments<'a>(pub Vec<&'a str>);

#[cfg(feature = "ast-comments")]
impl<'a> Comments<'a> {
  fn any_non_newline(&self) -> bool {
    self.0.iter().any(|c| *c != "\n")
  }

  fn all_newline(&self) -> bool {
    self.0.iter().all(|c| *c == "\n")
  }
}

#[cfg(feature = "ast-comments")]
impl<'a> fmt::Display for Comments<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.all_newline() {
      return write!(f, "");
    }

    let mut comment_str = String::new();

    for comment in &self.0 {
      if *comment == "\n" {
        comment_str.push('\n')
      } else {
        comment_str.push_str(&format!(";{}\n", comment));
      }
    }

    write!(f, "{}", comment_str)
  }
}

/// CDDL AST
///
/// ```abnf
/// cddl = S 1*(rule S)
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Default, Debug, PartialEq, Clone)]
pub struct CDDL<'a> {
  /// Zero or more production rules
  #[cfg_attr(target_arch = "wasm32", serde(borrow))]
  pub rules: Vec<Rule<'a>>,

  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments: Option<Comments<'a>>,
}

impl<'a> fmt::Display for CDDL<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    #[cfg(target_arch = "wasm32")]
    console_error_panic_hook::set_once();

    let mut cddl_output = String::new();

    #[cfg(feature = "ast-comments")]
    if let Some(comments) = &self.comments {
      cddl_output.push_str(&comments.to_string());
    }

    let mut previous_single_line_type = false;
    #[cfg(feature = "ast-comments")]
    let mut previous_comments_after_rule = false;

    #[cfg(feature = "ast-comments")]
    for (idx, rule) in self.rules.iter().enumerate() {
      if rule.has_comments_after_rule() {
        cddl_output.push_str(&rule.to_string());
        previous_comments_after_rule = true;
      } else if idx == self.rules.len() - 1 || rule.has_single_line_type() {
        cddl_output.push_str(&format!("{}\n", rule.to_string().trim_end()));
        previous_single_line_type = true;
        previous_comments_after_rule = false;
      } else if previous_single_line_type && !previous_comments_after_rule {
        cddl_output.push_str(&format!("\n{}\n\n", rule.to_string().trim_end()));
        previous_single_line_type = false;
        previous_comments_after_rule = false;
      } else {
        cddl_output.push_str(&format!("{}\n\n", rule.to_string().trim_end()));
        previous_comments_after_rule = false;
      }
    }

    #[cfg(not(feature = "ast-comments"))]
    for (idx, rule) in self.rules.iter().enumerate() {
      if idx == self.rules.len() - 1 || rule.has_single_line_type() {
        cddl_output.push_str(&format!("{}\n", rule.to_string().trim_end()));
        previous_single_line_type = true;
      } else if previous_single_line_type {
        cddl_output.push_str(&format!("\n{}\n\n", rule.to_string().trim_end()));
        previous_single_line_type = false;
      } else {
        cddl_output.push_str(&format!("{}\n\n", rule.to_string().trim_end()));
      }
    }

    write!(f, "{}", cddl_output)
  }
}

/// Identifier for a type name, group name or bareword, with an optional socket
///
/// ```abnf
/// id = EALPHA *(*("-" / ".") (EALPHA / DIGIT))
/// ALPHA = %x41-5A / %x61-7A
/// EALPHA = ALPHA / "@" / "_" / "$"
/// DIGIT = %x30-39
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone)]
pub struct Identifier<'a> {
  /// Identifier
  pub ident: &'a str,
  /// Optional socket
  pub socket: Option<SocketPlug>,
  /// Span
  #[cfg(feature = "ast-span")]
  pub span: Span,
}

impl<'a> PartialEq for Identifier<'a> {
  fn eq(&self, other: &Self) -> bool {
    self.to_string() == other.to_string()
  }
}

impl<'a> Eq for Identifier<'a> {}

impl<'a> fmt::Display for Identifier<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(sp) = &self.socket {
      return write!(f, "{}{}", sp, self.ident);
    }

    write!(f, "{}", self.ident)
  }
}

impl<'a> From<&'static str> for Identifier<'a> {
  fn from(ident: &'static str) -> Self {
    let mut socket = ident.chars().take(2);

    if let Some(c) = socket.next() {
      if c == '$' {
        if let Some(c) = socket.next() {
          if c == '$' {
            return Identifier {
              ident,
              socket: Some(SocketPlug::GROUP),
              #[cfg(feature = "ast-span")]
              span: Span::default(),
            };
          }
        }

        return Identifier {
          ident,
          socket: Some(SocketPlug::TYPE),
          #[cfg(feature = "ast-span")]
          span: Span::default(),
        };
      }
    }

    Identifier {
      ident,
      socket: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }
  }
}

impl<'a> From<Token<'a>> for Identifier<'a> {
  fn from(token: Token) -> Self {
    let token = if let Some(token) = token.in_standard_prelude() {
      token
    } else {
      ""
    };

    Identifier::from(token)
  }
}

/// Type or group expression
///
/// ```abnf
/// rule = typename [genericparm] S assignt S type
///     / groupname [genericparm] S assigng S grpent
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq, Clone)]
pub enum Rule<'a> {
  /// Type expression
  Type {
    /// Type rule
    #[cfg_attr(target_arch = "wasm32", serde(borrow))]
    rule: TypeRule<'a>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_rule: Option<Comments<'a>>,
  },
  /// Group expression
  Group {
    /// Group rule
    rule: Box<GroupRule<'a>>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_rule: Option<Comments<'a>>,
  },
}

impl<'a> Rule<'a> {
  /// Return `Span` for `Rule`
  #[cfg(feature = "ast-span")]
  pub fn span(&self) -> Span {
    match self {
      Rule::Type { span, .. } => *span,
      Rule::Group { span, .. } => *span,
    }
  }

  #[cfg(feature = "ast-comments")]
  fn has_comments_after_rule(&self) -> bool {
    matches!(self, Rule::Type {
      comments_after_rule: Some(comments),
      ..
    }
    | Rule::Group {
      comments_after_rule: Some(comments),
      ..
    } if comments.any_non_newline())
  }

  fn has_single_line_type(&self) -> bool {
    if let Rule::Type {
      rule: TypeRule {
        value: Type { type_choices, .. },
        ..
      },
      ..
    } = self
    {
      let type_check = |tc: &TypeChoice| {
        matches!(
          tc.type1.type2,
          Type2::Typename { .. }
            | Type2::FloatValue { .. }
            | Type2::IntValue { .. }
            | Type2::UintValue { .. }
            | Type2::TextValue { .. }
            | Type2::B16ByteString { .. }
            | Type2::B64ByteString { .. }
        )
      };

      if type_choices.len() <= 2 && type_choices.iter().all(type_check) {
        return true;
      }
    }

    false
  }
}

impl<'a> fmt::Display for Rule<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Rule::Type {
        rule,
        #[cfg(feature = "ast-comments")]
        comments_after_rule,
        ..
      } => {
        let mut rule_str = String::new();

        rule_str.push_str(&rule.to_string());

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_rule {
          if comments.any_non_newline() {
            if let Some(&"\n") = comments.0.first() {
              rule_str.push_str(&comments.to_string());
            } else {
              rule_str.push_str(&format!(" {}", comments));
            }
          }
        }

        write!(f, "{}", rule_str)
      }
      Rule::Group {
        rule,
        #[cfg(feature = "ast-comments")]
        comments_after_rule,
        ..
      } => {
        let mut rule_str = String::new();

        rule_str.push_str(&rule.to_string());

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_rule {
          if comments.any_non_newline() {
            if let Some(&"\n") = comments.0.first() {
              rule_str.push_str(&comments.to_string());
            } else {
              rule_str.push_str(&format!(" {}", comments.to_string()));
            }
          }
        }

        write!(f, "{}", rule_str)
      }
    }
  }
}

impl<'a> Rule<'a> {
  /// Returns the name id of a rule
  pub fn name(&self) -> String {
    match self {
      Rule::Type { rule, .. } => rule.name.to_string(),
      Rule::Group { rule, .. } => rule.name.to_string(),
    }
  }

  /// Returns whether or not a rule extends an existing type or group rule with
  /// additional choices
  pub fn is_choice_alternate(&self) -> bool {
    match self {
      Rule::Type { rule, .. } => rule.is_type_choice_alternate,
      Rule::Group { rule, .. } => rule.is_group_choice_alternate,
    }
  }
}

/// Type expression
///
/// ```abnf
/// typename [genericparm] S assignt S type
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq, Clone)]
pub struct TypeRule<'a> {
  /// Type name identifier
  #[cfg_attr(target_arch = "wasm32", serde(borrow))]
  pub name: Identifier<'a>,
  /// Optional generic parameters
  pub generic_params: Option<GenericParams<'a>>,
  /// Extends an existing type choice
  pub is_type_choice_alternate: bool,
  /// Type value
  pub value: Type<'a>,

  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_before_assignt: Option<Comments<'a>>,
  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_after_assignt: Option<Comments<'a>>,
}

impl<'a> fmt::Display for TypeRule<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut tr_output = self.name.to_string();

    if let Some(gp) = &self.generic_params {
      tr_output.push_str(&gp.to_string());
    }

    #[cfg(feature = "ast-comments")]
    if let Some(comments) = &self.comments_before_assignt {
      tr_output.push_str(&comments.to_string());
    }

    if self.is_type_choice_alternate {
      tr_output.push_str(" /= ");
    } else {
      tr_output.push_str(" = ");
    }

    #[cfg(feature = "ast-comments")]
    if let Some(comments) = &self.comments_after_assignt {
      tr_output.push_str(&comments.to_string());
    }

    tr_output.push_str(&self.value.to_string());

    write!(f, "{}", tr_output)
  }
}

/// Group expression
///
/// ```abnf
/// groupname [genericparm] S assigng S grpent
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq, Clone)]
pub struct GroupRule<'a> {
  /// Group name identifier
  #[cfg_attr(target_arch = "wasm32", serde(borrow))]
  pub name: Identifier<'a>,
  /// Optional generic parameters
  pub generic_params: Option<GenericParams<'a>>,
  /// Extends an existing group choice
  pub is_group_choice_alternate: bool,
  /// Group entry
  pub entry: GroupEntry<'a>,

  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_before_assigng: Option<Comments<'a>>,
  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_after_assigng: Option<Comments<'a>>,
}

impl<'a> fmt::Display for GroupRule<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut gr_output = self.name.to_string();

    if let Some(gp) = &self.generic_params {
      gr_output.push_str(&gp.to_string());
    }

    #[cfg(feature = "ast-comments")]
    if let Some(comments) = &self.comments_before_assigng {
      gr_output.push_str(&comments.to_string());
    }

    if self.is_group_choice_alternate {
      gr_output.push_str(" //= ");
    } else {
      gr_output.push_str(" = ");
    }

    gr_output.push_str(&self.entry.to_string());

    #[cfg(feature = "ast-comments")]
    if let Some(comments) = &self.comments_after_assigng {
      gr_output.push_str(&comments.to_string());
    }

    write!(f, "{}", gr_output)
  }
}

/// Generic parameters
///
/// ```abnf
/// genericparm =  "<" S id S *("," S id S ) ">"
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq, Clone)]
pub struct GenericParams<'a> {
  /// List of generic parameters
  pub params: Vec<GenericParam<'a>>,
  /// Span
  #[cfg(feature = "ast-span")]
  pub span: Span,
}

impl<'a> Default for GenericParams<'a> {
  fn default() -> Self {
    GenericParams {
      params: Vec::new(),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }
  }
}

/// Generic parameter
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq, Clone)]
pub struct GenericParam<'a> {
  /// Generic parameter
  pub param: Identifier<'a>,

  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_before_ident: Option<Comments<'a>>,
  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_after_ident: Option<Comments<'a>>,
}

impl<'a> fmt::Display for GenericParams<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut gp = String::from("<");
    for (idx, parm) in self.params.iter().enumerate() {
      if idx != 0 {
        gp.push_str(", ");
      }

      #[cfg(feature = "ast-comments")]
      if let Some(comments) = &parm.comments_before_ident {
        gp.push_str(&comments.to_string());
      }

      gp.push_str(&parm.param.to_string());

      #[cfg(feature = "ast-comments")]
      if let Some(comments) = &parm.comments_after_ident {
        gp.push_str(&comments.to_string());
      }
    }

    gp.push('>');

    write!(f, "{}", gp)
  }
}

/// Generic arguments
///
/// ```abnf
/// genericarg = "<" S type1 S *("," S type1 S )  ">"
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct GenericArgs<'a> {
  /// Generic arguments
  pub args: Vec<GenericArg<'a>>,
  /// Span
  #[cfg(feature = "ast-span")]
  pub span: Span,
}

impl<'a> GenericArgs<'a> {
  /// Default `GenericArg`
  pub fn default() -> Self {
    GenericArgs {
      args: Vec::new(),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }
  }
}

/// Generic argument
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct GenericArg<'a> {
  /// Generic argument
  pub arg: Box<Type1<'a>>,

  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_before_type: Option<Comments<'a>>,
  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_after_type: Option<Comments<'a>>,
}

impl<'a> fmt::Display for GenericArgs<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut ga = String::from("<");
    for (idx, arg) in self.args.iter().enumerate() {
      if idx != 0 {
        ga.push_str(", ");
      }

      #[cfg(feature = "ast-comments")]
      if let Some(comments) = &arg.comments_before_type {
        ga.push_str(&comments.to_string());
      }

      ga.push_str(&arg.arg.to_string());

      #[cfg(feature = "ast-comments")]
      if let Some(comments) = &arg.comments_after_type {
        ga.push_str(&comments.to_string());
      }
    }

    ga.push('>');

    write!(f, "{}", ga)
  }
}

/// A generic rule that has undergone monomorphization
#[derive(Clone, Debug)]
pub struct GenericRule<'a> {
  /// Name
  pub name: &'a str,
  /// Parameters
  pub params: Vec<&'a str>,
  /// Arguments
  pub args: Vec<Type1<'a>>,
}

/// Type choices
///
/// ```abnf
/// type = type1 *(S "/" S  type1)
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Type<'a> {
  /// Type choices
  pub type_choices: Vec<TypeChoice<'a>>,
  /// Span
  #[cfg(feature = "ast-span")]
  pub span: Span,
}

impl<'a> Type<'a> {
  #[cfg(feature = "ast-comments")]
  #[doc(hidden)]
  pub fn comments_after_type(&mut self) -> Option<Comments<'a>> {
    if let Some(TypeChoice {
      type1: Type1 {
        comments_after_type,
        ..
      },
      ..
    }) = self.type_choices.last_mut()
    {
      if let Some(comments) = comments_after_type {
        if comments.any_non_newline() {
          return comments_after_type.take();
        }
      }
    }

    None
  }
}

impl<'a> From<&Identifier<'a>> for Type<'a> {
  fn from(ident: &Identifier<'a>) -> Self {
    Type {
      type_choices: vec![TypeChoice {
        type1: Type1 {
          type2: Type2::Typename {
            ident: ident.clone(),
            generic_args: None,
            #[cfg(feature = "ast-span")]
            span: ident.span,
          },
          operator: None,
          #[cfg(feature = "ast-span")]
          span: ident.span,
          #[cfg(feature = "ast-comments")]
          comments_after_type: None,
        },
        #[cfg(feature = "ast-comments")]
        comments_before_type: None,
        #[cfg(feature = "ast-comments")]
        comments_after_type: None,
      }],
      #[cfg(feature = "ast-span")]
      span: ident.span,
    }
  }
}

/// Type choice
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]

pub struct TypeChoice<'a> {
  /// Type choice
  pub type1: Type1<'a>,
  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_before_type: Option<Comments<'a>>,
  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_after_type: Option<Comments<'a>>,
}

impl<'a> fmt::Display for Type<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut type_str = String::new();

    for (idx, tc) in self.type_choices.iter().enumerate() {
      if idx == 0 {
        type_str.push_str(&tc.type1.to_string());

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = &tc.comments_after_type {
          type_str.push_str(comments.to_string().trim_end());
        }

        continue;
      }

      #[cfg(feature = "ast-comments")]
      if let Some(comments) = &tc.comments_before_type {
        type_str.push_str(&comments.to_string());
      }

      if self.type_choices.len() > 2 {
        type_str.push_str(&format!("\n\t/ {}", tc.type1.to_string()));
      } else {
        type_str.push_str(&format!(" / {}", tc.type1.to_string()));
      }

      #[cfg(feature = "ast-comments")]
      if let Some(comments) = &tc.comments_after_type {
        type_str.push_str(&comments.to_string());
      }
    }

    write!(f, "{}", type_str)
  }
}

impl<'a> Type<'a> {
  /// Used to delineate between grpent with `Type` and group entry with group
  /// name identifier `id`
  #[allow(clippy::type_complexity)]
  #[cfg(feature = "ast-span")]
  pub fn groupname_entry(&self) -> Option<(Identifier<'a>, Option<GenericArgs<'a>>, Span)> {
    if self.type_choices.len() == 1 {
      if let Some(tc) = self.type_choices.first() {
        if tc.type1.operator.is_none() {
          if let Type2::Typename {
            ident,
            generic_args,
            span,
          } = &tc.type1.type2
          {
            return Some((ident.clone(), generic_args.clone(), *span));
          }
        }
      }
    }

    None
  }

  /// Used to delineate between grpent with `Type` and group entry with group
  /// name identifier `id`
  #[allow(clippy::type_complexity)]
  #[cfg(not(feature = "ast-span"))]
  pub fn groupname_entry(&self) -> Option<(Identifier<'a>, Option<GenericArgs<'a>>)> {
    if self.type_choices.len() == 1 {
      if let Some(tc) = self.type_choices.first() {
        if tc.type1.operator.is_none() {
          if let Type2::Typename {
            ident,
            generic_args,
          } = &tc.type1.type2
          {
            return Some((ident.clone(), generic_args.clone()));
          }
        }
      }
    }

    None
  }
}

/// Type with optional range or control operator
///
/// ```abnf
/// type1 = type2 [S (rangeop / ctlop) S type2]
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Type1<'a> {
  /// Type
  pub type2: Type2<'a>,
  /// Range or control operator over a second type
  pub operator: Option<Operator<'a>>,
  /// Span
  #[cfg(feature = "ast-span")]
  pub span: Span,

  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_after_type: Option<Comments<'a>>,
}

impl<'a> From<Value<'a>> for Type1<'a> {
  fn from(value: Value<'a>) -> Self {
    #[cfg(feature = "ast-span")]
    let span = Span::default();
    let type2 = match value {
      Value::TEXT(value) => Type2::TextValue {
        value,
        #[cfg(feature = "ast-span")]
        span,
      },
      Value::INT(value) => Type2::IntValue {
        value,
        #[cfg(feature = "ast-span")]
        span,
      },
      Value::FLOAT(value) => Type2::FloatValue {
        value,
        #[cfg(feature = "ast-span")]
        span,
      },
      Value::UINT(value) => Type2::UintValue {
        value,
        #[cfg(feature = "ast-span")]
        span,
      },
      Value::BYTE(ByteValue::B16(value)) => Type2::B16ByteString {
        value,
        #[cfg(feature = "ast-span")]
        span,
      },
      Value::BYTE(ByteValue::B64(value)) => Type2::B64ByteString {
        value,
        #[cfg(feature = "ast-span")]
        span,
      },
      Value::BYTE(ByteValue::UTF8(value)) => Type2::UTF8ByteString {
        value,
        #[cfg(feature = "ast-span")]
        span,
      },
    };

    Type1 {
      type2,
      #[cfg(feature = "ast-span")]
      span,
      operator: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
    }
  }
}

#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
/// Range or control operator
pub struct Operator<'a> {
  /// Operator
  pub operator: RangeCtlOp<'a>,
  /// Type bound by range or control operator
  pub type2: Type2<'a>,

  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_before_operator: Option<Comments<'a>>,
  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_after_operator: Option<Comments<'a>>,
}

impl<'a> fmt::Display for Type1<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut t1_str = String::new();

    t1_str.push_str(&self.type2.to_string());

    if let Type2::Typename { .. } = self.type2 {
      if self.operator.is_some() {
        t1_str.push(' ');
      }
    }

    #[cfg(feature = "ast-comments")]
    if let Some(o) = &self.operator {
      if let Some(comments) = &o.comments_before_operator {
        t1_str.push_str(&comments.to_string());
      }

      t1_str.push_str(&o.operator.to_string());

      if let Some(comments) = &o.comments_after_operator {
        t1_str.push_str(&comments.to_string());
      }

      if let Type2::Typename { .. } = self.type2 {
        t1_str.push(' ');
      }

      t1_str.push_str(&o.type2.to_string());
    } else if let Some(comments) = &self.comments_after_type {
      if comments.any_non_newline() {
        t1_str.push_str(&format!(" {}", comments));
      }
    }

    #[cfg(not(feature = "ast-comments"))]
    if let Some(o) = &self.operator {
      t1_str.push_str(&o.operator.to_string());

      if let Type2::Typename { .. } = self.type2 {
        t1_str.push(' ');
      }

      t1_str.push_str(&o.type2.to_string());
    }

    write!(f, "{}", t1_str)
  }
}

/// Range or control operator
///
/// ```abnf
/// rangeop = "..." / ".."
/// ctlop = "." id
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, PartialEq, Clone)]
pub enum RangeCtlOp<'a> {
  /// Range operator
  RangeOp {
    /// Is inclusive
    is_inclusive: bool,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },
  /// Control operator
  CtlOp {
    /// Control identifier
    ctrl: &'a str,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },
}

impl<'a> fmt::Display for RangeCtlOp<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      RangeCtlOp::RangeOp {
        is_inclusive: false,
        ..
      } => write!(f, "..."),
      RangeCtlOp::RangeOp {
        is_inclusive: true, ..
      } => write!(f, ".."),
      RangeCtlOp::CtlOp { ctrl, .. } => write!(f, "{}", ctrl),
    }
  }
}

/// Type
///
/// ```abnf
/// type2 = value
///     / typename [genericarg]
///     / "(" S type S ")"
///     / "{" S group S "}"
///     / "[" S group S "]"
///     / "~" S typename [genericarg]
///     / "&" S "(" S group S ")"
///     / "&" S groupname [genericarg]
///     / "#" "6" ["." uint] "(" S type S ")"
///     / "#" DIGIT ["." uint]                ; major/ai
///     / "#"                                 ; any
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum Type2<'a> {
  /// Integer value
  IntValue {
    /// Value
    value: isize,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },

  /// Unsigned integer value
  UintValue {
    /// Value
    value: usize,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },

  /// Float value
  FloatValue {
    /// Value
    value: f64,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },

  /// Text string value (enclosed by '"')
  TextValue {
    /// Value
    value: Cow<'a, str>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },

  /// UTF-8 encoded byte string (enclosed by '')
  UTF8ByteString {
    /// Value
    value: Cow<'a, [u8]>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },

  /// Base 16 encoded prefixed byte string
  B16ByteString {
    /// Value
    value: Cow<'a, [u8]>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },

  /// Base 64 encoded (URL safe) prefixed byte string
  B64ByteString {
    /// Value
    value: Cow<'a, [u8]>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },

  /// Type name identifier with optional generic arguments
  Typename {
    /// Identifier
    ident: Identifier<'a>,
    /// Generic arguments
    generic_args: Option<GenericArgs<'a>>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },

  /// Parenthesized type expression (for operator precedence)
  ParenthesizedType {
    /// Type
    pt: Type<'a>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_before_type: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_type: Option<Comments<'a>>,
  },

  /// Map expression
  Map {
    /// Group
    group: Group<'a>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_before_group: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_group: Option<Comments<'a>>,
  },

  /// Array expression
  Array {
    /// Span
    group: Group<'a>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_before_group: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_group: Option<Comments<'a>>,
  },

  /// Unwrapped group
  Unwrap {
    /// Identifier
    ident: Identifier<'a>,
    /// Generic arguments
    generic_args: Option<GenericArgs<'a>>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments: Option<Comments<'a>>,
  },

  /// Enumeration expression over an inline group
  ChoiceFromInlineGroup {
    /// Group
    group: Group<'a>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_before_group: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_group: Option<Comments<'a>>,
  },

  /// Enumeration expression over previously defined group
  ChoiceFromGroup {
    /// Identifier
    ident: Identifier<'a>,
    /// Generic arguments
    generic_args: Option<GenericArgs<'a>>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments: Option<Comments<'a>>,
  },

  /// Tagged data item where the first element is an optional tag and the second
  /// is the type of the tagged value
  TaggedData {
    /// Tag
    tag: Option<usize>,
    /// Type
    t: Type<'a>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_before_type: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_type: Option<Comments<'a>>,
  },

  /// Data item of a major type with optional data constraint
  DataMajorType {
    /// Major type
    mt: u8,
    /// Constraint
    constraint: Option<usize>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },

  /// Any data item
  #[cfg(feature = "ast-span")]
  Any(Span),

  /// Any data item
  #[cfg(not(feature = "ast-span"))]
  Any,
}

#[allow(clippy::cognitive_complexity)]
impl<'a> fmt::Display for Type2<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Type2::IntValue { value, .. } => write!(f, "{}", value),
      Type2::UintValue { value, .. } => write!(f, "{}", value),
      Type2::FloatValue { value, .. } => write!(f, "{}", value),
      Type2::TextValue { value, .. } => write!(f, "\"{}\"", value),
      Type2::UTF8ByteString { value, .. } => write!(
        f,
        "'{}'",
        std::str::from_utf8(value).map_err(|_| fmt::Error)?
      ),
      Type2::B16ByteString { value, .. } => {
        write!(f, "{}", std::str::from_utf8(value).map_err(|_| fmt::Error)?)
      }
      Type2::B64ByteString { value, .. } => {
        write!(f, "{}", std::str::from_utf8(value).map_err(|_| fmt::Error)?)
      }
      Type2::Typename {
        ident,
        generic_args,
        ..
      } => {
        if let Some(args) = generic_args {
          return write!(f, "{}{}", ident, args);
        }

        write!(f, "{}", ident)
      }
      Type2::ParenthesizedType {
        #[cfg(feature = "ast-comments")]
        comments_before_type,
        pt,
        #[cfg(feature = "ast-comments")]
        comments_after_type,
        ..
      } => {
        let mut pt_str = String::from("(");

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_before_type {
          if comments.any_non_newline() {
            pt_str.push_str(&format!(" {}\t", comments));
            pt_str.push_str(&pt.to_string().trim_start().to_string());
          } else {
            pt_str.push_str(&pt.to_string());
          }
        } else {
          pt_str.push_str(&pt.to_string());
        }

        #[cfg(not(feature = "ast-comments"))]
        {
          pt_str.push_str(&pt.to_string());
        }

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_type {
          pt_str.push_str(&comments.to_string());
        }

        pt_str.push(')');

        write!(f, "{}", pt_str)
      }
      Type2::Map {
        #[cfg(feature = "ast-comments")]
        comments_before_group,
        group,
        #[cfg(feature = "ast-comments")]
        comments_after_group,
        ..
      } => {
        let mut t2_str = String::from("{");

        #[cfg(feature = "ast-comments")]
        let mut non_newline_comments_before_group = false;

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_before_group {
          if comments.any_non_newline() {
            non_newline_comments_before_group = true;
            t2_str.push_str(&format!(" {}\t", comments));
            t2_str.push_str(&group.to_string().trim_start().to_string());
          } else {
            t2_str.push_str(&group.to_string());
          }
        } else {
          t2_str.push_str(&group.to_string());
        }

        #[cfg(not(feature = "ast-comments"))]
        {
          t2_str.push_str(&group.to_string());
        }

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_group {
          t2_str.push_str(&comments.to_string());
        }

        #[cfg(feature = "ast-comments")]
        if non_newline_comments_before_group
          && !group
            .group_choices
            .iter()
            .any(|gc| gc.has_entries_with_trailing_comments())
        {
          t2_str.push('\n');
        }

        #[cfg(not(feature = "ast-comments"))]
        {
          t2_str.push('\n');
        }

        t2_str.push('}');

        write!(f, "{}", t2_str)
      }
      Type2::Array {
        #[cfg(feature = "ast-comments")]
        comments_before_group,
        group,
        #[cfg(feature = "ast-comments")]
        comments_after_group,
        ..
      } => {
        let mut t2_str = String::from("[");

        #[cfg(feature = "ast-comments")]
        let mut non_newline_comments_before_group = false;

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_before_group {
          if comments.any_non_newline() {
            non_newline_comments_before_group = true;
            for (idx, comment) in comments.0.iter().enumerate() {
              if *comment != "\n" {
                if idx == 0 {
                  t2_str.push_str(&format!(" ;{}", comment));
                } else {
                  t2_str.push_str(&format!("\t;{}\n", comment));
                }
              } else {
                t2_str.push('\n');
              }
            }

            t2_str.push_str(&format!("\t{}", group.to_string().trim_start()));
          } else {
            t2_str.push_str(&group.to_string());
          }
        } else {
          t2_str.push_str(&group.to_string());
        }

        #[cfg(not(feature = "ast-comments"))]
        {
          t2_str.push_str(&group.to_string());
        }

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_group {
          t2_str.push_str(&comments.to_string());
        }

        #[cfg(feature = "ast-comments")]
        if non_newline_comments_before_group
          && !group
            .group_choices
            .iter()
            .any(|gc| gc.has_entries_with_trailing_comments())
        {
          t2_str.push('\n');
        }

        #[cfg(not(feature = "ast-comments"))]
        t2_str.push('\n');

        t2_str.push(']');

        write!(f, "{}", t2_str)
      }
      Type2::Unwrap {
        #[cfg(feature = "ast-comments")]
        comments,
        ident,
        generic_args,
        ..
      } => {
        let mut t2_str = String::new();

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments {
          t2_str.push_str(&comments.to_string());
        }

        if let Some(args) = generic_args {
          t2_str.push_str(&format!("{}{}", ident, args));
        } else {
          t2_str.push_str(&ident.to_string());
        }

        write!(f, "{}", t2_str)
      }
      Type2::ChoiceFromInlineGroup {
        #[cfg(feature = "ast-comments")]
        comments,
        #[cfg(feature = "ast-comments")]
        comments_before_group,
        group,
        #[cfg(feature = "ast-comments")]
        comments_after_group,
        ..
      } => {
        let mut t2_str = String::from("&");

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments {
          t2_str.push_str(&comments.to_string());
        }

        t2_str.push('(');

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_before_group {
          t2_str.push_str(&comments.to_string());
        }

        t2_str.push_str(&group.to_string());

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_group {
          t2_str.push_str(&comments.to_string());
        }

        if group.group_choices.len() == 1 && group.group_choices[0].group_entries.len() == 1 {
          t2_str.push_str(" )");
        } else {
          t2_str.push(')');
        }

        write!(f, "{}", t2_str)
      }
      Type2::ChoiceFromGroup {
        #[cfg(feature = "ast-comments")]
        comments,
        ident,
        generic_args,
        ..
      } => {
        let mut t2_str = String::from("&");

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments {
          t2_str.push_str(&comments.to_string());
        }

        if let Some(ga) = generic_args {
          t2_str.push_str(&format!("{}{}", ident, ga));
        } else {
          t2_str.push_str(&ident.to_string());
        }

        write!(f, "{}", t2_str)
      }
      Type2::TaggedData {
        tag,
        #[cfg(feature = "ast-comments")]
        comments_before_type,
        t,
        #[cfg(feature = "ast-comments")]
        comments_after_type,
        ..
      } => {
        let mut t2_str = String::from("#6");

        if let Some(tag_uint) = tag {
          t2_str.push_str(&format!(".{}", tag_uint));
        }

        t2_str.push('(');

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_before_type {
          if comments.any_non_newline() {
            t2_str.push_str(&format!(" {}", comments));
          }
        }

        t2_str.push_str(&t.to_string());

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_type {
          if comments.any_non_newline() {
            t2_str.push_str(&format!(" {}", comments));
          }
        }

        t2_str.push(')');

        write!(f, "{}", t2_str)
      }
      Type2::DataMajorType { mt, constraint, .. } => {
        if let Some(c) = constraint {
          return write!(f, "{}.{}", mt, c);
        }

        write!(f, "{}", mt)
      }
      #[cfg(feature = "ast-span")]
      Type2::Any(_) => write!(f, "#"),
      #[cfg(not(feature = "ast-span"))]
      Type2::Any => write!(f, "#"),
    }
  }
}

impl<'a> From<RangeValue<'a>> for Type2<'a> {
  fn from(rv: RangeValue<'a>) -> Self {
    #[cfg(feature = "ast-span")]
    let span = Span::default();

    match rv {
      RangeValue::IDENT(ident) => Type2::Typename {
        ident: Identifier {
          ident: ident.0,
          socket: ident.1,
          #[cfg(feature = "ast-span")]
          span,
        },
        generic_args: None,
        #[cfg(feature = "ast-span")]
        span,
      },
      RangeValue::INT(value) => Type2::IntValue {
        value,
        #[cfg(feature = "ast-span")]
        span,
      },
      RangeValue::UINT(value) => Type2::UintValue {
        value,
        #[cfg(feature = "ast-span")]
        span,
      },
      RangeValue::FLOAT(value) => Type2::FloatValue {
        value,
        #[cfg(feature = "ast-span")]
        span,
      },
    }
  }
}

impl<'a> From<Type1<'a>> for Type2<'a> {
  fn from(type1: Type1<'a>) -> Self {
    Type2::ParenthesizedType {
      pt: Type {
        type_choices: vec![TypeChoice {
          type1,
          #[cfg(feature = "ast-comments")]
          comments_after_type: None,
          #[cfg(feature = "ast-comments")]
          comments_before_type: None,
        }],
        #[cfg(feature = "ast-span")]
        span: Span::default(),
      },
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }
  }
}

impl<'a> From<usize> for Type2<'a> {
  fn from(value: usize) -> Self {
    Type2::UintValue {
      value,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }
  }
}

impl<'a> From<isize> for Type2<'a> {
  fn from(value: isize) -> Self {
    Type2::IntValue {
      value,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }
  }
}

impl<'a> From<f64> for Type2<'a> {
  fn from(value: f64) -> Self {
    Type2::FloatValue {
      value,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }
  }
}

impl<'a> From<String> for Type2<'a> {
  fn from(value: String) -> Self {
    Type2::TextValue {
      value: value.into(),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }
  }
}

// Convenience method for testing
impl<'a> From<&'a str> for Type2<'a> {
  fn from(value: &'a str) -> Self {
    Type2::UTF8ByteString {
      value: value.as_bytes().into(),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }
  }
}

impl<'a> From<ByteValue<'a>> for Type2<'a> {
  fn from(value: ByteValue<'a>) -> Self {
    match value {
      ByteValue::UTF8(value) => Type2::UTF8ByteString {
        value,
        #[cfg(feature = "ast-span")]
        span: Span::default(),
      },
      ByteValue::B16(value) => Type2::B16ByteString {
        value,
        #[cfg(feature = "ast-span")]
        span: Span::default(),
      },
      ByteValue::B64(value) => Type2::B64ByteString {
        value,
        #[cfg(feature = "ast-span")]
        span: Span::default(),
      },
    }
  }
}

/// Retrieve `Type2` from token if it is a tag type in the standard prelude
pub fn tag_from_token<'a>(token: &Token) -> Option<Type2<'a>> {
  match token {
    Token::TDATE => Some(Type2::TaggedData {
      tag: Some(0),
      t: type_from_token(Token::TSTR),
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::TIME => Some(Type2::TaggedData {
      tag: Some(1),
      t: type_from_token(Token::NUMBER),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::BIGUINT => Some(Type2::TaggedData {
      tag: Some(2),
      t: type_from_token(Token::BSTR),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::BIGNINT => Some(Type2::TaggedData {
      tag: Some(3),
      t: type_from_token(Token::BSTR),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::DECFRAC => unimplemented!(),
    Token::BIGFLOAT => unimplemented!(),
    Token::EB64URL => Some(Type2::TaggedData {
      tag: Some(21),
      t: type_from_token(Token::ANY),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::EB64LEGACY => Some(Type2::TaggedData {
      tag: Some(22),
      t: type_from_token(Token::ANY),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::EB16 => Some(Type2::TaggedData {
      tag: Some(23),
      t: type_from_token(Token::ANY),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::ENCODEDCBOR => Some(Type2::TaggedData {
      tag: Some(24),
      t: type_from_token(Token::BSTR),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::URI => Some(Type2::TaggedData {
      tag: Some(32),
      t: type_from_token(Token::TSTR),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::B64URL => Some(Type2::TaggedData {
      tag: Some(33),
      t: type_from_token(Token::TSTR),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::B64LEGACY => Some(Type2::TaggedData {
      tag: Some(34),
      t: type_from_token(Token::TSTR),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::REGEXP => Some(Type2::TaggedData {
      tag: Some(35),
      t: type_from_token(Token::TSTR),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::MIMEMESSAGE => Some(Type2::TaggedData {
      tag: Some(36),
      t: type_from_token(Token::TSTR),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    Token::CBORANY => Some(Type2::TaggedData {
      tag: Some(55799),
      t: type_from_token(Token::ANY),
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-span")]
      span: Span::default(),
    }),
    _ => None,
  }
}

/// New `Type` from a given `token::Token`
pub fn type_from_token(token: Token) -> Type {
  Type {
    type_choices: vec![TypeChoice {
      type1: Type1 {
        #[cfg(feature = "ast-comments")]
        comments_after_type: None,
        operator: None,
        #[cfg(feature = "ast-span")]
        span: Span::default(),
        type2: Type2::Typename {
          ident: Identifier::from(token),
          generic_args: None,
          #[cfg(feature = "ast-span")]
          span: Span::default(),
        },
      },
      #[cfg(feature = "ast-comments")]
      comments_after_type: None,
      #[cfg(feature = "ast-comments")]
      comments_before_type: None,
    }],
    #[cfg(feature = "ast-span")]
    span: Span::default(),
  }
}

/// Group choices
///
/// ```abnf
/// group = grpchoice * (S "//" S grpchoice)
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Group<'a> {
  /// Group choices
  #[cfg_attr(target_arch = "wasm32", serde(borrow))]
  pub group_choices: Vec<GroupChoice<'a>>,
  /// Span
  #[cfg(feature = "ast-span")]
  pub span: Span,
}

impl<'a> fmt::Display for Group<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut group_str = String::new();

    for (idx, gc) in self.group_choices.iter().enumerate() {
      let mut gc_str = gc.to_string();

      #[cfg(feature = "ast-comments")]
      if self.group_choices.len() > 2
        && gc.group_entries.len() <= 3
        && !gc.has_entries_with_comments_before_comma()
      {
        gc_str = gc_str.replace('\n', "");
      }

      #[cfg(not(feature = "ast-comments"))]
      if self.group_choices.len() > 2 && gc.group_entries.len() <= 3 {
        gc_str = gc_str.replace('\n', "");
      }

      if idx == 0 {
        if self.group_choices.len() > 2 && gc.group_entries.len() <= 3 {
          group_str.push_str("\n\t");
          if gc_str.ends_with(' ') {
            gc_str.pop();
          }

          #[cfg(feature = "ast-comments")]
          if self.group_choices.len() > 2 && gc.has_entries_with_comments_before_comma() {
            gc_str = gc_str.replace('\n', "\n\t\t");
            group_str.push_str(gc_str.trim());
          } else {
            group_str.push_str(gc_str.trim_start());
          }

          #[cfg(not(feature = "ast-comments"))]
          if self.group_choices.len() > 2 {
            gc_str = gc_str.replace('\n', "\n\t\t");
            group_str.push_str(gc_str.trim());
          } else {
            group_str.push_str(gc_str.trim_start());
          }
        } else {
          group_str.push_str(&gc.to_string());
        }

        if self.group_choices.len() > 2 && gc.group_entries.len() <= 3 {
          group_str.push('\n');
        }

        continue;
      }

      gc_str = gc_str.trim().to_string();

      #[cfg(feature = "ast-comments")]
      if self.group_choices.len() > 2 && gc.has_entries_with_comments_before_comma() {
        gc_str = gc_str.replace('\n', "\n\t\t");
      }

      #[cfg(not(feature = "ast-comments"))]
      if self.group_choices.len() > 2 {
        gc_str = gc_str.replace('\n', "\n\t\t");
      }

      if self.group_choices.len() <= 2 {
        group_str.push_str(&format!("// {} ", gc_str));
      } else {
        group_str.push_str(&format!("\t// {}\n", gc_str));
      }
    }

    write!(f, "{}", group_str)
  }
}

/// Group entries
///
/// ```abnf
/// grpchoice = *(grpent optcom)
/// ```
///
/// If tuple is true, then entry is marked by a trailing comma
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct GroupChoice<'a> {
  /// Group entries where the second item in the tuple indicates where or not a
  /// trailing comma is present
  #[cfg_attr(target_arch = "wasm32", serde(borrow))]
  pub group_entries: Vec<(GroupEntry<'a>, OptionalComma<'a>)>,
  /// Span
  #[cfg(feature = "ast-span")]
  pub span: Span,

  // No trailing comments since these will be captured by the S ["," S] matching
  // rule
  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments_before_grpchoice: Option<Comments<'a>>,
}

impl<'a> GroupChoice<'a> {
  /// Create new group choice from group entries
  pub fn new(group_entries: Vec<GroupEntry<'a>>) -> Self {
    GroupChoice {
      group_entries: group_entries
        .iter()
        .cloned()
        .map(|ge| (ge, OptionalComma::default()))
        .collect::<Vec<_>>(),
      #[cfg(feature = "ast-span")]
      span: Span::default(),
      #[cfg(feature = "ast-comments")]
      comments_before_grpchoice: None,
    }
  }

  #[cfg(feature = "ast-comments")]
  fn has_entries_with_comments_before_comma(&self) -> bool {
    for ge in self.group_entries.iter() {
      if let GroupEntry::ValueMemberKey { ge: vmke, .. } = &ge.0 {
        if vmke
          .entry_type
          .type_choices
          .iter()
          .any(|tc| tc.type1.comments_after_type.is_some())
          && ge.1.optional_comma
        {
          return true;
        }
      }

      if let GroupEntry::TypeGroupname {
        trailing_comments, ..
      } = &ge.0
      {
        if trailing_comments.is_some() && ge.1.optional_comma {
          return true;
        }
      }
    }

    false
  }

  #[cfg(feature = "ast-comments")]
  fn has_entries_with_trailing_comments(&self) -> bool {
    self
      .group_entries
      .iter()
      .any(|ge| ge.0.has_trailing_comments() || ge.1.has_trailing_comments())
  }
}

impl<'a> fmt::Display for GroupChoice<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut gc_str = String::new();

    if self.group_entries.len() == 1 {
      gc_str.push_str(&format!(
        " {}{}",
        self.group_entries[0].0, self.group_entries[0].1
      ));

      #[cfg(feature = "ast-comments")]
      if !self.group_entries[0].1.has_trailing_comments() {
        gc_str.push(' ');
      }

      return write!(f, "{}", gc_str);
    }

    #[cfg(feature = "ast-comments")]
    if let Some(comments) = &self.comments_before_grpchoice {
      gc_str.push_str(&comments.to_string());
    }

    // Keep track of group entries with comments written before commas for
    // proper formatting
    #[cfg(feature = "ast-comments")]
    let mut entries_with_comment_before_comma: Vec<(usize, bool)> = Vec::new();

    #[cfg(feature = "ast-comments")]
    for (idx, ge) in self.group_entries.iter().enumerate() {
      if let GroupEntry::ValueMemberKey {
        trailing_comments: Some(comments),
        ..
      } = &ge.0
      {
        if comments.any_non_newline() && ge.1.optional_comma {
          entries_with_comment_before_comma.push((idx, true));

          continue;
        }
      }

      if let GroupEntry::TypeGroupname {
        trailing_comments: Some(comments),
        ..
      } = &ge.0
      {
        if comments.any_non_newline() && ge.1.optional_comma {
          entries_with_comment_before_comma.push((idx, true));

          continue;
        }
      }

      entries_with_comment_before_comma.push((idx, false));
    }

    #[cfg(feature = "ast-comments")]
    let has_trailing_comments_after_comma = self
      .group_entries
      .iter()
      .any(|ge| ge.1.has_trailing_comments());

    #[cfg(feature = "ast-comments")]
    if self.group_entries.len() > 3
      || (self.group_entries.len() <= 3 && has_trailing_comments_after_comma)
    {
      gc_str.push('\n');
    } else {
      gc_str.push(' ');
    }

    #[cfg(not(feature = "ast-comments"))]
    if self.group_entries.len() > 3 {
      gc_str.push('\n');
    } else {
      gc_str.push(' ');
    }

    for (idx, ge) in self.group_entries.iter().enumerate() {
      #[cfg(feature = "ast-comments")]
      if self.group_entries.len() > 3
        || (self.group_entries.len() <= 3 && has_trailing_comments_after_comma)
      {
        gc_str.push('\t');
      }

      #[cfg(not(feature = "ast-comments"))]
      if self.group_entries.len() > 3 {
        gc_str.push('\t');
      }

      #[cfg(feature = "ast-comments")]
      if entries_with_comment_before_comma.iter().any(|e| e.1) {
        if idx == 0 {
          if entries_with_comment_before_comma[idx].1 {
            gc_str.push_str(&format!("{}", ge.0));
          } else {
            gc_str.push_str(&format!("{}\n", ge.0));
          }
        } else if entries_with_comment_before_comma[idx].1 {
          gc_str.push_str(&format!(", {}", ge.0));
        } else if idx != self.group_entries.len() - 1 {
          gc_str.push_str(&format!(", {}\n", ge.0.to_string().trim_end()));
        } else {
          gc_str.push_str(&format!(", {}", ge.0.to_string().trim_end()));
        }
      } else {
        gc_str.push_str(&format!(
          "{}{}",
          ge.0.to_string().trim_end(),
          ge.1.to_string().trim_end()
        ));

        // if idx != self.group_entries.len() - 1 {
        //   gc_str.push_str(",");
        // }

        if self.group_entries.len() <= 3 && !has_trailing_comments_after_comma {
          gc_str.push(' ');
        }
      }

      #[cfg(not(feature = "ast-comments"))]
      {
        gc_str.push_str(&format!(
          "{}{}",
          ge.0.to_string().trim_end(),
          ge.1.to_string().trim_end()
        ));

        // if idx != self.group_entries.len() - 1 {
        //   gc_str.push_str(",");
        // }

        if self.group_entries.len() <= 3 {
          gc_str.push(' ');
        }
      }

      if idx == self.group_entries.len() - 1 && self.group_entries.len() > 3 {
        gc_str.push('\n');

        break;
      }

      #[cfg(feature = "ast-comments")]
      if self.group_entries.len() > 3 && entries_with_comment_before_comma.iter().all(|e| !e.1) {
        gc_str.push('\n');
      }

      #[cfg(not(feature = "ast-comments"))]
      if self.group_entries.len() > 3 {
        gc_str.push('\n');
      }
    }

    write!(f, "{}", gc_str)
  }
}

/// Group entry
///
/// ```abnf
/// grpent = [occur S] [memberkey S] type
///       / [occur S] groupname [genericarg]  ; preempted by above
///       / [occur S] "(" S group S ")"
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum GroupEntry<'a> {
  /// Value group entry type
  ValueMemberKey {
    /// Group entry
    #[cfg_attr(target_arch = "wasm32", serde(borrow))]
    ge: Box<ValueMemberKeyEntry<'a>>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    leading_comments: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    trailing_comments: Option<Comments<'a>>,
  },

  /// Group entry from a named group or type
  TypeGroupname {
    /// Group entry
    #[cfg_attr(target_arch = "wasm32", serde(borrow))]
    ge: TypeGroupnameEntry<'a>,
    /// span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    leading_comments: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    trailing_comments: Option<Comments<'a>>,
  },

  /// Parenthesized group with optional occurrence indicator
  InlineGroup {
    /// Occurrence
    occur: Option<Occurrence<'a>>,
    /// Group
    group: Group<'a>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_before_group: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_group: Option<Comments<'a>>,
  },
}

impl<'a> GroupEntry<'a> {
  #[cfg(feature = "ast-comments")]
  fn has_trailing_comments(&self) -> bool {
    matches!(self,
      GroupEntry::ValueMemberKey {
        trailing_comments: Some(comments),
        ..
      }
      | GroupEntry::TypeGroupname {
        trailing_comments: Some(comments),
        ..
      } if comments.any_non_newline()
    )
  }
}

/// Optional comma
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq, Default)]
pub struct OptionalComma<'a> {
  /// Optional comma
  pub optional_comma: bool,

  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub trailing_comments: Option<Comments<'a>>,

  #[doc(hidden)]
  pub _a: PhantomData<&'a ()>,
}

impl<'a> fmt::Display for OptionalComma<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut optcomma_str = String::new();

    if self.optional_comma {
      optcomma_str.push(',');
    }

    #[cfg(feature = "ast-comments")]
    if let Some(comments) = &self.trailing_comments {
      if comments.any_non_newline() {
        if let Some(comment) = comments.0.first() {
          if *comment != "\n" && self.optional_comma {
            optcomma_str.push(' ');
          }
        }

        for (idx, &comment) in comments.0.iter().enumerate() {
          if idx == 0 && comment != "\n" {
            optcomma_str.push_str(&format!(";{}\n", comment));
          } else if idx == 0 {
            optcomma_str.push_str(&comment.to_string());
          } else if comment != "\n" {
            optcomma_str.push_str(&format!("\t;{}\n", comment));
          } else {
            optcomma_str.push_str(&format!("\t{}", comment));
          }
        }
      }
    }

    write!(f, "{}", optcomma_str)
  }
}

impl<'a> OptionalComma<'a> {
  #[cfg(feature = "ast-comments")]
  fn has_trailing_comments(&self) -> bool {
    if let Some(comments) = &self.trailing_comments {
      return comments.any_non_newline();
    }

    false
  }
}

impl<'a> fmt::Display for GroupEntry<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      GroupEntry::ValueMemberKey {
        ge,
        #[cfg(feature = "ast-comments")]
        leading_comments,
        #[cfg(feature = "ast-comments")]
        trailing_comments,
        ..
      } => {
        let mut ge_str = String::new();

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = leading_comments {
          ge_str.push_str(&comments.to_string());
        }

        ge_str.push_str(&ge.to_string());

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = trailing_comments {
          if comments.any_non_newline() {
            ge_str.push_str(&format!(" {}", comments));
          }
        }

        write!(f, "{}", ge_str)
      }
      GroupEntry::TypeGroupname {
        ge,
        #[cfg(feature = "ast-comments")]
        leading_comments,
        #[cfg(feature = "ast-comments")]
        trailing_comments,
        ..
      } => {
        let mut ge_str = String::new();

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = leading_comments {
          ge_str.push_str(&comments.to_string());
        }

        ge_str.push_str(&ge.to_string());

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = trailing_comments {
          if comments.any_non_newline() {
            ge_str.push_str(&format!(" {}", comments));
          }
        }

        write!(f, "{}", ge_str)
      }
      GroupEntry::InlineGroup {
        occur,
        group,
        #[cfg(feature = "ast-comments")]
        comments_before_group,
        #[cfg(feature = "ast-comments")]
        comments_after_group,
        ..
      } => {
        let mut ge_str = String::new();

        if let Some(o) = occur {
          ge_str.push_str(&format!("{} ", o.occur.to_string()));

          #[cfg(feature = "ast-comments")]
          if let Some(comments) = &o.comments {
            ge_str.push_str(&comments.to_string());
          }
        }

        ge_str.push('(');

        #[cfg(feature = "ast-comments")]
        let mut non_newline_comments_before_group = false;

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_before_group {
          if comments.any_non_newline() {
            non_newline_comments_before_group = true;

            ge_str.push_str(&format!(" {}", comments));

            if !group
              .group_choices
              .iter()
              .all(|gc| gc.group_entries.is_empty())
            {
              ge_str.push_str(&format!("\t{}", group.to_string().trim_start()));
            }
          } else {
            ge_str.push_str(&group.to_string());
          }
        } else {
          ge_str.push_str(&group.to_string());
        }

        #[cfg(not(feature = "ast-comments"))]
        {
          ge_str.push_str(&group.to_string());
        }

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_group {
          ge_str.push_str(&comments.to_string());
        }

        #[cfg(feature = "ast-comments")]
        if non_newline_comments_before_group
          && !group
            .group_choices
            .iter()
            .any(|gc| gc.has_entries_with_trailing_comments())
        {
          ge_str.push('\n');
        }

        #[cfg(not(feature = "ast-comments"))]
        ge_str.push('\n');

        ge_str.push(')');

        write!(f, "{}", ge_str)
      }
    }
  }
}

/// Occurrence indicator
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Occurrence<'a> {
  /// Occurrence indicator
  pub occur: Occur,

  #[cfg(feature = "ast-comments")]
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  pub comments: Option<Comments<'a>>,

  #[doc(hidden)]
  pub _a: PhantomData<&'a ()>,
}

impl<'a> fmt::Display for Occurrence<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    #[cfg(feature = "ast-comments")]
    let mut occur_str = self.occur.to_string();
    #[cfg(not(feature = "ast-comments"))]
    let occur_str = self.occur.to_string();

    #[cfg(feature = "ast-comments")]
    if let Some(comments) = &self.comments {
      occur_str.push_str(&comments.to_string());
    }

    write!(f, "{}", occur_str)
  }
}

/// Value group entry type with optional occurrence indicator and optional
/// member key
///
/// ```abnf
/// [occur S] [memberkey S] type
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ValueMemberKeyEntry<'a> {
  /// Optional occurrence indicator
  pub occur: Option<Occurrence<'a>>,
  /// Optional member key
  pub member_key: Option<MemberKey<'a>>,
  /// Entry type
  #[cfg_attr(target_arch = "wasm32", serde(borrow))]
  pub entry_type: Type<'a>,
}

impl<'a> fmt::Display for ValueMemberKeyEntry<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut vmke_str = String::new();

    if let Some(o) = &self.occur {
      vmke_str.push_str(&format!("{} ", o.to_string()));
    }

    if let Some(mk) = &self.member_key {
      vmke_str.push_str(&format!("{} ", mk.to_string()));
    }

    vmke_str.push_str(&self.entry_type.to_string());

    write!(f, "{}", vmke_str)
  }
}

/// Group entry from a named type or group
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct TypeGroupnameEntry<'a> {
  /// Optional occurrence indicator
  pub occur: Option<Occurrence<'a>>,
  /// Type or group name identifier
  #[cfg_attr(target_arch = "wasm32", serde(borrow))]
  pub name: Identifier<'a>,
  /// Optional generic arguments
  pub generic_args: Option<GenericArgs<'a>>,
}

impl<'a> fmt::Display for TypeGroupnameEntry<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut tge_str = String::new();

    if let Some(o) = &self.occur {
      tge_str.push_str(&format!("{} ", o.to_string()));
    }

    tge_str.push_str(&self.name.to_string());

    if let Some(ga) = &self.generic_args {
      tge_str.push_str(&ga.to_string());
    }

    write!(f, "{}", tge_str)
  }
}

/// Member key
/// ```abnf
/// memberkey = type1 S ["^" S] "=>"
///           / bareword S ":"
///           / value S ":"
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum MemberKey<'a> {
  /// Type expression
  Type1 {
    /// Type1
    #[cfg_attr(target_arch = "wasm32", serde(borrow))]
    t1: Box<Type1<'a>>,
    /// Is cut indicator present
    is_cut: bool,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_before_cut: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_cut: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_arrowmap: Option<Comments<'a>>,
  },

  /// Bareword string type
  Bareword {
    /// Identifier
    #[cfg_attr(target_arch = "wasm32", serde(borrow))]
    ident: Identifier<'a>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_colon: Option<Comments<'a>>,
  },

  /// Value type
  Value {
    /// Value
    #[cfg_attr(target_arch = "wasm32", serde(borrow))]
    value: Value<'a>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,

    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    #[cfg_attr(target_arch = "wasm32", serde(skip))]
    #[doc(hidden)]
    comments_after_colon: Option<Comments<'a>>,
  },

  // Used while parsing a parenthesized type that is not followed by a cut nor
  // an arrow map
  #[cfg_attr(target_arch = "wasm32", serde(skip))]
  #[doc(hidden)]
  ParenthesizedType {
    non_member_key: ParenthesizedType<'a>,
    #[cfg(feature = "ast-comments")]
    comments_before_type_or_group: Option<Comments<'a>>,
    #[cfg(feature = "ast-comments")]
    comments_after_type_or_group: Option<Comments<'a>>,
  },
}

#[derive(Debug, Clone, PartialEq)]
#[doc(hidden)]
pub enum ParenthesizedType<'a> {
  Group(Group<'a>),
  Type(Type<'a>),
}

impl<'a> fmt::Display for MemberKey<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      MemberKey::Type1 {
        t1,
        #[cfg(feature = "ast-comments")]
        comments_before_cut,
        is_cut,
        #[cfg(feature = "ast-comments")]
        comments_after_cut,
        #[cfg(feature = "ast-comments")]
        comments_after_arrowmap,
        ..
      } => {
        let mut mk_str = format!("{} ", t1);

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_before_cut {
          if comments.any_non_newline() {
            mk_str.push_str(&comments.to_string());
          }
        }

        if *is_cut {
          mk_str.push_str("^ ");
        }

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_cut {
          if comments.any_non_newline() {
            mk_str.push_str(&comments.to_string());
          }
        }

        mk_str.push_str("=>");

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_arrowmap {
          if comments.any_non_newline() {
            mk_str.push_str(&format!(" {}", comments));
          }
        }

        write!(f, "{}", mk_str)
      }
      MemberKey::Bareword {
        ident,
        #[cfg(feature = "ast-comments")]
        comments,
        #[cfg(feature = "ast-comments")]
        comments_after_colon,
        ..
      } => {
        let mut mk_str = format!("{}", ident);

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments {
          if comments.any_non_newline() {
            mk_str.push_str(&format!(" {}", comments));
          }
        }

        mk_str.push(':');

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_colon {
          if comments.any_non_newline() {
            mk_str.push_str(&format!(" {}", comments));
          }
        }

        write!(f, "{}", mk_str)
      }
      MemberKey::Value {
        value,
        #[cfg(feature = "ast-comments")]
        comments,
        #[cfg(feature = "ast-comments")]
        comments_after_colon,
        ..
      } => {
        let mut mk_str = format!("{}", value);

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments {
          if comments.any_non_newline() {
            mk_str.push_str(&format!(" {}", comments));
          }
        }

        mk_str.push(':');

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_colon {
          if comments.any_non_newline() {
            mk_str.push_str(&format!(" {}", comments));
          }
        }

        write!(f, "{}", mk_str)
      }
      MemberKey::ParenthesizedType {
        non_member_key: ParenthesizedType::Group(g),
        #[cfg(feature = "ast-comments")]
        comments_before_type_or_group,
        #[cfg(feature = "ast-comments")]
        comments_after_type_or_group,
      } => {
        let mut nmk_str = String::new();

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_before_type_or_group {
          nmk_str.push_str(&comments.to_string());
        }

        nmk_str.push_str(&g.to_string());

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_type_or_group {
          nmk_str.push_str(&comments.to_string());
        }

        write!(f, "{}", nmk_str)
      }
      MemberKey::ParenthesizedType {
        non_member_key: ParenthesizedType::Type(t),
        #[cfg(feature = "ast-comments")]
        comments_before_type_or_group,
        #[cfg(feature = "ast-comments")]
        comments_after_type_or_group,
      } => {
        let mut nmk_str = String::new();

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_before_type_or_group {
          nmk_str.push_str(&comments.to_string());
        }

        nmk_str.push_str(&t.to_string());

        #[cfg(feature = "ast-comments")]
        if let Some(comments) = comments_after_type_or_group {
          nmk_str.push_str(&comments.to_string());
        }

        write!(f, "{}", nmk_str)
      }
    }
  }
}

/// Occurrence indicator
/// ```abnf
/// occur = [uint] "*" [uint]
///       / "+"
///       / "?"
/// ```
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum Occur {
  /// Occurrence indicator in the form n*m, where n is an optional lower limit
  /// and m is an optional upper limit
  Exact {
    /// Lower bound
    lower: Option<usize>,
    /// Upper bound
    upper: Option<usize>,
    /// Span
    #[cfg(feature = "ast-span")]
    span: Span,
  },

  /// Occurrence indicator in the form *, allowing zero or more occurrences
  #[cfg(feature = "ast-span")]
  ZeroOrMore(Span),

  /// Occurrence indicator in the form *, allowing zero or more occurrences
  #[cfg(not(feature = "ast-span"))]
  ZeroOrMore,

  /// Occurrence indicator in the form +, allowing one or more occurrences
  #[cfg(feature = "ast-span")]
  OneOrMore(Span),

  #[cfg(not(feature = "ast-span"))]
  /// Occurrence indicator in the form +, allowing one or more occurrences
  OneOrMore,

  /// Occurrence indicator in the form ?, allowing an optional occurrence
  #[cfg(feature = "ast-span")]
  Optional(Span),

  /// Occurrence indicator in the form ?, allowing an optional occurrence
  #[cfg(not(feature = "ast-span"))]
  Optional,
}

impl fmt::Display for Occur {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      #[cfg(feature = "ast-span")]
      Occur::ZeroOrMore(_) => write!(f, "*"),
      #[cfg(not(feature = "ast-span"))]
      Occur::ZeroOrMore => write!(f, "*"),
      Occur::Exact { lower, upper, .. } => {
        if let Some(li) = lower {
          if let Some(ui) = upper {
            return write!(f, "{}*{}", li, ui);
          }

          return write!(f, "{}*", li);
        }

        if let Some(ui) = upper {
          return write!(f, "*{}", ui);
        }

        write!(f, "*")
      }
      #[cfg(feature = "ast-span")]
      Occur::OneOrMore(_) => write!(f, "+"),
      #[cfg(not(feature = "ast-span"))]
      Occur::OneOrMore => write!(f, "+"),
      #[cfg(feature = "ast-span")]
      Occur::Optional(_) => write!(f, "?"),
      #[cfg(not(feature = "ast-span"))]
      Occur::Optional => write!(f, "?"),
    }
  }
}

#[cfg(test)]
#[allow(unused_imports)]
#[cfg(feature = "ast-comments")]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn verify_groupentry_output() {
    assert_eq!(
      GroupEntry::TypeGroupname {
        ge: TypeGroupnameEntry {
          occur: None,
          name: Identifier::from("entry1"),
          generic_args: None,
        },
        leading_comments: None,
        trailing_comments: None,
        #[cfg(feature = "ast-span")]
        span: Span::default(),
      }
      .to_string(),
      "entry1".to_string()
    )
  }

  #[test]
  fn verify_group_output() {
    assert_eq!(
      Group {
        group_choices: vec![GroupChoice {
          group_entries: vec![
            (
              GroupEntry::ValueMemberKey {
                ge: Box::from(ValueMemberKeyEntry {
                  occur: None,
                  member_key: Some(MemberKey::Bareword {
                    ident: "key1".into(),
                    comments: None,
                    comments_after_colon: None,
                    #[cfg(feature = "ast-span")]
                    span: Span::default(),
                  }),
                  entry_type: Type {
                    type_choices: vec![TypeChoice {
                      type1: Type1 {
                        type2: Type2::TextValue {
                          value: "value1".into(),
                          #[cfg(feature = "ast-span")]
                          span: Span::default(),
                        },
                        operator: None,
                        comments_after_type: None,
                        #[cfg(feature = "ast-span")]
                        span: Span::default(),
                      },
                      comments_before_type: None,
                      comments_after_type: None,
                    }],
                    #[cfg(feature = "ast-span")]
                    span: Span::default(),
                  },
                }),
                leading_comments: None,
                trailing_comments: None,
                #[cfg(feature = "ast-span")]
                span: Span::default(),
              },
              OptionalComma {
                optional_comma: true,
                trailing_comments: None,
                _a: PhantomData::default(),
              }
            ),
            (
              GroupEntry::ValueMemberKey {
                ge: Box::from(ValueMemberKeyEntry {
                  occur: None,
                  member_key: Some(MemberKey::Bareword {
                    ident: "key2".into(),
                    comments: None,
                    comments_after_colon: None,
                    #[cfg(feature = "ast-span")]
                    span: Span::default(),
                  }),
                  entry_type: Type {
                    type_choices: vec![TypeChoice {
                      type1: Type1 {
                        type2: Type2::TextValue {
                          value: "value2".into(),
                          #[cfg(feature = "ast-span")]
                          span: Span::default(),
                        },
                        operator: None,
                        comments_after_type: None,
                        #[cfg(feature = "ast-span")]
                        span: Span::default(),
                      },
                      comments_before_type: None,
                      comments_after_type: None,
                    }],
                    #[cfg(feature = "ast-span")]
                    span: Span::default(),
                  },
                }),
                leading_comments: None,
                trailing_comments: None,
                #[cfg(feature = "ast-span")]
                span: Span::default(),
              },
              OptionalComma {
                optional_comma: true,
                trailing_comments: None,
                _a: PhantomData::default(),
              }
            ),
          ],
          comments_before_grpchoice: None,
          #[cfg(feature = "ast-span")]
          span: Span::default(),
        }],
        #[cfg(feature = "ast-span")]
        span: Span::default(),
      }
      .to_string(),
      " key1: \"value1\", key2: \"value2\", ".to_string()
    )
  }
}
