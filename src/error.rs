use std::fmt;

#[cfg(target_arch = "wasm32")]
use serde::Serialize;

#[cfg(not(feature = "std"))]
use alloc::string::String;

#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
#[derive(Debug)]
pub struct ErrorMsg {
  short: String,
  extended: Option<String>,
}

impl fmt::Display for ErrorMsg {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.short)
  }
}

#[derive(Debug, Copy, Clone)]
pub enum MsgType {
  // Parser
  DuplicateRuleIdentifier,
  InvalidRuleIdentifier,
  MissingAssignmentToken,
  InvalidGenericSyntax,
  MissingGenericClosingDelimiter,
  InvalidGenericIdentifier,
  InvalidUnwrapSyntax,
  InvalidGroupToChoiceEnumSyntax,
  InvalidTagSyntax,
  MissingGroupEntryMemberKey,
  MissingGroupEntry,
  InvalidGroupEntrySyntax,
  MissingClosingDelimiter,
  MissingClosingParend,
  InvalidMemberKeyArrowMapSyntax,
  InvalidMemberKeySyntax,
  InvalidOccurrenceSyntax,
  NoRulesDefined,
  IncompleteRuleEntry,
  TypeSocketNamesMustBeTypeAugmentations,
  GroupSocketNamesMustBeGroupAugmentations,

  // Lexer
  UnableToAdvanceToken,
  InvalidControlOperator,
  InvalidCharacter,
  InvalidEscapeCharacter,
  InvalidTextStringLiteralCharacter,
  EmptyTextStringLiteral,
  InvalidByteStringLiteralCharacter,
  EmptyByteStringLiteral,
}

impl From<MsgType> for ErrorMsg {
  fn from(mt: MsgType) -> ErrorMsg {
    match mt {
      MsgType::DuplicateRuleIdentifier => ErrorMsg {
        short: "rule with the same identifier is already defined".into(),
        extended: None,
      },
      MsgType::InvalidRuleIdentifier => ErrorMsg {
        short: "expected rule identifier followed by an assignment token '=', '/=' or '//='".into(),
        extended: None,
      },
      MsgType::MissingAssignmentToken => ErrorMsg {
        short: "expected assignment token '=', '/=' or '//=' after rule identifier".into(),
        extended: None,
      },
      MsgType::InvalidGenericSyntax => ErrorMsg {
        short: "generic parameters should be between angle brackets '<' and '>' and separated by a comma ','".into(),
        extended: None,
      },
      MsgType::MissingGenericClosingDelimiter => ErrorMsg {
        short: "missing closing '>'".into(),
        extended: None,
      },
      MsgType::InvalidGenericIdentifier => ErrorMsg {
        short: "generic parameters must be named identifiers".into(),
        extended: None,
      },
      MsgType::InvalidUnwrapSyntax => ErrorMsg {
        short: "invalid unwrap syntax".into(),
        extended: None,
      },
      MsgType::InvalidGroupToChoiceEnumSyntax => ErrorMsg {
        short: "invalid group to choice enumeration syntax".into(),
        extended: None,
      },
      MsgType::InvalidTagSyntax => ErrorMsg {
        short: "invalid tag syntax".into(),
        extended: None,
      },
      MsgType::MissingGroupEntryMemberKey => ErrorMsg {
        short: "missing group entry member key".into(),
        extended: None,
      },
      MsgType::MissingGroupEntry => ErrorMsg {
        short: "missing group entry".into(),
        extended: None,
      },
      MsgType::InvalidGroupEntrySyntax => ErrorMsg {
        short: "invalid group entry syntax".into(),
        extended: None,
      },
      MsgType::MissingClosingDelimiter => ErrorMsg {
        short: "missing closing delimiter".into(),
        extended: None,
      },
      MsgType::MissingClosingParend => ErrorMsg {
        short: "missing closing parend ')'".into(),
        extended: None,
      },
      MsgType::InvalidMemberKeyArrowMapSyntax => ErrorMsg {
        short: "invalid memberkey. missing '=>'".into(),
        extended: None,
      },
      MsgType::InvalidMemberKeySyntax => ErrorMsg {
        short: "invalid memberkey. missing '=>' or ':'".into(),
        extended: None,
      },
      MsgType::InvalidOccurrenceSyntax => ErrorMsg {
        short: "invalid occurrence indicator syntax".into(),
        extended: None,
      },
      MsgType::UnableToAdvanceToken => ErrorMsg {
        short: "unable to advance to the next token".into(),
        extended: None,
      },
      MsgType::InvalidControlOperator => ErrorMsg {
        short: "invalid control operator".into(),
        extended: None,
      },
      MsgType::InvalidCharacter => ErrorMsg {
        short: "invalid character".into(),
        extended: None,
      },
      MsgType::InvalidEscapeCharacter => ErrorMsg {
        short: "invalid escape character".into(),
        extended: None,
      },
      MsgType::InvalidTextStringLiteralCharacter => ErrorMsg {
        short: "invalid character in text string literal. expected closing \"".into(),
        extended: None,
      },
      MsgType::EmptyTextStringLiteral => ErrorMsg {
        short: "empty text string literal".into(),
        extended: None,
      },
      MsgType::InvalidByteStringLiteralCharacter => ErrorMsg {
        short: "invalid character in byte string literal. expected closing '".into(),
        extended: None,
      },
      MsgType::EmptyByteStringLiteral => ErrorMsg {
        short: "empty byte string literal".into(),
        extended: None,
      },
      MsgType::NoRulesDefined => ErrorMsg {
        short: "you must have at least one rule defined".into(),
        extended: None,
      },
      MsgType::IncompleteRuleEntry => ErrorMsg {
        short: "missing rule entry after assignment".into(),
        extended: None,
      },
      MsgType::TypeSocketNamesMustBeTypeAugmentations => ErrorMsg {
        short: "all plugs for type socket names must be augmentations using '/='".into(),
        extended: None,
      },
      MsgType::GroupSocketNamesMustBeGroupAugmentations => ErrorMsg {
        short: "all plugs for group socket names must be augmentations using '//='".into(),
        extended: None,
      },
    }
  }
}
