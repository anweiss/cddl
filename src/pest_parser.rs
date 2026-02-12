//! Pest-based parser for CDDL
//!
//! This module provides an alternative parser implementation using the Pest parsing library.
//! It parses CDDL grammar as defined in RFC 8610 using the grammar file `cddl.pest`.

#![allow(missing_docs)]

#[allow(unused_imports)]
use pest::Parser;
use pest_derive::Parser;

/// Pest parser for CDDL
///
/// This parser is generated from the `cddl.pest` grammar file using pest_derive.
/// It provides an alternative parsing approach that can coexist with the existing
/// handwritten parser.
///
/// The grammar file is located at the repository root as `cddl.pest`.
#[allow(missing_docs)]
#[derive(Parser)]
#[grammar = "../cddl.pest"]
pub struct CddlParser;

#[cfg(test)]
mod tests {
  use super::*;
  #[cfg(not(target_arch = "wasm32"))]
  use crate::cddl_from_str;

  #[test]
  fn test_pest_parser_basic() {
    // Test that the parser can be instantiated
    let input = "myrule = int\n";
    let result = CddlParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "Failed to parse basic CDDL: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_pest_parser_simple_rule() {
    let input = "person = { name: tstr, age: uint }\n";
    let result = CddlParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "Failed to parse struct rule: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_pest_parser_multiple_rules() {
    let input = r#"
person = {
  name: tstr,
  age: uint
}

address = {
  street: tstr,
  city: tstr
}
"#;
    let result = CddlParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "Failed to parse multiple rules: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_pest_parser_with_comments() {
    let input = r#"
; This is a comment
person = {
  name: tstr,  ; person's name
  age: uint   ; person's age
}
"#;
    let result = CddlParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "Failed to parse with comments: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_pest_parser_choice() {
    let input = "value = int / text / bool\n";
    let result = CddlParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "Failed to parse type choice: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_pest_parser_array() {
    let input = "my-array = [* int]\n";
    let result = CddlParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse array: {:?}", result.err());
  }

  #[test]
  fn test_pest_parser_generic() {
    let input = r#"
map<K, V> = { * K => V }
my-map = map<text, int>
"#;
    let result = CddlParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "Failed to parse generic: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_pest_parser_range() {
    let input = "port = 0..65535\n";
    let result = CddlParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse range: {:?}", result.err());
  }

  #[test]
  fn test_pest_parser_control_operator() {
    let input = r#"email = tstr .regexp "[^@]+@[^@]+""#;
    let result = CddlParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "Failed to parse control operator: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_pest_parser_tag() {
    let input = "tagged-value = #6.32(tstr)\n";
    let result = CddlParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse tag: {:?}", result.err());
  }

  #[cfg(not(target_arch = "wasm32"))]
  #[test]
  fn test_pest_parser_coexistence_with_existing_parser() {
    // Demonstrate that both the public API and Pest parser directly
    // can parse the same CDDL input
    let input = r#"
person = {
  name: tstr,
  age: uint
}
"#;

    // Test public API (now backed by Pest)
    let existing_result = cddl_from_str(input, true);
    assert!(
      existing_result.is_ok(),
      "Parser failed: {:?}",
      existing_result.err()
    );

    // Test Pest parser directly
    let pest_result = CddlParser::parse(Rule::cddl, input);
    assert!(
      pest_result.is_ok(),
      "Pest parser failed: {:?}",
      pest_result.err()
    );
  }

  #[test]
  fn test_pest_parser_rfc8610_examples() {
    // Test various examples from RFC 8610
    let examples = vec![
      "reputation-object = { application: tstr, reputons: [* reputon] }\n",
      "reputon = { rating: float16-32, ? confidence: float16-32, ? sample-size: uint }\n",
      "CDDLtest = thing\n",
      "thing = ( int / float )\n",
      "size = uint .size 4\n",
    ];

    for example in examples {
      let result = CddlParser::parse(Rule::cddl, example);
      assert!(
        result.is_ok(),
        "Failed to parse: {}\nError: {:?}",
        example,
        result.err()
      );
    }
  }
}
