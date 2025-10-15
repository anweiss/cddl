//! Pest-based parser for CDDL
//!
//! This module provides an alternative parser implementation using the Pest parsing library.
//! It parses CDDL grammar as defined in RFC 8610 using the grammar file `cddl.pest`.

use pest::Parser;
use pest_derive::Parser;

/// Pest parser for CDDL
///
/// This parser is generated from the `cddl.pest` grammar file using pest_derive.
/// It provides an alternative parsing approach that can coexist with the existing
/// handwritten parser.
#[allow(missing_docs)]
#[derive(Parser)]
#[grammar = "cddl.pest"]
pub struct CddlParser;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pest_parser_basic() {
        // Test that the parser can be instantiated
        let input = "myrule = int\n";
        let result = CddlParser::parse(Rule::cddl, input);
        assert!(result.is_ok(), "Failed to parse basic CDDL: {:?}", result.err());
    }

    #[test]
    fn test_pest_parser_simple_rule() {
        let input = "person = { name: tstr, age: uint }\n";
        let result = CddlParser::parse(Rule::cddl, input);
        assert!(result.is_ok(), "Failed to parse struct rule: {:?}", result.err());
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
        assert!(result.is_ok(), "Failed to parse multiple rules: {:?}", result.err());
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
        assert!(result.is_ok(), "Failed to parse with comments: {:?}", result.err());
    }

    #[test]
    fn test_pest_parser_choice() {
        let input = "value = int / text / bool\n";
        let result = CddlParser::parse(Rule::cddl, input);
        assert!(result.is_ok(), "Failed to parse type choice: {:?}", result.err());
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
        assert!(result.is_ok(), "Failed to parse generic: {:?}", result.err());
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
        assert!(result.is_ok(), "Failed to parse control operator: {:?}", result.err());
    }

    #[test]
    fn test_pest_parser_tag() {
        let input = "tagged-value = #6.32(tstr)\n";
        let result = CddlParser::parse(Rule::cddl, input);
        assert!(result.is_ok(), "Failed to parse tag: {:?}", result.err());
    }
}
