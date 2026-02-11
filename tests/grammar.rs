// Test file for CDDL Pest grammar

#[cfg(test)]
mod grammar_tests {
  use pest::Parser;
  use pest_derive::Parser;
  use std::fs;

  #[derive(Parser)]
  #[grammar = "cddl.pest"]
  struct CDDLParser;

  #[test]
  fn test_basic_type_rule() {
    let input = "myrule = int";
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_type_rule_with_comment() {
    let input = "; This is a comment\nmyrule = text";
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_group_rule() {
    let input = "mygroup = (name: text, age: uint)";
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_map_structure() {
    let input = r#"person = {
  name: tstr,
  age: uint,
  ? email: tstr
}"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_array_structure() {
    let input = "coordinates = [x: int, y: int, z: int]";
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_type_choice() {
    let input = "value = int / text / bool";
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_group_choice() {
    let input = r#"entry = (
  ( name: text, id: int ) //
  ( label: text, value: int )
)"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_occurrence_indicators() {
    let input = r#"data = {
  ? optional: text,
  * any_number: int,
  + at_least_one: uint,
  1*5 limited: text
}"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_control_operators() {
    let input = r#"hash = bstr .size 32
email = tstr .regexp "[^@]+@[^@]+"
config = bstr .cbor { timeout: uint }"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_ranges() {
    let input = r#"port = 0..65535
age = 0..150"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_tags() {
    let input = r#"tagged = #6.32(tstr)
tagged_type = #6.<type>
major_type = #1.5"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_socket_plug() {
    let input = r#"$socket /= option1
$$group-socket //= (field: int)"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_generics() {
    let input = r#"map<K, V> = { * K => V }
my_map = map<text, int>"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_unwrap() {
    let input = "unwrapped = ~group_name";
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_cut_operator() {
    let input = r#"data = {
  type: int,
  tstr ^ => any
}"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_string_values() {
    let input = r#"name = "literal text"
escaped = "text with \"quotes\" and \n newline""#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_byte_strings() {
    let input = r#"b16 = h'48656c6c6f'
b64 = 'SGVsbG8='"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_numeric_values() {
    let input = r#"unsigned = 42
signed = -10
floating = 3.14
scientific = 1.5e10
hex_float = 0x1.5p10"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_arrow_map() {
    let input = r#"table = {
  * text => int
}"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_group_to_choice() {
    let input = r#"colors = &(red: 1, green: 2, blue: 3)
color_choice = &color_group"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
  }

  #[test]
  fn test_ethereum_address_cddl() {
    let content = fs::read_to_string("tests/fixtures/did/ethereumAddress/ethereumAddress.cddl")
      .expect("Failed to read ethereumAddress.cddl");
    let result = CDDLParser::parse(Rule::cddl, &content);
    assert!(
      result.is_ok(),
      "Failed to parse ethereumAddress.cddl: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_intellisense_demo_cddl() {
    let content = fs::read_to_string("cddl-lsp/client/testFixture/intellisense-demo.cddl")
      .expect("Failed to read intellisense-demo.cddl");
    let result = CDDLParser::parse(Rule::cddl, &content);
    assert!(
      result.is_ok(),
      "Failed to parse intellisense-demo.cddl: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_completion_cddl() {
    let content = fs::read_to_string("cddl-lsp/client/testFixture/completion.cddl")
      .expect("Failed to read completion.cddl");
    let result = CDDLParser::parse(Rule::cddl, &content);
    assert!(
      result.is_ok(),
      "Failed to parse completion.cddl: {:?}",
      result.err()
    );
  }

  // =========================================================================
  // RFC 9682 tests
  // =========================================================================

  #[test]
  fn test_rfc9682_empty_data_model() {
    // RFC 9682 Section 3.1: CDDL files can have zero rules
    let input = "";
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "Empty CDDL should be valid per RFC 9682: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_rfc9682_empty_data_model_with_comments() {
    // RFC 9682 Section 3.1: CDDL files with only comments and no rules
    let input = "; This is a module with only comments\n; No rules here\n";
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "Comment-only CDDL should be valid per RFC 9682: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_rfc9682_unicode_brace_escape() {
    // RFC 9682 Section 2.1.1: \u{hex} escape form
    let input = r#"a = "D\u{6f}mino's \u{1F073} + \u{2318}""#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "\\u{{hex}} escape should be valid per RFC 9682: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_rfc9682_unicode_brace_escape_leading_zeros() {
    // RFC 9682: \u{0...hex} with leading zeros is valid
    let input = r#"a = "test \u{006f}""#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "\\u{{hex}} with leading zeros should be valid: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_rfc9682_traditional_unicode_escape() {
    // Existing \uXXXX escapes still work
    let input = r#"b = "Domino's \uD83C\uDC73 + \u2318""#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "Traditional \\uXXXX escape should still work: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_rfc9682_nonliteral_tag_number() {
    // RFC 9682 Section 3.2: Non-literal tag numbers
    let input = r#"ct-tag<content> = #6.<ct-tag-number>(content)
ct-tag-number = 1668546817..1668612095"#;
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "Non-literal tag number should be valid per RFC 9682: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_rfc9682_simple_value_type_expression() {
    // RFC 9682 Section 3.2: #7.<head-number> with type expression
    let input = "my-simple = #7.<0..23>";
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "#7.<type> should be valid per RFC 9682: {:?}",
      result.err()
    );
  }

  #[test]
  fn test_rfc9682_simple_value_literal() {
    // RFC 9682 Section 3.2: #7.25 is float16
    let input = "my-float16 = #7.25";
    let result = CDDLParser::parse(Rule::cddl, input);
    assert!(
      result.is_ok(),
      "#7.25 should be valid per RFC 9682: {:?}",
      result.err()
    );
  }
}
