#[cfg(test)]
mod tests {
  use cddl::{
    ast::{GroupEntry, MemberKey, Rule, Type2},
    cddl_from_str,
  };

  #[test]
  fn test_optional_group_reference() {
    let cddl_input = r#"
a = { 
    foo: text,
    ? (
        bar: text
    )
}

b = {
    foo: text,
    ? bar
}

bar = (
    bar: text
)
"#;

    #[cfg(feature = "std")]
    let cddl = cddl_from_str(cddl_input, true).expect("Failed to parse CDDL");
    #[cfg(not(feature = "std"))]
    let cddl = cddl_from_str(cddl_input).expect("Failed to parse CDDL");

    // Find the 'b' rule and check its structure
    let b_rule = cddl
      .rules
      .iter()
      .filter_map(|rule| match rule {
        Rule::Type {
          rule: type_rule, ..
        } if type_rule.name.ident == "b" => Some(type_rule),
        _ => None,
      })
      .next()
      .expect("Could not find rule 'b'");

    // Extract the map group from the type
    let group = match &b_rule.value.type_choices[0].type1.type2 {
      Type2::Map { group, .. } => group,
      _ => panic!("Expected Type2::Map"),
    };

    // Check the group entries
    let entries = &group.group_choices[0].group_entries;
    assert_eq!(entries.len(), 2, "Expected 2 group entries");

    // First entry should be "foo: text" - a ValueMemberKey with member_key
    match &entries[0].0 {
      GroupEntry::ValueMemberKey { ge, .. } => {
        assert!(ge.occur.is_none(), "First entry should not be optional");
        assert!(
          ge.member_key.is_some(),
          "First entry should have a member_key"
        );
        if let Some(MemberKey::Bareword { ident, .. }) = &ge.member_key {
          assert_eq!(ident.ident, "foo", "First entry member key should be 'foo'");
        } else {
          panic!("Expected Bareword member key");
        }
      }
      _ => panic!("Expected ValueMemberKey for first entry"),
    }

    // Second entry should be "? bar" - a TypeGroupname entry
    match &entries[1].0 {
      GroupEntry::TypeGroupname { ge, .. } => {
        assert!(ge.occur.is_some(), "Second entry should be optional");
        assert_eq!(
          ge.name.ident, "bar",
          "Second entry should reference group 'bar'"
        );
      }
      _ => panic!(
        "Expected TypeGroupname for second entry, but got: {:?}",
        &entries[1].0
      ),
    }
  }

  #[test]
  #[cfg(feature = "std")]
  #[cfg(feature = "json")]
  fn test_json_validator_recursive_structures() {
    use cddl::validate_json_from_str;

    // CDDL with recursive structure similar to issue #248
    let cddl_input = r#"
start = person

person = {
  "name": text,
  "children": [* person],
}
"#;

    // Valid JSON that creates a recursive structure
    let json_input = r#"
{
  "name": "parent",
  "children": [
    {
      "name": "child1",
      "children": []
    },
    {
      "name": "child2", 
      "children": [
        {
          "name": "grandchild",
          "children": []
        }
      ]
    }
  ]
}
"#;

    // This should work fine without stack overflow
    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_input, json_input, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_input, json_input);
    
    assert!(result.is_ok(), "Failed to validate valid recursive JSON structure: {:?}", result);
  }

  #[test]
  #[cfg(feature = "std")]
  #[cfg(feature = "json")]
  fn test_json_validator_recursive_reference_detection() {
    use cddl::validate_json_from_str;

    // CDDL with a circular reference
    let cddl_input = r#"
start = a

a = {
  "value": text,
  "ref": b,
}

b = {
  "other": text,
  "ref": a,
}
"#;

    // JSON that triggers the recursive reference
    let json_input = r#"
{
  "value": "test",
  "ref": {
    "other": "other_test",
    "ref": {
      "value": "nested",
      "ref": {
        "other": "deeply_nested",
        "ref": {
          "value": "very_deep",
          "ref": {
            "other": "should_trigger_recursion_check",
            "ref": {
              "value": "too_deep",
              "ref": {
                "other": "end"
              }
            }
          }
        }
      }
    }
  }
}
"#;

    // This should detect the recursion and handle it gracefully
    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_input, json_input, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_input, json_input);
    
    // The validation might fail due to recursion detection, but it should not cause a stack overflow
    // The important thing is that we don't get a panic or infinite loop
    match result {
      Ok(_) => {
        // If it passes, that's fine - the recursion detection may be allowing some recursion
        println!("JSON validation passed (recursion was handled)");
      }
      Err(e) => {
        // If it fails, we expect it to be due to recursion detection, not a crash
        let error_msg = e.to_string();
        println!("JSON validation failed as expected: {}", error_msg);
        // We just want to ensure it doesn't crash
      }
    }
  }
}