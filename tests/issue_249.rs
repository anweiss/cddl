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

    let cddl = cddl_from_str(cddl_input, true).expect("Failed to parse CDDL");

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
}
