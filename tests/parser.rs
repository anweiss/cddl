#![cfg(feature = "ast-span")]
#![cfg(feature = "ast-comments")]
#![cfg(not(feature = "ast-parent"))]

use cddl::{ast::*, cddl_from_str};
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn test_issue_268_ast_behavior() -> std::result::Result<(), String> {
  let input = indoc!(
    r#"
        CapabilityRequest = {}
        CapabilitiesRequest = {
          firstMatch: [*CapabilityRequest]
        }
      "#
  );

  let cddl = cddl_from_str(input, false)?;

  // Get the CapabilitiesRequest rule
  let rule = &cddl.rules[1]; // CapabilitiesRequest
  if let Rule::Type { rule, .. } = rule {
    if let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 {
      if let GroupChoice { group_entries, .. } = &group.group_choices[0] {
        let (entry, _) = &group_entries[0]; // firstMatch entry (tuple of entry and optional comma)

        match entry {
          GroupEntry::ValueMemberKey { ge, .. } => {
            if let Some(member_key) = &ge.member_key {
              if let MemberKey::Bareword { ident, .. } = member_key {
                assert_eq!(ident.ident, "firstMatch");

                // Check the entry_type (the array)
                if let Type { type_choices, .. } = &ge.entry_type {
                  if let Type1 { type2, .. } = &type_choices[0].type1 {
                    if let Type2::Array { group, .. } = type2 {
                      if let GroupChoice { group_entries, .. } = &group.group_choices[0] {
                        let (array_entry, _) = &group_entries[0];

                        // This should be ValueMemberKey with no member_key (correct behavior)
                        match array_entry {
                          GroupEntry::ValueMemberKey { ge, .. } => {
                            assert!(ge.member_key.is_none());
                            println!("PASS: Array entry is ValueMemberKey (correct behavior)");
                          }
                          GroupEntry::TypeGroupname { ge, .. } => {
                            assert_eq!(ge.name.ident, "CapabilityRequest");
                            println!("FAIL: Array entry is TypeGroupname (incorrect for arrays)");
                            panic!("Array entries should use ValueMemberKey, not TypeGroupname");
                          }
                          _ => {
                            panic!("Unexpected group entry type");
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          _ => {
            panic!("Expected ValueMemberKey for firstMatch entry");
          }
        }
      }
    }
  }

  Ok(())
}

#[test]
#[allow(unused_variables)]
fn verify_cddl() -> std::result::Result<(), String> {
  let input = indoc!(
    r#"
        myrule = secondrule
        myrange = 10..upper
        upper = 500 / 600
        gr = 2* ( test )
        messages = message<"reboot", "now">
        message<t, v> = {type: 2, value: v}
        color = &colors
        colors = ( red: "red" )
        test = ( int / float )
      "#
  );

  let cddl = cddl_from_str(input, false)?;

  // Verify the number of rules
  assert_eq!(cddl.rules.len(), 9);

  // Verify rule names
  let rule_names: Vec<_> = cddl.rules.iter().map(|r| r.name()).collect();
  assert_eq!(rule_names[0], "myrule");
  assert_eq!(rule_names[1], "myrange");
  assert_eq!(rule_names[2], "upper");
  assert_eq!(rule_names[3], "gr");
  assert_eq!(rule_names[4], "messages");
  assert_eq!(rule_names[5], "message");
  assert_eq!(rule_names[6], "color");
  assert_eq!(rule_names[7], "colors");
  assert_eq!(rule_names[8], "test");

  // Verify first rule: myrule = secondrule
  if let Rule::Type { rule, .. } = &cddl.rules[0] {
    assert_eq!(rule.name.ident, "myrule");
    assert!(!rule.is_type_choice_alternate);
    assert!(rule.generic_params.is_none());
    if let Type2::Typename { ident, .. } = &rule.value.type_choices[0].type1.type2 {
      assert_eq!(ident.ident, "secondrule");
    } else {
      panic!("Expected typename for myrule");
    }
  } else {
    panic!("Expected type rule for myrule");
  }

  // Verify second rule: myrange = 10..upper
  if let Rule::Type { rule, .. } = &cddl.rules[1] {
    assert_eq!(rule.name.ident, "myrange");
    let t1 = &rule.value.type_choices[0].type1;
    if let Type2::UintValue { value, .. } = &t1.type2 {
      assert_eq!(*value, 10);
    }
    let op = t1.operator.as_ref().unwrap();
    if let RangeCtlOp::RangeOp { is_inclusive, .. } = &op.operator {
      assert!(*is_inclusive);
    }
    if let Type2::Typename { ident, .. } = &op.type2 {
      assert_eq!(ident.ident, "upper");
    }
  }

  // Verify third rule: upper = 500 / 600
  if let Rule::Type { rule, .. } = &cddl.rules[2] {
    assert_eq!(rule.name.ident, "upper");
    assert_eq!(rule.value.type_choices.len(), 2);
    if let Type2::UintValue { value, .. } = &rule.value.type_choices[0].type1.type2 {
      assert_eq!(*value, 500);
    }
    if let Type2::UintValue { value, .. } = &rule.value.type_choices[1].type1.type2 {
      assert_eq!(*value, 600);
    }
  }

  // Verify fifth rule: messages = message<"reboot", "now">
  if let Rule::Type { rule, .. } = &cddl.rules[4] {
    assert_eq!(rule.name.ident, "messages");
    if let Type2::Typename {
      ident,
      generic_args: Some(ga),
      ..
    } = &rule.value.type_choices[0].type1.type2
    {
      assert_eq!(ident.ident, "message");
      assert_eq!(ga.args.len(), 2);
      if let Type2::TextValue { value, .. } = &ga.args[0].arg.type2 {
        assert_eq!(value.as_ref(), "reboot");
      }
      if let Type2::TextValue { value, .. } = &ga.args[1].arg.type2 {
        assert_eq!(value.as_ref(), "now");
      }
    }
  }

  // Verify sixth rule: message<t, v> = {type: 2, value: v}
  if let Rule::Type { rule, .. } = &cddl.rules[5] {
    assert_eq!(rule.name.ident, "message");
    let gps = rule.generic_params.as_ref().unwrap();
    assert_eq!(gps.params.len(), 2);
    assert_eq!(gps.params[0].param.ident, "t");
    assert_eq!(gps.params[1].param.ident, "v");

    if let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 {
      assert_eq!(group.group_choices[0].group_entries.len(), 2);

      // Check "type: 2" entry
      if let GroupEntry::ValueMemberKey { ge, .. } = &group.group_choices[0].group_entries[0].0 {
        if let Some(MemberKey::Bareword { ident, .. }) = &ge.member_key {
          assert_eq!(ident.ident, "type");
        }
        if let Type2::UintValue { value, .. } = &ge.entry_type.type_choices[0].type1.type2 {
          assert_eq!(*value, 2);
        }
      }

      // Check "value: v" entry
      if let GroupEntry::ValueMemberKey { ge, .. } = &group.group_choices[0].group_entries[1].0 {
        if let Some(MemberKey::Bareword { ident, .. }) = &ge.member_key {
          assert_eq!(ident.ident, "value");
        }
        if let Type2::Typename { ident, .. } = &ge.entry_type.type_choices[0].type1.type2 {
          assert_eq!(ident.ident, "v");
        }
      }
    }
  }

  // Verify seventh rule: color = &colors
  if let Rule::Type { rule, .. } = &cddl.rules[6] {
    assert_eq!(rule.name.ident, "color");
    assert!(matches!(
      rule.value.type_choices[0].type1.type2,
      Type2::ChoiceFromGroup { .. }
    ));
  }

  // Verify ninth rule: test = ( int / float )
  if let Rule::Type { rule, .. } = &cddl.rules[8] {
    assert_eq!(rule.name.ident, "test");
    if let Type2::ParenthesizedType { pt, .. } = &rule.value.type_choices[0].type1.type2 {
      assert_eq!(pt.type_choices.len(), 2);
      if let Type2::Typename { ident, .. } = &pt.type_choices[0].type1.type2 {
        assert_eq!(ident.ident, "int");
      }
      if let Type2::Typename { ident, .. } = &pt.type_choices[1].type1.type2 {
        assert_eq!(ident.ident, "float");
      }
    }
  }

  // Verify the formatted output can be re-parsed
  let formatted = cddl.to_string();
  let reparsed = cddl_from_str(&formatted, false);
  assert!(
    reparsed.is_ok(),
    "Failed to reparse formatted output: {:?}\nFormatted: {}",
    reparsed.err(),
    formatted
  );

  Ok(())
}

#[test]
#[cfg(not_target_arch = "wasm32")]
fn cri_reference() -> std::result::Result<(), String> {
  let cddl = indoc!(
    r#"
      CRI-Reference = [
        (?scheme, ?((host.name // host.ip), ?port) // path.type),
        *path,
        *query,
        ?fragment
      ]

      scheme    = (0, text .regexp "[a-z][a-z0-9+.-]*")
      host.name = (1, text)
      host.ip   = (2, bytes .size 4 / bytes .size 16)
      port      = (3, 0..65535)
      path.type = (4, 0..127)
      path      = (5, text)
      query     = (6, text)
      fragment  = (7, text)
    "#
  );

  let c_ast = cddl_from_str(cddl, true)?;

  println!("{}", c_ast);

  Ok(())
}
