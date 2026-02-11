#[cfg(test)]
#[allow(unused_imports)]
mod tests {
  use crate::{ast::*, parser::cddl_from_str, pest_bridge::cddl_from_pest_str, token};

  use pretty_assertions::assert_eq;

  fn parse_ok(input: &str) -> CDDL<'_> {
    cddl_from_str(input, false).unwrap_or_else(|e| panic!("Failed to parse CDDL: {}", e))
  }

  fn parse_err(input: &str) -> String {
    cddl_from_str(input, false).unwrap_err()
  }

  #[test]
  fn verify_simple_type_rule() {
    let input = "a = 1234\n";
    let cddl = parse_ok(input);
    assert_eq!(cddl.rules.len(), 1);
    assert_eq!(cddl.rules[0].name(), "a");
  }

  #[test]
  fn verify_duplicate_rule_error() {
    let input = "a = 1234\na = b\n";
    let err = parse_err(input);
    assert!(
      err.contains("already defined") || err.contains("error"),
      "Expected duplicate rule error, got: {}",
      err
    );
  }

  #[test]
  fn verify_genericparams() {
    let input = "myrule<t, v> = t / v\n";
    let cddl = parse_ok(input);
    assert_eq!(cddl.rules.len(), 1);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      let gps = rule.generic_params.as_ref().unwrap();
      assert_eq!(gps.params.len(), 2);
      assert_eq!(gps.params[0].param.ident, "t");
      assert_eq!(gps.params[1].param.ident, "v");
    } else {
      panic!("Expected type rule");
    }
  }

  #[test]
  fn verify_genericargs() {
    let input = r#"myrule = message<"reboot", "now">
message<a, b> = { action: a, time: b }
"#;
    let cddl = parse_ok(input);
    assert_eq!(cddl.rules.len(), 2);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      let tc = &rule.value.type_choices[0];
      if let Type2::Typename {
        ident,
        generic_args: Some(ga),
        ..
      } = &tc.type1.type2
      {
        assert_eq!(ident.ident, "message");
        assert_eq!(ga.args.len(), 2);
      } else {
        panic!("Expected typename with generic args");
      }
    } else {
      panic!("Expected type rule");
    }
  }

  #[test]
  fn verify_type_choice() {
    let input = "myrule = ( tchoice1 / tchoice2 )\n";
    let cddl = parse_ok(input);
    assert_eq!(cddl.rules.len(), 1);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::ParenthesizedType { pt, .. } = &rule.value.type_choices[0].type1.type2 {
        assert_eq!(pt.type_choices.len(), 2);
      } else {
        panic!("Expected parenthesized type");
      }
    }
  }

  #[test]
  fn verify_type1_ranges() {
    let input = "r1 = 5..10\nr2 = -10.5...10.1\nr3 = 1.5..4.5\n";
    let cddl = parse_ok(input);
    assert_eq!(cddl.rules.len(), 3);

    // r1 = 5..10
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      let t1 = &rule.value.type_choices[0].type1;
      assert!(t1.operator.is_some());
      let op = t1.operator.as_ref().unwrap();
      if let RangeCtlOp::RangeOp { is_inclusive, .. } = &op.operator {
        assert!(*is_inclusive);
      } else {
        panic!("Expected range op");
      }
    }

    // r2 = -10.5...10.1
    if let Rule::Type { rule, .. } = &cddl.rules[1] {
      let t1 = &rule.value.type_choices[0].type1;
      assert!(t1.operator.is_some());
      let op = t1.operator.as_ref().unwrap();
      if let RangeCtlOp::RangeOp { is_inclusive, .. } = &op.operator {
        assert!(!*is_inclusive);
      } else {
        panic!("Expected range op");
      }
    }
  }

  #[test]
  fn verify_type1_control() {
    let input = "myrule = target .lt controller\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      let t1 = &rule.value.type_choices[0].type1;
      if let Type2::Typename { ident, .. } = &t1.type2 {
        assert_eq!(ident.ident, "target");
      }
      let op = t1.operator.as_ref().unwrap();
      if let RangeCtlOp::CtlOp { ctrl, .. } = &op.operator {
        assert_eq!(*ctrl, token::ControlOperator::LT);
      } else {
        panic!("Expected control op");
      }
    }
  }

  #[test]
  fn verify_type2_text_value() {
    let input = r#"myrule = "myvalue"
"#;
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::TextValue { value, .. } = &rule.value.type_choices[0].type1.type2 {
        assert_eq!(value.as_ref(), "myvalue");
      } else {
        panic!("Expected text value");
      }
    }
  }

  #[test]
  fn verify_type2_typename_with_generics() {
    let input = r#"myrule = message<"reboot", "now">
message<a, b> = a / b
"#;
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::Typename {
        ident,
        generic_args,
        ..
      } = &rule.value.type_choices[0].type1.type2
      {
        assert_eq!(ident.ident, "message");
        assert!(generic_args.is_some());
        let ga = generic_args.as_ref().unwrap();
        assert_eq!(ga.args.len(), 2);
      } else {
        panic!("Expected typename");
      }
    }
  }

  #[test]
  fn verify_type2_socket_plug() {
    let input = "myrule = $$tcp-option\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::Typename { ident, .. } = &rule.value.type_choices[0].type1.type2 {
        assert_eq!(ident.ident, "tcp-option");
        assert_eq!(ident.socket, Some(token::SocketPlug::GROUP));
      } else {
        panic!("Expected typename");
      }
    }
  }

  #[test]
  fn verify_type2_unwrap() {
    let input = "myrule = ~group1\ngroup1 = (int)\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::Unwrap { ident, .. } = &rule.value.type_choices[0].type1.type2 {
        assert_eq!(ident.ident, "group1");
      } else {
        panic!(
          "Expected unwrap, got {:?}",
          rule.value.type_choices[0].type1.type2
        );
      }
    }
  }

  #[test]
  fn verify_type2_tagged_data() {
    let input = "myrule = #6.997(tstr)\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::TaggedData { tag, t, .. } = &rule.value.type_choices[0].type1.type2 {
        assert_eq!(*tag, Some(token::TagConstraint::Literal(997)));
        assert_eq!(t.type_choices.len(), 1);
      } else {
        panic!(
          "Expected tagged data, got {:?}",
          rule.value.type_choices[0].type1.type2
        );
      }
    }
  }

  #[test]
  fn verify_type2_float() {
    let input = "myrule = 9.9\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::FloatValue { value, .. } = &rule.value.type_choices[0].type1.type2 {
        assert!((*value - 9.9).abs() < f64::EPSILON);
      } else {
        panic!("Expected float value");
      }
    }
  }

  #[test]
  fn verify_type2_any() {
    let input = "myrule = #\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      assert!(matches!(
        rule.value.type_choices[0].type1.type2,
        Type2::Any { .. }
      ));
    }
  }

  #[test]
  fn verify_type2_array_with_occurrence() {
    let input = "myrule = [*3 reputon]\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::Array { group, .. } = &rule.value.type_choices[0].type1.type2 {
        assert_eq!(group.group_choices.len(), 1);
        let ge = &group.group_choices[0].group_entries[0].0;
        if let GroupEntry::TypeGroupname { ge, .. } = ge {
          assert_eq!(ge.name.ident, "reputon");
          assert!(ge.occur.is_some());
        } else {
          panic!("Expected type groupname entry");
        }
      } else {
        panic!("Expected array");
      }
    }
  }

  #[test]
  fn verify_type2_choice_from_group() {
    let input = "myrule = &groupname\ngroupname = (int)\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::ChoiceFromGroup { ident, .. } = &rule.value.type_choices[0].type1.type2 {
        assert_eq!(ident.ident, "groupname");
      } else {
        panic!(
          "Expected choice from group, got {:?}",
          rule.value.type_choices[0].type1.type2
        );
      }
    }
  }

  #[test]
  fn verify_type2_choice_from_inline_group() {
    let input = "myrule = &( inlinegroup )\ninlinegroup = (int)\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      assert!(matches!(
        rule.value.type_choices[0].type1.type2,
        Type2::ChoiceFromInlineGroup { .. }
      ));
    }
  }

  #[test]
  fn verify_type2_map_with_cut() {
    let input = r#"myrule = { ? "optional-key" ^ => int }
"#;
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 {
        let ge = &group.group_choices[0].group_entries[0].0;
        if let GroupEntry::ValueMemberKey { ge, .. } = ge {
          assert!(ge.occur.is_some());
          if let Some(MemberKey::Type1 { is_cut, .. }) = &ge.member_key {
            assert!(*is_cut);
          } else {
            panic!("Expected type1 member key");
          }
        } else {
          panic!("Expected value member key entry");
        }
      } else {
        panic!("Expected map");
      }
    }
  }

  #[test]
  fn verify_grpent_bareword_key() {
    let input = "myrule = { type1: type2 }\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 {
        let ge = &group.group_choices[0].group_entries[0].0;
        if let GroupEntry::ValueMemberKey { ge, .. } = ge {
          if let Some(MemberKey::Bareword { ident, .. }) = &ge.member_key {
            assert_eq!(ident.ident, "type1");
          } else {
            panic!("Expected bareword member key");
          }
        }
      }
    }
  }

  #[test]
  fn verify_grpent_value_key() {
    let input = "myrule = { ? 0: addrdistr }\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 {
        let ge = &group.group_choices[0].group_entries[0].0;
        if let GroupEntry::ValueMemberKey { ge, .. } = ge {
          assert!(ge.occur.is_some());
          if let Some(MemberKey::Value { value, .. }) = &ge.member_key {
            assert_eq!(*value, token::Value::UINT(0));
          } else {
            panic!("Expected value member key");
          }
        }
      }
    }
  }

  #[test]
  fn verify_occurrence_indicators() {
    let input = "r1 = [1*3 int]\nr2 = [* int]\nr3 = [+ int]\nr4 = [? int]\n";
    let cddl = parse_ok(input);
    assert_eq!(cddl.rules.len(), 4);

    // Check occurrences
    for (idx, expected) in ["Exact(1,3)", "ZeroOrMore", "OneOrMore", "Optional"]
      .iter()
      .enumerate()
    {
      if let Rule::Type { rule, .. } = &cddl.rules[idx] {
        if let Type2::Array { group, .. } = &rule.value.type_choices[0].type1.type2 {
          let ge = &group.group_choices[0].group_entries[0].0;
          let occur = match ge {
            GroupEntry::TypeGroupname { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
            GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
            _ => None,
          };
          let occur = occur.unwrap();
          match (expected, occur) {
            (&"Exact(1,3)", Occur::Exact { lower, upper, .. }) => {
              assert_eq!(*lower, Some(1));
              assert_eq!(*upper, Some(3));
            }
            (&"ZeroOrMore", Occur::ZeroOrMore { .. }) => {}
            (&"OneOrMore", Occur::OneOrMore { .. }) => {}
            (&"Optional", Occur::Optional { .. }) => {}
            _ => panic!(
              "Occurrence mismatch for rule {}: expected {}",
              idx, expected
            ),
          }
        }
      }
    }
  }

  #[test]
  fn verify_group_choices() {
    let input = "myrule = { int, int // int, tstr }\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 {
        assert_eq!(group.group_choices.len(), 2);
        assert_eq!(group.group_choices[0].group_entries.len(), 2);
        assert_eq!(group.group_choices[1].group_entries.len(), 2);
      }
    }
  }

  #[test]
  fn verify_nested_arrays() {
    let input = "myrule = [ [* file-entry], [* directory-entry] ]\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::Array { group, .. } = &rule.value.type_choices[0].type1.type2 {
        assert_eq!(group.group_choices[0].group_entries.len(), 2);
      } else {
        panic!("Expected array");
      }
    }
  }

  #[test]
  fn verify_type_choice_alternates() {
    let input = "myrule = int\nmyrule /= tstr\n";
    let cddl = parse_ok(input);
    assert_eq!(cddl.rules.len(), 2);
    if let Rule::Type { rule, .. } = &cddl.rules[1] {
      assert!(rule.is_type_choice_alternate);
    }
  }

  #[test]
  fn verify_group_rule() {
    let input = "mygroup = ( a: int, b: tstr )\n";
    let cddl = parse_ok(input);
    assert_eq!(cddl.rules.len(), 1);
    match &cddl.rules[0] {
      Rule::Group { rule, .. } => {
        assert_eq!(rule.name.ident, "mygroup");
      }
      Rule::Type { rule, .. } => {
        // The pest parser may parse this as a type rule with parenthesized type
        assert_eq!(rule.name.ident, "mygroup");
      }
    }
  }

  #[test]
  fn verify_roundtrip_formatting() {
    let inputs = [
      "myrule = int\n",
      "myrule = { name: tstr, age: uint }\n",
      "myrule = [* int]\n",
      "myrule = 5..10\n",
    ];

    for input in &inputs {
      let cddl = parse_ok(input);
      let formatted = cddl.to_string();
      // Verify the formatted output can be re-parsed
      let reparsed = cddl_from_str(&formatted, false);
      assert!(
        reparsed.is_ok(),
        "Failed to reparse formatted output for '{}': {:?}\nFormatted: {}",
        input,
        reparsed.err(),
        formatted
      );
    }
  }

  #[test]
  fn verify_inline_group_in_array() {
    let input = "myrule = [ ( a: int, b: tstr ) ]\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::Array { group, .. } = &rule.value.type_choices[0].type1.type2 {
        assert!(!group.group_choices.is_empty());
      }
    }
  }

  #[test]
  fn verify_generic_rule_with_args() {
    let input = "map<K, V> = { * K => V }\nmy-map = map<text, int>\n";
    let cddl = parse_ok(input);
    assert_eq!(cddl.rules.len(), 2);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      assert!(rule.generic_params.is_some());
      let gps = rule.generic_params.as_ref().unwrap();
      assert_eq!(gps.params.len(), 2);
    }
  }

  #[test]
  fn verify_array_member_key_with_generics() {
    let input = "myrule = { 0: finite_set<transaction_input> }\n";
    let cddl = parse_ok(input);
    if let Rule::Type { rule, .. } = &cddl.rules[0] {
      if let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 {
        let ge = &group.group_choices[0].group_entries[0].0;
        if let GroupEntry::ValueMemberKey { ge, .. } = ge {
          if let Some(MemberKey::Value { value, .. }) = &ge.member_key {
            assert_eq!(*value, token::Value::UINT(0));
          }
          // entry_type should have generic args
          let t2 = &ge.entry_type.type_choices[0].type1.type2;
          if let Type2::Typename {
            ident,
            generic_args,
            ..
          } = t2
          {
            assert_eq!(ident.ident, "finite_set");
            assert!(generic_args.is_some());
          }
        }
      }
    }
  }

  #[test]
  fn verify_multiple_rules() {
    let input = r#"reputation-object = {
  application: tstr,
  reputons: [* reputon],
}

reputon = {
  rating: float16-32,
  ? confidence: float16-32,
  ? sample-size: uint,
}
"#;
    let cddl = parse_ok(input);
    assert_eq!(cddl.rules.len(), 2);
    assert_eq!(cddl.rules[0].name(), "reputation-object");
    assert_eq!(cddl.rules[1].name(), "reputon");
  }

  #[test]
  fn verify_control_operators() {
    use token::ControlOperator;

    let inputs: Vec<(&str, ControlOperator)> = vec![
      ("myrule = uint .size 4\n", ControlOperator::SIZE),
      (
        r#"myrule = tstr .regexp "[^@]+@[^@]+""#,
        ControlOperator::REGEXP,
      ),
      ("myrule = tstr .eq \"hello\"\n", ControlOperator::EQ),
    ];

    for (input, expected_ctrl) in &inputs {
      let cddl = parse_ok(input);
      if let Rule::Type { rule, .. } = &cddl.rules[0] {
        let op = rule.value.type_choices[0].type1.operator.as_ref().unwrap();
        if let RangeCtlOp::CtlOp { ctrl, .. } = &op.operator {
          assert_eq!(ctrl, expected_ctrl);
        } else {
          panic!("Expected control op for {}", input);
        }
      }
    }
  }
}
