#[test]
fn test_member_key_type() {
    use cddl::{cddl_from_str, ast};
    
    let input = r#"thing = {* minor: bool}"#;
    let cddl = cddl_from_str(input, false).unwrap();
    
    if let ast::Rule::Type { rule, .. } = &cddl.rules[0] {
        if let ast::Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 {
            if let ast::GroupEntry::ValueMemberKey { ge, .. } = &group.group_choices[0].group_entries[0].0 {
                eprintln!("Member key: {:#?}", ge.member_key);
                match &ge.member_key {
                    Some(ast::MemberKey::Bareword { ident, .. }) => {
                        eprintln!("It's a Bareword with ident: '{}'", ident.ident);
                    }
                    Some(ast::MemberKey::Value { value, .. }) => {
                        eprintln!("It's a Value: {:#?}", value);
                    }
                    _ => {
                        eprintln!("It's something else");
                    }
                }
            }
        }
    }
}
