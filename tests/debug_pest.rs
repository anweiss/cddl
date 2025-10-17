#[test]
fn test_member_key_ident() {
    use cddl::cddl_from_str;
    
    let input = r#"thing = {* minor: bool}"#;
    let cddl = cddl_from_str(input, false).unwrap();
    
    if let cddl::ast::Rule::Type { rule, .. } = &cddl.rules[0] {
        if let cddl::ast::Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 {
            if let cddl::ast::GroupEntry::ValueMemberKey { ge, .. } = &group.group_choices[0].group_entries[0].0 {
                if let Some(cddl::ast::MemberKey::Bareword { ident, .. }) = &ge.member_key {
                    eprintln!("Member key ident: '{}'", ident.ident);
                    assert_eq!(ident.ident, "minor", "Expected 'minor' but got '{}'", ident.ident);
                }
            }
        }
    }
}
