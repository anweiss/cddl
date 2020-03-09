extern crate cddl;

use cddl::ast::*;

pub fn reputon() -> CDDL {
  CDDL {
    rules: vec![
      Rule::Type(TypeRule {
        name: Identifier {
          ident: "reputation-object".into(),
          socket: None,
          range: (0, 17),
        },
        generic_param: Some(GenericParm {
          params: vec![Identifier::from("t"), Identifier::from("v")],
          range: (17, 23),
        }),
        is_type_choice_alternate: false,
        value: Type(vec![Type1 {
          type2: Type2::Map(Group(vec![GroupChoice(vec![
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: None,
                name: Identifier {
                  ident: "reputation-context".into(),
                  socket: None,
                  range: (30, 48),
                },
                generic_arg: None,
              }),
              true,
            ),
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: None,
                name: Identifier {
                  ident: "reputon-list".into(),
                  socket: None,
                  range: (52, 64),
                },
                generic_arg: None,
              }),
              false,
            ),
          ])])),
          operator: None,
        }]),
        range: (0, 65),
      }),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "reputation-context".into(),
          socket: None,
          range: (68, 86),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Bareword(Identifier {
                ident: "application".into(),
                socket: None,
                range: (94, 105),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "text".into(),
                    socket: None,
                    range: (107, 111),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (68, 111),
      })),
      // Rule::Group(Box::from(GroupRule {
      //   name: Identifier(("reputon-list".into(), None)),
      //   generic_param: None,
      //   is_group_choice_alternate: false,
      //   entry: GroupEntry::InlineGroup((
      //     None,
      //     Group(vec![GroupChoice(vec![(
      //       GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      //         occur: None,
      //         member_key: Some(MemberKey::Bareword(Identifier(("reputons".into(), None)))),
      //         entry_type: Type(vec![Type1 {
      //           type2: Type2::Typename((Identifier(("reputon-array".into(), None)), None)),
      //           operator: None,
      //         }]),
      //       })),
      //       false,
      //     )])]),
      //   )),
      //   range: (108, 124),
      // })),
      // Rule::Type(TypeRule {
      //   name: Identifier(("reputon-array".into(), None)),
      //   generic_param: None,
      //   is_type_choice_alternate: false,
      //   value: Type(vec![Type1 {
      //     type2: Type2::Array(Group(vec![GroupChoice(vec![(
      //       GroupEntry::TypeGroupname(TypeGroupnameEntry {
      //         occur: Some(Occur::ZeroOrMore),
      //         name: Identifier(("reputon".into(), None)),
      //         generic_arg: None,
      //       }),
      //       false,
      //     )])])),
      //     operator: None,
      //   }]),
      //   range: (154, 171),
      // }),
      // Rule::Type(TypeRule {
      //   name: Identifier(("reputon".into(), None)),
      //   generic_param: None,
      //   is_type_choice_alternate: false,
      //   value: Type(vec![Type1 {
      //     type2: Type2::Map(Group(vec![GroupChoice(vec![
      //       (
      //         GroupEntry::TypeGroupname(TypeGroupnameEntry {
      //           occur: None,
      //           name: Identifier(("rater-value".into(), None)),
      //           generic_arg: None,
      //         }),
      //         true,
      //       ),
      //       (
      //         GroupEntry::TypeGroupname(TypeGroupnameEntry {
      //           occur: None,
      //           name: Identifier(("assertion-value".into(), None)),
      //           generic_arg: None,
      //         }),
      //         true,
      //       ),
      //       (
      //         GroupEntry::TypeGroupname(TypeGroupnameEntry {
      //           occur: None,
      //           name: Identifier(("rated-value".into(), None)),
      //           generic_arg: None,
      //         }),
      //         true,
      //       ),
      //       (
      //         GroupEntry::TypeGroupname(TypeGroupnameEntry {
      //           occur: None,
      //           name: Identifier(("rating-value".into(), None)),
      //           generic_arg: None,
      //         }),
      //         true,
      //       ),
      //       (
      //         GroupEntry::TypeGroupname(TypeGroupnameEntry {
      //           occur: Some(Occur::Optional),
      //           name: Identifier(("conf-value".into(), None)),
      //           generic_arg: None,
      //         }),
      //         true,
      //       ),
      //       (
      //         GroupEntry::TypeGroupname(TypeGroupnameEntry {
      //           occur: Some(Occur::Optional),
      //           name: Identifier(("normal-value".into(), None)),
      //           generic_arg: None,
      //         }),
      //         true,
      //       ),
      //       (
      //         GroupEntry::TypeGroupname(TypeGroupnameEntry {
      //           occur: Some(Occur::Optional),
      //           name: Identifier(("sample-value".into(), None)),
      //           generic_arg: None,
      //         }),
      //         true,
      //       ),
      //       (
      //         GroupEntry::TypeGroupname(TypeGroupnameEntry {
      //           occur: Some(Occur::Optional),
      //           name: Identifier(("gen-value".into(), None)),
      //           generic_arg: None,
      //         }),
      //         true,
      //       ),
      //       (
      //         GroupEntry::TypeGroupname(TypeGroupnameEntry {
      //           occur: Some(Occur::Optional),
      //           name: Identifier(("expire-value".into(), None)),
      //           generic_arg: None,
      //         }),
      //         true,
      //       ),
      //       (
      //         GroupEntry::TypeGroupname(TypeGroupnameEntry {
      //           occur: Some(Occur::ZeroOrMore),
      //           name: Identifier(("ext-value".into(), None)),
      //           generic_arg: None,
      //         }),
      //         true,
      //       ),
      //     ])])),
      //     operator: None,
      //   }]),
      //   range: (183, 194),
      // }),
      // Rule::Group(Box::from(GroupRule {
      //   name: Identifier(("rater-value".into(), None)),
      //   generic_param: None,
      //   is_group_choice_alternate: false,
      //   entry: GroupEntry::InlineGroup((
      //     None,
      //     Group(vec![GroupChoice(vec![(
      //       GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      //         occur: None,
      //         member_key: Some(MemberKey::Bareword(Identifier(("rater".into(), None)))),
      //         entry_type: Type(vec![Type1 {
      //           type2: Type2::Typename((Identifier(("text".into(), None)), None)),
      //           operator: None,
      //         }]),
      //       })),
      //       false,
      //     )])]),
      //   )),
      //   range: (363, 378),
      // })),
      // Rule::Group(Box::from(GroupRule {
      //   name: Identifier(("assertion-value".into(), None)),
      //   generic_param: None,
      //   is_group_choice_alternate: false,
      //   entry: GroupEntry::InlineGroup((
      //     None,
      //     Group(vec![GroupChoice(vec![(
      //       GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      //         occur: None,
      //         member_key: Some(MemberKey::Bareword(Identifier(("assertion".into(), None)))),
      //         entry_type: Type(vec![Type1 {
      //           type2: Type2::Typename((Identifier(("text".into(), None)), None)),
      //           operator: None,
      //         }]),
      //       })),
      //       false,
      //     )])]),
      //   )),
      //   range: (393, 412),
      // })),
      // Rule::Group(Box::from(GroupRule {
      //   name: Identifier(("rated-value".into(), None)),
      //   generic_param: None,
      //   is_group_choice_alternate: false,
      //   entry: GroupEntry::InlineGroup((
      //     None,
      //     Group(vec![GroupChoice(vec![(
      //       GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      //         occur: None,
      //         member_key: Some(MemberKey::Bareword(Identifier(("rated".into(), None)))),
      //         entry_type: Type(vec![Type1 {
      //           type2: Type2::Typename((Identifier(("text".into(), None)), None)),
      //           operator: None,
      //         }]),
      //       })),
      //       false,
      //     )])]),
      //   )),
      //   range: (431, 446),
      // })),
      // Rule::Group(Box::from(GroupRule {
      //   name: Identifier(("rating-value".into(), None)),
      //   generic_param: None,
      //   is_group_choice_alternate: false,
      //   entry: GroupEntry::InlineGroup((
      //     None,
      //     Group(vec![GroupChoice(vec![(
      //       GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      //         occur: None,
      //         member_key: Some(MemberKey::Bareword(Identifier(("rating".into(), None)))),
      //         entry_type: Type(vec![Type1 {
      //           type2: Type2::Typename((Identifier(("float16".into(), None)), None)),
      //           operator: None,
      //         }]),
      //       })),
      //       false,
      //     )])]),
      //   )),
      //   range: (461, 477),
      // })),
      // Rule::Group(Box::from(GroupRule {
      //   name: Identifier(("conf-value".into(), None)),
      //   generic_param: None,
      //   is_group_choice_alternate: false,
      //   entry: GroupEntry::InlineGroup((
      //     None,
      //     Group(vec![GroupChoice(vec![(
      //       GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      //         occur: None,
      //         member_key: Some(MemberKey::Bareword(Identifier(("confidence".into(), None)))),
      //         entry_type: Type(vec![Type1 {
      //           type2: Type2::Typename((Identifier(("float16".into(), None)), None)),
      //           operator: None,
      //         }]),
      //       })),
      //       false,
      //     )])]),
      //   )),
      //   range: (496, 510),
      // })),
      // Rule::Group(Box::from(GroupRule {
      //   name: Identifier(("normal-value".into(), None)),
      //   generic_param: None,
      //   is_group_choice_alternate: false,
      //   entry: GroupEntry::InlineGroup((
      //     None,
      //     Group(vec![GroupChoice(vec![(
      //       GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      //         occur: None,
      //         member_key: Some(MemberKey::Bareword(Identifier((
      //           "normal-rating".into(),
      //           None,
      //         )))),
      //         entry_type: Type(vec![Type1 {
      //           type2: Type2::Typename((Identifier(("float16".into(), None)), None)),
      //           operator: None,
      //         }]),
      //       })),
      //       false,
      //     )])]),
      //   )),
      //   range: (533, 549),
      // })),
      // Rule::Group(Box::from(GroupRule {
      //   name: Identifier(("sample-value".into(), None)),
      //   generic_param: None,
      //   is_group_choice_alternate: false,
      //   entry: GroupEntry::InlineGroup((
      //     None,
      //     Group(vec![GroupChoice(vec![(
      //       GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      //         occur: None,
      //         member_key: Some(MemberKey::Bareword(Identifier((
      //           "sample-size".into(),
      //           None,
      //         )))),
      //         entry_type: Type(vec![Type1 {
      //           type2: Type2::Typename((Identifier(("uint".into(), None)), None)),
      //           operator: None,
      //         }]),
      //       })),
      //       false,
      //     )])]),
      //   )),
      //   range: (575, 591),
      // })),
      // Rule::Group(Box::from(GroupRule {
      //   name: Identifier(("gen-value".into(), None)),
      //   generic_param: None,
      //   is_group_choice_alternate: false,
      //   entry: GroupEntry::InlineGroup((
      //     None,
      //     Group(vec![GroupChoice(vec![(
      //       GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      //         occur: None,
      //         member_key: Some(MemberKey::Bareword(Identifier(("generated".into(), None)))),
      //         entry_type: Type(vec![Type1 {
      //           type2: Type2::Typename((Identifier(("uint".into(), None)), None)),
      //           operator: None,
      //         }]),
      //       })),
      //       false,
      //     )])]),
      //   )),
      //   range: (612, 625),
      // })),
      // Rule::Group(Box::from(GroupRule {
      //   name: Identifier(("expire-value".into(), None)),
      //   generic_param: None,
      //   is_group_choice_alternate: false,
      //   entry: GroupEntry::InlineGroup((
      //     None,
      //     Group(vec![GroupChoice(vec![(
      //       GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      //         occur: None,
      //         member_key: Some(MemberKey::Bareword(Identifier(("expires".into(), None)))),
      //         entry_type: Type(vec![Type1 {
      //           type2: Type2::Typename((Identifier(("uint".into(), None)), None)),
      //           operator: None,
      //         }]),
      //       })),
      //       false,
      //     )])]),
      //   )),
      //   range: (644, 660),
      // })),
      // Rule::Group(Box::from(GroupRule {
      //   name: Identifier(("ext-value".into(), None)),
      //   generic_param: None,
      //   is_group_choice_alternate: false,
      //   entry: GroupEntry::InlineGroup((
      //     None,
      //     Group(vec![GroupChoice(vec![(
      //       GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
      //         occur: None,
      //         member_key: Some(MemberKey::Type1(Box::from((
      //           Type1 {
      //             type2: Type2::Typename((Identifier(("text".into(), None)), None)),
      //             operator: None,
      //           },
      //           false,
      //         )))),
      //         entry_type: Type(vec![Type1 {
      //           type2: Type2::Typename((Identifier(("any".into(), None)), None)),
      //           operator: None,
      //         }]),
      //       })),
      //       false,
      //     )])]),
      //   )),
      //   range: (677, 690),
      // })),
    ],
  }
}
