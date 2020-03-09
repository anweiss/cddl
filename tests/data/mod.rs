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
          params: vec![
            Identifier {
              ident: "t".into(),
              socket: None,
              range: (18, 19),
            },
            Identifier {
              ident: "v".into(),
              socket: None,
              range: (21, 22),
            },
          ],
          range: (17, 24),
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
        range: (0, 66),
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
                range: (93, 104),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "text".into(),
                    socket: None,
                    range: (106, 110),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (68, 112),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "reputon-list".into(),
          socket: None,
          range: (114, 126),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Bareword(Identifier {
                ident: "reputons".into(),
                socket: None,
                range: (133, 141),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "reputon-array".into(),
                    socket: None,
                    range: (143, 156),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (114, 158),
      })),
      Rule::Type(TypeRule {
        name: Identifier {
          ident: "reputon-array".into(),
          socket: None,
          range: (160, 173),
        },
        generic_param: None,
        is_type_choice_alternate: false,
        value: Type(vec![Type1 {
          type2: Type2::Array(Group(vec![GroupChoice(vec![(
            GroupEntry::TypeGroupname(TypeGroupnameEntry {
              occur: Some(Occur::ZeroOrMore),
              name: Identifier {
                ident: "reputon".into(),
                socket: None,
                range: (179, 186),
              },
              generic_arg: None,
            }),
            false,
          )])])),
          operator: None,
        }]),
        range: (160, 187),
      }),
      Rule::Type(TypeRule {
        name: Identifier {
          ident: "reputon".into(),
          socket: None,
          range: (189, 196),
        },
        generic_param: None,
        is_type_choice_alternate: false,
        value: Type(vec![Type1 {
          type2: Type2::Map(Group(vec![GroupChoice(vec![
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: None,
                name: Identifier {
                  ident: "rater-value".into(),
                  socket: None,
                  range: (203, 214),
                },
                generic_arg: None,
              }),
              true,
            ),
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: None,
                name: Identifier {
                  ident: "assertion-value".into(),
                  socket: None,
                  range: (218, 233),
                },
                generic_arg: None,
              }),
              true,
            ),
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: None,
                name: Identifier {
                  ident: "rated-value".into(),
                  socket: None,
                  range: (237, 248),
                },
                generic_arg: None,
              }),
              true,
            ),
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: None,
                name: Identifier {
                  ident: "rating-value".into(),
                  socket: None,
                  range: (252, 264),
                },
                generic_arg: None,
              }),
              true,
            ),
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: Some(Occur::Optional),
                name: Identifier {
                  ident: "conf-value".into(),
                  socket: None,
                  range: (270, 280),
                },
                generic_arg: None,
              }),
              true,
            ),
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: Some(Occur::Optional),
                name: Identifier {
                  ident: "normal-value".into(),
                  socket: None,
                  range: (286, 298),
                },
                generic_arg: None,
              }),
              true,
            ),
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: Some(Occur::Optional),
                name: Identifier {
                  ident: "sample-value".into(),
                  socket: None,
                  range: (304, 316),
                },
                generic_arg: None,
              }),
              true,
            ),
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: Some(Occur::Optional),
                name: Identifier {
                  ident: "gen-value".into(),
                  socket: None,
                  range: (322, 331),
                },
                generic_arg: None,
              }),
              true,
            ),
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: Some(Occur::Optional),
                name: Identifier {
                  ident: "expire-value".into(),
                  socket: None,
                  range: (337, 349),
                },
                generic_arg: None,
              }),
              true,
            ),
            (
              GroupEntry::TypeGroupname(TypeGroupnameEntry {
                occur: Some(Occur::ZeroOrMore),
                name: Identifier {
                  ident: "ext-value".into(),
                  socket: None,
                  range: (355, 364),
                },
                generic_arg: None,
              }),
              true,
            ),
          ])])),
          operator: None,
        }]),
        range: (189, 367),
      }),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "rater-value".into(),
          socket: None,
          range: (369, 380),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Bareword(Identifier {
                ident: "rater".into(),
                socket: None,
                range: (385, 390),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "text".into(),
                    socket: None,
                    range: (392, 396),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (369, 398),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "assertion-value".into(),
          socket: None,
          range: (399, 414),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Bareword(Identifier {
                ident: "assertion".into(),
                socket: None,
                range: (419, 428),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "text".into(),
                    socket: None,
                    range: (430, 434),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (399, 436),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "rated-value".into(),
          socket: None,
          range: (437, 448),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Bareword(Identifier {
                ident: "rated".into(),
                socket: None,
                range: (453, 458),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "text".into(),
                    socket: None,
                    range: (460, 464),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (437, 466),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "rating-value".into(),
          socket: None,
          range: (467, 479),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Bareword(Identifier {
                ident: "rating".into(),
                socket: None,
                range: (484, 490),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "float16".into(),
                    socket: None,
                    range: (492, 499),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (467, 501),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "conf-value".into(),
          socket: None,
          range: (502, 512),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Bareword(Identifier {
                ident: "confidence".into(),
                socket: None,
                range: (517, 527),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "float16".into(),
                    socket: None,
                    range: (529, 536),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (502, 538),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "normal-value".into(),
          socket: None,
          range: (539, 551),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Bareword(Identifier {
                ident: "normal-rating".into(),
                socket: None,
                range: (556, 569),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "float16".into(),
                    socket: None,
                    range: (571, 578),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (539, 580),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "sample-value".into(),
          socket: None,
          range: (581, 593),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Bareword(Identifier {
                ident: "sample-size".into(),
                socket: None,
                range: (598, 609),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "uint".into(),
                    socket: None,
                    range: (611, 615),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (581, 617),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "gen-value".into(),
          socket: None,
          range: (618, 627),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Bareword(Identifier {
                ident: "generated".into(),
                socket: None,
                range: (632, 641),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "uint".into(),
                    socket: None,
                    range: (643, 647),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (618, 649),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "expire-value".into(),
          socket: None,
          range: (650, 662),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Bareword(Identifier {
                ident: "expires".into(),
                socket: None,
                range: (667, 674),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "uint".into(),
                    socket: None,
                    range: (676, 680),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (650, 682),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "ext-value".into(),
          socket: None,
          range: (683, 692),
        },
        generic_param: None,
        is_group_choice_alternate: false,
        entry: GroupEntry::InlineGroup((
          None,
          Group(vec![GroupChoice(vec![(
            GroupEntry::ValueMemberKey(Box::from(ValueMemberKeyEntry {
              occur: None,
              member_key: Some(MemberKey::Type1(Box::from((
                Type1 {
                  type2: Type2::Typename((
                    Identifier {
                      ident: "text".into(),
                      socket: None,
                      range: (697, 701),
                    },
                    None,
                  )),
                  operator: None,
                },
                false,
              )))),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "any".into(),
                    socket: None,
                    range: (705, 708),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        range: (683, 710),
      })),
    ],
  }
}
