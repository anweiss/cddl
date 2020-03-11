extern crate cddl;

use cddl::ast::*;

pub fn reputon() -> CDDL {
  CDDL {
    rules: vec![
      Rule::Type(TypeRule {
        name: Identifier {
          ident: "reputation-object".into(),
          socket: None,
          span: (0, 17, 1),
        },
        generic_param: Some(GenericParm {
          params: vec![
            Identifier {
              ident: "t".into(),
              socket: None,
              span: (18, 19, 1),
            },
            Identifier {
              ident: "v".into(),
              socket: None,
              span: (21, 22, 1),
            },
          ],
          span: (17, 24, 1),
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
                  span: (30, 48, 2),
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
                  span: (52, 64, 3),
                },
                generic_arg: None,
              }),
              false,
            ),
          ])])),
          operator: None,
        }]),
        span: (0, 66, 1),
      }),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "reputation-context".into(),
          socket: None,
          span: (68, 86, 6),
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
                span: (93, 104, 7),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "text".into(),
                    socket: None,
                    span: (106, 110, 7),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (68, 112, 6),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "reputon-list".into(),
          socket: None,
          span: (114, 126, 10),
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
                span: (133, 141, 11),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "reputon-array".into(),
                    socket: None,
                    span: (143, 156, 11),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (114, 158, 10),
      })),
      Rule::Type(TypeRule {
        name: Identifier {
          ident: "reputon-array".into(),
          socket: None,
          span: (160, 173, 14),
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
                span: (179, 186, 14),
              },
              generic_arg: None,
            }),
            false,
          )])])),
          operator: None,
        }]),
        span: (160, 187, 14),
      }),
      Rule::Type(TypeRule {
        name: Identifier {
          ident: "reputon".into(),
          socket: None,
          span: (189, 196, 16),
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
                  span: (203, 214, 17),
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
                  span: (218, 233, 18),
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
                  span: (237, 248, 19),
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
                  span: (252, 264, 20),
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
                  span: (270, 280, 21),
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
                  span: (286, 298, 22),
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
                  span: (304, 316, 23),
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
                  span: (322, 331, 24),
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
                  span: (337, 349, 25),
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
                  span: (355, 364, 26),
                },
                generic_arg: None,
              }),
              true,
            ),
          ])])),
          operator: None,
        }]),
        span: (189, 367, 16),
      }),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "rater-value".into(),
          socket: None,
          span: (369, 380, 29),
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
                span: (385, 390, 29),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "text".into(),
                    socket: None,
                    span: (392, 396, 29),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (369, 398, 29),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "assertion-value".into(),
          socket: None,
          span: (399, 414, 30),
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
                span: (419, 428, 30),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "text".into(),
                    socket: None,
                    span: (430, 434, 30),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (399, 436, 30),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "rated-value".into(),
          socket: None,
          span: (437, 448, 31),
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
                span: (453, 458, 31),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "text".into(),
                    socket: None,
                    span: (460, 464, 31),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (437, 466, 31),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "rating-value".into(),
          socket: None,
          span: (467, 479, 32),
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
                span: (484, 490, 32),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "float16".into(),
                    socket: None,
                    span: (492, 499, 32),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (467, 501, 32),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "conf-value".into(),
          socket: None,
          span: (502, 512, 33),
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
                span: (517, 527, 33),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "float16".into(),
                    socket: None,
                    span: (529, 536, 33),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (502, 538, 33),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "normal-value".into(),
          socket: None,
          span: (539, 551, 34),
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
                span: (556, 569, 34),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "float16".into(),
                    socket: None,
                    span: (571, 578, 34),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (539, 580, 34),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "sample-value".into(),
          socket: None,
          span: (581, 593, 35),
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
                span: (598, 609, 35),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "uint".into(),
                    socket: None,
                    span: (611, 615, 35),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (581, 617, 35),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "gen-value".into(),
          socket: None,
          span: (618, 627, 36),
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
                span: (632, 641, 36),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "uint".into(),
                    socket: None,
                    span: (643, 647, 36),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (618, 649, 36),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "expire-value".into(),
          socket: None,
          span: (650, 662, 37),
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
                span: (667, 674, 37),
              })),
              entry_type: Type(vec![Type1 {
                type2: Type2::Typename((
                  Identifier {
                    ident: "uint".into(),
                    socket: None,
                    span: (676, 680, 37),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (650, 682, 37),
      })),
      Rule::Group(Box::from(GroupRule {
        name: Identifier {
          ident: "ext-value".into(),
          socket: None,
          span: (683, 692, 38),
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
                      span: (697, 701, 38),
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
                    span: (705, 708, 38),
                  },
                  None,
                )),
                operator: None,
              }]),
            })),
            false,
          )])]),
        )),
        span: (683, 710, 38),
      })),
    ],
  }
}
