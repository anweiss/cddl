use cddl::{
  ast::*,
  lexer::Lexer,
  parser::{Error, Parser, Result},
};
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
#[allow(unused_variables)]
fn verify_cddl() -> Result<()> {
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
        thing = ( int / float )
      "#
  );

  match Parser::new(Lexer::new(input).iter(), input) {
    Ok(mut p) => match p.parse_cddl() {
      Ok(cddl) => {
        let expected_output = CDDL {
          rules: vec![
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "myrule".into(),
                  socket: None,
                  span: (0, 6, 1),
                },
                generic_params: None,
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::Typename {
                        ident: Identifier {
                          ident: "secondrule".into(),
                          socket: None,
                          span: (9, 19, 1),
                        },
                        generic_args: None,
                        span: (9, 19, 1),
                      },
                      operator: None,
                      comments_after_type: None,
                      span: (9, 19, 1),
                    },
                    comments_before_type: None,
                    comments_after_type: None,
                  }],

                  span: (9, 19, 1),
                },
                comments_before_assignt: None,
                comments_after_assignt: None,
              },
              comments_after_rule: None,
              span: (0, 19, 1),
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "myrange".into(),
                  socket: None,
                  span: (20, 27, 2),
                },
                generic_params: None,
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::UintValue {
                        value: 10,
                        span: (30, 32, 2),
                      },
                      operator: Some(Operator {
                        operator: RangeCtlOp::RangeOp {
                          is_inclusive: true,
                          span: (32, 34, 2),
                        },
                        type2: Type2::Typename {
                          ident: Identifier {
                            ident: "upper".into(),
                            socket: None,
                            span: (34, 39, 2),
                          },
                          generic_args: None,
                          span: (34, 39, 2),
                        },
                        comments_before_operator: None,
                        comments_after_operator: None,
                      }),
                      comments_after_type: None,
                      span: (30, 39, 2),
                    },
                    comments_before_type: None,
                    comments_after_type: None,
                  }],
                  span: (30, 39, 2),
                },
                comments_before_assignt: None,
                comments_after_assignt: None,
              },
              comments_after_rule: None,
              span: (20, 39, 2),
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "upper".into(),
                  socket: None,
                  span: (40, 45, 3),
                },
                generic_params: None,
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![
                    TypeChoice {
                      type1: Type1 {
                        type2: Type2::UintValue {
                          value: 500,
                          span: (48, 51, 3),
                        },
                        operator: None,
                        comments_after_type: None,
                        span: (48, 51, 3),
                      },
                      comments_before_type: None,
                      comments_after_type: None,
                    },
                    TypeChoice {
                      type1: Type1 {
                        type2: Type2::UintValue {
                          value: 600,
                          span: (54, 57, 3),
                        },
                        operator: None,
                        comments_after_type: None,
                        span: (54, 57, 3),
                      },
                      comments_before_type: None,
                      comments_after_type: None,
                    },
                  ],
                  span: (48, 57, 3),
                },
                comments_before_assignt: None,
                comments_after_assignt: None,
              },
              comments_after_rule: None,
              span: (40, 57, 3),
            },
            Rule::Group {
              rule: Box::from(GroupRule {
                name: Identifier {
                  ident: "gr".into(),
                  socket: None,
                  span: (58, 60, 4),
                },
                generic_params: None,
                is_group_choice_alternate: false,
                entry: GroupEntry::InlineGroup {
                  occur: Some(Occurrence {
                    occur: Occur::Exact {
                      lower: Some(2),
                      upper: None,
                      span: (63, 65, 4),
                    },
                    comments: None,
                  }),
                  group: Group {
                    group_choices: vec![GroupChoice {
                      group_entries: vec![(
                        GroupEntry::TypeGroupname {
                          ge: TypeGroupnameEntry {
                            occur: None,
                            name: Identifier {
                              ident: "test".into(),
                              socket: None,
                              span: (68, 72, 4),
                            },
                            generic_args: None,
                          },
                          leading_comments: None,
                          trailing_comments: None,
                          span: (68, 72, 4),
                        },
                        OptionalComma {
                          optional_comma: false,
                          trailing_comments: None,
                        },
                      )],
                      comments_before_grpchoice: None,
                      span: (68, 72, 4),
                    }],
                    span: (68, 72, 4),
                  },
                  comments_before_group: None,
                  comments_after_group: None,
                  span: (63, 74, 4),
                },
                comments_before_assigng: None,
                comments_after_assigng: None,
              }),
              comments_after_rule: None,
              span: (58, 74, 4),
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "messages".into(),
                  socket: None,
                  span: (75, 83, 5),
                },
                generic_params: None,
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::Typename {
                        ident: Identifier {
                          ident: "message".into(),
                          socket: None,
                          span: (86, 93, 5),
                        },
                        generic_args: Some(GenericArgs {
                          args: vec![
                            GenericArg {
                              arg: Box::from(Type1 {
                                type2: Type2::TextValue {
                                  value: "reboot".into(),
                                  span: (94, 102, 5),
                                },
                                operator: None,
                                comments_after_type: None,
                                span: (94, 102, 5),
                              }),
                              comments_before_type: None,
                              comments_after_type: None,
                            },
                            GenericArg {
                              arg: Box::from(Type1 {
                                type2: Type2::TextValue {
                                  value: "now".into(),
                                  span: (104, 109, 5),
                                },
                                operator: None,
                                comments_after_type: None,
                                span: (104, 109, 5),
                              }),
                              comments_before_type: None,
                              comments_after_type: None,
                            },
                          ],

                          span: (93, 110, 5),
                        }),
                        span: (86, 110, 5),
                      },
                      operator: None,
                      comments_after_type: None,
                      span: (86, 110, 5),
                    },
                    comments_before_type: None,
                    comments_after_type: None,
                  }],
                  span: (86, 110, 5),
                },
                comments_before_assignt: None,
                comments_after_assignt: None,
              },
              comments_after_rule: None,
              span: (75, 110, 5),
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "message".into(),
                  socket: None,
                  span: (111, 118, 6),
                },
                generic_params: Some(GenericParams {
                  params: vec![
                    GenericParam {
                      param: Identifier {
                        ident: "t".into(),
                        socket: None,
                        span: (119, 120, 6),
                      },
                      comments_before_ident: None,
                      comments_after_ident: None,
                    },
                    GenericParam {
                      param: Identifier {
                        ident: "v".into(),
                        socket: None,
                        span: (122, 123, 6),
                      },
                      comments_before_ident: None,
                      comments_after_ident: None,
                    },
                  ],
                  span: (118, 124, 6),
                }),
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::Map {
                        group: Group {
                          group_choices: vec![GroupChoice {
                            group_entries: vec![
                              (
                                GroupEntry::ValueMemberKey {
                                  ge: Box::from(ValueMemberKeyEntry {
                                    occur: None,
                                    member_key: Some(MemberKey::Bareword {
                                      ident: Identifier {
                                        ident: "type".into(),
                                        socket: None,
                                        span: (128, 132, 6),
                                      },
                                      comments: None,
                                      comments_after_colon: None,
                                      span: (128, 133, 6),
                                    }),
                                    entry_type: Type {
                                      type_choices: vec![TypeChoice {
                                        type1: Type1 {
                                          type2: Type2::UintValue {
                                            value: 2,
                                            span: (134, 135, 6),
                                          },
                                          operator: None,
                                          comments_after_type: None,
                                          span: (134, 135, 6),
                                        },
                                        comments_before_type: None,
                                        comments_after_type: None,
                                      }],
                                      span: (134, 135, 6),
                                    },
                                  }),
                                  leading_comments: None,
                                  trailing_comments: None,
                                  span: (128, 136, 6),
                                },
                                OptionalComma {
                                  optional_comma: true,
                                  trailing_comments: None,
                                },
                              ),
                              (
                                GroupEntry::ValueMemberKey {
                                  ge: Box::from(ValueMemberKeyEntry {
                                    occur: None,
                                    member_key: Some(MemberKey::Bareword {
                                      ident: Identifier {
                                        ident: "value".into(),
                                        socket: None,
                                        span: (137, 142, 6),
                                      },
                                      comments: None,
                                      comments_after_colon: None,
                                      span: (137, 143, 6),
                                    }),
                                    entry_type: Type {
                                      type_choices: vec![TypeChoice {
                                        type1: Type1 {
                                          type2: Type2::Typename {
                                            ident: Identifier {
                                              ident: "v".into(),
                                              socket: None,
                                              span: (144, 145, 6),
                                            },
                                            generic_args: None,
                                            span: (144, 145, 6),
                                          },
                                          operator: None,
                                          comments_after_type: None,
                                          span: (144, 145, 6),
                                        },
                                        comments_before_type: None,
                                        comments_after_type: None,
                                      }],
                                      span: (144, 145, 6),
                                    },
                                  }),
                                  leading_comments: None,
                                  trailing_comments: None,
                                  span: (137, 145, 6),
                                },
                                OptionalComma {
                                  optional_comma: false,
                                  trailing_comments: None,
                                },
                              ),
                            ],
                            comments_before_grpchoice: None,
                            span: (128, 145, 6),
                          }],
                          span: (128, 145, 6),
                        },
                        comments_before_group: None,
                        comments_after_group: None,
                        span: (127, 146, 6),
                      },
                      operator: None,
                      comments_after_type: None,
                      span: (127, 146, 6),
                    },
                    comments_before_type: None,
                    comments_after_type: None,
                  }],
                  span: (127, 146, 6),
                },
                comments_before_assignt: None,
                comments_after_assignt: None,
              },
              comments_after_rule: None,
              span: (111, 146, 6),
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "color".into(),
                  socket: None,
                  span: (147, 152, 7),
                },
                generic_params: None,
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::ChoiceFromGroup {
                        ident: Identifier {
                          ident: "colors".into(),
                          socket: None,
                          span: (156, 162, 7),
                        },
                        generic_args: None,
                        comments: None,
                        span: (155, 162, 7),
                      },
                      operator: None,
                      comments_after_type: None,
                      span: (155, 162, 7),
                    },
                    comments_before_type: None,
                    comments_after_type: None,
                  }],
                  span: (155, 162, 7),
                },
                comments_before_assignt: None,
                comments_after_assignt: None,
              },
              comments_after_rule: None,
              span: (147, 162, 7),
            },
            Rule::Group {
              rule: Box::from(GroupRule {
                name: Identifier {
                  ident: "colors".into(),
                  socket: None,
                  span: (163, 169, 8),
                },
                generic_params: None,
                is_group_choice_alternate: false,
                entry: GroupEntry::InlineGroup {
                  occur: None,
                  group: Group {
                    group_choices: vec![GroupChoice {
                      group_entries: vec![(
                        GroupEntry::ValueMemberKey {
                          ge: Box::from(ValueMemberKeyEntry {
                            occur: None,
                            member_key: Some(MemberKey::Bareword {
                              ident: Identifier {
                                ident: "red".into(),
                                socket: None,
                                span: (174, 177, 8),
                              },
                              comments: None,
                              comments_after_colon: None,
                              span: (174, 178, 8),
                            }),
                            entry_type: Type {
                              type_choices: vec![TypeChoice {
                                type1: Type1 {
                                  type2: Type2::TextValue {
                                    value: "red".into(),
                                    span: (179, 184, 8),
                                  },
                                  operator: None,
                                  comments_after_type: None,
                                  span: (179, 184, 8),
                                },
                                comments_before_type: None,
                                comments_after_type: None,
                              }],
                              span: (179, 184, 8),
                            },
                          }),
                          leading_comments: None,
                          trailing_comments: None,
                          span: (174, 184, 8),
                        },
                        OptionalComma {
                          optional_comma: false,
                          trailing_comments: None,
                        },
                      )],
                      comments_before_grpchoice: None,
                      span: (174, 184, 8),
                    }],
                    span: (174, 184, 8),
                  },
                  comments_before_group: None,
                  comments_after_group: None,
                  span: (172, 186, 8),
                },
                comments_before_assigng: None,
                comments_after_assigng: None,
              }),
              comments_after_rule: None,
              span: (163, 186, 8),
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "thing".into(),
                  socket: None,
                  span: (187, 192, 9),
                },
                generic_params: None,
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::ParenthesizedType {
                        pt: Type {
                          type_choices: vec![
                            TypeChoice {
                              type1: Type1 {
                                type2: Type2::Typename {
                                  ident: Identifier {
                                    ident: "int".into(),
                                    socket: None,
                                    span: (197, 200, 9),
                                  },
                                  generic_args: None,
                                  span: (197, 200, 9),
                                },
                                operator: None,
                                comments_after_type: None,
                                span: (197, 200, 9),
                              },
                              comments_before_type: None,
                              comments_after_type: None,
                            },
                            TypeChoice {
                              type1: Type1 {
                                type2: Type2::Typename {
                                  ident: Identifier {
                                    ident: "float".into(),
                                    socket: None,
                                    span: (203, 208, 9),
                                  },
                                  generic_args: None,
                                  span: (203, 208, 9),
                                },
                                operator: None,
                                comments_after_type: None,
                                span: (203, 208, 9),
                              },
                              comments_before_type: None,
                              comments_after_type: None,
                            },
                          ],
                          span: (197, 208, 9),
                        },
                        comments_before_type: None,
                        comments_after_type: None,
                        span: (195, 210, 9),
                      },
                      operator: None,
                      comments_after_type: None,
                      span: (195, 211, 9),
                    },
                    comments_before_type: None,
                    comments_after_type: None,
                  }],

                  span: (195, 211, 9),
                },
                comments_before_assignt: None,
                comments_after_assignt: None,
              },
              comments_after_rule: None,
              span: (187, 211, 9),
            },
          ],
          comments: None,
        };

        assert_eq!(cddl, expected_output);
        assert_eq!(cddl.to_string(), expected_output.to_string());

        Ok(())
      }

      #[cfg(feature = "std")]
      Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
        let _ = p.report_errors(true);

        Err(Error::CDDL(p.report_errors(false).unwrap().unwrap()))
      }
      #[cfg(not(feature = "std"))]
      Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
        let _ = p.report_errors();

        Err(Error::CDDL(p.report_errors().unwrap()))
      }
      Err(e) => Err(e),
    },
    Err(e) => Err(e),
  }
}
