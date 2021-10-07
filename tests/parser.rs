#![cfg(feature = "ast-span")]
#![cfg(feature = "ast-comments")]

use std::marker::PhantomData;

use cddl::{
  ast::*,
  cddl_from_str,
  lexer::Lexer,
  lexer_from_str,
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
        test = ( int / float )
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
                  span: Span(0, 6, 1),
                },
                generic_params: None,
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::Typename(Typename {
                        ident: Identifier {
                          ident: "secondrule".into(),
                          socket: None,
                          span: Span(9, 19, 1),
                        },
                        generic_args: None,
                        span: Span(9, 19, 1),
                      }),
                      span: Span(9, 19, 1),
                      ..Default::default()
                    },
                    ..Default::default()
                  }],

                  span: Span(9, 19, 1),
                },
                ..Default::default()
              },
              comments_after_rule: None,
              span: Span(0, 19, 1),
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "myrange".into(),
                  socket: None,
                  span: Span(20, 27, 2),
                },
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::UintValue(UintValue {
                        value: 10,
                        span: Span(30, 32, 2),
                      }),
                      operator: Some(Operator {
                        operator: RangeCtlOp::RangeOp {
                          is_inclusive: true,
                          span: Span(32, 34, 2),
                        },
                        type2: Type2::Typename(Typename {
                          ident: Identifier {
                            ident: "upper".into(),
                            socket: None,
                            span: Span(34, 39, 2),
                          },
                          generic_args: None,
                          span: Span(34, 39, 2),
                        }),
                        comments_before_operator: None,
                        comments_after_operator: None,
                      }),
                      comments_after_type: None,
                      span: Span(30, 39, 2),
                    },
                    comments_before_type: None,
                    comments_after_type: None,
                  }],
                  span: Span(30, 39, 2),
                },
                ..Default::default()
              },
              comments_after_rule: None,
              span: Span(20, 39, 2),
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "upper".into(),
                  socket: None,
                  span: Span(40, 45, 3),
                },
                generic_params: None,
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![
                    TypeChoice {
                      type1: Type1 {
                        type2: Type2::UintValue(UintValue {
                          value: 500,
                          span: Span(48, 51, 3),
                        }),
                        span: Span(48, 51, 3),
                        ..Default::default()
                      },
                      ..Default::default()
                    },
                    TypeChoice {
                      type1: Type1 {
                        type2: Type2::UintValue(UintValue {
                          value: 600,
                          span: Span(54, 57, 3),
                        }),
                        span: Span(54, 57, 3),
                        ..Default::default()
                      },
                      ..Default::default()
                    },
                  ],
                  span: Span(48, 57, 3),
                },
                ..Default::default()
              },
              comments_after_rule: None,
              span: Span(40, 57, 3),
            },
            Rule::Group {
              rule: Box::from(GroupRule {
                name: Identifier {
                  ident: "gr".into(),
                  socket: None,
                  span: Span(58, 60, 4),
                },
                generic_params: None,
                is_group_choice_alternate: false,
                entry: GroupEntry::InlineGroup {
                  occur: Some(Occurrence {
                    occur: Occur::Exact {
                      lower: Some(2),
                      upper: None,
                      span: Span(63, 65, 4),
                    },
                    comments: None,
                    _a: PhantomData::default(),
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
                              span: Span(68, 72, 4),
                            },
                            generic_args: None,
                          },
                          leading_comments: None,
                          trailing_comments: None,
                          span: Span(68, 72, 4),
                        },
                        OptionalComma {
                          optional_comma: false,
                          trailing_comments: None,
                          _a: PhantomData::default(),
                        },
                      )],
                      comments_before_grpchoice: None,
                      span: Span(68, 72, 4),
                    }],
                    span: Span(68, 72, 4),
                  },
                  comments_before_group: None,
                  comments_after_group: None,
                  span: Span(63, 74, 4),
                },
                comments_before_assigng: None,
                comments_after_assigng: None,
              }),
              comments_after_rule: None,
              span: Span(58, 74, 4),
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "messages".into(),
                  socket: None,
                  span: Span(75, 83, 5),
                },
                generic_params: None,
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::Typename(Typename {
                        ident: Identifier {
                          ident: "message".into(),
                          socket: None,
                          span: Span(86, 93, 5),
                        },
                        generic_args: Some(GenericArgs {
                          args: vec![
                            GenericArg {
                              arg: Box::from(Type1 {
                                type2: Type2::TextValue(TextValue {
                                  value: "reboot".into(),
                                  span: Span(94, 102, 5),
                                }),
                                span: Span(94, 102, 5),
                                ..Default::default()
                              }),
                              ..Default::default()
                            },
                            GenericArg {
                              arg: Box::from(Type1 {
                                type2: Type2::TextValue(TextValue {
                                  value: "now".into(),
                                  span: Span(104, 109, 5),
                                }),
                                span: Span(104, 109, 5),
                                ..Default::default()
                              }),
                              ..Default::default()
                            },
                          ],
                          span: Span(93, 110, 5),
                        }),
                        span: Span(86, 110, 5),
                      }),
                      span: Span(86, 110, 5),
                      ..Default::default()
                    },
                    ..Default::default()
                  }],
                  span: Span(86, 110, 5),
                },
                ..Default::default()
              },
              span: Span(75, 110, 5),
              comments_after_rule: None,
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "message".into(),
                  socket: None,
                  span: Span(111, 118, 6),
                },
                generic_params: Some(GenericParams {
                  params: vec![
                    GenericParam {
                      param: Identifier {
                        ident: "t".into(),
                        socket: None,
                        span: Span(119, 120, 6),
                      },
                      ..Default::default()
                    },
                    GenericParam {
                      param: Identifier {
                        ident: "v".into(),
                        socket: None,
                        span: Span(122, 123, 6),
                      },
                      ..Default::default()
                    },
                  ],
                  span: Span(118, 124, 6),
                }),
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::Map(Map {
                        group: Group {
                          group_choices: vec![GroupChoice {
                            group_entries: vec![
                              (
                                GroupEntry::ValueMemberKey {
                                  ge: Box::from(ValueMemberKeyEntry {
                                    occur: None,
                                    member_key: Some(MemberKey::Bareword(BarewordMemberKey {
                                      ident: Identifier {
                                        ident: "type".into(),
                                        socket: None,
                                        span: Span(128, 132, 6),
                                      },
                                      span: Span(128, 133, 6),
                                      ..Default::default()
                                    })),
                                    entry_type: Type {
                                      type_choices: vec![TypeChoice {
                                        type1: Type1 {
                                          type2: Type2::UintValue(UintValue {
                                            value: 2,
                                            span: Span(134, 135, 6),
                                          }),
                                          span: Span(134, 135, 6),
                                          ..Default::default()
                                        },
                                        ..Default::default()
                                      }],
                                      span: Span(134, 135, 6),
                                    },
                                  }),
                                  leading_comments: None,
                                  trailing_comments: None,
                                  span: Span(128, 136, 6),
                                },
                                OptionalComma {
                                  optional_comma: true,
                                  trailing_comments: None,
                                  _a: PhantomData::default(),
                                },
                              ),
                              (
                                GroupEntry::ValueMemberKey {
                                  ge: Box::from(ValueMemberKeyEntry {
                                    occur: None,
                                    member_key: Some(MemberKey::Bareword(BarewordMemberKey {
                                      ident: Identifier {
                                        ident: "value".into(),
                                        socket: None,
                                        span: Span(137, 142, 6),
                                      },
                                      span: Span(137, 143, 6),
                                      ..Default::default()
                                    })),
                                    entry_type: Type {
                                      type_choices: vec![TypeChoice {
                                        type1: Type1 {
                                          type2: Type2::Typename(Typename {
                                            ident: Identifier {
                                              ident: "v".into(),
                                              socket: None,
                                              span: Span(144, 145, 6),
                                            },
                                            generic_args: None,
                                            span: Span(144, 145, 6),
                                          }),
                                          span: Span(144, 145, 6),
                                          ..Default::default()
                                        },
                                        ..Default::default()
                                      }],
                                      span: Span(144, 145, 6),
                                    },
                                  }),
                                  leading_comments: None,
                                  trailing_comments: None,
                                  span: Span(137, 145, 6),
                                },
                                OptionalComma {
                                  optional_comma: false,
                                  trailing_comments: None,
                                  _a: PhantomData::default(),
                                },
                              ),
                            ],
                            comments_before_grpchoice: None,
                            span: Span(128, 145, 6),
                          }],
                          span: Span(128, 145, 6),
                        },
                        span: Span(127, 146, 6),
                        ..Default::default()
                      }),
                      span: Span(127, 146, 6),
                      ..Default::default()
                    },
                    ..Default::default()
                  }],
                  span: Span(127, 146, 6),
                },
                ..Default::default()
              },
              comments_after_rule: None,
              span: Span(111, 146, 6),
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "color".into(),
                  socket: None,
                  span: Span(147, 152, 7),
                },
                generic_params: None,
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::ChoiceFromGroup(ChoiceFromGroup {
                        ident: Identifier {
                          ident: "colors".into(),
                          socket: None,
                          span: Span(156, 162, 7),
                        },
                        span: Span(155, 162, 7),
                        ..Default::default()
                      }),
                      span: Span(155, 162, 7),
                      ..Default::default()
                    },
                    ..Default::default()
                  }],
                  span: Span(155, 162, 7),
                },
                ..Default::default()
              },
              comments_after_rule: None,
              span: Span(147, 162, 7),
            },
            Rule::Group {
              rule: Box::from(GroupRule {
                name: Identifier {
                  ident: "colors".into(),
                  socket: None,
                  span: Span(163, 169, 8),
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
                            member_key: Some(MemberKey::Bareword(BarewordMemberKey {
                              ident: Identifier {
                                ident: "red".into(),
                                socket: None,
                                span: Span(174, 177, 8),
                              },
                              span: Span(174, 178, 8),
                              ..Default::default()
                            })),
                            entry_type: Type {
                              type_choices: vec![TypeChoice {
                                type1: Type1 {
                                  type2: Type2::TextValue(TextValue {
                                    value: "red".into(),
                                    span: Span(179, 184, 8),
                                  }),
                                  span: Span(179, 184, 8),
                                  ..Default::default()
                                },
                                ..Default::default()
                              }],
                              span: Span(179, 184, 8),
                            },
                          }),
                          leading_comments: None,
                          trailing_comments: None,
                          span: Span(174, 184, 8),
                        },
                        OptionalComma {
                          optional_comma: false,
                          trailing_comments: None,
                          _a: PhantomData::default(),
                        },
                      )],
                      comments_before_grpchoice: None,
                      span: Span(174, 184, 8),
                    }],
                    span: Span(174, 184, 8),
                  },
                  comments_before_group: None,
                  comments_after_group: None,
                  span: Span(172, 186, 8),
                },
                comments_before_assigng: None,
                comments_after_assigng: None,
              }),
              comments_after_rule: None,
              span: Span(163, 186, 8),
            },
            Rule::Type {
              rule: TypeRule {
                name: Identifier {
                  ident: "test".into(),
                  socket: None,
                  span: Span(187, 191, 9),
                },
                generic_params: None,
                is_type_choice_alternate: false,
                value: Type {
                  type_choices: vec![TypeChoice {
                    type1: Type1 {
                      type2: Type2::ParenthesizedType(ParenthesizedType {
                        pt: Type {
                          type_choices: vec![
                            TypeChoice {
                              type1: Type1 {
                                type2: Type2::Typename(Typename {
                                  ident: Identifier {
                                    ident: "int".into(),
                                    socket: None,
                                    span: Span(196, 199, 9),
                                  },
                                  generic_args: None,
                                  span: Span(196, 199, 9),
                                }),
                                span: Span(196, 199, 9),
                                ..Default::default()
                              },
                              ..Default::default()
                            },
                            TypeChoice {
                              type1: Type1 {
                                type2: Type2::Typename(Typename {
                                  ident: Identifier {
                                    ident: "float".into(),
                                    socket: None,
                                    span: Span(202, 207, 9),
                                  },
                                  generic_args: None,
                                  span: Span(202, 207, 9),
                                }),
                                span: Span(202, 207, 9),
                                ..Default::default()
                              },
                              ..Default::default()
                            },
                          ],
                          span: Span(196, 207, 9),
                        },
                        span: Span(194, 209, 9),
                        ..Default::default()
                      }),
                      span: Span(194, 210, 9),
                      ..Default::default()
                    },
                    ..Default::default()
                  }],

                  span: Span(194, 210, 9),
                },
                comments_before_assignt: None,
                comments_after_assignt: None,
              },
              comments_after_rule: None,
              span: Span(187, 210, 9),
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

#[test]
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

  let mut l = lexer_from_str(cddl);
  let c_ast = cddl_from_str(&mut l, cddl, true)?;

  println!("{}", c_ast);

  Ok(())
}
