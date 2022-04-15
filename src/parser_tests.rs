#[cfg(test)]
#[allow(unused_imports)]
#[cfg(feature = "ast-span")]
#[cfg(feature = "ast-comments")]
mod tests {
  use crate::{
    ast::*,
    lexer::Lexer,
    parser::*,
    token::{self, SocketPlug},
  };

  use std::marker::PhantomData;

  use indoc::indoc;
  use pretty_assertions::assert_eq;

  #[test]
  fn verify_rule_diagnostic() -> Result<()> {
    let input = indoc!(
      r#"
        a = 1234
        a = b
      "#
    );

    match Parser::new(input, Box::new(Lexer::new(input).iter())) {
      Ok(mut p) => match p.parse_cddl() {
        Ok(_) => Ok(()),
        #[cfg(feature = "std")]
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
          let e = p.report_errors(false).unwrap().unwrap();

          #[cfg(feature = "std")]
          println!("{}", e);

          assert_eq!(
            e,
            indoc!(
              r#"
                error: parser errors
                  ┌─ input:2:1
                  │
                2 │ a = b
                  │ ^^^^^ rule with the same identifier is already defined

              "#
            )
          );
          Ok(())
        }
        #[cfg(not(feature = "std"))]
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
          assert_eq!(
            p.report_errors().unwrap(),
            indoc!(
              r#"
                error: parser errors

                   ┌── input:2:1 ───
                   │
                 2 │ a = b
                   │ ^^^^^ rule with the same identifier is already defined

              "#
            )
          );
          Ok(())
        }
        Err(e) => Err(e),
      },
      Err(e) => Err(e),
    }
  }

  #[test]
  fn verify_genericparams() -> Result<()> {
    let input = r#"<t, v>"#;

    let l = Lexer::new(input);
    let gps = Parser::new(input, Box::new(l.iter()))?.parse_genericparm()?;

    let expected_output = GenericParams {
      params: vec![
        GenericParam {
          param: Identifier {
            ident: "t".into(),
            socket: None,
            span: (1, 2, 1),
          },
          comments_before_ident: None,
          comments_after_ident: None,
        },
        GenericParam {
          param: Identifier {
            ident: "v".into(),
            socket: None,
            span: (4, 5, 1),
          },
          comments_before_ident: None,
          comments_after_ident: None,
        },
      ],
      span: (0, 6, 1),
    };

    assert_eq!(gps, expected_output);
    assert_eq!(gps.to_string(), expected_output.to_string());

    Ok(())
  }

  #[test]
  fn verify_genericparm_diagnostic() -> Result<()> {
    let input = r#"<1, 2>"#;

    match Parser::new(input, Box::new(Lexer::new(input).iter())) {
      Ok(mut p) => match p.parse_genericparm() {
        Ok(_) => Ok(()),
        #[cfg(feature = "std")]
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
          let e = p.report_errors(false).unwrap().unwrap();

          #[cfg(feature = "std")]
          println!("{}", e);

          assert_eq!(
            e,
            indoc!(
              r#"
              error: parser errors
                ┌─ input:1:2
                │
              1 │ <1, 2>
                │  ^ generic parameters must be named identifiers

              "#
            )
          );
          Ok(())
        }
        #[cfg(not(feature = "std"))]
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
          assert_eq!(
            p.report_errors().unwrap(),
            indoc!(
              r#"
                error: parser errors

                   ┌── input:1:2 ───
                   │
                 1 │ <1, 2>
                   │  ^ generic parameters must be named identifiers

              "#
            )
          );
          Ok(())
        }
        Err(e) => Err(e),
      },
      Err(e) => Err(e),
    }
  }

  #[test]
  fn verify_genericparm_rule_diagnostic() -> Result<()> {
    let input = indoc!(
      r#"
        rule<paramA paramB> = test
        ruleb = rulec
        ruleb = ruled
        rulec = rulee
        rulec = rulee2
      "#
    );

    match Parser::new(input, Box::new(Lexer::new(input).iter())) {
      Ok(mut p) => match p.parse_cddl() {
        Ok(_) => Ok(()),
        #[cfg(feature = "std")]
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
          let e = p.report_errors(false).unwrap().unwrap();

          println!("{}", e);

          assert_eq!(
            e,
            indoc!(
              r#"
                error: parser errors
                  ┌─ input:1:6
                  │
                1 │ rule<paramA paramB> = test
                  │      ^^^^^^^^^^^^^ generic parameters should be between angle brackets '<' and '>' and separated by a comma ','
                2 │ ruleb = rulec
                3 │ ruleb = ruled
                  │ ^^^^^^^^^^^^^ rule with the same identifier is already defined
                4 │ rulec = rulee
                5 │ rulec = rulee2
                  │ ^^^^^^^^^^^^^^ rule with the same identifier is already defined

              "#
            )
          );
          Ok(())
        }
        #[cfg(not(feature = "std"))]
        Err(Error::INCREMENTAL) if !p.errors.is_empty() => {
          assert_eq!(
            p.report_errors().unwrap(),
            indoc!(
              r#"
                error: parser errors

                   ┌── input:1:6 ───
                   │
                 1 │ rule<paramA paramB> = test
                   │      ^^^^^^^^^^^^^ Generic parameters should be between angle brackets '<' and '>' and separated by a comma ','
                 2 │ ruleb = rulec
                 3 │ ruleb = ruled
                   │ ^^^^^^^^^^^^^ rule with the same identifier is already defined
                 4 │ rulec = rulee
                 5 │ rulec = rulee2
                   │ ^^^^^^^^^^^^^^ rule with the same identifier is already defined

              "#
            )
          );
          Ok(())
        }
        Err(e) => Err(e),
      },
      Err(e) => Err(e),
    }
  }

  #[test]
  fn verify_genericargs() -> Result<()> {
    let input = r#"<"reboot", "now">"#;

    let generic_args =
      Parser::new(input, Box::new(Lexer::new(input).iter()))?.parse_genericargs()?;

    let expected_output = GenericArgs {
      args: vec![
        GenericArg {
          arg: Box::from(Type1 {
            type2: Type2::TextValue {
              value: "reboot".into(),
              span: (1, 9, 1),
            },
            operator: None,
            comments_after_type: None,
            span: (1, 9, 1),
          }),
          comments_before_type: None,
          comments_after_type: None,
        },
        GenericArg {
          arg: Box::from(Type1 {
            type2: Type2::TextValue {
              value: "now".into(),
              span: (11, 16, 1),
            },
            operator: None,
            comments_after_type: None,
            span: (11, 16, 1),
          }),
          comments_before_type: None,
          comments_after_type: None,
        },
      ],
      span: (0, 17, 1),
    };

    assert_eq!(generic_args, expected_output);
    assert_eq!(generic_args.to_string(), expected_output.to_string());

    Ok(())
  }

  #[test]
  fn verify_type() -> Result<()> {
    let input = r#"( tchoice1 / tchoice2 )"#;

    let t = Parser::new(input, Box::new(Lexer::new(input).iter()))?.parse_type(None)?;

    let expected_output = Type {
      type_choices: vec![TypeChoice {
        type1: Type1 {
          type2: Type2::ParenthesizedType {
            pt: Type {
              type_choices: vec![
                TypeChoice {
                  type1: Type1 {
                    type2: Type2::Typename {
                      ident: Identifier {
                        ident: "tchoice1".into(),
                        socket: None,
                        span: (2, 10, 1),
                      },
                      generic_args: None,
                      span: (2, 10, 1),
                    },
                    operator: None,
                    comments_after_type: None,
                    span: (2, 10, 1),
                  },
                  comments_before_type: None,
                  comments_after_type: None,
                },
                TypeChoice {
                  type1: Type1 {
                    type2: Type2::Typename {
                      ident: Identifier {
                        ident: "tchoice2".into(),
                        socket: None,
                        span: (13, 21, 1),
                      },
                      generic_args: None,
                      span: (13, 21, 1),
                    },
                    operator: None,
                    comments_after_type: None,
                    span: (13, 21, 1),
                  },
                  comments_before_type: None,
                  comments_after_type: None,
                },
              ],
              span: (2, 21, 1),
            },
            comments_before_type: None,
            comments_after_type: None,
            span: (0, 23, 1),
          },
          operator: None,
          comments_after_type: None,
          span: (0, 23, 1),
        },
        comments_before_type: None,
        comments_after_type: None,
      }],
      span: (0, 23, 1),
    };

    assert_eq!(t, expected_output);
    assert_eq!(t.to_string(), expected_output.to_string());

    Ok(())
  }

  #[test]
  fn verify_type1() -> Result<()> {
    let inputs = [
      r#"5..10"#,
      r#"-10.5...10.1"#,
      r#"1.5..4.5"#,
      r#"my..lower ... upper"#,
      r#"target .lt controller"#,
      r#"( text / tstr ) .eq "hello""#,
    ];

    let expected_outputs = [
      Type1 {
        type2: Type2::UintValue {
          value: 5,
          span: (0, 1, 1),
        },
        operator: Some(Operator {
          operator: RangeCtlOp::RangeOp {
            is_inclusive: true,
            span: (1, 3, 1),
          },
          type2: Type2::UintValue {
            value: 10,
            span: (3, 5, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
        span: (0, 5, 1),
      },
      Type1 {
        type2: Type2::FloatValue {
          value: -10.5,
          span: (0, 5, 1),
        },
        operator: Some(Operator {
          operator: RangeCtlOp::RangeOp {
            is_inclusive: false,
            span: (5, 8, 1),
          },
          type2: Type2::FloatValue {
            value: 10.1,
            span: (8, 12, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
        span: (0, 12, 1),
      },
      Type1 {
        type2: Type2::FloatValue {
          value: 1.5,
          span: (0, 3, 1),
        },
        operator: Some(Operator {
          operator: RangeCtlOp::RangeOp {
            is_inclusive: true,
            span: (3, 5, 1),
          },
          type2: Type2::FloatValue {
            value: 4.5,
            span: (5, 8, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
        span: (0, 8, 1),
      },
      Type1 {
        type2: Type2::Typename {
          ident: Identifier {
            ident: "my..lower".into(),
            socket: None,
            span: (0, 9, 1),
          },
          generic_args: None,
          span: (0, 9, 1),
        },
        operator: Some(Operator {
          operator: RangeCtlOp::RangeOp {
            is_inclusive: false,
            span: (10, 13, 1),
          },
          type2: Type2::Typename {
            ident: Identifier {
              ident: "upper".into(),
              socket: None,
              span: (14, 19, 1),
            },
            generic_args: None,
            span: (14, 19, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
        span: (0, 19, 1),
      },
      Type1 {
        type2: Type2::Typename {
          ident: Identifier {
            ident: "target".into(),
            socket: None,
            span: (0, 6, 1),
          },
          generic_args: None,
          span: (0, 6, 1),
        },
        operator: Some(Operator {
          operator: RangeCtlOp::CtlOp {
            ctrl: ".lt",
            span: (7, 10, 1),
          },
          type2: Type2::Typename {
            ident: Identifier {
              ident: "controller".into(),
              socket: None,
              span: (11, 21, 1),
            },
            generic_args: None,
            span: (11, 21, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
        span: (0, 21, 1),
      },
      Type1 {
        type2: Type2::ParenthesizedType {
          pt: Type {
            type_choices: vec![
              TypeChoice {
                type1: Type1 {
                  type2: Type2::Typename {
                    ident: Identifier {
                      ident: "text".into(),
                      socket: None,
                      span: (2, 6, 1),
                    },
                    generic_args: None,
                    span: (2, 6, 1),
                  },
                  operator: None,
                  comments_after_type: None,
                  span: (2, 6, 1),
                },
                comments_before_type: None,
                comments_after_type: None,
              },
              TypeChoice {
                type1: Type1 {
                  type2: Type2::Typename {
                    ident: Identifier {
                      ident: "tstr".into(),
                      socket: None,
                      span: (9, 13, 1),
                    },
                    generic_args: None,
                    span: (9, 13, 1),
                  },
                  operator: None,
                  comments_after_type: None,
                  span: (9, 13, 1),
                },
                comments_before_type: None,
                comments_after_type: None,
              },
            ],

            span: (2, 13, 1),
          },
          comments_before_type: None,
          comments_after_type: None,
          span: (0, 15, 1),
        },
        operator: Some(Operator {
          operator: RangeCtlOp::CtlOp {
            ctrl: ".eq",
            span: (16, 19, 1),
          },
          type2: Type2::TextValue {
            value: "hello".into(),
            span: (20, 27, 1),
          },
          comments_before_operator: None,
          comments_after_operator: None,
        }),
        comments_after_type: None,
        span: (0, 27, 1),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(inputs[idx]);
      let t1 = Parser::new(inputs[idx], Box::new(l.iter()))?.parse_type1(None)?;

      assert_eq!(&t1, expected_output);
      assert_eq!(t1.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_type2() -> Result<()> {
    let inputs = [
      r#""myvalue""#,
      r#"message<"reboot", "now">"#,
      r#"$$tcp-option"#,
      r#"~group1"#,
      r#"#6.997(tstr)"#,
      r#"9.9"#,
      r#"#"#,
      r#"[*3 reputon]"#,
      r#"[+ reputon]"#,
      r#"&groupname"#,
      r#"&( inlinegroup )"#,
      r#"{ ? "optional-key" ^ => int, }"#,
      r#"[ ( a: int, b: tstr ) ]"#,
    ];

    let expected_outputs = [
      Type2::TextValue {
        value: "myvalue".into(),
        span: (0, 9, 1),
      },
      Type2::Typename {
        ident: Identifier {
          ident: "message".into(),
          socket: None,
          span: (0, 7, 1),
        },
        generic_args: Some(GenericArgs {
          args: vec![
            GenericArg {
              arg: Box::from(Type1 {
                type2: Type2::TextValue {
                  value: "reboot".into(),
                  span: (8, 16, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (8, 16, 1),
              }),
              comments_before_type: None,
              comments_after_type: None,
            },
            GenericArg {
              arg: Box::from(Type1 {
                type2: Type2::TextValue {
                  value: "now".into(),
                  span: (18, 23, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (18, 23, 1),
              }),
              comments_before_type: None,
              comments_after_type: None,
            },
          ],
          span: (7, 24, 1),
        }),
        span: (0, 24, 1),
      },
      Type2::Typename {
        ident: Identifier {
          ident: "tcp-option".into(),
          socket: Some(SocketPlug::GROUP),
          span: (0, 12, 1),
        },
        generic_args: None,
        span: (0, 12, 1),
      },
      Type2::Unwrap {
        ident: Identifier {
          ident: "group1".into(),
          socket: None,
          span: (1, 7, 1),
        },
        generic_args: None,
        comments: None,
        span: (0, 0, 0),
      },
      Type2::TaggedData {
        tag: Some(997),
        t: Type {
          type_choices: vec![TypeChoice {
            type1: Type1 {
              type2: Type2::Typename {
                ident: Identifier {
                  ident: "tstr".into(),
                  socket: None,
                  span: (7, 11, 1),
                },
                generic_args: None,
                span: (7, 11, 1),
              },
              operator: None,
              comments_after_type: None,
              span: (7, 11, 1),
            },
            comments_before_type: None,
            comments_after_type: None,
          }],
          span: (7, 11, 1),
        },
        comments_before_type: None,
        comments_after_type: None,
        span: (0, 11, 1),
      },
      Type2::FloatValue {
        value: 9.9,
        span: (0, 3, 1),
      },
      Type2::Any((0, 1, 1)),
      Type2::Array {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::TypeGroupname {
                ge: TypeGroupnameEntry {
                  occur: Some(Occurrence {
                    occur: Occur::Exact {
                      lower: None,
                      upper: Some(3),
                      span: (1, 3, 1),
                    },
                    comments: None,
                    _a: PhantomData::default(),
                  }),
                  name: Identifier {
                    ident: "reputon".into(),
                    socket: None,
                    span: (4, 11, 1),
                  },
                  generic_args: None,
                },
                leading_comments: None,
                trailing_comments: None,
                span: (1, 11, 1),
              },
              OptionalComma {
                optional_comma: false,
                trailing_comments: None,
                _a: PhantomData::default(),
              },
            )],
            comments_before_grpchoice: None,
            span: (1, 11, 1),
          }],
          span: (1, 11, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
        span: (0, 12, 1),
      },
      Type2::Array {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::TypeGroupname {
                ge: TypeGroupnameEntry {
                  occur: Some(Occurrence {
                    occur: Occur::OneOrMore((1, 2, 1)),
                    comments: None,
                    _a: PhantomData::default(),
                  }),
                  name: Identifier {
                    ident: "reputon".into(),
                    socket: None,
                    span: (3, 10, 1),
                  },
                  generic_args: None,
                },
                leading_comments: None,
                trailing_comments: None,
                span: (1, 10, 1),
              },
              OptionalComma {
                optional_comma: false,
                trailing_comments: None,
                _a: PhantomData::default(),
              },
            )],
            comments_before_grpchoice: None,
            span: (1, 10, 1),
          }],
          span: (1, 10, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
        span: (0, 11, 1),
      },
      Type2::ChoiceFromGroup {
        ident: Identifier {
          ident: "groupname".into(),
          socket: None,
          span: (1, 10, 1),
        },
        generic_args: None,
        comments: None,
        span: (0, 10, 1),
      },
      Type2::ChoiceFromInlineGroup {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::TypeGroupname {
                ge: TypeGroupnameEntry {
                  occur: None,
                  name: Identifier {
                    ident: "inlinegroup".into(),
                    socket: None,
                    span: (3, 14, 1),
                  },
                  generic_args: None,
                },
                leading_comments: None,
                trailing_comments: None,
                span: (3, 14, 1),
              },
              OptionalComma {
                optional_comma: false,
                trailing_comments: None,
                _a: PhantomData::default(),
              },
            )],
            comments_before_grpchoice: None,
            span: (3, 14, 1),
          }],
          span: (3, 14, 1),
        },
        comments: None,
        comments_before_group: None,
        comments_after_group: None,
        span: (0, 14, 1),
      },
      Type2::Map {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::ValueMemberKey {
                ge: Box::from(ValueMemberKeyEntry {
                  occur: Some(Occurrence {
                    occur: Occur::Optional((2, 3, 1)),
                    comments: None,
                    _a: PhantomData::default(),
                  }),
                  member_key: Some(MemberKey::Type1 {
                    t1: Box::from(Type1 {
                      type2: Type2::TextValue {
                        value: "optional-key".into(),
                        span: (4, 18, 1),
                      },
                      operator: None,
                      comments_after_type: None,
                      span: (4, 18, 1),
                    }),
                    is_cut: true,
                    comments_before_cut: None,
                    comments_after_cut: None,
                    comments_after_arrowmap: None,
                    span: (4, 23, 1),
                  }),
                  entry_type: Type {
                    type_choices: vec![TypeChoice {
                      type1: Type1 {
                        type2: Type2::Typename {
                          ident: Identifier {
                            ident: "int".into(),
                            socket: None,
                            span: (24, 27, 1),
                          },
                          generic_args: None,
                          span: (24, 27, 1),
                        },
                        operator: None,
                        comments_after_type: None,
                        span: (24, 27, 1),
                      },
                      comments_before_type: None,
                      comments_after_type: None,
                    }],
                    span: (24, 27, 1),
                  },
                }),
                leading_comments: None,
                trailing_comments: None,
                span: (2, 28, 1),
              },
              OptionalComma {
                optional_comma: true,
                trailing_comments: None,
                _a: PhantomData::default(),
              },
            )],
            comments_before_grpchoice: None,
            span: (2, 28, 1),
          }],
          span: (2, 28, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
        span: (0, 30, 1),
      },
      Type2::Array {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![(
              GroupEntry::InlineGroup {
                group: Group {
                  group_choices: vec![GroupChoice {
                    group_entries: vec![
                      (
                        GroupEntry::ValueMemberKey {
                          ge: Box::from(ValueMemberKeyEntry {
                            occur: None,
                            member_key: Some(MemberKey::Bareword {
                              ident: Identifier {
                                ident: "a".into(),
                                socket: None,
                                span: (4, 5, 1),
                              },
                              comments: None,
                              comments_after_colon: None,
                              span: (4, 6, 1),
                            }),
                            entry_type: Type {
                              type_choices: vec![TypeChoice {
                                type1: Type1 {
                                  type2: Type2::Typename {
                                    ident: Identifier {
                                      ident: "int".into(),
                                      socket: None,
                                      span: (7, 10, 1),
                                    },
                                    generic_args: None,
                                    span: (7, 10, 1),
                                  },
                                  operator: None,
                                  comments_after_type: None,
                                  span: (7, 10, 1),
                                },
                                comments_before_type: None,
                                comments_after_type: None,
                              }],
                              span: (7, 10, 1),
                            },
                          }),
                          leading_comments: None,
                          trailing_comments: None,
                          span: (4, 11, 1),
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
                            member_key: Some(MemberKey::Bareword {
                              ident: Identifier {
                                ident: "b".into(),
                                socket: None,
                                span: (12, 13, 1),
                              },
                              comments: None,
                              comments_after_colon: None,
                              span: (12, 14, 1),
                            }),
                            entry_type: Type {
                              type_choices: vec![TypeChoice {
                                type1: Type1 {
                                  type2: Type2::Typename {
                                    ident: Identifier {
                                      ident: "tstr".into(),
                                      socket: None,
                                      span: (15, 19, 1),
                                    },
                                    generic_args: None,
                                    span: (15, 19, 1),
                                  },
                                  operator: None,
                                  comments_after_type: None,
                                  span: (15, 19, 1),
                                },
                                comments_before_type: None,
                                comments_after_type: None,
                              }],
                              span: (15, 19, 1),
                            },
                          }),
                          leading_comments: None,
                          trailing_comments: None,
                          span: (12, 19, 1),
                        },
                        OptionalComma {
                          optional_comma: false,
                          trailing_comments: None,
                          _a: PhantomData::default(),
                        },
                      ),
                    ],
                    comments_before_grpchoice: None,
                    span: (4, 19, 1),
                  }],
                  span: (4, 19, 1),
                },
                occur: None,
                comments_before_group: None,
                comments_after_group: None,
                span: (2, 21, 1),
              },
              OptionalComma {
                optional_comma: false,
                trailing_comments: None,
                _a: PhantomData::default(),
              },
            )],
            comments_before_grpchoice: None,
            span: (2, 21, 1),
          }],
          span: (2, 21, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
        span: (0, 23, 1),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(inputs[idx]);
      let t2 = Parser::new(inputs[idx], Box::new(l.iter()))?.parse_type2()?;

      assert_eq!(&t2, expected_output);
      assert_eq!(t2.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_type2_complex() -> Result<()> {
    let inputs = [
      r#"[ [* file-entry], [* directory-entry] ]"#,
      r#"{ int, int // int, tstr }"#,
      r#"{ int, int, int, tstr }"#,
    ];

    let expected_ouputs = [
      Type2::Array {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![
              (
                GroupEntry::ValueMemberKey {
                  ge: Box::from(ValueMemberKeyEntry {
                    occur: None,
                    member_key: None,
                    entry_type: Type {
                      type_choices: vec![TypeChoice {
                        type1: Type1 {
                          type2: Type2::Array {
                            group: Group {
                              group_choices: vec![GroupChoice {
                                group_entries: vec![(
                                  GroupEntry::TypeGroupname {
                                    ge: TypeGroupnameEntry {
                                      occur: Some(Occurrence {
                                        occur: Occur::ZeroOrMore((3, 4, 1)),
                                        comments: None,
                                        _a: PhantomData::default(),
                                      }),
                                      name: Identifier {
                                        ident: "file-entry".into(),
                                        socket: None,
                                        span: (5, 15, 1),
                                      },
                                      generic_args: None,
                                    },
                                    leading_comments: None,
                                    trailing_comments: None,
                                    span: (3, 15, 1),
                                  },
                                  OptionalComma {
                                    optional_comma: false,
                                    trailing_comments: None,
                                    _a: PhantomData::default(),
                                  },
                                )],
                                comments_before_grpchoice: None,
                                span: (3, 15, 1),
                              }],
                              span: (3, 15, 1),
                            },
                            comments_before_group: None,
                            comments_after_group: None,
                            span: (2, 16, 1),
                          },
                          operator: None,
                          comments_after_type: None,
                          span: (2, 16, 1),
                        },
                        comments_before_type: None,
                        comments_after_type: None,
                      }],
                      span: (2, 16, 1),
                    },
                  }),
                  leading_comments: None,
                  trailing_comments: None,
                  span: (2, 17, 1),
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
                    member_key: None,
                    entry_type: Type {
                      type_choices: vec![TypeChoice {
                        type1: Type1 {
                          type2: Type2::Array {
                            group: Group {
                              group_choices: vec![GroupChoice {
                                group_entries: vec![(
                                  GroupEntry::TypeGroupname {
                                    ge: TypeGroupnameEntry {
                                      occur: Some(Occurrence {
                                        occur: Occur::ZeroOrMore((19, 20, 1)),
                                        comments: None,
                                        _a: PhantomData::default(),
                                      }),
                                      name: Identifier {
                                        ident: "directory-entry".into(),
                                        socket: None,
                                        span: (21, 36, 1),
                                      },
                                      generic_args: None,
                                    },
                                    leading_comments: None,
                                    trailing_comments: None,
                                    span: (19, 36, 1),
                                  },
                                  OptionalComma {
                                    optional_comma: false,
                                    trailing_comments: None,
                                    _a: PhantomData::default(),
                                  },
                                )],
                                comments_before_grpchoice: None,
                                span: (19, 36, 1),
                              }],
                              span: (19, 36, 1),
                            },
                            comments_before_group: None,
                            comments_after_group: None,
                            span: (18, 37, 1),
                          },
                          operator: None,
                          comments_after_type: None,
                          span: (18, 37, 1),
                        },
                        comments_before_type: None,
                        comments_after_type: None,
                      }],
                      span: (18, 37, 1),
                    },
                  }),
                  leading_comments: None,
                  trailing_comments: None,
                  span: (18, 37, 1),
                },
                OptionalComma {
                  optional_comma: false,
                  trailing_comments: None,
                  _a: PhantomData::default(),
                },
              ),
            ],
            comments_before_grpchoice: None,
            span: (2, 37, 1),
          }],
          span: (2, 37, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
        span: (0, 39, 1),
      },
      Type2::Map {
        group: Group {
          group_choices: vec![
            GroupChoice {
              group_entries: vec![
                (
                  GroupEntry::TypeGroupname {
                    ge: TypeGroupnameEntry {
                      occur: None,
                      name: Identifier {
                        ident: "int".into(),
                        socket: None,
                        span: (2, 5, 1),
                      },
                      generic_args: None,
                    },
                    leading_comments: None,
                    trailing_comments: None,
                    span: (2, 6, 1),
                  },
                  OptionalComma {
                    optional_comma: true,
                    trailing_comments: None,
                    _a: PhantomData::default(),
                  },
                ),
                (
                  GroupEntry::TypeGroupname {
                    ge: TypeGroupnameEntry {
                      occur: None,
                      name: Identifier {
                        ident: "int".into(),
                        socket: None,
                        span: (7, 10, 1),
                      },
                      generic_args: None,
                    },
                    leading_comments: None,
                    trailing_comments: None,
                    span: (7, 10, 1),
                  },
                  OptionalComma {
                    optional_comma: false,
                    trailing_comments: None,
                    _a: PhantomData::default(),
                  },
                ),
              ],
              comments_before_grpchoice: None,
              span: (2, 10, 1),
            },
            GroupChoice {
              group_entries: vec![
                (
                  GroupEntry::TypeGroupname {
                    ge: TypeGroupnameEntry {
                      occur: None,
                      name: Identifier {
                        ident: "int".into(),
                        socket: None,
                        span: (14, 17, 1),
                      },
                      generic_args: None,
                    },
                    leading_comments: None,
                    trailing_comments: None,
                    span: (14, 18, 1),
                  },
                  OptionalComma {
                    optional_comma: true,
                    trailing_comments: None,
                    _a: PhantomData::default(),
                  },
                ),
                (
                  GroupEntry::TypeGroupname {
                    ge: TypeGroupnameEntry {
                      occur: None,
                      name: Identifier {
                        ident: "tstr".into(),
                        socket: None,
                        span: (19, 23, 1),
                      },
                      generic_args: None,
                    },
                    leading_comments: None,
                    trailing_comments: None,
                    span: (19, 23, 1),
                  },
                  OptionalComma {
                    optional_comma: false,
                    trailing_comments: None,
                    _a: PhantomData::default(),
                  },
                ),
              ],
              comments_before_grpchoice: None,
              span: (14, 23, 1),
            },
          ],
          span: (2, 23, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
        span: (0, 25, 1),
      },
      Type2::Map {
        group: Group {
          group_choices: vec![GroupChoice {
            group_entries: vec![
              (
                GroupEntry::TypeGroupname {
                  ge: TypeGroupnameEntry {
                    occur: None,
                    name: Identifier {
                      ident: "int".into(),
                      socket: None,
                      span: (2, 5, 1),
                    },
                    generic_args: None,
                  },
                  leading_comments: None,
                  trailing_comments: None,
                  span: (2, 6, 1),
                },
                OptionalComma {
                  optional_comma: true,
                  trailing_comments: None,
                  _a: PhantomData::default(),
                },
              ),
              (
                GroupEntry::TypeGroupname {
                  ge: TypeGroupnameEntry {
                    occur: None,
                    name: Identifier {
                      ident: "int".into(),
                      socket: None,
                      span: (7, 10, 1),
                    },
                    generic_args: None,
                  },
                  leading_comments: None,
                  trailing_comments: None,
                  span: (7, 11, 1),
                },
                OptionalComma {
                  optional_comma: true,
                  trailing_comments: None,
                  _a: PhantomData::default(),
                },
              ),
              (
                GroupEntry::TypeGroupname {
                  ge: TypeGroupnameEntry {
                    occur: None,
                    name: Identifier {
                      ident: "int".into(),
                      socket: None,
                      span: (12, 15, 1),
                    },
                    generic_args: None,
                  },
                  leading_comments: None,
                  trailing_comments: None,
                  span: (12, 16, 1),
                },
                OptionalComma {
                  optional_comma: true,
                  trailing_comments: None,
                  _a: PhantomData::default(),
                },
              ),
              (
                GroupEntry::TypeGroupname {
                  ge: TypeGroupnameEntry {
                    occur: None,
                    name: Identifier {
                      ident: "tstr".into(),
                      socket: None,
                      span: (17, 21, 1),
                    },
                    generic_args: None,
                  },
                  leading_comments: None,
                  trailing_comments: None,
                  span: (17, 21, 1),
                },
                OptionalComma {
                  optional_comma: false,
                  trailing_comments: None,
                  _a: PhantomData::default(),
                },
              ),
            ],
            comments_before_grpchoice: None,
            span: (2, 21, 1),
          }],
          span: (2, 21, 1),
        },
        comments_before_group: None,
        comments_after_group: None,
        span: (0, 23, 1),
      },
    ];

    for (idx, expected_output) in expected_ouputs.iter().enumerate() {
      let l = Lexer::new(inputs[idx]);
      let t2 = Parser::new(inputs[idx], Box::new(l.iter()))?.parse_type2()?;

      assert_eq!(&t2, expected_output);
      assert_eq!(t2.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_grpent() -> Result<()> {
    let inputs = [
      r#"* type1 ^ => "value""#,
      r#"type1: type2"#,
      r#"typename"#,
      r#"? 0: addrdistr"#,
      r#"0: finite_set<transaction_input>"#,
      r#"* [credential] => coin"#,
    ];

    let expected_outputs = [
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: Some(Occurrence {
            occur: Occur::ZeroOrMore((0, 1, 1)),
            comments: None,
            _a: PhantomData::default(),
          }),
          member_key: Some(MemberKey::Type1 {
            t1: Box::from(Type1 {
              type2: Type2::Typename {
                ident: Identifier {
                  ident: "type1".into(),
                  socket: None,
                  span: (2, 7, 1),
                },
                generic_args: None,
                span: (2, 7, 1),
              },
              operator: None,
              comments_after_type: None,
              span: (2, 7, 1),
            }),
            is_cut: true,
            comments_before_cut: None,
            comments_after_cut: None,
            comments_after_arrowmap: None,
            span: (2, 12, 1),
          }),
          entry_type: Type {
            type_choices: vec![TypeChoice {
              type1: Type1 {
                type2: Type2::TextValue {
                  value: "value".into(),
                  span: (13, 20, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (13, 20, 1),
              },
              comments_before_type: None,
              comments_after_type: None,
            }],
            span: (13, 20, 1),
          },
        }),
        leading_comments: None,
        trailing_comments: None,
        span: (0, 20, 1),
      },
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: None,
          member_key: Some(MemberKey::Bareword {
            ident: Identifier {
              ident: "type1".into(),
              socket: None,
              span: (0, 5, 1),
            },
            comments: None,
            comments_after_colon: None,
            span: (0, 6, 1),
          }),
          entry_type: Type {
            type_choices: vec![TypeChoice {
              type1: Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "type2".into(),
                    socket: None,
                    span: (7, 12, 1),
                  },
                  generic_args: None,
                  span: (7, 12, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (7, 12, 1),
              },
              comments_before_type: None,
              comments_after_type: None,
            }],
            span: (7, 12, 1),
          },
        }),
        leading_comments: None,
        trailing_comments: None,
        span: (0, 12, 1),
      },
      GroupEntry::TypeGroupname {
        ge: TypeGroupnameEntry {
          occur: None,
          name: Identifier {
            ident: "typename".into(),
            socket: None,
            span: (0, 8, 1),
          },
          generic_args: None,
        },
        leading_comments: None,
        trailing_comments: None,
        span: (0, 8, 1),
      },
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: Some(Occurrence {
            occur: Occur::Optional((0, 1, 1)),
            comments: None,
            _a: PhantomData::default(),
          }),
          member_key: Some(MemberKey::Value {
            value: token::Value::UINT(0),
            comments: None,
            comments_after_colon: None,
            span: (2, 4, 1),
          }),
          entry_type: Type {
            type_choices: vec![TypeChoice {
              type1: Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "addrdistr".into(),
                    socket: None,
                    span: (5, 14, 1),
                  },
                  generic_args: None,
                  span: (5, 14, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (5, 14, 1),
              },
              comments_before_type: None,
              comments_after_type: None,
            }],
            span: (5, 14, 1),
          },
        }),
        leading_comments: None,
        trailing_comments: None,
        span: (0, 14, 1),
      },
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: None,
          member_key: Some(MemberKey::Value {
            value: token::Value::UINT(0),
            comments: None,
            comments_after_colon: None,
            span: (0, 2, 1),
          }),
          entry_type: Type {
            type_choices: vec![TypeChoice {
              type1: Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "finite_set".into(),
                    socket: None,
                    span: (3, 13, 1),
                  },
                  generic_args: Some(GenericArgs {
                    args: vec![GenericArg {
                      arg: Box::from(Type1 {
                        type2: Type2::Typename {
                          ident: Identifier {
                            ident: "transaction_input".into(),
                            socket: None,
                            span: (14, 31, 1),
                          },
                          generic_args: None,
                          span: (14, 31, 1),
                        },
                        operator: None,
                        comments_after_type: None,
                        span: (14, 31, 1),
                      }),
                      comments_before_type: None,
                      comments_after_type: None,
                    }],
                    span: (13, 32, 1),
                  }),

                  span: (3, 32, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (3, 32, 1),
              },
              comments_before_type: None,
              comments_after_type: None,
            }],
            span: (3, 32, 1),
          },
        }),
        leading_comments: None,
        trailing_comments: None,
        span: (0, 32, 1),
      },
      GroupEntry::ValueMemberKey {
        ge: Box::from(ValueMemberKeyEntry {
          occur: Some(Occurrence {
            occur: Occur::ZeroOrMore((0, 1, 1)),
            comments: None,
            _a: PhantomData::default(),
          }),
          member_key: Some(MemberKey::Type1 {
            t1: Box::from(Type1 {
              type2: Type2::Array {
                group: Group {
                  group_choices: vec![GroupChoice {
                    group_entries: vec![(
                      GroupEntry::TypeGroupname {
                        ge: TypeGroupnameEntry {
                          occur: None,
                          name: Identifier {
                            ident: "credential".into(),
                            socket: None,
                            span: (3, 13, 1),
                          },
                          generic_args: None,
                        },
                        leading_comments: None,
                        trailing_comments: None,
                        span: (3, 13, 1),
                      },
                      OptionalComma {
                        optional_comma: false,
                        trailing_comments: None,
                        _a: PhantomData::default(),
                      },
                    )],
                    comments_before_grpchoice: None,
                    span: (3, 13, 1),
                  }],
                  span: (3, 13, 1),
                },
                comments_before_group: None,
                comments_after_group: None,
                span: (2, 14, 1),
              },
              operator: None,
              comments_after_type: None,
              span: (2, 14, 1),
            }),
            is_cut: false,
            comments_before_cut: None,
            comments_after_cut: None,
            comments_after_arrowmap: None,
            span: (2, 22, 1),
          }),
          entry_type: Type {
            type_choices: vec![TypeChoice {
              type1: Type1 {
                type2: Type2::Typename {
                  ident: Identifier {
                    ident: "coin".into(),
                    socket: None,
                    span: (18, 22, 1),
                  },
                  generic_args: None,
                  span: (18, 22, 1),
                },
                operator: None,
                comments_after_type: None,
                span: (18, 22, 1),
              },
              comments_before_type: None,
              comments_after_type: None,
            }],
            span: (18, 22, 1),
          },
        }),
        leading_comments: None,
        trailing_comments: None,
        span: (0, 22, 1),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(inputs[idx]);
      let grpent = Parser::new(inputs[idx], Box::new(l.iter()))?.parse_grpent(false)?;

      assert_eq!(&grpent, expected_output);
      assert_eq!(grpent.to_string(), expected_output.to_string());
    }

    Ok(())
  }

  #[test]
  fn verify_memberkey() -> Result<()> {
    let inputs = [
      r#"type1 =>"#,
      r#"( "mytype1" / int ) ^ =>"#,
      r#"mybareword:"#,
      r#"my..bareword:"#,
      r#""myvalue": "#,
      r#"0:"#,
    ];

    let expected_outputs = [
      MemberKey::Type1 {
        t1: Box::from(Type1 {
          type2: Type2::Typename {
            ident: Identifier {
              ident: "type1".into(),
              socket: None,
              span: (0, 5, 1),
            },
            generic_args: None,
            span: (0, 5, 1),
          },
          operator: None,
          comments_after_type: None,
          span: (0, 5, 1),
        }),
        is_cut: false,
        comments_before_cut: None,
        comments_after_cut: None,
        comments_after_arrowmap: None,
        span: (0, 8, 1),
      },
      MemberKey::Type1 {
        t1: Box::from(Type1 {
          type2: Type2::ParenthesizedType {
            pt: Type {
              type_choices: vec![
                TypeChoice {
                  type1: Type1 {
                    type2: Type2::TextValue {
                      value: "mytype1".into(),
                      span: (2, 11, 1),
                    },
                    operator: None,
                    span: (2, 11, 1),
                    comments_after_type: None,
                  },
                  comments_after_type: None,
                  comments_before_type: None,
                },
                TypeChoice {
                  type1: Type1 {
                    type2: Type2::Typename {
                      ident: Identifier {
                        ident: "int",
                        span: (14, 17, 1),
                        socket: None,
                      },
                      span: (14, 17, 1),
                      generic_args: None,
                    },
                    span: (14, 17, 1),
                    comments_after_type: None,
                    operator: None,
                  },
                  comments_before_type: None,
                  comments_after_type: None,
                },
              ],
              span: (2, 17, 1),
            },
            span: (0, 19, 1),
            comments_before_type: None,
            comments_after_type: None,
          },
          operator: None,
          comments_after_type: None,
          span: (0, 19, 1),
        }),
        is_cut: true,
        comments_before_cut: None,
        comments_after_cut: None,
        comments_after_arrowmap: None,
        span: (0, 24, 1),
      },
      MemberKey::Bareword {
        ident: Identifier {
          ident: "mybareword".into(),
          socket: None,
          span: (0, 10, 1),
        },
        comments: None,
        comments_after_colon: None,
        span: (0, 11, 1),
      },
      MemberKey::Bareword {
        ident: Identifier {
          ident: "my..bareword".into(),
          socket: None,
          span: (0, 12, 1),
        },
        comments: None,
        comments_after_colon: None,
        span: (0, 13, 1),
      },
      MemberKey::Value {
        value: token::Value::TEXT("myvalue".into()),
        comments: None,
        comments_after_colon: None,
        span: (0, 10, 1),
      },
      MemberKey::Value {
        value: token::Value::UINT(0),
        comments: None,
        comments_after_colon: None,
        span: (0, 2, 1),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(inputs[idx]);
      let mk = Parser::new(inputs[idx], Box::new(l.iter()))?.parse_memberkey(false)?;

      if let Some(mk) = mk {
        assert_eq!(&mk, expected_output);
        assert_eq!(mk.to_string(), expected_output.to_string());
      }
    }

    Ok(())
  }

  #[test]
  fn verify_occur() -> Result<()> {
    let inputs = [r#"1*3"#, r#"*"#, r#"+"#, r#"5*"#, r#"*3"#, r#"?"#];

    let expected_outputs = [
      Occurrence {
        occur: Occur::Exact {
          lower: Some(1),
          upper: Some(3),
          span: (0, 3, 1),
        },
        comments: None,
        _a: PhantomData::default(),
      },
      Occurrence {
        occur: Occur::ZeroOrMore((0, 1, 1)),
        comments: None,
        _a: PhantomData::default(),
      },
      Occurrence {
        occur: Occur::OneOrMore((0, 1, 1)),
        comments: None,
        _a: PhantomData::default(),
      },
      Occurrence {
        occur: Occur::Exact {
          lower: Some(5),
          upper: None,
          span: (0, 2, 1),
        },
        comments: None,
        _a: PhantomData::default(),
      },
      Occurrence {
        occur: Occur::Exact {
          lower: None,
          upper: Some(3),
          span: (0, 2, 1),
        },
        comments: None,
        _a: PhantomData::default(),
      },
      Occurrence {
        occur: Occur::Optional((0, 1, 1)),
        comments: None,
        _a: PhantomData::default(),
      },
    ];

    for (idx, expected_output) in expected_outputs.iter().enumerate() {
      let l = Lexer::new(inputs[idx]);
      let o = Parser::new(inputs[idx], Box::new(l.iter()))?.parse_occur(false)?;

      if let Some(o) = o {
        assert_eq!(&o, expected_output);
        assert_eq!(o.to_string(), expected_output.to_string());
      }
    }

    Ok(())
  }
}
