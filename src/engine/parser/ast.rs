#![allow(dead_code)]

use crate::engine::parser::Token;

#[derive(Debug, PartialEq)]
enum CommandType {
    Single(Command),
    Pipeline(Vec<Command>),
}

#[derive(Debug, PartialEq)]
struct Command {
    name: Word,
    prefix: Vec<Meta>,
    suffix: Vec<Meta>,
}

#[derive(Debug, PartialEq)]
struct Word(String, Vec<Expansion>);

impl Word {
    fn new(name: String) -> Self {
        Self(name, Vec::new())
    }
}

#[derive(Debug, PartialEq)]
enum Meta {
    Redirect(Redirect),
    Word(Word),
    Assignment(Word, Word),
}

#[derive(Debug, PartialEq)]
enum Redirect {
    Output(Option<String>, String),
    Input(String),
}

#[derive(Debug, PartialEq)]
enum Expansion {
    Parameter {
        name: String,
        range: std::ops::RangeInclusive<usize>,
    },
    Command {
        ast: AST,
        range: std::ops::RangeInclusive<usize>,
    },
}

#[derive(Debug, PartialEq)]
struct AST {
    commands: Vec<CommandType>,
}

impl AST {
    pub fn new() -> Self {
        Self {
            commands: Default::default(),
        }
    }

    pub fn add_command(&mut self, command: CommandType) {
        self.commands.push(command);
    }
}

fn parse_expr(tokens: &[Token]) {
    let mut tokens = tokens.iter().peekable();

    while let Some(token) = tokens.next() {
        match token {
            // Token::Assignment(dest, var) => todo!(),
            Token::String(s) => todo!(),
            Token::SingleQuotedString(s) => todo!(),
            Token::DoubleQuotedString(s) => todo!(),
            Token::RedirectInput(to) => todo!(),
            Token::RedirectOutput(from, to) => todo!(),
            // Token::LParen => todo!(),
            // Token::RParen => todo!(),
            // Token::LBrace => todo!(),
            // Token::RBrace => todo!(),
            Token::Pipe => todo!(),
            Token::And => todo!(),
            Token::Or => todo!(),
            Token::Colon => todo!(),
            Token::Semicolon => todo!(),
            Token::Ampersand => todo!(),
        }
    }
}

fn parse_command(tokens: &[Token]) -> Option<Command> {
    let mut tokens = tokens.into_iter().peekable();

    let mut name = None;
    let mut prefix = Vec::new();
    let mut suffix = Vec::new();

    while let Some(token) = tokens.next() {
        match token {
            Token::Colon => { /* noop */ }

            token @ (Token::String(_)
            | Token::SingleQuotedString(_)
            | Token::DoubleQuotedString(_)) => {
                match parse_meta(token) {
                    Some(word @ Meta::Word(_)) => {
                        if name.is_none() {
                            name = Some(word);
                        } else {
                            suffix.push(word);
                        }
                    },
                    Some(Meta::Assignment(dest, var)) => {
                        if name.is_none() {
                            prefix.push(Meta::Assignment(dest, var));
                        } else {
                            suffix.push(Meta::Assignment(dest, var));
                        }
                    },
                    Some(meta) => panic!("disallowed type: {:?}", meta),
                    None => {},
                }
            }

            token @ Token::RedirectOutput(_, _) => {
                if let Some(redirect) = parse_meta(token) {
                    match name {
                        Some(_) => suffix.push(redirect),
                        None => prefix.push(redirect),
                    }
                }
            }

            Token::RedirectInput(_) => todo!(),

            // Token::LParen => todo!("( subshells are not yet implemented"),
            // Token::RParen => todo!(") subshells are not yet implemented"),

            // Token::LBrace => todo!("{{ command grouping is not yet implemented"),
            // Token::RBrace => todo!("}} command grouping is not yet implemented"),

            Token::And => todo!("AND is not yet implemented"),
            Token::Or => todo!("OR is not yet implemented"),

            Token::Ampersand => todo!("asynchronous execution is not yet implemented"),

            Token::Semicolon => unreachable!("semicolons should have been found already"),
            Token::Pipe => unreachable!("pipes should have been found already"),
        }
    }

    if let Some(Meta::Word(name)) = name {
        Some(Command {
            name,
            prefix,
            suffix,
        })
    } else {
        eprintln!("{name:?}");
        None
    }
}

fn parse_meta(token: &Token) -> Option<Meta> {
    match token {
        Token::String(s) => {
            let item = match s.split_once('=') {
                Some((var, val)) => {
                    Meta::Assignment(Word(var.to_string(), vec![]), Word(val.to_string(), vec![]))
                }
                None => Meta::Word(Word(s.to_string(), vec![])),
            };

            return Some(item);
        }

        Token::SingleQuotedString(s) => {
            return Some(Meta::Word(Word(s.to_string(), vec![])));
        }

        Token::DoubleQuotedString(s) => {
            let mut chars = s.chars().peekable();
            let mut expansions = Vec::new();
            let mut index = 0;

            while let Some(ch) = chars.next() {
                match ch {
                    ' ' => {}
                    '$' => match chars.peek() {
                        Some(&c) if c.is_alphanumeric() => {
                            let c = chars.next().unwrap();

                            let mut var = c.to_string();
                            let start_index = index;

                            while let Some(&c) = chars.peek() {
                                if !c.is_alphanumeric() {
                                    break;
                                }
                                var.push(chars.next().unwrap());
                                index += 1;
                            }

                            index += 1;

                            expansions.push(Expansion::Parameter {
                                name: var,
                                range: start_index..=index,
                            });
                        }

                        Some(&'(') => {
                            let start_index = index;
                            chars.next();
                            let mut subcmd = String::new();
                            while let Some(next) = chars.next() {
                                index += 1;
                                if next == ')' {
                                    break;
                                }
                                subcmd.push(next);
                            }
                            index += 1;
                            let ast = parse(super::lex(subcmd));
                            expansions.push(Expansion::Command {
                                ast,
                                range: start_index..=index,
                            });
                        }

                        c => panic!("got unexpected: {c:?}"),
                    },

                    _ => {}
                }
                index += 1;
            }

            return Some(Meta::Word(Word(s.to_string(), expansions)));
        }

        Token::RedirectInput(s) => {
            return Some(Meta::Redirect(Redirect::Input(s.to_string())));
        }

        Token::RedirectOutput(from, to) => {
            let from = match from {
                Some(s) => Some(s),
                None => None,
            };
            let to = to.to_string();
            return Some(Meta::Redirect(Redirect::Output(from.cloned(), to)));
        }

        _ => unreachable!(),
    }
}

fn parse(tokens: Vec<Token>) -> AST {
    // Split tokens by semicolons to get list of commands,
    // then each command by pipe to get pipeline in command
    let commands = tokens
        .split(|t| matches!(t, Token::Semicolon))
        .map(|tokens| {
            tokens
                .split(|t| matches!(t, Token::Pipe))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut ast = AST {
        commands: Vec::with_capacity(commands.len()),
    };

    for pipeline in commands {
        match &pipeline[..] {
            &[cmd] if cmd.len() > 0 => {
                if let Some(cmd) = parse_command(cmd) {
                    ast.add_command(CommandType::Single(cmd));
                } else {
                    panic!("could not parse command");
                    // FIXME: syntax error?
                }
            }

            cmds => {
                let mut commands = Vec::new();

                for &command in cmds {
                    if command.len() == 0 {
                        continue;
                    }

                    if let Some(cmd) = parse_command(command) {
                        commands.push(cmd);
                    } else {
                        // FIXME: syntax error?
                        panic!("could not parse command");
                    }
                }

                if commands.len() > 0 {
                    ast.add_command(CommandType::Pipeline(commands));
                }
            }
        };
    }

    ast
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::parser::lex;

    #[test]
    fn basic_parsing() {
        let input = "2>&1 echo hello world | lolcat -n;".to_string();
        let tokens = lex(input);
        let ast = parse(tokens);

        println!("{:#?}", &ast);

        assert_eq!(
            AST {
                commands: vec![CommandType::Pipeline(vec![
                    Command {
                        name: Word("echo".to_string(), vec![]),
                        prefix: vec![Meta::Redirect(Redirect::Output(
                            Some("2".into()),
                            "&1".into()
                        )),],
                        suffix: vec![
                            Meta::Word(Word("hello".into(), vec![])),
                            Meta::Word(Word("world".into(), vec![])),
                        ],
                    },
                    Command {
                        name: Word("lolcat".to_string(), vec![]),
                        prefix: vec![],
                        suffix: vec![Meta::Word(Word("-n".into(), vec![])),],
                    }
                ]),],
            },
            ast
        );
    }

    #[test]
    fn variable_expansion_parsing() {
        let input = "echo \"yo $foo $A\"".to_string();
        let tokens = lex(input);
        let ast = parse(tokens);

        assert_eq!(
            AST {
                commands: vec![CommandType::Single(Command {
                    name: Word("echo".to_string(), vec![]),
                    prefix: Vec::new(),
                    suffix: vec![Meta::Word(Word(
                        "yo $foo $A".to_string(),
                        vec![
                            Expansion::Parameter {
                                name: "foo".to_string(),
                                range: 3..=6,
                            },
                            Expansion::Parameter {
                                name: "A".to_string(),
                                range: 8..=9,
                            },
                        ],
                    )),],
                })],
            },
            ast
        );
    }

    #[test]
    fn complicated_parsing() {
        let input = r#"CMD=exec=async 2>&1 grep ": $(whoami)" ~/.cache/ | xargs -I {} echo "$CMD: {}" >foo.log"#.to_string();
        let tokens = lex(input);
        let ast = parse(tokens);

        let expected = AST {
            commands: vec![
                CommandType::Pipeline(vec![
                    Command {
                        name: Word("grep".into(), vec![]),
                        prefix: vec![
                            Meta::Assignment(Word("CMD".into(), vec![]), Word("exec=async".into(), vec![])),
                            Meta::Redirect(Redirect::Output(Some("2".into()), "&1".into())),
                        ],
                        suffix: vec![
                            Meta::Word(Word(": $(whoami)".into(), vec![
                                Expansion::Command {
                                    range: 2..=10,
                                    ast: AST {
                                        commands: vec![
                                            CommandType::Single(Command {
                                                name: Word("whoami".into(), vec![]),
                                                prefix: Vec::new(),
                                                suffix: Vec::new(),
                                            }),
                                        ],
                                    },
                                },
                            ])),
                            Meta::Word(Word("~/.cache/".into(), vec![])),
                        ],
                    },
                    Command {
                        name: Word("xargs".into(), vec![]),
                        prefix: Vec::new(),
                        suffix: vec![
                            Meta::Word(Word("-I".into(), vec![])),
                            Meta::Word(Word("{}".into(), vec![])),
                            Meta::Word(Word("echo".into(), vec![])),
                            Meta::Word(Word("$CMD: {}".into(), vec![
                                Expansion::Parameter {
                                    name: "CMD".into(),
                                    range: 0..=3,
                                },
                            ])),
                            Meta::Redirect(Redirect::Output(None, "foo.log".into())),
                        ],
                    },
                ],),
            ],
        };

        assert_eq!(expected, ast);
    }
}
