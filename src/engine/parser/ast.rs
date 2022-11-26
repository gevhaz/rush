use std::ops::RangeInclusive;

use crate::engine::parser::Token;

use super::util;

#[derive(Debug, PartialEq)]
pub enum CommandType {
    Single(Command),
    Pipeline(Vec<Command>),
}

#[derive(Debug, PartialEq)]
pub struct Command {
    name: Word,
    prefix: Vec<Meta>,
    suffix: Vec<Meta>,
}

#[derive(Debug, PartialEq)]
pub struct Word {
    name: String,
    expansions: Vec<Expansion>,
}

impl Word {
    fn new(name: impl ToString, expansions: Vec<Expansion>) -> Self {
        Self {
            name: name.to_string(),
            expansions,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Meta {
    Redirect(Redirect),
    Word(Word),
    Assignment(Word, Word),
}

#[derive(Debug, PartialEq)]
pub enum Redirect {
    Output { from: Option<String>, to: String },
    Input { to: String },
}

#[derive(Debug, PartialEq)]
pub enum Expansion {
    Parameter {
        name: String,
        range: RangeInclusive<usize>,
    },
    Command {
        ast: AST,
        range: RangeInclusive<usize>,
    },
    Glob {
        pattern: String,
        recursive: bool,
        range: RangeInclusive<usize>,
    },
    Tilde {
        index: usize,
    },
}

#[derive(Debug, PartialEq)]
pub struct AST {
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

    pub fn commands(&self) -> &[CommandType] {
        &self.commands
    }
}

pub fn parse(line: String) -> AST {
    let tokens = super::lex(line);
    parse_tokens(tokens)
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
            | Token::DoubleQuotedString(_)) => match parse_meta(token) {
                Some(word @ Meta::Word(_)) => {
                    if name.is_none() {
                        name = Some(word);
                    } else {
                        suffix.push(word);
                    }
                }
                Some(Meta::Assignment(dest, var)) => {
                    if name.is_none() {
                        prefix.push(Meta::Assignment(dest, var));
                    } else {
                        suffix.push(Meta::Assignment(dest, var));
                    }
                }
                Some(meta) => panic!("disallowed type: {:?}", meta),
                None => {}
            },

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

enum Expand {
    All,
    VariablesAndCommands,
    None,
}

fn parse_word(s: impl AsRef<str>, expand: Expand) -> Word {
    if let Expand::None = expand {
        return Word::new(s.as_ref(), Vec::new());
    }

    let s = s.as_ref();
    let mut chars = s.chars().peekable();
    let mut expansions = Vec::new();
    let mut index = 0;

    let mut prev_char = None;

    while let Some(ch) = chars.next() {
        match ch {
            ' ' => {}

            // should be guarded by !matches!(expand, Expand::None), but since
            // we have an early return specifically for Expand::None, it is not
            // needed.
            '$' => match chars.peek() {
                Some(&c) if util::is_valid_first_character_of_expansion(c) => {
                    let c = chars.next().unwrap();

                    let mut var = c.to_string();
                    let start_index = index;

                    while let Some(&c) = chars.peek() {
                        if !util::is_valid_first_character_of_expansion(c) {
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
                    let ast = parse(subcmd);
                    expansions.push(Expansion::Command {
                        ast,
                        range: start_index..=index,
                    });
                }

                c => panic!("got unexpected: {c:?}"),
            },

            '*' if matches!(expand, Expand::All) => {
                let mut recursive = false;
                let mut pattern = '*'.to_string();
                let start_index = index;

                while let Some(&c) = chars.peek() {
                    match c {
                        '*' => {
                            chars.next();
                            index += 1;
                            recursive = true;
                            pattern.push('*');
                        }

                        c => {
                            if " /".contains(c) {
                                break;
                            }
                            pattern.push(c);
                            chars.next();
                            index += 1;
                        }
                    }
                }

                expansions.push(Expansion::Glob {
                    pattern,
                    recursive,
                    range: start_index..=index,
                });
            }

            '~' if matches!(expand, Expand::All) && matches!(prev_char, Some(' ') | None) => {
                match chars.peek() {
                    Some(' ') | Some('/') | None => expansions.push(Expansion::Tilde { index }),
                    _ => {}
                }
                index += 1;
            }

            _ => {}
        }
        index += 1;
        prev_char = Some(ch);
    }

    return Word::new(s, expansions);
}

fn parse_meta(token: &Token) -> Option<Meta> {
    match token {
        Token::String(s) => {
            let item = match s.split_once('=') {
                Some((var, val)) => {
                    let var_word = parse_word(var, Expand::None);
                    let val_word = parse_word(val, Expand::All);
                    Meta::Assignment(var_word, val_word)
                }
                None => Meta::Word(parse_word(s, Expand::All)),
            };

            return Some(item);
        }

        Token::SingleQuotedString(s) => {
            let word = parse_word(s, Expand::None);
            Some(Meta::Word(word))
        }

        Token::DoubleQuotedString(s) => {
            let word = parse_word(s, Expand::VariablesAndCommands);
            Some(Meta::Word(word))
        }

        Token::RedirectInput(s) => {
            return Some(Meta::Redirect(Redirect::Input { to: s.to_string() }));
        }

        Token::RedirectOutput(from, to) => {
            let from = match from {
                Some(s) => Some(s),
                None => None,
            };
            let to = to.to_string();
            return Some(Meta::Redirect(Redirect::Output {
                from: from.cloned(),
                to,
            }));
        }

        _ => unreachable!(),
    }
}

fn parse_tokens(tokens: Vec<Token>) -> AST {
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

    #[test]
    fn basic_parsing() {
        let input = "2>&1 echo hello world | lolcat -n;".to_string();
        let ast = parse(input);

        println!("{:#?}", &ast);

        assert_eq!(
            AST {
                commands: vec![CommandType::Pipeline(vec![
                    Command {
                        name: Word::new("echo", vec![]),
                        prefix: vec![Meta::Redirect(Redirect::Output {
                            from: Some("2".into()),
                            to: "&1".into()
                        }),],
                        suffix: vec![
                            Meta::Word(Word::new("hello", vec![])),
                            Meta::Word(Word::new("world", vec![])),
                        ],
                    },
                    Command {
                        name: Word::new("lolcat", vec![]),
                        prefix: vec![],
                        suffix: vec![Meta::Word(Word::new("-n", vec![])),],
                    }
                ]),],
            },
            ast
        );
    }

    #[test]
    fn asterisk_expansion_parsing() {
        let input = "echo **/*.rs".to_string();
        let ast = parse(input);

        let expected = AST {
            commands: vec![CommandType::Single(Command {
                name: Word::new("echo", vec![]),
                prefix: Vec::new(),
                suffix: vec![Meta::Word(Word::new(
                    "**/*.rs",
                    vec![
                        Expansion::Glob {
                            pattern: "**".to_string(),
                            recursive: true,
                            range: 0..=1,
                        },
                        Expansion::Glob {
                            pattern: "*.rs".to_string(),
                            recursive: false,
                            range: 3..=6,
                        },
                    ],
                ))],
            })],
        };
        assert_eq!(expected, ast);
    }

    #[test]
    fn variable_expansion_parsing() {
        let input = "echo \"yo $foo $A\"".to_string();
        let ast = parse(input);

        assert_eq!(
            AST {
                commands: vec![CommandType::Single(Command {
                    name: Word::new("echo", vec![]),
                    prefix: Vec::new(),
                    suffix: vec![Meta::Word(Word::new(
                        "yo $foo $A",
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
    fn single_quote_doesnt_expand_parsing() {
        let input = "echo '** $foo'".to_string();
        let ast = parse(input);

        let expected = AST {
            commands: vec![CommandType::Single(Command {
                name: Word::new("echo", vec![]),
                prefix: vec![],
                suffix: vec![Meta::Word(Word::new("** $foo", vec![]))],
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    fn nested_pipeline_parsing() {
        let input = r#"echo "I \"am\": $(whoami | rev | grep -o -v foo)" | less"#.to_string();
        let ast = parse(input);

        let expected = AST {
            commands: vec![CommandType::Pipeline(vec![
                Command {
                    name: Word::new("echo", vec![]),
                    prefix: Vec::new(),
                    suffix: vec![Meta::Word(Word::new(
                        "I \"am\": $(whoami | rev | grep -o -v foo)",
                        vec![Expansion::Command {
                            range: 8..=39,
                            ast: AST {
                                commands: vec![CommandType::Pipeline(vec![
                                    Command {
                                        name: Word::new("whoami", vec![]),
                                        prefix: Vec::new(),
                                        suffix: Vec::new(),
                                    },
                                    Command {
                                        name: Word::new("rev", vec![]),
                                        prefix: Vec::new(),
                                        suffix: Vec::new(),
                                    },
                                    Command {
                                        name: Word::new("grep", vec![]),
                                        prefix: Vec::new(),
                                        suffix: vec![
                                            Meta::Word(Word::new("-o", vec![])),
                                            Meta::Word(Word::new("-v", vec![])),
                                            Meta::Word(Word::new("foo", vec![])),
                                        ],
                                    },
                                ])],
                            },
                        }],
                    ))],
                },
                Command {
                    name: Word::new("less", vec![]),
                    prefix: Vec::new(),
                    suffix: Vec::new(),
                },
            ])],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    fn complicated_parsing() {
        let input = r#"CMD=exec=async 2>&1 grep ": $(whoami)" ~/.cache/ | xargs -I {} echo "$CMD: {}" >foo.log"#.to_string();
        let ast = parse(input);

        let expected = AST {
            commands: vec![CommandType::Pipeline(vec![
                Command {
                    name: Word::new("grep", vec![]),
                    prefix: vec![
                        Meta::Assignment(Word::new("CMD", vec![]), Word::new("exec=async", vec![])),
                        Meta::Redirect(Redirect::Output {
                            from: Some("2".into()),
                            to: "&1".into(),
                        }),
                    ],
                    suffix: vec![
                        Meta::Word(Word::new(
                            ": $(whoami)",
                            vec![Expansion::Command {
                                range: 2..=10,
                                ast: AST {
                                    commands: vec![CommandType::Single(Command {
                                        name: Word::new("whoami", vec![]),
                                        prefix: Vec::new(),
                                        suffix: Vec::new(),
                                    })],
                                },
                            }],
                        )),
                        Meta::Word(Word::new("~/.cache/", vec![])),
                    ],
                },
                Command {
                    name: Word::new("xargs", vec![]),
                    prefix: Vec::new(),
                    suffix: vec![
                        Meta::Word(Word::new("-I", vec![])),
                        Meta::Word(Word::new("{}", vec![])),
                        Meta::Word(Word::new("echo", vec![])),
                        Meta::Word(Word::new(
                            "$CMD: {}",
                            vec![Expansion::Parameter {
                                name: "CMD".into(),
                                range: 0..=3,
                            }],
                        )),
                        Meta::Redirect(Redirect::Output {
                            from: None,
                            to: "foo.log".into(),
                        }),
                    ],
                },
            ])],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    fn basic_command_expansion_parsing() {
        let input = r#"echo "bat: $(cat /sys/class/power_supply/BAT0/capacity)""#.to_string();
        let ast = parse(input);

        let expected = AST {
            commands: vec![CommandType::Single(Command {
                name: Word::new("echo", vec![]),
                prefix: vec![],
                suffix: vec![Meta::Word(Word::new(
                    "bat: $(cat /sys/class/power_supply/BAT0/capacity)",
                    vec![Expansion::Command {
                        range: 5..=48,
                        ast: AST {
                            commands: vec![CommandType::Single(Command {
                                name: Word::new("cat", vec![]),
                                prefix: vec![],
                                suffix: vec![Meta::Word(Word::new(
                                    "/sys/class/power_supply/BAT0/capacity",
                                    vec![],
                                ))],
                            })],
                        },
                    }],
                ))],
            })],
        };

        assert_eq!(expected, ast);
    }

    #[test]
    fn tilde_expansion_parsing() {
        let input = "ls ~ ~/ ~/foo".to_string();
        let ast = parse(input);

        let expected = AST {
            commands: vec![CommandType::Single(Command {
                name: Word::new("ls", vec![]),
                prefix: vec![],
                suffix: vec![
                    Meta::Word(Word::new("~", vec![Expansion::Tilde { index: 0 }])),
                    Meta::Word(Word::new("~/", vec![Expansion::Tilde { index: 0 }])),
                    Meta::Word(Word::new("~/foo", vec![Expansion::Tilde { index: 0 }])),
                ],
            })],
        };

        assert_eq!(expected, ast);

        let input = "ls foo~ bar/~ ./~ ~% ~baz".to_string();
        let ast = parse(input);

        let expected = AST {
            commands: vec![CommandType::Single(Command {
                name: Word::new("ls", vec![]),
                prefix: vec![],
                suffix: vec![
                    Meta::Word(Word::new("foo~", vec![])),
                    Meta::Word(Word::new("bar/~", vec![])),
                    Meta::Word(Word::new("./~", vec![])),
                    Meta::Word(Word::new("~%", vec![])),
                    Meta::Word(Word::new("~baz", vec![])),
                ],
            })],
        };

        assert_eq!(expected, ast);
    }

    // FIXME: this probably requires (major?) changes to the lexing.
    //        it's making me wonder if the lexing part should be
    //        removed entirely, since lexing a POSIX shell-ish language
    //        seems really difficult
    // #[test]
    // fn nested_quotes_in_command_expansion_parsing() {
    //     let input = r#"echo "bat: $(cat "/sys/class/power_supply/BAT0/capacity")""#.to_string();
    //     let ast = parse(input);

    //     let expected = AST {
    //         commands: vec![CommandType::Single(Command {
    //             name: Word::new("echo", vec![]),
    //             prefix: vec![],
    //             suffix: vec![Meta::Word(Word::new(
    //                 "bat: $(cat \"/sys/class/power_supply/BAT0/capacity\")",
    //                 vec![Expansion::Command {
    //                     range: 5..=50,
    //                     ast: AST { commands: vec![
    //                         CommandType::Single(Command {
    //                             name: Word::new("cat", vec![]),
    //                             prefix: vec![],
    //                             suffix: vec![
    //                                 Meta::Word(Word::new("/sys/class/power_supply/BAT0/capacity", vec![])),
    //                             ],
    //                         }),
    //                     ] },
    //                 }],
    //             ))],
    //         })],
    //     };

    //     assert_eq!(expected, ast);
    // }
}
