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
struct Word(String);
// struct Word {
// name: String,
// should_expand: bool,
// expansions: Vec<Expansion>,
// }

// impl Word {
//     fn new(name: String, should_expand: bool) -> Self {
//         if should_expand {
//             for index in name.match_indices(|c| c == '$') {

//             }
//         }

//         Self {
//             name,
//             should_expand,
//             expansions: Vec::new(),
//         }
//     }
// }

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
    Parameter(ParameterExpansion),
    Command(CommandExpansion),
}

#[derive(Debug, PartialEq)]
struct ParameterExpansion {
    range: std::ops::Range<usize>,
}

#[derive(Debug, PartialEq)]
struct CommandExpansion {
    range: std::ops::Range<usize>,
    ast: AST,
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
            Token::Assignment(dest, var) => todo!(),
            Token::String(s) => todo!(),
            Token::SingleQuotedString(s) => todo!(),
            Token::DoubleQuotedString(s) => todo!(),
            Token::RedirectInput(to) => todo!(),
            Token::RedirectOutput(from, to) => todo!(),
            Token::LParen => todo!(),
            Token::RParen => todo!(),
            Token::LBrace => todo!(),
            Token::RBrace => todo!(),
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
            token @ (Token::String(_)
            | Token::SingleQuotedString(_)
            | Token::DoubleQuotedString(_)) => {
                let word = parse_meta(token);

                if name.is_none() {
                    name = word;
                } else {
                    if let Some(word) = word {
                        suffix.push(word);
                    }
                }
            }

            token @ Token::Assignment(_, _) => {
                if !name.is_none() {
                    todo!("cannot parse assignment at this place");
                } else {
                    if let Some(assignment) = parse_meta(token) {
                        prefix.push(assignment);
                    }
                }
            }

            token @ Token::RedirectOutput(_, _) => {
                if let Some(redirect) = parse_meta(token) {
                    if name.is_none() {
                        prefix.push(redirect);
                    } else {
                        suffix.push(redirect);
                    }
                }
            }

            Token::Semicolon => unreachable!("semicolons should have been found already"),
            Token::Pipe => unreachable!("pipes should have been found already"),

            t => {
                panic!("unexpected token: {:?}", t);
            }
        }
    }

    if let Some(Meta::Word(name)) = name {
        Some(Command {
            name,
            prefix,
            suffix,
        })
    } else {
        None
    }
}

fn parse_meta(token: &Token) -> Option<Meta> {
    match token {
        Token::Assignment(dest, var) => {
            let dest = dest.to_string();
            let var = var.to_string();
            return Some(Meta::Assignment(Word(dest), Word(var)));
        }
        Token::String(s) => {
            return Some(Meta::Word(Word(s.to_string())));
        }
        Token::SingleQuotedString(s) => {
            return Some(Meta::Word(Word(s.to_string())));
        }
        Token::DoubleQuotedString(s) => {
            return Some(Meta::Word(Word(s.to_string())));
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
    use crate::engine::parser::lex;
    use super::*;

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
                        name: Word("echo".to_string()),
                        prefix: vec![Meta::Redirect(Redirect::Output(
                            Some("2".into()),
                            "&1".into()
                        )),],
                        suffix: vec![
                            Meta::Word(Word("hello".into())),
                            Meta::Word(Word("world".into())),
                        ],
                    },
                    Command {
                        name: Word("lolcat".to_string()),
                        prefix: vec![],
                        suffix: vec![Meta::Word(Word("-n".into())),],
                    }
                ]),],
            },
            ast
        );
    }
}
