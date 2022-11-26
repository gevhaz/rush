pub mod ast;
pub mod lexer;
mod util;

use std::io::Write;

pub use lexer::{lex, Token};
pub use ast::AST;

use crate::path;
use crate::{Engine, Result};

use super::Command;

pub fn parse_line<W: Write>(
    engine: &mut Engine<W>,
    line: impl ToString,
) -> Result<Option<Command>> {
    Ok(match Line::parse(engine, line.to_string())? {
        None => None,
        Some(input) if engine.has_builtin(&input.cmd) => Some(Command::Builtin(input)),
        Some(input) if engine.has_command(&input.cmd) => Some(Command::Valid(input)),
        Some(input) => Some(Command::Invalid(input)),
    })
}

pub struct Line {
    pub cmd: String,
    pub raw_args: Vec<String>,
    // options, etc. in the future
}

impl Line {
    pub fn parse<W: Write>(engine: &mut Engine<W>, line: String) -> Result<Option<Self>> {
        if line.is_empty() {
            return Ok(None);
        }

        let home = path::home_dir()?;

        engine.history.append(&line)?;

        match line.find(' ') {
            Some(i) => {
                let (cmd, args) = line.split_at(i);
                let args = args
                    .split_ascii_whitespace()
                    .map(|s| s.replace('~', &home))
                    .collect::<Vec<_>>();

                Ok(Some(Self {
                    cmd: cmd.to_string(),
                    raw_args: args,
                }))
            }

            _ => Ok(Some(Self {
                cmd: line,
                raw_args: Default::default(),
            })),
        }
    }

    pub fn raw_args(&self) -> &[String] {
        &self.raw_args
    }
}
