pub mod builtin;
pub mod history;
pub mod parser;

use std::io::{self, Stdout, Write};
use std::path::PathBuf;
use std::process;

use crate::config::ABBREVIATIONS;
use crate::repl::input::read_line;
use crate::{path, Result};

pub use self::builtin::Builtins;
pub use self::history::History;
pub use self::parser::AST;
pub use self::parser::Line;
use self::parser::ast::parse;

pub struct Engine<W: Write> {
    pub writer: W,
    pub prev_dir: Option<PathBuf>,
    pub commands: Vec<String>,
    pub builtins: Vec<&'static str>,
    pub history: History,
}

impl Engine<Vec<u8>> {
    pub fn in_memory() -> Self {
        let history = History::init().expect("could not initialize history");
        Self {
            writer: Vec::new(),
            prev_dir: None,
            commands: path::get_cmds_from_path(),
            builtins: Self::builtin_names(),
            history,
        }
    }
}

impl<W: Write> Engine<W> {
    pub fn has_builtin(&self, builtin: impl AsRef<str>) -> bool {
        self.builtins.iter().any(|&b| b == builtin.as_ref())
    }

    pub fn has_command(&self, cmd: impl AsRef<str>) -> bool {
        self.commands
            .iter()
            .any(|c| c.ends_with(&format!("/{}", cmd.as_ref())))
    }

    pub fn has_abbreviation(&self, cmd: impl AsRef<str>) -> bool {
        let cmd = cmd.as_ref();
        ABBREVIATIONS.iter().any(|&(a, _)| a == cmd)
    }

    pub fn writer(&mut self) -> &mut W {
        &mut self.writer
    }

    pub fn execute(&mut self, cmd: Command) -> Result<ExitStatus> {
        match cmd {
            Command::Builtin(input) => self.execute_builtin(&input),

            Command::Valid(input) => execute_command(input),

            Command::Invalid(input) => {
                writeln!(self.writer, "Unknown command: {}", input.cmd)?;
                Ok(ExitStatus { code: 1 })
            }
        }
    }
}

impl Engine<Stdout> {
    pub fn new() -> Self {
        let history = History::init().expect("could not initialize history");
        Self {
            prev_dir: None,
            writer: io::stdout(),
            commands: path::get_cmds_from_path(),
            builtins: Self::builtin_names(),
            history,
        }
    }
}

impl Default for Engine<Stdout> {
    fn default() -> Self {
        Self::new()
    }
}

pub enum Command {
    Builtin(Line),
    Valid(Line),
    Invalid(Line),
}

pub struct ExitStatus {
    pub code: i32,
}

impl ExitStatus {
    pub fn from(code: i32) -> Self {
        Self { code }
    }
}

pub fn read_and_execute<W: Write>(engine: &mut Engine<W>) -> Result<()> {
    let line = read_line(engine)?;
    let ast = parse(line);
    walk_ast(engine, ast)
}

fn walk_ast<W: Write>(engine: &mut Engine<W>, ast: AST) -> Result<()> {
    writeln!(engine.writer, "{ast:#?}")?;
    Ok(())
}

fn execute_command(input: Line) -> Result<ExitStatus> {
    let child = process::Command::new(&input.cmd)
        .args(input.raw_args())
        .spawn()?;
    let result = child.wait_with_output()?;

    // FIXME: append new line if child did not print one?
    // println!("stdout: '{}'", String::from_utf8_lossy(&result.stdout));

    // FIXME: `ExitStatus.code()` returns None if killed by signal,
    //        handle this in a more thoughtful way?
    let code = result.status.code().unwrap_or_default();
    Ok(ExitStatus::from(code))
}
