use crossterm::cursor;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::queue;
use crossterm::style;
use crossterm::terminal;

use std::env;
use std::io::Write;

use crate::config::{Colors, ABBREVIATIONS};
use crate::engine::{Command, ExitStatus};
use crate::path;
use crate::Engine;
use crate::Result;

pub fn input<W: Write>(engine: &mut Engine<W>) -> Result<Option<Command>> {
    Ok(match Input::read(engine)? {
        None => None,
        Some(input) if engine.has_builtin(&input.cmd) => Some(Command::Builtin(input)),
        Some(input) if engine.has_command(&input.cmd) => Some(Command::Valid(input)),
        Some(input) => Some(Command::Invalid(input)),
    })
}

pub fn prompt<W: Write>(writer: &mut W, last_status: &Option<ExitStatus>) -> Result<()> {
    crossterm::terminal::enable_raw_mode()?;

    let cwd = format!(
        "{} ",
        env::current_dir()
            .unwrap()
            .display()
            .to_string()
            .replace(&path::home_dir()?, "~")
    );
    queue!(
        writer,
        style::SetForegroundColor(Colors::CWD),
        style::Print(cwd),
    )?;

    match last_status {
        Some(ExitStatus { code }) if *code != 0 => {
            let exit_code = format!("[{code}] ");
            queue!(
                writer,
                style::SetForegroundColor(Colors::NON_ZERO_RC),
                style::Print(exit_code),
            )?;
        }

        _ => {}
    }

    queue!(
        writer,
        style::SetForegroundColor(Colors::PROMPT),
        style::Print("$ "),
        style::SetForegroundColor(style::Color::Reset)
    )?;

    crossterm::terminal::disable_raw_mode()?;
    Ok(writer.flush()?)
}

pub struct Input {
    pub cmd: String,
    pub raw_args: Vec<String>,
    // options, etc. in the future
}

impl Input {
    pub fn raw_args(&self) -> &[String] {
        &self.raw_args
    }

    pub fn read<W: Write>(engine: &mut Engine<W>) -> Result<Option<Self>> {
        let buffer = read_line(engine)?.trim().to_string();

        if buffer.is_empty() {
            return Ok(None);
        }

        let home = path::home_dir()?;

        engine.history.append(&buffer)?;

        match buffer.find(' ') {
            Some(_) => {
                let (cmd, args) = buffer.split_once(' ').unwrap();
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
                cmd: buffer,
                raw_args: Default::default(),
            })),
        }
    }
}

pub fn read_line<W: Write>(engine: &mut Engine<W>) -> Result<String> {
    terminal::enable_raw_mode()?;
    let line = sys::read_line(engine);
    terminal::disable_raw_mode()?;
    line
}

mod sys {
    use super::*;

    pub fn read_line<W: Write>(engine: &mut Engine<W>) -> Result<String> {
        let mut line = String::new();
        let mut index = 0;

        let (start_x, start_y) = cursor::position()?;
        let (_width, height) = terminal::size()?;

        let mut about_to_exit = false;
        let mut cancelled = false;

        while !about_to_exit {
            let (code, modifiers) = match event::read()? {
                Event::Key(KeyEvent {
                    code, modifiers, ..
                }) => (code, modifiers),
                _ => break,
            };

            match (code, modifiers) {
                (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                    if line.is_empty() {
                        continue;
                    }

                    line += "^C";
                    about_to_exit = true;
                    cancelled = true;
                }

                (KeyCode::Enter, _) => {
                    for (a, b) in ABBREVIATIONS {
                        if line.starts_with(a) {
                            line = line.replacen(a, b, 1);
                            break;
                        }
                    }

                    about_to_exit = true;
                }

                (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                    if !line.is_empty() {
                        continue;
                    }

                    execute!(engine.writer, style::Print("\n\r"))?;

                    // FIXME: better control flow than this
                    std::process::exit(0);
                }

                (KeyCode::Up, _) | (KeyCode::Char('p'), KeyModifiers::CONTROL) => {
                    line = engine
                        .history
                        .prev()?
                        .cloned()
                        .unwrap_or_else(|| "".to_string());
                    index = line.len();

                    execute!(
                        engine.writer,
                        cursor::MoveTo(start_x + index as u16, start_y)
                    )?;
                }

                (KeyCode::Down, _) | (KeyCode::Char('n'), KeyModifiers::CONTROL) => {
                    line = engine
                        .history
                        .next()?
                        .cloned()
                        .unwrap_or_else(|| "".to_string());
                    index = line.len();

                    execute!(
                        engine.writer,
                        cursor::MoveTo(start_x + index as u16, start_y)
                    )?;
                }

                (KeyCode::Char('u'), KeyModifiers::CONTROL) => {
                    line.clear();
                    index = 0;
                    execute!(engine.writer, cursor::MoveTo(start_x, start_y))?;
                }

                (KeyCode::Char('w'), KeyModifiers::CONTROL) => {
                    if index == 0 {
                        continue;
                    }

                    let mut space_index = None;
                    for i in (0..index).rev() {
                        if let Some(' ') = line.chars().nth(i) {
                            space_index = Some(i);
                            break;
                        }
                    }

                    if let Some(' ') = line.chars().nth(index - 1) {
                        // FIXME: this should find the previous space
                        space_index = Some(0);
                    }

                    let space_index = space_index.unwrap_or(0);
                    let offset = (index - space_index) as u16;
                    line.replace_range(space_index..index, "");
                    index = space_index;
                    execute!(engine.writer, cursor::MoveLeft(offset))?;
                }

                (KeyCode::Char('l'), KeyModifiers::CONTROL) => {
                    execute!(
                        engine.writer,
                        terminal::Clear(terminal::ClearType::All),
                        cursor::MoveTo(0, 0)
                    )?;
                    break;
                }

                (KeyCode::Left, _) | (KeyCode::Char('b'), KeyModifiers::CONTROL) if index > 0 => {
                    index -= 1;
                    execute!(engine.writer, cursor::MoveLeft(1))?;
                }

                (KeyCode::Right, _) | (KeyCode::Char('f'), KeyModifiers::CONTROL)
                    if index < line.len() =>
                {
                    index += 1;
                    execute!(engine.writer, cursor::MoveRight(1))?;
                }

                (KeyCode::Char(' '), KeyModifiers::NONE) => {
                    let (mut x, y) = cursor::position()?;

                    for (a, b) in ABBREVIATIONS {
                        if line.starts_with(a) {
                            let diff = b.len() - a.len();
                            line = line.replacen(a, b, 1);
                            x += diff as u16;
                            index += diff;
                            break;
                        }
                    }

                    line.insert(index, ' ');
                    index += 1;

                    execute!(
                        engine.writer,
                        terminal::Clear(terminal::ClearType::UntilNewLine),
                        style::Print(&line[index - 1..]),
                        cursor::MoveTo(x + 1, y),
                    )?;
                }

                (KeyCode::Char(' '), KeyModifiers::CONTROL) => {
                    let (x, y) = cursor::position()?;

                    line.insert(index, ' ');
                    index += 1;

                    execute!(
                        engine.writer,
                        terminal::Clear(terminal::ClearType::UntilNewLine),
                        style::Print(&line[index - 1..]),
                        cursor::MoveTo(x + 1, y),
                    )?;
                }

                (KeyCode::Char(c), KeyModifiers::NONE) => {
                    let (x, y) = cursor::position()?;

                    line.insert(index, c);
                    index += 1;

                    execute!(
                        engine.writer,
                        terminal::Clear(terminal::ClearType::UntilNewLine),
                        style::Print(&line[index - 1..]),
                        cursor::MoveTo(x + 1, y),
                    )?;
                }

                (KeyCode::Backspace, _) if index > 0 => {
                    let (x, y) = cursor::position()?;

                    index -= 1;
                    line.remove(index);

                    execute!(
                        engine.writer,
                        cursor::MoveTo(x - 1, y),
                        terminal::Clear(terminal::ClearType::UntilNewLine),
                        style::Print(&line[index..]),
                        cursor::MoveTo(x - 1, y),
                    )?;
                }

                _ => {}
            }

            let mut highlight_until_index = line.find(' ').unwrap_or(line.len());
            let mut cmd = &line[..highlight_until_index];
            if cmd.ends_with("^C") {
                highlight_until_index -= 2;
                cmd = &cmd[..cmd.len() - 2];
            }
            let cmd_exists = engine
                .commands
                .iter()
                .any(|s| s.ends_with(&format!("/{}", cmd)));
            let builtin_exists = engine.builtins.iter().any(|&s| s == cmd.trim());
            let (x, y) = cursor::position()?;
            let abbr_exists = ABBREVIATIONS.iter().any(|(a, _)| a == &cmd);

            let highlight_color = if builtin_exists {
                Colors::VALID_BUILTIN
            } else if abbr_exists {
                Colors::VALID_ABBR
            } else if cmd_exists {
                Colors::VALID_CMD
            } else {
                Colors::INVALID_CMD
            };

            execute!(
                engine.writer,
                cursor::MoveTo(start_x, start_y),
                terminal::Clear(terminal::ClearType::UntilNewLine),
                style::SetForegroundColor(highlight_color),
                style::Print(&line[..highlight_until_index]),
                style::ResetColor,
                style::Print(&line[highlight_until_index..]),
                cursor::MoveTo(x, y)
            )?;

            if about_to_exit {
                write!(engine.writer, "\r")?;
                if start_y + 1 >= height {
                    execute!(engine.writer, terminal::ScrollUp(1))?;
                }
                execute!(engine.writer, cursor::MoveTo(0, start_y + 1))?;
            }
        }

        // FIXME: should probably return Result<Option<String>> with Ok(None) here?
        if cancelled {
            Ok("".to_string())
        } else {
            Ok(line)
        }
    }
}
