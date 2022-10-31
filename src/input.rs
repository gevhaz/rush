use crossterm::cursor;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::queue;
use crossterm::style;
use crossterm::terminal;

use std::env;
use std::fs;
use std::io::Write;

use crate::config::Colors;
use crate::engine::{Command, ExitStatus};
use crate::path;
use crate::Engine;
use crate::Result;

pub fn input<W: Write>(engine: &mut Engine<W>) -> Result<Option<Command>> {
    match Input::read(engine)? {
        None => Ok(None),
        Some(input) if engine.has_builtin(&input.cmd) => Ok(Some(Command::Builtin(input))),
        Some(input) if engine.has_command(&input.cmd) => Ok(Some(Command::Valid(input))),
        Some(input) => Ok(Some(Command::Invalid(input))),
    }
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

        let mut file = fs::OpenOptions::new()
            .write(true)
            .append(true)
            .create(true)
            .open(path::hist_file()?)?;
        file.write_all(buffer.as_bytes())?;
        file.write_all(b"\n")?;

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

        let hist_file = path::hist_file()?;
        let history = fs::read_to_string(hist_file)?;
        let history: Vec<_> = history.trim().split('\n').collect();
        let mut hist_index = match history.len() {
            0 => 0,
            n => n - 1,
        };

        while let Event::Key(KeyEvent {
            code, modifiers, ..
        }) = event::read()?
        {
            match code {
                KeyCode::Char('c') if modifiers.contains(KeyModifiers::CONTROL) => {
                    if line.is_empty() {
                        continue;
                    }

                    line += "^C";
                    execute!(
                        engine.writer,
                        cursor::MoveTo(start_x, start_y),
                        terminal::Clear(terminal::ClearType::UntilNewLine),
                        style::Print(&line)
                    )?;
                    line.clear();
                    write!(engine.writer, "\r")?;
                    execute!(engine.writer, cursor::MoveTo(0, start_y + 1))?;
                    if start_y + 1 >= height {
                        execute!(engine.writer, terminal::ScrollUp(1))?;
                    }
                    break;
                }
                KeyCode::Enter => {
                    write!(engine.writer, "\r")?;
                    execute!(engine.writer, cursor::MoveTo(0, start_y + 1))?;
                    if start_y + 1 >= height {
                        execute!(engine.writer, terminal::ScrollUp(1))?;
                    }
                    break;
                }

                KeyCode::Char('d') if modifiers.contains(KeyModifiers::CONTROL) => {
                    if !line.is_empty() {
                        continue;
                    }

                    execute!(engine.writer, style::Print("\n\r"))?;

                    // FIXME: better control flow than this
                    std::process::exit(0);
                }

                KeyCode::Char('p') if modifiers.contains(KeyModifiers::CONTROL) => {
                    if hist_index > 0 {
                        hist_index -= 1;
                    }
                    let hist_content = history[hist_index];
                    line = hist_content.to_string();
                    index = hist_content.len();
                    execute!(
                        engine.writer,
                        cursor::MoveTo(start_x, start_y),
                        terminal::Clear(terminal::ClearType::UntilNewLine),
                        cursor::MoveTo(start_x + index as u16, start_y)
                    )?;
                }

                KeyCode::Char('n') if modifiers.contains(KeyModifiers::CONTROL) => {
                    if hist_index < history.len() - 1 {
                        hist_index += 1;
                    }
                    let hist_content = history[hist_index];
                    line = hist_content.to_string();
                    index = hist_content.len();
                    execute!(
                        engine.writer,
                        cursor::MoveTo(start_x, start_y),
                        terminal::Clear(terminal::ClearType::UntilNewLine),
                        cursor::MoveTo(start_x + index as u16, start_y)
                    )?;
                }

                KeyCode::Char('u') if modifiers.contains(KeyModifiers::CONTROL) => {
                    line.clear();
                    index = 0;
                    execute!(engine.writer, cursor::MoveTo(start_x, start_y))?;
                }

                KeyCode::Char('w') if modifiers.contains(KeyModifiers::CONTROL) => {
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

                KeyCode::Char('l') if modifiers.contains(KeyModifiers::CONTROL) => {
                    execute!(
                        engine.writer,
                        terminal::Clear(terminal::ClearType::All),
                        cursor::MoveTo(0, 0)
                    )?;
                    break;
                }

                KeyCode::Char('b') if modifiers.contains(KeyModifiers::CONTROL) && index > 0 => {
                    index -= 1;
                    execute!(engine.writer, cursor::MoveLeft(1))?;
                }

                KeyCode::Left if index > 0 => {
                    index -= 1;
                    execute!(engine.writer, cursor::MoveLeft(1))?;
                }

                KeyCode::Char('f')
                    if modifiers.contains(KeyModifiers::CONTROL) && index < line.len() =>
                {
                    index += 1;
                    execute!(engine.writer, cursor::MoveRight(1))?;
                }

                KeyCode::Right if index < line.len() => {
                    index += 1;
                    execute!(engine.writer, cursor::MoveRight(1))?;
                }

                KeyCode::Char(c) => {
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

                KeyCode::Backspace if index > 0 => {
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

            let highlight_until_index = line.find(' ').unwrap_or(line.len());
            let cmd = &line[..highlight_until_index];
            let cmd_exists = engine
                .commands
                .iter()
                .any(|s| s.ends_with(&format!("/{}", cmd)));
            let builtin_exists = engine.builtins.iter().any(|&s| s == cmd.trim());
            let (x, y) = cursor::position()?;

            let highlight_color = if builtin_exists {
                Colors::VALID_BUILTIN
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
                style::SetForegroundColor(style::Color::Reset),
                style::Print(&line[highlight_until_index..]),
                cursor::MoveTo(x, y)
            )?;
        }
        Ok(line)
    }
}
