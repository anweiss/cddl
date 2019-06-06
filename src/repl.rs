use super::{lexer::Lexer, parser::Parser};
use std::{
  error,
  io::{BufRead, Write},
};
use termion::{clear, cursor};

const PROMPT: &[u8] = b">> ";

pub fn start<R: BufRead, W: Write>(mut reader: R, mut writer: W) -> Result<(), Box<error::Error>> {
  loop {
    writer.write_all(PROMPT)?;
    writer.flush()?;

    let mut line = String::new();
    reader.read_line(&mut line)?;

    if let Some(s) = control(&line) {
      writer.write_all(s.as_bytes())?;
    } else {
      let mut l = Lexer::new(&line);
      let mut p = Parser::new(&mut l)?;

      let cddl = p.parse_cddl()?;

      writer.write_all(format!("{:?}\n", cddl).as_bytes())?;
    }

    writer.flush()?;
  }
}

fn control(line: &str) -> Option<String> {
  match line {
    "clear()\n" => Some(format!("{}{}", clear::All, cursor::Goto(1, 1))),
    "quit()\n" | "exit()\n" => std::process::exit(0),
    _ => None,
  }
}

