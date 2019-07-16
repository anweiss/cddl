#![cfg(feature = "std")]

use super::parser;
use crossterm::{terminal, ClearType};
use std::{
  error,
  io::{BufRead, Write},
};

const PROMPT: &[u8] = b">> ";

pub fn start<R: BufRead, W: Write>(mut reader: R, mut writer: W) -> Result<(), Box<error::Error>> {
  loop {
    writer.write_all(PROMPT)?;
    writer.flush()?;

    let mut line = String::new();
    reader.read_line(&mut line)?;

    if let Ok(Some(_)) = control(&line) {
      writer.flush()?;
    } else {
      writer.write_all(format!("{:#?}\n", parser::cddl_from_str(&line)?).as_bytes())?;

      writer.flush()?;
    }
  }
}

fn control(line: &str) -> Result<Option<()>, Box<error::Error>> {
  match line {
    "clear\n" => {
      let terminal = terminal();

      terminal.clear(ClearType::All)?;

      Ok(Some(()))
    }
    "quit\n" | "exit\n" => std::process::exit(0),
    _ => Ok(None),
  }
}
