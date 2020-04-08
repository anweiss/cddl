#![cfg(feature = "std")]
#![cfg(not(target_arch = "wasm32"))]

use super::parser;
use crossterm::{terminal, ClearType};
use std::io::{BufRead, Result, Write};

const PROMPT: &[u8] = b">> ";

/// Instantiates a new REPL
///
/// # Arguments
///
/// `reader` - A mutable `io::BufRead`
/// `writer` - A mutable `io::Write`
pub fn start<R: BufRead, W: Write>(mut reader: R, mut writer: W) -> Result<()> {
  loop {
    writer.write_all(PROMPT)?;
    writer.flush()?;

    let mut line = String::new();
    reader.read_line(&mut line)?;

    if let Ok(Some(_)) = control(&line) {
      writer.flush()?;
    } else {
      writer.write_all(
        format!(
          "{:#?}\n",
          parser::cddl_from_str(&line, false)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?
        )
        .as_bytes(),
      )?;

      writer.flush()?;
    }
  }
}

fn control(line: &str) -> Result<Option<()>> {
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
