#![cfg(feature = "std")]
#![cfg(not(target_arch = "wasm32"))]

use super::{lexer, parser};
use crossterm::{
  cursor, execute,
  terminal::{Clear, ClearType},
  Result,
};
use std::io::{stdout, BufRead, Write};

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
      if let Ok(c) = parser::cddl_from_str(&mut lexer::Lexer::new(&line), &line, true) {
        writer.write_all(format!("{:#?}\n", c).as_bytes())?;
      }

      writer.flush()?;
    }
  }
}

fn control(line: &str) -> Result<Option<()>> {
  let mut stdout = stdout();

  match line {
    "clear\n" => {
      execute!(stdout, Clear(ClearType::All))?;
      execute!(stdout, cursor::MoveTo(0, 0))?;

      Ok(Some(()))
    }
    "quit\n" | "exit\n" => {
      execute!(stdout, Clear(ClearType::All))?;
      execute!(stdout, cursor::MoveTo(0, 0))?;

      std::process::exit(0)
    }
    _ => Ok(None),
  }
}
