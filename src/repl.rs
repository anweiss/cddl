use super::{lexer::Lexer, parser::Parser};
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

    let mut l = Lexer::new(&line);
    let mut p = Parser::new(&mut l)?;

    let cddl = p.parse_cddl()?;

    writer.write_all(format!("{:?}\n", cddl).as_bytes())?;
    writer.flush()?;
  }
}
