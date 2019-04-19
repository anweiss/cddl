use super::lexer::Lexer;
use super::token::Token;
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

    let mut l = Lexer::new(&*line);
    let mut tok = l.next_token()?;
    while tok != Token::EOF {
      writer.write_all(format!("{:?} [literal: \"{}\"]\n", tok, tok.to_string()).as_bytes())?;
      writer.flush()?;
      tok = l.next_token()?;
    }
  }
}
