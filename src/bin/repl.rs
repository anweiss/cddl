use cddl::repl;
use std::{error::Error, io};
use whoami;

fn main() -> Result<(), Box<Error>> {
  let username = whoami::username();

  println!("Hello {}! This is the CDDL language!", username);

  println!("Feel free to type in commands");

  let input = io::stdin();
  let output = io::stdout();
  repl::start(input.lock(), output.lock())?;
  Ok(())
}
