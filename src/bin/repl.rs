use cddl::repl;
use std::{error::Error, io};

fn main() -> Result<(), Box<Error>> {
  println!("CDDL REPL");

  let input = io::stdin();
  let output = io::stdout();
  repl::start(input.lock(), output.lock())?;
  Ok(())
}
