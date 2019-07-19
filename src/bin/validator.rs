#![cfg(feature = "std")]

use cddl::validator::validate_json_from_str;
use crossterm::{Color, Colored};
use std::{env, error::Error, fs};

fn main() -> Result<(), Box<Error>> {
  let args: Vec<String> = env::args().collect();
  let cddl_contents = fs::read_to_string(&args[1])?;
  let json_contents = fs::read_to_string(&args[2])?;

  match validate_json_from_str(&cddl_contents, &json_contents).map_err(Box::new) {
    Ok(()) => {
      println!("{}Success", Colored::Fg(Color::Green));

      Ok(())
    }
    Err(e) => {
      println!("{}{}", Colored::Fg(Color::Red), e);
      Ok(())
    }
  }
}
