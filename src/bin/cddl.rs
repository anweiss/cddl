#![cfg(feature = "std")]

#[macro_use]
extern crate clap;

use cddl::{compile_cddl_from_str, validate_json_from_str};
use clap::{App, AppSettings, SubCommand};
use crossterm::{Color, Colored};
use std::{error::Error, fs};

fn main() -> Result<(), Box<Error>> {
  let app = App::new("cddl")
                    .version(crate_version!())
                    .author(crate_authors!())
                    .about("Tool for verifying conformance of CDDL definitions against RFC 8610 and for validating JSON documents")
                    .setting(AppSettings::SubcommandRequiredElseHelp)
                    .subcommand(SubCommand::with_name("compile")
                                .about("compiles CDDL against RFC 8610")
                                .arg_from_usage("-c --cddl=<FILE> 'CDDL input file'"))
                    .subcommand(SubCommand::with_name("validate")
                                .about("validate JSON against CDDL definition")
                                .arg_from_usage("-c --cddl=<FILE> 'CDDL input file'")
                                .arg_from_usage("-j --json=<FILE> 'JSON input file"));

  let matches = app.get_matches();

  if let Some(matches) = matches.subcommand_matches("compile") {
    if let Some(c) = matches.value_of("cddl") {
      match compile_cddl_from_str(&fs::read_to_string(c)?) {
        Ok(()) => {
          println!("{}{} is conformant", Colored::Fg(Color::Green), c);

          return Ok(());
        }
        Err(e) => {
          eprintln!("{}{} is not conformant. {}", Colored::Fg(Color::Red), c, e);

          return Ok(());
        }
      }
    }
  }

  if let Some(matches) = matches.subcommand_matches("validate") {
    if let Some(cddl) = matches.value_of("cddl") {
      if let Some(json) = matches.value_of("json") {
        return validate_json_from_str(&fs::read_to_string(cddl)?, &fs::read_to_string(json)?)
          .map_err(Box::from);
      }
    }
  }

  Ok(())
}
