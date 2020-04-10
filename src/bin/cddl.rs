#[macro_use]
extern crate clap;

use cddl::{cddl_from_str, lexer_from_str, validate_json_from_str};
use clap::{App, AppSettings, SubCommand};
use codespan_reporting::term::termcolor::{
  Color, ColorChoice, ColorSpec, StandardStream, WriteColor,
};
use std::{error::Error, fs, io::Write};

fn main() -> Result<(), Box<dyn Error>> {
  let app = App::new("cddl")
                    .version(crate_version!())
                    .author(crate_authors!())
                    .about("Tool for verifying conformance of CDDL definitions against RFC 8610 and for validating JSON documents")
                    .setting(AppSettings::SubcommandRequiredElseHelp)
                    .subcommand(SubCommand::with_name("compile-cddl")
                                .about("compiles CDDL against RFC 8610")
                                .arg_from_usage("-c --cddl=<FILE> 'CDDL input file'"))
                    .subcommand(SubCommand::with_name("compile-json")
                                .about("compiles JSON")
                                .arg_from_usage("-j --json=<FILE> 'JSON input file'"))
                    .subcommand(SubCommand::with_name("validate")
                                .about("validate JSON against CDDL definition")
                                .arg_from_usage("-c --cddl=<FILE> 'CDDL input file'")
                                .arg_from_usage("-j --json=<FILE> 'JSON input file"));

  let matches = app.get_matches();

  if let Some(matches) = matches.subcommand_matches("compile-cddl") {
    if let Some(c) = matches.value_of("cddl") {
      if let Some(e) = std::path::Path::new(c).extension() {
        if e.to_string_lossy() != "cddl" {
          println!("File \"{}\" must have the \".cddl\" extension", c);

          return Ok(());
        }
      }

      let file_content = fs::read_to_string(c)?;
      cddl_from_str(&mut lexer_from_str(&file_content), &file_content, true).map(|_| ())?;

      let mut stdout = StandardStream::stdout(ColorChoice::Auto);
      stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
      writeln!(&mut stdout, "{} is conformant", c)?;
    }
  }

  if let Some(matches) = matches.subcommand_matches("compile-json") {
    if let Some(c) = matches.value_of("json") {
      let file = std::fs::File::open(c)?;
      let reader = std::io::BufReader::new(file);
      let _: serde_json::Value = serde_json::from_reader(reader)?;

      return Ok(());
    }
  }

  if let Some(matches) = matches.subcommand_matches("validate") {
    if let Some(cddl) = matches.value_of("cddl") {
      if let Some(json) = matches.value_of("json") {
        match validate_json_from_str(&fs::read_to_string(cddl)?, &fs::read_to_string(json)?) {
          Ok(()) => {
            let mut stdout = StandardStream::stdout(ColorChoice::Auto);
            stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
            writeln!(&mut stdout, "Validation is successful")?;
          }
          Err(e) => {
            let mut stderr = StandardStream::stderr(ColorChoice::Auto);
            stderr.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
            writeln!(&mut stderr, "Validation failed. {}", e)?;
          }
        }

        return Ok(());
      }
    }
  }

  Ok(())
}
