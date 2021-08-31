#[macro_use]
extern crate clap;

use cddl::{cddl_from_str, lexer_from_str, validate_cbor_from_slice, validate_json_from_str};
use clap::{App, AppSettings, Arg, SubCommand};
use codespan_reporting::term::termcolor::{
  BufferWriter, Color, ColorChoice, ColorSpec, WriteColor,
};
use std::{
  error::Error,
  fs::{self, File},
  io::{self, BufReader, Read, Write},
  path::Path,
};

fn main() -> Result<(), Box<dyn Error>> {
  let app = App::new("cddl")
                    .version(crate_version!())
                    .author(crate_authors!())
                    .about("Tool for verifying conformance of CDDL definitions against RFC 8610 and for validating JSON documents")
                    .setting(AppSettings::SubcommandRequiredElseHelp)
                    .subcommand(SubCommand::with_name("compile-cddl")
                                .about("Compile CDDL against RFC 8610")
                                .arg_from_usage("-c --cddl=<FILE> 'CDDL input file'"))
                    .subcommand(SubCommand::with_name("compile-json")
                                .about("Compile JSON against RFC 8259")
                                .arg_from_usage("-j --json=<FILE> 'JSON input file'"))
                    .subcommand(SubCommand::with_name("validate")
                                .about("Validate JSON or CBOR against CDDL definition")
                                .arg_from_usage("-c --cddl=<CDDL> 'CDDL input file'")
                                .arg(Arg::with_name("stdin").long("stdin").takes_value(false).help("JSON or CBOR input from stdin"))
                                .arg(Arg::with_name("file").value_name("FILE").help("JSON or CBOR input file(s)").multiple(true).required(false)));

  let matches = app.get_matches();

  let stdoutbuffwrtr = BufferWriter::stdout(ColorChoice::Auto);
  let mut stdout = stdoutbuffwrtr.buffer();
  stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;

  let stderrbuffwrtr = BufferWriter::stdout(ColorChoice::Auto);
  let mut stderr = stderrbuffwrtr.buffer();
  stderr.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;

  if let Some(matches) = matches.subcommand_matches("compile-cddl") {
    if let Some(c) = matches.value_of("cddl") {
      let p = Path::new(c);
      if !p.exists() {
        writeln!(&mut stderr, "CDDL document at path {:?} does not exist", p)?;
        stderrbuffwrtr.print(&stderr)?;

        return Ok(());
      }

      if let Some(e) = p.extension() {
        if e.to_string_lossy() != "cddl" {
          writeln!(
            &mut stderr,
            "File \"{}\" must have the \".cddl\" extension",
            c
          )?;
          stderrbuffwrtr.print(&stderr)?;

          return Ok(());
        }
      }

      let file_content = fs::read_to_string(c)?;
      cddl_from_str(&mut lexer_from_str(&file_content), &file_content, true).map(|_| ())?;

      writeln!(&mut stdout, "{} is conformant", c)?;
      stdoutbuffwrtr.print(&stdout)?;
    }
  }

  if let Some(matches) = matches.subcommand_matches("compile-json") {
    if let Some(j) = matches.value_of("json") {
      let p = Path::new(j);
      if !p.exists() {
        writeln!(
          &mut stderr,
          "\nCDDL document at path {:?} does not exist",
          p
        )?;
        stderrbuffwrtr.print(&stderr)?;

        return Ok(());
      }

      if let Some(e) = p.extension() {
        if e.to_string_lossy() != "json" {
          writeln!(
            &mut stderr,
            "File \"{}\" must have the \".json\" extension",
            j
          )?;
          stderrbuffwrtr.print(&stderr)?;

          return Ok(());
        }
      }

      let file = File::open(j)?;
      let reader = BufReader::new(file);
      let _: serde_json::Value = serde_json::from_reader(reader)?;

      return Ok(());
    }
  }

  if let Some(matches) = matches.subcommand_matches("validate") {
    if let Some(cddl) = matches.value_of("cddl") {
      let p = Path::new(cddl);
      if !p.exists() {
        writeln!(
          &mut stderr,
          "\nCDDL document at path {:?} does not exist",
          p
        )?;
        stderrbuffwrtr.print(&stderr)?;

        return Ok(());
      }

      if let Some(e) = p.extension() {
        if e.to_string_lossy() != "cddl" {
          writeln!(
            &mut stderr,
            "File \"{}\" must have the \".cddl\" extension",
            cddl
          )?;
          stderrbuffwrtr.print(&stderr)?;

          return Ok(());
        }
      }

      let cddl_str = fs::read_to_string(cddl)?;

      writeln!(&mut stdout)?;

      if let Some(files) = matches.values_of("file") {
        if matches.is_present("stdin") {
          return Err(Box::from(clap::Error {
            message: "cannot use --stdin flag with <FILE>... arg".to_string(),
            kind: clap::ErrorKind::ArgumentConflict,
            info: None,
          }));
        }

        for file in files {
          let p = Path::new(file);
          if !p.exists() {
            writeln!(&mut stderr, "\nFile at path {:?} does not exist", p)?;
            stderrbuffwrtr.print(&stderr)?;

            return Ok(());
          }

          if let Some(ext) = p.extension() {
            match ext.to_str() {
              Some("json") => {
                match validate_json_from_str(&cddl_str, &fs::read_to_string(file)?, None) {
                  Ok(()) => {
                    writeln!(&mut stdout, "Validation of {:?} is successful", p)?;
                    stdoutbuffwrtr.print(&stdout)?;
                  }
                  Err(e) => {
                    writeln!(&mut stderr, "Validation of {:?} failed", p)?;
                    writeln!(&mut stderr, "\n{}", e)?;
                    stderrbuffwrtr.print(&stderr)?;
                  }
                }
              }
              Some("cbor") => {
                let mut f = File::open(p)?;
                let mut data = Vec::new();
                f.read_to_end(&mut data)?;

                match validate_cbor_from_slice(&cddl_str, &data) {
                  Ok(()) => {
                    writeln!(&mut stdout, "Validation of {:?} is successful", p)?;
                    stdoutbuffwrtr.print(&stdout)?;
                  }
                  Err(e) => {
                    writeln!(&mut stderr, "Validation of {:?} failed", p)?;
                    writeln!(&mut stderr, "\n{}", e)?;
                    stderrbuffwrtr.print(&stderr)?;
                  }
                }
              }
              _ => {
                writeln!(
                  &mut stderr,
                  "\nFile {:?} is an unsupported file type. Must be either .json or .cbor",
                  p
                )?;
                stderrbuffwrtr.print(&stderr)?;
              }
            }
          }
        }

        return Ok(());
      }

      if matches.is_present("stdin") {
        let stdin = io::stdin();

        let mut reader = stdin.lock();
        let mut data = Vec::new();
        reader.read_to_end(&mut data)?;
        if let Ok(json) = std::str::from_utf8(&data) {
          match validate_json_from_str(&cddl_str, json, None) {
            Ok(()) => {
              writeln!(&mut stdout, "Validation from stdin is successful")?;
              stdoutbuffwrtr.print(&stdout)?;
            }
            Err(e) => {
              writeln!(&mut stderr, "Validation from stdin failed")?;
              writeln!(&mut stderr, "\n{}", e)?;
              stderrbuffwrtr.print(&stderr)?;
            }
          }
        } else {
          match validate_cbor_from_slice(&cddl_str, &data) {
            Ok(()) => {
              writeln!(&mut stdout, "Validation from stdin is successful")?;
              stdoutbuffwrtr.print(&stdout)?;
            }
            Err(e) => {
              writeln!(&mut stderr, "Validation from stdin failed")?;
              writeln!(&mut stderr, "\n{}", e)?;
              stderrbuffwrtr.print(&stderr)?;
            }
          }
        }

        return Ok(());
      }

      writeln!(&mut stderr, "\nMissing files to validate")?;
      stderrbuffwrtr.print(&stderr)?;
    }
  }

  Ok(())
}
