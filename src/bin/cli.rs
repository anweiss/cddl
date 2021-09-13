#![cfg(feature = "cbor")]
#![cfg(feature = "json")]
#![cfg(not(feature = "lsp"))]

#[macro_use]
extern crate clap;
#[macro_use]
extern crate log;

use cddl::{cddl_from_str, lexer_from_str, validate_cbor_from_slice, validate_json_from_str};
use clap::{App, AppSettings, Arg, SubCommand};

use simplelog::*;
use std::{
  error::Error,
  fs::{self, File},
  io::{self, BufReader, Read},
  path::Path,
};

fn main() -> Result<(), Box<dyn Error>> {
  TermLogger::init(
    LevelFilter::Info,
    ConfigBuilder::new()
      .set_time_level(LevelFilter::Off)
      .build(),
    TerminalMode::Mixed,
    ColorChoice::Auto,
  )?;

  #[cfg(feature = "additional-controls")]
  let validate_subcommand = SubCommand::with_name("validate")
    .about("Validate JSON and/or CBOR against a CDDL definition")
    .usage("cddl validate --cddl <CDDL> --json <FILE>... --cbor <FILE>...\n    cat hello.json | cddl validate --stdin")
    .arg_from_usage("-c --cddl <CDDL> 'CDDL input file'")
    .arg_from_usage("-f --features [FEATURE]... 'optional features to enable during validation'")
    .arg(
      Arg::with_name("json")
        .long("json")
        .takes_value(true)
        .multiple(true)
        .empty_values(false)
        .help("JSON files to validate")
        .required_unless_one(&["cbor", "stdin"])
    )
    .arg(
      Arg::with_name("cbor")
        .long("cbor")
        .takes_value(true)
        .multiple(true)
        .empty_values(false)
        .help("CBOR binary files to validate")
        .required_unless_one(&["json", "stdin"]),
    )
    // .arg_from_usage("--json [FILE]... 'JSON files to validate'")
    // .arg_from_usage("--cbor [FILE]... 'CBOR binary files to validate'")
    .arg(
      Arg::with_name("stdin")
        .long("stdin")
        .takes_value(false)
        .help("JSON or CBOR input from stdin")
        .long_help("files read from stdin which are encoded as valid utf-8\nwill be implied as JSON whereas invalid utf-8 encoded\ninput is implied as CBOR")
    )
    .setting(AppSettings::DeriveDisplayOrder);

  #[cfg(not(feature = "additional-controls"))]
  let validate_subcommand = SubCommand::with_name("validate")
    .about("Validate JSON or CBOR against CDDL definition")
    .arg_from_usage("-c --cddl <CDDL> 'CDDL input file'")
    .arg_from_usage("--json [FILE]... 'JSON files to validate'")
    .arg_from_usage("--cbor [FILE]... 'CBOR binary files to validate'")
    .arg(
      Arg::with_name("stdin")
        .long("stdin")
        .takes_value(false)
        .help("JSON or CBOR input from stdin")
        .conflicts_with_all(&["json", "cbor"]),
    );

  let app = App::new("cddl")
                    .version(crate_version!())
                    .author(crate_authors!())
                    .about("Tool for verifying conformance of CDDL definitions against RFC 8610 and for validating JSON documents and CBOR binary files")
                    .setting(AppSettings::SubcommandRequiredElseHelp)
                    .subcommand(SubCommand::with_name("compile-cddl")
                                .about("Compile CDDL against RFC 8610")
                                .arg_from_usage("-c --cddl=<FILE> 'CDDL input file'"))
                    .subcommand(SubCommand::with_name("compile-json")
                                .about("Compile JSON against RFC 8259")
                                .arg_from_usage("-j --json=<FILE> 'JSON input file'"))
                    .subcommand(validate_subcommand);

  let matches = app.get_matches();

  if let Some(matches) = matches.subcommand_matches("compile-cddl") {
    if let Some(c) = matches.value_of("cddl") {
      let p = Path::new(c);
      if !p.exists() {
        error!("CDDL document {:?} does not exist", p);

        return Ok(());
      }

      if let Some(e) = p.extension() {
        if e.to_string_lossy() != "cddl" {
          error!("File \"{}\" must have the \".cddl\" extension", c);

          return Ok(());
        }
      }

      let file_content = fs::read_to_string(c)?;
      cddl_from_str(&mut lexer_from_str(&file_content), &file_content, true).map(|_| ())?;

      error!("{} is conformant", c);
    }
  }

  if let Some(matches) = matches.subcommand_matches("compile-json") {
    if let Some(j) = matches.value_of("json") {
      let p = Path::new(j);
      if !p.exists() {
        error!("CDDL document {:?} does not exist", p);

        return Ok(());
      }

      if let Some(e) = p.extension() {
        if e.to_string_lossy() != "json" {
          error!("File \"{}\" must have the \".json\" extension", j);

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
      #[cfg(feature = "additional-controls")]
      let enabled_features: Option<Vec<&str>> = matches.values_of("features").map(|f| f.collect());
      #[cfg(feature = "additional-controls")]
      if let Some(enabled_features) = &enabled_features {
        let mut feature_str = String::from("enabled features: [");
        for (idx, feature) in enabled_features.iter().enumerate() {
          if idx == 0 {
            feature_str.push_str(&format!("\"{}\"", feature));
          } else {
            feature_str.push_str(&format!(", \"{}\"", feature));
          }
        }
        feature_str.push(']');

        info!("{}", feature_str);
      }

      let p = Path::new(cddl);
      if !p.exists() {
        error!("CDDL document {:?} does not exist", p);

        return Ok(());
      }

      if let Some(e) = p.extension() {
        if e.to_string_lossy() != "cddl" {
          error!("File \"{}\" must have the \".cddl\" extension", cddl);

          return Ok(());
        }
      }

      let cddl_str = fs::read_to_string(cddl)?;

      if let Some(files) = matches.values_of("json") {
        for file in files {
          let p = Path::new(file);
          if !p.exists() {
            error!("File {:?} does not exist", p);

            continue;
          }

          #[cfg(feature = "additional-controls")]
          let r = validate_json_from_str(
            &cddl_str,
            &fs::read_to_string(file)?,
            enabled_features.as_deref(),
          );
          #[cfg(not(feature = "additional-controls"))]
          let r = validate_json_from_str(&cddl_str, &fs::read_to_string(file)?);

          match r {
            Ok(()) => {
              info!("Validation of {:?} is successful", p);
            }
            Err(e) => {
              error!("Validation of {:?} failed: {}", p, e);
            }
          }
        }
      }

      if let Some(files) = matches.values_of("cbor") {
        for file in files {
          let p = Path::new(file);
          if !p.exists() {
            error!("File {:?} does not exist", p);

            continue;
          }
          let mut f = File::open(p)?;
          let mut data = Vec::new();
          f.read_to_end(&mut data)?;

          #[cfg(feature = "additional-controls")]
          let c = validate_cbor_from_slice(&cddl_str, &data, None);
          #[cfg(not(feature = "additional-controls"))]
          let c = validate_cbor_from_slice(&cddl_str, &data);

          match c {
            Ok(()) => {
              info!("Validation of {:?} is successful", p);
            }
            Err(e) => {
              error!("Validation of {:?} failed: {}", p, e);
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
          #[cfg(feature = "additional-controls")]
          let r = validate_json_from_str(&cddl_str, json, None);
          #[cfg(not(feature = "additional-controls"))]
          let r = validate_json_from_str(&cddl_str, json);

          match r {
            Ok(()) => {
              info!("Validation from stdin is successful");
            }
            Err(e) => {
              error!("Validation from stdin failed: {}", e);
            }
          }
        } else {
          #[cfg(feature = "additional-controls")]
          let c = validate_cbor_from_slice(&cddl_str, &data, enabled_features.as_deref());
          #[cfg(not(feature = "additional-controls"))]
          let c = validate_cbor_from_slice(&cddl_str, &data);

          match c {
            Ok(()) => {
              info!("Validation from stdin is successful");
            }
            Err(e) => {
              error!("Validation from stdin failed: {}", e);
            }
          }
        }

        return Ok(());
      }
    }
  }

  Ok(())
}
