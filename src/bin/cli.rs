#![cfg(all(feature = "cbor", feature = "json"))]
#![cfg(not(feature = "lsp"))]
#![cfg(not(test))]
#![allow(unused_imports)]
#![allow(unused_macros)]

#[macro_use]
extern crate log;

use cddl::cddl_from_str;
#[cfg(not(target_arch = "wasm32"))]
use cddl::{
  parser::root_type_name_from_cddl_str, validate_cbor_from_slice, validate_json_from_str,
};
use clap::{ArgGroup, Args, Parser, Subcommand};

use simplelog::*;
use std::{
  error::Error,
  fmt::Write,
  fs::{self, File},
  io::{self, BufReader, Read},
  path::Path,
};

#[derive(Parser)]
#[clap(author, version, about = "Tool for verifying conformance of CDDL definitions against RFC 8610 and for validating JSON documents and CBOR binary files", long_about = None)]
struct Cli {
  /// Enable CI mode, failing if files cannot be found or other edge cases.
  #[clap(long)]
  ci: bool,

  #[clap(subcommand)]
  command: Commands,
}

#[derive(Subcommand)]
enum Commands {
  #[clap(name = "compile-cddl", about = "Compile CDDL against RFC 8610")]
  CompileCddl {
    #[clap(short = 'c', long = "cddl", help = "Path to CDDL document")]
    file: String,
  },
  #[clap(name = "compile-json", about = "Compile JSON against RFC 8259")]
  CompileJson {
    #[clap(short = 'j', long = "json", help = "Path to JSON document")]
    file: String,
  },
  Validate(Validate),
}

#[derive(Args)]
#[clap(about = "Validate JSON and/or CBOR against a CDDL definition")]
#[clap(group(ArgGroup::new("targets").required(true).multiple(true).args(&["stdin", "json", "cbor"])))]
struct Validate {
  #[clap(short = 'd', long = "cddl", help = "CDDL document")]
  cddl: String,
  #[clap(
    short = 'f',
    long = "features",
    help = "Optional features to enable during validation",
    use_value_delimiter = true
  )]
  features: Option<Vec<String>>,
  #[clap(
    short = 'j',
    long = "json",
    help = "JSON document(s) to validate",
    use_value_delimiter = true,
    multiple_values = true
  )]
  json: Option<Vec<String>>,
  #[clap(
    short = 'c',
    long = "cbor",
    help = "CBOR binary file(s) to validate",
    multiple_values = true,
    use_value_delimiter = true
  )]
  cbor: Option<Vec<String>>,
  #[clap(
    long = "stdin",
    help = "JSON or CBOR input from stdin. Assumes UTF-8 encoding is JSON, otherwise parses as CBOR"
  )]
  stdin: bool,
}

macro_rules! error {
    ($ci: expr, $($args: tt)+ ) => {
      log::error!($($args)+);
      if $ci {
        return Err(format!($($args)+).into());
      }
    };
}

#[cfg(not(target_arch = "wasm32"))]
fn main() -> Result<(), Box<dyn Error>> {
  TermLogger::init(
    LevelFilter::Info,
    ConfigBuilder::new()
      .set_time_level(LevelFilter::Off)
      .build(),
    TerminalMode::Mixed,
    ColorChoice::Auto,
  )?;

  let cli = Cli::parse();

  match &cli.command {
    Commands::CompileCddl { file } => {
      let p = Path::new(file);
      if !p.exists() {
        error!(cli.ci, "CDDL document {:?} does not exist", p);
        return Ok(());
      }

      let file_content = fs::read_to_string(file)?;
      cddl_from_str(&file_content, true).map(|_| ())?;

      info!("{} is conformant", file);
    }
    Commands::CompileJson { file } => {
      let p = Path::new(file);
      if !p.exists() {
        error!(cli.ci, "JSON document {:?} does not exist", p);

        return Ok(());
      }

      let file = File::open(file)?;
      let reader = BufReader::new(file);
      let _: serde_json::Value = serde_json::from_reader(reader)?;

      return Ok(());
    }
    Commands::Validate(validate) => {
      #[cfg(feature = "additional-controls")]
      let enabled_features: Option<Vec<&str>> = validate
        .features
        .as_ref()
        .map(|f| f.iter().map(|s| s.as_str()).collect());

      #[cfg(feature = "additional-controls")]
      if let Some(enabled_features) = &enabled_features {
        let mut feature_str = String::from("enabled features: [");
        for (idx, feature) in enabled_features.iter().enumerate() {
          if idx == 0 {
            let _ = write!(feature_str, "\"{}\"", feature);
          } else {
            let _ = write!(feature_str, ", \"{}\"", feature);
          }
        }
        feature_str.push(']');

        info!("{}", feature_str);
      }

      let p = Path::new(&validate.cddl);
      if !p.exists() {
        error!(cli.ci, "CDDL document {:?} does not exist", p);

        return Ok(());
      }

      let cddl_str = fs::read_to_string(&validate.cddl)?;

      info!(
        "Root type for validation: {}",
        root_type_name_from_cddl_str(&cddl_str)?
      );

      if let Some(files) = &validate.json {
        for file in files {
          let p = Path::new(file);
          if !p.exists() {
            error!(cli.ci, "File {:?} does not exist", p);

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
            Ok(_) => {
              info!("Validation of {:?} is successful", p);
            }
            Err(e) => {
              error!(
                cli.ci,
                "Validation of {:?} failed: {}",
                p,
                e.to_string().trim_end()
              );
            }
          }
        }
      }

      if let Some(files) = &validate.cbor {
        for file in files {
          let p = Path::new(file);
          if !p.exists() {
            error!(cli.ci, "CBOR binary file {:?} does not exist", p);

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
            Ok(_) => {
              info!("Validation of {:?} is successful", p);
            }
            Err(e) => {
              error!(
                cli.ci,
                "Validation of {:?} failed: {}",
                p,
                e.to_string().trim_end()
              );
            }
          }
        }
      }

      if validate.stdin {
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
            Ok(_) => {
              info!("Validation from stdin is successful");
            }
            Err(e) => {
              error!(
                cli.ci,
                "Validation from stdin failed: {}",
                e.to_string().trim_end()
              );
            }
          }
        } else {
          #[cfg(feature = "additional-controls")]
          let c = validate_cbor_from_slice(&cddl_str, &data, enabled_features.as_deref());
          #[cfg(not(feature = "additional-controls"))]
          let c = validate_cbor_from_slice(&cddl_str, &data);

          match c {
            Ok(_) => {
              info!("Validation from stdin is successful");
            }
            Err(e) => {
              error!(
                cli.ci,
                "Validation from stdin failed: {}",
                e.to_string().trim_end()
              );
            }
          }
        }
      }
    }
  }

  Ok(())
}

#[cfg(target_arch = "wasm32")]
fn main() {}
