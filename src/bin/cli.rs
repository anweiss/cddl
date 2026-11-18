#![cfg(all(feature = "cbor", feature = "json", feature = "csv-validate"))]
#![cfg(not(feature = "lsp"))]
#![cfg(not(test))]
#![allow(unused_imports)]
#![allow(unused_macros)]

#[macro_use]
extern crate log;

use cddl::cddl_from_str;
#[cfg(all(feature = "modules", not(target_arch = "wasm32")))]
use cddl::modules::{resolve_modules, FsModuleSource, ModuleError, ResolveOptions};
#[cfg(not(target_arch = "wasm32"))]
use cddl::{
  parser::root_type_name_from_cddl_str, validate_cbor_from_slice, validate_csv_from_str,
  validate_json_from_str,
};
use clap::{ArgAction, ArgGroup, Args, Parser, Subcommand};

use simplelog::*;
use std::{
  error::Error,
  fs::{self, File},
  io::{self, BufReader, IsTerminal, Read},
  path::Path,
};

/// Help text for `--include-path`, whose separator is platform-specific: `;` on
/// Windows, where `:` occurs in drive-letter paths, and `:` elsewhere.
#[cfg(all(feature = "modules", windows))]
const INCLUDE_PATH_HELP: &str =
  "Semicolon-separated module search path; defaults to CDDL_INCLUDE_PATH, then \".;\"";

/// Help text for `--include-path`, whose separator is platform-specific: `;` on
/// Windows, where `:` occurs in drive-letter paths, and `:` elsewhere.
#[cfg(all(feature = "modules", not(windows)))]
const INCLUDE_PATH_HELP: &str =
  "Colon-separated module search path; defaults to CDDL_INCLUDE_PATH, then \".:\"";

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
  #[cfg(feature = "modules")]
  #[clap(
    name = "resolve-modules",
    about = "Resolve CDDL module directives (draft-ietf-cbor-cddl-modules) into basic RFC 8610 CDDL"
  )]
  ResolveModules {
    #[clap(
      short = 'c',
      long = "cddl",
      help = "Path to a module-structured CDDL document; omit or pass \"-\" to read stdin"
    )]
    file: Option<String>,
    #[clap(
      short = 'i',
      long = "import",
      help = "Synthesize an import directive: -icose=rfc9052 is shorthand for \";# import rfc9052 as cose\"",
      action = ArgAction::Append
    )]
    imports: Vec<String>,
    #[clap(
      short = 's',
      long = "start",
      help = "Emit a start (root) rule, as $.start.$ = <rule>"
    )]
    start: Option<String>,
    #[clap(
      long = "include-path",
      help = INCLUDE_PATH_HELP
    )]
    include_path: Option<String>,
  },
}

#[derive(Args)]
#[clap(about = "Validate JSON, CBOR, and/or CSV against a CDDL definition")]
#[clap(group(ArgGroup::new("targets").required(true).multiple(true).args(&["stdin", "json", "cbor", "csv"])))]
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
    action = ArgAction::Append
  )]
  json: Option<Vec<String>>,
  #[clap(
    short = 'c',
    long = "cbor",
    help = "CBOR binary file(s) to validate",
    action = ArgAction::Append,
    use_value_delimiter = true
  )]
  cbor: Option<Vec<String>>,
  #[clap(
    long = "csv",
    help = "CSV file(s) to validate (per draft-bormann-cbor-cddl-csv-08)",
    action = ArgAction::Append,
    use_value_delimiter = true
  )]
  csv: Option<Vec<String>>,
  #[clap(long = "csv-header", help = "Indicates the CSV file has a header row")]
  csv_header: bool,
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

  run_command(&Cli::parse())
}

#[cfg(not(target_arch = "wasm32"))]
fn run_command(cli: &Cli) -> Result<(), Box<dyn Error>> {
  match &cli.command {
    Commands::CompileCddl { file } => compile_cddl(file, cli.ci)?,
    Commands::CompileJson { file } => compile_json(file, cli.ci)?,
    Commands::Validate(validate) => validate_command(validate, cli.ci)?,
    #[cfg(feature = "modules")]
    Commands::ResolveModules {
      file,
      imports,
      start,
      include_path,
    } => resolve_modules_command(file.as_deref(), imports, start, include_path.as_deref())?,
  }

  Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
fn compile_cddl(file: &str, ci: bool) -> Result<(), Box<dyn Error>> {
  let path = Path::new(file);
  if !path.exists() {
    error!(ci, "CDDL document {:?} does not exist", path);
    return Ok(());
  }
  let file_content = fs::read_to_string(file)?;
  cddl_from_str(&file_content, true).map(|_| ())?;
  info!("{} is conformant", file);
  Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
fn compile_json(file: &str, ci: bool) -> Result<(), Box<dyn Error>> {
  let path = Path::new(file);
  if !path.exists() {
    error!(ci, "JSON document {:?} does not exist", path);
    return Ok(());
  }
  let file = File::open(file)?;
  let reader = BufReader::new(file);
  let _: serde_json::Value = serde_json::from_reader(reader)?;
  Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
fn validate_command(validate: &Validate, ci: bool) -> Result<(), Box<dyn Error>> {
  #[cfg(feature = "additional-controls")]
  let enabled_features = validate
    .features
    .as_ref()
    .map(|features| features.iter().map(String::as_str).collect::<Vec<_>>());

  #[cfg(feature = "additional-controls")]
  if let Some(features) = &enabled_features {
    log_enabled_features(features);
  }

  let path = Path::new(&validate.cddl);
  if !path.exists() {
    error!(ci, "CDDL document {:?} does not exist", path);
    return Ok(());
  }
  let cddl = fs::read_to_string(&validate.cddl)?;
  info!(
    "Root type for validation: {}",
    root_type_name_from_cddl_str(&cddl)?
  );
  validate_json_files(
    &cddl,
    validate.json.as_deref(),
    ci,
    #[cfg(feature = "additional-controls")]
    enabled_features.as_deref(),
  )?;
  validate_cbor_files(
    &cddl,
    validate.cbor.as_deref(),
    ci,
    #[cfg(feature = "additional-controls")]
    enabled_features.as_deref(),
  )?;
  validate_csv_files(
    &cddl,
    validate.csv.as_deref(),
    validate.csv_header,
    ci,
    #[cfg(feature = "additional-controls")]
    enabled_features.as_deref(),
  )?;
  if validate.stdin {
    validate_stdin(
      &cddl,
      ci,
      #[cfg(feature = "additional-controls")]
      enabled_features.as_deref(),
    )?;
  }
  Ok(())
}

#[cfg(all(not(target_arch = "wasm32"), feature = "additional-controls"))]
fn log_enabled_features(features: &[&str]) {
  let formatted = features
    .iter()
    .map(|feature| format!("\"{}\"", feature))
    .collect::<Vec<_>>()
    .join(", ");
  info!("enabled features: [{}]", formatted);
}

#[cfg(not(target_arch = "wasm32"))]
fn validate_json_files(
  cddl: &str,
  files: Option<&[String]>,
  ci: bool,
  #[cfg(feature = "additional-controls")] enabled_features: Option<&[&str]>,
) -> Result<(), Box<dyn Error>> {
  let Some(files) = files else {
    return Ok(());
  };
  for file in files {
    let path = Path::new(file);
    if !path.exists() {
      error!(ci, "File {:?} does not exist", path);
      continue;
    }
    let result = validate_json_from_str(
      cddl,
      &fs::read_to_string(file)?,
      #[cfg(feature = "additional-controls")]
      enabled_features,
    );
    report_file_result(result, path, ci)?;
  }
  Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
fn validate_cbor_files(
  cddl: &str,
  files: Option<&[String]>,
  ci: bool,
  #[cfg(feature = "additional-controls")] _enabled_features: Option<&[&str]>,
) -> Result<(), Box<dyn Error>> {
  let Some(files) = files else {
    return Ok(());
  };
  for file in files {
    let path = Path::new(file);
    if !path.exists() {
      error!(ci, "CBOR binary file {:?} does not exist", path);
      continue;
    }
    let mut file = File::open(path)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    let result = validate_cbor_from_slice(
      cddl,
      &data,
      #[cfg(feature = "additional-controls")]
      None,
    );
    report_file_result(result, path, ci)?;
  }
  Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
fn validate_csv_files(
  cddl: &str,
  files: Option<&[String]>,
  csv_header: bool,
  ci: bool,
  #[cfg(feature = "additional-controls")] enabled_features: Option<&[&str]>,
) -> Result<(), Box<dyn Error>> {
  let has_header = csv_header.then_some(true);
  let Some(files) = files else {
    return Ok(());
  };
  for file in files {
    let path = Path::new(file);
    if !path.exists() {
      error!(ci, "CSV file {:?} does not exist", path);
      continue;
    }
    let result = validate_csv_from_str(
      cddl,
      &fs::read_to_string(file)?,
      has_header,
      #[cfg(feature = "additional-controls")]
      enabled_features,
    );
    report_file_result(result, path, ci)?;
  }
  Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
fn report_file_result<T, E: std::fmt::Display>(
  result: Result<T, E>,
  path: &Path,
  ci: bool,
) -> Result<(), Box<dyn Error>> {
  match result {
    Ok(_) => info!("Validation of {:?} is successful", path),
    Err(error) => {
      error!(
        ci,
        "Validation of {:?} failed: {}",
        path,
        error.to_string().trim_end()
      );
    }
  }
  Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
fn validate_stdin(
  cddl: &str,
  ci: bool,
  #[cfg(feature = "additional-controls")] enabled_features: Option<&[&str]>,
) -> Result<(), Box<dyn Error>> {
  let mut data = Vec::new();
  io::stdin().lock().read_to_end(&mut data)?;
  if let Ok(json) = std::str::from_utf8(&data) {
    let result = validate_json_from_str(
      cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    report_stdin_result(result, ci)?;
  } else {
    let result = validate_cbor_from_slice(
      cddl,
      &data,
      #[cfg(feature = "additional-controls")]
      enabled_features,
    );
    report_stdin_result(result, ci)?;
  }
  Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
fn report_stdin_result<T, E: std::fmt::Display>(
  result: Result<T, E>,
  ci: bool,
) -> Result<(), Box<dyn Error>> {
  match result {
    Ok(_) => info!("Validation from stdin is successful"),
    Err(error) => {
      error!(
        ci,
        "Validation from stdin failed: {}",
        error.to_string().trim_end()
      );
    }
  }
  Ok(())
}

#[cfg(all(feature = "modules", not(target_arch = "wasm32")))]
fn resolve_modules_command(
  file: Option<&str>,
  imports: &[String],
  start: &Option<String>,
  include_path: Option<&str>,
) -> Result<(), Box<dyn Error>> {
  let input = match file {
    // With neither a file nor piped input there is no document: the `-i`/`-s`
    // shortcuts synthesize one (§2.7). Do not block reading a terminal.
    None if io::stdin().is_terminal() => String::new(),
    None | Some("-") => {
      let mut buffer = String::new();
      io::stdin()
        .read_to_string(&mut buffer)
        .map_err(|_| "module input could not be read")?;
      buffer
    }
    Some(path) => fs::read_to_string(path).map_err(|_| "module input could not be read")?,
  };

  let command_line_imports = imports
    .iter()
    .map(|spec| match spec.split_once('=') {
      Some((namespace, module)) if !namespace.is_empty() && !module.is_empty() => {
        Ok((namespace.to_string(), module.to_string()))
      }
      _ => Err("invalid --import; expected <namespace>=<module>"),
    })
    .collect::<Result<Vec<_>, _>>()?;

  let source = match include_path {
    Some(path) => FsModuleSource::from_include_path(path),
    None => FsModuleSource::from_env(),
  };

  let resolved = resolve_modules(
    &input,
    &source,
    &ResolveOptions {
      start_rule: start.clone(),
      command_line_imports,
    },
  )
  .map_err(module_error_message)?;

  print!("{}", resolved);
  Ok(())
}

#[cfg(all(feature = "modules", not(target_arch = "wasm32")))]
fn module_error_message(error: ModuleError) -> &'static str {
  match error {
    ModuleError::Directive { .. } => "not a valid start rule or module directive",
    ModuleError::ModuleNotFound { .. } => "module not found in the configured sources",
    ModuleError::ModuleUnreadable { .. } => "module source could not be read",
    ModuleError::ModuleParse { .. } => "module source is not valid CDDL",
    ModuleError::RuleNotFound { .. } => "selected rule not found in the module",
    ModuleError::CircularReference { .. } => "circular module reference",
  }
}
#[cfg(target_arch = "wasm32")]
fn main() {}
