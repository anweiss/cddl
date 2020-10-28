use cddl::{validate_cbor_from_slice, validate_json_from_str};
use std::{
  error::Error,
  ffi::OsStr,
  fs::{self, File},
  io::Read,
};

#[test]
fn validate_did_json_examples() -> Result<(), Box<dyn Error>> {
  for entry in fs::read_dir("tests/fixtures/did/")? {
    let entry = entry?;
    if entry.file_type()?.is_dir() {
      let mut cddl = String::new();

      for file in fs::read_dir(entry.path())? {
        let file = file?;
        if file.path().extension().and_then(OsStr::to_str).unwrap() == "cddl" {
          cddl = fs::read_to_string(file.path())?;
          break;
        }
      }

      if cddl.is_empty() {
        return Err(format!("missing cddl file at {:?}", entry.path()).into());
      }

      for file in fs::read_dir(entry.path())? {
        let file = file?;
        if file.path().extension().and_then(OsStr::to_str).unwrap() == "json" {
          if let Err(e) = validate_json_from_str(&cddl, &fs::read_to_string(file.path())?) {
            println!("{}", e);
          }
        }
      }
    }
  }

  Ok(())
}

#[test]
fn validate_did_cbor_examples() -> Result<(), Box<dyn Error>> {
  for entry in fs::read_dir("tests/fixtures/did/")? {
    let entry = entry?;
    if entry.file_type()?.is_dir() {
      let mut cddl = String::new();

      for file in fs::read_dir(entry.path())? {
        let file = file?;
        if file.path().extension().and_then(OsStr::to_str).unwrap() == "cddl" {
          cddl = fs::read_to_string(file.path())?;
          break;
        }
      }

      if cddl.is_empty() {
        return Err(format!("missing cddl file at {:?}", entry.path()).into());
      }

      for file in fs::read_dir(entry.path())? {
        let file = file?;
        if file.path().extension().and_then(OsStr::to_str).unwrap() == "cbor" {
          let mut f = File::open(file.path())?;
          let mut data = Vec::new();
          f.read_to_end(&mut data)?;
          if let Err(e) = validate_cbor_from_slice(&cddl, &data) {
            println!("{}", e);
          }
        }
      }
    }
  }

  Ok(())
}
