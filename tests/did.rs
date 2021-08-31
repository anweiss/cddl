#![cfg(feature = "std")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{validate_cbor_from_slice, validate_json_from_str};
use std::{
  error::Error,
  ffi::OsStr,
  fs::{self, File},
  io::Read,
};

static KNOWN_BAD: &'static [&'static str] = &[
  "bad_context.json",
  "example2.json",
  "example2.cbor",
  "example3.cbor",
  "example12.cbor",
  "example1a.cbor",
  "service_example1.json",
];

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
          let r = validate_json_from_str(&cddl, &fs::read_to_string(file.path())?, None);
          println!("assert ok {:?}", file.path());
          if let Err(e) = &r {
            println!("error validating {:?}\n", file.path());
            println!("{}", e);
          }

          // Files with known validation errors
          if KNOWN_BAD.contains(&file.file_name().to_str().unwrap()) {
            println!("assert error {:?}", file.path());
            assert!(r.is_err());
          } else {
            assert!(r.is_ok());
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
          let r = validate_cbor_from_slice(&cddl, &data);
          println!("assert ok {:?}", file.path());
          if let Err(e) = &r {
            println!("error validating {:?}\n", file.path());
            println!("{}", e);
          }

          // Files with known validation errors
          if KNOWN_BAD.contains(&file.file_name().to_str().unwrap()) {
            println!("assert error {:?}", file.path());
            assert!(r.is_err());
          } else {
            assert!(r.is_ok());
          }
        }
      }
    }
  }

  Ok(())
}
