#![cfg(feature = "std")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{validate_cbor_from_slice, validate_json_from_str};
use std::{
  error::Error,
  ffi::OsStr,
  fs::{self, File},
  io::Read,
};

/// Convention: fixture files whose name starts with "bad-" are expected to fail
/// validation; all other fixture files are expected to pass.
fn is_expected_bad(file: &fs::DirEntry) -> bool {
  file
    .file_name()
    .to_str()
    .map(|n| n.starts_with("bad-"))
    .unwrap_or(false)
}

/// Read the single `.cddl` schema file from a fixture directory.
fn read_cddl_schema(dir: &std::path::Path) -> Result<String, Box<dyn Error>> {
  for file in fs::read_dir(dir)? {
    let file = file?;
    if file.path().extension().and_then(OsStr::to_str) == Some("cddl") {
      return Ok(fs::read_to_string(file.path())?);
    }
  }
  Err(format!("missing cddl file at {:?}", dir).into())
}

#[test]
fn validate_did_json_examples() -> Result<(), Box<dyn Error>> {
  for entry in fs::read_dir("tests/fixtures/did/")? {
    let entry = entry?;
    if !entry.file_type()?.is_dir() {
      continue;
    }

    let cddl = read_cddl_schema(&entry.path())?;

    for file in fs::read_dir(entry.path())? {
      let file = file?;
      if file.path().extension().and_then(OsStr::to_str) != Some("json") {
        continue;
      }

      #[cfg(feature = "additional-controls")]
      let r = validate_json_from_str(&cddl, &fs::read_to_string(file.path())?, None);
      #[cfg(not(feature = "additional-controls"))]
      let r = validate_json_from_str(&cddl, &fs::read_to_string(file.path())?);

      if is_expected_bad(&file) {
        println!("assert error {:?}", file.path());
        if r.is_ok() {
          panic!(
            "expected validation failure for {:?}, but it passed",
            file.path()
          );
        }
      } else {
        println!("assert ok {:?}", file.path());
        if let Err(e) = &r {
          panic!(
            "expected validation success for {:?}, but got error:\n{}",
            file.path(),
            e
          );
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
    if !entry.file_type()?.is_dir() {
      continue;
    }

    let cddl = read_cddl_schema(&entry.path())?;

    for file in fs::read_dir(entry.path())? {
      let file = file?;
      if file.path().extension().and_then(OsStr::to_str) != Some("cbor") {
        continue;
      }

      let mut f = File::open(file.path())?;
      let mut data = Vec::new();
      f.read_to_end(&mut data)?;
      #[cfg(feature = "additional-controls")]
      let r = validate_cbor_from_slice(&cddl, &data, None);
      #[cfg(not(feature = "additional-controls"))]
      let r = validate_cbor_from_slice(&cddl, &data);

      if is_expected_bad(&file) {
        println!("assert error {:?}", file.path());
        if r.is_ok() {
          panic!(
            "expected validation failure for {:?}, but it passed",
            file.path()
          );
        }
      } else {
        println!("assert ok {:?}", file.path());
        if let Err(e) = &r {
          panic!(
            "expected validation success for {:?}, but got error:\n{}",
            file.path(),
            e
          );
        }
      }
    }
  }

  Ok(())
}
