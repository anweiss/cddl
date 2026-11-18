#![cfg(all(
  not(target_arch = "wasm32"),
  feature = "std",
  feature = "json",
  feature = "cbor",
  feature = "csv-validate",
  not(feature = "lsp")
))]

use std::{
  fs,
  io::Write,
  path::PathBuf,
  process::{Command, Output, Stdio},
  sync::atomic::{AtomicUsize, Ordering},
};

struct Fixtures(PathBuf);

impl Fixtures {
  fn new() -> Self {
    static NEXT_ID: AtomicUsize = AtomicUsize::new(0);
    let path = std::env::temp_dir().join(format!(
      "cddl-cli-{}-{}",
      std::process::id(),
      NEXT_ID.fetch_add(1, Ordering::Relaxed)
    ));
    fs::create_dir(&path).unwrap();
    let fixtures = Self(path);
    for (name, data) in [
      ("boolean.cddl", b"root = bool\n".as_slice()),
      ("rows.cddl", b"root = [* [tstr]]\n"),
      ("invalid.cddl", b"root = {"),
      ("non-utf8.cddl", b"\xff"),
      ("good.json", b"true"),
      ("wrong.json", b"1"),
      ("invalid.json", b"{"),
      ("good.cbor", b"\xf5"),
      ("wrong.cbor", b"\xf6"),
      ("invalid.cbor", b"\xff"),
      ("good.csv", b"hello\n"),
      ("wrong.csv", b"hello,world\n"),
      ("header.csv", b"1\nhello\n"),
    ] {
      fs::write(fixtures.0.join(name), data).unwrap();
    }
    fixtures
  }

  fn run(&self, ci: bool, args: &[&str], input: Option<&[u8]>) -> Output {
    let mut command = Command::new(env!("CARGO_BIN_EXE_cddl"));
    command.current_dir(&self.0);
    if ci {
      command.arg("--ci");
    }
    let mut child = command
      .args(args)
      .stdin(if input.is_some() {
        Stdio::piped()
      } else {
        Stdio::null()
      })
      .stdout(Stdio::piped())
      .stderr(Stdio::piped())
      .spawn()
      .unwrap();
    if let Some(input) = input {
      child.stdin.take().unwrap().write_all(input).unwrap();
    }
    child.wait_with_output().unwrap()
  }
}

impl Drop for Fixtures {
  fn drop(&mut self) {
    let _ = fs::remove_dir_all(&self.0);
  }
}

fn assert_exit(output: Output, code: i32) {
  assert_eq!(output.status.code(), Some(code), "{output:?}");
}

#[test]
fn compile_commands_propagate_parse_and_read_errors() {
  let fixtures = Fixtures::new();
  for ci in [false, true] {
    for (command, flag, valid, invalid) in [
      ("compile-cddl", "--cddl", "boolean.cddl", "invalid.cddl"),
      ("compile-json", "--json", "good.json", "invalid.json"),
    ] {
      assert_exit(fixtures.run(ci, &[command, flag, valid], None), 0);
      assert_exit(fixtures.run(ci, &[command, flag, invalid], None), 1);
      assert_exit(
        fixtures.run(ci, &[command, flag, "missing"], None),
        i32::from(ci),
      );
      assert_exit(fixtures.run(ci, &[command, flag, "."], None), 1);
    }
    assert_exit(
      fixtures.run(ci, &["compile-cddl", "--cddl", "non-utf8.cddl"], None),
      1,
    );
  }
}

#[test]
fn validate_files_preserves_ci_exit_codes() {
  let fixtures = Fixtures::new();
  for ci in [false, true] {
    for (schema, flag, file, valid) in [
      ("boolean.cddl", "--json", "good.json", true),
      ("boolean.cddl", "--json", "wrong.json", false),
      ("boolean.cddl", "--json", "invalid.json", false),
      ("boolean.cddl", "--json", "missing", false),
      ("boolean.cddl", "--cbor", "good.cbor", true),
      ("boolean.cddl", "--cbor", "wrong.cbor", false),
      ("boolean.cddl", "--cbor", "invalid.cbor", false),
      ("boolean.cddl", "--cbor", "missing", false),
      ("rows.cddl", "--csv", "good.csv", true),
      ("rows.cddl", "--csv", "wrong.csv", false),
      ("rows.cddl", "--csv", "header.csv", false),
      ("rows.cddl", "--csv", "missing", false),
    ] {
      assert_exit(
        fixtures.run(ci, &["validate", "--cddl", schema, flag, file], None),
        i32::from(ci && !valid),
      );
    }
    for (schema, code) in [("missing", i32::from(ci)), ("invalid.cddl", 1)] {
      assert_exit(
        fixtures.run(
          ci,
          &["validate", "--cddl", schema, "--json", "good.json"],
          None,
        ),
        code,
      );
    }
    assert_exit(
      fixtures.run(
        ci,
        &[
          "validate",
          "--cddl",
          "rows.cddl",
          "--csv",
          "header.csv",
          "--csv-header",
        ],
        None,
      ),
      0,
    );
  }
}

#[test]
fn validate_stdin_preserves_json_and_cbor_exit_codes() {
  let fixtures = Fixtures::new();
  for ci in [false, true] {
    for (input, valid) in [
      (b"true".as_slice(), true),
      (b"1", false),
      (b"{", false),
      (b"\xf5", true),
      (b"\xf6", false),
      (b"\xff", false),
    ] {
      assert_exit(
        fixtures.run(
          ci,
          &["validate", "--cddl", "boolean.cddl", "--stdin"],
          Some(input),
        ),
        i32::from(ci && !valid),
      );
    }
  }
}
