use std::{env, error::Error, fs, path::PathBuf};

fn main() -> Result<(), Box<dyn Error>> {
  println!("cargo:rerun-if-changed=schema.cddl");
  let schema = fs::read_to_string("schema.cddl")?;
  let output = PathBuf::from(env::var_os("OUT_DIR").ok_or("OUT_DIR not set")?);
  fs::write(
    output.join("generated.rs"),
    cddl_codegen_rust::generate_rust_code(&schema)?,
  )?;
  let mut options = cddl_codegen_rust::CodegenOptions::default();
  options.fundamental_aliases = true;
  fs::write(
    output.join("fundamental.rs"),
    cddl_codegen_rust::generate_rust_code_with_options(&schema, &options)?,
  )?;
  Ok(())
}
