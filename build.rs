use std::env;

fn main() {
  // Only generate the no_std parser when the `std` feature is NOT enabled.
  // When `std` is enabled, pest_derive's #[derive(Parser)] is used directly.
  let std_enabled = env::var("CARGO_FEATURE_STD").is_ok();
  if std_enabled {
    return;
  }

  #[cfg(feature = "_build-parser")]
  no_std_codegen::generate();

  #[cfg(not(feature = "_build-parser"))]
  panic!(
    "Building without the `std` feature requires the `_build-parser` feature \
     to generate a no_std-compatible parser. Use: --features _build-parser"
  );
}

#[cfg(feature = "_build-parser")]
mod no_std_codegen {
  use std::env;
  use std::fs;
  use std::iter::FromIterator;
  use std::path::Path;

  pub fn generate() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("pest_parser_generated.rs");

    let grammar_path = Path::new("cddl.pest");
    let grammar = fs::read_to_string(grammar_path).expect("Failed to read cddl.pest");

    // Use pest_generator to derive the parser implementation
    let name = quote_ident("CddlParser");
    let generated = pest_generator::derive_parser(
      // Build the token stream that pest_generator expects:
      // A struct with #[grammar_inline = "..."] attribute
      proc_macro2::TokenStream::from_iter(vec![
        // #[grammar_inline = "<grammar>"]
        proc_macro2::TokenTree::Punct(proc_macro2::Punct::new('#', proc_macro2::Spacing::Alone)),
        proc_macro2::TokenTree::Group(proc_macro2::Group::new(proc_macro2::Delimiter::Bracket, {
          let mut ts = proc_macro2::TokenStream::new();
          ts.extend(vec![
            proc_macro2::TokenTree::Ident(proc_macro2::Ident::new(
              "grammar_inline",
              proc_macro2::Span::call_site(),
            )),
            proc_macro2::TokenTree::Punct(proc_macro2::Punct::new(
              '=',
              proc_macro2::Spacing::Alone,
            )),
            proc_macro2::TokenTree::Literal(proc_macro2::Literal::string(&grammar)),
          ]);
          ts
        })),
        // pub struct CddlParser;
        proc_macro2::TokenTree::Ident(proc_macro2::Ident::new(
          "pub",
          proc_macro2::Span::call_site(),
        )),
        proc_macro2::TokenTree::Ident(proc_macro2::Ident::new(
          "struct",
          proc_macro2::Span::call_site(),
        )),
        name,
        proc_macro2::TokenTree::Punct(proc_macro2::Punct::new(';', proc_macro2::Spacing::Alone)),
      ]),
      false, // include_grammar = false since we inline it
    );

    let code = generated.to_string();

    // Replace ::std:: paths with no_std equivalents
    // proc-macro2's to_string() adds spaces between tokens
    let code = code
      .replace(":: std :: boxed :: Box", ":: alloc :: boxed :: Box")
      .replace(":: std :: result :: Result", ":: core :: result :: Result")
      .replace(":: std :: option :: Option", ":: core :: option :: Option")
      .replace("::std::boxed::Box", "::alloc::boxed::Box")
      .replace("::std::result::Result", "::core::result::Result")
      .replace("::std::option::Option", "::core::option::Option");

    // The generated code doesn't include the struct definition itself
    let code = format!("pub struct CddlParser;\n\n{}", code);

    fs::write(&dest_path, code).expect("Failed to write generated parser");

    println!("cargo::rerun-if-changed=cddl.pest");
    println!("cargo::rerun-if-changed=build.rs");
  }

  fn quote_ident(name: &str) -> proc_macro2::TokenTree {
    proc_macro2::TokenTree::Ident(proc_macro2::Ident::new(
      name,
      proc_macro2::Span::call_site(),
    ))
  }
}
