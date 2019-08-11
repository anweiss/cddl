//! # cddl
//!
//! > This crate is very much experimental and is being developed as a personal
//! > learning exercise for getting acquainted with Rust and about parsing in
//! > general. It is far from complete. There are likely more performant and
//! > stable libraries out there for parsing CDDL. This one should not be used
//! > in production in any form or fashion.
//!
//! An implementation of the Concise data definition language (CDDL). CDDL is an
//! IETF standard that "proposes a notational convention to express CBOR and
//! JSON data structures." As of 2019-06-12, it is published as RFC 8610
//! (Proposed Standard) at https://tools.ietf.org/html/rfc8610.
//!
//! This crate includes a handwritten parser and lexer for CDDL and is heavily
//! inspired by Thorsten Ball's book ["Writing An Interpretor In
//! Go"](https://interpreterbook.com/). The AST has been built to closely match
//! the rules defined by the ABNF grammar in [Appendix
//! B.](https://tools.ietf.org/html/rfc8610#appendix-B) of the spec. All CDDL
//! must use UTF-8 for its encoding per the spec.
//!
//! ## Goals
//!
//! - Parse CDDL documents into an AST
//! - Verify conformance of CDDL documents against RFC 8610
//! - Validate CBOR data structures
//! - Validate JSON documents
//! - Basic REPL
//! - Generate dummy JSON from conformant CDDL
//! - Close to zero-copy as possible
//! - Compile for use in the browser with WebAssembly
//! - `no_std` support (lexing and parsing only)
//!
//! ## Non-goals
//!
//! - Performance (if this crate gains enough traction, it may be prudent to
//!   explore using a parser-combinator framework like
//!   [nom](https://github.com/Geal/nom))
//! - Support CBOR diagnostic notation
//! - I-JSON compatibility
//!
//! ## Features supported by the parser
//!
//! - [x] maps
//!   - [x] structs
//!   - [x] tables
//!   - [x] cuts
//! - [x] groups
//! - [x] arrays
//! - [x] values
//! - [x] choices
//! - [x] ranges
//! - [x] enumeration (building a choice from a group)
//! - [x] root type
//! - [x] occurrence
//! - [x] predefined types
//! - [X] tags
//! - [x] unwrapping
//! - [x] controls
//! - [x] socket/plug
//! - [x] generics
//! - [ ] operator precedence
//! - [x] comments
//! - [x] numerical int/uint values
//! - [ ] numerical hexfloat values
//! - [ ] numerical values with exponents
//!
//! ## Validating JSON
//!
//! > Incomplete. Under development
//!
//! This crate uses the [Serde](https://serde.rs/) framework, and more
//! specifically, the [serde_json](https://crates.io/crates/serde_json) crate,
//! for parsing and validating JSON. Serde was chosen due to its maturity in the
//! ecosystem and its support for serializing and deserializing CBOR via the
//! [serde_cbor](https://crates.io/crates/serde_cbor) crate.
//!
//! As outlined in [Appendix E.](https://tools.ietf.org/html/rfc8610#appendix-E)
//! of the standard, only the JSON data model subset of CBOR can be used for
//! validation. The limited prelude from the spec has been included below for
//! brevity:
//!
//! ```cddl
//! any = #
//!
//! uint = #0
//! nint = #1
//! int = uint / nint
//!
//! tstr = #3
//! text = tstr
//!
//! number = int / float
//!
//! float16 = #7.25
//! float32 = #7.26
//! float64 = #7.27
//! float16-32 = float16 / float32
//! float32-64 = float32 / float64
//! float = float16-32 / float64
//!
//! false = #7.20
//! true = #7.21
//! bool = false / true
//! nil = #7.22
//! null = nil
//! ```
//!
//! The first non-group rule defined by a CDDL data structure definition
//! determines the root type, which is subsequently used for validating the
//! top-level JSON data type.
//!
//! ### Supported JSON validation features
//!
//! The following types and features of CDDL are supported by this crate for
//! validating JSON:
//!
//! |CDDL|JSON|
//! |----|----|
//! |structs|objects|
//! |arrays|arrays|
//! |text / tstr|string|
//! |number / int / float|number*|
//! |bool / true / false|boolean|
//! |null / nil|null|
//! |any|any valid JSON|
//!
//! Occurrence indicators can be used to validate key/value pairs in a JSON
//! object and the number of elements in a JSON array; depending on how the
//! indicators are defined in a CDDL data definition. CDDL groups, generics,
//! sockets/plugs and group-to-choice enumerations are all parsed into their
//! full representations before being evaluated for JSON validation.
//!
//! All CDDL control operators can be used for validating JSON, with the
//! exception of the `.cbor` and `.cborseq` operators.
//!
//! *Note: While JSON itself does not distinguish between integers and
//! floating-point numbers, this crate does provide the ability to validate
//! numbers against a more specific numerical CBOR type, provided that its
//! equivalent representation is allowed by JSON.
//!
//! ### Comparing with JSON schema and JSON schema language
//!
//! [CDDL](https://www.rfc-editor.org/rfc/rfc8610.html), [JSON
//! schema](https://json-schema.org/) and [JSON schema
//! language](https://tools.ietf.org/html/draft-json-schema-language-02) can all
//! be used to define JSON data structures. However, the approaches taken to
//! develop each of these are vastly different. A good place to find past
//! discussions on the differences between thse formats is the [IETF mail
//! archive](https://mailarchive.ietf.org/arch/), specifically in the JSON and
//! CBOR lists. The purpose of this crate is not to argue for the use of CDDL
//! over any one of these formats, but simply to provide an example
//! implementation in Rust.
//!
//! ## Validating CBOR
//!
//! > Incomplete. Under development
//!
//! This crate also uses [Serde](https://serde.rs/) and
//! [serde_cbor](https://crates.io/crates/serde_cbor) for validating CBOR data
//! structures. Similary to the JSON validation implementation, CBOR validation
//! is done via the loosely typed
//! [`Value`](https://docs.rs/serde_cbor/0.10.1/serde_cbor/enum.Value.html)
//! enum. Unfortunately, due to a [limitation of
//! Serde](https://github.com/pyfisch/cbor/issues/3), CBOR tags are ignored
//! during deserialization.
//!
//! ## `no_std` support
//!
//! Only the lexer and parser can be used in a `no_std` context provided that a
//! heap allocator is available. This can be enabled by opting out of the
//! default features in your `Cargo.toml` file as follows:
//!
//! ```toml
//! [dependencies]
//! cddl = { version = "<version>", default-features = false }
//! ```
//!
//! Zero-copy parsing is implemented to the extent that is possible, with
//! prefixed byte strings containing whitespace being one of the few exceptions
//! where allocation is required.
//!
//! Both JSON and CBOR validation are dependent on their respective heap
//! allocated `Value` types, but since these types aren't supported in a
//! `no_std` context, they subsequently aren't supported in a `no_std` context
//! in this crate.f

#![allow(dead_code)]
#![cfg_attr(not(feature = "std"), no_std)]
#![warn(missing_docs)]
// #![feature(box_patterns)]

#[macro_use]
#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
extern crate core as std;

#[cfg(feature = "std")]
extern crate serde_json;

#[cfg(feature = "std")]
extern crate serde_cbor;

/// Abstract syntax tree representing a CDDL definition
pub mod ast;
/// Lexer for CDDL
pub mod lexer;
/// Parser for CDDL
pub mod parser;
/// Basic REPL for CDDL lexing
pub mod repl;
/// CDDL tokens for lexing
pub mod token;
/// Validation against various data structures (e.g. JSON, CBOR)
#[cfg(feature = "std")]
pub mod validation;

#[doc(inline)]
pub use self::{
  lexer::LexerError, parser::compile_cddl_from_str, parser::ParserError, token::Token,
};

#[doc(inline)]
#[cfg(feature = "std")]
pub use self::validation::{
  cbor::{self as cbor_validator, validate_cbor_from_slice},
  json::{self as json_validator, validate_json_from_str},
  Error as ValidationError, Validator,
};
