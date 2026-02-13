//! # cddl-rs
//!
//! [![crates.io](https://img.shields.io/crates/v/cddl.svg)](https://crates.io/crates/cddl)
//! [![docs.rs](https://docs.rs/cddl/badge.svg)](https://docs.rs/cddl)
//! [![Build and
//! Test](https://github.com/anweiss/cddl/workflows/Build%20and%20Test/badge.svg)](https://github.com/anweiss/cddl/actions?query=workflow%3A%22Build+and+Test%22)
//! [![Active
//! Development](https://img.shields.io/badge/Maintenance%20Level-Actively%20Developed-brightgreen.svg)](https://gist.github.com/cheerfulstoic/d107229326a01ff0f333a1d3476e068d)
//!
//! > This crate was originally developed as a personal learning exercise for
//! > getting acquainted with Rust and parsing in general. There are likely more
//! > performant and stable libraries out there for parsing CDDL. While there
//! > are some examples of this crate being used in production, careful
//! > consideration should be made prior to using this crate as such.
//!
//! A Rust implementation of the Concise data definition language (CDDL). CDDL
//! is an IETF standard that "proposes a notational convention to express CBOR
//! and JSON data structures." As of 2019-06-12, it is published as RFC 8610
//! (Proposed Standard) at
//! [https://tools.ietf.org/html/rfc8610](https://tools.ietf.org/html/rfc8610).
//!
//! This crate uses the [Pest](https://pest.rs/) parser generator for parsing
//! CDDL. The AST has been built to closely match the rules defined by the ABNF
//! grammar in [Appendix B.](https://tools.ietf.org/html/rfc8610#appendix-B) of
//! the spec. All CDDL must use UTF-8 for its encoding per the spec.
//!
//! This crate supports validation of both CBOR and JSON data structures. This
//! crate's minimum supported Rust version (MSRV) is 1.81.0.
//!
//! Also bundled into this repository is a basic language server implementation
//! and extension for Visual Studio Code for editing CDDL. The implementation is
//! backed by the compiled WebAssembly target included in this crate.
//!
//! ## Goals
//!
//! - [x] Parse CDDL documents into an AST
//! - [x] Verify conformance of CDDL documents against RFC 8610
//! - [x] Validate CBOR data structures
//! - [x] Validate JSON documents
//! - [ ] Generate dummy JSON from conformant CDDL
//! - [x] As close to zero-copy as possible
//! - [x] Compile WebAssembly target for browser and Node.js
//! - [x] Language server implementation and Visual Studio Code Extension
//!
//! ## Non-goals
//!
//! - Support CBOR diagnostic notation
//! - I-JSON compatibility
//!
//! ## Why Rust?
//!
//! Rust is a systems programming language designed around safety and is
//! ideally-suited for resource-constrained systems. CDDL and CBOR are designed
//! around small code and message sizes and constrained nodes, scenarios for
//! which Rust has also been designed.
//!
//! ## CLI
//!
//! A CLI is available for various platforms. The tool supports parsing of CDDL
//! files for verifying conformance against RFC 8610. It can also be used to
//! validate JSON documents and CBOR binary files against CDDL documents.
//! Detailed information about the JSON and CBOR validation implementation can
//! be found in the sections below.
//!
//! ### Installation
//!
//! #### GitHub Releases
//!
//! Binaries for Linux, macOS and Windows can be downloaded from GitHub
//! [Releases](https://github.com/anweiss/cddl/releases).
//!
//! #### Cargo
//!
//! ```sh
//! cargo install cddl
//! ```
//!
//! #### Docker
//!
//! ```sh
//! docker pull ghcr.io/anweiss/cddl-cli:latest
//! ```
//!
//! ### CLI usage
//!
//! Instructions for using the tool can be viewed by executing the `help`
//! subcommand:
//!
//! ```sh
//! cddl help
//! ```
//!
//! If using Docker:
//!
//! > Replace `<version>` with an appropriate
//! > [release](https://github.com/anweiss/cddl/releases) tag. Requires use of
//! > the `--volume` argument for mounting `.cddl` documents into the container
//! > when executing the command. JSON or CBOR files can either be included in
//! > the volume mount or passed into the command via STDIN.
//!
//! ```sh
//! docker run -it --rm -v $PWD:/cddl -w /cddl ghcr.io/anweiss/cddl-cli:<version> help
//! ```
//!
//! You can validate JSON documents and/or CBOR binary files:
//!
//! ```sh
//! cddl validate [OPTIONS] --cddl <CDDL> <--stdin|--json <JSON>...|--cbor <CBOR>...>
//! ```
//!
//! It also supports validating files from STDIN (if it detects the input as
//! valid UTF-8, it will attempt to validate the input as JSON, otherwise it
//! will treat it as CBOR):
//!
//! ```sh
//! cat reputon.json | cddl validate --cddl reputon.cddl --stdin
//! cat reputon.cbor | cddl validate --cddl reputon.cddl --stdin
//! ```
//!
//! or using Docker:
//!
//! ```sh
//! docker run -i --rm -v $PWD:/data -w /data ghcr.io/anweiss/cddl-cli:0.10.1 validate --cddl reputon.cddl --stdin < reputon.json
//! ```
//!
//! ## Website
//!
//! You can also find a simple RFC 8610 conformance tool at
//! [https://cddl.anweiss.tech](https://cddl.anweiss.tech). This same codebase
//! has been compiled for use in the browser via WebAssembly.
//!
//! ## Visual Studio Code extension
//!
//! An extension for editing CDDL documents with Visual Studio Code has been
//! published to the Marketplace
//! [here](https://marketplace.visualstudio.com/items?itemName=anweiss.cddl-languageserver).
//! You can find more information in the [README](cddl-lsp/README.md).
//!
//! ## Supported features
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
//! - [x] tags
//! - [x] unwrapping
//! - [x] controls
//! - [x] socket/plug
//! - [x] generics
//! - [x] operator precedence
//! - [x] comments
//! - [x] numerical int/uint values
//! - [x] numerical hexfloat values
//! - [x] numerical values with exponents
//! - [x] unprefixed byte strings
//! - [x] prefixed byte strings
//!
//! ## Usage
//!
//! Simply add the dependency to `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! cddl = "0.10.1"
//! ```
//!
//! ### Feature flags
//!
//! A few convenience features have been included to make the AST more concise
//! and for enabling additional functionality.
//!
//! **`--feature ast-span`**
//!
//! Add the `Span` type to the AST for keeping track of the position of the
//! lexer and parser. Enabled by default.
//!
//! **`--feature ast-comments`**
//!
//! Include comment strings in the AST. Enabled by default.
//!
//! **`--feature ast-parent`**
//!
//! Add the `ParentVisitor` implementation so that the AST can be traversed
//! using parent pointers. Enabled by default.
//!
//! **`--feature json`**
//!
//! Enable JSON validation. Enabled by default.
//!
//! **`--feature cbor`**
//!
//! Enable CBOR validation. Enabled by default.
//!
//! **`--feature additional-controls`**
//!
//! Enable validation support for the additional control operators defined in
//! [RFC 9165](https://datatracker.ietf.org/doc/html/rfc9165). Enabled by
//! default.
//!
//! ### Parsing CDDL
//!
//! ```rust
//! use cddl::cddl_from_str;
//!
//! let input = r#"myrule = int"#;
//! assert!(cddl_from_str(input, true).is_ok())
//! ```
//!
//! ### Validating JSON
//!
//! ```rust
//! use cddl::validate_json_from_str;
//!
//! let cddl = r#"person = {
//!   name: tstr,
//!   age: uint,
//!   address: tstr,
//! }"#;
//!
//! let json = r#"{
//!   "name": "John",
//!   "age": 50,
//!   "address": "1234 Lakeshore Dr"
//! }"#;
//!
//! #[cfg(not(feature = "additional-controls"))]
//! assert!(validate_json_from_str(cddl, json).is_ok())
//! ```
//!
//! This crate uses the [Serde](https://serde.rs/) framework, and more
//! specifically, the [serde_json](https://crates.io/crates/serde_json) crate,
//! for parsing and validating JSON. Serde was chosen due to its maturity in the
//! ecosystem and its support for serializing and deserializing CBOR via the
//! [ciborium](https://crates.io/crates/ciborium) crate.
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
//! Furthermore, the following data types from the standard prelude can be used
//! for validating JSON strings and numbers:
//!
//! ```cddl
//! tdate = #6.0(tstr)
//! uri = #6.32(tstr)
//! b64url = #6.33(tstr)
//! time = #6.1(number)
//! ```
//!
//! The first non-group rule defined by a CDDL data structure definition
//! determines the root type, which is subsequently used for validating the
//! top-level JSON data type.
//!
//! #### Supported JSON validation features
//!
//! The following types and features of CDDL are supported by this crate for
//! validating JSON:
//!
//! | CDDL                   | JSON                                                        |
//! | ---------------------- | ----------------------------------------------------------- |
//! | structs                | objects                                                     |
//! | arrays                 | arrays<sup>[1](#arrays)</sup>                               |
//! | `text / tstr`          | string                                                      |
//! | `uri`                  | string (valid RFC3986 URI)                                  |
//! | `tdate`                | string (valid RFC3339 date/time)                            |
//! | `b64url`               | string (base64url-encoded)                                  |
//! | `time`                 | number (valid UNIX timestamp integer in seconds)            |
//! | `number / int / float` | number<sup>[2](#number)</sup>                               |
//! | `bool / true / false`  | boolean                                                     |
//! | `null / nil`           | null                                                        |
//! | `any`                  | any valid JSON                                              |
//! | byte strings           | not yet implemented                                         |
//! | unwrap (`~`)           | any JSON that matches unwrapped type from map, array or tag |
//!
//! CDDL groups, generics, sockets/plugs and group-to-choice enumerations can
//! all be used when validating JSON.
//!
//! Since JSON objects only support keys whose types are JSON strings, when
//! validating JSON, member keys defined in CDDL structs must use either the
//! colon syntax (`mykey: tstr` or `"mykey": tstr`) or the double arrow syntax
//! provided that the member key is either a text string value (`"mykey" =>
//! tstr`) or a bareword that resolves to either a string data type (`text` or
//! `tstr`) or another text string value (`* tstr => any`).
//!
//! Occurrence indicators can be used to validate key/value pairs in a JSON
//! object and the number of elements in a JSON array; depending on how the
//! indicators are defined in a CDDL data definition.
//!
//! Below is the table of supported control operators:
//!
//! | Control operator | Supported                                                                                                                                                                                   |
//! | ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
//! | `.pcre`          | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji><sup>[3](#regex)</sup>                     |
//! | `.regex`         | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji><sup>[3](#regex)</sup> (alias for `.pcre`) |
//! | `.size`          | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
//! | `.bits`          | Ignored when validating JSON                                                                                                                                                                |
//! | `.cbor`          | Ignored when validating JSON                                                                                                                                                                |
//! | `.cborseq`       | Ignored when validating JSON                                                                                                                                                                |
//! | `.within`        | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
//! | `.and`           | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
//! | `.lt`            | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
//! | `.le`            | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
//! | `.gt`            | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
//! | `.ge`            | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
//! | `.eq`            | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
//! | `.ne`            | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
//! | `.default`       | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
//!
//! <a name="arrays">1</a>: When groups with multiple group entries are used to
//! validate arrays, occurrence indicators are "greedy" in that only the first
//! occurrence indicator that is come across is used in the validation.
//! Subsequent entries with occurrence indicators are ignored due to
//! complexities involved with processing these ambiguities. For proper JSON
//! validation, avoid writing CDDL that looks like the following: `[ * a: int,
//! b: tstr, ? c: int ]`.
//!
//! <a name="number">2</a>: While JSON itself does not distinguish between
//! integers and floating-point numbers, this crate does provide the ability to
//! validate numbers against a more specific numerical CBOR type, provided that
//! its equivalent representation is allowed by JSON. Refer to [Appendix
//! E.](https://tools.ietf.org/html/rfc8610#appendix-E) of the standard for more
//! details on the implications of using CDDL with JSON numbers.
//!
//! <a name="regex">3</a>: Due to Perl-Compatible Regular Expressions (PCREs)
//! being more widely used than XSD regular expressions, this crate also
//! provides support for the proposed `.pcre` control extension in place of the
//! `.regexp` operator (see
//! [Discussion](https://tools.ietf.org/html/rfc8610#section-3.8.3.2) and
//! [CDDL-Freezer
//! proposal](https://tools.ietf.org/html/draft-bormann-cbor-cddl-freezer-03#section-5.1)).
//! Ensure that your regex string is properly JSON escaped when using this
//! control.
//!
//! If you've enabled the `additional-controls` feature, the table of controls
//! below is also available for use:
//!
//! | Control operator | Supported                                                                                                                                         |
//! | ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
//! | `.plus`          | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `.cat`           | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `.det`           | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `.abnf`          | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `.abnfb`         | Ignored when validating JSON                                                                                                                      |
//! | `.feature`       | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//!
//! You can activate features during validation as follows:
//!
//! ```rust
//! use cddl::validate_json_from_str;
//!
//! let cddl = r#"
//!   v = JC<"v", 2>
//!   JC<J, C> =  C .feature "cbor" / J .feature "json"
//! "#;
//!
//! let json = r#""v""#;
//!
//! #[cfg(not(feature = "additional-controls"))]
//! assert!(validate_json_from_str(cddl, json, Some(&["json"])).is_ok())
//! ```
//!
//! #### Comparing with JSON schema and JSON schema language
//!
//! [CDDL](https://tools.ietf.org/html/rfc8610), [JSON
//! schema](https://json-schema.org/) and [JSON schema
//! language](https://tools.ietf.org/html/draft-json-schema-language-02) can all
//! be used to define JSON data structures. However, the approaches taken to
//! develop each of these are vastly different. A good place to find past
//! discussions on the differences between these formats is the [IETF mail
//! archive](https://mailarchive.ietf.org/arch/), specifically in the JSON and
//! CBOR lists. The purpose of this crate is not to argue for the use of CDDL
//! over any one of these formats, but simply to provide an example
//! implementation in Rust.
//!
//! ### Validating CBOR
//!
//! ```rust
//! use cddl::validate_cbor_from_slice;
//!
//! let cddl = r#"rule = false"#;
//!
//! let cbor = b"\xF4";
//!
//! #[cfg(not(feature = "additional-controls"))]
//! assert!(validate_cbor_from_slice(cddl, cbor).is_ok())
//! ```
//!
//! This crate also uses [Serde](https://serde.rs/) and
//! [ciborium](https://crates.io/crates/ciborium) for validating CBOR data
//! structures. CBOR validation is done via the loosely typed
//! [`ciborium::value::Value`](https://github.com/enarx/ciborium/blob/main/ciborium/src/value/mod.rs#L22)
//! enum. In addition to all of the same features implemented by the JSON
//! validator, this crate also supports validating CBOR tags (e.g.
//! `#6.32(tstr)`), CBOR major types (e.g. `#1.2`), table types (e.g. `{ [ +
//! tstr ] => int }`) and byte strings. The `.bits`, `.cbor` and `.cborseq`
//! control operators are all supported as well.
//!
//! The following tags are supported when validating CBOR:
//!
//! | Tag                                      | Supported                                                                                                                                         |
//! | ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
//! | `tdate = #6.0(tstr)`                     | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `time = #6.1(number)`                    | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `biguint = #6.2(bstr)`                   | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `bignint = #6.3(bstr)`                   | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `decfrac = #6.4([e10: int, m: integer])` | not yet implemented                                                                                                                               |
//! | `bigfloat = #6.5([e2: int, m: integer])` | not yet implemented                                                                                                                               |
//! | `eb64url = #6.21(any)`                   | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `eb64legacy = #6.22(any)`                | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `eb16 = #6.23(any)`                      | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `encoded-cbor = #6.24(bstr)`             | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `uri = #6.32(tstr)`                      | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `b64url = #6.33(tstr)`                   | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `b64legacy = #6.34(tstr)`                | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `regexp = #6.35(tstr)`                   | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `mime-message = #6.36(tstr)`             | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `cbor-any = #6.55799(any)`               | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//!
//! If you've enabled the `additional-controls` feature, the table of controls
//! below is also available for use:
//!
//! | Control operator | Supported                                                                                                                                         |
//! | ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
//! | `.plus`          | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `.cat`           | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `.det`           | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `.abnf`          | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `.abnfb`         | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//! | `.feature`       | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji> |
//!
//! You can activate features during validation by passing a slice of feature
//! strings as follows:
//!
//! ```rust
//! use cddl::validate_cbor_from_slice;
//!
//! let cddl = r#"
//!   v = JC<"v", 2>
//!   JC<J, C> =  C .feature "cbor" / J .feature "json"
//! "#;
//!
//! let cbor = b"\x02";
//!
//! assert!(validate_cbor_from_slice(cddl, cbor, Some(&["cbor"])).is_ok())
//! ```
//!
//! ## Projects using this crate
//!
//! Below are some known projects that leverage this crate:
//!
//! - [https://github.com/Emurgo/cddl-codegen](https://github.com/Emurgo/cddl-codegen)
//! - [https://github.com/p2panda/p2panda](https://github.com/p2panda/p2panda)
//!

#![allow(dead_code)]
#![warn(missing_docs)]

#[cfg(feature = "std")]
extern crate serde_json;

#[cfg(feature = "std")]
extern crate uriparse;

#[cfg(feature = "std")]
extern crate base64_url;

/// Abstract syntax tree representing a CDDL definition
pub mod ast;
/// Static error messages
#[allow(missing_docs)]
pub mod error;
/// Lexer types (position information)
pub mod lexer;
/// Parser for CDDL
pub mod parser;
/// Bridge layer between Pest parser and existing AST
pub mod pest_bridge;
/// Pest-based parser for CDDL
pub mod pest_parser;
/// CDDL tokens
pub mod token;
/// Validators for JSON and CBOR data structures
#[cfg(feature = "std")]
pub mod validator;

/// CDDL AST visitor
pub mod visitor;

#[cfg(not(target_arch = "wasm32"))]
mod parser_tests;

#[doc(inline)]
pub use self::{
  parser::{cddl_from_str, Error},
  token::Token,
};

#[doc(inline)]
#[cfg(feature = "std")]
#[cfg(feature = "cbor")]
#[cfg(not(feature = "lsp"))]
#[cfg(not(target_arch = "wasm32"))]
pub use self::validator::validate_cbor_from_slice;

#[doc(inline)]
#[cfg(feature = "std")]
#[cfg(feature = "json")]
#[cfg(not(feature = "lsp"))]
#[cfg(not(target_arch = "wasm32"))]
pub use self::validator::validate_json_from_str;

#[doc(inline)]
#[cfg(feature = "std")]
#[cfg(feature = "csv-validate")]
#[cfg(not(feature = "lsp"))]
#[cfg(not(target_arch = "wasm32"))]
pub use self::validator::validate_csv_from_str;
