# cddl-rs

[![crates.io](https://img.shields.io/crates/v/cddl.svg)](https://crates.io/crates/cddl) [![docs.rs](https://docs.rs/cddl/badge.svg)](https://docs.rs/cddl) [![Publish packages](https://github.com/anweiss/cddl/workflows/Publish%20packages/badge.svg?branch=0.6.0&event=release)](https://github.com/anweiss/cddl/actions?query=workflow%3A%22Publish+packages%22) [![Build and Test](https://github.com/anweiss/cddl/workflows/Build%20and%20Test/badge.svg)](https://github.com/anweiss/cddl/actions?query=workflow%3A%22Build+and+Test%22)

> This crate is very much experimental and is being developed as a personal learning exercise for getting acquainted with Rust and about parsing in general. It does not yet completely conform to the spec. There are likely more performant and stable libraries out there for parsing CDDL. This one should not be used in production in any form or fashion.

A Rust implementation of the Concise data definition language (CDDL). CDDL is an IETF standard that "proposes a notational convention to express CBOR and JSON data structures." As of 2019-06-12, it is published as RFC 8610 (Proposed Standard) at https://tools.ietf.org/html/rfc8610.

This crate includes a handwritten parser and lexer for CDDL and is heavily inspired by the techniques outlined in Thorsten Ball's book ["Writing An Interpretor In Go"](https://interpreterbook.com/). The AST has been built to closely match the rules defined by the ABNF grammar in [Appendix B.](https://tools.ietf.org/html/rfc8610#appendix-B) of the spec. All CDDL must use UTF-8 for its encoding per the spec.

This crate supports validation of both CBOR and JSON data structures. An extremely basic REPL is included as well, along with a compiled WebAssembly target. This crate requires Rust v1.37.0+.

## Goals

- Parse CDDL documents into an AST
- Verify conformance of CDDL documents against RFC 8610
- Validate CBOR data structures
- Validate JSON documents
- Basic REPL
- Generate dummy JSON from conformant CDDL
- Close to zero-copy as possible
- Compile WebAssembly target for browser and Node.js
- `no_std` support (lexing and parsing only)

## Non-goals

- Performance (if this crate gains enough traction, it may be prudent to explore using a parser-combinator framework like [nom](https://github.com/Geal/nom))
- Support CBOR diagnostic notation
- I-JSON compatibility

## Why Rust?

Rust is a systems programming language designed around safety and is ideally-suited for resource-constrained systems. CDDL and CBOR are designed around small code and message sizes and constrained nodes, scenarios that Rust has also been designed for.

## CLI

A CLI has been made available for various platforms and as a Docker image. It can downloaded from the [Releases](https://github.com/anweiss/cddl/releases) tab. The tool supports parsing of `.cddl` files for verifying conformance against RFC 8610. It also supports validation of `.cddl` documents against `.json` files. Detailed information about the JSON validation functions can be found in the [validating JSON](#validating-json) section below. Instructions for using the tool can be viewed by executing the `help` subcommand:

    $ cddl help

If using Docker:

> Ensure your Docker client has been [authenticated](https://help.github.com/en/articles/configuring-docker-for-use-with-github-package-registry#authenticating-to-github-package-registry) into GitHub Package Registry. Replace `<version>` with an appropriate [release](https://github.com/anweiss/cddl/releases) tag. Requires use of the `--volume` argument for mounting `.cddl` and `.json` documents into the container when executing the command. The command below assumes these documents are in your current working directory.

    $ docker run -it --rm -v $PWD:/cddl -w /cddl docker.pkg.github.com/anweiss/cddl/cddl:<version> help

## Website

You can also find a simple RFC 8610 conformance tool at https://cddl.anweiss.tech. This same codebase has been compiled for use in the browser via WebAssembly.

## Features supported by the parser

- [x] maps
  - [x] structs
  - [x] tables
  - [x] cuts
- [x] groups
- [x] arrays
- [x] values
- [x] choices
- [x] ranges
- [x] enumeration (building a choice from a group)
- [x] root type
- [x] occurrence
- [x] predefined types
- [x] tags
- [x] unwrapping
- [x] controls
- [x] socket/plug
- [x] generics
- [x] operator precedence
- [x] comments
- [x] numerical int/uint values
- [ ] numerical hexfloat values
- [ ] numerical values with exponents
- [x] unprefixed byte strings
- [x] prefixed byte strings

## Validating JSON

> Incomplete. Under development

This crate uses the [Serde](https://serde.rs/) framework, and more specifically, the [serde_json](https://crates.io/crates/serde_json) crate, for parsing and validating JSON. Serde was chosen due to its maturity in the ecosystem and its support for serializing and deserializing CBOR via the [serde_cbor](https://crates.io/crates/serde_cbor) crate.

As outlined in [Appendix E.](https://tools.ietf.org/html/rfc8610#appendix-E) of the standard, only the JSON data model subset of CBOR can be used for validation. The limited prelude from the spec has been included below for brevity:

```cddl
any = #

uint = #0
nint = #1
int = uint / nint

tstr = #3
text = tstr

number = int / float

float16 = #7.25
float32 = #7.26
float64 = #7.27
float16-32 = float16 / float32
float32-64 = float32 / float64
float = float16-32 / float64

false = #7.20
true = #7.21
bool = false / true
nil = #7.22
null = nil
```

Furthermore, the following data types from the standard prelude can be used to validate JSON strings:

```cddl
tdate = #6.0(tstr)
uri = #6.32(tstr)
```

The first non-group rule defined by a CDDL data structure definition determines the root type, which is subsequently used for validating the top-level JSON data type.

### Supported JSON validation features

The following types and features of CDDL are supported by this crate for validating JSON:

| CDDL                 | JSON                          |
| -------------------- | ----------------------------- |
| structs              | objects                       |
| arrays               | arrays<sup>[1](#arrays)</sup> |
| text / tstr          | string                        |
| number / int / float | number<sup>[2](#number)</sup> |
| bool / true / false  | boolean                       |
| null / nil           | null                          |
| any                  | any valid JSON                |

Since JSON objects only support keys whose types are JSON strings, member keys defined in CDDL structs must use either the colon syntax (`mykey: tstr`) or the double arrow syntax with double quotes (`"mykey" => tstr`). Unquoted member keys used with the double arrow syntax must resolve to one of the supported data types that can be used to validate JSON strings (`text` or `tstr`). Occurrence indicators can be used to validate key/value pairs in a JSON object and the number of elements in a JSON array; depending on how the indicators are defined in a CDDL data definition. CDDL groups, generics, sockets/plugs and group-to-choice enumerations are all parsed and monomorphized into their full representations before being evaluated for JSON validation.

Below is the table of supported control operators and whether or not they've been implemented as of the current release:

| Control operator | Implementation status                                                                                                                                                                       |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `.pcre`          | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji><sup>[3](#regex)</sup>                     |
| `.regex`         | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji><sup>[3](#regex)</sup> (alias for `.pcre`) |
| `.size`          | Incomplete                                                                                                                                                                                  |
| `.bits`          | Unsupported for JSON validation                                                                                                                                                             |
| `.cbor`          | Unsupported for JSON validation                                                                                                                                                             |
| `.cborseq`       | Unsupported for JSON validation                                                                                                                                                             |
| `.within`        | Incomplete                                                                                                                                                                                  |
| `.and`           | Incomplete                                                                                                                                                                                  |
| `.lt`            | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
| `.le`            | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
| `.gt`            | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
| `.ge`            | <g-emoji class="g-emoji" alias="heavy_check_mark" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2714.png">✔️</g-emoji>                                           |
| `.eq`            | Partial (text and numeric values)                                                                                                                                                           |
| `.ne`            | Incomplete                                                                                                                                                                                  |
| `.default`       | Incomplete                                                                                                                                                                                  |

<a name="arrays">1</a>: When groups are used to validate arrays, group entries with occurrence indicators are ignored due to complexities involved with processing these ambiguities. For proper JSON validation, avoid writing CDDL that looks like the following: `[ * a: int, b: tstr, ? c: int ]`.

<a name="number">2</a>: While JSON itself does not distinguish between integers and floating-point numbers, this crate does provide the ability to validate numbers against a more specific numerical CBOR type, provided that its equivalent representation is allowed by JSON.

<a name="regex">3</a>: Due to Perl-Compatible Regular Expressions (PCREs) being more widely used than XSD regular expressions, this crate also provides support for the proposed `.pcre` control extension in place of the `.regexp` operator (see [Discussion](https://tools.ietf.org/html/rfc8610#section-3.8.3.2) and [CDDL-Freezer proposal](https://tools.ietf.org/html/draft-bormann-cbor-cddl-freezer-02#section-5.1)). Ensure that your regex string is properly JSON escaped when using this control.

### Comparing with JSON schema and JSON schema language

[CDDL](https://tools.ietf.org/html/rfc8610), [JSON schema](https://json-schema.org/) and [JSON schema language](https://tools.ietf.org/html/draft-json-schema-language-02) can all be used to define JSON data structures. However, the approaches taken to develop each of these are vastly different. A good place to find past discussions on the differences between thse formats is the [IETF mail archive](https://mailarchive.ietf.org/arch/), specifically in the JSON and CBOR lists. The purpose of this crate is not to argue for the use of CDDL over any one of these formats, but simply to provide an example implementation in Rust.

## Validating CBOR

> Incomplete. Under development. Less complete than JSON validation functions.

This crate also uses [Serde](https://serde.rs/) and [serde_cbor](https://crates.io/crates/serde_cbor) for validating CBOR data structures. Similary to the JSON validation implementation, CBOR validation is done via the loosely typed [`serde_cbor::Value`](https://docs.rs/serde_cbor/0.10.1/serde_cbor/enum.Value.html) enum. Unfortunately, due to a [limitation of Serde](https://github.com/pyfisch/cbor/issues/3), CBOR tags are ignored during deserialization.

## `no_std` support

Only the lexer and parser can be used in a `no_std` context provided that a heap allocator is available. This can be enabled by opting out of the default features in your `Cargo.toml` file as follows:

```toml
[dependencies]
cddl = { version = "<version>", default-features = false }
```

Zero-copy parsing is implemented to the extent that is possible, with prefixed byte strings containing whitespace being one of the few exceptions where allocation is required. Allocation is also used for error handling and diagnostics.

Both JSON and CBOR validation are dependent on their respective heap allocated `Value` types, but since these types aren't supported in a `no_std` context, they subsequently aren't supported in a `no_std` context in this crate.
