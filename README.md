# cddl-rs

[![crates.io](https://img.shields.io/crates/v/cddl.svg)](https://crates.io/crates/cddl) [![docs.rs](https://docs.rs/cddl/badge.svg)](https://docs.rs/cddl) [![Actions Status](https://wdp9fww0r9.execute-api.us-west-2.amazonaws.com/production/badge/anweiss/cddl)](https://wdp9fww0r9.execute-api.us-west-2.amazonaws.com/production/results/anweiss/cddl)

> This crate is very much experimental and is being developed as a personal learning exercise for getting acquainted with Rust and about parsing in general. It is far from complete. There are likely more performant and stable libraries out there for parsing CDDL. This one should not be used in production in any form or fashion.

A Rust implementation of the Concise data definition language (CDDL). CDDL is an IETF standard that "proposes a notational convention to express CBOR and JSON data structures." As of 2019-06-12, it is published as RFC 8610 (Proposed Standard) at https://tools.ietf.org/html/rfc8610.

This crate includes a handwritten parser and lexer for CDDL and is heavily inspired by Thorsten Ball's book ["Writing An Interpretor In Go"](https://interpreterbook.com/). The AST has been built to closely match the rules defined by the ABNF grammar in [Appendix B.](https://tools.ietf.org/html/rfc8610#appendix-B) of the spec. All CDDL must use UTF-8 for its encoding per the spec.

This crate supports validation of both CBOR and JSON data structures. An extremely basic REPL is included as well, with plans to compile it for use in the browser with WebAssembly.

## Goals

- Parse CDDL documents into an AST
- Verify conformance of CDDL documents against RFC 8610
- Validate CBOR data structures
- Validate JSON documents
- Basic REPL
- Generate dummy JSON from conformant CDDL
- Close to zero-copy as possible
- Compile for use in the browser with WebAssembly
- `no_std` support (lexing and parsing only)

## Non-goals

- Performance (if this crate gains enough traction, it may be prudent to explore using a parser-combinator framework like [nom](https://github.com/Geal/nom))
- Support CBOR diagnostic notation
- I-JSON compatibility

## Why Rust?

Rust is a systems programming language designed around safety and is ideally-suited for resource-constrained systems. CDDL and CBOR are designed around small code and message sizes and constrained nodes, scenarios that Rust has also been designed for.

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
- [X] tags
- [x] unwrapping
- [x] controls
- [x] socket/plug
- [x] generics
- [ ] operator precedence
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

The first non-group rule defined by a CDDL data structure definition determines the root type, which is subsequently used for validating the top-level JSON data type.

### Supported JSON validation features

The following types and features of CDDL are supported by this crate for validating JSON:

|CDDL|JSON|
|----|----|
|structs|objects|
|arrays|arrays|
|text / tstr|string|
|number / int / float|number<sup>[1](#number)</sup>|
|bool / true / false|boolean|
|null / nil|null|
|any|any valid JSON|

Occurrence indicators can be used to validate key/value pairs in a JSON object and the number of elements in a JSON array; depending on how the indicators are defined in a CDDL data definition. CDDL groups, generics, sockets/plugs and group-to-choice enumerations are all parsed and monomorphized into their full representations before being evaluated for JSON validation.

Below is the table of supported control operators and whether or not they've been implemented as of the current release:

|Control operator|Implementation status|
|----------------|---------------------|
|.pcre|Implemented<sup>[2](#regex)</sup>|
|.regex|Implemented<sup>[2](#regex)</sup>|
|.size|Incomplete|
|.bits|Unsupported for JSON validation|
|.cbor|Unsupported for JSON validation|
|.cborseq|Unsupported for JSON validation|
|.within|Incomplete|
|.and|Incomplete|
|.lt|Incomplete|
|.le|Incomplete|
|.gt|Incomplete|
|.ge|Incomplete|
|.eq|Incomplete|
|.ne|Incomplete|
|.default|Incomplete|

<a name="number">1</a>: While JSON itself does not distinguish between integers and floating-point numbers, this crate does provide the ability to validate numbers against a more specific numerical CBOR type, provided that its equivalent representation is allowed by JSON.

<a name="regex">2</a>: Due to Perl-Compatible Regular Expressions (PCREs) being more widely used than XSD regular expressions, this crate also provides support for the proposed `.pcre` control extension in place of the `.regexp` operator (see [Discussion](https://tools.ietf.org/html/rfc8610#section-3.8.3.2) and [CDDL-Freezer proposal](https://tools.ietf.org/html/draft-bormann-cbor-cddl-freezer-02#section-5.1)). Ensure that your regex string is properly JSON escaped when using this control.

### Comparing with JSON schema and JSON schema language

[CDDL](https://tools.ietf.org/html/rfc8610), [JSON schema](https://json-schema.org/) and [JSON schema language](https://tools.ietf.org/html/draft-json-schema-language-02) can all be used to define JSON data structures. However, the approaches taken to develop each of these are vastly different. A good place to find past discussions on the differences between thse formats is the [IETF mail archive](https://mailarchive.ietf.org/arch/), specifically in the JSON and CBOR lists. The purpose of this crate is not to argue for the use of CDDL over any one of these formats, but simply to provide an example implementation in Rust.

## Validating CBOR

> Incomplete. Under development

This crate also uses [Serde](https://serde.rs/) and [serde_cbor](https://crates.io/crates/serde_cbor) for validating CBOR data structures. Similary to the JSON validation implementation, CBOR validation is done via the loosely typed [`serde_cbor::Value`](https://docs.rs/serde_cbor/0.10.1/serde_cbor/enum.Value.html) enum. Unfortunately, due to a [limitation of Serde](https://github.com/pyfisch/cbor/issues/3), CBOR tags are ignored during deserialization.

## `no_std` support

Only the lexer and parser can be used in a `no_std` context provided that a heap allocator is available. This can be enabled by opting out of the default features in your `Cargo.toml` file as follows:

```toml
[dependencies]
cddl = { version = "<version>", default-features = false }
```

Zero-copy parsing is implemented to the extent that is possible, with prefixed byte strings containing whitespace being one of the few exceptions where allocation is required.

Both JSON and CBOR validation are dependent on their respective heap allocated `Value` types, but since these types aren't supported in a `no_std` context, they subsequently aren't supported in a `no_std` context in this crate.

## Dependency graph

Below is a graph of the dependencies used by this project. It was generated using [`cargo-deps`](https://github.com/m-cat/cargo-deps).

![cddl dependencies](https://github.com/anweiss/cddl/raw/master/dep-graph.png)