# cddl-rs

> This library is very much experimental and is being developed as a personal learning exercise. It is far from complete. There are likely more performant and stable libraries out there for parsing CDDL. This one should not be used in production in any form or fashion.

A Rust implementation of the Concise data definition language (CDDL). CDDL is an IETF draft standard that "proposes a notational convention to express CBOR and JSON data structures." The current draft can be found at https://tools.ietf.org/html/draft-ietf-cbor-cddl-08 and is currently in AUTH48 state as of 2019-05-24 (RFC-to-be 8610), per https://www.rfc-editor.org/auth48/rfc8610.

This library includes a handwritten parser and lexer for CDDL and is heavily inspired by Thorsten Ball's book "Writing An Interpretor In Go". I'm personally only focused on using CDDL as a means for validating JSON data, and as such, work is being done to build a JSON validation component into the library.

## Goals

- Parse CDDL documents into an AST
- Verify conformance of CDDL documents against the draft standard
- Validate JSON documents using CDDL (only interested in the JSON subset of the CBOR generic data model for now)

## Non-goals

- Validate CBOR data structures (will eventually support this given it's the main reason for CDDL, but not focused on this at the moment)
- Performance
