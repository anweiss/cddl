# CDDL Pest Grammar

This directory contains a comprehensive [Pest](https://pest.rs/) grammar file for the Concise Data Definition Language (CDDL) as defined in [RFC 8610](https://tools.ietf.org/html/rfc8610) and its extensions.

## Files

- `cddl.pest` - The Pest grammar definition
- `tests/grammar.rs` - Comprehensive test suite validating the grammar

## Grammar Features

The grammar supports all CDDL language constructs:

### Core Features
- **Type rules**: `typename = type`
- **Group rules**: `groupname = (entries)`
- **Type choices**: `value = int / text / bool`
- **Group choices**: `entry = (a: int) // (b: text)`
- **Comments**: `; This is a comment`

### Type Expressions
- **Basic types**: `int`, `uint`, `float`, `bool`, `text`, `bytes`, etc.
- **Literal values**: `42`, `-10`, `3.14`, `"text"`, `h'deadbeef'`
- **Ranges**: `0..100`, `0...100`
- **Generic types**: `map<K, V> = { * K => V }`
- **Tags**: `#6.32(tstr)`, `#6.<typename>`

### Group Constructs
- **Maps**: `{ name: text, age: uint }`
- **Arrays**: `[ int, text, bool ]`
- **Arrow maps**: `{ * text => int }`
- **Optional commas**: Commas between entries are optional

### Occurrence Indicators
- **Optional**: `? field: text`
- **Zero or more**: `* field: int`
- **One or more**: `+ field: uint`
- **Exact count**: `3* field: text`
- **Range**: `1*5 field: int`

### Advanced Features
- **Control operators**: `.size`, `.bits`, `.regexp`, `.pcre`, `.cbor`, `.within`, `.and`, `.lt`, `.le`, `.gt`, `.ge`, `.eq`, `.ne`, `.default`
- **Additional controls** (RFC 9165, RFC 9741): `.cat`, `.det`, `.plus`, `.abnf`, `.abnfb`, `.feature`, `.b64u`, `.hex`, etc.
- **Unwrap operator**: `~typename`
- **Cut operator**: `^ => type` or `^label: type`
- **Group-to-choice enumeration**: `&(group)` or `&groupname`
- **Socket/plug syntax**: `$typename`, `$$groupname`

### String and Byte String Support
- **Text strings**: `"hello world"` with escape sequences (`\"`, `\\`, `\n`, `\t`, `\uXXXX`)
- **Base64 byte strings**: `'SGVsbG8='`
- **Hex byte strings**: `h'48656c6c6f'`
- **Hex-quoted strings**: `h"text"`

## Usage Example

```rust
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "cddl.pest"]
struct CDDLParser;

fn main() {
    let cddl = r#"
        person = {
            name: tstr,
            age: uint,
            ? email: tstr
        }
    "#;
    
    match CDDLParser::parse(Rule::cddl, cddl) {
        Ok(pairs) => println!("Successfully parsed CDDL"),
        Err(e) => eprintln!("Parse error: {}", e),
    }
}
```

## Testing

Run the grammar tests with:

```bash
cargo test --test grammar
```

All 23 tests should pass, including tests against real CDDL files from the repository.

## Compatibility

The grammar is designed to:
- Parse all valid CDDL from RFC 8610
- Support additional control operators from RFC 9165 and RFC 9741
- Match the existing AST structure used by the handwritten parser
- Handle all CDDL constructs in the test suite

## Grammar Structure

The grammar is organized into logical sections:

1. **Entry Point and Whitespace** - Main parsing entry and whitespace handling
2. **Rules** - Type and group rule definitions
3. **Generic Parameters** - Template parameters and arguments
4. **Type Expressions** - Type definitions and choices
5. **Operators** - Range and control operators
6. **Type2** - Primary type constructs
7. **Groups** - Map and array group definitions
8. **Group Entries** - Individual map/array entries
9. **Occurrence Indicators** - Repetition specifiers
10. **Identifiers** - Names and socket/plug syntax
11. **Values** - Literal value parsing
12. **Numbers** - Integer, float, and hexfloat parsing
13. **Text Values** - String literals with escapes
14. **Byte Strings** - Base64 and hex byte strings
15. **Prelude Types** - Standard type names

Each section is documented with examples and references to the RFC specification.

## References

- [RFC 8610: Concise Data Definition Language (CDDL)](https://tools.ietf.org/html/rfc8610)
- [RFC 9165: Additional Control Operators for CDDL](https://datatracker.ietf.org/doc/html/rfc9165)
- [RFC 9741: Additional Control Operators for CDDL](https://datatracker.ietf.org/doc/html/rfc9741)
- [Pest Parser](https://pest.rs/)
