# Enhanced Error Handling and Reporting

This document describes the enhanced error handling improvements made to leverage Pest's superior error reporting capabilities.

## Overview

The CDDL parser now provides significantly improved error messages when using the Pest-based parser through the `pest_bridge` module. These enhancements maintain full backward compatibility with the existing error handling system while providing more helpful and user-friendly error messages.

## Key Enhancements

### 1. User-Friendly Rule Names

Internal Pest rule names are automatically mapped to human-readable descriptions:

**Before:**
```
expected assign_t
```

**After:**
```
expected type assignment ('=' or '/=')
```

### 2. Extended Error Messages with Context

Errors now include both short and extended descriptions:

```rust
ErrorMsg {
    short: "expected one of: type assignment ('=' or '/='), group assignment ('=' or '//=')",
    extended: Some("At line 1, column 7: Expected type assignment ('=' or '/=') or group assignment ('=' or '//=').\n\nHint: Every rule needs an assignment operator...")
}
```

### 3. Contextual Hints

The error system provides helpful hints based on the error type:

- **Missing assignment operators**: Explains the difference between `=`, `/=`, and `//=`
- **Incomplete type expressions**: Suggests checking for proper formatting
- **Malformed groups**: Reminds about valid group entry requirements

### 4. Accurate Position Tracking

Error positions are accurately tracked from Pest's span information:
- Line and column numbers
- Byte ranges for precise highlighting
- Multi-line error span support

## Usage

### Using the Pest Parser with Enhanced Errors

```rust
use cddl::pest_bridge::cddl_from_pest_str;

let input = "myrule";  // Missing assignment operator

match cddl_from_pest_str(input) {
    Ok(_) => println!("Success"),
    Err(e) => {
        if let cddl::parser::Error::PARSER { position, msg } = e {
            println!("Error at line {}, column {}", position.line, position.column);
            println!("{}", msg.short);
            
            if let Some(extended) = msg.extended {
                println!("\nDetails: {}", extended);
            }
        }
    }
}
```

**Output:**
```
Error at line 1, column 7
expected one of: type assignment ('=' or '/='), group assignment ('=' or '//='), generic parameters '<...>'

Details: At line 1, column 7: Expected type assignment ('=' or '/=') or group assignment ('=' or '//=') or generic parameters '<...>'.

Hint: Every rule needs an assignment operator ('=' for new rules, '/=' for type alternatives, or '//=' for group alternatives).
```

### Integration with Error Reporting

The enhanced error messages automatically integrate with the existing `report_errors()` function:

```rust
use cddl::parser::Parser;
use cddl::lexer::Lexer;

let input = "invalid syntax";
if let Ok(mut p) = Parser::new(input, Box::new(Lexer::new(input).iter())) {
    if let Err(_) = p.parse_cddl() {
        // Extended messages are automatically included as diagnostic notes
        p.report_errors(true).ok();
    }
}
```

## WASM and Language Server Integration

The enhanced error messages are fully compatible with WASM serialization and the language server:

- `ErrorMsg` structure remains unchanged (maintains `short` and `extended` fields)
- Serialization works automatically via existing `#[cfg_attr(target_arch = "wasm32", derive(Serialize))]`
- VS Code extension receives enhanced error messages through existing JSON serialization

## Backward Compatibility

All enhancements maintain 100% backward compatibility:

- Existing error API is unchanged
- Both handwritten and Pest parsers coexist
- Error structure (`ErrorMsg`) is identical
- Display trait behavior is preserved
- All 94 existing tests continue to pass

## Rule Name Mappings

The following internal Pest rules are mapped to user-friendly descriptions:

| Pest Rule | User-Friendly Description |
|-----------|---------------------------|
| `assign_t` | type assignment ('=' or '/=') |
| `assign_g` | group assignment ('=' or '//=') |
| `generic_params` | generic parameters '<...>' |
| `type_expr` | type expression |
| `control_op` | control operator |
| `range_op` | range operator ('..' or '...') |
| `group_entry` | group entry |
| `occur_optional` | optional '?' |
| `occur_zero_or_more` | zero or more '*' |

And many more...

## Implementation Details

### Enhanced Error Conversion

The `convert_pest_error()` function in `pest_bridge.rs` has been enhanced to:

1. Extract position information from Pest errors
2. Map rule names to friendly descriptions
3. Generate contextual hints based on expected rules
4. Create both short and extended error messages

### Error Reporting Enhancement

The `report_errors()` function in `parser.rs` has been updated to:

1. Display short error messages as primary labels
2. Include extended messages as diagnostic notes
3. Maintain existing codespan-reporting integration
4. Support both `std` and `no_std` configurations

## Testing

Comprehensive tests verify:

- ✅ Enhanced error message generation
- ✅ Position tracking accuracy
- ✅ WASM serialization compatibility
- ✅ Parser comparison (handwritten vs Pest)
- ✅ Error scenario coverage
- ✅ Backward compatibility

All 94 existing tests pass without modification.

## Future Enhancements

Potential future improvements:

- Add support for error recovery suggestions
- Implement error message localization
- Add more specific hints for common mistakes
- Enhanced multi-error reporting with prioritization
