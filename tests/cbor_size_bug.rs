#![cfg(feature = "std")]
#![cfg(feature = "cbor")]
#![cfg(not(target_arch = "wasm32"))]

use cddl::{
  cddl_from_str,
  validator::{cbor::CBORValidator, validate_cbor_from_slice, Validator},
};
use ciborium::value::Value;
use std::error::Error;

#[test]
fn test_type_check_with_size_constraint() -> Result<(), Box<dyn Error>> {
    // Test case 1: Without size constraint - should correctly reject byte array
    let cddl_without_size = r#"
        root = {
            "digitalSourceType": tstr
        }
    "#;
    
    // Create a CBOR map with a byte array where a text string is expected
    let cbor_bytes = Value::Map(vec![
        (Value::Text("digitalSourceType".to_string()), Value::Bytes(vec![1, 2, 3, 4]))
    ]);
    
    // This should fail because we're providing bytes where text is expected
    let mut cbor_bytes_vec = Vec::new();
    ciborium::ser::into_writer(&cbor_bytes, &mut cbor_bytes_vec)?;
    let result_without_size = validate_cbor_from_slice(cddl_without_size, &cbor_bytes_vec, None);
    assert!(result_without_size.is_err(), "Validation should fail when bytes are provided instead of text");
    
    if let Err(e) = result_without_size {
        println!("Expected error (without size): {}", e);
        assert!(e.to_string().contains("expected type tstr, got Bytes"), 
                "Error message should indicate type mismatch");
    }
    
    // Test case 2: With size constraint - should still reject byte array but currently doesn't
    let cddl_with_size = r#"
        root = {
            "digitalSourceType": tstr .size (1..500)
        }
    "#;
    
    // Same CBOR as before
    let mut cbor_bytes_vec = Vec::new();
    ciborium::ser::into_writer(&cbor_bytes, &mut cbor_bytes_vec)?;
    let result_with_size = validate_cbor_from_slice(cddl_with_size, &cbor_bytes_vec, None);
    
    // This currently passes but should fail
    assert!(result_with_size.is_err(), "Validation should fail when bytes are provided instead of text, even with size constraint");
    
    if let Err(e) = result_with_size {
        println!("Expected error (with size): {}", e);
        assert!(e.to_string().contains("expected type tstr"), 
                "Error message should indicate type mismatch");
    }
    
    Ok(())
}
