use cddl::parser::{cddl_from_str, Parser};
use cddl::lexer::Lexer;
use cddl::pest_bridge::cddl_from_pest_str;

fn main() {
    println!("=== Parser Comparison Test ===\n");
    
    let valid_inputs = vec![
        "myrule = int",
        "person = { name: tstr, age: uint }",
        "value = int / text / bool",
        "port = 0..65535",
        "map<K, V> = { * K => V }",
    ];
    
    println!("Testing valid CDDL inputs:\n");
    for input in valid_inputs {
        println!("Input: {}", input);
        
        let handwritten = cddl_from_str(input, false);
        let pest = cddl_from_pest_str(input);
        
        match (handwritten, pest) {
            (Ok(_), Ok(_)) => println!("✓ Both parsers succeeded\n"),
            (Ok(_), Err(e)) => println!("✗ Handwritten succeeded but Pest failed: {:?}\n", e),
            (Err(e), Ok(_)) => println!("✗ Pest succeeded but handwritten failed: {:?}\n", e),
            (Err(e1), Err(e2)) => println!("✗ Both failed:\n  Handwritten: {:?}\n  Pest: {:?}\n", e1, e2),
        }
    }
    
    let invalid_inputs = vec![
        "myrule",
        "myrule = ",
        "invalid @#$",
    ];
    
    println!("\nTesting invalid CDDL inputs (both should fail):\n");
    for input in invalid_inputs {
        println!("Input: {}", input);
        
        let handwritten = cddl_from_str(input, false);
        let pest = cddl_from_pest_str(input);
        
        match (handwritten, pest) {
            (Ok(_), Ok(_)) => println!("✗ Both parsers incorrectly succeeded\n"),
            (Ok(_), Err(_)) => println!("⚠ Handwritten succeeded (might be incremental), Pest failed as expected\n"),
            (Err(_), Ok(_)) => println!("⚠ Pest succeeded (might be incremental), handwritten failed as expected\n"),
            (Err(_), Err(_)) => println!("✓ Both correctly failed\n"),
        }
    }
    
    println!("\nTesting enhanced Pest error messages:\n");
    
    if let Err(e) = cddl_from_pest_str("myrule") {
        if let cddl::parser::Error::PARSER { msg, .. } = e {
            println!("Input: myrule");
            println!("Short error: {}", msg.short);
            if let Some(extended) = msg.extended {
                println!("Extended error: {}", extended);
            }
            println!();
        }
    }
}
