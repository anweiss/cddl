use cddl::pest_bridge::cddl_from_pest_str;

fn main() {
    println!("=== Testing Various Error Scenarios ===\n");
    
    let scenarios = vec![
        ("rule1", "Missing assignment"),
        ("rule2 = ", "Incomplete assignment"),
        ("rule3 = @#$%", "Invalid characters"),
        ("rule4 = { key", "Malformed group"),
        ("rule5<T V> = int", "Malformed generic"),
        ("rule6 = 1..2..3", "Invalid range"),
        ("rule7 = tstr .regexp", "Incomplete control"),
        ("x = { }", "Empty group (should succeed)"),
        ("y = int / text / bool", "Type choice (should succeed)"),
    ];
    
    for (input, description) in scenarios {
        println!("--- {} ---", description);
        println!("Input: {}", input);
        
        match cddl_from_pest_str(input) {
            Ok(_) => println!("✓ Successfully parsed\n"),
            Err(e) => {
                if let cddl::parser::Error::PARSER { msg, .. } = e {
                    println!("Error: {}", msg.short);
                    if let Some(extended) = msg.extended {
                        println!("Details: {}\n", extended);
                    } else {
                        println!();
                    }
                } else {
                    println!("Unexpected error type: {:?}\n", e);
                }
            }
        }
    }
}
