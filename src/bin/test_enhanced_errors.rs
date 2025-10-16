use cddl::pest_bridge::cddl_from_pest_str;

fn main() {
    let test_cases = vec![
        ("invalid @#$", "Invalid syntax"),
        ("myrule", "Missing assignment"),
        ("myrule = ", "Missing value"),
        ("person = { name }", "Missing type in group entry"),
        ("x = 1..100..", "Double range operator"),
    ];
    
    for (input, description) in test_cases {
        println!("\n=== {} ===", description);
        println!("Input: {}", input);
        match cddl_from_pest_str(input) {
            Ok(_) => println!("✓ Unexpectedly succeeded"),
            Err(e) => {
                println!("Error: {:?}", e);
            }
        }
    }
}
