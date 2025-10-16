use cddl::pest_parser::{CddlParser, Rule};
use pest::Parser;

fn main() {
    let test_cases = vec![
        "invalid @#$",
        "myrule",  // Missing assignment
        "myrule = ",  // Missing value
        "person = { name }", // Missing type
    ];
    
    for (i, input) in test_cases.iter().enumerate() {
        println!("\n=== Test case {}: {} ===", i + 1, input);
        match CddlParser::parse(Rule::cddl, input) {
            Ok(_) => println!("Unexpectedly succeeded"),
            Err(e) => {
                println!("Error variant: {:?}", e.variant);
                println!("Line-col: {:?}", e.line_col);
                println!("Error string: {}", e);
                println!("Error location: {:?}", e.location);
            }
        }
    }
}
