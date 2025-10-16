use cddl::pest_bridge::cddl_from_pest_str;

fn main() {
    println!("=== Testing Enhanced Error Reporting ===\n");
    
    let test_input = "myrule";
    
    println!("Input: {}", test_input);
    println!("\nExpected: Should show missing assignment error with hint\n");
    
    match cddl_from_pest_str(test_input) {
        Ok(_) => println!("Unexpectedly succeeded"),
        Err(e) => {
            // The error is already formatted nicely
            println!("Error details:\n{:?}\n", e);
        }
    }
    
    println!("\n=== Testing with multiple errors ===\n");
    let test_input2 = "rule1 = \nrule2 = invalid @#$";
    println!("Input:\n{}\n", test_input2);
    
    match cddl_from_pest_str(test_input2) {
        Ok(_) => println!("Unexpectedly succeeded"),
        Err(e) => {
            println!("Error details:\n{:?}\n", e);
        }
    }
}
