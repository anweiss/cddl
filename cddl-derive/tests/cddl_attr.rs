use cddl_derive::cddl;

// Attribute macro — struct name maps to CDDL rule `person`
#[cddl(path = "tests/fixtures/schema.cddl")]
struct Person;

#[test]
fn test_cddl_attr_person_fields() {
  let p = Person {
    name: "Grace".to_string(),
    age: 22,
    email: Some("grace@example.com".to_string()),
  };
  assert_eq!(p.name, "Grace");
  assert_eq!(p.age, 22);
  assert_eq!(p.email, Some("grace@example.com".to_string()));
}

// Attribute macro with explicit rule name
#[cddl(path = "tests/fixtures/schema.cddl", rule = "address")]
struct Addr;

#[test]
fn test_cddl_attr_explicit_rule() {
  // The struct should use the user's name (Addr) with fields from the address rule
  let a = Addr {
    street: "789 Oak Ave".to_string(),
    city: "Capital City".to_string(),
    zip_code: None,
  };
  assert_eq!(a.street, "789 Oak Ave");
}
