use cddl_derive::cddl_typegen;

// Generate all types from the schema file
cddl_typegen!("tests/fixtures/schema.cddl");

#[test]
fn test_person_struct_exists() {
  let p = Person {
    name: "Alice".to_string(),
    age: 30,
    email: Some("alice@example.com".to_string()),
  };
  assert_eq!(p.name, "Alice");
  assert_eq!(p.age, 30);
  assert_eq!(p.email, Some("alice@example.com".to_string()));
}

#[test]
fn test_person_optional_field() {
  let p = Person {
    name: "Bob".to_string(),
    age: 25,
    email: None,
  };
  assert_eq!(p.email, None);
}

#[test]
fn test_address_struct_exists() {
  let a = Address {
    street: "123 Main St".to_string(),
    city: "Springfield".to_string(),
    zip_code: Some("12345".to_string()),
  };
  assert_eq!(a.city, "Springfield");
}

#[test]
fn test_score_type_alias() {
  let s: Score = 42;
  assert_eq!(s, 42);
}

#[test]
fn test_person_record_struct() {
  let pr = PersonRecord {
    person: Person {
      name: "Charlie".to_string(),
      age: 40,
      email: None,
    },
    home_address: Address {
      street: "456 Elm St".to_string(),
      city: "Shelbyville".to_string(),
      zip_code: None,
    },
    work_address: None,
  };
  assert_eq!(pr.person.name, "Charlie");
}

#[test]
fn test_person_serde_roundtrip() {
  let p = Person {
    name: "Diana".to_string(),
    age: 28,
    email: Some("diana@example.com".to_string()),
  };
  let json = serde_json::to_string(&p).unwrap();
  let deserialized: Person = serde_json::from_str(&json).unwrap();
  assert_eq!(deserialized.name, "Diana");
  assert_eq!(deserialized.age, 28);
}

#[test]
fn test_person_serde_optional_skipped() {
  let p = Person {
    name: "Eve".to_string(),
    age: 35,
    email: None,
  };
  let json = serde_json::to_string(&p).unwrap();
  // email should not appear in JSON when None (skip_serializing_if)
  assert!(!json.contains("email"));
}

#[test]
fn test_person_clone_and_debug() {
  let p = Person {
    name: "Frank".to_string(),
    age: 50,
    email: None,
  };
  let p2 = p.clone();
  assert_eq!(p2.name, "Frank");
  // Debug should work
  let debug = format!("{:?}", p2);
  assert!(debug.contains("Frank"));
}
