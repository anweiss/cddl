//! JSON data generation from CDDL schemas

use super::{GeneratorConfig, GeneratorError};
use crate::ast::*;
use crate::parser::cddl_from_str;
use crate::token::{ControlOperator, Value as TokenValue};
use serde_json::{json, Map, Value};
use std::collections::HashMap;

/// Random number generator state (simple LCG for reproducibility)
struct Rng {
  state: u64,
}

impl Rng {
  fn new(seed: Option<u64>) -> Self {
    Self {
      state: seed.unwrap_or_else(|| {
        std::time::SystemTime::now()
          .duration_since(std::time::UNIX_EPOCH)
          .map(|d| d.as_nanos() as u64)
          .unwrap_or(12345)
      }),
    }
  }

  fn next(&mut self) -> u64 {
    // LCG parameters from Numerical Recipes
    self.state = self.state.wrapping_mul(6364136223846793005).wrapping_add(1);
    self.state
  }

  fn next_range(&mut self, min: usize, max: usize) -> usize {
    if min >= max {
      return min;
    }
    min + (self.next() as usize % (max - min + 1))
  }

  fn next_bool(&mut self) -> bool {
    self.next() % 2 == 0
  }

  fn next_f64(&mut self) -> f64 {
    (self.next() as f64) / (u64::MAX as f64)
  }
}

/// Stored rule information
enum StoredRule<'a> {
  TypeRule(&'a TypeRule<'a>),
  GroupRule(&'a GroupRule<'a>),
}

/// Internal state for the generator
struct Generator<'a> {
  config: &'a GeneratorConfig,
  rng: Rng,
  rules: HashMap<String, StoredRule<'a>>,
  depth: usize,
}

impl<'a> Generator<'a> {
  fn new(config: &'a GeneratorConfig, cddl: &'a CDDL<'a>) -> Self {
    let mut rules = HashMap::new();

    // Index all rules by name
    for rule in &cddl.rules {
      match rule {
        Rule::Type { rule, .. } => {
          rules.insert(rule.name.ident.to_string(), StoredRule::TypeRule(rule));
        }
        Rule::Group { rule, .. } => {
          rules.insert(rule.name.ident.to_string(), StoredRule::GroupRule(rule));
        }
      }
    }

    Self {
      config,
      rng: Rng::new(config.seed),
      rules,
      depth: 0,
    }
  }

  fn generate_from_type(&mut self, t: &Type) -> Result<Value, GeneratorError> {
    if self.depth > self.config.max_depth {
      return Err(GeneratorError::MaxDepthExceeded);
    }
    self.depth += 1;

    // Handle type choices - pick one randomly
    let choice_idx = if t.type_choices.len() > 1 {
      self.rng.next_range(0, t.type_choices.len() - 1)
    } else {
      0
    };

    let result = self.generate_from_type_choice(&t.type_choices[choice_idx]);
    self.depth -= 1;
    result
  }

  fn generate_from_type_choice(&mut self, tc: &TypeChoice) -> Result<Value, GeneratorError> {
    self.generate_from_type1(&tc.type1)
  }

  fn generate_from_type1(&mut self, t1: &Type1) -> Result<Value, GeneratorError> {
    // Handle range operators
    if let Some(op) = &t1.operator {
      return self.generate_from_operator(&t1.type2, op);
    }

    self.generate_from_type2(&t1.type2)
  }

  fn generate_from_operator(
    &mut self,
    lower: &Type2,
    op: &Operator,
  ) -> Result<Value, GeneratorError> {
    match &op.operator {
      RangeCtlOp::RangeOp { is_inclusive, .. } => {
        // Extract numeric bounds
        let lower_val = self.extract_number(lower)?;
        let upper_val = self.extract_number(&op.type2)?;

        let (lower_int, upper_int) = (lower_val as i64, upper_val as i64);
        let upper_bound = if *is_inclusive {
          upper_int
        } else {
          upper_int - 1
        };

        if lower_int > upper_bound {
          return Ok(json!(lower_int));
        }

        let range = (upper_bound - lower_int + 1) as u64;
        let value = lower_int + (self.rng.next() % range) as i64;
        Ok(json!(value))
      }
      RangeCtlOp::CtlOp { ctrl, .. } => {
        // Handle control operators
        self.generate_with_control(lower, ctrl, &op.type2)
      }
    }
  }

  fn extract_number(&self, t2: &Type2) -> Result<f64, GeneratorError> {
    match t2 {
      Type2::UintValue { value, .. } => Ok(*value as f64),
      Type2::IntValue { value, .. } => Ok(*value as f64),
      Type2::FloatValue { value, .. } => Ok(*value),
      _ => Err(GeneratorError::UnsupportedConstruct(
        "Non-numeric range bound".to_string(),
      )),
    }
  }

  fn generate_with_control(
    &mut self,
    base: &Type2,
    ctrl: &ControlOperator,
    arg: &Type2,
  ) -> Result<Value, GeneratorError> {
    match ctrl {
      ControlOperator::SIZE => {
        let size = self.extract_number(arg)? as usize;
        // Generate a string of the specified size
        Ok(Value::String(self.generate_string_of_length(size)))
      }
      ControlOperator::LT
      | ControlOperator::LE
      | ControlOperator::GT
      | ControlOperator::GE
      | ControlOperator::EQ
      | ControlOperator::NE => {
        // For comparison operators, generate from base type
        self.generate_from_type2(base)
      }
      ControlOperator::REGEXP | ControlOperator::PCRE => {
        // Try to generate a string matching the regex pattern
        if let Type2::TextValue { value, .. } = arg {
          Ok(Value::String(
            self.generate_string_matching_pattern(value.as_ref()),
          ))
        } else {
          self.generate_string()
        }
      }
      ControlOperator::DEFAULT => {
        // Use the default value
        self.generate_from_type2(arg)
      }
      _ => {
        // For other controls, just generate from base type
        self.generate_from_type2(base)
      }
    }
  }

  fn generate_from_type2(&mut self, t2: &Type2) -> Result<Value, GeneratorError> {
    match t2 {
      // Literal values
      Type2::UintValue { value, .. } => Ok(json!(*value)),
      Type2::IntValue { value, .. } => Ok(json!(*value)),
      Type2::FloatValue { value, .. } => Ok(json!(*value)),
      Type2::TextValue { value, .. } => Ok(Value::String(value.to_string())),

      // Type references
      Type2::Typename {
        ident,
        generic_args,
        ..
      } => self.generate_from_typename(ident.ident, generic_args.as_ref()),

      // Map type
      Type2::Map { group, .. } => self.generate_map(group),

      // Array type
      Type2::Array { group, .. } => self.generate_array(group),

      // Parenthesized type
      Type2::ParenthesizedType { pt, .. } => self.generate_from_type(pt),

      // Unwrap (~)
      Type2::Unwrap {
        ident,
        generic_args,
        ..
      } => self.generate_from_typename(ident.ident, generic_args.as_ref()),

      // Choice from group
      Type2::ChoiceFromGroup {
        ident,
        generic_args,
        ..
      } => self.generate_from_typename(ident.ident, generic_args.as_ref()),

      // Choice from inline group - generate from the group directly
      Type2::ChoiceFromInlineGroup { group, .. } => {
        // For inline groups used as choices, pick one entry
        if let Some(gc) = group.group_choices.first() {
          if let Some((ge, _)) = gc.group_entries.first() {
            return self.generate_array_entry(ge);
          }
        }
        Err(GeneratorError::NoValidChoice(
          "Empty inline group".to_string(),
        ))
      }

      // Tagged values (for JSON, we ignore the tag)
      Type2::TaggedData { t, .. } => self.generate_from_type(t),

      // Data types that need special handling
      Type2::DataMajorType { mt, .. } => self.generate_from_major_type(*mt),

      // Any type
      Type2::Any { .. } => self.generate_any(),

      // Byte strings - generate base64 for JSON
      Type2::UTF8ByteString { .. }
      | Type2::B16ByteString { .. }
      | Type2::B64ByteString { .. } => Ok(json!(self.generate_base64())),
    }
  }

  fn generate_from_typename(
    &mut self,
    name: &str,
    _generic_args: Option<&GenericArgs>,
  ) -> Result<Value, GeneratorError> {
    // Check for custom values first
    if let Some(custom) = self.config.custom_values.get(name) {
      return Ok(custom.clone());
    }

    // Handle prelude types
    match name {
      // Boolean
      "bool" => return Ok(json!(self.rng.next_bool())),
      "true" => return Ok(json!(true)),
      "false" => return Ok(json!(false)),

      // Integers
      "uint" => return Ok(json!(self.rng.next_range(0, 1000))),
      "nint" => return Ok(json!(-(self.rng.next_range(1, 1000) as i64))),
      "int" => {
        return Ok(if self.rng.next_bool() {
          json!(self.rng.next_range(0, 1000))
        } else {
          json!(-(self.rng.next_range(1, 1000) as i64))
        })
      }

      // Floating point
      "float" | "float16" | "float32" | "float64" | "float16-32" | "float32-64" | "number" => {
        let val = self.rng.next_f64() * 1000.0 - 500.0;
        return Ok(json!(val));
      }

      // Strings
      "tstr" | "text" => return self.generate_string(),

      // URI
      "uri" => return Ok(json!(self.generate_uri())),

      // Date/time
      "tdate" => return Ok(json!(self.generate_tdate())),

      // Base64
      "b64url" => return Ok(json!(self.generate_base64())),

      // Timestamp
      "time" => {
        let timestamp = 1700000000 + self.rng.next_range(0, 100000000);
        return Ok(json!(timestamp));
      }

      // Null
      "null" | "nil" => return Ok(Value::Null),

      // Any
      "any" => return self.generate_any(),

      // Bytes - for JSON, generate base64-encoded string
      "bstr" | "bytes" => return Ok(json!(self.generate_base64())),

      _ => {}
    }

    // Look up the rule
    if let Some(stored) = self.rules.get(name) {
      match stored {
        StoredRule::TypeRule(rule) => {
          // Clone to avoid borrow issues
          let value_clone = rule.value.clone();
          return self.generate_from_type(&value_clone);
        }
        StoredRule::GroupRule(rule) => {
          // Group rule - generate as map from the entry
          let entry_clone = rule.entry.clone();
          return self.generate_from_group_entry(&entry_clone);
        }
      }
    }

    // Unknown type - generate a placeholder string
    Ok(Value::String(format!("<{}>", name)))
  }

  fn generate_from_major_type(&mut self, mt: u8) -> Result<Value, GeneratorError> {
    match mt {
      0 => {
        // Unsigned int
        Ok(json!(self.rng.next_range(0, 1000)))
      }
      1 => {
        // Negative int
        Ok(json!(-(self.rng.next_range(1, 1000) as i64)))
      }
      2 => {
        // Byte string - encode as base64 for JSON
        Ok(json!(self.generate_base64()))
      }
      3 => {
        // Text string
        self.generate_string()
      }
      4 => {
        // Array
        Ok(json!([]))
      }
      5 => {
        // Map
        Ok(json!({}))
      }
      6 => {
        // Tagged - for JSON, return the inner value
        self.generate_any()
      }
      7 => {
        // Simple values / floats
        Ok(json!(self.rng.next_bool()))
      }
      _ => Err(GeneratorError::UnsupportedConstruct(format!(
        "Major type: {}",
        mt
      ))),
    }
  }

  fn generate_map(&mut self, group: &Group) -> Result<Value, GeneratorError> {
    let mut map = Map::new();

    for gc in &group.group_choices {
      self.generate_group_choice_into_map(&mut map, gc)?;
    }

    Ok(Value::Object(map))
  }

  fn generate_group_choice_into_map(
    &mut self,
    map: &mut Map<String, Value>,
    gc: &GroupChoice,
  ) -> Result<(), GeneratorError> {
    for (ge, _) in &gc.group_entries {
      self.generate_group_entry_into_map(map, ge)?;
    }
    Ok(())
  }

  fn generate_group_entry_into_map(
    &mut self,
    map: &mut Map<String, Value>,
    ge: &GroupEntry,
  ) -> Result<(), GeneratorError> {
    match ge {
      GroupEntry::ValueMemberKey { ge, .. } => {
        let (key, is_optional) = self.extract_member_key(&ge.member_key)?;

        // Skip optional fields based on config
        if is_optional && !self.config.include_optional {
          return Ok(());
        }
        if is_optional && !self.rng.next_bool() {
          return Ok(());
        }

        let value = self.generate_from_type(&ge.entry_type)?;
        map.insert(key, value);
      }
      GroupEntry::TypeGroupname { ge, .. } => {
        // Reference to another group - merge its entries
        if let Some(stored) = self.rules.get(ge.name.ident) {
          if let StoredRule::GroupRule(rule) = stored {
            let entry_clone = rule.entry.clone();
            if let GroupEntry::InlineGroup { group, .. } = &entry_clone {
              for gc in &group.group_choices {
                self.generate_group_choice_into_map(map, gc)?;
              }
            } else {
              self.generate_group_entry_into_map(map, &entry_clone)?;
            }
          }
        }
      }
      GroupEntry::InlineGroup { group, .. } => {
        for gc in &group.group_choices {
          self.generate_group_choice_into_map(map, gc)?;
        }
      }
    }
    Ok(())
  }

  fn generate_from_group_entry(&mut self, ge: &GroupEntry) -> Result<Value, GeneratorError> {
    match ge {
      GroupEntry::InlineGroup { group, .. } => self.generate_map(group),
      GroupEntry::ValueMemberKey { ge, .. } => self.generate_from_type(&ge.entry_type),
      GroupEntry::TypeGroupname { ge, .. } => self.generate_from_typename(ge.name.ident, None),
    }
  }

  fn extract_member_key(&self, mk: &Option<MemberKey>) -> Result<(String, bool), GeneratorError> {
    match mk {
      Some(MemberKey::Type1 { t1, .. }) => {
        // The type1 should resolve to a string key
        if let Type2::TextValue { value, .. } = &t1.type2 {
          return Ok((value.to_string(), false));
        }
        // Try to get the identifier name
        if let Type2::Typename { ident, .. } = &t1.type2 {
          return Ok((ident.ident.to_string(), false));
        }
        Err(GeneratorError::UnsupportedConstruct(
          "Complex member key type".to_string(),
        ))
      }
      Some(MemberKey::Bareword { ident, .. }) => Ok((ident.ident.to_string(), false)),
      Some(MemberKey::Value { value, .. }) => match value {
        TokenValue::TEXT(s) => Ok((s.to_string(), false)),
        TokenValue::INT(i) => Ok((i.to_string(), false)),
        TokenValue::UINT(u) => Ok((u.to_string(), false)),
        _ => Err(GeneratorError::UnsupportedConstruct(
          "Non-string member key".to_string(),
        )),
      },
      None => Err(GeneratorError::UnsupportedConstruct(
        "Missing member key".to_string(),
      )),
      _ => Err(GeneratorError::UnsupportedConstruct(
        "Unsupported member key type".to_string(),
      )),
    }
  }

  fn generate_array(&mut self, group: &Group) -> Result<Value, GeneratorError> {
    let mut arr = Vec::new();

    for gc in &group.group_choices {
      for (ge, _) in &gc.group_entries {
        let (occur_min, occur_max) = self.extract_occurrence(ge);

        let count = self.rng.next_range(occur_min, occur_max);
        for _ in 0..count {
          let value = self.generate_array_entry(ge)?;
          arr.push(value);
        }
      }
    }

    Ok(Value::Array(arr))
  }

  fn extract_occurrence(&self, ge: &GroupEntry) -> (usize, usize) {
    let occur = match ge {
      GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref(),
      GroupEntry::TypeGroupname { ge, .. } => ge.occur.as_ref(),
      GroupEntry::InlineGroup { occur, .. } => occur.as_ref(),
    };

    match occur {
      Some(Occurrence { occur, .. }) => match occur {
        Occur::ZeroOrMore { .. } => (self.config.min_array_items, self.config.max_array_items),
        Occur::OneOrMore { .. } => (1, self.config.max_array_items),
        Occur::Optional { .. } => (0, 1),
        Occur::Exact { lower, upper, .. } => {
          let min = lower.unwrap_or(0) as usize;
          let max = upper.map(|u| u as usize).unwrap_or(min);
          (min, max)
        }
      },
      None => (1, 1),
    }
  }

  fn generate_array_entry(&mut self, ge: &GroupEntry) -> Result<Value, GeneratorError> {
    match ge {
      GroupEntry::ValueMemberKey { ge, .. } => {
        // For arrays, we might have unnamed entries (just types)
        self.generate_from_type(&ge.entry_type)
      }
      GroupEntry::TypeGroupname { ge, .. } => self.generate_from_typename(ge.name.ident, None),
      GroupEntry::InlineGroup { group, .. } => {
        // Inline group in array - generate its contents as a nested structure
        self.generate_map(group)
      }
    }
  }

  fn generate_string(&mut self) -> Result<Value, GeneratorError> {
    let len = self
      .rng
      .next_range(self.config.min_string_length, self.config.max_string_length);
    Ok(Value::String(self.generate_string_of_length(len)))
  }

  fn generate_string_of_length(&mut self, len: usize) -> String {
    if self.config.realistic_data {
      // Generate more realistic looking strings
      let words = [
        "lorem", "ipsum", "dolor", "sit", "amet", "consectetur", "adipiscing", "elit", "alpha",
        "beta", "gamma", "delta", "epsilon", "data", "test", "example", "sample", "value", "item",
        "entry",
      ];
      let mut result = String::new();
      while result.len() < len {
        let word = words[self.rng.next_range(0, words.len() - 1)];
        if !result.is_empty() {
          result.push(' ');
        }
        result.push_str(word);
      }
      result.truncate(len);
      result
    } else {
      // Generate random alphanumeric
      let chars: Vec<char> = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        .chars()
        .collect();
      (0..len)
        .map(|_| chars[self.rng.next_range(0, chars.len() - 1)])
        .collect()
    }
  }

  fn generate_string_matching_pattern(&mut self, pattern: &str) -> String {
    // Simple pattern handling - this is a basic implementation
    // For complex patterns, we'd need a proper regex-to-string generator
    if pattern.contains("[a-z]") {
      return self.generate_string_of_length(10);
    }
    if pattern.contains("[0-9]") {
      return (0..10)
        .map(|_| ((self.rng.next() % 10) as u8 + b'0') as char)
        .collect();
    }
    // Default: return a simple string
    self.generate_string_of_length(10)
  }

  fn generate_uri(&mut self) -> String {
    let domains = ["example.com", "test.org", "sample.net", "demo.io"];
    let paths = ["api", "v1", "data", "resource", "items"];

    let domain = domains[self.rng.next_range(0, domains.len() - 1)];
    let path = paths[self.rng.next_range(0, paths.len() - 1)];
    let id = self.rng.next_range(1, 9999);

    format!("https://{}/{}/{}", domain, path, id)
  }

  fn generate_tdate(&mut self) -> String {
    let year = self.rng.next_range(2020, 2025);
    let month = self.rng.next_range(1, 12);
    let day = self.rng.next_range(1, 28);
    let hour = self.rng.next_range(0, 23);
    let minute = self.rng.next_range(0, 59);
    let second = self.rng.next_range(0, 59);

    format!(
      "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}Z",
      year, month, day, hour, minute, second
    )
  }

  fn generate_base64(&mut self) -> String {
    let len = self.rng.next_range(8, 32);
    let bytes: Vec<u8> = (0..len).map(|_| self.rng.next() as u8).collect();
    base64_url::encode(&bytes)
  }

  fn generate_any(&mut self) -> Result<Value, GeneratorError> {
    // Generate a random simple value
    match self.rng.next() % 5 {
      0 => Ok(json!(self.rng.next_range(0, 100))),
      1 => self.generate_string(),
      2 => Ok(json!(self.rng.next_bool())),
      3 => Ok(Value::Null),
      _ => Ok(json!(self.rng.next_f64() * 100.0)),
    }
  }
}

/// Generate a JSON value from a CDDL schema string.
///
/// This function parses the CDDL schema, finds the root type (first rule),
/// and generates a valid JSON value that conforms to the schema.
///
/// # Arguments
///
/// * `cddl_str` - The CDDL schema as a string
/// * `config` - Configuration options for generation
///
/// # Returns
///
/// A `Result` containing either the generated `serde_json::Value` or a `GeneratorError`
///
/// # Example
///
/// ```rust
/// use cddl::generator::{generate_json_from_cddl, GeneratorConfig};
///
/// let cddl = r#"
/// message = {
///   id: uint,
///   content: tstr,
/// }
/// "#;
///
/// let config = GeneratorConfig::with_seed(42);
/// let json = generate_json_from_cddl(cddl, &config).unwrap();
/// ```
pub fn generate_json_from_cddl(
  cddl_str: &str,
  config: &GeneratorConfig,
) -> Result<Value, GeneratorError> {
  let cddl =
    cddl_from_str(cddl_str, true).map_err(|e| GeneratorError::ParseError(e.to_string()))?;

  let mut generator = Generator::new(config, &cddl);

  // Find the root rule (first type or group rule)
  if let Some(first_rule) = cddl.rules.first() {
    match first_rule {
      Rule::Type { rule, .. } => generator.generate_from_type(&rule.value),
      Rule::Group { rule, .. } => generator.generate_from_group_entry(&rule.entry),
    }
  } else {
    Err(GeneratorError::ParseError(
      "No rules found in CDDL".to_string(),
    ))
  }
}

/// Generate a pretty-printed JSON string from a CDDL schema.
///
/// This is a convenience function that calls `generate_json_from_cddl` and
/// formats the output as a pretty-printed JSON string.
pub fn generate_json_string_from_cddl(
  cddl_str: &str,
  config: &GeneratorConfig,
) -> Result<String, GeneratorError> {
  let value = generate_json_from_cddl(cddl_str, config)?;
  serde_json::to_string_pretty(&value)
    .map_err(|e| GeneratorError::Custom(format!("JSON serialization error: {}", e)))
}

/// Generate multiple JSON samples from a CDDL schema.
///
/// Useful for generating test data sets.
pub fn generate_json_samples(
  cddl_str: &str,
  count: usize,
  base_config: &GeneratorConfig,
) -> Result<Vec<Value>, GeneratorError> {
  let mut results = Vec::with_capacity(count);

  for i in 0..count {
    let mut config = base_config.clone();
    config.seed = Some(base_config.seed.unwrap_or(0) + i as u64);
    results.push(generate_json_from_cddl(cddl_str, &config)?);
  }

  Ok(results)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_simple_struct() {
    let cddl = r#"
    person = {
      name: tstr,
      age: uint,
    }
    "#;

    let config = GeneratorConfig::with_seed(42);
    let result = generate_json_from_cddl(cddl, &config).unwrap();

    assert!(result.is_object());
    let obj = result.as_object().unwrap();
    assert!(obj.contains_key("name"));
    assert!(obj.contains_key("age"));
    assert!(obj["name"].is_string());
    assert!(obj["age"].is_number());
  }

  #[test]
  fn test_array() {
    let cddl = r#"
    numbers = [* uint]
    "#;

    let config = GeneratorConfig::with_seed(42);
    let result = generate_json_from_cddl(cddl, &config).unwrap();

    assert!(result.is_array());
    for item in result.as_array().unwrap() {
      assert!(item.is_number());
    }
  }

  #[test]
  fn test_nested_struct() {
    let cddl = r#"
    order = {
      id: uint,
      customer: {
        name: tstr,
        email: tstr,
      },
      items: [+ item],
    }
    
    item = {
      sku: tstr,
      quantity: uint,
    }
    "#;

    let config = GeneratorConfig::with_seed(42);
    let result = generate_json_from_cddl(cddl, &config).unwrap();

    assert!(result.is_object());
    let obj = result.as_object().unwrap();
    assert!(obj.contains_key("id"));
    assert!(obj.contains_key("customer"));
    assert!(obj.contains_key("items"));
    assert!(obj["customer"].is_object());
    assert!(obj["items"].is_array());
  }

  #[test]
  fn test_reproducible_with_seed() {
    let cddl = r#"test = { value: uint }"#;

    let config1 = GeneratorConfig::with_seed(12345);
    let config2 = GeneratorConfig::with_seed(12345);

    let result1 = generate_json_from_cddl(cddl, &config1).unwrap();
    let result2 = generate_json_from_cddl(cddl, &config2).unwrap();

    assert_eq!(result1, result2);
  }

  #[test]
  fn test_literal_values() {
    let cddl = r#"
    config = {
      version: 1,
      name: "test",
      enabled: true,
    }
    "#;

    let config = GeneratorConfig::with_seed(42);
    let result = generate_json_from_cddl(cddl, &config).unwrap();

    let obj = result.as_object().unwrap();
    assert_eq!(obj["version"], 1);
    assert_eq!(obj["name"], "test");
    assert_eq!(obj["enabled"], true);
  }

  #[test]
  fn test_range() {
    let cddl = r#"age = 0..120"#;

    let config = GeneratorConfig::with_seed(42);
    let result = generate_json_from_cddl(cddl, &config).unwrap();

    let value = result.as_i64().unwrap();
    assert!(value >= 0 && value <= 120);
  }
}
