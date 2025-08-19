use std::fmt;

#[cfg(target_arch = "wasm32")]
use serde::Serialize;

/// Enhanced validation error with improved context and formatting
#[derive(Debug, Clone)]
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
pub struct ValidationError {
  /// Error message
  pub reason: String,
  /// Location in CDDL where error occurred (with context)
  pub cddl_location: CDDLLocation,
  /// Location in JSON/CBOR where error occurred (with context)
  pub data_location: DataLocation,
  /// Whether or not the error is associated with multiple type choices
  pub is_multi_type_choice: bool,
  /// Whether or not the error is associated with multiple group choices
  pub is_multi_group_choice: bool,
  /// Whether or not the error is associated with a group to choice enumeration
  pub is_group_to_choice_enum: bool,
  /// Error is associated with a type/group name group entry
  pub type_group_name_entry: Option<String>,
  /// Suggested fixes for the error
  pub suggestions: Vec<String>,
  /// Additional context for the error
  pub context: Option<String>,
}

/// Location information in CDDL with enhanced context
#[derive(Debug, Clone)]
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
pub struct CDDLLocation {
  /// The rule name that failed
  pub rule_name: Option<String>,
  /// The specific CDDL fragment that failed
  pub fragment: Option<String>,
  /// Line number in CDDL (if available)
  pub line: Option<usize>,
  /// Column number in CDDL (if available)
  pub column: Option<usize>,
  /// The full path to this location (e.g., "person.address.street")
  pub path: String,
}

/// Location information in JSON/CBOR with enhanced context
#[derive(Debug, Clone)]
#[cfg_attr(target_arch = "wasm32", derive(Serialize))]
pub struct DataLocation {
  /// JSON pointer path (e.g., "/person/age")
  pub path: String,
  /// The actual value at this location
  pub actual_value: String,
  /// The type of the actual value
  pub actual_type: String,
  /// Context around the location (parent structure info)
  pub context: Option<String>,
}

/// Enhanced validation error reporter
pub struct ValidationErrorReporter {
  /// Whether to use colored output
  pub use_colors: bool,
  /// Whether to include suggestions
  pub include_suggestions: bool,
  /// Whether to include context
  pub include_context: bool,
}

impl Default for ValidationErrorReporter {
  fn default() -> Self {
    Self {
      use_colors: true,
      include_suggestions: true,
      include_context: true,
    }
  }
}

impl ValidationErrorReporter {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn with_colors(mut self, use_colors: bool) -> Self {
    self.use_colors = use_colors;
    self
  }

  pub fn with_suggestions(mut self, include_suggestions: bool) -> Self {
    self.include_suggestions = include_suggestions;
    self
  }

  pub fn with_context(mut self, include_context: bool) -> Self {
    self.include_context = include_context;
    self
  }

  /// Format a single validation error with enhanced presentation
  pub fn format_error(&self, error: &ValidationError) -> String {
    let mut output = String::new();

    // Header with error type
    if self.use_colors {
      output.push_str(&format!("\x1b[1;31m✗ Validation Failed\x1b[0m\n"));
    } else {
      output.push_str("✗ Validation Failed\n");
    }

    // Main error message
    output.push_str(&format!("  {}\n\n", error.reason));

    // Location information
    self.format_locations(&mut output, error);

    // Context information
    if self.include_context && error.context.is_some() {
      if let Some(context) = &error.context {
        output.push_str(&format!("  Context: {}\n\n", context));
      }
    }

    // Type choice information
    if error.is_multi_type_choice || error.is_multi_group_choice || error.is_group_to_choice_enum {
      self.format_choice_info(&mut output, error);
    }

    // Suggestions
    if self.include_suggestions && !error.suggestions.is_empty() {
      self.format_suggestions(&mut output, error);
    }

    output
  }

  /// Format multiple validation errors with summary
  pub fn format_errors(&self, errors: &[ValidationError]) -> String {
    if errors.is_empty() {
      return String::new();
    }

    let mut output = String::new();

    // Summary header
    if errors.len() == 1 {
      output.push_str(&self.format_error(&errors[0]));
    } else {
      if self.use_colors {
        output.push_str(&format!(
          "\x1b[1;31m✗ {} Validation Errors Found\x1b[0m\n\n",
          errors.len()
        ));
      } else {
        output.push_str(&format!("✗ {} Validation Errors Found\n\n", errors.len()));
      }

      // Format each error
      for (i, error) in errors.iter().enumerate() {
        if self.use_colors {
          output.push_str(&format!("\x1b[1;33mError {}\x1b[0m\n", i + 1));
        } else {
          output.push_str(&format!("Error {}\n", i + 1));
        }
        output.push_str(&self.format_error(error));

        if i < errors.len() - 1 {
          output.push_str(&format!("{}\n", "─".repeat(60)));
        }
      }
    }

    output
  }

  fn format_locations(&self, output: &mut String, error: &ValidationError) {
    // Data location
    if self.use_colors {
      output.push_str(&format!(
        "  \x1b[1;34mData Location:\x1b[0m {}\n",
        error.data_location.path
      ));
      output.push_str(&format!(
        "  \x1b[1;34mActual Value:\x1b[0m {} ({})\n",
        error.data_location.actual_value, error.data_location.actual_type
      ));
    } else {
      output.push_str(&format!("  Data Location: {}\n", error.data_location.path));
      output.push_str(&format!(
        "  Actual Value: {} ({})\n",
        error.data_location.actual_value, error.data_location.actual_type
      ));
    }

    // CDDL location
    if let Some(rule_name) = &error.cddl_location.rule_name {
      if self.use_colors {
        output.push_str(&format!("  \x1b[1;32mCDDL Rule:\x1b[0m {}\n", rule_name));
      } else {
        output.push_str(&format!("  CDDL Rule: {}\n", rule_name));
      }
    }

    if let Some(fragment) = &error.cddl_location.fragment {
      if self.use_colors {
        output.push_str(&format!("  \x1b[1;32mCDDL Fragment:\x1b[0m {}\n", fragment));
      } else {
        output.push_str(&format!("  CDDL Fragment: {}\n", fragment));
      }
    }

    if !error.cddl_location.path.is_empty() {
      if self.use_colors {
        output.push_str(&format!(
          "  \x1b[1;32mCDDL Path:\x1b[0m {}\n",
          error.cddl_location.path
        ));
      } else {
        output.push_str(&format!("  CDDL Path: {}\n", error.cddl_location.path));
      }
    }

    output.push('\n');
  }

  fn format_choice_info(&self, output: &mut String, error: &ValidationError) {
    if error.is_multi_type_choice {
      if self.use_colors {
        output.push_str(
          "  \x1b[1;33mNote:\x1b[0m This error occurred while validating multiple type choices\n",
        );
      } else {
        output.push_str("  Note: This error occurred while validating multiple type choices\n");
      }
    }

    if error.is_multi_group_choice {
      if self.use_colors {
        output.push_str(
          "  \x1b[1;33mNote:\x1b[0m This error occurred while validating multiple group choices\n",
        );
      } else {
        output.push_str("  Note: This error occurred while validating multiple group choices\n");
      }
    }

    if error.is_group_to_choice_enum {
      if self.use_colors {
        output.push_str(
          "  \x1b[1;33mNote:\x1b[0m This error occurred in a group-to-choice enumeration\n",
        );
      } else {
        output.push_str("  Note: This error occurred in a group-to-choice enumeration\n");
      }
    }

    if let Some(entry) = &error.type_group_name_entry {
      if self.use_colors {
        output.push_str(&format!(
          "  \x1b[1;33mAssociated Rule:\x1b[0m \"{}\"\n",
          entry
        ));
      } else {
        output.push_str(&format!("  Associated Rule: \"{}\"\n", entry));
      }
    }

    output.push('\n');
  }

  fn format_suggestions(&self, output: &mut String, error: &ValidationError) {
    if self.use_colors {
      output.push_str("\x1b[1;36mSuggestions:\x1b[0m\n");
    } else {
      output.push_str("Suggestions:\n");
    }

    for suggestion in &error.suggestions {
      output.push_str(&format!("  • {}\n", suggestion));
    }
    output.push('\n');
  }
}

impl Default for ValidationError {
  fn default() -> Self {
    Self {
      reason: String::new(),
      cddl_location: CDDLLocation::default(),
      data_location: DataLocation::default(),
      is_multi_type_choice: false,
      is_multi_group_choice: false,
      is_group_to_choice_enum: false,
      type_group_name_entry: None,
      suggestions: Vec::new(),
      context: None,
    }
  }
}

impl Default for CDDLLocation {
  fn default() -> Self {
    Self {
      rule_name: None,
      fragment: None,
      line: None,
      column: None,
      path: String::new(),
    }
  }
}

impl Default for DataLocation {
  fn default() -> Self {
    Self {
      path: String::new(),
      actual_value: String::new(),
      actual_type: String::new(),
      context: None,
    }
  }
}

impl fmt::Display for ValidationError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let reporter = ValidationErrorReporter::new().with_colors(false);
    write!(f, "{}", reporter.format_error(self))
  }
}

/// Builder for creating enhanced validation errors
pub struct ValidationErrorBuilder {
  error: ValidationError,
}

impl ValidationErrorBuilder {
  pub fn new(reason: String) -> Self {
    Self {
      error: ValidationError {
        reason,
        ..Default::default()
      },
    }
  }

  pub fn with_cddl_location(
    mut self,
    rule_name: Option<String>,
    fragment: Option<String>,
    path: String,
  ) -> Self {
    self.error.cddl_location = CDDLLocation {
      rule_name,
      fragment,
      line: None,
      column: None,
      path,
    };
    self
  }

  pub fn with_data_location(
    mut self,
    path: String,
    actual_value: String,
    actual_type: String,
  ) -> Self {
    self.error.data_location = DataLocation {
      path,
      actual_value,
      actual_type,
      context: None,
    };
    self
  }

  pub fn with_suggestions(mut self, suggestions: Vec<String>) -> Self {
    self.error.suggestions = suggestions;
    self
  }

  pub fn with_context(mut self, context: String) -> Self {
    self.error.context = Some(context);
    self
  }

  pub fn with_multi_type_choice(mut self, is_multi: bool) -> Self {
    self.error.is_multi_type_choice = is_multi;
    self
  }

  pub fn with_multi_group_choice(mut self, is_multi: bool) -> Self {
    self.error.is_multi_group_choice = is_multi;
    self
  }

  pub fn with_group_to_choice_enum(mut self, is_enum: bool) -> Self {
    self.error.is_group_to_choice_enum = is_enum;
    self
  }

  pub fn with_type_group_name_entry(mut self, entry: Option<String>) -> Self {
    self.error.type_group_name_entry = entry;
    self
  }

  pub fn build(self) -> ValidationError {
    self.error
  }
}

/// Helper functions to determine data types and create suggestions
pub fn get_data_type_name(value: &str) -> String {
  if value == "null" {
    "null".to_string()
  } else if value == "true" || value == "false" {
    "boolean".to_string()
  } else if value.starts_with('"') && value.ends_with('"') {
    "string".to_string()
  } else if value.starts_with('[') && value.ends_with(']') {
    "array".to_string()
  } else if value.starts_with('{') && value.ends_with('}') {
    "object".to_string()
  } else if value.parse::<i64>().is_ok() {
    "integer".to_string()
  } else if value.parse::<f64>().is_ok() {
    "number".to_string()
  } else {
    "unknown".to_string()
  }
}

pub fn create_type_mismatch_suggestions(expected: &str, actual: &str) -> Vec<String> {
  let mut suggestions = Vec::new();

  match (expected, actual) {
    ("uint", "string") | ("integer", "string") => {
      suggestions.push("Convert the string to a number, e.g., \"123\" → 123".to_string());
      suggestions.push("Remove quotes around numeric values".to_string());
    }
    ("tstr" | "text", "integer") | ("tstr" | "text", "number") => {
      suggestions.push("Convert the number to a string, e.g., 123 → \"123\"".to_string());
      suggestions.push("Add quotes around the value".to_string());
    }
    ("bool", _) => {
      suggestions.push("Use true or false instead".to_string());
    }
    ("null", _) => {
      suggestions.push("Use null instead".to_string());
    }
    (_, _) => {
      suggestions.push(format!(
        "Change the value to match the expected type: {}",
        expected
      ));
    }
  }

  suggestions
    .push("Check your CDDL specification to ensure the type requirement is correct".to_string());
  suggestions
}

pub fn create_missing_field_suggestions(field: &str) -> Vec<String> {
  vec![
    format!("Add the required field \"{}\" to your data", field),
    format!(
      "Make the field optional in CDDL with '{}?:' if it's not always required",
      field
    ),
    "Check if the field name is spelled correctly".to_string(),
  ]
}

pub fn create_unexpected_field_suggestions(field: &str) -> Vec<String> {
  vec![
    format!("Remove the field \"{}\" from your data", field),
    format!(
      "Add the field \"{}\" to your CDDL specification if it should be allowed",
      field
    ),
    "Check if the field name is spelled correctly".to_string(),
  ]
}

pub fn create_array_length_suggestions(expected: usize, actual: usize) -> Vec<String> {
  let mut suggestions = Vec::new();

  if actual < expected {
    suggestions.push(format!(
      "Add {} more item(s) to the array",
      expected - actual
    ));
  } else if actual > expected {
    suggestions.push(format!(
      "Remove {} item(s) from the array",
      actual - expected
    ));
  }

  suggestions.push(
    "Check if the array should have a different occurrence indicator (*, +, ?) in CDDL".to_string(),
  );
  suggestions.push("Verify the expected array length in your CDDL specification".to_string());

  suggestions
}

pub fn create_range_suggestions(expected_range: &str, actual_value: &str) -> Vec<String> {
  vec![
    format!("Use a value within the range {}", expected_range),
    format!(
      "Current value {} is outside the expected range",
      actual_value
    ),
    "Check if the range constraints in your CDDL are correct".to_string(),
  ]
}

/// Helper trait to create suggestions for common validation errors
pub trait ValidationSuggestions {
  fn suggest_type_mismatch(&self, expected: &str, actual: &str) -> Vec<String>;
  fn suggest_missing_field(&self, field: &str) -> Vec<String>;
  fn suggest_unexpected_field(&self, field: &str) -> Vec<String>;
  fn suggest_array_length(&self, expected: usize, actual: usize) -> Vec<String>;
  fn suggest_range_error(&self, expected_range: &str, actual: &str) -> Vec<String>;
}

impl ValidationSuggestions for Vec<String> {
  fn suggest_type_mismatch(&self, expected: &str, actual: &str) -> Vec<String> {
    let mut suggestions = Vec::new();

    match (expected, actual) {
      ("uint", "string") => {
        suggestions.push(format!(
          "Convert the string to a number, e.g., \"123\" → 123"
        ));
        suggestions.push(format!(
          "Check if the value should be a numeric type in your data"
        ));
      }
      ("tstr", "number") => {
        suggestions.push(format!(
          "Convert the number to a string, e.g., 123 → \"123\""
        ));
        suggestions.push(format!(
          "Check if the value should be a string type in your CDDL"
        ));
      }
      ("bool", _) => {
        suggestions.push(format!("Use true or false instead of {}", actual));
      }
      _ => {
        suggestions.push(format!("Expected {} but got {}", expected, actual));
        suggestions.push(format!("Check your data format and CDDL specification"));
      }
    }

    suggestions
  }

  fn suggest_missing_field(&self, field: &str) -> Vec<String> {
    vec![
      format!("Add the required field \"{}\" to your data", field),
      format!(
        "Check if \"{}\" should be marked as optional with '?' in CDDL",
        field
      ),
    ]
  }

  fn suggest_unexpected_field(&self, field: &str) -> Vec<String> {
    vec![
      format!("Remove the unexpected field \"{}\" from your data", field),
      format!(
        "Add \"{}\" to your CDDL specification if it should be allowed",
        field
      ),
    ]
  }

  fn suggest_array_length(&self, expected: usize, actual: usize) -> Vec<String> {
    if actual < expected {
      vec![
        format!("Add {} more items to the array", expected - actual),
        format!("Check if the array should have a different occurrence indicator in CDDL"),
      ]
    } else {
      vec![
        format!("Remove {} items from the array", actual - expected),
        format!("Check if the array should allow more items with '*' or '+' in CDDL"),
      ]
    }
  }

  fn suggest_range_error(&self, expected_range: &str, actual: &str) -> Vec<String> {
    vec![
      format!("Use a value within the range {}", expected_range),
      format!("Current value {} is outside the expected range", actual),
    ]
  }
}
