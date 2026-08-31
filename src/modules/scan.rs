//! A minimal structural scanner over CDDL source text.
//!
//! Module resolution is a source-to-source transformation, so it needs three
//! things from a module's text and nothing more: where each rule begins and
//! ends, what each rule is named, and which identifiers a rule references. A
//! full parse would give all three, but it would also discard the original
//! formatting that the resolved output is expected to preserve, and it would
//! couple resolution to the AST's lifetimes. This scanner works directly on the
//! text instead.
//!
//! It is deliberately conservative: string literals, comments and control
//! operator names are recognized only well enough to keep them from being
//! mistaken for rule references. A false positive is harmless — an identifier
//! that names no rule in the module is simply ignored during selection.

#[cfg(not(feature = "std"))]
use alloc::{
  string::{String, ToString},
  vec::Vec,
};

/// What an identifier occurrence means structurally.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IdentRole {
  /// The name being defined by a rule.
  RuleHead,
  /// A reference to some other name.
  Reference,
  /// The name of a control operator, e.g. the `size` in `.size`.
  ControlName,
  /// A generic parameter in a rule head, which is local to that rule.
  GenericParam,
}

/// A single identifier occurrence, as a byte range into the scanned source.
#[derive(Debug, Clone)]
pub(crate) struct IdentToken {
  /// Byte offset of the first character.
  pub start: usize,
  /// Byte offset one past the last character.
  pub end: usize,
  /// The identifier text.
  pub text: String,
  /// What this occurrence means.
  pub role: IdentRole,
}

/// The extent of a single rule definition.
#[derive(Debug, Clone)]
pub(crate) struct RuleSpan {
  /// The defined name.
  pub name: String,
  /// Byte offset where the rule begins (at its head identifier).
  pub start: usize,
  /// Byte offset one past the end of the rule, trailing whitespace trimmed.
  pub end: usize,
}

/// The result of scanning a CDDL document.
#[derive(Debug, Clone, Default)]
pub(crate) struct Scan {
  /// Every identifier occurrence, in source order.
  pub idents: Vec<IdentToken>,
  /// Every rule definition, in source order.
  pub rules: Vec<RuleSpan>,
}

fn is_ident_start(b: u8) -> bool {
  b == b'$' || b == b'_' || b == b'@' || b.is_ascii_alphabetic()
}

fn is_ident_continue(b: u8) -> bool {
  is_ident_start(b) || b.is_ascii_digit()
}

/// If the `<` at `open` begins a generic *parameter* list — that is, if the
/// group it opens is followed by an assignment operator — return the byte
/// offset of its closing `>`.
fn generic_params_end(bytes: &[u8], open: usize) -> Option<usize> {
  let mut i = open + 1;
  while i < bytes.len() && bytes[i] != b'>' {
    // Parameters are plain identifiers separated by commas and whitespace;
    // anything structural means this is not a parameter list.
    if matches!(bytes[i], b'(' | b'[' | b'{' | b'<' | b'"' | b'\'' | b';') {
      return None;
    }
    i += 1;
  }

  if i >= bytes.len() {
    return None;
  }

  let close = i;
  let mut j = close + 1;
  while j < bytes.len() && matches!(bytes[j], b' ' | b'\t' | b'\r' | b'\n') {
    j += 1;
  }

  while j < bytes.len() && bytes[j] == b'/' {
    j += 1;
  }

  if j < bytes.len() && bytes[j] == b'=' && !(j + 1 < bytes.len() && bytes[j + 1] == b'>') {
    Some(close)
  } else {
    None
  }
}

/// Scan `src`, recovering rule extents and identifier occurrences.
pub(crate) fn scan(src: &str) -> Scan {
  let bytes = src.as_bytes();
  let mut idents: Vec<IdentToken> = Vec::new();
  // Byte offsets of assignment operators found at bracket depth zero.
  let mut top_level_assigns: Vec<usize> = Vec::new();

  let mut i = 0usize;
  let mut depth = 0usize;
  // Byte offset of the previous non-whitespace byte, used to tell a control
  // operator (`bstr .size 4`, where `.` follows whitespace) from a namespaced
  // identifier (`cose.label`, where `.` is part of the identifier itself).
  let mut prev_was_ident_end = false;

  while i < bytes.len() {
    let b = bytes[i];

    match b {
      b' ' | b'\t' | b'\r' | b'\n' => {
        prev_was_ident_end = false;
        i += 1;
      }

      // Comment (including a `;#` directive line) runs to end of line.
      b';' => {
        prev_was_ident_end = false;
        while i < bytes.len() && bytes[i] != b'\n' {
          i += 1;
        }
      }

      // Text string.
      b'"' => {
        prev_was_ident_end = false;
        i += 1;
        while i < bytes.len() {
          match bytes[i] {
            b'\\' => i += 2,
            b'"' => {
              i += 1;
              break;
            }
            _ => i += 1,
          }
        }
      }

      // Byte string.
      b'\'' => {
        prev_was_ident_end = false;
        i += 1;
        while i < bytes.len() {
          match bytes[i] {
            b'\\' => i += 2,
            b'\'' => {
              i += 1;
              break;
            }
            _ => i += 1,
          }
        }
      }

      b'(' | b'[' | b'{' => {
        prev_was_ident_end = false;
        depth += 1;
        i += 1;
      }

      b')' | b']' | b'}' => {
        prev_was_ident_end = false;
        depth = depth.saturating_sub(1);
        i += 1;
      }

      // A control operator: a `.` that does not directly follow an identifier
      // character. The name that follows is an operator name, not a reference.
      b'.' if !prev_was_ident_end => {
        i += 1;
        // `..` and `...` are range operators, not control operators.
        while i < bytes.len() && bytes[i] == b'.' {
          i += 1;
        }
        if i < bytes.len() && is_ident_start(bytes[i]) {
          let start = i;
          while i < bytes.len() && (is_ident_continue(bytes[i]) || bytes[i] == b'-') {
            i += 1;
          }
          idents.push(IdentToken {
            start,
            end: i,
            text: src[start..i].to_string(),
            role: IdentRole::ControlName,
          });
        }
        prev_was_ident_end = false;
      }

      // Generic parameters: a `<` group that is immediately followed by an
      // assignment operator, i.e. the head of `messages<a, b> = ...`. Anywhere
      // else a `<` group holds generic *arguments*, whose identifiers are real
      // references and are scanned normally.
      b'<' if depth == 0 => {
        match generic_params_end(bytes, i) {
          Some(close) => {
            i += 1;
            while i < close {
              if is_ident_start(bytes[i]) {
                let start = i;
                while i < close && (is_ident_continue(bytes[i]) || bytes[i] == b'-') {
                  i += 1;
                }
                idents.push(IdentToken {
                  start,
                  end: i,
                  text: src[start..i].to_string(),
                  role: IdentRole::GenericParam,
                });
              } else {
                i += 1;
              }
            }
            i = close + 1;
          }
          None => i += 1,
        }

        prev_was_ident_end = false;
      }

      // Assignment operators: `=`, `/=`, `//=`. `=>` is an entry separator and
      // must not be mistaken for one.
      b'=' if i + 1 >= bytes.len() || bytes[i + 1] != b'>' => {
        if depth == 0 {
          top_level_assigns.push(i);
        }
        prev_was_ident_end = false;
        i += 1;
      }

      _ if is_ident_start(b) => {
        let start = i;
        while i < bytes.len() {
          let c = bytes[i];
          if is_ident_continue(c) || c == b'-' {
            i += 1;
          } else if c == b'.'
            && i + 1 < bytes.len()
            && (is_ident_continue(bytes[i + 1]) || bytes[i + 1] == b'-')
          {
            // A namespaced name such as `cose.label`: the dot binds tightly to
            // the surrounding identifier characters.
            i += 2;
          } else {
            break;
          }
        }
        idents.push(IdentToken {
          start,
          end: i,
          text: src[start..i].to_string(),
          role: IdentRole::Reference,
        });
        prev_was_ident_end = true;
      }

      _ => {
        prev_was_ident_end = false;
        i += 1;
      }
    }
  }

  // The head of a rule is the last plain reference occurring before its
  // assignment operator; generic parameters and control names are excluded by
  // construction.
  let mut head_indices: Vec<usize> = Vec::new();
  for assign in &top_level_assigns {
    let head = idents
      .iter()
      .enumerate()
      .rev()
      .find(|(_, id)| id.end <= *assign && id.role == IdentRole::Reference)
      .map(|(idx, _)| idx);

    if let Some(idx) = head {
      if !head_indices.contains(&idx) {
        head_indices.push(idx);
      }
    }
  }

  for idx in &head_indices {
    idents[*idx].role = IdentRole::RuleHead;
  }

  let mut rules: Vec<RuleSpan> = Vec::new();
  for (n, idx) in head_indices.iter().enumerate() {
    let start = idents[*idx].start;
    let end = match head_indices.get(n + 1) {
      Some(next) => idents[*next].start,
      None => src.len(),
    };
    let end = src[start..end].trim_end().len() + start;

    rules.push(RuleSpan {
      name: idents[*idx].text.clone(),
      start,
      end,
    });
  }

  Scan { idents, rules }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn finds_rule_extents_and_names() {
    let src = "label = int / tstr\nvalues = any\n";
    let scan = scan(src);
    let names: Vec<_> = scan.rules.iter().map(|r| r.name.as_str()).collect();
    assert_eq!(names, ["label", "values"]);
    assert_eq!(
      &src[scan.rules[0].start..scan.rules[0].end],
      "label = int / tstr"
    );
  }

  #[test]
  fn namespaced_names_scan_as_one_identifier() {
    let scan = scan("mydata = {Fritz: cose.empty_or_serialized_map}");
    assert!(scan
      .idents
      .iter()
      .any(|i| i.text == "cose.empty_or_serialized_map"));
  }

  #[test]
  fn control_names_are_not_references() {
    let scan = scan("a = bstr .cbor header_map");
    let cbor = scan.idents.iter().find(|i| i.text == "cbor").unwrap();
    assert_eq!(cbor.role, IdentRole::ControlName);
    assert!(scan
      .idents
      .iter()
      .any(|i| i.text == "header_map" && i.role == IdentRole::Reference));
  }

  #[test]
  fn entry_separator_is_not_an_assignment() {
    let scan = scan("m = {\n  1 => tstr,\n}\nn = int\n");
    let names: Vec<_> = scan.rules.iter().map(|r| r.name.as_str()).collect();
    assert_eq!(names, ["m", "n"]);
  }

  #[test]
  fn generic_parameters_are_local() {
    let scan = scan("messages<a, b> = [a, b]\n");
    assert_eq!(scan.rules.len(), 1);
    assert_eq!(scan.rules[0].name, "messages");
    assert!(scan
      .idents
      .iter()
      .filter(|i| i.text == "a")
      .any(|i| i.role == IdentRole::GenericParam));
  }

  #[test]
  fn generic_parameters_may_span_lines() {
    let scan = scan("messages<\n  a,\n  b\n> = [a, b]\n");
    assert_eq!(scan.rules.len(), 1);
    assert_eq!(scan.rules[0].name, "messages");
  }

  #[test]
  fn generic_arguments_are_references() {
    let scan = scan("a = messages<int>\n");
    assert_eq!(scan.rules.len(), 1);
    assert_eq!(scan.rules[0].name, "a");
    assert!(scan
      .idents
      .iter()
      .any(|i| i.text == "messages" && i.role == IdentRole::Reference));
  }

  #[test]
  fn extensions_are_separate_spans() {
    let scan = scan("foo = int\nbar = tstr\nfoo /= tstr\n");
    let names: Vec<_> = scan.rules.iter().map(|r| r.name.as_str()).collect();
    assert_eq!(names, ["foo", "bar", "foo"]);
  }

  #[test]
  fn comments_and_strings_are_skipped() {
    let scan = scan("a = \"not_a_rule = x\" ; nor = this\nb = int\n");
    let names: Vec<_> = scan.rules.iter().map(|r| r.name.as_str()).collect();
    assert_eq!(names, ["a", "b"]);
  }
}
