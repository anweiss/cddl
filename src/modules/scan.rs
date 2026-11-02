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
//! String literals, comments and non-reference identifier roles must be kept
//! distinct: a false reference can pull in an unrelated rule or alter literal
//! data during namespace rewriting.

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
  /// A generic parameter declaration or use, which is local to that rule.
  GenericParam,
  /// A literal bareword member key before `:`, not a rule reference.
  BarewordKey,
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
  /// Whether the rule extends an existing definition (`/=` or `//=`) rather
  /// than defining the name.
  pub extension: bool,
  /// The generic parameters declared by this definition, if any.
  pub params: Vec<String>,
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

/// Skip a literal, respecting escapes only in text strings.
fn literal_end(bytes: &[u8], start: usize) -> Option<usize> {
  let (mut i, quote, escaped) = match bytes[start] {
    b'h' if matches!(bytes.get(start + 1), Some(b'\'' | b'"')) => {
      (start + 2, bytes[start + 1], false)
    }
    b'"' => (start + 1, b'"', true),
    b'\'' => (start + 1, b'\'', false),
    _ => return None,
  };
  while i < bytes.len() {
    if bytes[i] == quote {
      return Some(i + 1);
    }
    if escaped && bytes[i] == b'\\' {
      i += 1;
    }
    i += 1;
  }
  Some(bytes.len())
}

/// End of a tag or major-type head: `#`, `#7`, `#6.32`. The grammar allows
/// spacing and comments between the components, as in `#7. 32`. The dot is
/// consumed only when a tag value follows; in `#7.eq 32` it introduces a
/// control operator instead. Tag values are unsigned integers, so `#7.32e5` ends
/// after `32`.
fn tag_end(bytes: &[u8], start: usize) -> usize {
  let mut i = start + 1;
  let major = skip_spacing(bytes, i);
  if bytes.get(major).is_some_and(u8::is_ascii_digit) {
    i = major + 1;
    let dot = skip_spacing(bytes, i);
    let value = skip_spacing(bytes, dot + 1);
    if bytes.get(dot) == Some(&b'.') && bytes.get(value).is_some_and(u8::is_ascii_digit) {
      i = uint_end(bytes, value).0;
    }
  }
  i
}

/// Directive comments inside a tag head, which can contain only spacing,
/// comments and numeric characters, so any `;` there starts a comment.
fn tag_directives(
  bytes: &[u8],
  start: usize,
  end: usize,
  ranges: &mut Vec<core::ops::Range<usize>>,
) {
  let mut i = start;
  while i < end {
    if bytes[i] != b';' {
      i += 1;
      continue;
    }
    let comment = i;
    while i < bytes.len() && bytes[i] != b'\n' {
      i += 1;
    }
    if bytes[comment - 1] == b'\n' && bytes.get(comment + 1) == Some(&b'#') {
      ranges.push(comment..i + usize::from(i < bytes.len()));
    }
  }
}

/// End of an occurrence indicator's upper bound, for the `*` at `star`. The
/// bound is an unsigned integer, so in `1*3e3` it ends after `3` and `e3` is
/// a reference.
fn occurrence_end(bytes: &[u8], star: usize) -> usize {
  if bytes.get(star + 1).is_some_and(u8::is_ascii_digit) {
    uint_end(bytes, star + 1).0
  } else {
    star + 1
  }
}

/// Actual directive comments at column zero, excluding literal contents.
pub(super) fn directive_ranges(src: &str) -> Vec<core::ops::Range<usize>> {
  let bytes = src.as_bytes();
  let mut ranges = Vec::new();
  let mut i = 0;
  while i < bytes.len() {
    // Identifiers and numbers are skipped as whole tokens, so a byte-string
    // prefix is only recognized where a token can start: the `h` ending
    // `path` in `path"..."` is not one.
    if let Some(end) = literal_end(bytes, i) {
      i = end;
    } else if is_ident_start(bytes[i]) {
      i = ident_end(bytes, i);
    } else if bytes[i] == b'#' {
      let end = tag_end(bytes, i);
      tag_directives(bytes, i + 1, end, &mut ranges);
      i = end;
    } else if bytes[i] == b'*' {
      i = occurrence_end(bytes, i);
    } else if bytes[i].is_ascii_digit() {
      i = number_end(bytes, i);
    } else if bytes[i] == b';' {
      let start = i;
      while i < bytes.len() && bytes[i] != b'\n' {
        i += 1;
      }
      if (start == 0 || bytes[start - 1] == b'\n') && bytes.get(start + 1) == Some(&b'#') {
        ranges.push(start..i + usize::from(i < bytes.len()));
      }
    } else {
      i += 1;
    }
  }
  ranges
}

fn skip_spacing(bytes: &[u8], mut i: usize) -> usize {
  loop {
    match bytes.get(i) {
      Some(b' ' | b'\t' | b'\r' | b'\n') => i += 1,
      Some(b';') => {
        while i < bytes.len() && bytes[i] != b'\n' {
          i += 1;
        }
      }
      _ => return i,
    }
  }
}

fn ident_end(bytes: &[u8], mut i: usize) -> usize {
  while i < bytes.len() {
    let c = bytes[i];
    if is_ident_continue(c) || c == b'-' {
      i += 1;
    } else if c == b'.'
      && i + 1 < bytes.len()
      && (is_ident_continue(bytes[i + 1]) || bytes[i + 1] == b'-')
    {
      i += 2;
    } else {
      break;
    }
  }
  i
}

fn uint_end(bytes: &[u8], mut i: usize) -> (usize, u32) {
  let radix = match (bytes[i], bytes.get(i + 1), bytes.get(i + 2)) {
    (b'0', Some(b'x' | b'X'), Some(digit)) if digit.is_ascii_hexdigit() => 16,
    (b'0', Some(b'b' | b'B'), Some(b'0' | b'1')) => 2,
    _ => 10,
  };
  if radix != 10 {
    i += 2;
  }
  if radix == 10 && bytes[i] == b'0' {
    i += 1;
  } else {
    while bytes.get(i).is_some_and(|b| (*b as char).is_digit(radix)) {
      i += 1;
    }
  }
  (i, radix)
}

fn number_end(bytes: &[u8], start: usize) -> usize {
  let (mut i, radix) = uint_end(bytes, start);
  let integer_end = i;
  if radix == 2 {
    return i;
  }
  if bytes.get(i) == Some(&b'.')
    && bytes
      .get(i + 1)
      .is_some_and(|b| (*b as char).is_digit(radix))
  {
    i += 1;
    while bytes.get(i).is_some_and(|b| (*b as char).is_digit(radix)) {
      i += 1;
    }
  }
  let mantissa_end = i;
  let exponent = if radix == 16 { b'p' } else { b'e' };
  if bytes
    .get(i)
    .is_some_and(|b| b.to_ascii_lowercase() == exponent)
  {
    i += 1;
    if matches!(bytes.get(i), Some(b'+' | b'-')) {
      i += 1;
    }
    let digits = i;
    while bytes.get(i).is_some_and(u8::is_ascii_digit) {
      i += 1;
    }
    if i > digits {
      return i;
    }
  }
  // A hex fraction belongs to a number only when followed by a p exponent.
  if radix == 16 {
    integer_end
  } else {
    mantissa_end
  }
}

/// If the `<` at `open` begins a generic *parameter* list — that is, if the
/// group it opens is followed by an assignment operator — return the byte
/// offset of its closing `>`.
fn generic_params_end(bytes: &[u8], open: usize) -> Option<usize> {
  let mut i = skip_spacing(bytes, open + 1);
  while i < bytes.len() && bytes[i] != b'>' {
    // Parameters are plain identifiers separated by commas and whitespace;
    // anything structural means this is not a parameter list.
    if matches!(bytes[i], b'(' | b'[' | b'{' | b'<' | b'"' | b'\'') {
      return None;
    }
    i = skip_spacing(bytes, i + 1);
  }

  if i >= bytes.len() {
    return None;
  }

  let close = i;
  let mut j = skip_spacing(bytes, close + 1);

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
  while i < bytes.len() {
    let b = bytes[i];

    if let Some(end) = literal_end(bytes, i) {
      i = end;
      continue;
    }

    match b {
      b' ' | b'\t' | b'\r' | b'\n' => {
        i += 1;
      }

      // Comment (including a `;#` directive line) runs to end of line.
      b';' => {
        while i < bytes.len() && bytes[i] != b'\n' {
          i += 1;
        }
      }

      b'b' if bytes[i..].starts_with(b"b64'") => {
        i += 3;
      }

      b'(' | b'[' | b'{' => {
        depth += 1;
        i += 1;
      }

      b')' | b']' | b'}' => {
        depth = depth.saturating_sub(1);
        i += 1;
      }

      // A dot that reaches here is never part of an identifier (`ident_end`
      // consumes `cose.label` whole), so a single dot introduces a control
      // operator. The grammar allows spacing and comments between the dot
      // and the operator name; either way the name is not a reference.
      b'.' => {
        let start = i;
        i += 1;
        // `..` and `...` are range operators, not control operators.
        while i < bytes.len() && bytes[i] == b'.' {
          i += 1;
        }
        let single = i == start + 1;
        if single {
          i = skip_spacing(bytes, i);
        }
        if single && i < bytes.len() && is_ident_start(bytes[i]) {
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
      }

      // Generic parameters: a `<` group that is immediately followed by an
      // assignment operator, i.e. the head of `messages<a, b> = ...`. Anywhere
      // else a `<` group holds generic *arguments*, whose identifiers are real
      // references and are scanned normally.
      b'<' if depth == 0 => match generic_params_end(bytes, i) {
        Some(close) => {
          i = skip_spacing(bytes, i + 1);
          while i < close {
            if is_ident_start(bytes[i]) {
              let start = i;
              i = ident_end(bytes, i);
              idents.push(IdentToken {
                start,
                end: i,
                text: src[start..i].to_string(),
                role: IdentRole::GenericParam,
              });
            } else {
              i += 1;
            }
            i = skip_spacing(bytes, i);
          }
          i = close + 1;
        }
        None => i += 1,
      },

      // Assignment operators: `=`, `/=`, `//=`. `=>` is an entry separator and
      // must not be mistaken for one.
      b'=' if i + 1 >= bytes.len() || bytes[i + 1] != b'>' => {
        if depth == 0 {
          top_level_assigns.push(i);
        }
        i += 1;
      }

      // Keep a tag's separator out of decimal-fraction scanning.
      b'#' => {
        i = tag_end(bytes, i);
      }

      b'*' => {
        i = occurrence_end(bytes, i);
      }

      b'0'..=b'9' => {
        i = number_end(bytes, i);
      }

      _ if is_ident_start(b) => {
        let start = i;
        i = ident_end(bytes, i);
        idents.push(IdentToken {
          start,
          end: i,
          text: src[start..i].to_string(),
          role: if bytes.get(skip_spacing(bytes, i)) == Some(&b':') {
            IdentRole::BarewordKey
          } else {
            IdentRole::Reference
          },
        });
      }

      _ => {
        i += 1;
      }
    }
  }

  // The head of a rule is the last plain reference occurring before its
  // assignment operator; generic parameters and control names are excluded by
  // construction.
  let heads = assignment_heads(&idents, &top_level_assigns);

  for (idx, _) in &heads {
    idents[*idx].role = IdentRole::RuleHead;
  }

  let mut rules: Vec<RuleSpan> = Vec::new();
  for (n, (idx, assign)) in heads.iter().enumerate() {
    let next = heads
      .get(n + 1)
      .map(|(idx, _)| *idx)
      .unwrap_or(idents.len());
    let params: Vec<String> = idents[*idx..next]
      .iter()
      .filter(|ident| ident.role == IdentRole::GenericParam)
      .map(|ident| ident.text.clone())
      .collect();
    for ident in &mut idents[*idx..next] {
      if ident.role == IdentRole::Reference && params.contains(&ident.text) {
        ident.role = IdentRole::GenericParam;
      }
    }

    let start = idents[*idx].start;
    let end = match heads.get(n + 1) {
      Some((next, _)) => idents[*next].start,
      None => src.len(),
    };
    let end = src[start..end].trim_end().len() + start;

    // `/=` and `//=` extend an existing rule; the `/` directly precedes `=`.
    let extension = assign > &0 && bytes[assign - 1] == b'/';

    rules.push(RuleSpan {
      name: idents[*idx].text.clone(),
      start,
      end,
      extension,
      params,
    });
  }

  Scan { idents, rules }
}

/// Both lists are source-ordered, so recovering heads needs only one pass.
fn assignment_heads(idents: &[IdentToken], assigns: &[usize]) -> Vec<(usize, usize)> {
  let mut heads = Vec::new();
  let mut cursor = 0;
  let mut head = None;
  for assign in assigns {
    while cursor < idents.len() && idents[cursor].end <= *assign {
      if idents[cursor].role == IdentRole::Reference {
        head = Some(cursor);
      }
      cursor += 1;
    }
    if let Some(idx) = head {
      if heads.last().map(|(previous, _)| *previous) != Some(idx) {
        heads.push((idx, *assign));
      }
    }
  }
  heads
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
  fn recovers_many_source_ordered_assignment_heads() {
    let mut src = String::new();
    for n in 0..10_000 {
      src.push_str(&format!("rule{}<a> = [a, int]\n", n));
    }
    let scanned = scan(&src);
    assert_eq!(scanned.rules.len(), 10_000);
    for (n, rule) in scanned.rules.iter().enumerate() {
      assert_eq!(rule.name, format!("rule{}", n));
      assert_eq!(rule.params, ["a"]);
    }
  }

  #[test]
  fn assignment_head_deduplication_keeps_only_the_last_candidate() {
    let scanned = scan("rule = = int\nnext = tstr\n");
    let names: Vec<_> = scanned
      .rules
      .iter()
      .map(|rule| rule.name.as_str())
      .collect();
    assert_eq!(names, ["rule", "next"]);
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
      .all(|i| i.role == IdentRole::GenericParam));
  }

  #[test]
  fn generic_parameters_are_scoped_to_each_rule() {
    let scan =
      scan("a = int\nmessages<a, ; parameter comment\n b.c> = [a, b.c]\nother = [a, b.c]\n");
    let roles: Vec<_> = scan
      .idents
      .iter()
      .filter(|i| i.text == "a" || i.text == "b.c")
      .map(|i| i.role)
      .collect();
    assert_eq!(
      roles,
      [
        IdentRole::RuleHead,
        IdentRole::GenericParam,
        IdentRole::GenericParam,
        IdentRole::GenericParam,
        IdentRole::GenericParam,
        IdentRole::Reference,
        IdentRole::Reference,
      ]
    );
  }

  #[test]
  fn bareword_keys_are_not_references() {
    let scan = scan("m = {label: tstr, ? label ; comment\n : int, * label => tstr}\n");
    let roles: Vec<_> = scan
      .idents
      .iter()
      .filter(|i| i.text == "label")
      .map(|i| i.role)
      .collect();
    assert_eq!(
      roles,
      [
        IdentRole::BarewordKey,
        IdentRole::BarewordKey,
        IdentRole::Reference
      ]
    );
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

  #[test]
  fn byte_string_prefixes_are_not_references() {
    let scan = scan("a = [h'00', b64'AA==', h\"00\", h, b64]\n");
    let references: Vec<_> = scan
      .idents
      .iter()
      .filter(|ident| ident.role == IdentRole::Reference)
      .map(|ident| ident.text.as_str())
      .collect();
    assert_eq!(references, ["h", "b64"]);
  }

  #[test]
  fn numeric_tokens_leave_adjacent_references_and_controls_intact() {
    for (number, rest) in [
      ("0", "x"),
      ("0", "b"),
      ("0", "42"),
      ("0", ""),
      ("10", ""),
      ("0xff", ""),
      ("0b10", ""),
      ("1.5", "e"),
      ("1", "e+"),
      ("0x1", ".ap"),
      ("1e+5", ".eq limit"),
      ("0x1.8p-1", "..limit"),
      ("0b10", "...limit"),
      ("1", "item"),
    ] {
      let input = format!("{}{}", number, rest);
      assert_eq!(number_end(input.as_bytes(), 0), number.len(), "{}", input);
    }
    let scanned = scan("value = [1e+5 item, 0x10 other, 0b10 third]\n");
    let references: Vec<_> = scanned
      .idents
      .iter()
      .filter(|ident| ident.role == IdentRole::Reference)
      .map(|ident| ident.text.as_str())
      .collect();
    assert_eq!(references, ["item", "other", "third"]);
    let scan = scan("value = 1e+5.eq limit\n");
    assert!(scan
      .idents
      .iter()
      .any(|ident| ident.text == "eq" && ident.role == IdentRole::ControlName));
    assert!(scan
      .idents
      .iter()
      .any(|ident| ident.text == "limit" && ident.role == IdentRole::Reference));
  }

  #[test]
  fn tag_numbers_leave_adjacent_references_intact() {
    let scan = scan("value = [#7.32e5, #7.0x20p1, #7.0b100000e5]\n");
    let references: Vec<_> = scan
      .idents
      .iter()
      .filter(|ident| ident.role == IdentRole::Reference)
      .map(|ident| ident.text.as_str())
      .collect();
    assert_eq!(references, ["e5", "p1", "e5"]);
  }

  #[test]
  fn spaced_control_names_are_not_references() {
    let scan = scan("a = bstr . ; note\n size 4\nb = c. size 2\nd = 1 .. e\nf = g.");
    let controls: Vec<_> = scan
      .idents
      .iter()
      .filter(|ident| ident.role == IdentRole::ControlName)
      .map(|ident| ident.text.as_str())
      .collect();
    assert_eq!(controls, ["size", "size"]);
    assert!(scan
      .idents
      .iter()
      .any(|ident| ident.text == "e" && ident.role == IdentRole::Reference));
  }

  #[test]
  fn tag_heads_and_occurrence_bounds_scan_as_unsigned_integers() {
    let scan = scan("value = [#7.32e5, #7.eq 32, 1*3e3]\n");
    let roles: Vec<_> = scan
      .idents
      .iter()
      .map(|ident| (ident.text.as_str(), ident.role))
      .collect();
    assert_eq!(
      roles,
      [
        ("value", IdentRole::RuleHead),
        ("e5", IdentRole::Reference),
        ("eq", IdentRole::ControlName),
        ("e3", IdentRole::Reference),
      ]
    );
  }

  #[test]
  fn tag_heads_and_occurrence_bounds_respect_literal_boundaries() {
    // A real text string after a tag value or occurrence bound must not be
    // mistaken for an `h"..."` byte string ending at its escaped quote.
    for head in ["#7.32e5h", "1*3e3h"] {
      let src = format!(
        "a = [{}\"x\\\"\n;# import missing\n\"]\n;# import real\n",
        head
      );
      let ranges = directive_ranges(&src);
      assert_eq!(ranges.len(), 1, "{}", src);
      assert_eq!(&src[ranges[0].clone()], ";# import real\n", "{}", src);
    }
  }

  #[test]
  fn spaced_tag_heads_scan_as_unsigned_integers() {
    let scan = scan("value = [#7. 32e5, # 6 ; note\n . 32e3, #7 .eq 32]\n");
    let roles: Vec<_> = scan
      .idents
      .iter()
      .map(|ident| (ident.text.as_str(), ident.role))
      .collect();
    assert_eq!(
      roles,
      [
        ("value", IdentRole::RuleHead),
        ("e5", IdentRole::Reference),
        ("e3", IdentRole::Reference),
        ("eq", IdentRole::ControlName),
      ]
    );
  }

  #[test]
  fn directives_inside_spaced_tag_heads_are_found() {
    let src = "a = [#7.\n;# import real\n32e5h\"x\\\"\n;# import missing\n\"]\n";
    let ranges = directive_ranges(src);
    assert_eq!(ranges.len(), 1, "{}", src);
    assert_eq!(&src[ranges[0].clone()], ";# import real\n");
  }

  #[test]
  fn byte_string_prefixes_need_a_token_boundary() {
    let src = "a = { path\"x\\\"\n;# import missing\n\": int }\n;# import real\n";
    let ranges = directive_ranges(src);
    assert_eq!(ranges.len(), 1);
    assert_eq!(&src[ranges[0].clone()], ";# import real\n");
  }

  #[test]
  fn text_strings_still_honor_escaped_quotes() {
    let scan = scan(
      r#"value = "escaped \"not = a rule"
following = int
"#,
    );
    let names: Vec<_> = scan.rules.iter().map(|rule| rule.name.as_str()).collect();
    assert_eq!(names, ["value", "following"]);
  }
}
