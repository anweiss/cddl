//! Parsing of `;#` module directives per Appendix A of
//! draft-ietf-cbor-cddl-modules-06.
//!
//! ```abnf
//! directive = ";#" RS (%s"import" / %s"include") RS [from-clause]
//!                     filename [as-clause] CRLF
//! from-clause = 1*(id-or-all [","] RS) %s"from" RS
//! as-clause = RS %s"as" RS id
//! filename = 1*("-" / "." / %x30-39 / %x41-5a / "_" / %x61-7a)
//! id = ("$" / %x40-5a / "_" / %x61-7a)
//!      *("$" / %x30-39 / %x40-5a / "_" / %x61-7a)
//! id-or-all = id / "*"
//! RS = 1*WS
//! WS = SP
//! SP = %x20
//! CRLF = %x0A / %x0D.0A
//! ```

#[cfg(not(feature = "std"))]
use alloc::{
  format,
  string::{String, ToString},
  vec::Vec,
};

use super::ModuleError;

/// Which of the two directive groups a directive belongs to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirectiveKind {
  /// `include` — bring in the named rules (or all rules) verbatim.
  Include,
  /// `import` — bring in only the rules that are referenced, transitively.
  Import,
}

/// A single entry in a `from` clause.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameSelector {
  /// The `*` wildcard, selecting every rule of the module.
  All,
  /// An explicitly named rule, as written in the directive (which may carry a
  /// namespace prefix, e.g. `cose.label`).
  Name(String),
}

/// A parsed `;#` directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Directive {
  /// `import` or `include`.
  pub kind: DirectiveKind,
  /// The `from` clause, if present. `None` means no explicit selection was
  /// made, which for `include` means "all rules" and for `import` means "the
  /// rules referenced by the importing module".
  pub names: Option<Vec<NameSelector>>,
  /// The module name (a filename, without any directory component).
  pub filename: String,
  /// The namespace prefix supplied by an `as` clause, if present.
  pub alias: Option<String>,
  /// 1-based line number the directive appeared on.
  pub line: usize,
}

/// Extract and parse every `;#` directive in `input`, in source order.
///
/// A line whose first two characters are `;#` is a directive; if it does not
/// match the grammar above, a [`ModuleError::Directive`] is returned rather
/// than the line being silently treated as an ordinary comment.
pub fn parse_directives(input: &str) -> Result<Vec<Directive>, ModuleError> {
  let mut directives = Vec::new();

  for (index, raw) in input.lines().enumerate() {
    let line = index + 1;
    let text = raw.trim_end_matches('\r');

    if !text.starts_with(";#") {
      continue;
    }

    directives.push(parse_line(&text[2..], line)?);
  }

  Ok(directives)
}

fn err(line: usize, message: &str) -> ModuleError {
  ModuleError::Directive {
    line,
    message: message.to_string(),
  }
}

fn parse_line(rest: &str, line: usize) -> Result<Directive, ModuleError> {
  if !rest.starts_with([' ', '\t']) {
    return Err(err(line, "expected whitespace after \";#\""));
  }

  let tokens: Vec<&str> = rest.split_whitespace().collect();

  let kind = match tokens.first() {
    Some(&"import") => DirectiveKind::Import,
    Some(&"include") => DirectiveKind::Include,
    Some(other) => {
      return Err(ModuleError::Directive {
        line,
        message: format!(
          "unknown directive \"{}\"; expected \"import\" or \"include\"",
          other
        ),
      })
    }
    None => return Err(err(line, "empty directive")),
  };

  let tail = &tokens[1..];

  // The `from` keyword is only a keyword when at least one name precedes it,
  // which keeps `from` usable as an ordinary rule name.
  let from_at = tail
    .iter()
    .position(|t| *t == "from")
    .filter(|position| *position >= 1);

  let (names, remainder) = match from_at {
    Some(position) => {
      let mut selectors = Vec::new();

      for token in &tail[..position] {
        for name in token.split(',') {
          if name.is_empty() {
            continue;
          }

          if name == "*" {
            selectors.push(NameSelector::All);
          } else if is_id(name) {
            selectors.push(NameSelector::Name(name.to_string()));
          } else {
            return Err(ModuleError::Directive {
              line,
              message: format!("\"{}\" is not a valid rule name", name),
            });
          }
        }
      }

      if selectors.is_empty() {
        return Err(err(line, "\"from\" clause names no rules"));
      }

      (Some(selectors), &tail[position + 1..])
    }
    None => (None, tail),
  };

  let filename = match remainder.first() {
    Some(name) if is_filename(name) => (*name).to_string(),
    Some(name) => {
      return Err(ModuleError::Directive {
        line,
        message: format!("\"{}\" is not a valid module name", name),
      })
    }
    None => return Err(err(line, "directive names no module")),
  };

  let alias = match &remainder[1..] {
    [] => None,
    ["as"] => return Err(err(line, "\"as\" clause names no namespace")),
    ["as", alias] if is_id(alias) => Some((*alias).to_string()),
    ["as", alias] => {
      return Err(ModuleError::Directive {
        line,
        message: format!("\"{}\" is not a valid namespace", alias),
      })
    }
    extra => {
      return Err(ModuleError::Directive {
        line,
        message: format!("unexpected trailing text: \"{}\"", extra.join(" ")),
      })
    }
  };

  Ok(Directive {
    kind,
    names,
    filename,
    alias,
    line,
  })
}

/// Whether `name` is a valid `id`.
///
/// Appendix A does not admit `.` or `-` in an `id`, but §2.6 of the same
/// document writes namespaced selectors such as `cose.label`, and RFC 8610
/// names admit both characters internally. Both are accepted here so that the
/// specification's own examples parse.
fn is_id(name: &str) -> bool {
  let mut chars = name.chars();

  match chars.next() {
    Some(c) if c == '$' || c == '@' || c == '_' || c.is_ascii_alphabetic() => {}
    _ => return false,
  }

  chars
    .all(|c| c == '$' || c == '@' || c == '_' || c == '.' || c == '-' || c.is_ascii_alphanumeric())
    && !name.ends_with(['.', '-'])
}

/// Whether `name` matches the `filename` production. Note that the production
/// admits no path separator, so a module name can never escape a configured
/// source directory; `.` and `..` are rejected explicitly all the same.
fn is_filename(name: &str) -> bool {
  !name.is_empty()
    && name != "."
    && name != ".."
    && name
      .chars()
      .all(|c| c == '-' || c == '.' || c == '_' || c.is_ascii_alphanumeric())
}

#[cfg(test)]
mod tests {
  use super::*;

  fn one(input: &str) -> Directive {
    let mut directives = parse_directives(input).unwrap();
    assert_eq!(directives.len(), 1);
    directives.pop().unwrap()
  }

  #[test]
  fn plain_import() {
    let directive = one("start = COSE_Key\n;# import rfc9052\n");
    assert_eq!(directive.kind, DirectiveKind::Import);
    assert_eq!(directive.filename, "rfc9052");
    assert_eq!(directive.names, None);
    assert_eq!(directive.alias, None);
    assert_eq!(directive.line, 2);
  }

  #[test]
  fn import_with_alias() {
    let directive = one(";# import rfc9052 as cose\n");
    assert_eq!(directive.alias.as_deref(), Some("cose"));
  }

  #[test]
  fn include_with_from_clause() {
    let directive = one(";# include label, values from rfc9052\n");
    assert_eq!(directive.kind, DirectiveKind::Include);
    assert_eq!(
      directive.names,
      Some(vec![
        NameSelector::Name("label".to_string()),
        NameSelector::Name("values".to_string()),
      ])
    );
  }

  #[test]
  fn namespaced_from_clause_with_alias() {
    let directive = one(";# include cose.label, cose.values from rfc9052 as cose\n");
    assert_eq!(
      directive.names,
      Some(vec![
        NameSelector::Name("cose.label".to_string()),
        NameSelector::Name("cose.values".to_string()),
      ])
    );
    assert_eq!(directive.alias.as_deref(), Some("cose"));
  }

  #[test]
  fn wildcard_selector() {
    let directive = one(";# include * from rfc9052\n");
    assert_eq!(directive.names, Some(vec![NameSelector::All]));
  }

  #[test]
  fn ordinary_comments_are_not_directives() {
    assert!(parse_directives("; import rfc9052\na = int\n")
      .unwrap()
      .is_empty());
  }

  #[test]
  fn malformed_directives_are_errors_not_comments() {
    for input in [
      ";# imprt rfc9052\n",
      ";#import rfc9052\n",
      ";# import\n",
      ";# import rfc9052 as\n",
      ";# import rfc9052 as cose extra\n",
      ";# import ../etc/passwd\n",
      ";# include from rfc9052\n",
    ] {
      assert!(
        matches!(parse_directives(input), Err(ModuleError::Directive { .. })),
        "expected {:?} to be rejected",
        input
      );
    }
  }
}
