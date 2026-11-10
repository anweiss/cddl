use super::*;
use std::fmt::{self, Write};
use std::sync::{Arc, Mutex};

/// Interned, collision-checked structural fragments, rather than a digest or
/// a byte-limited rendering. Literal payload size does not limit valid inputs.
#[derive(Default)]
pub(super) struct NumericIdentities {
  nodes: HashMap<IdentityNode, usize>,
}

#[derive(Hash, PartialEq, Eq)]
enum IdentityNode {
  Fragment(String),
  Structure(Vec<usize>),
}

impl NumericIdentities {
  fn intern(&mut self, node: IdentityNode) -> usize {
    let next = self.nodes.len();
    *self.nodes.entry(node).or_insert(next)
  }
}

struct IdentityWriter<'s> {
  identities: &'s mut NumericIdentities,
  fragments: Vec<usize>,
}

impl Write for IdentityWriter<'_> {
  fn write_str(&mut self, value: &str) -> fmt::Result {
    if !value.is_empty() {
      self.fragments.push(
        self
          .identities
          .intern(IdentityNode::Fragment(value.to_string())),
      );
    }
    Ok(())
  }
}

#[derive(Clone, Default)]
pub(super) struct NumericContext {
  identities: Arc<Mutex<NumericIdentities>>,
  negative_boundaries: HashSet<String>,
  pub(super) work: Option<Arc<std::sync::atomic::AtomicUsize>>,
}

pub(super) enum NumericRecursion {
  Cut,
  InvalidConstraint,
}

impl NumericContext {
  pub(super) fn enter_negative(&mut self, state: &ValidationState<'_>) {
    self
      .negative_boundaries
      .extend(state.visited_rules.iter().cloned());
  }

  pub(super) fn candidate(&self) -> Self {
    Self {
      identities: self.identities.clone(),
      negative_boundaries: HashSet::new(),
      work: self.work.clone(),
    }
  }

  pub(super) fn recursion(&self, marker: &str) -> NumericRecursion {
    if self.negative_boundaries.contains(marker) {
      NumericRecursion::InvalidConstraint
    } else {
      NumericRecursion::Cut
    }
  }
}

pub(super) fn compact_numeric_key(context: &NumericContext, value: fmt::Arguments) -> String {
  let mut identities = context
    .identities
    .lock()
    .unwrap_or_else(|error| error.into_inner());
  let mut writer = IdentityWriter {
    identities: &mut identities,
    fragments: Vec::new(),
  };
  // Debug preserves AST variants (including integer/float literals), unlike
  // Display. Fragments are interned without constructing the full rendering.
  fmt::write(&mut writer, value).expect("structural identity writing is infallible");
  let fragments = writer.fragments;
  identities
    .intern(IdentityNode::Structure(fragments))
    .to_string()
}

pub(super) const MAX_GENERIC_STRUCTURE_WORK: usize = 4096;
const MAX_GENERIC_STRUCTURE_DEPTH: usize = 128;

struct SubstitutionWork<'s, 'a> {
  bindings: Option<&'s HashMap<&'a str, Type1<'a>>>,
  remaining: usize,
  depth: usize,
}

impl<'s, 'a> SubstitutionWork<'s, 'a> {
  fn new(bindings: &'s HashMap<&'a str, Type1<'a>>) -> Self {
    Self {
      bindings: Some(bindings),
      remaining: MAX_GENERIC_STRUCTURE_WORK,
      depth: 0,
    }
  }

  fn descend(&mut self, visit: impl FnOnce(&mut Self) -> fmt::Result) -> fmt::Result {
    if self.remaining == 0 || self.depth >= MAX_GENERIC_STRUCTURE_DEPTH {
      return Err(fmt::Error);
    }
    self.remaining -= 1;
    self.depth += 1;
    let result = visit(self);
    self.depth -= 1;
    result
  }

  fn bound_argument(&mut self, argument: &Type1<'a>) -> fmt::Result {
    // A replacement is closed in its originating scope. Never substitute
    // identifiers introduced by that replacement a second time.
    let previous = self.bindings.take();
    let result = self.visit_type1(argument);
    self.bindings = previous;
    result
  }
}

impl<'a, 'b> Visitor<'a, 'b, fmt::Error> for SubstitutionWork<'_, 'a> {
  fn visit_type1(&mut self, value: &'b Type1<'a>) -> fmt::Result {
    self.descend(|work| {
      if let Type2::Typename {
        ident,
        generic_args: None,
        ..
      } = &value.type2
      {
        if let Some(argument) = work
          .bindings
          .and_then(|bindings| param_binding(bindings, ident))
        {
          work.bound_argument(argument)?;
          return match &value.operator {
            Some(operator) => work.visit_type2(&operator.type2),
            None => Ok(()),
          };
        }
      }
      crate::visitor::walk_type1(work, value)
    })
  }

  fn visit_type2(&mut self, value: &'b Type2<'a>) -> fmt::Result {
    self.descend(|work| {
      if let Type2::Typename {
        ident,
        generic_args,
        ..
      } = value
      {
        if let Some(arguments) = generic_args {
          work.visit_generic_args(arguments)?;
        } else if let Some(argument) = work
          .bindings
          .and_then(|bindings| param_binding(bindings, ident))
        {
          return work.bound_argument(argument);
        }
      }
      crate::visitor::walk_type2(work, value)
    })
  }

  fn visit_group_entry(&mut self, entry: &'b GroupEntry<'a>) -> fmt::Result {
    self.descend(|work| {
      if let GroupEntry::TypeGroupname { ge, .. } = entry {
        if ge.generic_args.is_none() {
          if let Some(argument) = work
            .bindings
            .and_then(|bindings| param_binding(bindings, &ge.name))
          {
            return work.bound_argument(argument);
          }
        }
      }
      crate::visitor::walk_group_entry(work, entry)
    })
  }
}

pub(super) fn numeric_substitution_fits<'a>(
  value: &Type1<'a>,
  bindings: &HashMap<&'a str, Type1<'a>>,
) -> bool {
  SubstitutionWork::new(bindings).visit_type1(value).is_ok()
}

pub(super) fn numeric_type2_substitution_fits<'a>(
  value: &Type2<'a>,
  bindings: &HashMap<&'a str, Type1<'a>>,
) -> bool {
  SubstitutionWork::new(bindings).visit_type2(value).is_ok()
}

pub(super) fn generic_arguments_fit<'a>(
  args: &GenericArgs<'a>,
  bindings: &HashMap<&'a str, Type1<'a>>,
) -> bool {
  SubstitutionWork::new(bindings)
    .visit_generic_args(args)
    .is_ok()
}

pub(super) fn group_entry_substitution_fits<'a>(
  value: &GroupEntry<'a>,
  bindings: &HashMap<&'a str, Type1<'a>>,
) -> bool {
  SubstitutionWork::new(bindings)
    .visit_group_entry(value)
    .is_ok()
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
  use super::*;

  #[test]
  fn marker_depth_compares_complete_raw_locations() {
    let cddl = crate::cddl_from_str("root = any", true).unwrap();
    let mut state = ValidationState::new(
      &cddl,
      #[cfg(feature = "additional-controls")]
      None,
    );
    state.data_location = "/leaf".to_string();
    for index in 0..MAX_NUMERIC_RESOLUTION_DEPTH {
      state
        .visited_rules
        .insert(format!("#generic\0{index}\0/ancestor\0/leaf"));
    }
    let context = NumericContext::default();
    assert!(numeric_recursion_marker(&state, &context, "#generic", &0).is_some());
    state.visited_rules.clear();
    for index in 0..MAX_NUMERIC_RESOLUTION_DEPTH {
      state
        .visited_rules
        .insert(format!("#generic\0{index}\0/leaf"));
    }
    assert!(numeric_recursion_marker(&state, &context, "#generic", &0).is_none());
  }

  #[cfg(feature = "json")]
  fn json_identity_count(elements: usize) -> usize {
    let cddl = crate::cddl_from_str("root = [* float64 .gt 0.0]", true).unwrap();
    let value = serde_json::Value::Array(vec![serde_json::Value::from(1.5); elements]);
    let mut validator = json::JSONValidator::new(
      &cddl,
      value,
      #[cfg(feature = "additional-controls")]
      None,
    );
    validator.validate().unwrap();
    let count = validator
      .numeric_context
      .identities
      .lock()
      .unwrap()
      .nodes
      .len();
    assert!(validator.state.visited_rules.is_empty());
    count
  }

  #[cfg(feature = "json")]
  #[test]
  fn completed_json_locations_do_not_accumulate_guard_metadata() {
    assert_eq!(json_identity_count(1), json_identity_count(1000));
  }

  #[cfg(feature = "cbor")]
  fn cbor_identity_count(elements: usize) -> usize {
    let cddl = crate::cddl_from_str("root = [* float64 .gt 0.0]", true).unwrap();
    let value = cbor_value::Value::Array(vec![cbor_value::Value::Float(1.5); elements]);
    let mut validator = cbor::CBORValidator::new(
      &cddl,
      value,
      #[cfg(feature = "additional-controls")]
      None,
    );
    let result: Result<(), cbor::Error<std::io::Error>> = validator.validate();
    result.unwrap();
    let count = validator
      .numeric_context
      .identities
      .lock()
      .unwrap()
      .nodes
      .len();
    assert!(validator.state.visited_rules.is_empty());
    count
  }

  #[cfg(feature = "cbor")]
  #[test]
  fn completed_cbor_locations_do_not_accumulate_guard_metadata() {
    assert_eq!(cbor_identity_count(1), cbor_identity_count(1000));
  }
}
