#![cfg(feature = "std")]
#![cfg(feature = "json")]
#![cfg(not(feature = "lsp"))]

use super::*;
use crate::{
  ast::*,
  token,
  visitor::{self, *},
};

use std::{
  borrow::Cow,
  collections::{HashMap, HashSet},
  convert::TryFrom,
  fmt::{self, Write},
  sync::{
    atomic::{AtomicUsize, Ordering},
    Arc, Mutex,
  },
};

use chrono::{TimeZone, Utc};
use serde_json::Value;

/// Finds an augmenting path in the claim-to-pair compatibility graph; the
/// DFS form of Kuhn's maximum bipartite matching.
fn augment_choice_assignment(
  claim_slot: usize,
  compatibility: &[Arc<Vec<bool>>],
  visited_pairs: &mut [bool],
  pair_owners: &mut [Option<usize>],
  interchangeable: bool,
  classes: &[usize],
  positions: &mut [usize],
) -> bool {
  if interchangeable {
    for (pair_slot, compatible) in compatibility[claim_slot].iter().enumerate() {
      if *compatible && !visited_pairs[pair_slot] && pair_owners[pair_slot].is_none() {
        visited_pairs[pair_slot] = true;
        pair_owners[pair_slot] = Some(claim_slot);
        return true;
      }
    }
  }
  let mut claim = claim_slot;
  let mut path = Vec::new();
  loop {
    let class = classes[claim];
    let mut next = None;
    // Earlier slots in this class are incompatible or permanently visited
    // during this augmentation. Sharing the cursor preserves DFS order.
    while positions[class] < compatibility[claim].len() {
      let pair = positions[class];
      positions[class] += 1;
      if compatibility[claim][pair] && !visited_pairs[pair] {
        next = Some(pair);
        break;
      }
    }
    let Some(pair_slot) = next else {
      let Some((parent, _)) = path.pop() else {
        return false;
      };
      claim = parent;
      continue;
    };
    visited_pairs[pair_slot] = true;
    path.push((claim, pair_slot));
    if let Some(owner) = pair_owners[pair_slot] {
      claim = owner;
      continue;
    }
    for (claim, pair) in path {
      pair_owners[pair] = Some(claim);
    }
    return true;
  }
}

fn is_json_unsigned(cddl: &CDDL, ident: &Identifier) -> bool {
  is_ident_uint_data_type(cddl, ident) || matches!(lookup_ident(ident.ident), Token::UNSIGNED)
}

#[cfg(feature = "additional-controls")]
use control::{abnf_from_complex_controller, cat_operation, plus_operation, validate_abnf};

/// JSON validation Result
pub type Result = std::result::Result<(), Error>;

/// JSON validation error
#[derive(Debug)]
pub enum Error {
  /// Zero or more validation errors
  Validation(Vec<ValidationError>),
  /// JSON parsing error
  JSONParsing(serde_json::Error),
  /// CDDL parsing error
  CDDLParsing(String),
  /// UTF8 parsing error,
  UTF8Parsing(std::str::Utf8Error),
  /// Disabled feature
  DisabledFeature(String),
  /// Validation stopped before completion because map replay exhausted its budget.
  ResourceLimit {
    /// Maximum additional map-candidate replays for this validation.
    limit: usize,
  },
  /// Validation stopped before completion because active map rule calls were too deep.
  RecursionLimit {
    /// Maximum simultaneously active named-rule calls during map validation.
    limit: usize,
  },
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Error::Validation(errors) => {
        let mut error_str = String::new();
        for e in errors.iter() {
          let _ = writeln!(error_str, "{}", e);
        }
        write!(f, "{}", error_str)
      }
      Error::JSONParsing(error) => write!(f, "error parsing JSON: {}", error),
      Error::CDDLParsing(error) => write!(f, "error parsing CDDL: {}", error),
      Error::UTF8Parsing(error) => write!(f, "error pasing utf8: {}", error),
      Error::DisabledFeature(feature) => write!(f, "feature {} is not enabled", feature),
      Error::ResourceLimit { limit } => write!(
        f,
        "JSON validation incomplete: the bounded map-choice search exhausted its budget of {limit} additional map-candidate replays"
      ),
      Error::RecursionLimit { limit } => write!(
        f,
        "JSON validation incomplete: map rule recursion exceeded its depth limit of {limit} active calls"
      ),
    }
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Error::JSONParsing(error) => Some(error),
      _ => None,
    }
  }
}

impl Error {
  fn from_validator(jv: &JSONValidator, reason: String) -> Self {
    Error::Validation(vec![ValidationError {
      cddl_location: jv.state.cddl_location.clone(),
      json_location: jv.state.data_location.clone(),
      reason,
      is_multi_type_choice: jv.state.is_multi_type_choice,
      is_group_to_choice_enum: jv.state.is_group_to_choice_enum,
      type_group_name_entry: jv.state.type_group_name_entry.map(|e| e.to_string()),
      is_multi_group_choice: jv.state.is_multi_group_choice,
    }])
  }
}

/// JSON validation error
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValidationError {
  /// Error message
  pub reason: String,
  /// Location in CDDL where error occurred
  pub cddl_location: String,
  /// Location in JSON (in JSONPointer notation) where error occurred
  pub json_location: String,
  /// Whether or not the error is associated with multiple type choices
  pub is_multi_type_choice: bool,
  /// Whether or not the error is associated with multiple group choices
  pub is_multi_group_choice: bool,
  /// Whether or not the error is associated with a group to choice enumeration
  pub is_group_to_choice_enum: bool,
  /// Error is associated with a type/group name group entry
  pub type_group_name_entry: Option<String>,
}

impl fmt::Display for ValidationError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut error_str = String::from("error validating");
    if self.is_multi_group_choice {
      error_str.push_str(" group choice");
    }
    if self.is_multi_type_choice {
      error_str.push_str(" type choice");
    }
    if self.is_group_to_choice_enum {
      error_str.push_str(" type choice in group to choice enumeration");
    }
    if let Some(entry) = &self.type_group_name_entry {
      let _ = write!(error_str, " group entry associated with rule \"{}\"", entry);
    }

    if self.json_location.is_empty() {
      return write!(
        f,
        "{} at the root of the JSON document: {}",
        error_str, self.reason
      );
    }

    write!(
      f,
      "{} at JSON location {}: {}",
      error_str, self.json_location, self.reason
    )
  }
}

impl std::error::Error for ValidationError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    None
  }
}

/// The generic evaluation context active when a claim was recorded, so a
/// reassignment can revalidate the pair under the same bindings.
#[derive(Clone)]
struct ClaimEvalContext<'a> {
  rule_name: &'a str,
  generic_rules: Vec<GenericRule<'a>>,
}

impl PartialEq for ClaimEvalContext<'_> {
  fn eq(&self, other: &Self) -> bool {
    self.rule_name == other.rule_name
      && self.generic_rules.len() == other.generic_rules.len()
      && self
        .generic_rules
        .iter()
        .zip(&other.generic_rules)
        .all(|(left, right)| {
          left.name == right.name && left.params == right.params && left.args == right.args
        })
  }
}

/// A complete object pair claimed by a single-occurrence member: choice-key
/// members and plain key-domain members alike, so reassignment can move any
/// of them when a later member finds no pair of its own.
#[derive(Clone)]
struct MemberClaim<'a> {
  key: String,
  entry: ValueMemberKeyEntry<'a>,
  context: Option<ClaimEvalContext<'a>>,
}

type ClaimCompatibility = Vec<Arc<Vec<bool>>>;

#[derive(Clone, Copy, PartialEq, Eq)]
enum ProvenScalarType {
  Uint,
  Other,
}

enum KeyProofStep<'b, 'a> {
  Entry(&'b GroupEntry<'a>),
  Exit(&'a str),
}

struct LiteralKeyProof<'b, 'a> {
  keys: HashSet<String>,
  pending: Vec<KeyProofStep<'b, 'a>>,
  active: HashSet<&'a str>,
  complete: HashMap<&'a str, usize>,
  depths: Vec<usize>,
  inspect_values: bool,
}

impl<'b, 'a> LiteralKeyProof<'b, 'a> {
  fn new(inspect_values: bool) -> Self {
    Self {
      keys: HashSet::new(),
      pending: Vec::new(),
      active: HashSet::new(),
      complete: HashMap::new(),
      depths: Vec::new(),
      inspect_values,
    }
  }

  fn push_group(&mut self, group: &'b Group<'a>) {
    self
      .pending
      .extend(group.group_choices.iter().flat_map(|choice| {
        choice
          .group_entries
          .iter()
          .map(|(entry, _)| KeyProofStep::Entry(entry))
      }));
  }

  fn inherit_depth(&mut self, depth: usize) {
    if let Some(parent) = self.depths.last_mut() {
      *parent = (*parent).max(depth + 1);
    }
  }

  fn finish_named(&mut self, name: &'a str) {
    let depth = self.depths.pop().expect("named proof path");
    self.active.remove(name);
    self.complete.insert(name, depth);
    self.inherit_depth(depth);
  }
}

fn compatibility_classes(rows: &[Arc<Vec<bool>>]) -> (Vec<usize>, usize) {
  let mut classes = HashMap::new();
  let indices = rows
    .iter()
    .map(|row| {
      let next = classes.len();
      *classes.entry(Arc::as_ptr(row)).or_insert(next)
    })
    .collect();
  (indices, classes.len())
}

fn claim_compatibility<'a, F>(
  claims: &[MemberClaim<'a>],
  pair_count: usize,
  mut probe: F,
) -> std::result::Result<Option<ClaimCompatibility>, Error>
where
  F: FnMut(&MemberClaim<'a>, usize) -> std::result::Result<bool, Error>,
{
  let mut classes: Vec<(usize, Arc<Vec<bool>>, usize, usize)> = Vec::new();
  let mut rows = Vec::with_capacity(claims.len());
  for (index, claim) in claims.iter().enumerate() {
    let existing = classes.iter_mut().find(|(representative, _, _, _)| {
      let prior = &claims[*representative];
      prior.entry == claim.entry && prior.context == claim.context
    });
    if let Some((_, row, demand, capacity)) = existing {
      *demand += 1;
      if *demand > *capacity {
        return Ok(None);
      }
      rows.push(row.clone());
    } else {
      let row = Arc::new(
        (0..pair_count)
          .map(|pair| probe(claim, pair))
          .collect::<std::result::Result<Vec<_>, _>>()?,
      );
      let capacity = row.iter().filter(|matches| **matches).count();
      if capacity == 0 {
        return Ok(None);
      }
      rows.push(row.clone());
      classes.push((index, row, 1, capacity));
    }
  }
  Ok(Some(rows))
}

/// Does this member key carry an explicit cut (`^ =>`)?
fn member_key_is_cut(entry: &ValueMemberKeyEntry) -> bool {
  matches!(
    &entry.member_key,
    Some(MemberKey::Type1 { is_cut: true, .. })
  )
}

fn group_entry_bounds(entry: &GroupEntry<'_>) -> (usize, Option<usize>) {
  let occur = match entry {
    GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref(),
    GroupEntry::TypeGroupname { ge, .. } => ge.occur.as_ref(),
    GroupEntry::InlineGroup { occur, .. } => occur.as_ref(),
  };
  occurrence_bounds(occur.map(|occur| occur.occur))
}

fn bounded_zero_minimum_colon(entry: &GroupEntry<'_>) -> bool {
  let GroupEntry::ValueMemberKey { ge, .. } = entry else {
    return false;
  };
  matches!(
    &ge.member_key,
    Some(MemberKey::Bareword { .. } | MemberKey::Value { .. })
  ) && group_entry_bounds(entry).0 == 0
    && !matches!(
      ge.occur.as_ref().map(|occur| occur.occur),
      Some(Occur::Optional { .. })
    )
}

#[derive(Clone)]
struct MapCheckpoint<'a> {
  state: super::ValidationState<'a>,
  errors: Vec<ValidationError>,
  object_value: Option<Value>,
  cut_value: Option<Cow<'a, str>>,
  validated_keys: Option<Vec<String>>,
  member_claims: Vec<MemberClaim<'a>>,
  deferred_map_errors: Vec<(String, ValidationError)>,
  object_value_key: Option<String>,
  map_entry_candidates: Option<Vec<(String, Value)>>,
  map_traversal_pruned: bool,
  map_decisions_suppressed: bool,
}

/// Replay group alternatives lazily, including choices inside named groups.
/// A path is accepted only after the enclosing map has complete coverage.
#[derive(Default)]
struct MapChoiceSearch {
  path: Vec<usize>,
  widths: Vec<usize>,
  traversal_pruned: bool,
  decisions_suppressed: bool,
  // Like widths, decision provenance survives optional rollback.
  has_unsuppressed_decision: bool,
}

const MAX_ADDITIONAL_MAP_REPLAYS: usize = 4096;
const MAX_ACTIVE_MAP_RULE_CALLS: usize = 64;

struct MapRuleDepth {
  limit: usize,
  active: AtomicUsize,
}

struct MapRuleGuard(Arc<MapRuleDepth>);

impl Drop for MapRuleGuard {
  fn drop(&mut self) {
    self.0.active.fetch_sub(1, Ordering::Relaxed);
  }
}

impl MapRuleDepth {
  fn new(limit: usize) -> Self {
    Self {
      limit,
      active: AtomicUsize::new(0),
    }
  }

  fn enter(self: &Arc<Self>) -> std::result::Result<MapRuleGuard, Error> {
    self
      .active
      .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |active| {
        active.checked_add(1).filter(|next| *next <= self.limit)
      })
      .map_err(|_| Error::RecursionLimit { limit: self.limit })?;
    Ok(MapRuleGuard(self.clone()))
  }
}

struct MapReplayBudget {
  limit: usize,
  remaining: AtomicUsize,
  #[cfg(test)]
  generic_input_copies: AtomicUsize,
}

impl MapReplayBudget {
  fn new(limit: usize) -> Self {
    Self {
      limit,
      remaining: AtomicUsize::new(limit),
      #[cfg(test)]
      generic_input_copies: AtomicUsize::new(0),
    }
  }

  fn charge_replay(&self) -> Result {
    self
      .remaining
      .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |remaining| {
        remaining.checked_sub(1)
      })
      .map(|_| ())
      .map_err(|_| Error::ResourceLimit { limit: self.limit })
  }
}

impl MapChoiceSearch {
  fn select(&mut self, width: usize) -> usize {
    let depth = self.widths.len();
    self.has_unsuppressed_decision |= !self.decisions_suppressed;
    // Keep the first traversal's pruning context without alternatives that
    // cannot repair an earlier required failure.
    self
      .widths
      .push(if self.decisions_suppressed { 1 } else { width });
    self.path.get(depth).copied().unwrap_or(0)
  }

  fn advance(&mut self) -> bool {
    self.traversal_pruned = false;
    self.decisions_suppressed = false;
    self.has_unsuppressed_decision = false;
    self.path.resize(self.widths.len(), 0);
    for depth in (0..self.widths.len()).rev() {
      if self.path[depth] + 1 < self.widths[depth] {
        self.path[depth] += 1;
        // A changed decision invalidates every dependent choice; unchanged
        // prefixes replay the same traversal and retain their original widths.
        self.path.truncate(depth + 1);
        self.widths.clear();
        return true;
      }
    }
    false
  }
}

/// JSON validator type
#[derive(Clone)]
pub struct JSONValidator<'a> {
  /// Shared validation state (common CDDL tracking fields).
  /// Access shared fields directly via `Deref` (e.g., `self.state.cddl`,
  /// `self.state.is_root`, `self.state.data_location`).
  pub state: super::ValidationState<'a>,
  json: Arc<Value>,
  errors: Vec<ValidationError>,
  // JSON object value hoisted from previous state of AST evaluation
  object_value: Option<Value>,
  // Str value of cut detected in current state of AST evaluation
  cut_value: Option<Cow<'a, str>>,
  // Collect map entry keys that have already been validated
  validated_keys: Option<Vec<String>>,
  // Complete pairs claimed by single-occurrence members, retried as a
  // one-to-one assignment when a later member finds no pair
  member_claims: Vec<MemberClaim<'a>>,
  // Value mismatches from optional members remain inactive unless their
  // exact object pair is still unclaimed at the final coverage check.
  deferred_map_errors: Vec<(String, ValidationError)>,
  // Key of the pair relayed through object_value, retained so the member
  // can record a movable claim once the value validates
  object_value_key: Option<String>,
  // Candidate pairs produced while visiting one repeating map member's key.
  // The group entry takes this relay immediately and commits only candidates
  // whose values also match the member.
  map_entry_candidates: Option<Vec<(String, Value)>>,
  // Generic group validators share the current map's search. Arc/Mutex
  // preserves the native validator's Send/Sync traits.
  map_choice_search: Option<Arc<Mutex<MapChoiceSearch>>>,
  // Shared by child validators, deliberately excluded from speculative checkpoints.
  map_replay_budget: Arc<MapReplayBudget>,
  map_rule_depth: Arc<MapRuleDepth>,
}

impl<'a> JSONValidator<'a> {
  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(feature = "additional-controls")]
  /// New JSONValidation from CDDL AST and JSON value
  pub fn new(cddl: &'a CDDL<'a>, json: Value, enabled_features: Option<&'a [&'a str]>) -> Self {
    JSONValidator {
      state: super::ValidationState::new(cddl, enabled_features),
      json: Arc::new(json),
      errors: Vec::default(),
      object_value: None,
      cut_value: None,
      validated_keys: None,
      member_claims: Vec::new(),
      deferred_map_errors: Vec::new(),
      object_value_key: None,
      map_entry_candidates: None,
      map_choice_search: None,
      map_replay_budget: Arc::new(MapReplayBudget::new(MAX_ADDITIONAL_MAP_REPLAYS)),
      map_rule_depth: Arc::new(MapRuleDepth::new(MAX_ACTIVE_MAP_RULE_CALLS)),
    }
  }

  #[cfg(not(target_arch = "wasm32"))]
  #[cfg(not(feature = "additional-controls"))]
  /// New JSONValidation from CDDL AST and JSON value
  pub fn new(cddl: &'a CDDL<'a>, json: Value) -> Self {
    JSONValidator {
      state: super::ValidationState::new(cddl),
      json: Arc::new(json),
      errors: Vec::default(),
      object_value: None,
      cut_value: None,
      validated_keys: None,
      member_claims: Vec::new(),
      deferred_map_errors: Vec::new(),
      object_value_key: None,
      map_entry_candidates: None,
      map_choice_search: None,
      map_replay_budget: Arc::new(MapReplayBudget::new(MAX_ADDITIONAL_MAP_REPLAYS)),
      map_rule_depth: Arc::new(MapRuleDepth::new(MAX_ACTIVE_MAP_RULE_CALLS)),
    }
  }

  fn new_child_validator(&self, json: impl Into<Arc<Value>>) -> JSONValidator<'a> {
    #[cfg(all(feature = "additional-controls", target_arch = "wasm32"))]
    let mut jv = JSONValidator::new(
      self.state.cddl,
      Value::Null,
      self.state.enabled_features.clone(),
    );
    #[cfg(all(feature = "additional-controls", not(target_arch = "wasm32")))]
    let mut jv = JSONValidator::new(self.state.cddl, Value::Null, self.state.enabled_features);
    #[cfg(not(feature = "additional-controls"))]
    let mut jv = JSONValidator::new(self.state.cddl, Value::Null);

    jv.json = json.into();
    jv.map_replay_budget = self.map_replay_budget.clone();
    jv.map_rule_depth = self.map_rule_depth.clone();
    jv
  }

  fn reset_for_validation(&mut self) {
    let limit = self.map_replay_budget.limit;
    let json = std::mem::take(&mut self.json);
    let mut fresh = self.new_child_validator(json);
    fresh.map_replay_budget = Arc::new(MapReplayBudget::new(limit));
    fresh.map_rule_depth = Arc::new(MapRuleDepth::new(self.map_rule_depth.limit));
    *self = fresh;
  }

  fn new_with_recursion_state(&self, json: impl Into<Arc<Value>>) -> JSONValidator<'a> {
    let mut jv = self.new_child_validator(json);
    jv.state.generic_rules = self.state.generic_rules.clone();
    jv.state.eval_generic_rule = self.state.eval_generic_rule;
    jv.state.visited_rules = self.state.visited_rules.clone();
    jv
  }

  fn enter_map_rule(&self) -> std::result::Result<Option<MapRuleGuard>, Error> {
    self
      .map_choice_search
      .as_ref()
      .map(|_| self.map_rule_depth.enter())
      .transpose()
  }

  fn map_rule_depth_allows(&self, additional: usize) -> bool {
    self.map_choice_search.is_none()
      || self
        .map_rule_depth
        .active
        .load(Ordering::Relaxed)
        .saturating_add(additional)
        <= self.map_rule_depth.limit
  }

  fn visit_named_rule_path<F>(&mut self, marker: String, visit: F) -> Result
  where
    F: FnOnce(&mut Self) -> Result,
  {
    let _depth = self.enter_map_rule()?;
    self.state.visited_rules.insert(marker.clone());
    let result = visit(self);
    self.state.visited_rules.remove(&marker);
    result
  }

  fn repeating_member_upper_bound(entry: &ValueMemberKeyEntry<'a>) -> Option<usize> {
    entry
      .occur
      .as_ref()
      .and_then(|occurrence| occurrence_bounds(Some(occurrence.occur)).1)
  }

  fn validate_repeating_member_count(&mut self, entry: &ValueMemberKeyEntry<'a>, count: usize) {
    let Some(occurrence) = &entry.occur else {
      return;
    };
    let member_key = entry
      .member_key
      .as_ref()
      .map(ToString::to_string)
      .unwrap_or_else(|| "member".to_string());

    match occurrence.occur {
      Occur::OneOrMore { .. } if count == 0 => self.add_error(format!(
        "object must contain at least one entry matching {}",
        member_key
      )),
      Occur::Exact {
        lower: Some(lower),
        upper,
        ..
      } if count < lower => match upper {
        Some(upper) if lower == upper => self.add_error(format!(
          "object must contain exactly {} entries matching {}",
          lower, member_key
        )),
        Some(upper) => self.add_error(format!(
          "object must contain between {} and {} entries matching {}",
          lower, upper, member_key
        )),
        None => self.add_error(format!(
          "object must contain at least {} entries matching {}",
          lower, member_key
        )),
      },
      _ => {}
    }
  }

  #[cfg(target_arch = "wasm32")]
  #[cfg(feature = "additional-controls")]
  /// New JSONValidation from CDDL AST and JSON value
  pub fn new(cddl: &'a CDDL<'a>, json: Value, enabled_features: Option<Box<[JsValue]>>) -> Self {
    JSONValidator {
      state: super::ValidationState::new(cddl, enabled_features),
      json: Arc::new(json),
      errors: Vec::default(),
      object_value: None,
      cut_value: None,
      validated_keys: None,
      member_claims: Vec::new(),
      deferred_map_errors: Vec::new(),
      object_value_key: None,
      map_entry_candidates: None,
      map_choice_search: None,
      map_replay_budget: Arc::new(MapReplayBudget::new(MAX_ADDITIONAL_MAP_REPLAYS)),
      map_rule_depth: Arc::new(MapRuleDepth::new(MAX_ACTIVE_MAP_RULE_CALLS)),
    }
  }

  #[cfg(target_arch = "wasm32")]
  #[cfg(not(feature = "additional-controls"))]
  /// New JSONValidation from CDDL AST and JSON value
  pub fn new(cddl: &'a CDDL<'a>, json: Value) -> Self {
    JSONValidator {
      state: super::ValidationState::new(cddl),
      json: Arc::new(json),
      errors: Vec::default(),
      object_value: None,
      cut_value: None,
      validated_keys: None,
      member_claims: Vec::new(),
      deferred_map_errors: Vec::new(),
      object_value_key: None,
      map_entry_candidates: None,
      map_choice_search: None,
      map_replay_budget: Arc::new(MapReplayBudget::new(MAX_ADDITIONAL_MAP_REPLAYS)),
      map_rule_depth: Arc::new(MapRuleDepth::new(MAX_ACTIVE_MAP_RULE_CALLS)),
    }
  }

  fn validate_array_items(&mut self, token: &ArrayItemToken) -> visitor::Result<Error> {
    // Arrays are matched against array-shaped types by the sequence matcher
    // (seq_match_group); reaching this point means an array value is being
    // validated against a non-array type
    if let Value::Array(_) = self.json.as_ref() {
      // Member keys are annotation only in an array context
      if self.state.is_member_key {
        return Ok(());
      }

      self.add_error(format!("{}, got {}", token.error_msg(None), self.json));
    }

    Ok(())
  }

  /// Match a member whose key type is a choice against complete object
  /// members. Different keys may satisfy different alternatives, and a member
  /// is claimed only when its value also validates, leaving others to later
  /// group entries. Occurrence bounds count members across all alternatives.
  /// Without its own indicator, a member repeats with its enclosing group
  /// reference.
  fn visit_choice_key_entry(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
    key_type: &Type1<'a>,
    alternatives: &[Type1<'a>],
    is_cut: bool,
  ) -> visitor::Result<Error> {
    let json = self.json.clone();
    let Value::Object(entries) = json.as_ref() else {
      return Ok(());
    };
    let (occurrence, min, max) = choice_member_occurrence(entry, self.state.occurrence);
    let single = max == Some(1);
    let mut count = 0;
    let mut cut_committed = false;
    let mut value_errors = Vec::new();
    for (key, value) in entries.iter() {
      if max.is_some_and(|max| count >= max) {
        break;
      }
      let claimed = self
        .validated_keys
        .as_ref()
        .is_some_and(|keys| keys.contains(key));
      if claimed || !self.object_key_matches_alternatives(key, alternatives)? {
        continue;
      }
      let errors = self.object_value_errors(&entry.entry_type, key, value)?;
      if errors.is_empty() || is_cut {
        // A cut commits a matching key to this member, so its failing value
        // rejects the object rather than falling through (RFC 8610 §3.5.4).
        count += usize::from(errors.is_empty());
        cut_committed |= is_cut;
        self
          .validated_keys
          .get_or_insert_with(Vec::new)
          .push(key.clone());
        if single && errors.is_empty() {
          self.member_claims.push(MemberClaim {
            key: key.clone(),
            entry: entry.clone(),
            context: self.current_claim_context(),
          });
        }
        self.errors.extend(errors);
      } else {
        value_errors = errors;
      }
    }

    // Before a cut member matches a key it has no commitment to keep, so the
    // retry may still hand it a pair. An optional member that matched
    // nothing retries only for a key another member holds.
    if count == 0
      && single
      && !cut_committed
      && self.retry_choice_key_entry(entry, entries, alternatives, min > 0)?
    {
      count += 1;
    }
    if count < min {
      self.report_missing_choice_key(key_type, value_errors);
    }
    if occurrence.is_some() {
      self.state.occurrence = None;
    }
    Ok(())
  }

  /// Retry a single-occurrence member that matched no pair through the
  /// one-to-one reassignment. An optional member retries only for a key
  /// another member holds.
  fn retry_choice_key_entry(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
    entries: &serde_json::Map<String, Value>,
    alternatives: &[Type1<'a>],
    required: bool,
  ) -> std::result::Result<bool, Error> {
    if !required && !self.choice_key_held(entries, alternatives)? {
      return Ok(false);
    }
    self.reassign_choice_claims(entry)
  }

  fn report_missing_choice_key(
    &mut self,
    key_type: &Type1<'a>,
    mut value_errors: Vec<ValidationError>,
  ) {
    if value_errors.is_empty() {
      self.add_error(format!("object requires entry key of type {}", key_type));
    } else {
      self.errors.append(&mut value_errors);
    }
  }

  /// Whether another member holds a key this member's alternatives match.
  fn choice_key_held(
    &self,
    entries: &serde_json::Map<String, Value>,
    alternatives: &[Type1<'a>],
  ) -> std::result::Result<bool, Error> {
    let Some(validated) = &self.validated_keys else {
      return Ok(false);
    };
    for key in entries.keys() {
      if validated.contains(key) && self.object_key_matches_alternatives(key, alternatives)? {
        return Ok(true);
      }
    }
    Ok(false)
  }

  /// The generic evaluation context to capture with a new claim.
  fn current_claim_context(&self) -> Option<ClaimEvalContext<'a>> {
    let rule_name = self.state.eval_generic_rule?;
    self
      .state
      .generic_rules
      .iter()
      .any(|rule| rule.name == rule_name)
      .then(|| ClaimEvalContext {
        rule_name,
        generic_rules: self.state.generic_rules.clone(),
      })
  }

  /// A probe validator for one pair, evaluated under the claim's captured
  /// generic bindings when it has any.
  fn probe_with_context(
    &self,
    value: Value,
    context: Option<&ClaimEvalContext<'a>>,
  ) -> JSONValidator<'a> {
    let mut probe = self.new_with_recursion_state(value);
    match context {
      Some(context) => {
        probe.state.generic_rules = context.generic_rules.clone();
        probe.state.eval_generic_rule = Some(context.rule_name);
      }
      // A claim captured outside any generic evaluation must not see the
      // bindings active where it is rechecked.
      None => {
        probe.state.generic_rules.clear();
        probe.state.eval_generic_rule = None;
      }
    }
    probe
  }

  /// A probe for one object key, at a location of its own: the recursion
  /// guard would otherwise mistake the key for the object it belongs to.
  fn key_probe(&self, key: &str, context: Option<&ClaimEvalContext<'a>>) -> JSONValidator<'a> {
    let mut probe = self.probe_with_context(Value::String(key.to_string()), context);
    probe.state.data_location = key_probe_location(&self.state.data_location, &key);
    probe
  }

  /// Whether a pair key satisfies a claimed member that has no choice key:
  /// its key type, bareword or literal.
  fn member_key_accepts(
    &self,
    claim: &MemberClaim<'a>,
    key: &str,
  ) -> std::result::Result<bool, Error> {
    match &claim.entry.member_key {
      Some(MemberKey::Type1 { t1, .. }) => {
        let mut probe = self.key_probe(key, claim.context.as_ref());
        probe.visit_type1(t1)?;
        Ok(probe.errors.is_empty())
      }
      Some(MemberKey::Bareword { ident, .. }) => Ok(key == ident.ident),
      Some(MemberKey::Value { value, .. }) => {
        Ok(matches!(value, token::Value::TEXT(t) if t.as_ref() == key))
      }
      _ => Ok(false),
    }
  }

  /// Whether a complete pair satisfies a claimed member: its key matches one
  /// alternative (or the plain member key) and its value validates against
  /// the member's type, under the claim's captured generic bindings.
  fn pair_satisfies_member(
    &self,
    claim: &MemberClaim<'a>,
    key: &str,
    value: &Value,
  ) -> std::result::Result<bool, Error> {
    let mut state = self.state.clone();
    match &claim.context {
      Some(context) => {
        state.generic_rules = context.generic_rules.clone();
        state.eval_generic_rule = Some(context.rule_name);
      }
      None => {
        state.generic_rules.clear();
        state.eval_generic_rule = None;
      }
    }
    let state = &state;
    let key_matches = match choice_member_key(state, &claim.entry) {
      Some((_, alternatives, _)) => {
        let mut matched = false;
        for alternative in &alternatives {
          let mut probe = self.key_probe(key, claim.context.as_ref());
          probe.visit_type1(alternative)?;
          if probe.errors.is_empty() {
            matched = true;
            break;
          }
        }
        matched
      }
      None => self.member_key_accepts(claim, key)?,
    };
    if !key_matches {
      return Ok(false);
    }
    let mut probe = self.probe_with_context(value.clone(), claim.context.as_ref());
    // The value's own location keeps recursion guards from mistaking a
    // nested value for zero-progress recursion at the object's location.
    probe.state.data_location = format!("{}/{}", self.state.data_location, key);
    probe.visit_type(&claim.entry.entry_type)?;
    Ok(probe.errors.is_empty())
  }

  /// key-domain type, committing only those whose values also match.
  fn validate_candidate_pairs(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
    candidates: Vec<(String, Value)>,
    current_location: String,
    single_member_claim: Option<&ValueMemberKeyEntry<'a>>,
    key_alternatives: Option<&[Type1<'a>]>,
  ) -> visitor::Result<Error> {
    let max_matches = Self::repeating_member_upper_bound(entry).unwrap_or(usize::MAX);
    let mut match_count = 0;

    for (key, value) in candidates {
      if match_count >= max_matches {
        break;
      }
      if let Some(alternatives) = key_alternatives {
        if !self.object_key_matches_alternatives(&key, alternatives)? {
          continue;
        }
      }

      let mut jv = self.new_with_recursion_state(value);
      jv.state.is_multi_type_choice = self.state.is_multi_type_choice;
      jv.state.is_multi_group_choice = self.state.is_multi_group_choice;
      let _ = write!(
        jv.state.data_location,
        "{}/{}",
        self.state.data_location, key
      );
      jv.state.type_group_name_entry = self.state.type_group_name_entry;
      jv.visit_type(&entry.entry_type)?;

      if jv.errors.is_empty() {
        if let Some(member) = single_member_claim {
          self.member_claims.push(MemberClaim {
            key: key.clone(),
            entry: member.clone(),
            context: self.current_claim_context(),
          });
        }
        self.validated_keys.get_or_insert_with(Vec::new).push(key);
        match_count += 1;
      } else {
        self.defer_optional_value_errors(entry, Some(&key), &jv.errors);
      }
    }

    self.validate_repeating_member_count(entry, match_count);
    self.state.data_location = current_location;
    self.state.occurrence = None;
    Ok(())
  }

  fn map_checkpoint(&self) -> MapCheckpoint<'a> {
    MapCheckpoint {
      state: self.state.clone(),
      errors: self.errors.clone(),
      object_value: self.object_value.clone(),
      cut_value: self.cut_value.clone(),
      validated_keys: self.validated_keys.clone(),
      member_claims: self.member_claims.clone(),
      deferred_map_errors: self.deferred_map_errors.clone(),
      object_value_key: self.object_value_key.clone(),
      map_entry_candidates: self.map_entry_candidates.clone(),
      map_traversal_pruned: self.map_traversal_pruned(),
      map_decisions_suppressed: self.map_choice_search.as_ref().is_some_and(|search| {
        search
          .lock()
          .expect("map choice search lock")
          .decisions_suppressed
      }),
    }
  }

  fn restore_map_checkpoint(&mut self, checkpoint: MapCheckpoint<'a>) {
    self.state = checkpoint.state;
    self.errors = checkpoint.errors;
    self.object_value = checkpoint.object_value;
    self.cut_value = checkpoint.cut_value;
    self.validated_keys = checkpoint.validated_keys;
    self.member_claims = checkpoint.member_claims;
    self.deferred_map_errors = checkpoint.deferred_map_errors;
    self.object_value_key = checkpoint.object_value_key;
    self.map_entry_candidates = checkpoint.map_entry_candidates;
    if let Some(search) = &self.map_choice_search {
      let mut search = search.lock().expect("map choice search lock");
      search.traversal_pruned = checkpoint.map_traversal_pruned;
      search.decisions_suppressed = checkpoint.map_decisions_suppressed;
    }
  }

  fn map_traversal_pruned(&self) -> bool {
    self.map_choice_search.as_ref().is_some_and(|search| {
      search
        .lock()
        .expect("map choice search lock")
        .traversal_pruned
    })
  }

  fn validate_map_coverage(&mut self, keys: &[String]) {
    let claimed: HashSet<_> = self.validated_keys.iter().flatten().collect();
    let unclaimed: Vec<_> = keys.iter().filter(|key| !claimed.contains(key)).collect();
    let mut rejected: HashMap<_, Vec<_>> = HashMap::new();
    for (key, error) in &self.deferred_map_errors {
      rejected.entry(key.clone()).or_default().push(error.clone());
    }
    for key in unclaimed {
      if let Some(errors) = rejected.remove(key) {
        self.errors.extend(errors);
      } else {
        self.add_error(format!("unexpected key {:?}", key));
      }
    }
  }

  fn defer_optional_value_errors(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
    key: Option<&str>,
    errors: &[ValidationError],
  ) {
    let key = key.or_else(|| match &entry.member_key {
      Some(MemberKey::Type1 { t1, .. }) => match &t1.type2 {
        Type2::TextValue { value, .. } => Some(value.as_ref()),
        _ => None,
      },
      _ => None,
    });
    if !member_key_is_cut(entry)
      && occurrence_bounds(entry.occur.as_ref().map(|occur| occur.occur)).0 == 0
    {
      if let Some(key) = key {
        self
          .deferred_map_errors
          .extend(errors.iter().cloned().map(|error| (key.to_owned(), error)));
      }
    }
  }

  fn rollback_optional_map_checkpoint(&mut self, checkpoint: MapCheckpoint<'a>) {
    let rejected = self
      .deferred_map_errors
      .split_off(checkpoint.deferred_map_errors.len());
    self.restore_map_checkpoint(checkpoint);
    self.deferred_map_errors.extend(rejected);
  }

  fn visit_closed_map(&mut self, group: &Group<'a>, keys: &[String]) -> visitor::Result<Error> {
    let search = Arc::new(Mutex::new(MapChoiceSearch::default()));
    let previous_search = self.map_choice_search.replace(search.clone());
    let previous_deferred = std::mem::take(&mut self.deferred_map_errors);
    let result = self.visit_map_candidates(group, keys, &search);
    self.map_choice_search = previous_search;
    self.deferred_map_errors = previous_deferred;
    result
  }

  fn planned_group_choice(&self, width: usize) -> std::result::Result<Option<usize>, Error> {
    if width < 2 || self.state.is_group_to_choice_enum {
      return Ok(None);
    }
    let Some(search) = self.map_choice_search.as_ref() else {
      return Ok(None);
    };
    let selected = {
      let mut search = search.lock().expect("map choice search lock");
      search.select(width)
    };
    if selected >= width {
      return Err(Error::from_validator(
        self,
        format!("invalid map group-choice replay index {selected} for {width} alternatives"),
      ));
    }
    Ok(Some(selected))
  }

  fn visit_planned_group_reference(
    &mut self,
    entry: &TypeGroupnameEntry<'a>,
    alternates: &[&GroupEntry<'a>],
  ) -> visitor::Result<Error> {
    if rule_from_ident(self.state.cddl, &entry.name).is_none() {
      let selected = self.planned_group_choice(alternates.len())?.unwrap_or(0);
      if let Some(alternate) = alternates.get(selected) {
        return self.visit_incremental_group_alternative(&entry.name, alternate);
      }
    }
    // The rule visitor records incremental alternatives once, together
    // with the base definition, in the enclosing map's replay path.
    walk_type_groupname_entry(self, entry)?;
    self.state.type_group_name_entry = None;
    Ok(())
  }

  fn visit_incremental_group_alternative(
    &mut self,
    name: &Identifier<'a>,
    alternate: &GroupEntry<'a>,
  ) -> Result {
    let (marker, recursive) = self.rule_recursion_marker(name);
    if recursive {
      self.add_error("recursive incremental group reference without consuming a map entry".into());
      return Ok(());
    }
    self.visit_named_rule_path(marker, |validator| validator.visit_group_entry(alternate))
  }

  fn visit_selected_group_choice(
    &mut self,
    group: &Group<'a>,
    selected: usize,
  ) -> visitor::Result<Error> {
    let choice = group
      .group_choices
      .get(selected)
      .ok_or_else(|| Error::from_validator(self, "invalid map group-choice replay index".into()))?;
    self.visit_group_choice(choice)
  }

  fn visit_map_candidates(
    &mut self,
    group: &Group<'a>,
    keys: &[String],
    search: &Mutex<MapChoiceSearch>,
  ) -> visitor::Result<Error> {
    let checkpoint = self.map_checkpoint();
    let error_count = self.errors.len();
    let mut choice_errors = Vec::new();
    let mut seen_errors = HashSet::new();
    let has_unknown_key = self.provably_unknown_map_key(group);
    loop {
      if let Err(error) = self.visit_group(group) {
        self.restore_map_checkpoint(checkpoint);
        return Err(error);
      }
      // Pruning only suppresses diagnostics on an already-failed candidate.
      let matched_before_coverage = self.errors.len() == error_count;
      if matched_before_coverage || !self.map_traversal_pruned() {
        self.validate_map_coverage(keys);
      }
      if self.errors.len() == error_count {
        return Ok(());
      }
      for error in self.errors.drain(error_count..) {
        if seen_errors.insert(error.clone()) {
          choice_errors.push(error);
        }
      }
      self.restore_map_checkpoint(checkpoint.clone());
      if matched_before_coverage && has_unknown_key {
        self.errors.extend(choice_errors);
        return Ok(());
      }
      let has_next = {
        let mut search = search.lock().expect("map choice search lock");
        search.advance()
      };
      if !has_next {
        self.errors.extend(choice_errors);
        return Ok(());
      }
      self.map_replay_budget.charge_replay()?;
    }
  }

  fn visit_zero_minimum_entry(
    &mut self,
    entry: &GroupEntry<'a>,
    max_occurrences: Option<usize>,
  ) -> visitor::Result<Error> {
    if max_occurrences == Some(0) {
      self.state.occurrence = None;
      return Ok(());
    }
    // A failing zero-minimum entry can take its zero-width path.
    // Key lookup can reassign earlier claims, so skipping a failed member
    // must restore the whole checkpoint, not just release its selected key.
    let checkpoint = self.map_checkpoint();
    let result = walk_group_entry(self, entry);
    if result.is_err() {
      self.restore_map_checkpoint(checkpoint);
    } else if self.errors.len() > checkpoint.errors.len() {
      self.rollback_optional_map_checkpoint(checkpoint);
    }
    self.state.occurrence = None;
    result
  }

  fn claimed_map_key_count(&self) -> usize {
    self
      .validated_keys
      .as_ref()
      .map_or(0, |keys| keys.iter().collect::<HashSet<_>>().len())
  }

  fn rule_recursion_marker(&self, ident: &Identifier<'a>) -> (String, bool) {
    let consumed = if self.map_choice_search.is_some() {
      self.claimed_map_key_count()
    } else {
      0
    };
    let key = format!(
      "{}\u{0}{}\u{0}{consumed}",
      qualified_ident_key(ident),
      self.state.data_location
    );
    let recursive =
      !self.state.is_colon_shortcut_present && self.state.visited_rules.contains(&key);
    (key, recursive)
  }

  fn batch_key_is_parameter(
    &self,
    ident: &Identifier<'a>,
    params: Option<&GenericParams<'a>>,
  ) -> bool {
    params.is_some_and(|params| {
      params
        .params
        .iter()
        .any(|param| param.param.ident == ident.ident)
    }) || self.state.generic_rules.iter().any(|rule| {
      Some(rule.name) == self.state.eval_generic_rule && rule.params.contains(&ident.ident)
    })
  }

  fn batch_key_depth(
    &self,
    mut ident: &Identifier<'a>,
    params: Option<&GenericParams<'a>>,
  ) -> Option<usize> {
    let mut visited = HashSet::new();
    loop {
      if ident.socket.is_some()
        || self.batch_key_is_parameter(ident, params)
        || !visited.insert(ident.ident)
      {
        return None;
      }
      let definitions = type_choice_types_from_ident(self.state.cddl, ident);
      let Some(rule) = rule_from_ident(self.state.cddl, ident) else {
        return (definitions.is_empty() && matches!(ident.ident, "tstr" | "text" | "any"))
          .then_some(visited.len() - 1);
      };
      let Rule::Type { rule, .. } = rule else {
        return None;
      };
      if rule.generic_params.is_some() || definitions.len() != 1 {
        return None;
      }
      let [choice] = rule.value.type_choices.as_slice() else {
        return None;
      };
      if choice.type1.operator.is_some() {
        return None;
      }
      let Type2::Typename {
        ident: next,
        generic_args: None,
        ..
      } = &choice.type1.type2
      else {
        return None;
      };
      ident = next;
    }
  }

  fn batchable_map_member<'b>(
    &self,
    group: &'b Group<'a>,
    params: Option<&GenericParams<'a>>,
    pair_source: &Self,
    pending_calls: usize,
  ) -> Option<&'b ValueMemberKeyEntry<'a>> {
    let has_movable_claims = pair_source.member_claims.iter().any(|claim| {
      matches!(
        &claim.entry.member_key,
        Some(MemberKey::Type1 { is_cut: false, .. })
      )
    });
    let [choice] = group.group_choices.as_slice() else {
      return None;
    };
    let [(GroupEntry::ValueMemberKey { ge, .. }, _)] = choice.group_entries.as_slice() else {
      return None;
    };
    if ge.occur.is_some() {
      return None;
    }
    let Some(MemberKey::Type1 {
      t1, is_cut: false, ..
    }) = &ge.member_key
    else {
      return None;
    };
    if choice_member_key(&self.state, ge).is_some() {
      return (!has_movable_claims).then_some(ge);
    }
    let Type2::Typename {
      ident,
      generic_args: None,
      ..
    } = &t1.type2
    else {
      return None;
    };
    let key_depth = self.batch_key_depth(ident, params)?;
    if t1.operator.is_none()
      && pair_source.map_rule_depth_allows(key_depth.saturating_add(pending_calls))
      && (!has_movable_claims
        || pair_source.batch_covers_unclaimed_values(self.batch_value_predicate(&ge.entry_type)))
    {
      Some(ge)
    } else {
      None
    }
  }

  fn batch_value_predicate(&self, value_type: &Type<'a>) -> Option<fn(&Value) -> bool> {
    // The value visitor and argument resolver index differently when bindings
    // accumulate or arity differs. Do not prove equivalence in that case.
    if self.state.generic_rules.iter().any(|rule| {
      Some(rule.name) == self.state.eval_generic_rule && rule.args.len() != rule.params.len()
    }) {
      return None;
    }
    let [choice] = value_type.type_choices.as_slice() else {
      return None;
    };
    let resolved = resolve_generic_argument(&self.state, &choice.type1);
    let Type2::Typename {
      ident,
      generic_args: None,
      ..
    } = &resolved.type2
    else {
      return None;
    };
    if resolved.operator.is_some()
      || ident.socket.is_some()
      || self.batch_key_is_parameter(ident, None)
      || rule_from_ident(self.state.cddl, ident).is_some()
      || !type_choice_types_from_ident(self.state.cddl, ident).is_empty()
    {
      return None;
    }
    match ident.ident {
      "tstr" | "text" => Some(Value::is_string),
      "uint" => Some(Value::is_u64),
      "any" => Some(|_| true),
      _ => None,
    }
  }

  fn proven_scalar_type(
    &self,
    value_type: &Type<'a>,
    params: Option<&GenericParams<'a>>,
  ) -> Option<ProvenScalarType> {
    if self.state.generic_rules.iter().any(|rule| {
      Some(rule.name) == self.state.eval_generic_rule && rule.args.len() != rule.params.len()
    }) {
      return None;
    }
    let [choice] = value_type.type_choices.as_slice() else {
      return None;
    };
    if choice.type1.operator.is_some() {
      return None;
    }
    let Type2::Typename {
      ident,
      generic_args: None,
      ..
    } = &choice.type1.type2
    else {
      return None;
    };
    let mut ident = ident;
    let mut visited = HashSet::new();
    loop {
      if ident.socket.is_some()
        || self.batch_key_is_parameter(ident, params)
        || !visited.insert(ident.ident)
      {
        return None;
      }
      let definitions = type_choice_types_from_ident(self.state.cddl, ident);
      let Some(rule) = rule_from_ident(self.state.cddl, ident) else {
        if !definitions.is_empty() {
          return None;
        }
        return match ident.ident {
          "uint" => Some(ProvenScalarType::Uint),
          "tstr" | "text" | "any" | "int" | "nint" | "number" | "bool" | "true" | "false"
          | "null" | "nil" => Some(ProvenScalarType::Other),
          _ => None,
        };
      };
      let Rule::Type { rule, .. } = rule else {
        return None;
      };
      if rule.generic_params.is_some()
        || definitions.len() != 1
        || token::lookup_ident(ident.ident)
          .in_standard_prelude()
          .is_some()
      {
        return None;
      }
      let [choice] = rule.value.type_choices.as_slice() else {
        return None;
      };
      if choice.type1.operator.is_some() {
        return None;
      }
      let Type2::Typename {
        ident: next,
        generic_args: None,
        ..
      } = &choice.type1.type2
      else {
        return None;
      };
      ident = next;
    }
  }

  fn provably_unknown_map_key(&self, group: &Group<'a>) -> bool {
    let Value::Object(object) = self.json.as_ref() else {
      return false;
    };
    if self.state.ctrl.is_some()
      || self.state.is_group_to_choice_enum
      || self.state.eval_generic_rule.is_some()
      || self.state.is_cut_present
      || self.cut_value.is_some()
      || !self.member_claims.is_empty()
      || self
        .validated_keys
        .as_ref()
        .is_some_and(|keys| !keys.is_empty())
    {
      return false;
    }
    let mut proof = LiteralKeyProof::new(true);
    proof.push_group(group);
    if !self.finish_literal_key_proof(&mut proof) {
      return false;
    }
    // Unknown values are never visited by a literal-key-only schema. Known
    // structured values retain the normal path and its nested hard errors.
    object.iter().all(|(key, value)| {
      !proof.keys.contains(key) || !matches!(value, Value::Array(_) | Value::Object(_))
    }) && object.keys().any(|key| !proof.keys.contains(key))
  }

  fn finish_literal_key_proof<'b>(&'b self, proof: &mut LiteralKeyProof<'b, 'a>) -> bool {
    while let Some(step) = proof.pending.pop() {
      match step {
        KeyProofStep::Exit(name) => proof.finish_named(name),
        KeyProofStep::Entry(entry) => {
          if self.add_literal_key_proof(entry, proof).is_none() {
            return false;
          }
        }
      }
    }
    true
  }

  fn add_literal_key_proof<'b>(
    &'b self,
    entry: &'b GroupEntry<'a>,
    proof: &mut LiteralKeyProof<'b, 'a>,
  ) -> Option<()> {
    if let GroupEntry::ValueMemberKey { ge, .. } = entry {
      if member_key_is_cut(ge) || bounded_zero_minimum_colon(entry) {
        return None;
      }
    }
    if !proof.inspect_values && group_entry_bounds(entry) == (0, Some(0)) {
      return Some(());
    }
    match entry {
      GroupEntry::ValueMemberKey { ge, .. } => {
        if proof.inspect_values && self.proven_scalar_type(&ge.entry_type, None).is_none() {
          return None;
        }
        proof
          .keys
          .insert(self.plain_literal_key(entry, None)?.to_owned());
      }
      GroupEntry::InlineGroup { group, .. } => proof.push_group(group),
      GroupEntry::TypeGroupname { ge, .. } => self.add_named_key_proof(ge, proof)?,
    }
    Some(())
  }

  fn add_named_key_proof<'b>(
    &'b self,
    entry: &'b TypeGroupnameEntry<'a>,
    proof: &mut LiteralKeyProof<'b, 'a>,
  ) -> Option<()> {
    if entry.generic_args.is_some()
      || entry.name.socket.is_some()
      || self.batch_key_is_parameter(&entry.name, None)
      || !group_choice_alternates_from_ident(self.state.cddl, &entry.name).is_empty()
    {
      return None;
    }
    let Rule::Group { rule, .. } = rule_from_ident(self.state.cddl, &entry.name)? else {
      return None;
    };
    if rule.generic_params.is_some() {
      return None;
    }
    let depth = proof.complete.get(entry.name.ident).copied();
    let required = self
      .map_rule_depth
      .active
      .load(Ordering::Relaxed)
      .saturating_add(proof.active.len())
      .saturating_add(depth.unwrap_or(1));
    if required > self.map_rule_depth.limit {
      return None;
    }
    if let Some(depth) = depth {
      proof.inherit_depth(depth);
      return Some(());
    }
    if !proof.active.insert(entry.name.ident) {
      return None;
    }
    proof.depths.push(1);
    proof.pending.push(KeyProofStep::Exit(entry.name.ident));
    proof.pending.push(KeyProofStep::Entry(&rule.entry));
    Some(())
  }

  fn batch_covers_unclaimed_values(&self, predicate: Option<fn(&Value) -> bool>) -> bool {
    let (Value::Object(object), Some(matches)) = (self.json.as_ref(), predicate) else {
      return false;
    };
    let claimed: HashSet<_> = self.validated_keys.iter().flatten().collect();
    // With plain string keys and no remaining value mismatch, the incremental
    // path never reassigns earlier claims. Otherwise keep that path unchanged.
    object
      .iter()
      .all(|(key, value)| claimed.contains(key) || matches(value))
  }

  fn batchable_named_map_member(
    &self,
    entry: &TypeGroupnameEntry<'a>,
    bindings_ready: bool,
  ) -> Option<ValueMemberKeyEntry<'a>> {
    if !matches!(self.json.as_ref(), Value::Object(_)) {
      return None;
    }
    let occurrence = entry.occur.as_ref()?;
    if occurrence_bounds(Some(occurrence.occur)).0 != 0
      || !group_choice_alternates_from_ident(self.state.cddl, &entry.name).is_empty()
    {
      return None;
    }
    let Rule::Group { rule, .. } = rule_from_ident(self.state.cddl, &entry.name)? else {
      return None;
    };
    let GroupEntry::InlineGroup {
      occur: None, group, ..
    } = &rule.entry
    else {
      return None;
    };
    if let Some(args) = entry.generic_args.as_ref().filter(|_| !bindings_ready) {
      // Resolve in the caller's scope, never again after entering the callee.
      let mut scoped = self.new_with_recursion_state(Value::Null);
      scoped.set_generic_group_bindings(
        entry.name.ident,
        resolved_generic_args(&self.state, args),
        rule.generic_params.as_ref().map(|params| {
          params
            .params
            .iter()
            .map(|param| param.param.ident)
            .collect()
        }),
      );
      scoped
        .batchable_map_member(group, rule.generic_params.as_ref(), self, 1)
        .cloned()
    } else {
      self
        .batchable_map_member(
          group,
          rule.generic_params.as_ref(),
          self,
          usize::from(!bindings_ready),
        )
        .cloned()
    }
  }

  fn visit_batched_named_map_member(
    &mut self,
    member: &ValueMemberKeyEntry<'a>,
    occurrence: &Occurrence<'a>,
  ) -> Result {
    let _depth = self.enter_map_rule()?;
    self.visit_batched_map_member(member, occurrence)
  }

  fn visit_batched_map_member(
    &mut self,
    member: &ValueMemberKeyEntry<'a>,
    occurrence: &Occurrence<'a>,
  ) -> Result {
    let prior_occurrence = self.state.occurrence;
    let Value::Object(object) = self.json.as_ref() else {
      return Ok(());
    };
    // Enumerate unclaimed keys once, but retain individual movable claims:
    // a later literal member may still need to reassign a bounded repetition.
    let claimed: HashSet<_> = self.validated_keys.iter().flatten().collect();
    let candidates = object
      .iter()
      .filter(|(key, _)| !claimed.contains(key))
      .map(|(key, value)| (key.clone(), value.clone()))
      .collect();
    let checkpoint = self.map_checkpoint();
    let mut repeated = member.clone();
    repeated.occur = Some(occurrence.clone());
    let alternatives =
      choice_member_key(&self.state, member).map(|(_, alternatives, _)| alternatives);
    let result = self.validate_candidate_pairs(
      &repeated,
      candidates,
      self.state.data_location.clone(),
      Some(member),
      alternatives.as_deref(),
    );
    if result.is_err() {
      self.restore_map_checkpoint(checkpoint);
    }
    self.state.occurrence = prior_occurrence;
    result
  }

  fn visit_zero_minimum_group<F>(&mut self, max: Option<usize>, mut visit: F) -> Result
  where
    F: FnMut(&mut Self) -> Result,
  {
    let prior_occurrence = self.state.occurrence;
    // The occurrence repeats complete groups, not each member inside the group.
    let group_checkpoint = self.map_checkpoint();
    let mut count = 0;
    while max.is_none_or(|max| count < max) {
      let checkpoint = self.map_checkpoint();
      let consumed = self.claimed_map_key_count();
      self.state.occurrence = None;
      if let Err(error) = visit(self) {
        self.restore_map_checkpoint(group_checkpoint);
        self.state.occurrence = prior_occurrence;
        return Err(error);
      }
      if self.errors.len() > checkpoint.errors.len() {
        self.rollback_optional_map_checkpoint(checkpoint);
        break;
      }
      count += 1;
      if self.claimed_map_key_count() <= consumed {
        break;
      }
    }
    self.state.occurrence = prior_occurrence;
    Ok(())
  }

  fn plain_literal_key<'b>(
    &self,
    entry: &'b GroupEntry<'a>,
    params: Option<&GenericParams<'a>>,
  ) -> Option<&'b str> {
    let GroupEntry::ValueMemberKey { ge, .. } = entry else {
      return None;
    };
    let key = match &ge.member_key {
      Some(MemberKey::Bareword { ident, .. })
        if ident.socket.is_none()
          && !self.batch_key_is_parameter(ident, params)
          && rule_from_ident(self.state.cddl, ident).is_none()
          && type_choice_types_from_ident(self.state.cddl, ident).is_empty()
          && token::lookup_ident(ident.ident)
            .in_standard_prelude()
            .is_none() =>
      {
        ident.ident
      }
      Some(MemberKey::Value {
        value: token::Value::TEXT(value),
        ..
      }) => value.as_ref(),
      Some(MemberKey::Type1 { t1, .. }) if t1.operator.is_none() => match &t1.type2 {
        Type2::TextValue { value, .. } => value.as_ref(),
        _ => return None,
      },
      _ => return None,
    };
    // The existing literal-key visitor gives "any" a special non-literal path.
    (key != "any").then_some(key)
  }

  fn required_uint_definitely_mismatches(
    &self,
    entry: &ValueMemberKeyEntry<'a>,
    key: &str,
    value: &Value,
    params: Option<&GenericParams<'a>>,
  ) -> bool {
    if entry.occur.is_some()
      || member_key_is_cut(entry)
      || self.state.is_cut_present
      || self.cut_value.is_some()
      || !value.is_string()
      || self
        .validated_keys
        .as_ref()
        .is_some_and(|keys| keys.iter().any(|claimed| claimed == key))
    {
      return false;
    }
    self.proven_scalar_type(&entry.entry_type, params) == Some(ProvenScalarType::Uint)
  }

  fn optional_choice_definitely_fails(
    &self,
    choice: &GroupChoice<'a>,
    object: &serde_json::Map<String, Value>,
    params: Option<&GenericParams<'a>>,
  ) -> bool {
    for (entry, _) in &choice.group_entries {
      let GroupEntry::ValueMemberKey { ge, .. } = entry else {
        return false;
      };
      let Some(key) = self.plain_literal_key(entry, params) else {
        return false;
      };
      let required = occurrence_bounds(ge.occur.as_ref().map(|o| o.occur)).0 > 0;
      if let Some(value) = object.get(key) {
        return required && self.required_uint_definitely_mismatches(ge, key, value, params);
      }
      if required {
        return true;
      }
      // Only absent plain optional members can be crossed without evaluating
      // a value, recording a claim, or deferring a value error.
      if member_key_is_cut(ge)
        || !matches!(
          ge.occur.as_ref().map(|o| o.occur),
          Some(Occur::Optional { .. })
        )
      {
        return false;
      }
    }
    false
  }

  fn optional_choices_definitely_fail(
    &self,
    group: &Group<'a>,
    params: Option<&GenericParams<'a>>,
  ) -> bool {
    let Value::Object(object) = self.json.as_ref() else {
      return false;
    };
    if self.map_choice_search.is_none()
      || self.state.ctrl.is_some()
      || self.state.is_group_to_choice_enum
      || group.group_choices.len() < 2
    {
      return false;
    }
    // A planned choice stops on its first required mismatch. Prove that
    // every arm fails without any earlier value evaluation or claimed-key
    // reassignment. A required failure is rolled back, not deferred.
    group
      .group_choices
      .iter()
      .all(|choice| self.optional_choice_definitely_fails(choice, object, params))
  }

  fn group_choices_are_empty(&self, group: &Group<'a>) -> bool {
    let Value::Object(object) = self.json.as_ref() else {
      return false;
    };
    if self.map_choice_search.is_none()
      || self.state.ctrl.is_some()
      || self.state.is_group_to_choice_enum
      || self.state.is_cut_present
      || self.cut_value.is_some()
    {
      return false;
    }
    if !group.group_choices.iter().all(|choice| {
      choice
        .group_entries
        .iter()
        .all(|(entry, _)| group_entry_bounds(entry).0 == 0)
    }) {
      return false;
    }
    // All reachable literal keys must be absent. No value is evaluated, but
    // named calls still need the same depth proof as ordinary coverage analysis.
    let mut proof = LiteralKeyProof::new(false);
    proof.push_group(group);
    self.finish_literal_key_proof(&mut proof)
      && proof.keys.iter().all(|key| !object.contains_key(key))
  }

  fn planned_concrete_group_choice(
    &self,
    group: &Group<'a>,
  ) -> std::result::Result<Option<usize>, Error> {
    if group.group_choices.len() < 2 || !self.group_choices_are_empty(group) {
      return self.planned_group_choice(group.group_choices.len());
    }
    let selected = self
      .map_choice_search
      .as_ref()
      .unwrap()
      .lock()
      .expect("map choice search lock")
      .select(1);
    if selected != 0 {
      return Err(Error::from_validator(
        self,
        "invalid no-progress group-choice replay index".into(),
      ));
    }
    Ok(Some(selected))
  }

  fn skip_proven_optional_choices(
    &self,
    group: &Group<'a>,
    params: Option<&GenericParams<'a>>,
    can_iterate: bool,
  ) -> std::result::Result<bool, Error> {
    if !self.optional_choices_definitely_fail(group, params) {
      return Ok(false);
    }
    let Value::Object(object) = self.json.as_ref() else {
      return Ok(false);
    };
    let originally_absent = group.group_choices.iter().all(|choice| {
      choice.group_entries.first().is_some_and(|(entry, _)| {
        let GroupEntry::ValueMemberKey { ge, .. } = entry else {
          return false;
        };
        occurrence_bounds(ge.occur.as_ref().map(|o| o.occur)).0 > 0
          && self
            .plain_literal_key(entry, params)
            .is_some_and(|key| !object.contains_key(key))
      })
    });
    if !originally_absent && can_iterate {
      if let Some(search) = &self.map_choice_search {
        // The new proofs replace a previously recorded choice. Keep its
        // pruning context, but no alternatives or additional replay work.
        let selected = search.lock().expect("map choice search lock").select(1);
        if selected != 0 {
          return Err(Error::from_validator(
            self,
            "invalid map group-choice replay index for proven optional failure".into(),
          ));
        }
      }
    }
    Ok(true)
  }

  fn skip_proven_named_choices(
    &self,
    entry: &TypeGroupnameEntry<'a>,
  ) -> std::result::Result<bool, Error> {
    if !self.map_rule_depth_allows(1)
      || !group_choice_alternates_from_ident(self.state.cddl, &entry.name).is_empty()
    {
      return Ok(false);
    }
    let Some(Rule::Group { rule, .. }) = rule_from_ident(self.state.cddl, &entry.name) else {
      return Ok(false);
    };
    let GroupEntry::InlineGroup {
      occur: None, group, ..
    } = &rule.entry
    else {
      return Ok(false);
    };
    self.skip_proven_optional_choices(
      group,
      rule.generic_params.as_ref(),
      occurrence_bounds(entry.occur.as_ref().map(|o| o.occur)).1 != Some(0),
    )
  }

  fn visit_optional_named_group(&mut self, entry: &TypeGroupnameEntry<'a>) -> Result {
    if occurrence_bounds(entry.occur.as_ref().map(|o| o.occur)).1 == Some(0) {
      return Ok(());
    }
    if self.skip_proven_named_choices(entry)? {
      return Ok(());
    }
    if self.batchable_named_map_member(entry, false).is_some() {
      let prior_occurrence = self.state.occurrence;
      let checkpoint = self.map_checkpoint();
      let result = self.visit_type_groupname_entry(entry);
      if result.is_err() {
        self.restore_map_checkpoint(checkpoint);
      }
      self.state.occurrence = prior_occurrence;
      return result;
    }
    let max = occurrence_bounds(entry.occur.as_ref().map(|o| o.occur)).1;
    let mut required = entry.clone();
    required.occur = None;
    self.visit_zero_minimum_group(max, |validator| {
      validator.visit_type_groupname_entry(&required)
    })
  }

  fn generic_group_child(&mut self, instance: String) -> Option<JSONValidator<'a>> {
    let consumed = self.claimed_map_key_count();
    let key = format!("{instance}\u{0}{}\u{0}{consumed}", self.state.data_location);
    if self.map_choice_search.is_some() && self.state.visited_rules.contains(&key) {
      self.add_error("recursive generic group reference without consuming a map entry".into());
      return None;
    }
    let mut child = self.new_with_recursion_state(self.json.clone());
    #[cfg(test)]
    if !Arc::ptr_eq(&self.json, &child.json) {
      self
        .map_replay_budget
        .generic_input_copies
        .fetch_add(1, Ordering::Relaxed);
    }
    child.state.data_location = self.state.data_location.clone();
    if self.map_choice_search.is_some() {
      child.state.visited_rules.insert(key);
    }
    Some(child)
  }

  fn visit_generic_group_reference(
    &mut self,
    entry: &TypeGroupnameEntry<'a>,
    args: &GenericArgs<'a>,
    rule: &Rule<'a>,
  ) -> Result {
    let resolved_args = resolved_generic_args(&self.state, args);
    let instance =
      generic_instantiation_key(&qualified_ident_key(&entry.name), resolved_args.iter());
    let Some(mut jv) = self.generic_group_child(instance) else {
      return Ok(());
    };
    let _depth = self.enter_map_rule()?;
    jv.set_generic_group_bindings(
      entry.name.ident,
      resolved_args,
      generic_params_from_rule(rule),
    );

    jv.state.is_multi_type_choice = self.state.is_multi_type_choice;
    jv.validated_keys = self.validated_keys.clone();
    jv.member_claims = self.member_claims.clone();
    jv.map_choice_search = self.map_choice_search.clone();
    if let (Some(member), Some(occurrence)) = (
      jv.batchable_named_map_member(entry, true),
      entry.occur.as_ref(),
    ) {
      jv.visit_batched_map_member(&member, occurrence)?;
    } else {
      jv.visit_rule(rule)?;
    }

    let child_succeeded = jv.errors.is_empty();
    self.errors.append(&mut jv.errors);
    self.deferred_map_errors.append(&mut jv.deferred_map_errors);
    if child_succeeded {
      self.validated_keys = jv.validated_keys;
      self.member_claims = jv.member_claims;
    } else if let Some(keys) = jv.validated_keys {
      // Failed children can still have consumed keys in contexts tolerating
      // their mismatch; retain those keys for the enclosing coverage check.
      let claimed = self.validated_keys.get_or_insert_with(Vec::new);
      let mut seen: HashSet<_> = claimed.iter().cloned().collect();
      claimed.extend(keys.into_iter().filter(|key| seen.insert(key.clone())));
    }
    Ok(())
  }

  fn set_generic_group_bindings(
    &mut self,
    name: &'a str,
    args: Vec<Type1<'a>>,
    params: Option<Vec<&'a str>>,
  ) {
    if let Some(gr) = self
      .state
      .generic_rules
      .iter_mut()
      .find(|gr| gr.name == name)
    {
      // Claims retain their own captured bindings when another instantiation
      // replaces the current positional arguments.
      gr.params = params.unwrap_or_default();
      gr.args = args;
    } else if let Some(params) = params {
      self
        .state
        .generic_rules
        .push(GenericRule { name, params, args });
    }
    self.state.eval_generic_rule = Some(name);
  }

  fn visit_group_sequence(&mut self, choice: &GroupChoice<'a>) -> Result {
    let saved = self.map_choice_search.as_ref().map(|search| {
      let previous = search
        .lock()
        .expect("map choice search lock")
        .decisions_suppressed;
      (search.clone(), previous)
    });
    let result = self.visit_group_sequence_entries(choice);
    if let Some((search, previous)) = saved {
      search
        .lock()
        .expect("map choice search lock")
        .decisions_suppressed = previous;
    }
    result
  }

  fn visit_group_sequence_entries(&mut self, choice: &GroupChoice<'a>) -> Result {
    let mut effect_free = None;
    for (index, (entry, _)) in choice.group_entries.iter().enumerate() {
      let error_count = self.errors.len();
      self.visit_group_entry(entry)?;
      // Later siblings cannot repair an already-reported required mismatch.
      // Do not record their choices in this failed replay prefix.
      if self.errors.len() > error_count {
        if let Some(search) = &self.map_choice_search {
          let mut search = search.lock().expect("map choice search lock");
          if !search.widths.is_empty()
            && (search.decisions_suppressed || search.has_unsuppressed_decision)
          {
            search.traversal_pruned |= index + 1 < choice.group_entries.len();
            break;
          }
          if !search.decisions_suppressed {
            search.decisions_suppressed =
              *effect_free.get_or_insert_with(|| self.map_sequence_is_effect_free(choice));
          }
        }
      }
    }
    Ok(())
  }

  fn map_sequence_is_effect_free(&self, choice: &GroupChoice<'a>) -> bool {
    let Value::Object(object) = self.json.as_ref() else {
      return false;
    };
    if self.state.ctrl.is_some()
      || self.state.is_group_to_choice_enum
      || self.state.eval_generic_rule.is_some()
      || !self.member_claims.is_empty()
      || object
        .values()
        .any(|value| matches!(value, Value::Array(_) | Value::Object(_)))
    {
      return false;
    }
    let mut pending = choice
      .group_entries
      .iter()
      .map(|(entry, _)| entry)
      .collect::<Vec<_>>();
    while let Some(entry) = pending.pop() {
      match entry {
        GroupEntry::ValueMemberKey { .. } if self.map_member_is_effect_free(entry) => {}
        GroupEntry::InlineGroup { group, .. } => {
          pending.extend(
            group
              .group_choices
              .iter()
              .flat_map(|choice| choice.group_entries.iter().map(|(entry, _)| entry)),
          );
        }
        // Named calls can themselves reach the separate recursion-depth limit.
        _ => return false,
      }
    }
    true
  }

  fn map_member_is_effect_free(&self, entry: &GroupEntry<'a>) -> bool {
    let GroupEntry::ValueMemberKey { ge, .. } = entry else {
      return false;
    };
    if member_key_is_cut(ge)
      || bounded_zero_minimum_colon(entry)
      || self.proven_scalar_type(&ge.entry_type, None).is_none()
    {
      return false;
    }
    if self.plain_literal_key(entry, None).is_some() {
      return true;
    }
    let Some(MemberKey::Type1 { t1, .. }) = &ge.member_key else {
      return false;
    };
    let Type2::Typename {
      ident,
      generic_args: None,
      ..
    } = &t1.type2
    else {
      return false;
    };
    t1.operator.is_none()
      && matches!(ident.ident, "tstr" | "text" | "any")
      && rule_from_ident(self.state.cddl, ident).is_none()
      && self
        .batch_key_depth(ident, None)
        .is_some_and(|depth| self.map_rule_depth_allows(depth))
  }

  /// Validate the pair relayed from a key-domain member key. On success the
  /// member records a movable claim; on failure the tentative key is
  /// released and every single-occurrence member is retried as a one-to-one
  /// assignment before the failure is reported.
  fn validate_relayed_pair(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
    value: Value,
    object_value_key: Option<String>,
    current_location: String,
  ) -> visitor::Result<Error> {
    let mut jv = self.new_with_recursion_state(value);
    jv.state.is_multi_type_choice = self.state.is_multi_type_choice;
    jv.state.is_multi_group_choice = self.state.is_multi_group_choice;
    jv.state.data_location.push_str(&self.state.data_location);
    jv.state.type_group_name_entry = self.state.type_group_name_entry;
    jv.visit_type(&entry.entry_type)?;

    self.state.data_location = current_location;

    if jv.errors.is_empty() {
      if let Some(key) = object_value_key {
        self.member_claims.push(MemberClaim {
          key,
          entry: entry.clone(),
          context: self.current_claim_context(),
        });
      }
      self.errors.append(&mut jv.errors);
    } else {
      if let Some(key) = &object_value_key {
        if let Some(keys) = &mut self.validated_keys {
          keys.retain(|claimed| claimed != key);
        }
      }
      // A cut commits the member to the pair whose key it matched; its value
      // failure must be reported, not retried away.
      if member_key_is_cut(entry) || !self.reassign_choice_claims(entry)? {
        self.defer_optional_value_errors(entry, object_value_key.as_deref(), &jv.errors);
        if let Some(key) = &object_value_key {
          self
            .validated_keys
            .get_or_insert_with(Vec::new)
            .push(key.clone());
        }
        self.errors.append(&mut jv.errors);
      }
    }
    if entry.occur.is_some() {
      self.state.occurrence = None;
    }

    Ok(())
  }
  /// Retry the single-occurrence members as a one-to-one assignment of
  /// complete pairs, so an earlier member can give up a pair a later member
  /// needs. Choice-key and plain key-domain claims alike may move.
  fn reassign_choice_claims(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
  ) -> std::result::Result<bool, Error> {
    let claim = MemberClaim {
      key: String::new(),
      entry: entry.clone(),
      context: self.current_claim_context(),
    };
    self.reassign_movable_claims(Some(claim), None)
  }

  /// Free a key that a fixed member needs but a movable claim holds, by
  /// moving the movable claims one-to-one onto the remaining pairs.
  fn release_claimed_key(&mut self, key: &str) -> std::result::Result<bool, Error> {
    let held = self
      .member_claims
      .iter()
      .any(|claim| claim.key == key && !member_key_is_cut(&claim.entry));
    if !held {
      return Ok(false);
    }
    self.reassign_movable_claims(None, Some(key))
  }

  /// Solve the one-to-one assignment of the movable claims, plus an optional
  /// new claim, over the unclaimed and movably claimed pairs other than
  /// `excluded`, and commit it when every claim gets a pair.
  fn reassign_movable_claims(
    &mut self,
    new_claim: Option<MemberClaim<'a>>,
    excluded: Option<&str>,
  ) -> std::result::Result<bool, Error> {
    let Value::Object(entries) = self.json.as_ref() else {
      return Ok(false);
    };
    // Pairs claimed under a cut stay committed; only movable claims take
    // part in the retry.
    let movable: Vec<usize> = self
      .member_claims
      .iter()
      .enumerate()
      .filter(|(_, claim)| !member_key_is_cut(&claim.entry))
      .map(|(index, _)| index)
      .collect();
    let mut claims: Vec<MemberClaim<'a>> = movable
      .iter()
      .map(|&index| self.member_claims[index].clone())
      .collect();
    let validated: HashSet<_> = self.validated_keys.iter().flatten().collect();
    let movable_keys: HashSet<_> = claims.iter().map(|claim| claim.key.as_str()).collect();
    // Ownership is checked against the existing movable claims only: the new
    // claim's empty-key sentinel must not make a real empty-string key movable.
    let candidates: Vec<(String, Value)> = entries
      .iter()
      .filter(|(key, _)| {
        excluded != Some(key.as_str())
          && (!validated.contains(key) || movable_keys.contains(key.as_str()))
      })
      .map(|(key, value)| (key.clone(), value.clone()))
      .collect();
    claims.extend(new_claim);
    if candidates.len() < claims.len() {
      return Ok(false);
    }

    let Some(compatibility) = claim_compatibility(&claims, candidates.len(), |claim, pair| {
      let (key, value) = &candidates[pair];
      self.pair_satisfies_member(claim, key, value)
    })?
    else {
      return Ok(false);
    };

    let mut pair_owners = vec![None; candidates.len()];
    // Different classes must retain the original displacement order: which
    // pairs remain free can determine whether a later table member succeeds.
    let (classes, class_count) = compatibility_classes(&compatibility);
    let interchangeable = class_count < 2;
    for claim_slot in 0..claims.len() {
      let mut visited_pairs = vec![false; candidates.len()];
      let mut positions = vec![0; class_count];
      if !augment_choice_assignment(
        claim_slot,
        &compatibility,
        &mut visited_pairs,
        &mut pair_owners,
        interchangeable,
        &classes,
        &mut positions,
      ) {
        return Ok(false);
      }
    }

    self.commit_choice_assignment(&movable, &claims, &candidates, &pair_owners)
  }

  /// Commit a solved assignment: movable claims move to their new keys and
  /// the validated-key set follows. Cut-claimed pairs are untouched.
  fn commit_choice_assignment(
    &mut self,
    movable: &[usize],
    claims: &[MemberClaim<'a>],
    candidates: &[(String, Value)],
    pair_owners: &[Option<usize>],
  ) -> std::result::Result<bool, Error> {
    let mut assigned = vec![None; claims.len()];
    for (pair_slot, owner) in pair_owners.iter().enumerate() {
      if let Some(claim_slot) = owner {
        assigned[*claim_slot] = Some(pair_slot);
      }
    }

    let old_keys: HashSet<&str> = claims
      .iter()
      .map(|claim| claim.key.as_str())
      .filter(|key| !key.is_empty())
      .collect();
    let keys = self.validated_keys.get_or_insert_with(Vec::new);
    keys.retain(|key| !old_keys.contains(&key.as_str()));
    let mut reassigned: Vec<(usize, String)> = Vec::new();
    let mut new_claim: Option<MemberClaim<'a>> = None;
    for (claim_slot, pair_slot) in assigned.into_iter().enumerate() {
      let Some(pair_slot) = pair_slot else {
        return Ok(false);
      };
      let key = candidates[pair_slot].0.clone();
      keys.push(key.clone());
      // Retain every assigned claim, including the member that just became
      // satisfiable, so later reassignments can move its pair again.
      if claim_slot < movable.len() {
        reassigned.push((movable[claim_slot], key));
      } else {
        let mut claim = claims[claim_slot].clone();
        claim.key = key;
        new_claim = Some(claim);
      }
    }
    for (index, key) in reassigned {
      self.member_claims[index].key = key;
    }
    if let Some(claim) = new_claim {
      self.member_claims.push(claim);
    }
    Ok(true)
  }

  fn object_key_matches_alternatives(
    &self,
    key: &str,
    alternatives: &[Type1<'a>],
  ) -> std::result::Result<bool, Error> {
    for alternative in alternatives {
      let mut probe = self.new_with_recursion_state(Value::String(key.to_string()));
      probe.state.data_location = key_probe_location(&self.state.data_location, &key);
      probe.visit_type1(alternative)?;
      if probe.errors.is_empty() {
        return Ok(true);
      }
    }
    Ok(false)
  }

  fn object_value_errors(
    &self,
    entry_type: &Type<'a>,
    key: &str,
    value: &Value,
  ) -> std::result::Result<Vec<ValidationError>, Error> {
    let mut jv = self.new_with_recursion_state(value.clone());
    jv.state.is_multi_type_choice = self.state.is_multi_type_choice;
    jv.state.is_multi_group_choice = self.state.is_multi_group_choice;
    jv.state.type_group_name_entry = self.state.type_group_name_entry;
    jv.state.data_location = format!("{}/{}", self.state.data_location, key);
    jv.visit_type(entry_type)?;
    Ok(jv.errors)
  }
  /// Validate every definition contributing to a named type choice from one
  /// error checkpoint. Failed arms are tentative; their errors are retained
  /// only when every arm fails.
  fn visit_named_type_choice(&mut self, ident: &Identifier<'a>) -> visitor::Result<Error> {
    let choices = type_choice_types_from_ident(self.state.cddl, ident);

    if choices.len() > 1 {
      self.state.is_multi_type_choice = true;

      if self.json.is_array() {
        self.state.is_multi_type_choice_type_rule_validating_array = true;
      }

      let error_count = self.errors.len();
      for choice in choices {
        let current_error_count = self.errors.len();
        self.visit_type(choice)?;
        if self.errors.len() == current_error_count {
          self.errors.truncate(error_count);
          return Ok(());
        }
      }

      return Ok(());
    }

    if let Some(choice) = choices.first() {
      if choice.type_choices.len() > 1 && self.json.is_array() {
        self.state.is_multi_type_choice_type_rule_validating_array = true;
      }

      return self.visit_type(choice);
    }

    Ok(())
  }

  fn validate_equality_control(
    &mut self,
    target: &Type2<'a>,
    controller: &Type2<'a>,
  ) -> visitor::Result<Error> {
    match target {
      Type2::Typename { ident, .. } => {
        if is_ident_string_data_type(self.state.cddl, ident)
          || is_ident_numeric_data_type(self.state.cddl, ident)
        {
          return self.visit_type2(controller);
        }
      }
      Type2::Array { .. } => {
        if let Value::Array(_) = self.json.as_ref() {
          self.visit_type2(controller)?;
          return Ok(());
        }
      }
      Type2::Map { .. } => {
        if let Value::Object(_) = self.json.as_ref() {
          self.state.ctrl = Some(ControlOperator::EQ);
          self.state.is_ctrl_map_equality = true;
          self.visit_type2(controller)?;
          self.state.ctrl = None;
          self.state.is_ctrl_map_equality = false;
          return Ok(());
        }
      }
      _ => self.add_error(format!(
        "target for .eq operator must be a string, numerical, array or map data type, got {}",
        target
      )),
    }
    Ok(())
  }

  /// The value of an object key no member has claimed yet. A key held by a
  /// movable claim is released when the claims can move to other pairs.
  fn unclaimed_object_value(&mut self, key: &str) -> std::result::Result<Option<Value>, Error> {
    let Value::Object(o) = self.json.as_ref() else {
      return Ok(None);
    };
    let Some(value) = o.get(key).cloned() else {
      return Ok(None);
    };
    let claimed = self
      .validated_keys
      .as_ref()
      .is_some_and(|keys| keys.iter().any(|claimed| claimed == key));
    if claimed && !self.release_claimed_key(key)? {
      return Ok(None);
    }
    Ok(Some(value))
  }

  fn validate_object_value(&mut self, value: &token::Value<'a>) -> visitor::Result<Error> {
    if self.json.is_object() {
      // Bareword member keys are converted to text string values
      if let token::Value::TEXT(t) = value {
        if self.state.is_cut_present {
          self.cut_value = Some(t.clone());
        }

        if *t == "any" {
          return Ok(());
        }

        // Retrieve the value from key unless optional/zero or more, in which
        // case advance to next group entry
        #[cfg(feature = "ast-span")]
        if let Some(v) = self.unclaimed_object_value(t.as_ref())? {
          self
            .validated_keys
            .get_or_insert_with(Vec::new)
            .push(t.to_string());
          self.object_value = Some(v);
          let _ = write!(self.state.data_location, "/{}", t);

          return Ok(());
        } else if let Some(Occur::Optional { .. }) | Some(Occur::ZeroOrMore { .. }) =
          &self.state.occurrence.take()
        {
          self.state.advance_to_next_entry = true;
          return Ok(());
        } else if let Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT) = &self.state.ctrl
        {
          return Ok(());
        } else {
          self.add_error(format!("object missing key: {}", t))
        }

        // Retrieve the value from key unless optional/zero or more, in which
        // case advance to next group entry
        #[cfg(not(feature = "ast-span"))]
        if let Some(v) = self.unclaimed_object_value(t.as_ref())? {
          self
            .validated_keys
            .get_or_insert_with(Vec::new)
            .push(t.to_string());
          self.object_value = Some(v);
          self.state.data_location.push_str(&format!("/{}", t));

          return Ok(());
        } else if let Some(Occur::Optional {}) | Some(Occur::ZeroOrMore {}) =
          &self.state.occurrence.take()
        {
          self.state.advance_to_next_entry = true;
          return Ok(());
        } else if let Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT) = &self.state.ctrl
        {
          return Ok(());
        } else {
          self.add_error(format!("object missing key: {}", t))
        }
      } else {
        self.add_error(format!(
          "CDDL member key must be string data type. got {}",
          value
        ))
      }
    }

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  fn validate_b64_control(
    &mut self,
    bytes: &[u8],
    is_classic: bool,
    is_sloppy: bool,
  ) -> visitor::Result<Error> {
    if let Value::String(s) = self.json.as_ref() {
      // Try strict decoding first
      let decoded_result = if is_classic {
        data_encoding::BASE64.decode(s.as_bytes())
      } else {
        data_encoding::BASE64URL_NOPAD.decode(s.as_bytes())
      };

      match decoded_result {
        Ok(decoded_bytes) => {
          if decoded_bytes == bytes {
            // Validation succeeded
          } else {
            let expected_encoding = if is_classic {
              data_encoding::BASE64.encode(bytes)
            } else {
              data_encoding::BASE64URL_NOPAD.encode(bytes)
            };
            self.add_error(format!(
              "string \"{}\" does not match expected base64 encoding \"{}\"",
              s, expected_encoding
            ));
          }
        }
        Err(e) => {
          if is_sloppy {
            // RFC 9741: Sloppy mode - try to decode without checking trailing bits
            let cleaned = s.trim_end_matches('=');
            let sloppy_result = if is_classic {
              data_encoding::BASE64_NOPAD.decode(cleaned.as_bytes())
            } else {
              data_encoding::BASE64URL_NOPAD.decode(cleaned.as_bytes())
            };
            match sloppy_result {
              Ok(decoded_bytes) => {
                if decoded_bytes != bytes {
                  let expected_encoding = if is_classic {
                    data_encoding::BASE64.encode(bytes)
                  } else {
                    data_encoding::BASE64URL_NOPAD.encode(bytes)
                  };
                  self.add_error(format!(
                    "string \"{}\" does not match expected base64 encoding \"{}\"",
                    s, expected_encoding
                  ));
                }
              }
              Err(_) => {
                self.add_error(format!("invalid base64 encoding: {}", e));
              }
            }
          } else {
            self.add_error(format!("invalid base64 encoding: {}", e));
          }
        }
      }
    } else {
      self.add_error(format!(
        "expected string for base64 validation, got {}",
        self.json
      ));
    }
    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  fn validate_hex_control(&mut self, bytes: &[u8], is_lowercase: bool) -> visitor::Result<Error> {
    if let Value::String(s) = self.json.as_ref() {
      match hex::decode(s) {
        Ok(decoded_bytes) => {
          if decoded_bytes == bytes {
            // Validation succeeded
          } else {
            let expected_encoding = if is_lowercase {
              hex::encode(bytes)
            } else {
              hex::encode_upper(bytes)
            };
            self.add_error(format!(
              "string \"{}\" does not match expected hex encoding \"{}\"",
              s, expected_encoding
            ));
          }
        }
        Err(e) => {
          self.add_error(format!("invalid hex encoding: {}", e));
        }
      }
    } else {
      self.add_error(format!(
        "expected string for hex validation, got {}",
        self.json
      ));
    }
    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  /// Substitute generic parameters in a Type2 expression.
  ///
  /// When validating generic rules, generic parameters need to be replaced with
  /// their actual argument values before control operations are evaluated.
  ///
  /// For example, given:
  /// ```cddl
  /// interval<BASE> = ( "test" => BASE .plus a )
  /// rect = { interval<X> }
  /// ```
  /// When validating rect with X=0, this function substitutes BASE with X (which resolves to 0).
  ///
  /// # Arguments
  /// * `t2` - The Type2 expression to check for generic parameters
  ///
  /// # Returns
  /// * `Some(Type2)` - The substituted Type2 if the expression is a generic parameter
  /// * `None` - If the expression is not a generic parameter or no substitution is available
  fn substitute_generic_param(&self, t2: &Type2<'a>) -> Option<Type2<'a>> {
    if let Type2::Typename { ident, .. } = t2 {
      // Check if this typename matches a generic parameter in any of our generic rules
      for gr in self.state.generic_rules.iter() {
        for (idx, param) in gr.params.iter().enumerate() {
          if ident.ident == *param {
            // Found a match! Return the corresponding argument
            if let Some(arg) = gr.args.get(idx) {
              return Some(arg.type2.clone());
            }
          }
        }
      }
    }
    None
  }

  // ---------------------------------------------------------------------
  // Array sequence matcher (RFC 8610 Appendix A PEG semantics).
  //
  // Mirror of the matcher in src/validator/cbor.rs; see the comments there.
  // Matching a group against an array is sequence matching over the array's
  // elements with a cursor:
  // - a leaf entry consumes exactly one element
  // - a group (inline, group-rule reference or unwrap) matches a subsequence,
  // - an occurrence indicator greedily quantifies its entry (no backtracking out of a repetition)
  // - "//" is prioritized choice that locks in the first successful alternative.
  // Leaf checks delegate to the existing
  // single-value machinery via a child validator.
  //
  // All matcher functions return
  // - Ok(Some(new_cursor)) on a match
  // - Ok(None) on a (silent, speculative) mismatch
  // - Err only for fatal errors that abort validation entirely.
  // ---------------------------------------------------------------------

  /// Match a group against elems[cursor..]:
  /// prioritized choice over the group's choices
  fn seq_match_group(
    &mut self,
    group: &Group<'a>,
    elems: &[Value],
    cursor: usize,
    ctx: &mut ArraySeqCtx<'a, ValidationError>,
  ) -> std::result::Result<Option<usize>, Error> {
    for gc in group.group_choices.iter() {
      if let Some(end) = self.seq_match_group_choice(gc, elems, cursor, ctx)? {
        return Ok(Some(end));
      }
    }

    Ok(None)
  }

  /// Match a single group choice:
  /// fold its entries over the cursor from left to right
  fn seq_match_group_choice(
    &mut self,
    gc: &GroupChoice<'a>,
    elems: &[Value],
    cursor: usize,
    ctx: &mut ArraySeqCtx<'a, ValidationError>,
  ) -> std::result::Result<Option<usize>, Error> {
    let mut cur = cursor;
    for (entry, _) in gc.group_entries.iter() {
      match self.seq_match_entry(entry, elems, cur, ctx)? {
        Some(next) => cur = next,
        None => return Ok(None),
      }
    }

    Ok(Some(cur))
  }

  /// Match a group entry, applying its occurrence indicator: greedily match
  /// up to the upper bound of iterations, where a failed iteration rewinds
  /// only itself, then require the lower bound
  fn seq_match_entry(
    &mut self,
    entry: &GroupEntry<'a>,
    elems: &[Value],
    cursor: usize,
    ctx: &mut ArraySeqCtx<'a, ValidationError>,
  ) -> std::result::Result<Option<usize>, Error> {
    let occur = match entry {
      GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref().map(|o| o.occur),
      GroupEntry::TypeGroupname { ge, .. } => ge.occur.as_ref().map(|o| o.occur),
      GroupEntry::InlineGroup { occur, .. } => occur.as_ref().map(|o| o.occur),
    };

    let (min, max) = match occur {
      None => (1, Some(1)),
      Some(Occur::Optional { .. }) => (0, Some(1)),
      Some(Occur::ZeroOrMore { .. }) => (0, None),
      Some(Occur::OneOrMore { .. }) => (1, None),
      Some(Occur::Exact { lower, upper, .. }) => (lower.unwrap_or(0), upper),
    };

    let mut cur = cursor;
    let mut count = 0usize;
    while max.is_none_or(|m| count < m) {
      match self.seq_match_entry_once(entry, elems, cur, ctx)? {
        Some(next) => {
          count += 1;
          if next == cur {
            // Zero-width iteration: no further progress is possible, and any
            // remaining minimum is trivially satisfiable by more zero-width
            // matches. Stop to guarantee termination (e.g. [* ()]).
            count = count.max(min);
            break;
          }
          cur = next;
        }
        None => break,
      }
    }

    if count >= min {
      Ok(Some(cur))
    } else {
      Ok(None)
    }
  }

  /// Match a single iteration of a group entry, ignoring its occurrence
  /// indicator.
  ///
  /// Subsequences (inline groups, group-rule references, unwraps
  /// of array/map rules) recurse into the matcher;
  ///
  /// everything else is a leaf that consumes exactly one element
  /// validated with the single-value machinery.
  fn seq_match_entry_once(
    &mut self,
    entry: &GroupEntry<'a>,
    elems: &[Value],
    cursor: usize,
    ctx: &mut ArraySeqCtx<'a, ValidationError>,
  ) -> std::result::Result<Option<usize>, Error> {
    match entry {
      GroupEntry::InlineGroup { group, .. } => self.seq_match_group(group, elems, cursor, ctx),
      GroupEntry::TypeGroupname { ge, .. } => {
        let grule = group_rule_from_ident(self.state.cddl, &ge.name);
        let alternates = group_choice_alternates_from_ident(self.state.cddl, &ge.name);
        if grule.is_some() || !alternates.is_empty() {
          return self.seq_match_group_ref(ge, grule, &alternates, elems, cursor, ctx);
        }

        // Leaf: validate one element against the named type
        self.seq_match_leaf(elems, cursor, ctx, |jv| {
          if let Some(ga) = &ge.generic_args {
            if let Some(rule) = rule_from_ident(jv.state.cddl, &ge.name) {
              let resolved_args = resolved_generic_args(&jv.state, ga);
              if let Some(gr) = jv
                .state
                .generic_rules
                .iter_mut()
                .find(|gr| gr.name == ge.name.ident)
              {
                gr.args.extend(resolved_args);
              } else if let Some(params) = generic_params_from_rule(rule) {
                jv.state.generic_rules.push(GenericRule {
                  name: ge.name.ident,
                  params,
                  args: resolved_args,
                });
              }

              jv.state.eval_generic_rule = Some(ge.name.ident);
              return jv.visit_rule(rule);
            }
          }

          jv.visit_identifier(&ge.name)
        })
      }
      GroupEntry::ValueMemberKey { ge, .. } => {
        // RFC 8610 3.7: an unwrapped array/map rule contributes the
        // referenced rule's group as a subsequence, not as a single element
        // (a member key, being annotation-only in an array context, does not
        // change this)
        if ge.entry_type.type_choices.len() == 1 {
          let tc = &ge.entry_type.type_choices[0];
          if tc.type1.operator.is_none() {
            if let Type2::Unwrap {
              ident,
              generic_args,
              ..
            } = &tc.type1.type2
            {
              if let Some(rule) = unwrap_rule_from_ident(self.state.cddl, ident) {
                if let Rule::Type { rule: trule, .. } = rule {
                  if trule
                    .value
                    .type_choices
                    .iter()
                    .any(|tc| matches!(tc.type1.type2, Type2::Array { .. } | Type2::Map { .. }))
                  {
                    return self.seq_match_unwrap(
                      ident,
                      generic_args.as_ref(),
                      rule,
                      elems,
                      cursor,
                      ctx,
                    );
                  }
                }
              }
            }
          }
        }

        // Leaf: member keys are annotation-only in an array context;
        // validate one element against the entry type
        self.seq_match_leaf(elems, cursor, ctx, |jv| jv.visit_type(&ge.entry_type))
      }
    }
  }

  /// Match a group-rule reference (possibly generic, possibly with "//="
  /// alternates) as a subsequence
  fn seq_match_group_ref(
    &mut self,
    ge: &TypeGroupnameEntry<'a>,
    grule: Option<&'a GroupRule<'a>>,
    alternates: &[&'a GroupEntry<'a>],
    elems: &[Value],
    cursor: usize,
    ctx: &mut ArraySeqCtx<'a, ValidationError>,
  ) -> std::result::Result<Option<usize>, Error> {
    let key = (ge.name.ident, cursor);
    if ctx.active_group_refs.contains(&key) {
      // Recursive group reference without progress cannot match
      return Ok(None);
    }
    ctx.active_group_refs.push(key);

    // Snapshot generic state so args registered for this (possibly failing,
    // speculative) descent don't leak into sibling alternatives or later
    // iterations
    let prev_eval = self.state.eval_generic_rule;
    let prev_generic_rules = ge
      .generic_args
      .as_ref()
      .map(|_| self.state.generic_rules.clone());
    if let Some(ga) = &ge.generic_args {
      if let Some(rule) = rule_from_ident(self.state.cddl, &ge.name) {
        let resolved_args = resolved_generic_args(&self.state, ga);
        if let Some(gr) = self
          .state
          .generic_rules
          .iter_mut()
          .find(|gr| gr.name == ge.name.ident)
        {
          gr.args.extend(resolved_args);
        } else if let Some(params) = generic_params_from_rule(rule) {
          self.state.generic_rules.push(GenericRule {
            name: ge.name.ident,
            params,
            args: resolved_args,
          });
        }

        self.state.eval_generic_rule = Some(ge.name.ident);
      }
    }

    let mut result = None;
    if let Some(grule) = grule {
      result = self.seq_match_entry(&grule.entry, elems, cursor, ctx)?;
    }
    if result.is_none() {
      for alt in alternates {
        result = self.seq_match_entry(alt, elems, cursor, ctx)?;
        if result.is_some() {
          break;
        }
      }
    }

    self.state.eval_generic_rule = prev_eval;
    if let Some(prev) = prev_generic_rules {
      self.state.generic_rules = prev;
    }
    ctx.active_group_refs.pop();

    Ok(result)
  }

  /// Match an unwrapped rule (~rule, RFC 8610 3.7) as a subsequence: the
  /// group inside the referenced array/map rule is matched in place
  fn seq_match_unwrap(
    &mut self,
    ident: &Identifier<'a>,
    generic_args: Option<&GenericArgs<'a>>,
    rule: &'a Rule<'a>,
    elems: &[Value],
    cursor: usize,
    ctx: &mut ArraySeqCtx<'a, ValidationError>,
  ) -> std::result::Result<Option<usize>, Error> {
    // Guard against recursive unwrap references that would loop without
    // consuming elements (e.g. a = [~b], b = [~b])
    let key = (ident.ident, cursor);
    if ctx.active_group_refs.contains(&key) {
      return Ok(None);
    }
    ctx.active_group_refs.push(key);

    // Snapshot generic state so args registered for this (possibly failing,
    // speculative) descent don't leak into sibling alternatives or later
    // iterations
    let prev_eval = self.state.eval_generic_rule;
    let prev_generic_rules = generic_args.map(|_| self.state.generic_rules.clone());
    if let Some(ga) = generic_args {
      let resolved_args = resolved_generic_args(&self.state, ga);
      if let Some(gr) = self
        .state
        .generic_rules
        .iter_mut()
        .find(|gr| gr.name == ident.ident)
      {
        gr.args.extend(resolved_args);
      } else if let Some(params) = generic_params_from_rule(rule) {
        self.state.generic_rules.push(GenericRule {
          name: ident.ident,
          params,
          args: resolved_args,
        });
      }

      self.state.eval_generic_rule = Some(ident.ident);
    }

    let mut result = None;
    if let Rule::Type { rule: trule, .. } = rule {
      for tc in trule.value.type_choices.iter() {
        if let Type2::Array { group, .. } | Type2::Map { group, .. } = &tc.type1.type2 {
          result = self.seq_match_group(group, elems, cursor, ctx)?;
          if result.is_some() {
            break;
          }
        }
      }
    }

    self.state.eval_generic_rule = prev_eval;
    if let Some(prev) = prev_generic_rules {
      self.state.generic_rules = prev;
    }
    ctx.active_group_refs.pop();

    Ok(result)
  }

  /// Validate elems[cursor] against a leaf entry by spawning a child
  /// validator on the element value. Failures are silent (speculative):
  /// the child's errors are recorded in the context for later reporting
  /// but never bubble into self.errors here.
  fn seq_match_leaf<F>(
    &mut self,
    elems: &[Value],
    cursor: usize,
    ctx: &mut ArraySeqCtx<'a, ValidationError>,
    f: F,
  ) -> std::result::Result<Option<usize>, Error>
  where
    F: FnOnce(&mut JSONValidator<'a>) -> visitor::Result<Error>,
  {
    let value = match elems.get(cursor) {
      Some(v) => v.clone(),
      // Ran out of elements
      None => return Ok(None),
    };

    let mut jv = self.new_with_recursion_state(value);
    jv.state.ctrl = self.state.ctrl;
    let _ = write!(
      jv.state.data_location,
      "{}/{}",
      self.state.data_location, cursor
    );

    f(&mut jv)?;

    if jv.errors.is_empty() {
      Ok(Some(cursor + 1))
    } else {
      ctx.note_failure(cursor, jv.errors);
      Ok(None)
    }
  }
}

impl<'a> Validator<'a, '_, Error> for JSONValidator<'a> {
  /// Validate
  fn validate(&mut self) -> std::result::Result<(), Error> {
    self.reset_for_validation();
    for r in self.state.cddl.rules.iter() {
      // First type rule is root
      if let Rule::Type { rule, .. } = r {
        if rule.generic_params.is_none() {
          self.state.is_root = true;
          let result = self.visit_type_rule(rule);
          self.state.is_root = false;
          result?;
          break;
        }
      }
    }

    if !self.errors.is_empty() {
      return Err(Error::Validation(self.errors.clone()));
    }

    Ok(())
  }

  fn add_error(&mut self, reason: String) {
    self.errors.push(ValidationError {
      reason,
      cddl_location: self.state.cddl_location.clone(),
      json_location: self.state.data_location.clone(),
      is_multi_type_choice: self.state.is_multi_type_choice,
      is_multi_group_choice: self.state.is_multi_group_choice,
      is_group_to_choice_enum: self.state.is_group_to_choice_enum,
      type_group_name_entry: self.state.type_group_name_entry.map(|e| e.to_string()),
    });
  }
}

impl<'a> Visitor<'a, '_, Error> for JSONValidator<'a> {
  fn visit_type_rule(&mut self, tr: &TypeRule<'a>) -> visitor::Result<Error> {
    if let Some(gp) = &tr.generic_params {
      if let Some(gr) = self
        .state
        .generic_rules
        .iter_mut()
        .find(|r| r.name == tr.name.ident)
      {
        gr.params = gp.params.iter().map(|p| p.param.ident).collect();
      } else {
        self.state.generic_rules.push(GenericRule {
          name: tr.name.ident,
          params: gp.params.iter().map(|p| p.param.ident).collect(),
          args: vec![],
        });
      }
    }

    self.visit_named_type_choice(&tr.name)
  }

  fn visit_group_rule(&mut self, gr: &GroupRule<'a>) -> visitor::Result<Error> {
    if let Some(gp) = &gr.generic_params {
      if let Some(gr) = self
        .state
        .generic_rules
        .iter_mut()
        .find(|r| r.name == gr.name.ident)
      {
        gr.params = gp.params.iter().map(|p| p.param.ident).collect();
      } else {
        self.state.generic_rules.push(GenericRule {
          name: gr.name.ident,
          params: gp.params.iter().map(|p| p.param.ident).collect(),
          args: vec![],
        });
      }
    }

    let group_choice_alternates = group_choice_alternates_from_ident(self.state.cddl, &gr.name);
    if !group_choice_alternates.is_empty() {
      self.state.is_multi_group_choice = true;
    }

    if let Some(selected) = self.planned_group_choice(group_choice_alternates.len() + 1)? {
      let entry = group_choice_alternates
        .get(selected)
        .copied()
        .unwrap_or(&gr.entry);
      return self.visit_group_entry(entry);
    }

    let error_count = self.errors.len();
    for ge in group_choice_alternates {
      let cur_errors = self.errors.len();
      self.visit_group_entry(ge)?;
      if self.errors.len() == cur_errors {
        for _ in 0..self.errors.len() - error_count {
          self.errors.pop();
        }

        return Ok(());
      }
    }

    self.visit_group_entry(&gr.entry)
  }

  fn visit_type(&mut self, t: &Type<'a>) -> visitor::Result<Error> {
    if t.type_choices.len() > 1 {
      self.state.is_multi_type_choice = true;
    }

    let initial_error_count = self.errors.len();
    let mut choice_validation_succeeded = false;

    for type_choice in t.type_choices.iter() {
      // If validating an array whose elements are type choices (i.e. [ 1* tstr
      // / integer ]), collect all errors and filter after the fact
      if matches!(self.json.as_ref(), Value::Array(_))
        && !self.state.is_multi_type_choice_type_rule_validating_array
      {
        let error_count = self.errors.len();

        self.visit_type_choice(type_choice)?;

        #[cfg(feature = "additional-controls")]
        if self.errors.len() == error_count
          && !self.state.has_feature_errors
          && self.state.disabled_features.is_none()
        {
          // Disregard invalid type choice validation errors if one of the
          // choices validates successfully
          let type_choice_error_count = self.errors.len() - initial_error_count;
          if type_choice_error_count > 0 {
            for _ in 0..type_choice_error_count {
              self.errors.pop();
            }
          }
          choice_validation_succeeded = true;
        }

        #[cfg(not(feature = "additional-controls"))]
        if self.errors.len() == error_count {
          // Disregard invalid type choice validation errors if one of the
          // choices validates successfully
          let type_choice_error_count = self.errors.len() - initial_error_count;
          if type_choice_error_count > 0 {
            for _ in 0..type_choice_error_count {
              self.errors.pop();
            }
          }
          choice_validation_succeeded = true;
        }

        continue;
      }

      // Create a copy of the validator to test this choice in isolation
      let mut choice_validator = self.clone();
      choice_validator.errors.clear();

      choice_validator.visit_type_choice(type_choice)?;

      // If this choice validates successfully (no errors), use it
      if choice_validator.errors.is_empty() {
        #[cfg(feature = "additional-controls")]
        if !choice_validator.state.has_feature_errors
          || choice_validator.state.disabled_features.is_some()
        {
          // Clear any accumulated errors and return success
          let type_choice_error_count = self.errors.len() - initial_error_count;
          if type_choice_error_count > 0 {
            for _ in 0..type_choice_error_count {
              self.errors.pop();
            }
          }
          return Ok(());
        }

        #[cfg(not(feature = "additional-controls"))]
        {
          // Clear any accumulated errors and return success
          let type_choice_error_count = self.errors.len() - initial_error_count;
          if type_choice_error_count > 0 {
            for _ in 0..type_choice_error_count {
              self.errors.pop();
            }
          }
          return Ok(());
        }
      } else {
        // This choice failed, accumulate its errors
        self.errors.extend(choice_validator.errors);
      }
    }

    // If we got here and choice_validation_succeeded is true (for array case),
    // then validation succeeded; drop errors accumulated by other, failing
    // alternatives so they don't leak into the overall result
    if choice_validation_succeeded {
      self.errors.truncate(initial_error_count);
      return Ok(());
    }

    Ok(())
  }

  fn visit_group(&mut self, g: &Group<'a>) -> visitor::Result<Error> {
    if g.group_choices.len() > 1 {
      self.state.is_multi_group_choice = true;
    }

    // Map equality/inequality validation
    if self.state.is_ctrl_map_equality {
      if let Some(t) = &self.state.ctrl {
        if let Value::Object(o) = self.json.as_ref() {
          let entry_counts = entry_counts_from_group(self.state.cddl, g);

          let len = o.len();
          if let ControlOperator::EQ = t {
            if !validate_entry_count(&entry_counts, len) {
              for ec in entry_counts.iter() {
                if let Some(occur) = &ec.entry_occurrence {
                  self.add_error(format!(
                    "map equality error. expected object with number of entries per occurrence {}",
                    occur,
                  ));
                } else {
                  self.add_error(format!(
                    "map equality error, expected object with length {}, got {}",
                    ec.count, len
                  ));
                }
              }
              return Ok(());
            }
          } else if let ControlOperator::NE | ControlOperator::DEFAULT = t {
            if !validate_entry_count(&entry_counts, len) {
              for ec in entry_counts.iter() {
                if let Some(occur) = &ec.entry_occurrence {
                  self.add_error(format!(
                    "map inequality error. expected object with number of entries not per occurrence {}",
                    occur,
                  ));
                } else {
                  self.add_error(format!(
                    "map inequality error, expected object not with length {}, got {}",
                    ec.count, len
                  ));
                }
              }
              return Ok(());
            }
          }
        }
      }
    }

    self.state.is_ctrl_map_equality = false;

    if let Some(selected) = self.planned_concrete_group_choice(g)? {
      return self.visit_selected_group_choice(g, selected);
    }

    let initial_error_count = self.errors.len();
    for group_choice in g.group_choices.iter() {
      let error_count = self.errors.len();
      // A failed arm must not leave its pair ownership behind for the next:
      // reassignment may also have moved existing claims, so restore the
      // complete vector, not just the tail.
      let validated_checkpoint = self.validated_keys.clone();
      let claims_checkpoint = self.member_claims.clone();
      self.visit_group_choice(group_choice)?;
      if self.errors.len() == error_count {
        // Disregard invalid group choice validation errors if one of the
        // choices validates successfully
        let group_choice_error_count = self.errors.len() - initial_error_count;
        if group_choice_error_count > 0 {
          for _ in 0..group_choice_error_count {
            self.errors.pop();
          }
        }

        return Ok(());
      }
      self.validated_keys = validated_checkpoint;
      self.member_claims = claims_checkpoint;
    }

    Ok(())
  }

  fn visit_group_choice(&mut self, gc: &GroupChoice<'a>) -> visitor::Result<Error> {
    if self.state.is_group_to_choice_enum {
      let initial_error_count = self.errors.len();
      for tc in type_choices_from_group_choice(self.state.cddl, gc).iter() {
        let error_count = self.errors.len();
        let prev_is_group_to_choice_enum = self.state.is_group_to_choice_enum;
        // The group has been converted to type choices.
        // Validate each type normally so its nested groups are not converted again.
        self.state.is_group_to_choice_enum = false;
        let mut result = self.visit_type_choice(tc);
        for error in &mut self.errors[error_count..] {
          error.is_group_to_choice_enum = true;
        }
        if let Err(Error::Validation(errors)) = &mut result {
          for error in errors {
            error.is_group_to_choice_enum = true;
          }
        }
        // Restore enumeration mode for the remaining choices, even on errors.
        self.state.is_group_to_choice_enum = prev_is_group_to_choice_enum;
        result?;
        if self.errors.len() == error_count {
          let type_choice_error_count = self.errors.len() - initial_error_count;
          if type_choice_error_count > 0 {
            for _ in 0..type_choice_error_count {
              self.errors.pop();
            }
          }
          return Ok(());
        }
      }

      return Ok(());
    }

    self.visit_group_sequence(gc)
  }

  fn visit_range(
    &mut self,
    lower: &Type2,
    upper: &Type2,
    is_inclusive: bool,
  ) -> visitor::Result<Error> {
    if matches!(self.json.as_ref(), Value::Array(_)) {
      return self.validate_array_items(&ArrayItemToken::Range(lower, upper, is_inclusive));
    }

    match (lower, upper) {
      (Type2::IntValue { value: l, .. }, Type2::IntValue { value: u, .. }) => {
        let error_str = if is_inclusive {
          format!(
            "expected integer to be in range {} <= value <= {}, got {}",
            l, u, self.json
          )
        } else {
          // Changed: For ... ranges, make it inclusive on lower bound, exclusive on upper bound
          format!(
            "expected integer to be in range {} <= value < {}, got {}",
            l, u, self.json
          )
        };

        match self.json.as_ref() {
          Value::Number(n) => {
            if let Some(i) = n.as_i64() {
              if is_inclusive {
                if i < *l as i64 || i > *u as i64 {
                  self.add_error(error_str);
                }
              } else {
                // Changed: For ... ranges, make it inclusive on lower bound, exclusive on upper bound
                if i < *l as i64 || i >= *u as i64 {
                  self.add_error(error_str);
                }
              }
            } else {
              self.add_error(error_str);
              return Ok(());
            }
          }
          _ => {
            self.add_error(format!(
              "invalid cddl range. value must be an integer type. got {}",
              self.json
            ));
            return Ok(());
          }
        }
      }
      (Type2::UintValue { value: l, .. }, Type2::UintValue { value: u, .. }) => {
        let error_str = if is_inclusive {
          format!(
            "expected uint to be in range {} <= value <= {}, got {}",
            l, u, self.json
          )
        } else {
          // Changed: For ... ranges, make it inclusive on lower bound, exclusive on upper bound
          format!(
            "expected uint to be in range {} <= value < {}, got {}",
            l, u, self.json
          )
        };

        match self.json.as_ref() {
          Value::Number(n) => {
            if let Some(i) = n.as_u64() {
              // Fix: Cast all usize values to u64
              let lower = *l as u64;
              let upper = *u as u64;
              if is_inclusive {
                if i < lower || i > upper {
                  self.add_error(error_str);
                }
              } else if i < lower || i >= upper {
                self.add_error(error_str);
              }
            } else {
              self.add_error(error_str);
              return Ok(());
            }
          }
          Value::String(s) => match self.state.ctrl {
            Some(ControlOperator::SIZE) => {
              let len = s.len() as u64; // Fix: Convert len to u64
                                        // Fix: Cast all usize values to u64
              let lower = *l as u64;
              let upper = *u as u64;
              if is_inclusive {
                if len < lower || len > upper {
                  self.add_error(error_str);
                }
              } else if len < lower || len >= upper {
                self.add_error(error_str);
              }
            }
            _ => {
              self.add_error("string value cannot be validated against a range without the .size control operator".to_string());
              return Ok(());
            }
          },
          _ => {
            self.add_error(format!(
              "invalid cddl range. value must be a uint type. got {}",
              self.json
            ));
            return Ok(());
          }
        }
      }
      (Type2::FloatValue { value: l, .. }, Type2::FloatValue { value: u, .. }) => {
        let error_str = if is_inclusive {
          format!(
            "expected float to be in range {} <= value <= {}, got {}",
            l, u, self.json
          )
        } else {
          // Changed: For ... ranges, make it inclusive on lower bound, exclusive on upper bound
          format!(
            "expected float to be in range {} <= value < {}, got {}",
            l, u, self.json
          )
        };

        match self.json.as_ref() {
          Value::Number(n) => {
            if let Some(f) = n.as_f64() {
              if is_inclusive {
                if f < *l || f > *u {
                  self.add_error(error_str);
                }
              } else {
                // Changed: For ... ranges, make it inclusive on lower bound, exclusive on upper bound
                if f < *l || f >= *u {
                  self.add_error(error_str);
                }
              }
            } else {
              self.add_error(error_str);
              return Ok(());
            }
          }
          _ => {
            self.add_error(format!(
              "invalid cddl range. value must be a float type. got {}",
              self.json
            ));
            return Ok(());
          }
        }
      }
      _ => {
        self.add_error(
          "invalid cddl range. upper and lower values must be either integers or floats"
            .to_string(),
        );
        return Ok(());
      }
    }

    Ok(())
  }

  fn visit_control_operator(
    &mut self,
    target: &Type2<'a>,
    ctrl: ControlOperator,
    controller: &Type2<'a>,
  ) -> visitor::Result<Error> {
    if let Type2::Typename {
      ident: target_ident,
      ..
    } = target
    {
      if let Type2::Typename {
        ident: controller_ident,
        ..
      } = controller
      {
        if let Some(name) = self.state.eval_generic_rule {
          if let Some(gr) = self
            .state
            .generic_rules
            .iter()
            .find(|r| r.name == name)
            .cloned()
          {
            for (idx, gp) in gr.params.iter().enumerate() {
              if let Some(arg) = gr.args.get(idx) {
                if *gp == target_ident.ident {
                  let t2 = Type2::from(arg.clone());

                  if *gp == controller_ident.ident {
                    return self.visit_control_operator(&t2, ctrl, &t2);
                  }

                  return self.visit_control_operator(&arg.type2, ctrl, controller);
                }
              }
            }
          }
        }
      }

      if let Some(name) = self.state.eval_generic_rule {
        if let Some(gr) = self
          .state
          .generic_rules
          .iter()
          .find(|r| r.name == name)
          .cloned()
        {
          for (idx, gp) in gr.params.iter().enumerate() {
            if let Some(arg) = gr.args.get(idx) {
              if *gp == target_ident.ident {
                let t2 = Type2::from(arg.clone());
                return self.visit_control_operator(&t2, ctrl, controller);
              }
            }
          }
        }
      }
    }

    match ctrl {
      ControlOperator::EQ => return self.validate_equality_control(target, controller),
      ControlOperator::NE => match target {
        Type2::Typename { ident, .. } => {
          if is_ident_string_data_type(self.state.cddl, ident)
            || is_ident_numeric_data_type(self.state.cddl, ident)
          {
            self.state.ctrl = Some(ctrl);
            self.visit_type2(controller)?;
            self.state.ctrl = None;
            return Ok(());
          }
        }
        Type2::Array { .. } => {
          if let Value::Array(_) = self.json.as_ref() {
            self.state.ctrl = Some(ctrl);
            self.visit_type2(controller)?;
            self.state.ctrl = None;
            return Ok(());
          }
        }
        Type2::Map { .. } => {
          if let Value::Object(_) = self.json.as_ref() {
            self.state.ctrl = Some(ctrl);
            self.state.is_ctrl_map_equality = true;
            self.visit_type2(controller)?;
            self.state.ctrl = None;
            self.state.is_ctrl_map_equality = false;
            return Ok(());
          }
        }
        _ => self.add_error(format!(
          "target for .ne operator must be a string, numerical, array or map data type, got {}",
          target
        )),
      },
      ControlOperator::LT | ControlOperator::GT | ControlOperator::GE | ControlOperator::LE => {
        match target {
          Type2::Typename { ident, .. } if is_ident_numeric_data_type(self.state.cddl, ident) => {
            self.state.ctrl = Some(ctrl);
            self.visit_type2(controller)?;
            self.state.ctrl = None;
          }
          _ => {
            self.add_error(format!(
              "target for .lt, .gt, .ge or .le operator must be a numerical data type, got {}",
              target
            ));
          }
        }
      }
      ControlOperator::SIZE => match target {
        Type2::Typename { ident, .. }
          if is_ident_string_data_type(self.state.cddl, ident)
            || is_ident_uint_data_type(self.state.cddl, ident) =>
        {
          self.state.ctrl = Some(ctrl);
          self.visit_type2(controller)?;
          self.state.ctrl = None;
        }
        _ => {
          self.add_error(format!(
            "target for .size must a string or uint data type, got {}",
            target
          ));
        }
      },
      ControlOperator::AND => {
        self.state.ctrl = Some(ctrl);
        self.visit_type2(target)?;
        self.visit_type2(controller)?;
        self.state.ctrl = None;
      }
      ControlOperator::WITHIN => {
        self.state.ctrl = Some(ctrl);
        let error_count = self.errors.len();
        self.visit_type2(target)?;
        let no_errors = self.errors.len() == error_count;
        self.visit_type2(controller)?;
        if no_errors && self.errors.len() > error_count {
          for _ in 0..self.errors.len() - error_count {
            self.errors.pop();
          }

          self.add_error(format!(
            "expected type {} .within type {}, got {}",
            target, controller, self.json,
          ));
        }

        self.state.ctrl = None;
      }
      ControlOperator::DEFAULT => {
        self.state.ctrl = Some(ctrl);
        let error_count = self.errors.len();
        self.visit_type2(target)?;
        if self.errors.len() != error_count {
          if let Some(Occur::Optional { .. }) = self.state.occurrence.as_ref() {
            self.add_error(format!(
              "expected default value {}, got {}",
              controller, self.json
            ));
          }
        }
        self.state.ctrl = None;
      }
      ControlOperator::REGEXP => {
        self.state.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
            match *self.json {
              Value::String(_) | Value::Array(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".regexp control can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".regexp control can only be matched against string data type, got {}",
            target
          )),
        }
        self.state.ctrl = None;
      }
      ControlOperator::PCRE => {
        self.state.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
            match *self.json {
              Value::String(_) | Value::Array(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".pcre control can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".pcre control can only be matched against string data type, got {}",
            target
          )),
        }
        self.state.ctrl = None;
      }
      #[cfg(feature = "freezer")]
      ControlOperator::IREGEXP => {
        self.state.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
            match *self.json {
              Value::String(_) | Value::Array(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".iregexp control can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".iregexp control can only be matched against string data type, got {}",
            target
          )),
        }
        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::CAT => {
        self.state.ctrl = Some(ctrl);

        match cat_operation(self.state.cddl, target, controller, false) {
          Ok(values) => {
            let error_count = self.errors.len();

            for v in values.iter() {
              let cur_errors = self.errors.len();

              self.visit_type2(v)?;

              if self.errors.len() == cur_errors {
                for _ in 0..self.errors.len() - error_count {
                  self.errors.pop();
                }

                break;
              }
            }
          }
          Err(e) => self.add_error(e),
        }

        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::DET => {
        self.state.ctrl = Some(ctrl);

        match cat_operation(self.state.cddl, target, controller, true) {
          Ok(values) => {
            let error_count = self.errors.len();

            for v in values.iter() {
              let cur_errors = self.errors.len();

              self.visit_type2(v)?;

              if self.errors.len() == cur_errors {
                for _ in 0..self.errors.len() - error_count {
                  self.errors.pop();
                }

                break;
              }
            }
          }
          Err(e) => self.add_error(e),
        }

        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::PLUS => {
        self.state.ctrl = Some(ctrl);

        // Substitute generic parameters in target and controller before calling plus_operation
        let substituted_target = self
          .substitute_generic_param(target)
          .unwrap_or_else(|| target.clone());
        let substituted_controller = self
          .substitute_generic_param(controller)
          .unwrap_or_else(|| controller.clone());

        match plus_operation(
          self.state.cddl,
          &substituted_target,
          &substituted_controller,
        ) {
          Ok(values) => {
            let error_count = self.errors.len();
            for v in values.iter() {
              let cur_errors = self.errors.len();

              self.visit_type2(v)?;
              if self.errors.len() == cur_errors {
                for _ in 0..self.errors.len() - error_count {
                  self.errors.pop();
                }

                break;
              }
            }
          }

          Err(e) => self.add_error(e),
        }

        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::ABNF => {
        self.state.ctrl = Some(ctrl);

        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
            match *self.json {
              Value::String(_) | Value::Array(_) => {
                let complex_ctrl_result = if let Type2::ParenthesizedType { pt, .. } = controller {
                  Some(abnf_from_complex_controller(self.state.cddl, pt))
                } else if let Type2::Typename { ident, .. } = controller {
                  if let Some(Rule::Type { rule, .. }) = rule_from_ident(self.state.cddl, ident) {
                    match abnf_from_complex_controller(self.state.cddl, &rule.value) {
                      Ok(values) => Some(Ok(values)),
                      Err(_) => None,
                    }
                  } else {
                    None
                  }
                } else {
                  None
                };

                if let Some(result) = complex_ctrl_result {
                  match result {
                    Ok(values) => {
                      let error_count = self.errors.len();
                      for v in values.iter() {
                        let cur_errors = self.errors.len();

                        self.visit_type2(v)?;

                        if self.errors.len() == cur_errors {
                          for _ in 0..self.errors.len() - error_count {
                            self.errors.pop();
                          }

                          break;
                        }
                      }
                    }
                    Err(e) => self.add_error(e),
                  }
                } else {
                  self.visit_type2(controller)?
                }
              }
              _ => self.add_error(format!(
                ".abnf control can only be matched against a JSON string, got {}",
                self.json,
              )),
            }
          }
          _ => self.add_error(format!(
            ".abnf can only be matched against string data type, got {}",
            target,
          )),
        }

        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      #[cfg(not(target_arch = "wasm32"))]
      ControlOperator::FEATURE => {
        self.state.ctrl = Some(ctrl);

        if let Some(ef) = self.state.enabled_features {
          let tv = text_value_from_type2(self.state.cddl, controller);
          if let Some(Type2::TextValue { value, .. }) = tv {
            if ef.contains(&&**value) {
              let err_count = self.errors.len();
              self.visit_type2(target)?;
              if self.errors.len() > err_count {
                self.state.has_feature_errors = true;
              }
              self.state.ctrl = None;
            } else {
              self
                .state
                .disabled_features
                .get_or_insert(vec![value.to_string()])
                .push(value.to_string());
            }
          } else if let Some(Type2::UTF8ByteString { value, .. }) = tv {
            let value = std::str::from_utf8(value).map_err(Error::UTF8Parsing)?;
            if ef.contains(&value) {
              let err_count = self.errors.len();
              self.visit_type2(target)?;
              if self.errors.len() > err_count {
                self.state.has_feature_errors = true;
              }
              self.state.ctrl = None;
            } else {
              self
                .state
                .disabled_features
                .get_or_insert(vec![value.to_string()])
                .push(value.to_string());
            }
          }
        }

        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      #[cfg(target_arch = "wasm32")]
      ControlOperator::FEATURE => {
        self.state.ctrl = Some(ctrl);

        if let Some(ef) = &self.state.enabled_features {
          let tv = text_value_from_type2(self.state.cddl, controller);
          if let Some(Type2::TextValue { value, .. }) = tv {
            if ef.contains(&JsValue::from(value.as_ref())) {
              let err_count = self.errors.len();
              self.visit_type2(target)?;
              if self.errors.len() > err_count {
                self.state.has_feature_errors = true;
              }
              self.state.ctrl = None;
            } else {
              self
                .state
                .disabled_features
                .get_or_insert(vec![value.to_string()])
                .push(value.to_string());
            }
          } else if let Some(Type2::UTF8ByteString { value, .. }) = tv {
            let value = std::str::from_utf8(value).map_err(Error::UTF8Parsing)?;
            if ef.contains(&JsValue::from(value)) {
              let err_count = self.errors.len();
              self.visit_type2(target)?;
              if self.errors.len() > err_count {
                self.state.has_feature_errors = true;
              }
              self.state.ctrl = None;
            } else {
              self
                .state
                .disabled_features
                .get_or_insert(vec![value.to_string()])
                .push(value.to_string());
            }
          }
        }

        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64U => {
        self.state.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
            match self.json.as_ref() {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".b64u can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64u can only be matched against string data type, got {}",
            target
          )),
        }
        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64C => {
        self.state.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
            match self.json.as_ref() {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".b64c can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64c can only be matched against string data type, got {}",
            target
          )),
        }
        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64USLOPPY => {
        self.state.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
            match self.json.as_ref() {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".b64u-sloppy can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64u-sloppy can only be matched against string data type, got {}",
            target
          )),
        }
        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B64CSLOPPY => {
        self.state.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
            match self.json.as_ref() {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".b64c-sloppy can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".b64c-sloppy can only be matched against string data type, got {}",
            target
          )),
        }
        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::HEX => {
        self.state.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
            match self.json.as_ref() {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".hex can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".hex can only be matched against string data type, got {}",
            target
          )),
        }
        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::HEXLC => {
        self.state.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
            match self.json.as_ref() {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".hexlc can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".hexlc can only be matched against string data type, got {}",
            target
          )),
        }
        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::HEXUC => {
        self.state.ctrl = Some(ctrl);
        match target {
          Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
            match self.json.as_ref() {
              Value::String(_) => self.visit_type2(controller)?,
              _ => self.add_error(format!(
                ".hexuc can only be matched against JSON string, got {}",
                self.json
              )),
            }
          }
          _ => self.add_error(format!(
            ".hexuc can only be matched against string data type, got {}",
            target
          )),
        }
        self.state.ctrl = None;
      }
      #[cfg(feature = "additional-controls")]
      ControlOperator::B32 => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
          match self.json.as_ref() {
            Value::String(s) => {
              match crate::validator::control::validate_b32_text(target, controller, s, false) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!(
                      "string \"{}\" does not match .b32 encoded bytes",
                      s
                    ));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".b32 can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".b32 can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::H32 => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
          match self.json.as_ref() {
            Value::String(s) => {
              match crate::validator::control::validate_b32_text(target, controller, s, true) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!(
                      "string \"{}\" does not match .h32 encoded bytes",
                      s
                    ));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".h32 can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".h32 can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::B45 => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
          match self.json.as_ref() {
            Value::String(s) => {
              match crate::validator::control::validate_b45_text(target, controller, s) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!(
                      "string \"{}\" does not match .b45 encoded bytes",
                      s
                    ));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".b45 can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".b45 can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::BASE10 => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
          match self.json.as_ref() {
            Value::String(s) => {
              match crate::validator::control::validate_base10_text(target, controller, s) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!(
                      "string \"{}\" does not match .base10 integer format",
                      s
                    ));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".base10 can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".base10 can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::PRINTF => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
          match self.json.as_ref() {
            Value::String(s) => {
              match crate::validator::control::validate_printf_text(target, controller, s) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!("string \"{}\" does not match .printf format", s));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".printf can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".printf can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::JSON => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
          match self.json.as_ref() {
            Value::String(s) => {
              match crate::validator::control::validate_json_text(target, controller, s) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!("string \"{}\" does not contain valid JSON", s));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".json can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".json can only be matched against string data type, got {}",
          target
        )),
      },
      #[cfg(feature = "additional-controls")]
      ControlOperator::JOIN => match target {
        Type2::Typename { ident, .. } if is_ident_string_data_type(self.state.cddl, ident) => {
          match self.json.as_ref() {
            Value::String(s) => {
              match crate::validator::control::validate_join_text(
                target,
                controller,
                s,
                Some(self.state.cddl),
              ) {
                Ok(is_valid) => {
                  if !is_valid {
                    self.add_error(format!("string \"{}\" does not match .join result", s));
                  }
                }
                Err(e) => self.add_error(e),
              }
            }
            _ => self.add_error(format!(
              ".join can only be matched against JSON string, got {}",
              self.json
            )),
          }
        }
        _ => self.add_error(format!(
          ".join can only be matched against string data type, got {}",
          target
        )),
      },
      _ => {
        self.add_error(format!("unsupported control operator {}", ctrl));
      }
    }

    Ok(())
  }

  fn visit_type2(&mut self, t2: &Type2<'a>) -> visitor::Result<Error> {
    match t2 {
      Type2::TextValue { value, .. } => self.visit_value(&token::Value::TEXT(value.clone())),
      Type2::Map { group, .. } => match self.json.as_ref() {
        Value::Object(o) => {
          // Check if this is an empty map schema with non-empty JSON object
          if group.group_choices.len() == 1
            && group.group_choices[0].group_entries.is_empty()
            && !o.is_empty()
            && !matches!(
              self.state.ctrl,
              Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT)
            )
          {
            self.add_error(format!("expected empty map, got {}", self.json));
            return Ok(());
          }

          #[allow(clippy::needless_collect)]
          let o = o.keys().cloned().collect::<Vec<_>>();

          self.visit_closed_map(group, &o)?;

          self.state.is_cut_present = false;
          self.cut_value = None;
          Ok(())
        }
        Value::Array(_) => self.validate_array_items(&ArrayItemToken::Group(group)),
        _ => {
          self.add_error(format!("expected map object {}, got {}", t2, self.json));
          Ok(())
        }
      },
      Type2::Array { group, .. } => match self.json.as_ref() {
        Value::Array(a) => {
          if group.group_choices.len() == 1
            && group.group_choices[0].group_entries.is_empty()
            && !a.is_empty()
            && !matches!(
              self.state.ctrl,
              Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT)
            )
          {
            self.add_error(format!("expected empty array, got {}", self.json));
            return Ok(());
          }

          let elems = a.clone();
          let mut ctx = ArraySeqCtx::default();
          match self.seq_match_group(group, &elems, 0, &mut ctx)? {
            Some(end) if end == elems.len() => (),
            Some(end) => {
              self.add_error(format!(
                "array validation failed: group {} matched the first {} element(s), but the array has {} elements",
                group,
                end,
                elems.len()
              ));
            }
            None => {
              if let Some((idx, errors)) = ctx.best_failure.take() {
                self.errors.extend(errors);
                self.add_error(format!(
                  "array validation failed: element sequence does not match group {} (mismatch at element index {})",
                  group, idx
                ));
              } else {
                self.add_error(format!(
                  "array validation failed: element sequence does not match group {}",
                  group
                ));
              }
            }
          }

          Ok(())
        }
        _ => {
          self.add_error(format!("expected array type, got {}", self.json));
          Ok(())
        }
      },
      Type2::ChoiceFromGroup {
        ident,
        generic_args,
        ..
      } => {
        if let Some(ga) = generic_args {
          if let Some(rule) = rule_from_ident(self.state.cddl, ident) {
            let resolved_args = resolved_generic_args(&self.state, ga);
            if let Some(gr) = self
              .state
              .generic_rules
              .iter_mut()
              .find(|r| r.name == ident.ident)
            {
              gr.args.extend(resolved_args);
            } else if let Some(params) = generic_params_from_rule(rule) {
              self.state.generic_rules.push(GenericRule {
                name: ident.ident,
                params,
                args: resolved_args,
              });
            }

            let mut jv = self.new_child_validator(self.json.clone());

            jv.state.generic_rules = self.state.generic_rules.clone();
            jv.state.eval_generic_rule = Some(ident.ident);
            jv.state.is_group_to_choice_enum = true;
            jv.state.is_multi_type_choice = self.state.is_multi_type_choice;
            jv.visit_rule(rule)?;

            self.errors.append(&mut jv.errors);

            return Ok(());
          }
        }

        if group_rule_from_ident(self.state.cddl, ident).is_none() {
          self.add_error(format!(
            "rule {} must be a group rule to turn it into a choice",
            ident
          ));
          return Ok(());
        }

        self.state.is_group_to_choice_enum = true;
        self.visit_identifier(ident)?;
        self.state.is_group_to_choice_enum = false;

        Ok(())
      }
      Type2::ChoiceFromInlineGroup { group, .. } => {
        self.state.is_group_to_choice_enum = true;
        self.visit_group(group)?;
        self.state.is_group_to_choice_enum = false;
        Ok(())
      }
      Type2::Typename {
        ident,
        generic_args,
        ..
      } => {
        if let Some(ga) = generic_args {
          if let Some(rule) = rule_from_ident(self.state.cddl, ident) {
            let resolved_args = resolved_generic_args(&self.state, ga);
            if let Some(gr) = self
              .state
              .generic_rules
              .iter_mut()
              .find(|r| r.name == ident.ident)
            {
              gr.args.extend(resolved_args);
            } else if let Some(params) = generic_params_from_rule(rule) {
              self.state.generic_rules.push(GenericRule {
                name: ident.ident,
                params,
                args: resolved_args,
              });
            }

            let mut jv = self.new_child_validator(self.json.clone());

            jv.state.generic_rules = self.state.generic_rules.clone();
            jv.state.eval_generic_rule = Some(ident.ident);
            jv.state.is_multi_type_choice = self.state.is_multi_type_choice;
            jv.visit_rule(rule)?;

            self.errors.append(&mut jv.errors);

            return Ok(());
          }
        }

        self.visit_identifier(ident)
      }
      Type2::IntValue { value, .. } => self.visit_value(&token::Value::INT(*value)),
      Type2::UintValue { value, .. } => self.visit_value(&token::Value::UINT(*value)),
      Type2::FloatValue { value, .. } => self.visit_value(&token::Value::FLOAT(*value)),
      Type2::ParenthesizedType { pt, .. } => self.visit_type(pt),
      Type2::Unwrap {
        ident,
        generic_args,
        ..
      } => {
        // Per
        // https://github.com/w3c/did-spec-registries/pull/138#issuecomment-719739215,
        // strip tag and validate underlying type
        if let Some(Type2::TaggedData { t, .. }) = tag_from_token(&lookup_ident(ident.ident)) {
          return self.visit_type(&t);
        }

        if let Some(ga) = generic_args {
          if let Some(rule) = unwrap_rule_from_ident(self.state.cddl, ident) {
            let resolved_args = resolved_generic_args(&self.state, ga);
            if let Some(gr) = self
              .state
              .generic_rules
              .iter_mut()
              .find(|gr| gr.name == ident.ident)
            {
              gr.args.extend(resolved_args);
            } else if let Some(params) = generic_params_from_rule(rule) {
              self.state.generic_rules.push(GenericRule {
                name: ident.ident,
                params,
                args: resolved_args,
              });
            }

            let mut jv = self.new_child_validator(self.json.clone());

            jv.state.generic_rules = self.state.generic_rules.clone();
            jv.state.eval_generic_rule = Some(ident.ident);
            jv.state.is_multi_type_choice = self.state.is_multi_type_choice;
            jv.visit_rule(rule)?;

            self.errors.append(&mut jv.errors);

            return Ok(());
          }
        }

        if let Some(rule) = unwrap_rule_from_ident(self.state.cddl, ident) {
          return self.visit_rule(rule);
        }

        self.add_error(format!(
          "cannot unwrap identifier {}, rule not found",
          ident
        ));

        Ok(())
      }
      #[cfg(feature = "ast-span")]
      Type2::Any { .. } => Ok(()),
      #[cfg(not(feature = "ast-span"))]
      Type2::Any {} => Ok(()),
      #[cfg(feature = "additional-controls")]
      Type2::B16ByteString { value, .. } => {
        if let Some(ctrl) = &self.state.ctrl {
          // For B16ByteString, the value contains the hex string as bytes (ASCII representation)
          // We need to decode it to get the actual bytes
          let hex_str = match std::str::from_utf8(value) {
            Ok(s) => s,
            Err(_) => {
              self.add_error("invalid UTF-8 in hex string".to_string());
              return Ok(());
            }
          };

          match hex::decode(hex_str.replace(' ', "")) {
            Ok(actual_bytes) => match ctrl {
              ControlOperator::B64U => self.validate_b64_control(&actual_bytes, false, false),
              ControlOperator::B64C => self.validate_b64_control(&actual_bytes, true, false),
              ControlOperator::B64USLOPPY => self.validate_b64_control(&actual_bytes, false, true),
              ControlOperator::B64CSLOPPY => self.validate_b64_control(&actual_bytes, true, true),
              ControlOperator::HEX => self.validate_hex_control(&actual_bytes, false),
              ControlOperator::HEXLC => self.validate_hex_control(&actual_bytes, true),
              ControlOperator::HEXUC => self.validate_hex_control(&actual_bytes, true),
              _ => {
                self.add_error(format!(
                  "unsupported byte string data type for JSON validation with control {}, got {}",
                  ctrl, t2
                ));
                Ok(())
              }
            },
            Err(_) => {
              self.add_error("invalid hex encoding in byte string".to_string());
              Ok(())
            }
          }
        } else {
          self.add_error(format!(
            "byte string data type not supported for JSON validation without control, got {}",
            t2
          ));
          Ok(())
        }
      }
      #[cfg(feature = "additional-controls")]
      Type2::UTF8ByteString { value, .. } => {
        if let Some(ctrl) = &self.state.ctrl {
          match ctrl {
            ControlOperator::B64U => self.validate_b64_control(value, false, false),
            ControlOperator::B64C => self.validate_b64_control(value, true, false),
            ControlOperator::B64USLOPPY => self.validate_b64_control(value, false, true),
            ControlOperator::B64CSLOPPY => self.validate_b64_control(value, true, true),
            ControlOperator::HEX => self.validate_hex_control(value, false),
            ControlOperator::HEXLC => self.validate_hex_control(value, true),
            ControlOperator::HEXUC => self.validate_hex_control(value, true),
            _ => {
              self.add_error(format!(
                "unsupported byte string data type for JSON validation with control {}, got {}",
                ctrl, t2
              ));
              Ok(())
            }
          }
        } else {
          self.add_error(format!(
            "byte string data type not supported for JSON validation without control, got {}",
            t2
          ));
          Ok(())
        }
      }
      #[cfg(feature = "additional-controls")]
      Type2::B64ByteString { value, .. } => {
        if let Some(ctrl) = &self.state.ctrl {
          match ctrl {
            ControlOperator::B64U => self.validate_b64_control(value, false, false),
            ControlOperator::B64C => self.validate_b64_control(value, true, false),
            ControlOperator::B64USLOPPY => self.validate_b64_control(value, false, true),
            ControlOperator::B64CSLOPPY => self.validate_b64_control(value, true, true),
            ControlOperator::HEX => self.validate_hex_control(value, false),
            ControlOperator::HEXLC => self.validate_hex_control(value, true),
            ControlOperator::HEXUC => self.validate_hex_control(value, true),
            _ => {
              self.add_error(format!(
                "unsupported byte string data type for JSON validation with control {}, got {}",
                ctrl, t2
              ));
              Ok(())
            }
          }
        } else {
          self.add_error(format!(
            "byte string data type not supported for JSON validation without control, got {}",
            t2
          ));
          Ok(())
        }
      }
      _ => {
        self.add_error(format!(
          "unsupported data type for validating JSON, got {}",
          t2
        ));
        Ok(())
      }
    }
  }

  fn visit_identifier(&mut self, ident: &Identifier<'a>) -> visitor::Result<Error> {
    if let Some(name) = self.state.eval_generic_rule {
      if let Some(gr) = self
        .state
        .generic_rules
        .iter()
        .find(|&gr| gr.name == name)
        .cloned()
      {
        for (idx, gp) in gr.params.iter().enumerate() {
          if *gp == ident.ident {
            if let Some(arg) = gr.args.get(idx) {
              return self.visit_type1(arg);
            }
          }
        }
      }
    }

    // A revisited rule only indicates a cycle when validation has not
    // consumed any input since the previous visit, so recursion is tracked
    // per data location and consumed map pairs: `t /= [t]` must recurse
    // into nested elements
    // (RFC 8610 Appendix A PEG matching) while a zero-progress reference
    // like `a /= a` is still caught at its fixed position. NUL cannot
    // appear in an identifier, so the key is unambiguous.
    let (visited_key, recursive) = self.rule_recursion_marker(ident);

    // Check for recursion before processing the identifier
    if recursive {
      // We've seen this rule before in the current validation path, indicating a cycle
      self.add_error(format!(
        "Recursive rule reference detected: {}. This may indicate a circular definition in the CDDL schema.",
        ident.ident
      ));
      return Ok(());
    }

    // self.state.is_colon_shortcut_present is only true when the ident is part of a
    // member key
    if !self.state.is_colon_shortcut_present {
      if let Some(r) = rule_from_ident(self.state.cddl, ident) {
        return self.visit_named_rule_path(visited_key, |validator| validator.visit_rule(r));
      }

      // A name may consist entirely of `/=` definitions (RFC 8610 Section
      // 2.2.2). Such a choice has no base rule for `rule_from_ident` to find.
      if !type_choice_types_from_ident(self.state.cddl, ident).is_empty() {
        return self.visit_named_rule_path(visited_key, |validator| {
          validator.visit_named_type_choice(ident)
        });
      }
    }

    if is_ident_any_type(self.state.cddl, ident) {
      // As a member key, `any` must consume object entries rather than
      // short-circuit, otherwise `* any => any` leaves the remaining keys
      // unvalidated and the closed-map check rejects them. Fall through to
      // the object handling below
      if !(self.state.is_member_key && matches!(self.json.as_ref(), Value::Object(_))) {
        return Ok(());
      }
    }

    // Special case for array values - check if we're in an array context and this
    // is a reference to another array type
    if let Value::Array(_) = self.json.as_ref() {
      if let Some(Rule::Type { rule, .. }) = rule_from_ident(self.state.cddl, ident) {
        for tc in rule.value.type_choices.iter() {
          if let Type2::Array { .. } = &tc.type1.type2 {
            return self
              .visit_named_rule_path(visited_key, |validator| validator.visit_type_choice(tc));
          }
        }
      }
    }

    match self.json.as_ref() {
      Value::Null if is_ident_null_data_type(self.state.cddl, ident) => Ok(()),
      Value::Bool(b) => {
        if is_ident_bool_data_type(self.state.cddl, ident) {
          return Ok(());
        }

        if ident_matches_bool_value(self.state.cddl, ident, *b) {
          return Ok(());
        }

        self.add_error(format!("expected type {}, got {}", ident, self.json));
        Ok(())
      }
      Value::Number(n) => {
        if is_json_unsigned(self.state.cddl, ident) {
          if n.is_u64() {
            return Ok(());
          }
        } else if is_ident_nint_data_type(self.state.cddl, ident) {
          if let Some(n) = n.as_i64() {
            if n.is_negative() {
              return Ok(());
            }
          }
        } else if is_ident_time_data_type(self.state.cddl, ident) {
          if let Some(n) = n.as_i64() {
            if let chrono::LocalResult::None = Utc.timestamp_millis_opt(n * 1000) {
              self.add_error(format!(
                "expected time data type, invalid UNIX timestamp {}",
                n,
              ));
            }

            return Ok(());
          } else if let Some(n) = n.as_f64() {
            // truncates fractional milliseconds when validating
            if let chrono::LocalResult::None = Utc.timestamp_millis_opt((n * 1000f64) as i64) {
              self.add_error(format!(
                "expected time data type, invalid UNIX timestamp {}",
                n,
              ));
            }
          }
        } else if let Some(kind) = ident_numeric_kind(self.state.cddl, ident) {
          let matches_kind = match kind {
            NumericKind::Int => n.is_i64(),
            NumericKind::Float => n.is_f64(),
            NumericKind::Both => n.is_i64() || n.is_f64(),
          };
          if matches_kind {
            return Ok(());
          }
        }

        self.add_error(format!("expected type {}, got {}", ident, self.json));
        Ok(())
      }
      Value::String(s) => {
        if is_ident_uri_data_type(self.state.cddl, ident) {
          if let Err(e) = uriparse::URI::try_from(&**s) {
            self.add_error(format!("expected URI data type, decoding error: {}", e));
          }
        } else if is_ident_b64url_data_type(self.state.cddl, ident) {
          if let Err(e) = base64_url::decode(s) {
            self.add_error(format!(
              "expected base64 URL data type, decoding error: {}",
              e
            ));
          }
        } else if is_ident_tdate_data_type(self.state.cddl, ident) {
          if let Err(e) = chrono::DateTime::parse_from_rfc3339(s) {
            self.add_error(format!("expected tdate data type, decoding error: {}", e));
          }
        } else if is_ident_string_data_type(self.state.cddl, ident) {
          return Ok(());
        } else {
          self.add_error(format!("expected type {}, got {}", ident, self.json));
        }

        Ok(())
      }
      Value::Array(_) => {
        // For arrays, check if this identifier refers to an array type rule first
        if let Some(r) = rule_from_ident(self.state.cddl, ident) {
          if is_array_type_rule(self.state.cddl, ident) {
            // Mark this rule as visited for array type processing
            self.state.visited_rules.insert(visited_key.clone());
            let result = self.visit_rule(r);
            // Remove this rule from visited set when we're done
            self.state.visited_rules.remove(&visited_key);
            return result;
          }
        }

        // Then fallback to normal array validation
        self.validate_array_items(&ArrayItemToken::Identifier(ident))
      }
      Value::Object(o) => match &self.state.occurrence {
        #[cfg(feature = "ast-span")]
        Some(Occur::Optional { .. }) | None => {
          // A type-domain string key (e.g. `tstr => v`) matches any object
          // entry, since JSON object keys are always strings. An `any` key
          // likewise matches any entry; it consumes the first
          // not-yet-validated one. `?` permits the entry to be absent
          // (RFC 8610 §3.2), so a find-miss is only an error for an
          // occurrence-less entry
          if self.state.is_member_key
            && (is_ident_string_data_type(self.state.cddl, ident)
              || is_ident_any_type(self.state.cddl, ident))
          {
            if let Some((k, v)) = o.iter().find(|(k, _)| {
              !self
                .validated_keys
                .as_ref()
                .is_some_and(|keys| keys.contains(*k))
            }) {
              self
                .validated_keys
                .get_or_insert(vec![k.clone()])
                .push(k.clone());
              self.object_value = Some(v.clone());
              self.object_value_key = Some(k.clone());
              let _ = write!(self.state.data_location, "/{}", k);
            } else if let Some(Occur::Optional { .. }) = self.state.occurrence.take() {
              // absent optional entry: skip value validation for this entry
              self.state.advance_to_next_entry = true;
            } else {
              self.add_error(format!("object requires entry key of type {}", ident));
            }

            return Ok(());
          }

          if token::lookup_ident(ident.ident)
            .in_standard_prelude()
            .is_some()
          {
            self.add_error(format!(
              "expected object value of type {}, got object",
              ident.ident
            ));
            return Ok(());
          }

          self.visit_value(&token::Value::TEXT(ident.ident.into()))
        }
        #[cfg(not(feature = "ast-span"))]
        Some(Occur::Optional {}) | None => {
          if self.state.is_member_key
            && (is_ident_string_data_type(self.state.cddl, ident)
              || is_ident_any_type(self.state.cddl, ident))
          {
            if let Some((k, v)) = o.iter().find(|(k, _)| {
              !self
                .validated_keys
                .as_ref()
                .is_some_and(|keys| keys.contains(*k))
            }) {
              self
                .validated_keys
                .get_or_insert(vec![k.clone()])
                .push(k.clone());
              self.object_value = Some(v.clone());
              self.object_value_key = Some(k.clone());
              self.state.data_location.push_str(&format!("/{}", k));
            } else if let Some(Occur::Optional { .. }) = self.state.occurrence.take() {
              // absent optional entry: skip value validation for this entry
              self.state.advance_to_next_entry = true;
            } else {
              self.add_error(format!("object requires entry key of type {}", ident));
            }

            return Ok(());
          }

          if token::lookup_ident(ident.ident)
            .in_standard_prelude()
            .is_some()
          {
            self.add_error(format!(
              "expected object value of type {}, got object",
              ident.ident
            ));
            return Ok(());
          }

          self.visit_value(&token::Value::TEXT(ident.ident.into()))
        }
        Some(_) => {
          if is_ident_string_data_type(self.state.cddl, ident)
            || is_ident_any_type(self.state.cddl, ident)
          {
            let map_entry_candidates = o
              .iter()
              .filter_map(|(k, v)| match &self.validated_keys {
                Some(keys) if !keys.contains(k) => Some((k.clone(), v.clone())),
                Some(_) => None,
                None => Some((k.clone(), v.clone())),
              })
              .collect::<Vec<_>>();

            self.map_entry_candidates = Some(map_entry_candidates);
            return Ok(());
          }

          Ok(())
        }
      },
      _ => {
        if let Some(cut_value) = self.cut_value.take() {
          self.add_error(format!(
            "cut present for member key {}. expected type {}, got {}",
            cut_value, ident, self.json
          ));
        } else {
          self.add_error(format!("expected type {}, got {}", ident, self.json));
        }
        Ok(())
      }
    }
  }

  fn visit_value_member_key_entry(
    &mut self,
    entry: &ValueMemberKeyEntry<'a>,
  ) -> visitor::Result<Error> {
    debug_assert!(self.map_entry_candidates.is_none());
    debug_assert!(self.object_value.is_none());
    self.map_entry_candidates = None;
    self.object_value = None;

    if let Some(occur) = &entry.occur {
      self.visit_occurrence(occur)?;
    }

    let current_location = self.state.data_location.clone();

    let (map_entry_candidates, object_value, object_value_key) = if let Some(mk) = &entry.member_key
    {
      let error_count = self.errors.len();
      self.state.is_member_key = true;
      let member_key_result = self.visit_memberkey(mk);
      self.state.is_member_key = false;

      let map_entry_candidates = self.map_entry_candidates.take();
      let object_value = self.object_value.take();
      let object_value_key = self.object_value_key.take();
      member_key_result?;

      // Move to next entry if member key validation fails
      if self.errors.len() != error_count {
        self.state.advance_to_next_entry = true;
        return Ok(());
      }

      (map_entry_candidates, object_value, object_value_key)
    } else {
      (None, None, None)
    };

    if let Some(candidates) = map_entry_candidates {
      debug_assert!(object_value.is_none());
      return self.validate_candidate_pairs(entry, candidates, current_location, None, None);
    }

    if let Some(v) = object_value {
      return self.validate_relayed_pair(entry, v, object_value_key, current_location);
    }

    if !self.state.advance_to_next_entry {
      self.visit_type(&entry.entry_type)
    } else {
      Ok(())
    }
  }

  fn visit_type_groupname_entry(
    &mut self,
    entry: &TypeGroupnameEntry<'a>,
  ) -> visitor::Result<Error> {
    let group_choice_alternates = group_choice_alternates_from_ident(self.state.cddl, &entry.name);
    if entry.name.socket == Some(SocketPlug::GROUP)
      && group_choice_alternates.is_empty()
      && rule_from_ident(self.state.cddl, &entry.name).is_none()
    {
      // An unplugged socket has no matching alternatives (RFC 8610 §3.9).
      // Only an occurrence allowing zero matches can skip it.
      if occurrence_bounds(entry.occur.as_ref().map(|o| o.occur)).0 > 0 {
        self.add_error("undefined group socket requires zero occurrences".into());
      }
      return Ok(());
    }

    self.state.type_group_name_entry = Some(entry.name.ident);

    if let Some(ga) = &entry.generic_args {
      if let Some(rule) = rule_from_ident(self.state.cddl, &entry.name) {
        return self.visit_generic_group_reference(entry, ga, rule);
      }
    }

    if let (Some(member), Some(occurrence)) = (
      self.batchable_named_map_member(entry, false),
      entry.occur.as_ref(),
    ) {
      let result = self.visit_batched_named_map_member(&member, occurrence);
      self.state.type_group_name_entry = None;
      return result;
    }

    let error_count = self.errors.len();
    if !group_choice_alternates.is_empty() {
      self.state.is_multi_group_choice = true;
    }

    if self.map_choice_search.is_some() {
      return self.visit_planned_group_reference(entry, &group_choice_alternates);
    }

    for ge in group_choice_alternates {
      let cur_errors = self.errors.len();
      self.visit_group_entry(ge)?;
      if self.errors.len() == cur_errors {
        for _ in 0..self.errors.len() - error_count {
          self.errors.pop();
        }

        return Ok(());
      }
    }

    walk_type_groupname_entry(self, entry)?;
    self.state.type_group_name_entry = None;

    Ok(())
  }

  fn visit_group_entry(&mut self, entry: &GroupEntry<'a>) -> visitor::Result<Error> {
    if let (GroupEntry::ValueMemberKey { ge, .. }, Value::Object(_)) = (entry, self.json.as_ref()) {
      if let Some((key_type, alternatives, is_cut)) = choice_member_key(&self.state, ge) {
        return self.visit_choice_key_entry(ge, key_type, &alternatives, is_cut);
      }
      let (min, max) = occurrence_bounds(ge.occur.as_ref().map(|o| o.occur));
      if matches!(&ge.member_key, Some(MemberKey::Type1 { is_cut: false, .. })) && min == 0 {
        return self.visit_zero_minimum_entry(entry, max);
      }
    }
    if let GroupEntry::TypeGroupname { ge, .. } = entry {
      if ge.occur.is_some()
        && occurrence_bounds(ge.occur.as_ref().map(|o| o.occur)).0 == 0
        && matches!(self.json.as_ref(), Value::Object(_))
      {
        return self.visit_optional_named_group(ge);
      }
      // A group reference's occurrence applies to the referenced group
      // alone: once the group has matched, restore the prior indicator so
      // `? g` cannot relax later required members, mirroring inline groups.
      let prior_occurrence = self.state.occurrence;
      let error_count = self.errors.len();
      walk_group_entry(self, entry)?;
      if ge.occur.is_some() && self.errors.len() == error_count {
        self.state.occurrence = prior_occurrence;
      }
      return Ok(());
    }
    walk_group_entry(self, entry)
  }

  fn visit_inline_group_entry(
    &mut self,
    occur: Option<&Occurrence<'a>>,
    g: &Group<'a>,
  ) -> visitor::Result<Error> {
    let may_be_absent = occurrence_bounds(occur.map(|o| o.occur)).0 == 0;
    if !may_be_absent || !matches!(self.json.as_ref(), Value::Object(_)) {
      return walk_inline_group_entry(self, occur, g);
    }
    let max = occurrence_bounds(occur.map(|o| o.occur)).1;
    if self.skip_proven_optional_choices(g, None, max != Some(0))? {
      return Ok(());
    }
    if let (Some(member), Some(occurrence)) = (self.batchable_map_member(g, None, self, 0), occur) {
      return self.visit_batched_map_member(member, occurrence);
    }
    self.visit_zero_minimum_group(max, |validator| validator.visit_group(g))
  }

  fn visit_memberkey(&mut self, mk: &MemberKey<'a>) -> visitor::Result<Error> {
    match mk {
      MemberKey::Type1 { is_cut, .. } => {
        self.state.is_cut_present = *is_cut;
        walk_memberkey(self, mk)?;
        self.state.is_cut_present = false;
      }
      MemberKey::Bareword { .. } => {
        self.state.is_colon_shortcut_present = true;
        walk_memberkey(self, mk)?;
        self.state.is_colon_shortcut_present = false;
      }
      _ => return walk_memberkey(self, mk),
    }

    Ok(())
  }

  fn visit_value(&mut self, value: &token::Value<'a>) -> visitor::Result<Error> {
    // An array can never match a literal value; validate_array_items reports
    // the type mismatch (unless the value is an annotation-only member key)
    if let Value::Array(_) = self.json.as_ref() {
      return self.validate_array_items(&ArrayItemToken::Value(value));
    }

    if let Value::Object(_) = self.json.as_ref() {
      return self.validate_object_value(value);
    }

    let error: Option<String> = match value {
      token::Value::INT(v) => match self.json.as_ref() {
        Value::Number(n) => match n.as_i64() {
          Some(i) => match &self.state.ctrl {
            Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT) if i != *v as i64 => None,
            Some(ControlOperator::LT) if i < *v as i64 => None,
            Some(ControlOperator::LE) if i <= *v as i64 => None,
            Some(ControlOperator::GT) if i > *v as i64 => None,
            Some(ControlOperator::GE) if i >= *v as i64 => None,
            #[cfg(feature = "additional-controls")]
            Some(ControlOperator::PLUS) => {
              if i == *v as i64 {
                None
              } else {
                Some(format!("expected computed .plus value {}, got {}", v, n))
              }
            }
            #[cfg(feature = "additional-controls")]
            None | Some(ControlOperator::FEATURE) => {
              if i == *v as i64 {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            #[cfg(not(feature = "additional-controls"))]
            None => {
              if i == *v as i64 {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            _ => Some(format!(
              "expected value {} {}, got {}",
              self.state.ctrl.unwrap(),
              v,
              n
            )),
          },
          None => Some(format!("{} cannot be represented as an i64", n)),
        },
        _ => Some(format!("expected value {}, got {}", v, self.json)),
      },
      token::Value::UINT(v) => match self.json.as_ref() {
        Value::Number(n) => match n.as_u64() {
          Some(i) => match &self.state.ctrl {
            Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT) if i != *v as u64 => None,
            Some(ControlOperator::LT) if i < *v as u64 => None,
            Some(ControlOperator::LE) if i <= *v as u64 => None,
            Some(ControlOperator::GT) if i > *v as u64 => None,
            Some(ControlOperator::GE) if i >= *v as u64 => None,
            Some(ControlOperator::SIZE) => match 256u128.checked_pow(*v as u32) {
              Some(n) if (i as u128) < n => None,
              _ => Some(format!("expected value .size {}, got {}", v, n)),
            },
            #[cfg(feature = "additional-controls")]
            Some(ControlOperator::PLUS) => {
              if i == *v as u64 {
                None
              } else {
                Some(format!("expected computed .plus value {}, got {}", v, n))
              }
            }
            #[cfg(feature = "additional-controls")]
            None | Some(ControlOperator::FEATURE) => {
              if i == *v as u64 {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            #[cfg(not(feature = "additional-controls"))]
            None => {
              if i == *v as u64 {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            _ => Some(format!(
              "expected value {} {}, got {}",
              self.state.ctrl.unwrap(),
              v,
              n
            )),
          },
          None => Some(format!("{} cannot be represented as a u64", n)),
        },
        Value::String(s) => match &self.state.ctrl {
          Some(ControlOperator::SIZE) => {
            if s.len() == *v {
              None
            } else {
              Some(format!("expected \"{}\" .size {}, got {}", s, v, s.len()))
            }
          }
          _ => Some(format!("expected {}, got {}", v, s)),
        },
        _ => Some(format!("expected value {}, got {}", v, self.json)),
      },
      token::Value::FLOAT(v) => match self.json.as_ref() {
        Value::Number(n) => match n.as_f64() {
          Some(f) => match &self.state.ctrl {
            Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT)
              if (f - *v).abs() > f64::EPSILON =>
            {
              None
            }
            Some(ControlOperator::LT) if f < *v => None,
            Some(ControlOperator::LE) if f <= *v => None,
            Some(ControlOperator::GT) if f > *v => None,
            Some(ControlOperator::GE) if f >= *v => None,
            #[cfg(feature = "additional-controls")]
            Some(ControlOperator::PLUS) => {
              if (f - *v).abs() < f64::EPSILON {
                None
              } else {
                Some(format!("expected computed .plus value {}, got {}", v, n))
              }
            }
            #[cfg(feature = "additional-controls")]
            None | Some(ControlOperator::FEATURE) => {
              if (f - *v).abs() < f64::EPSILON {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            #[cfg(not(feature = "additional-controls"))]
            None => {
              if (f - *v).abs() < f64::EPSILON {
                None
              } else {
                Some(format!("expected value {}, got {}", v, n))
              }
            }
            _ => Some(format!(
              "expected value {} {}, got {}",
              self.state.ctrl.unwrap(),
              v,
              n
            )),
          },
          None => Some(format!("{} cannot be represented as an i64", n)),
        },
        _ => Some(format!("expected value {}, got {}", v, self.json)),
      },
      token::Value::TEXT(t) => match self.json.as_ref() {
        Value::String(s) => match &self.state.ctrl {
          Some(ControlOperator::NE) | Some(ControlOperator::DEFAULT) => {
            if s != t {
              None
            } else {
              Some(format!("expected {} .ne to \"{}\"", value, s))
            }
          }
          Some(ControlOperator::REGEXP) => {
            let re = regex::Regex::new(
              &format_regex(t)
                .ok_or_else(|| Error::from_validator(self, "malformed regex".to_string()))?,
            )
            .map_err(|e| Error::from_validator(self, e.to_string()))?;

            if re.is_match(s) {
              None
            } else {
              Some(format!("expected \"{}\" to match regex \"{}\"", s, t))
            }
          }
          Some(ControlOperator::PCRE) => {
            #[cfg(feature = "freezer")]
            {
              let anchored = format!("^(?:{})$", t);
              let re = fancy_regex::Regex::new(&anchored)
                .map_err(|e| Error::from_validator(self, e.to_string()))?;
              match re.is_match(s) {
                Ok(true) => None,
                Ok(false) => Some(format!("expected \"{}\" to match pcre \"{}\"", s, t)),
                Err(e) => Some(format!("pcre matching error: {}", e)),
              }
            }
            #[cfg(not(feature = "freezer"))]
            {
              let re = regex::Regex::new(
                &format_regex(t)
                  .ok_or_else(|| Error::from_validator(self, "malformed regex".to_string()))?,
              )
              .map_err(|e| Error::from_validator(self, e.to_string()))?;
              if re.is_match(s) {
                None
              } else {
                Some(format!("expected \"{}\" to match pcre \"{}\"", s, t))
              }
            }
          }
          #[cfg(feature = "freezer")]
          Some(ControlOperator::IREGEXP) => {
            let anchored = format!("^(?:{})$", t);
            let re = regex::Regex::new(&anchored)
              .map_err(|e| Error::from_validator(self, e.to_string()))?;
            if re.is_match(s) {
              None
            } else {
              Some(format!("expected \"{}\" to match iregexp \"{}\"", s, t))
            }
          }
          #[cfg(feature = "additional-controls")]
          Some(ControlOperator::ABNF) => validate_abnf(t, s)
            .err()
            .map(|e| format!("\"{}\" is not valid against abnf: {}", s, e)),

          _ => {
            #[cfg(feature = "additional-controls")]
            if s == t {
              None
            } else if let Some(ControlOperator::CAT) | Some(ControlOperator::DET) = &self.state.ctrl
            {
              Some(format!(
                "expected value to match concatenated string {}, got \"{}\"",
                value, s
              ))
            } else if let Some(ctrl) = &self.state.ctrl {
              Some(format!("expected value {} {}, got \"{}\"", ctrl, value, s))
            } else {
              Some(format!("expected value {} got \"{}\"", value, s))
            }

            #[cfg(not(feature = "additional-controls"))]
            if s == t {
              None
            } else if let Some(ctrl) = &self.state.ctrl {
              Some(format!("expected value {} {}, got \"{}\"", ctrl, value, s))
            } else {
              Some(format!("expected value {} got \"{}\"", value, s))
            }
          }
        },
        _ => Some(format!("expected value {}, got {}", t, self.json)),
      },
      token::Value::BYTE(token::ByteValue::UTF8(b)) => match self.json.as_ref() {
        Value::String(s) if s.as_bytes() == b.as_ref() => None,
        _ => Some(format!("expected byte value {:?}, got {}", b, self.json)),
      },
      token::Value::BYTE(token::ByteValue::B16(b)) => match self.json.as_ref() {
        Value::String(s) if s.as_bytes() == b.as_ref() => None,
        _ => Some(format!("expected byte value {:?}, got {}", b, self.json)),
      },
      token::Value::BYTE(token::ByteValue::B64(b)) => match self.json.as_ref() {
        Value::String(s) if s.as_bytes() == b.as_ref() => None,
        _ => Some(format!("expected byte value {:?}, got {}", b, self.json)),
      },
    };

    if let Some(e) = error {
      self.add_error(e);
    }

    Ok(())
  }

  fn visit_occurrence(&mut self, o: &Occurrence) -> visitor::Result<Error> {
    self.state.occurrence = Some(o.occur);

    Ok(())
  }
}

// Helper function to check if an identifier refers to an array type rule
fn is_array_type_rule<'a>(cddl: &'a CDDL<'a>, ident: &Identifier<'a>) -> bool {
  for r in cddl.rules.iter() {
    if let Rule::Type { rule, .. } = r {
      if rule.name.ident == ident.ident {
        // Check if the type rule defines an array
        for tc in rule.value.type_choices.iter() {
          if let Type2::Array { .. } = &tc.type1.type2 {
            return true;
          }
        }
      }
    }
  }
  false
}

#[cfg(test)]
mod map_replay_budget_tests {
  use super::*;
  use crate::pest_bridge::cddl_from_pest_str;

  fn limited_validator<'a>(cddl: &'a CDDL<'a>, json: Value, limit: usize) -> JSONValidator<'a> {
    #[cfg(feature = "additional-controls")]
    let mut validator = JSONValidator::new(cddl, json, None);
    #[cfg(not(feature = "additional-controls"))]
    let mut validator = JSONValidator::new(cddl, json);
    validator.map_replay_budget = Arc::new(MapReplayBudget::new(limit));
    validator
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_identical_claims_share_probes_but_not_generic_bindings() {
    let cddl =
      cddl_from_pest_str("root = { tstr => uint } integer = uint text-value = tstr").unwrap();
    let Rule::Type { rule, .. } = &cddl.rules[0] else {
      panic!("root type")
    };
    let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("map")
    };
    let validator = limited_validator(&cddl, serde_json::json!({}), 0);
    let member = validator
      .batchable_map_member(group, None, &validator, 0)
      .unwrap()
      .clone();
    let mut claims: Vec<_> = (0..64)
      .map(|index| MemberClaim {
        key: index.to_string(),
        entry: member.clone(),
        context: None,
      })
      .collect();
    let mut probes = 0;
    let incompatible = claim_compatibility(&claims, 64, |_, pair| {
      probes += 1;
      Ok(pair < 63)
    })
    .unwrap();
    assert!(incompatible.is_none());
    assert_eq!(probes, 64);
    let rows = claim_compatibility(&claims, 64, |_, _| Ok(true))
      .unwrap()
      .unwrap();
    assert!(Arc::ptr_eq(&rows[0], &rows[63]));

    for (claim, rule) in claims.iter_mut().zip(&cddl.rules[1..]) {
      let Rule::Type { rule, .. } = rule else {
        panic!("argument type")
      };
      claim.context = Some(ClaimEvalContext {
        rule_name: "g",
        generic_rules: vec![GenericRule {
          name: "g",
          params: vec!["T"],
          args: vec![rule.value.type_choices[0].type1.clone()],
        }],
      });
    }
    probes = 0;
    let rows = claim_compatibility(&claims[..2], 2, |_, _| {
      probes += 1;
      Ok(true)
    })
    .unwrap()
    .unwrap();
    assert_eq!(probes, 4);
    assert!(!Arc::ptr_eq(&rows[0], &rows[1]));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_budget_boundary_and_repeated_validation() {
    let cddl =
      cddl_from_pest_str(r#"root = { ? "a" => uint // ? "a" => bool // a: tstr }"#).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"a":"value"}), 1);
    for _ in 0..2 {
      assert!(matches!(
        validator.validate(),
        Err(Error::ResourceLimit { limit: 1 })
      ));
      assert!(validator.map_choice_search.is_none());
      assert!(validator.validated_keys.is_none());
      assert!(validator.deferred_map_errors.is_empty());
      assert!(!validator.state.is_root);
    }
    validator.map_replay_budget = Arc::new(MapReplayBudget::new(2));
    for _ in 0..2 {
      assert!(validator.validate().is_ok());
      assert_eq!(
        validator
          .map_replay_budget
          .remaining
          .load(Ordering::Relaxed),
        0
      );
    }
    let mut invalid = limited_validator(&cddl, serde_json::json!({"a":[]}), 2);
    assert!(matches!(invalid.validate(), Err(Error::Validation(_))));

    let schema = format!(
      "root = {{ {} }}",
      (0..13)
        .map(|index| format!("(a{index}: uint // b{index}: uint)"))
        .collect::<Vec<_>>()
        .join(",")
    );
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let input = Value::Object(
      (0..13)
        .map(|index| (format!("b{index}"), serde_json::json!(1)))
        .collect(),
    );
    let mut pruned = limited_validator(&cddl, input.clone(), 13);
    assert!(pruned.validate().is_ok());
    assert_eq!(
      pruned.map_replay_budget.remaining.load(Ordering::Relaxed),
      0
    );
    let mut exhausted = limited_validator(&cddl, input, 12);
    assert!(matches!(
      exhausted.validate(),
      Err(Error::ResourceLimit { limit: 12 })
    ));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_deferred_value_errors_reset_on_reuse() {
    let cddl = cddl_from_pest_str(r#"root = { ? "port" => uint }"#).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"port":"80"}), 0);
    let Err(Error::Validation(errors)) = validator.validate() else {
      panic!("expected a value mismatch");
    };
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].json_location, "/port");
    assert!(errors[0].reason.contains("expected type uint"));
    assert!(validator.deferred_map_errors.is_empty());
    validator.json = serde_json::json!({"other":true}).into();
    let Err(Error::Validation(errors)) = validator.validate() else {
      panic!("expected an unknown key");
    };
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].reason, "unexpected key \"other\"");
    for json in [serde_json::json!({"port":80}), serde_json::json!({})] {
      validator.json = json.into();
      assert!(validator.validate().is_ok());
      assert!(validator.deferred_map_errors.is_empty());
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_failed_generic_children_preserve_unique_ownership() {
    let cddl = cddl_from_pest_str("root = { a: uint, g<uint> } g<T> = (b: T,)").unwrap();
    let Rule::Type { rule, .. } = &cddl.rules[0] else {
      panic!("root type");
    };
    let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("map");
    };
    let entries = &group.group_choices[0].group_entries;
    let mut validator = limited_validator(&cddl, serde_json::json!({"a":1,"b":false}), 0);
    validator.visit_group_entry(&entries[0].0).unwrap();
    for _ in 0..8 {
      validator.visit_group_entry(&entries[1].0).unwrap();
      assert!(!validator.errors.is_empty());
      assert_eq!(validator.validated_keys, Some(vec!["a".to_owned()]));
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_budget_is_cumulative_across_nested_and_child_validators() {
    for (schema, json, required) in [
      (
        r#"root = { left: branch, right: branch }
           branch = { ? "x" => uint // x: tstr }"#,
        serde_json::json!({"left":{"x":"one"},"right":{"x":"two"}}),
        2,
      ),
      (
        r#"root = [* { ? "x" => uint // x: tstr }]"#,
        serde_json::json!([{"x":"one"},{"x":"two"}]),
        2,
      ),
      (
        r#"root = { left: branch<tstr>, right: branch<tstr> }
           branch<T> = { ? "x" => uint // x: T }"#,
        serde_json::json!({"left":{"x":"one"},"right":{"x":"two"}}),
        2,
      ),
      (
        r#"root = { (left: branch, right: uint) // (left: branch, right: tstr) }
           branch = { ? "x" => uint // x: tstr }"#,
        serde_json::json!({"left":{"x":"value"},"right":"value"}),
        3,
      ),
      (
        r#"root = { * members<branch> } members<T> = (tstr => T)
           branch = { ? "x" => uint // x: tstr }"#,
        serde_json::json!({"left":{"x":"one"},"right":{"x":"two"}}),
        2,
      ),
    ] {
      let cddl = cddl_from_pest_str(schema).unwrap();
      let mut exhausted = limited_validator(&cddl, json.clone(), required - 1);
      assert!(matches!(
        exhausted.validate(),
        Err(Error::ResourceLimit { limit }) if limit == required - 1
      ));
      let mut sufficient = limited_validator(&cddl, json, required);
      assert!(sufficient.validate().is_ok());
      assert_eq!(
        sufficient
          .map_replay_budget
          .remaining
          .load(Ordering::Relaxed),
        0
      );
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_resource_errors_cannot_be_swallowed_as_choice_mismatches() {
    for (root, json) in [
      ("root = branch / any", serde_json::json!({"x":"value"})),
      (
        "root = { left: branch // * tstr => any }",
        serde_json::json!({"left":{"x":"value"}}),
      ),
      (
        r#"root = { ? "left" => branch, * tstr => any }"#,
        serde_json::json!({"left":{"x":"value"}}),
      ),
      (
        "root = { ? members, * tstr => any } members = (left: branch)",
        serde_json::json!({"left":{"x":"value"}}),
      ),
      (
        "root = { ? (left: branch), * tstr => any }",
        serde_json::json!({"left":{"x":"value"}}),
      ),
      (
        "root = { * (tstr => branch), * tstr => any }",
        serde_json::json!({"left":{"x":"value"}}),
      ),
      (
        "root = { * members, * tstr => any } members = (tstr => branch)",
        serde_json::json!({"left":{"x":"value"}}),
      ),
      (
        "root = { * members<branch>, * tstr => any } members<T> = (tstr => T)",
        serde_json::json!({"left":{"x":"value"}}),
      ),
      ("root = [branch / any]", serde_json::json!([{"x":"value"}])),
    ] {
      let schema = format!("{root}\nbranch = {{ ? \"x\" => uint // x: tstr }}");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, json, 0);
      assert!(matches!(
        validator.validate(),
        Err(Error::ResourceLimit { limit: 0 })
      ));
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_initial_paths_do_not_consume_the_budget() {
    let cddl = cddl_from_pest_str("root = [* { x: uint // x: tstr }]").unwrap();
    let json = Value::Array((0..128).map(|n| serde_json::json!({"x":n})).collect());
    let mut validator = limited_validator(&cddl, json, 0);
    assert!(validator.validate().is_ok());
    let cddl = cddl_from_pest_str("root = [* { ? (a: uint // b: uint), * tstr => tstr }]").unwrap();
    let json = Value::Array((0..128).map(|_| serde_json::json!({"a":"text"})).collect());
    let mut validator = limited_validator(&cddl, json, 0);
    assert!(validator.validate().is_ok());
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_absent_optional_choices_do_not_multiply_paths() {
    for form in 0..9 {
      let mut entries = Vec::new();
      let mut rules = Vec::new();
      let mut input = serde_json::json!({"m":1});
      for index in 0..14 {
        let body = if form / 3 == 1 {
          format!("(? p{index}: uint, a{index}: uint // ? q{index}: uint, b{index}: uint)")
        } else {
          if form / 3 == 2 {
            input[format!("a{index}")] = Value::String("text".into());
            input[format!("b{index}")] = Value::String("text".into());
          }
          format!("(a{index}: uint // b{index}: uint)")
        };
        match form % 3 {
          0 => entries.push(format!("? {body}")),
          1 => {
            entries.push(format!("? g{index}"));
            rules.push(format!("g{index} = {body}"));
          }
          _ => {
            entries.push(format!("? g{index}<uint>"));
            rules.push(format!("g{index}<T> = {body}"));
          }
        }
      }
      let schema = format!(
        "root = {{ ? (k: uint // m: uint), {}, * tstr => tstr }} {}",
        entries.join(","),
        rules.join("\n")
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, input.clone(), 1);
      assert!(validator.validate().is_ok(), "{}", schema);
      assert_eq!(
        validator
          .map_replay_budget
          .remaining
          .load(Ordering::Relaxed),
        0
      );
      let mut exhausted = limited_validator(&cddl, input, 0);
      assert!(matches!(
        exhausted.validate(),
        Err(Error::ResourceLimit { limit: 0 })
      ));
    }
    let cddl =
      cddl_from_pest_str("root = { ? (? a: uint, m: uint // ? b: uint, m: uint) }").unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 0);
    assert!(validator.validate().is_ok());
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_optional_impossibility_keeps_effectful_paths() {
    for (body, input, proof) in [
      ("a: uint // b: uint", r#"{"a":"text","b":"text"}"#, true),
      ("? p: uint, a: uint // ? q: uint, b: uint", "{}", true),
      ("? p: uint, a: uint // b: uint", r#"{"p":1}"#, false),
      (
        r#""a" ^ => uint // "b" ^ => uint"#,
        r#"{"a":"text","b":"text"}"#,
        false,
      ),
      (
        "a: uint .size 1 // b: uint",
        r#"{"a":"text","b":"text"}"#,
        false,
      ),
      ("a: uint // b: tstr", r#"{"a":"text","b":"text"}"#, false),
      ("a: uint // b: uint", r#"{"a":[],"b":[]}"#, false),
    ] {
      let schema = format!("root = {{ ? ({body}) }}");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let Rule::Type { rule, .. } = &cddl.rules[0] else {
        panic!("type rule")
      };
      let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
        panic!("map")
      };
      let GroupEntry::InlineGroup { group, .. } = &group.group_choices[0].group_entries[0].0 else {
        panic!("inline group")
      };
      let mut validator = limited_validator(&cddl, serde_json::from_str(input).unwrap(), 0);
      validator.map_choice_search = Some(Arc::new(Mutex::new(MapChoiceSearch::default())));
      assert_eq!(
        validator.optional_choices_definitely_fail(group, None),
        proof,
        "{schema}"
      );
      if input.contains("\"a\":\"text\"") && proof {
        validator.validated_keys = Some(vec!["a".into()]);
        assert!(!validator.optional_choices_definitely_fail(group, None));
        validator.validated_keys = None;
        validator.cut_value = Some("outer".into());
        assert!(!validator.optional_choices_definitely_fail(group, None));
      }
    }
    for schema in [
      "root = { ? (a: uint // b: uint), * tstr => 0 } uint = tstr",
      "root = { ? (a: uint // b: uint), * tstr => 0 } uint /= tstr",
      "root = { ? g<tstr>, * tstr => 0 } g<uint> = (a: uint // b: uint)",
      "root = { ? g<tstr>, * tstr => 0 } g<T> = (a: T // b: T)",
    ] {
      let cddl = cddl_from_pest_str(schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"a":"text"}), 0);
      assert!(validator.validate().is_ok(), "{}", schema);
    }
    for input in [serde_json::json!({"required":1}), serde_json::json!({})] {
      let cddl = cddl_from_pest_str(
        "root = { ? (? p: uint, a: uint // ? q: uint, b: uint), required: uint }",
      )
      .unwrap();
      let mut validator = limited_validator(&cddl, input.clone(), 0);
      assert_eq!(
        validator.validate().is_ok(),
        input.get("required").is_some()
      );
      assert_eq!(
        validator.validate().is_ok(),
        input.get("required").is_some()
      );
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_alias_impossibility_preserves_binding_and_budget_rules() {
    let entries = (0..14)
      .map(|i| format!("? (a{i}: alias // b{i}: alias)"))
      .collect::<Vec<_>>()
      .join(",");
    let schema = format!(
      "root = {{ ? (k: uint // m: uint), {entries}, * tstr => tstr }} alias = link link = uint"
    );
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut input = serde_json::json!({"m":1});
    for i in 0..14 {
      input[format!("a{i}")] = Value::String("text".into());
      input[format!("b{i}")] = Value::String("text".into());
    }
    let mut validator = limited_validator(&cddl, input.clone(), 1);
    assert!(validator.validate().is_ok());
    let mut exhausted = limited_validator(&cddl, input, 0);
    assert!(matches!(
      exhausted.validate(),
      Err(Error::ResourceLimit { limit: 0 })
    ));
    for schema in [
      "root = { ? (a: alias // b: alias), * tstr => 0 } alias = uint uint = tstr",
      "root = { ? (a: alias // b: alias), * tstr => 0 } alias = uint alias /= tstr",
      "root = { ? g<tstr>, * tstr => 0 } g<alias> = (a: alias // b: alias) alias = uint",
    ] {
      let cddl = cddl_from_pest_str(schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"a":"text"}), 0);
      assert!(validator.validate().is_ok(), "{}", schema);
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_unknown_keys_do_not_enumerate_irrelevant_choices() {
    for named in [false, true] {
      let mut entries = Vec::new();
      let mut rules = Vec::new();
      let mut input = serde_json::json!({});
      for i in 0..13 {
        let body = format!("(a{i}: uint // b{i}: uint)");
        if named {
          entries.push(format!("? g{i}"));
          rules.push(format!("g{i} = {body}"));
        } else {
          entries.push(format!("? {body}"));
        }
        input[format!("a{i}")] = Value::from(1);
      }
      let schema = format!("root = {{ {} }} {}", entries.join(","), rules.join("\n"));
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut valid = limited_validator(&cddl, input.clone(), 0);
      assert!(valid.validate().is_ok());
      input["unknown"] = Value::Bool(true);
      let mut invalid = limited_validator(&cddl, input, 0);
      let Error::Validation(errors) = invalid.validate().unwrap_err() else {
        panic!("provably unknown key must not exhaust the replay budget");
      };
      assert_eq!(errors.len(), 1);
      assert!(errors[0].reason.contains("unexpected key \"unknown\""));
    }
    for schema in [
      "root = { fixed: uint // * tstr => any }",
      "root = { fixed: uint // all } all = (* tstr => any)",
      "root = { fixed: uint // unknown: uint, fixed: uint }",
    ] {
      let cddl = cddl_from_pest_str(schema).unwrap();
      let input = serde_json::json!({"fixed":1,"unknown":2});
      let mut valid = limited_validator(&cddl, input.clone(), 1);
      assert!(valid.validate().is_ok(), "{}", schema);
      let mut exhausted = limited_validator(&cddl, input, 0);
      assert!(matches!(
        exhausted.validate(),
        Err(Error::ResourceLimit { limit: 0 })
      ));
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_unknown_key_proof_retains_effects_and_recursive_paths() {
    for schema in [
      "root = { fixed: uint // fixed: uint .size 1 }",
      "root = { fixed: uint // dynamic } dynamic = (fixed: uint) dynamic //= (* tstr => any)",
      "root = { fixed: uint // dynamic<uint> } dynamic<T> = (fixed: T)",
      "root = { fixed: uint // recurse } recurse = (? recurse, fixed: uint)",
      "root = { fixed: uint // \"fixed\" ^ => uint }",
    ] {
      let cddl = cddl_from_pest_str(schema).unwrap();
      let mut validator =
        limited_validator(&cddl, serde_json::json!({"fixed":1,"unknown":true}), 0);
      assert!(
        matches!(validator.validate(), Err(Error::ResourceLimit { limit: 0 })),
        "{}",
        schema
      );
    }
    let cddl =
      cddl_from_pest_str("root = { fixed: any // fixed: branch } branch = { x: uint // x: tstr }")
        .unwrap();
    let mut validator = limited_validator(
      &cddl,
      serde_json::json!({"fixed":{"x":"text"},"unknown":true}),
      1,
    );
    assert!(matches!(
      validator.validate(),
      Err(Error::ResourceLimit { limit: 1 })
    ));

    // Shared named groups form a DAG, not a recursion cycle. Both alternatives
    // must contribute their literal keys before the proof can reject another key.
    let cddl = cddl_from_pest_str(
      "root = { left // right } left = (common,) right = (common, other: uint) common = (fixed: alias) alias = uint",
    ).unwrap();
    let mut valid = limited_validator(&cddl, serde_json::json!({"fixed":1,"other":2}), 1);
    assert!(valid.validate().is_ok());
    let mut invalid = limited_validator(&cddl, serde_json::json!({"fixed":1,"unknown":[]}), 0);
    assert!(matches!(invalid.validate(), Err(Error::Validation(_))));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_alias_proof_declines_cycles_controls_and_incremental_bindings() {
    for (definitions, expected) in [
      ("alias = link link = uint", true),
      ("alias = link link = alias", false),
      ("alias = uint .size 1", false),
      ("alias = uint alias /= tstr", false),
      ("alias = pick<uint> pick<T> = T", false),
      ("alias = any any = uint", false),
    ] {
      let schema = format!("root = {{ ? (a: alias // b: alias) }} {definitions}");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let Rule::Type { rule, .. } = &cddl.rules[0] else {
        panic!("root type")
      };
      let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
        panic!("map")
      };
      let GroupEntry::InlineGroup { group, .. } = &group.group_choices[0].group_entries[0].0 else {
        panic!("choices")
      };
      let mut validator = limited_validator(&cddl, serde_json::json!({"a":"text","b":"text"}), 0);
      validator.map_choice_search = Some(Arc::new(Mutex::new(MapChoiceSearch::default())));
      assert_eq!(
        validator.optional_choices_definitely_fail(group, None),
        expected,
        "{schema}"
      );
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_unknown_key_rejection_restores_failed_candidate_state() {
    let cddl = cddl_from_pest_str("root = { fixed: uint // other: uint }").unwrap();
    let Rule::Type { rule, .. } = &cddl.rules[0] else {
      panic!("root")
    };
    let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("map")
    };
    let mut validator = limited_validator(&cddl, serde_json::json!({"fixed":1,"unknown":true}), 0);
    validator.state.data_location = "/outer".into();
    let outer = Arc::new(Mutex::new(MapChoiceSearch::default()));
    validator.map_choice_search = Some(outer.clone());
    validator
      .visit_closed_map(group, &["fixed".into(), "unknown".into()])
      .unwrap();
    assert_eq!(validator.errors.len(), 1);
    assert!(validator.errors[0]
      .reason
      .contains("unexpected key \"unknown\""));
    assert!(validator.validated_keys.is_none());
    assert!(validator.member_claims.is_empty());
    assert!(validator.object_value.is_none());
    assert!(validator.object_value_key.is_none());
    assert_eq!(validator.state.data_location, "/outer");
    assert!(Arc::ptr_eq(
      validator.map_choice_search.as_ref().unwrap(),
      &outer
    ));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_empty_successful_suffix_choices_do_not_multiply_paths() {
    let suffix = (0..13)
      .map(|i| format!("(? \"a{i}\" => uint // ? \"b{i}\" => uint)"))
      .collect::<Vec<_>>()
      .join(",");
    let schema = format!("root = {{ (? \"k\" => uint // m: uint), {suffix} }}");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
    assert!(validator.validate().is_ok());
    let mut exhausted = limited_validator(&cddl, serde_json::json!({"m":1}), 0);
    assert!(matches!(
      exhausted.validate(),
      Err(Error::ResourceLimit { limit: 0 })
    ));
    let cddl = cddl_from_pest_str("root = { (? \"a\" => uint // ? \"b\" => uint) }").unwrap();
    let mut present = limited_validator(&cddl, serde_json::json!({"b":1}), 1);
    assert!(present.validate().is_ok());
    let mut exhausted = limited_validator(&cddl, serde_json::json!({"b":1}), 0);
    assert!(matches!(
      exhausted.validate(),
      Err(Error::ResourceLimit { limit: 0 })
    ));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_empty_named_suffixes_do_not_multiply_paths() {
    let suffix = (0..13)
      .map(|i| format!("(? g{i} // ? h{i})"))
      .collect::<Vec<_>>()
      .join(",");
    let rules = (0..13)
      .map(|i| format!("g{i} = (a{i}: uint) h{i} = (b{i}: uint)"))
      .collect::<Vec<_>>()
      .join("\n");
    let schema = format!("root = {{ (? \"k\" => uint // m: uint), {suffix} }} {rules}");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
    assert!(validator.validate().is_ok());
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_empty_named_proof_preserves_present_keys_and_depth_errors() {
    let cddl = cddl_from_pest_str("root = { (? g // ? h) } g = (a: uint) h = (b: uint)").unwrap();
    for budget in [0, 1] {
      let mut validator = limited_validator(&cddl, serde_json::json!({"b":1}), budget);
      let result = validator.validate();
      if budget == 0 {
        assert!(matches!(result, Err(Error::ResourceLimit { limit: 0 })));
      } else {
        assert!(result.is_ok());
      }
    }
    let rules = (0..80)
      .map(|i| format!("g{i} = (g{},)", i + 1))
      .collect::<Vec<_>>()
      .join("\n");
    for occurrence in ["?", "0*0"] {
      let schema = format!(
        "root = {{ (? \"k\" => uint // m: uint), (? shallow // {occurrence} g0) }}
         shallow = (absent: uint) {rules} g80 = (never: uint)"
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
      let result = validator.validate();
      if occurrence == "?" {
        assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
      } else {
        assert!(result.is_ok());
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_batch_alias_depth_matches_ordinary_calls() {
    for (entries, pending) in [
      ("* (k0 => uint)", 0),
      ("* fields", 1),
      ("* generic-fields<uint>", 1),
    ] {
      for aliases in [65, 64, 63] {
        let rules = (0..aliases - 1)
          .map(|i| format!("k{i} = k{}", i + 1))
          .collect::<Vec<_>>()
          .join("\n");
        let schema = format!(
          "root = {{ {entries} }} {rules} k{} = tstr
           fields = (k0 => uint) generic-fields<T> = (k0 => T)",
          aliases - 1
        );
        let cddl = cddl_from_pest_str(&schema).unwrap();
        let mut validator = limited_validator(&cddl, serde_json::json!({"a":1}), 0);
        let result = validator.validate();
        if aliases + pending > 64 {
          assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
        } else {
          assert!(result.is_ok());
        }
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_empty_proof_retains_quoted_colon_and_disabled_cut_choices() {
    for member in [
      "0*1 \"a\": uint",
      "*1 \"a\": uint",
      "0*2 \"a\": uint",
      "0*0 \"a\": uint",
      "0*0 \"a\" ^ => uint",
    ] {
      let schema = format!("root = {{ ({member} // ? b: uint) }}");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({}), 1);
      assert!(validator.validate().is_ok(), "{}", member);
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_empty_proof_retains_bounded_bareword_values() {
    let mut failures = Vec::new();
    for occurrence in ["0*1", "*1", "0*2", "0*0", "*", "?"] {
      let schema = format!("root = {{ ({occurrence} a: 1 // ? b: uint) }}");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({}), 1);
      if validator.validate().is_err() {
        failures.push(occurrence);
      }
    }
    assert!(failures.is_empty(), "{:?}", failures);
    for quoted in [false, true] {
      let suffix = (0..13)
        .map(|i| {
          if quoted {
            format!("(? \"a{i}\": 1 // ? \"b{i}\": 1)")
          } else {
            format!("(? a{i}: 1 // ? b{i}: 1)")
          }
        })
        .collect::<Vec<_>>()
        .join(",");
      let schema = format!("root = {{ (? \"k\" => uint // m: uint), {suffix} }}");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
      assert!(validator.validate().is_ok());
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_unknown_key_proof_retains_absent_bareword_value_depth() {
    for aliases in [65, 64] {
      let rules = (0..aliases - 1)
        .map(|i| format!("v{i} = v{}", i + 1))
        .collect::<Vec<_>>()
        .join("\n");
      let schema = format!(
        "root = {{ fixed: uint // 0*1 absent: v0 }} {rules} v{} = uint",
        aliases - 1
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator =
        limited_validator(&cddl, serde_json::json!({"fixed":1,"unknown":true}), 1);
      let result = validator.validate();
      if aliases == 65 {
        assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
      } else {
        assert!(matches!(result, Err(Error::Validation(_))));
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_suppression_preserves_absent_bareword_value_depth() {
    for aliases in [65, 64] {
      let rules = (0..aliases - 1)
        .map(|i| format!("v{i} = v{}", i + 1))
        .collect::<Vec<_>>()
        .join("\n");
      let schema = format!(
        "root = {{ required: uint, (a: uint // 0*1 absent: v0) }} / any
         {rules} v{} = uint",
        aliases - 1
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"required":"text"}), 1);
      let result = validator.validate();
      if aliases == 65 {
        assert!(
          matches!(result, Err(Error::RecursionLimit { limit: 64 })),
          "{:?}",
          result
        );
      } else {
        assert!(result.is_ok());
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_named_optional_proof_preserves_reference_depth() {
    let rules = (0..63)
      .map(|i| format!("g{i} = (g{},)", i + 1))
      .collect::<Vec<_>>()
      .join("\n");
    for reference in ["h", "generic-h<uint>"] {
      for occurrence in ["?", "0*0"] {
        let schema = format!(
          "root = {{ g0 }} {rules} g63 = ({occurrence} {reference},)
           h = (a: uint // b: uint) generic-h<T> = (a: T // b: T)"
        );
        let cddl = cddl_from_pest_str(&schema).unwrap();
        let mut validator = limited_validator(&cddl, serde_json::json!({}), 0);
        let result = validator.validate();
        if occurrence == "?" {
          assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
        } else {
          assert!(result.is_ok());
        }
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_generic_cycles_remain_mismatches_at_depth_boundary() {
    for (enclosing, argument) in [(63, "T"), (64, "T"), (63, "tstr")] {
      let rules = (0..enclosing - 1)
        .map(|i| format!("g{i} = (g{},)", i + 1))
        .collect::<Vec<_>>()
        .join("\n");
      let schema = format!(
        "root = {{ g0 }} {rules} g{} = (cycle<uint>,)
         cycle<T> = (cycle<{argument}> // a: T)",
        enclosing - 1
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"a":1}), 1);
      let result = validator.validate();
      if enclosing == 63 && argument == "T" {
        assert!(result.is_ok());
      } else {
        assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_non_generic_batches_count_named_depth() {
    for enclosing in [64, 63] {
      let rules = (0..enclosing - 1)
        .map(|i| format!("g{i} = (g{},)", i + 1))
        .collect::<Vec<_>>()
        .join("\n");
      let schema = format!(
        "root = {{ g0 }} {rules} g{} = (? h,) h = (tstr => uint)",
        enclosing - 1
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({}), 0);
      let result = validator.validate();
      if enclosing == 64 {
        assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
      } else {
        assert!(result.is_ok());
      }
    }
    for enclosing in [63, 62] {
      let rules = (0..enclosing - 1)
        .map(|i| format!("g{i} = (g{},)", i + 1))
        .collect::<Vec<_>>()
        .join("\n");
      let schema = format!(
        "root = {{ g0 }} {rules} g{} = (? h,)
         h = (tstr => {{ tail }}) tail = (a: uint)",
        enclosing - 1
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"outer":{"a":1}}), 0);
      let result = validator.validate();
      if enclosing == 63 {
        assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
      } else {
        assert!(result.is_ok());
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_suppressed_nested_nodes_preserve_enclosing_effects() {
    let cddl = cddl_from_pest_str(
      "root = { (required: uint, (x: uint // y: uint)),
                (a: uint // b: tstr .regexp \"[\") } / any",
    )
    .unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"b":"x"}), 2);
    assert!(validator
      .validate()
      .unwrap_err()
      .to_string()
      .to_lowercase()
      .contains("regex"));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_zero_minimum_empty_choices_do_not_multiply_paths() {
    for occurrence in ["0*1", "*1", "*", "0*2", "0*0"] {
      let suffix = (0..13)
        .map(|i| format!("({occurrence} \"a{i}\" => uint // {occurrence} \"b{i}\" => uint)"))
        .collect::<Vec<_>>()
        .join(",");
      let schema = format!("root = {{ (? \"k\" => uint // m: uint), {suffix} }}");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
      assert!(validator.validate().is_ok(), "{}", occurrence);
      if occurrence != "0*0" {
        let schema =
          format!("root = {{ ({occurrence} \"a\" => uint // {occurrence} \"b\" => uint) }}");
        let cddl = cddl_from_pest_str(&schema).unwrap();
        let mut validator = limited_validator(&cddl, serde_json::json!({"b":1}), 1);
        assert!(validator.validate().is_ok(), "{}", occurrence);
        let schema = format!(
          "root = {{ ({occurrence} \"a\" => uint //
                      {occurrence} \"b\" => tstr .regexp \"[\") }} / any"
        );
        let cddl = cddl_from_pest_str(&schema).unwrap();
        let mut validator = limited_validator(&cddl, serde_json::json!({"b":"x"}), 1);
        assert!(validator
          .validate()
          .unwrap_err()
          .to_string()
          .to_lowercase()
          .contains("regex"));
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_zero_maximum_names_do_not_enter_depth_slots() {
    let rules = (0..63)
      .map(|i| format!("g{i} = (g{},)", i + 1))
      .collect::<Vec<_>>()
      .join("\n");
    for maximum in [0, 1] {
      let schema =
        format!("root = {{ g0 }} {rules} g63 = (0*{maximum} h<uint>,) h<T> = (tstr => T)");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({}), 0);
      let result = validator.validate();
      if maximum == 0 {
        assert!(result.is_ok());
      } else {
        assert!(matches!(result, Err(Error::RecursionLimit { limit: 64 })));
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_nested_suppression_preserves_enclosing_effects() {
    for (pattern, valid) in [("[", false), ("^x$", true)] {
      let schema =
        format!("root = {{ (required: uint,), (a: uint // b: tstr .regexp \"{pattern}\") }} / any");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"b":"x"}), 2);
      let result = validator.validate();
      assert_eq!(result.is_ok(), valid);
      if !valid {
        assert!(result
          .unwrap_err()
          .to_string()
          .to_lowercase()
          .contains("regex"));
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_wrapped_generic_repetitions_do_not_copy_per_iteration() {
    for schema in [
      "root = { * (g<uint>,) } g<T> = (tstr => T, ? \"x\" => T)",
      "root = { * outer<uint> } outer<T> = (inner<T>,) inner<T> = (tstr => T, ? \"x\" => T)",
    ] {
      let cddl = cddl_from_pest_str(schema).unwrap();
      let input = Value::Object(
        (0..128)
          .map(|i| (format!("k{i:04}"), Value::from(i)))
          .collect(),
      );
      let mut validator = limited_validator(&cddl, input, 0);
      assert!(validator.validate().is_ok());
      assert_eq!(
        validator
          .map_replay_budget
          .generic_input_copies
          .load(Ordering::Relaxed),
        0
      );
      validator.json = serde_json::json!({"bad":false}).into();
      assert!(matches!(validator.validate(), Err(Error::Validation(_))));
      let schema = schema.replace("uint", "tstr .regexp \"[\"");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({"bad":"x"}), 0);
      assert!(validator
        .validate()
        .unwrap_err()
        .to_string()
        .to_lowercase()
        .contains("regex"));
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_required_prefix_failure_does_not_enumerate_suffixes() {
    let suffix = (0..13)
      .map(|i| format!("(a{i}: uint // b{i}: uint)"))
      .collect::<Vec<_>>()
      .join(",");
    let schema = format!("root = {{ required: uint, {suffix}, * tstr => uint }}");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut input = serde_json::json!({});
    for i in 0..13 {
      input[format!("a{i}")] = Value::from(1);
      input[format!("b{i}")] = Value::from(1);
    }
    let mut validator = limited_validator(&cddl, input, 0);
    let Error::Validation(errors) = validator.validate().unwrap_err() else {
      panic!("required prefix must retain its ordinary validation diagnostic")
    };
    assert!(errors
      .iter()
      .any(|e| e.reason.contains("missing key: required")));
    let cddl = cddl_from_pest_str(
      "root = { ? (required: uint, (a: uint // b: uint)), (missing: uint // m: uint) }",
    )
    .unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"m":1}), 1);
    assert!(
      validator.validate().is_ok(),
      "optional rollback must restore replay eligibility"
    );
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_generic_repetition_shares_input() {
    let cddl = cddl_from_pest_str("root = { * g<uint> } g<T> = (tstr => T, ? \"x\" => T)").unwrap();
    for size in [16, 128] {
      let input = Value::Object(
        (0..size)
          .map(|i| (format!("k{i:04}"), Value::from(i)))
          .collect(),
      );
      let mut validator = limited_validator(&cddl, input, 0);
      assert!(validator.validate().is_ok());
      assert_eq!(
        validator
          .map_replay_budget
          .generic_input_copies
          .load(Ordering::Relaxed),
        0
      );
    }
    for entries in [
      "* g<uint>, * g<tstr>",
      "* (g<uint>,), * (g<tstr>,)",
      "* outer<uint>, * outer<tstr>",
    ] {
      let schema =
        format!("root = {{ {entries} }} g<T> = (tstr => T, ? \"x\" => T) outer<T> = (g<T>,)");
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator = limited_validator(
        &cddl,
        serde_json::json!({"a":1,"b":2,"c":"three","x":"four"}),
        0,
      );
      for _ in 0..2 {
        assert!(validator.validate().is_ok());
        assert_eq!(
          validator
            .map_replay_budget
            .generic_input_copies
            .load(Ordering::Relaxed),
          0
        );
      }
      validator.json = serde_json::json!({"a":1,"x":false}).into();
      assert!(matches!(validator.validate(), Err(Error::Validation(_))));
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_depth_errors_are_distinct_shared_and_reset() {
    let input = Value::Object(
      (0..128)
        .map(|i| (format!("k{i:04}"), Value::from(i)))
        .collect(),
    );
    for schema in [
      "root = { g } g = (tstr => uint, ? g)",
      "root = { ? g } g = (tstr => uint, ? g)",
      "root = { g } / any g = (tstr => uint, ? g)",
      "root = { g<uint> } g<T> = (tstr => T, ? g<T>)",
      "root = { g } g //= (tstr => uint, ? g)",
    ] {
      let cddl = cddl_from_pest_str(schema).unwrap();
      let mut validator = limited_validator(&cddl, input.clone(), 0);
      assert!(
        matches!(
          validator.validate(),
          Err(Error::RecursionLimit {
            limit: MAX_ACTIVE_MAP_RULE_CALLS
          })
        ),
        "{}",
        schema
      );
      assert_eq!(validator.map_rule_depth.active.load(Ordering::Relaxed), 0);
      validator.json = serde_json::json!({"small":1}).into();
      assert!(validator.validate().is_ok(), "{}", schema);
    }
    for schema in ["root = { g } g = (g,)", "root = { g<uint> } g<T> = (g<T>,)"] {
      let cddl = cddl_from_pest_str(schema).unwrap();
      let mut validator = limited_validator(&cddl, serde_json::json!({}), 0);
      assert!(matches!(validator.validate(), Err(Error::Validation(_))));
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_unknown_key_proof_preserves_named_depth_errors() {
    let rules = (0..80)
      .map(|i| format!("g{i} = (g{},)", i + 1))
      .collect::<Vec<_>>()
      .join("\n");
    // Analyze the shared suffix first; a later caller has less remaining depth.
    let schema = format!("root = {{ fixed: uint // g0 // g20 }} {rules} g80 = (fixed: uint)");
    let cddl = cddl_from_pest_str(&schema).unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"fixed":1,"unknown":true}), 1);
    assert!(matches!(
      validator.validate(),
      Err(Error::RecursionLimit { .. })
    ));
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_proven_optional_choices_preserve_required_diagnostics() {
    for domain in ["alias", "uint"] {
      let schema = format!(
        "root = {{ ? (a: {domain} // b: {domain}), required: uint, c: uint }} alias = uint"
      );
      let cddl = cddl_from_pest_str(&schema).unwrap();
      let mut validator =
        limited_validator(&cddl, serde_json::json!({"a":"text","b":"text","c":1}), 1);
      let Error::Validation(errors) = validator.validate().unwrap_err() else {
        panic!("ordinary required mismatch")
      };
      assert_eq!(errors.len(), 1);
      assert!(errors[0].reason.contains("missing key: required"));
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_complete_remaining_pairs_can_batch_after_movable_claims() {
    for (schema, eligible, valid) in [
      ("root = { * (tstr => uint), * (tstr => tstr) }", true, true),
      (
        "root = { * fields<uint>, * fields<tstr> } fields<T> = (tstr => T)",
        true,
        true,
      ),
      (
        "root = { 0*1 (tstr => uint), * (tstr => tstr), * tstr => uint }",
        false,
        true,
      ),
      (
        "root = { * (tstr => uint), * (tstr => (tstr .size 1)) }",
        false,
        true,
      ),
      (
        "root = { * (tstr => tstr), * (tstr => uint) } uint = tstr",
        false,
        false,
      ),
    ] {
      let cddl = cddl_from_pest_str(schema).unwrap();
      let Rule::Type { rule, .. } = &cddl.rules[0] else {
        panic!("expected type rule");
      };
      let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
        panic!("expected map");
      };
      let entries = &group.group_choices[0].group_entries;
      let mut validator =
        limited_validator(&cddl, serde_json::json!({"a":1,"b":2,"c":"x","d":"y"}), 0);
      validator.visit_group_entry(&entries[0].0).unwrap();
      assert!(!validator.member_claims.is_empty());
      let member = match &entries[1].0 {
        GroupEntry::InlineGroup { group, .. } => validator
          .batchable_map_member(group, None, &validator, 0)
          .cloned(),
        GroupEntry::TypeGroupname { ge, .. } => validator.batchable_named_map_member(ge, false),
        _ => panic!("expected repeated group"),
      };
      assert_eq!(member.is_some(), eligible, "{schema}");
      assert_eq!(validator.validate().is_ok(), valid, "{schema}");
    }
    let cddl =
      cddl_from_pest_str("root = { tstr => any, * fields<uint, tstr> } fields<T> = (tstr => T)")
        .unwrap();
    let mut validator = limited_validator(&cddl, serde_json::json!({"a":1,"b":"x"}), 0);
    assert!(validator.validate().is_ok());
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_pruned_coverage_resets_on_reuse() {
    let cddl = cddl_from_pest_str("root = { (a: uint // b: uint), c: uint, d: uint }").unwrap();
    let input = serde_json::json!({"a":1,"c":"x","d":1});
    let mut validator = limited_validator(&cddl, input.clone(), 1);
    let error = validator.validate().unwrap_err().to_string();
    assert!(error.contains("expected type uint"));
    assert!(!error.contains("unexpected key \"d\""));
    validator.json = serde_json::json!({"a":1,"c":1,"d":1,"extra":2}).into();
    assert!(validator
      .validate()
      .unwrap_err()
      .to_string()
      .contains("unexpected key \"extra\""));
    validator.json = serde_json::json!({"a":1,"c":1,"d":1}).into();
    assert!(validator.validate().is_ok());
    let mut exhausted = limited_validator(&cddl, input, 0);
    assert!(matches!(
      exhausted.validate(),
      Err(Error::ResourceLimit { limit: 0 })
    ));
  }

  fn reference_assignment(
    claim: usize,
    rows: &[Arc<Vec<bool>>],
    visited: &mut [bool],
    owners: &mut [Option<usize>],
    checks: &mut usize,
  ) -> bool {
    for pair in 0..rows[claim].len() {
      *checks += 1;
      if !rows[claim][pair] || visited[pair] {
        continue;
      }
      visited[pair] = true;
      if owners[pair].is_none_or(|owner| reference_assignment(owner, rows, visited, owners, checks))
      {
        owners[pair] = Some(claim);
        return true;
      }
    }
    false
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_class_cursors_preserve_dfs_assignments() {
    for pattern in 0usize..512 {
      for share_rows in [false, true] {
        let mut rows: ClaimCompatibility = Vec::new();
        for claim in 0..3 {
          let values: Vec<_> = (0..3)
            .map(|pair| pattern & (1 << (claim * 3 + pair)) != 0)
            .collect();
          let row = if share_rows {
            rows
              .iter()
              .find(|row| row.as_ref() == &values)
              .cloned()
              .unwrap_or_else(|| Arc::new(values))
          } else {
            Arc::new(values)
          };
          rows.push(row);
        }
        let (classes, count) = compatibility_classes(&rows);
        let mut actual = vec![None; 3];
        let mut expected = actual.clone();
        for claim in 0..3 {
          let matched = augment_choice_assignment(
            claim,
            &rows,
            &mut [false; 3],
            &mut actual,
            false,
            &classes,
            &mut vec![0; count],
          );
          let reference =
            reference_assignment(claim, &rows, &mut [false; 3], &mut expected, &mut 0);
          assert_eq!(matched, reference);
          assert_eq!(actual, expected, "graph {pattern}, sharing {share_rows}");
        }
      }
    }
  }

  #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test::wasm_bindgen_test)]
  #[cfg_attr(not(target_arch = "wasm32"), test)]
  fn map_replay_mixed_classes_bound_pair_scans() {
    let pairs = 257;
    let mut rows = vec![Arc::new(vec![true; pairs]); pairs - 1];
    rows.push(Arc::new(vec![true; pairs]));
    let (classes, class_count) = compatibility_classes(&rows);
    let mut actual = vec![None; pairs];
    let mut expected = actual.clone();
    let mut scans = 0;
    let mut reference_scans = 0;
    for claim in 0..rows.len() {
      let mut positions = vec![0; class_count];
      assert!(augment_choice_assignment(
        claim,
        &rows,
        &mut vec![false; pairs],
        &mut actual,
        false,
        &classes,
        &mut positions,
      ));
      scans += positions.iter().sum::<usize>();
      assert!(reference_assignment(
        claim,
        &rows,
        &mut vec![false; pairs],
        &mut expected,
        &mut reference_scans,
      ));
      assert_eq!(actual, expected);
    }
    assert!(scans > 0 && scans <= class_count * pairs * rows.len());
    assert!(reference_scans > scans * 8);
    #[cfg(not(target_arch = "wasm32"))]
    eprintln!("mixed-class pair scans: {scans}; reference: {reference_scans}");
  }
}

#[cfg(test)]
#[cfg(not(target_arch = "wasm32"))]
mod tests {
  #![allow(unused_imports)]

  use super::*;
  use indoc::indoc;

  #[test]
  fn productive_flat_map_recursion_returns_a_distinct_hard_error() {
    // The larger stack makes the pre-fix bounded witness safe to execute.
    std::thread::Builder::new()
      .stack_size(8 * 1024 * 1024)
      .spawn(|| {
        let cddl = cddl_from_str("root = { g } g = (tstr => uint, ? g)", true).unwrap();
        for count in [8, 128, 4096] {
          let input = Value::Object(
            (0..count)
              .map(|i| (format!("k{i:04}"), Value::from(i)))
              .collect(),
          );
          let mut validator = JSONValidator::new(
            &cddl,
            input,
            #[cfg(feature = "additional-controls")]
            None,
          );
          let result = validator.validate();
          if count == 8 {
            assert!(result.is_ok());
          } else {
            assert!(matches!(result, Err(Error::RecursionLimit { .. })));
          }
        }
      })
      .unwrap()
      .join()
      .unwrap();
  }

  #[test]
  fn stale_map_replay_index_returns_an_error_without_panicking() -> std::result::Result<(), String>
  {
    let cddl = cddl_from_str("root = { a: uint // b: uint }", true)?;
    let Rule::Type { rule, .. } = &cddl.rules[0] else {
      panic!("expected type rule");
    };
    let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("expected map");
    };
    #[cfg(feature = "additional-controls")]
    let mut validator = JSONValidator::new(&cddl, serde_json::json!({"a": 1}), None);
    #[cfg(not(feature = "additional-controls"))]
    let mut validator = JSONValidator::new(&cddl, serde_json::json!({"a": 1}));
    validator.map_choice_search = Some(Arc::new(Mutex::new(MapChoiceSearch {
      path: vec![2],
      ..MapChoiceSearch::default()
    })));
    let error = validator.visit_group(group).unwrap_err();
    assert!(error
      .to_string()
      .contains("invalid map group-choice replay"));
    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn map_transactions_restore_state_after_hard_errors() {
    let cddl = cddl_from_str(r#"root = { ? "a" => uint .feature 'enabled' }"#, true).unwrap();
    let Rule::Type { rule, .. } = &cddl.rules[0] else {
      panic!("type rule")
    };
    let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("map type")
    };
    let mut group = group.clone();
    let GroupEntry::ValueMemberKey { ge, .. } = &mut group.group_choices[0].group_entries[0].0
    else {
      panic!("member entry")
    };
    ge.entry_type.type_choices[0]
      .type1
      .operator
      .as_mut()
      .unwrap()
      .type2 = Type2::from(token::ByteValue::UTF8(vec![0xff].into()));
    for whole_map in [false, true] {
      let mut validator = JSONValidator::new(
        &cddl,
        serde_json::json!({"a": 1, "prior": true}),
        Some(&["enabled"]),
      );
      validator.validated_keys = Some(vec!["prior".into()]);
      validator.state.data_location = "/root".into();
      let result = if whole_map {
        validator.visit_closed_map(&group, &["a".into(), "prior".into()])
      } else {
        validator.visit_zero_minimum_entry(&group.group_choices[0].group_entries[0].0, Some(1))
      };
      assert!(matches!(result, Err(Error::UTF8Parsing(_))));
      assert_eq!(validator.validated_keys, Some(vec!["prior".into()]));
      assert_eq!(validator.state.data_location, "/root");
      assert!(validator.state.occurrence.is_none());
      assert!(validator.object_value.is_none());
      assert!(validator.object_value_key.is_none());
      assert!(validator.map_choice_search.is_none());
    }
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn optional_impossibility_proof_preserves_hard_error_order_and_rollback() {
    let cddl = cddl_from_str(
      "root = { ? (? \"a\" => uint .feature 'enabled', x: uint // b: uint) }",
      true,
    )
    .unwrap();
    let Rule::Type { rule, .. } = &cddl.rules[0] else {
      panic!("type rule")
    };
    let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("map")
    };
    let mut group = group.clone();
    let GroupEntry::InlineGroup { group: choices, .. } =
      &mut group.group_choices[0].group_entries[0].0
    else {
      panic!("inline group")
    };
    let GroupEntry::ValueMemberKey { ge, .. } = &mut choices.group_choices[0].group_entries[0].0
    else {
      panic!("prefix")
    };
    ge.entry_type.type_choices[0]
      .type1
      .operator
      .as_mut()
      .unwrap()
      .type2 = Type2::from(token::ByteValue::UTF8(vec![0xff].into()));
    for present in [false, true] {
      let input = if present {
        serde_json::json!({"a":1})
      } else {
        serde_json::json!({})
      };
      let mut validator = JSONValidator::new(&cddl, input, Some(&["enabled"]));
      validator.state.data_location = "/root".into();
      validator.state.is_multi_group_choice = true;
      let outer = Arc::new(Mutex::new(MapChoiceSearch {
        path: vec![1],
        widths: vec![2],
        ..MapChoiceSearch::default()
      }));
      validator.map_choice_search = Some(outer.clone());
      let result = validator.visit_closed_map(&group, &[]);
      if present {
        assert!(matches!(result, Err(Error::UTF8Parsing(_))));
      } else {
        assert!(result.is_ok());
      }
      assert_eq!(validator.state.data_location, "/root");
      assert!(validator.state.occurrence.is_none());
      assert!(validator.validated_keys.is_none());
      assert!(validator.member_claims.is_empty());
      assert!(validator.deferred_map_errors.is_empty());
      assert!(validator.state.is_multi_group_choice);
      assert!(Arc::ptr_eq(
        validator.map_choice_search.as_ref().unwrap(),
        &outer
      ));
      let outer = outer.lock().unwrap();
      assert_eq!(outer.path, [1]);
      assert_eq!(outer.widths, [2]);
    }
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn optional_impossibility_proof_preserves_later_required_pruning() {
    for (domain, present, occur) in [
      ("alias", true, "?"),
      ("uint", true, "?"),
      ("uint", false, "?"),
      ("uint", true, "0*0"),
    ] {
      let schema = format!(
        "root = {{ {occur} (a: {domain} // b: {domain}), required: uint, c: uint .feature 'enabled' }} alias = uint"
      );
      let cddl = cddl_from_str(&schema, true).unwrap();
      let Rule::Type { rule, .. } = &cddl.rules[0] else {
        panic!("type rule")
      };
      let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
        panic!("map")
      };
      let mut group = group.clone();
      let GroupEntry::ValueMemberKey { ge, .. } = &mut group.group_choices[0].group_entries[2].0
      else {
        panic!("last entry")
      };
      ge.entry_type.type_choices[0]
        .type1
        .operator
        .as_mut()
        .unwrap()
        .type2 = Type2::from(token::ByteValue::UTF8(vec![0xff].into()));
      let input = if present {
        serde_json::json!({"a":"text","b":"text","c":1})
      } else {
        serde_json::json!({"c":1})
      };
      let mut validator = JSONValidator::new(&cddl, input, Some(&["enabled"]));
      let result = validator.visit_closed_map(&group, &["a".into(), "b".into(), "c".into()]);
      if present && occur != "0*0" {
        assert!(result.is_ok(), "{}: {:?}", domain, result);
        assert!(validator
          .errors
          .iter()
          .any(|e| e.reason.contains("missing key: required")));
      } else {
        assert!(matches!(result, Err(Error::UTF8Parsing(_))));
      }
      assert!(validator.member_claims.is_empty());
      assert!(validator.map_choice_search.is_none());
    }
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn unknown_key_proof_preserves_later_alternative_hard_errors() {
    for aliased in [false, true] {
      let schema = if aliased {
        "root = { fixed: uint // fixed: alias } alias = uint .feature 'enabled'"
      } else {
        "root = { fixed: uint // fixed: uint .feature 'enabled' }"
      };
      let mut cddl = cddl_from_str(schema, true).unwrap();
      let controller = if aliased {
        let Rule::Type { rule, .. } = &mut cddl.rules[1] else {
          panic!("alias")
        };
        &mut rule.value.type_choices[0].type1
      } else {
        let Rule::Type { rule, .. } = &mut cddl.rules[0] else {
          panic!("root")
        };
        let Type2::Map { group, .. } = &mut rule.value.type_choices[0].type1.type2 else {
          panic!("map")
        };
        let GroupEntry::ValueMemberKey { ge, .. } = &mut group.group_choices[1].group_entries[0].0
        else {
          panic!("alternative")
        };
        &mut ge.entry_type.type_choices[0].type1
      };
      controller.operator.as_mut().unwrap().type2 =
        Type2::from(token::ByteValue::UTF8(vec![0xff].into()));
      let mut validator = JSONValidator::new(
        &cddl,
        serde_json::json!({"fixed":1,"unknown":true}),
        Some(&["enabled"]),
      );
      assert!(matches!(validator.validate(), Err(Error::UTF8Parsing(_))));
      assert!(validator.member_claims.is_empty());
      assert!(validator.map_choice_search.is_none());
    }
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn required_prefix_failure_preserves_effectful_suffix_replays() {
    let cddl = cddl_from_str(
      "root = { required: uint, (a: uint // b: uint), c: uint .feature 'enabled' }",
      true,
    )
    .unwrap();
    let Rule::Type { rule, .. } = &cddl.rules[0] else {
      panic!("root")
    };
    let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("map")
    };
    let mut group = group.clone();
    let GroupEntry::ValueMemberKey { ge, .. } = &mut group.group_choices[0].group_entries[2].0
    else {
      panic!("suffix")
    };
    ge.entry_type.type_choices[0]
      .type1
      .operator
      .as_mut()
      .unwrap()
      .type2 = Type2::from(token::ByteValue::UTF8(vec![0xff].into()));
    for input in [
      serde_json::json!({"a":1,"c":1}),
      serde_json::json!({"a":"bad","b":1,"c":1}),
    ] {
      let mut validator = JSONValidator::new(&cddl, input, Some(&["enabled"]));
      assert!(matches!(
        validator.visit_closed_map(&group, &["a".into(), "b".into(), "c".into()]),
        Err(Error::UTF8Parsing(_)),
      ));
    }
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn reused_generic_input_preserves_later_iteration_hard_errors() {
    let mut cddl = cddl_from_str(
      "root = { * g<val> } g<T> = (tstr => T, ? \"x\" => T) val = uint / tstr .feature 'enabled'",
      true,
    )
    .unwrap();
    let Rule::Type { rule, .. } = &mut cddl.rules[2] else {
      panic!("value alias")
    };
    rule.value.type_choices[1]
      .type1
      .operator
      .as_mut()
      .unwrap()
      .type2 = Type2::from(token::ByteValue::UTF8(vec![0xff].into()));
    let mut validator = JSONValidator::new(
      &cddl,
      serde_json::json!({"a":1,"z":"text"}),
      Some(&["enabled"]),
    );
    for _ in 0..2 {
      assert!(matches!(validator.validate(), Err(Error::UTF8Parsing(_))));
      assert_eq!(validator.map_rule_depth.active.load(Ordering::Relaxed), 0);
    }
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn empty_choice_proof_retains_present_optional_hard_errors() {
    let cddl = cddl_from_str(
      "root = { (? \"a\" => uint // ? \"b\" => uint .feature 'enabled') }",
      true,
    )
    .unwrap();
    let Rule::Type { rule, .. } = &cddl.rules[0] else {
      panic!("root")
    };
    let Type2::Map { group, .. } = &rule.value.type_choices[0].type1.type2 else {
      panic!("map")
    };
    let mut group = group.clone();
    let GroupEntry::InlineGroup { group: inner, .. } =
      &mut group.group_choices[0].group_entries[0].0
    else {
      panic!("choices")
    };
    let GroupEntry::ValueMemberKey { ge, .. } = &mut inner.group_choices[1].group_entries[0].0
    else {
      panic!("optional value")
    };
    ge.entry_type.type_choices[0]
      .type1
      .operator
      .as_mut()
      .unwrap()
      .type2 = Type2::from(token::ByteValue::UTF8(vec![0xff].into()));
    let mut validator = JSONValidator::new(&cddl, serde_json::json!({"b":1}), Some(&["enabled"]));
    assert!(matches!(
      validator.visit_closed_map(&group, &["b".into()]),
      Err(Error::UTF8Parsing(_))
    ));
  }

  #[test]
  fn validate_number_accepts_float_and_int() -> std::result::Result<(), Box<dyn std::error::Error>>
  {
    let cddl = cddl_from_str("x = number", true).unwrap();

    for json in ["2.5", "5"] {
      let json = serde_json::from_str::<serde_json::Value>(json)?;
      let mut jv = JSONValidator::new(
        &cddl,
        json.clone(),
        #[cfg(feature = "additional-controls")]
        None,
      );
      assert!(jv.validate().is_ok(), "number should accept {}", json);
    }

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn validate_plus() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        interval<BASE> = (
          "test" => BASE .plus a
        )
    
        rect = {
          interval<X>
        }
        X = 0
        a = 10
      "#
    );
    let json = r#"{ "test": 10 }"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    jv.validate()?;

    Ok(())
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn validate_feature() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        v = JC<"v", 2>
        JC<J, C> =  C .feature "cbor" / J .feature "json"
      "#
    );

    let json = r#""v""#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(&cddl, json, Some(&["json"]));
    jv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_type_choice_alternate() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        tester = [ $vals ]
        $vals /= 12
        $vals /= 13
      "#
    );

    let json = r#"[ 13 ]"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    jv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_group_choice_alternate() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        tester = $$vals
        $$vals //= 18
        $$vals //= 12
      "#
    );

    let json = r#"15"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    jv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_group_choice_alternate_in_array_1(
  ) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        tester = [$$val]
        $$val //= (
          type: 10,
          data: uint,
          t: 11
        )
        $$val //= (
          type: 11,
          data: tstr
        )
      "#
    );

    let json = r#"[10, 11, 11]"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    jv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_group_choice_alternate_in_array_2(
  ) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        tester = [$$val]
        $$val //= (
          type: 10,
          extra,
        )
        extra = (
          something: uint,
        )
      "#
    );

    let json = r#"[10, 1]"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    jv.validate()?;

    Ok(())
  }

  #[test]
  fn size_control_validation_error() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        start = Record
        Record = {
          id: Id
        }
        Id = uint .size 8
      "#
    );

    let json = r#"{ "id": 5 }"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    jv.validate()?;

    Ok(())
  }

  #[test]
  fn validate_occurrences_in_object() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl = indoc!(
      r#"
        limited = { 1* tstr => tstr }
      "#
    );

    let json = r#"{ "A": "B" }"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    jv.validate().unwrap();

    Ok(())
  }

  #[test]
  fn validate_optional_occurrences_in_object() -> std::result::Result<(), Box<dyn std::error::Error>>
  {
    let cddl = indoc!(
      r#"
        argument = {
          name: text,
          ? valid: "yes" / "no",
        }
      "#
    );

    let json = r#"{
      "name": "foo",
      "valid": "no"
    }"#;

    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing);
    if let Err(e) = &cddl {
      println!("{}", e);
    }

    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let cddl = cddl.unwrap();

    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    jv.validate().unwrap();

    Ok(())
  }

  #[test]
  fn validate_range_operators() -> std::result::Result<(), Box<dyn std::error::Error>> {
    // Test inclusive range (..)
    let cddl = indoc!(
      r#"
        thing = 5..10
        "#
    );

    let json = serde_json::json!(5); // Should pass
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(10); // Should pass
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(4); // Should fail
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_err());

    let json = serde_json::json!(11); // Should fail
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_err());

    // Test inclusive-exclusive range (...)
    let cddl = indoc!(
      r#"
        thing = 5...10
        "#
    );
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;

    let json = serde_json::json!(5); // Should pass
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(9); // Should pass
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(10); // Should fail (exclusive upper bound)
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_err());

    let json = serde_json::json!(4); // Should fail
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_err());

    // Test inclusive-exclusive range with floats
    let cddl = indoc!(
      r#"
        thing = 1.5...3.5
        "#
    );
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;

    let json = serde_json::json!(1.5); // Should pass
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(2.5); // Should pass
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_ok());

    let json = serde_json::json!(3.5); // Should fail (exclusive upper bound)
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_err());

    let json = serde_json::json!(1.0); // Should fail
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_err());

    // Test range with string length using .size control
    let cddl = indoc!(
      r#"
        thing = tstr .size (2...5)
        "#
    );
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;

    let json = serde_json::json!("ab"); // Length 2 - should pass
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_ok());

    let json = serde_json::json!("abcd"); // Length 4 - should pass
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_ok());

    let json = serde_json::json!("abcde"); // Length 5 - should fail (exclusive)
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_err());

    let json = serde_json::json!("a"); // Length 1 - should fail
    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    assert!(jv.validate().is_err());

    Ok(())
  }

  #[test]
  fn validate_nested_arrays() -> std::result::Result<(), Box<dyn std::error::Error>> {
    // Test with explicit array type
    let cddl = indoc!(
      r#"
        array = [0, [* int]]
      "#
    );

    let json = r#"[0, [1, 2]]"#;
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;
    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    jv.validate()?;

    // Test with named arrays
    let cddl = indoc!(
      r#"
        root = [0, inner]
        inner = [* int]
      "#
    );

    let json = r#"[0, [1, 2]]"#;
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;
    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    jv.validate()?;

    // Test with explicit array literals
    let cddl = indoc!(
      r#"
        direct = [1, [2, 3]]
      "#
    );

    let json = r#"[1, [2, 3]]"#;
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;
    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );
    jv.validate()?; // If this passes, our fix works

    Ok(())
  }

  #[test]
  fn validate_direct_nested_array() -> std::result::Result<(), Box<dyn std::error::Error>> {
    // Simplest possible case with explicit literal array
    let cddl = indoc!(
      r#"
        direct = [1, [2, 3]]
        "#
    );

    let json = r#"[1, [2, 3]]"#;
    let cddl = cddl_from_str(cddl, true).map_err(json::Error::CDDLParsing)?;
    let json = serde_json::from_str::<serde_json::Value>(json).map_err(json::Error::JSONParsing)?;

    let mut jv = JSONValidator::new(
      &cddl,
      json,
      #[cfg(feature = "additional-controls")]
      None,
    );

    // Print detailed information for debugging
    match jv.validate() {
      Ok(_) => println!("Validation successful!"),
      Err(e) => {
        eprintln!("Validation error: {}", e);
        if let Error::Validation(errors) = &e {
          for (i, err) in errors.iter().enumerate() {
            eprintln!("Error {}: {} at {}", i, err.reason, err.json_location);
          }
        }
        return Err(e.into());
      }
    }

    Ok(())
  }

  #[test]
  fn test_issue_221_empty_map_with_extra_keys() {
    // This test reproduces issue #221
    // CDDL schema defines an empty map: root = {}
    // JSON has extra keys: {"x": "y"}
    // This should FAIL validation but currently passes

    let cddl_str = "root = {}";
    let json_str = r#"{"x": "y"}"#;

    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_str, json_str, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_str, json_str);

    // This should fail but currently passes (the bug)
    assert!(
      result.is_err(),
      "Validation should fail for extra keys in empty map schema"
    );

    // Verify the error message is what we expect
    if let Err(Error::Validation(errors)) = result {
      assert_eq!(errors.len(), 1);
      assert!(errors[0].reason.contains("expected empty map"));
    } else {
      panic!("Expected validation error but got something else");
    }
  }

  #[test]
  fn test_empty_map_schema_with_empty_json() {
    // This should pass - empty map schema with empty JSON map
    let cddl_str = "root = {}";
    let json_str = r#"{}"#;

    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_str, json_str, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_str, json_str);

    assert!(
      result.is_ok(),
      "Validation should pass for empty map with empty map schema"
    );
  }

  #[test]
  fn test_issue_174_choice_validation() {
    // Test for issue #174 - Choice validates in case where neither option validates
    let cddl_str = indoc!(
      r#"
        Root = Choice1 / Choice2

        Choice1 = {
           id1: text,
           ?id2: text,
           Extensible
        }

        Choice2 = {
           ?id1: text,
           id2: text,
           Extensible
        }

        Extensible = (*text => any)
      "#
    );

    // This JSON should fail because id2 is not text in either choice
    let json_str = r#"{"id1": "example", "id2": 2}"#;

    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_str, json_str, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_str, json_str);

    // This should fail validation since id2 is a number, not text
    match result {
      Err(Error::Validation(errors)) => {
        assert!(!errors.is_empty(), "Should have validation errors");
        // We expect errors related to type mismatch
      }
      Ok(_) => {
        panic!("Issue #174 bug detected: validation incorrectly passed when neither choice should validate")
      }
      Err(other) => panic!("Unexpected error type: {:?}", other),
    }

    // Test valid cases still work
    let valid_cases = [
      r#"{"id1": "example", "id2": "text"}"#, // Both fields as text
      r#"{"id1": "example"}"#,                // Only id1 (matches Choice1)
      r#"{"id2": "text"}"#,                   // Only id2 (matches Choice2)
    ];

    for valid_json in valid_cases.iter() {
      #[cfg(feature = "additional-controls")]
      let result = validate_json_from_str(cddl_str, valid_json, None);
      #[cfg(not(feature = "additional-controls"))]
      let result = validate_json_from_str(cddl_str, valid_json);

      match result {
        Ok(_) => {} // Expected
        Err(e) => panic!(
          "Valid JSON should pass validation: {}, error: {:?}",
          valid_json, e
        ),
      }
    }
  }

  #[test]
  fn test_issue_221_reproduce_exact_scenario() {
    // This test reproduces the exact scenario from issue #221
    let cddl_str = "root = {}";
    let json_str = r#"{"x": "y"}"#;

    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_str, json_str, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_str, json_str);

    // Before the fix, this would incorrectly pass. After the fix, it should fail.
    match result {
      Err(Error::Validation(errors)) => {
        assert!(!errors.is_empty(), "Should have validation errors");
        let error_message = &errors[0].reason;
        assert!(
          error_message.contains("expected empty map"),
          "Error message should indicate expected empty map, got: {}",
          error_message
        );
      }
      Ok(_) => {
        panic!("Issue #221 bug detected: validation incorrectly passed for extra keys in empty map")
      }
      Err(other) => panic!("Unexpected error type: {:?}", other),
    }
  }

  #[cfg(feature = "additional-controls")]
  #[test]
  fn validate_abnf_with_det_typename_controller(
  ) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let cddl_str = indoc!(
      r#"
        start = sdf-syntax

        sdf-syntax = {
          ? info: sdfinfo
        }

        sdfinfo = {
          ? modified: modified-date-time
        }

        modified-date-time = text .abnf modified-dt-abnf
        modified-dt-abnf = "modified-dt" .det rfc3339z

        rfc3339z = '
           date-fullyear   = 4DIGIT
           date-month      = 2DIGIT
           date-mday       = 2DIGIT
           time-hour       = 2DIGIT
           time-minute     = 2DIGIT
           time-second     = 2DIGIT
           time-secfrac    = "." 1*DIGIT
           DIGIT           =  %x30-39
           partial-time    = time-hour ":" time-minute ":" time-second [time-secfrac]
           full-date       = date-fullyear "-" date-month "-" date-mday
           modified-dt     = full-date ["T" partial-time "Z"]
        '
      "#
    );

    let json_str = r#"{ "info": { "modified": "1985-04-12T23:20:50.52Z" } }"#;

    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_str, json_str, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_str, json_str);

    assert!(
      result.is_ok(),
      "Expected validation to succeed for datetime value against .abnf with .det typename controller, got: {:?}",
      result.err()
    );

    // Also test that an invalid datetime is correctly rejected
    let invalid_json_str = r#"{ "info": { "modified": "not-a-date" } }"#;

    #[cfg(feature = "additional-controls")]
    let result = validate_json_from_str(cddl_str, invalid_json_str, None);
    #[cfg(not(feature = "additional-controls"))]
    let result = validate_json_from_str(cddl_str, invalid_json_str);

    assert!(
      result.is_err(),
      "Expected validation to fail for invalid datetime value"
    );

    Ok(())
  }
}
