use super::*;

fn single_plain_typename<'a, 'b>(value_type: &'b Type<'a>) -> Option<&'b Identifier<'a>> {
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
  Some(ident)
}

impl<'a> JSONValidator<'a> {
  pub(super) fn map_rule_depth_allows(&self, additional: usize) -> bool {
    self.map_choice_search.is_none()
      || self
        .map_rule_depth
        .active
        .load(Ordering::Relaxed)
        .saturating_add(additional)
        <= self.map_rule_depth.limit
  }

  pub(super) fn batch_key_is_parameter(
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

  pub(super) fn batch_key_depth(
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
      ident = single_plain_typename(&rule.value)?;
    }
  }

  pub(super) fn batchable_map_member<'b>(
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

  pub(super) fn batch_value_predicate(&self, value_type: &Type<'a>) -> Option<fn(&Value) -> bool> {
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

  pub(super) fn proven_scalar_type(
    &self,
    value_type: &Type<'a>,
    params: Option<&GenericParams<'a>>,
  ) -> Option<ProvenScalarType> {
    if self.state.generic_rules.iter().any(|rule| {
      Some(rule.name) == self.state.eval_generic_rule && rule.args.len() != rule.params.len()
    }) {
      return None;
    }
    let mut ident = single_plain_typename(value_type)?;
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
      ident = single_plain_typename(&rule.value)?;
    }
  }

  pub(super) fn provably_unknown_map_key(&self, group: &Group<'a>) -> bool {
    let Value::Object(object) = self.json.as_ref() else {
      return false;
    };
    if self.state.ctrl.is_some()
      || self.state.occurrence.is_some()
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

  pub(super) fn finish_literal_key_proof<'b>(
    &'b self,
    proof: &mut LiteralKeyProof<'b, 'a>,
  ) -> bool {
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

  pub(super) fn add_literal_key_proof<'b>(
    &'b self,
    entry: &'b GroupEntry<'a>,
    proof: &mut LiteralKeyProof<'b, 'a>,
  ) -> Option<()> {
    if let GroupEntry::ValueMemberKey { ge, .. } = entry {
      if member_key_is_cut(ge) || non_optional_colon_occurrence(entry) {
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
      GroupEntry::InlineGroup { occur, group, .. } => {
        if occur.is_some() && group_entry_bounds(entry).0 > 0 {
          return None;
        }
        proof.push_group(group);
      }
      GroupEntry::TypeGroupname { ge, .. } => self.add_named_key_proof(ge, proof)?,
    }
    Some(())
  }

  pub(super) fn add_named_key_proof<'b>(
    &'b self,
    entry: &'b TypeGroupnameEntry<'a>,
    proof: &mut LiteralKeyProof<'b, 'a>,
  ) -> Option<()> {
    if entry
      .occur
      .as_ref()
      .is_some_and(|occur| occurrence_bounds(Some(occur.occur)).0 > 0)
      || entry.generic_args.is_some()
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

  pub(super) fn batch_covers_unclaimed_values(
    &self,
    predicate: Option<fn(&Value) -> bool>,
  ) -> bool {
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

  pub(super) fn batchable_named_map_member(
    &self,
    entry: &TypeGroupnameEntry<'a>,
    bindings_ready: bool,
  ) -> Option<ValueMemberKeyEntry<'a>> {
    if !matches!(self.json.as_ref(), Value::Object(_)) {
      return None;
    }
    let occurrence = entry.occur.as_ref()?;
    if occurrence_bounds(Some(occurrence.occur)).0 != 0
      || self.batch_key_is_parameter(&entry.name, None)
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

  pub(super) fn plain_literal_key<'b>(
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

  pub(super) fn required_uint_definitely_mismatches(
    &self,
    entry: &ValueMemberKeyEntry<'a>,
    key: &str,
    value: &Value,
    params: Option<&GenericParams<'a>>,
  ) -> bool {
    // A value mismatch can revalidate earlier movable claims and raise an error.
    if entry.occur.is_some()
      || member_key_is_cut(entry)
      || self.state.is_cut_present
      || self.cut_value.is_some()
      || self
        .member_claims
        .iter()
        .any(|claim| !member_key_is_cut(&claim.entry))
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

  pub(super) fn optional_choice_definitely_fails(
    &self,
    choice: &GroupChoice<'a>,
    object: &serde_json::Map<String, Value>,
    params: Option<&GenericParams<'a>>,
  ) -> bool {
    // Named-group parameters are not yet installed in the visitor's bindings.
    let can_skip_tail = |index| {
      index + 1 == choice.group_entries.len()
        || (params.is_none() && self.map_sequence_is_effect_free(choice))
    };
    for (index, (entry, _)) in choice.group_entries.iter().enumerate() {
      let GroupEntry::ValueMemberKey { ge, .. } = entry else {
        return false;
      };
      if non_optional_colon_occurrence(entry) {
        return false;
      }
      let Some(key) = self.plain_literal_key(entry, params) else {
        return false;
      };
      let required = occurrence_bounds(ge.occur.as_ref().map(|o| o.occur)).0 > 0;
      if let Some(value) = object.get(key) {
        return required
          && self.required_uint_definitely_mismatches(ge, key, value, params)
          && can_skip_tail(index);
      }
      if required {
        return can_skip_tail(index);
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

  pub(super) fn optional_choices_definitely_fail(
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
    // Prove every arm fails without earlier value evaluation or claimed-key
    // reassignment, and that its remaining entries are safe to skip.
    // Otherwise normal traversal must preserve later hard errors.
    group
      .group_choices
      .iter()
      .all(|choice| self.optional_choice_definitely_fails(choice, object, params))
  }

  pub(super) fn group_choices_are_empty(&self, group: &Group<'a>) -> bool {
    let Value::Object(object) = self.json.as_ref() else {
      return false;
    };
    if self.map_choice_search.is_none()
      || self.state.occurrence.is_some()
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

  pub(super) fn map_sequence_is_effect_free(&self, choice: &GroupChoice<'a>) -> bool {
    let Value::Object(object) = self.json.as_ref() else {
      return false;
    };
    if self.state.ctrl.is_some()
      || self.state.occurrence.is_some()
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
        GroupEntry::InlineGroup { occur, group, .. } => {
          if occur.is_some() && group_entry_bounds(entry).0 > 0 {
            return false;
          }
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

  pub(super) fn map_member_is_effect_free(&self, entry: &GroupEntry<'a>) -> bool {
    let GroupEntry::ValueMemberKey { ge, .. } = entry else {
      return false;
    };
    if member_key_is_cut(ge)
      || non_optional_colon_occurrence(entry)
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
}
