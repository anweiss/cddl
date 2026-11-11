use super::*;

pub(super) fn integer_parts(domain: &NumericDomain<'_>) -> Option<Vec<IntegerBounds>> {
  match domain {
    NumericDomain::Empty => Some(Vec::new()),
    NumericDomain::IntegerSet(parts) => Some(parts.clone()),
    _ => domain.integer_bounds().map(|bounds| vec![bounds]),
  }
}

fn normalized_integers<'a>(mut parts: Vec<IntegerBounds>) -> NumericDomain<'a> {
  parts.sort_by_key(|bounds| bounds.lower);
  let mut result: Vec<IntegerBounds> = Vec::new();
  for bounds in parts {
    if let Some(last) = result.last_mut() {
      if bounds.lower <= last.upper + 1 {
        last.upper = last.upper.max(bounds.upper);
        continue;
      }
    }
    result.push(bounds);
  }
  match result.as_slice() {
    [] => NumericDomain::Empty,
    [bounds] if bounds.lower == bounds.upper => integer_token(bounds.lower)
      .map(NumericDomain::Singleton)
      .unwrap_or(NumericDomain::Unknown),
    [bounds] => NumericDomain::IntegerRange(*bounds),
    _ => NumericDomain::IntegerSet(result),
  }
}

pub(super) fn finite_literals<'a>(
  domain: &NumericDomain<'a>,
  limit: usize,
) -> Option<Vec<token::Value<'a>>> {
  match domain {
    NumericDomain::Empty => Some(Vec::new()),
    NumericDomain::Singleton(value) => Some(vec![value.clone()]),
    NumericDomain::Literals(values) => Some(values.clone()),
    _ => {
      let parts = integer_parts(domain)?;
      let mut values = Vec::new();
      for bounds in parts {
        if bounds.upper - bounds.lower + 1 > (limit - values.len()) as i128 {
          return None;
        }
        for value in bounds.lower..=bounds.upper {
          values.push(integer_token(value).ok()?);
        }
      }
      Some(values)
    }
  }
}

pub(super) fn union_numeric_domains<'a>(
  left: NumericDomain<'a>,
  right: NumericDomain<'a>,
) -> NumericDomain<'a> {
  if let (Some(mut a), Some(b)) = (integer_parts(&left), integer_parts(&right)) {
    a.extend(b);
    return normalized_integers(a);
  }
  let (Some(mut left), Some(right)) = (
    finite_literals(&left, MAX_NUMERIC_RESOLUTION_WORK),
    finite_literals(&right, MAX_NUMERIC_RESOLUTION_WORK),
  ) else {
    return NumericDomain::Unknown;
  };
  for value in right {
    if !left
      .iter()
      .any(|existing| numeric_literals_equal(existing, &value))
    {
      left.push(value);
    }
  }
  if left.len() > MAX_NUMERIC_RESOLUTION_WORK {
    return NumericDomain::Unknown;
  }
  NumericDomain::Literals(left)
}

impl<'s, 'a, E, F> NumericControllerResolver<'s, 'a, F>
where
  F: FnMut(&Type1<'a>, &token::Value<'a>) -> Result<bool, E>,
{
  pub(super) fn proof_step(&mut self) -> bool {
    if self.remaining == 0 {
      self.exhausted = true;
      return false;
    }
    self.remaining -= 1;
    true
  }

  pub(super) fn cut_domain(&mut self, key: &str) -> NumericDomain<'a> {
    self.cycle_cuts += 1;
    if self.negative.contains(key) {
      self.failed_proof(NumericProofFailure::NegativeRecursion)
    } else {
      NumericDomain::Empty
    }
  }

  pub(super) fn equality_value(
    &mut self,
    target: &Type2<'a>,
    controller: &Type2<'a>,
    invert: bool,
  ) -> Result<NumericDomain<'a>, E> {
    let previous = self.negative.clone();
    if invert {
      self.negative.extend(self.active.iter().cloned());
    }
    let right = self.type2_value(controller);
    self.negative = previous;
    let right = right?;
    if right == NumericDomain::Empty {
      self.type2_value(target)?;
    }
    if !invert {
      if let Some(values) = finite_literals(&right, self.remaining) {
        return self.filter_candidates(values, target, false);
      }
    }
    let left = self.type2_value(target)?;
    let right_integers = integer_parts(&right).or_else(|| {
      finite_literals(&right, self.remaining).map(|values| {
        values
          .iter()
          .filter_map(numeric_literal_integer)
          .map(|value| IntegerBounds {
            lower: value,
            upper: value,
          })
          .collect()
      })
    });
    if let (Some(a), Some(b)) = (integer_parts(&left), right_integers) {
      return Ok(self.integer_membership(a, b, invert));
    }
    let Some(values) = finite_literals(&left, self.remaining) else {
      return Ok(NumericDomain::Unknown);
    };
    self.filter_candidates(values, controller, invert)
  }

  fn filter_candidates(
    &mut self,
    values: Vec<token::Value<'a>>,
    operand: &Type2<'a>,
    invert: bool,
  ) -> Result<NumericDomain<'a>, E> {
    let mut domain = NumericDomain::Empty;
    for value in values {
      if !self.proof_step() {
        return Ok(NumericDomain::Unknown);
      }
      let mut alternative = Type1::from(value.clone());
      alternative.type2 = operand.clone();
      if (self.validate)(&alternative, &value)? != invert {
        domain = merge_numeric_domains(domain, NumericDomain::Singleton(value));
      }
    }
    Ok(domain)
  }

  pub(super) fn integer_membership(
    &mut self,
    left: Vec<IntegerBounds>,
    right: Vec<IntegerBounds>,
    invert: bool,
  ) -> NumericDomain<'a> {
    let mut result = Vec::new();
    for a in left {
      let mut pending = vec![a];
      for b in &right {
        if !self.proof_step() {
          return NumericDomain::Unknown;
        }
        if invert {
          let mut next = Vec::new();
          for part in pending {
            if !self.proof_step() {
              return NumericDomain::Unknown;
            }
            next.extend(subtract_interval(part, *b));
          }
          pending = next;
        } else {
          let lower = a.lower.max(b.lower);
          let upper = a.upper.min(b.upper);
          if lower <= upper {
            result.push(IntegerBounds { lower, upper });
          }
        }
      }
      if invert {
        result.extend(pending);
      }
    }
    let domain = normalized_integers(result);
    if domain == NumericDomain::Unknown {
      self.failed_proof(NumericProofFailure::Representability)
    } else {
      domain
    }
  }
}

fn subtract_interval(a: IntegerBounds, b: IntegerBounds) -> Vec<IntegerBounds> {
  if a.upper < b.lower || a.lower > b.upper {
    return vec![a];
  }
  let mut result = Vec::new();
  if a.lower < b.lower {
    result.push(IntegerBounds {
      lower: a.lower,
      upper: b.lower - 1,
    });
  }
  if a.upper > b.upper {
    result.push(IntegerBounds {
      lower: b.upper + 1,
      upper: a.upper,
    });
  }
  result
}
