use super::*;
use std::convert::TryFrom;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct IntegerBounds {
  pub(super) lower: i128,
  pub(super) upper: i128,
}

#[derive(Clone, Copy, Debug)]
pub(super) enum NumericProofFailure {
  Representability,
  ArithmeticOverflow,
  CyclicComputation,
  InvalidController,
}

impl NumericProofFailure {
  pub(super) fn message(self) -> String {
    match self {
      Self::Representability => "numeric controller result is not representable",
      Self::ArithmeticOverflow => "numeric addition overflow",
      Self::CyclicComputation => "numeric computation depends on an active cycle cut",
      Self::InvalidController => "numeric comparison controller must denote a singleton",
    }
    .to_string()
  }
}

impl NumericDomain<'_> {
  pub(super) fn integer_bounds(&self) -> Option<IntegerBounds> {
    match self {
      Self::IntegerRange(bounds) => Some(*bounds),
      Self::Singleton(value) => numeric_literal_integer(value).map(|value| IntegerBounds {
        lower: value,
        upper: value,
      }),
      _ => None,
    }
  }
}

pub(super) fn integer_token<'a>(value: i128) -> Result<token::Value<'a>, NumericProofFailure> {
  if value < 0 {
    isize::try_from(value)
      .map(token::Value::INT)
      .map_err(|_| NumericProofFailure::Representability)
  } else {
    usize::try_from(value)
      .map(token::Value::UINT)
      .map_err(|_| NumericProofFailure::Representability)
  }
}

pub(super) fn integer_domain<'a>(
  lower: i128,
  upper: i128,
) -> Result<NumericDomain<'a>, NumericProofFailure> {
  if lower > upper {
    return Ok(NumericDomain::Empty);
  }
  if lower == upper {
    return integer_token(lower).map(NumericDomain::Singleton);
  }
  Ok(NumericDomain::IntegerRange(IntegerBounds { lower, upper }))
}

pub(super) fn primitive_integer_domain<'a>(ident: &Identifier<'_>) -> NumericDomain<'a> {
  if ident.socket.is_some() {
    return NumericDomain::Unknown;
  }
  let lower = -(1i128 << 64);
  let upper = u64::MAX as i128;
  let bounds = match lookup_ident(ident.ident) {
    Token::UINT => IntegerBounds { lower: 0, upper },
    Token::NINT => IntegerBounds { lower, upper: -1 },
    Token::INT => IntegerBounds { lower, upper },
    _ => return NumericDomain::Unknown,
  };
  NumericDomain::IntegerRange(bounds)
}

pub(super) fn numeric_literal<'a>(value: &Type2<'_>) -> Option<token::Value<'a>> {
  match value {
    Type2::IntValue { value, .. } => Some(token::Value::INT(*value)),
    Type2::UintValue { value, .. } => Some(token::Value::UINT(*value)),
    Type2::FloatValue { value, .. } => Some(token::Value::FLOAT(*value)),
    _ => None,
  }
}

#[cfg(feature = "additional-controls")]
fn numeric_float(value: &token::Value<'_>) -> Option<f64> {
  match value {
    token::Value::FLOAT(value) => Some(*value),
    _ => numeric_literal_integer(value).map(|value| value as f64),
  }
}

#[cfg(feature = "additional-controls")]
pub(super) fn checked_numeric_addition<'a>(
  left: &token::Value<'a>,
  right: &token::Value<'a>,
) -> Result<token::Value<'a>, NumericProofFailure> {
  if let (Some(left), Some(right)) = (
    numeric_literal_integer(left),
    numeric_literal_integer(right),
  ) {
    return left
      .checked_add(right)
      .ok_or(NumericProofFailure::ArithmeticOverflow)
      .and_then(integer_token);
  }
  let sum = numeric_float(left).ok_or(NumericProofFailure::Representability)?
    + numeric_float(right).ok_or(NumericProofFailure::Representability)?;
  if !sum.is_finite() {
    return Err(NumericProofFailure::ArithmeticOverflow);
  }
  Ok(token::Value::FLOAT(sum))
}

impl<'s, 'a, E, F> NumericControllerResolver<'s, 'a, F>
where
  F: FnMut(&Type1<'a>, &token::Value<'a>) -> Result<bool, E>,
{
  pub(super) fn failed_proof(&mut self, failure: NumericProofFailure) -> NumericDomain<'a> {
    self.failure.get_or_insert(failure);
    NumericDomain::Unknown
  }

  pub(super) fn proved_integer(&mut self, lower: i128, upper: i128) -> NumericDomain<'a> {
    match integer_domain(lower, upper) {
      Ok(domain) => domain,
      Err(failure) => self.failed_proof(failure),
    }
  }

  pub(super) fn ordered_integer_value(
    &mut self,
    value: &Type1<'a>,
  ) -> Result<Option<NumericDomain<'a>>, E> {
    let Some(operator) = &value.operator else {
      return Ok(None);
    };
    let RangeCtlOp::CtlOp { ctrl, .. } = operator.operator else {
      return Ok(None);
    };
    if !matches!(
      ctrl,
      ControlOperator::GT | ControlOperator::GE | ControlOperator::LT | ControlOperator::LE
    ) {
      return Ok(None);
    }
    let target = self.type2_value(&value.type2)?;
    let Some(bounds) = target.integer_bounds() else {
      return Ok(None);
    };
    let controller = self.type2_value(&operator.type2)?;
    let NumericDomain::Singleton(controller) = controller else {
      return Ok(Some(
        self.failed_proof(NumericProofFailure::InvalidController),
      ));
    };
    Ok(Some(self.constrained_integer(bounds, ctrl, &controller)))
  }

  fn constrained_integer(
    &mut self,
    bounds: IntegerBounds,
    ctrl: ControlOperator,
    controller: &token::Value<'a>,
  ) -> NumericDomain<'a> {
    let lower_matches = NumericValue::Integer(bounds.lower).compare(ctrl, controller);
    let upper_matches = NumericValue::Integer(bounds.upper).compare(ctrl, controller);
    if lower_matches == upper_matches {
      return if lower_matches {
        self.proved_integer(bounds.lower, bounds.upper)
      } else {
        NumericDomain::Empty
      };
    }
    let increasing = matches!(ctrl, ControlOperator::GT | ControlOperator::GE);
    let Some(cut) = self.integer_transition(bounds, ctrl, controller, increasing) else {
      return NumericDomain::Unknown;
    };
    if increasing {
      self.proved_integer(cut, bounds.upper)
    } else {
      self.proved_integer(bounds.lower, cut - 1)
    }
  }

  fn integer_transition(
    &mut self,
    bounds: IntegerBounds,
    ctrl: ControlOperator,
    controller: &token::Value<'a>,
    increasing: bool,
  ) -> Option<i128> {
    let (mut lower, mut upper) = (bounds.lower, bounds.upper);
    while lower < upper {
      if self.remaining == 0 {
        self.exhausted = true;
        return None;
      }
      self.remaining -= 1;
      let midpoint = lower + (upper - lower) / 2;
      if NumericValue::Integer(midpoint).compare(ctrl, controller) == increasing {
        upper = midpoint;
      } else {
        lower = midpoint + 1;
      }
    }
    Some(lower)
  }

  pub(super) fn intersection_value(
    &mut self,
    target: &Type2<'a>,
    controller: &Type2<'a>,
  ) -> Result<NumericDomain<'a>, E> {
    let (left, right) = (self.type2_value(target)?, self.type2_value(controller)?);
    if left == NumericDomain::Empty || right == NumericDomain::Empty {
      return Ok(NumericDomain::Empty);
    }
    if let (Some(a), Some(b)) = (left.integer_bounds(), right.integer_bounds()) {
      return Ok(self.proved_integer(a.lower.max(b.lower), a.upper.min(b.upper)));
    }
    let candidate = match (&left, &right) {
      (NumericDomain::Singleton(value), _) | (_, NumericDomain::Singleton(value)) => value.clone(),
      _ => return Ok(NumericDomain::Unknown),
    };
    if self.candidate_in_domain(target, &left, &candidate)?
      && self.candidate_in_domain(controller, &right, &candidate)?
    {
      Ok(NumericDomain::Singleton(candidate))
    } else {
      Ok(NumericDomain::Empty)
    }
  }

  fn candidate_in_domain(
    &mut self,
    value: &Type2<'a>,
    domain: &NumericDomain<'a>,
    candidate: &token::Value<'a>,
  ) -> Result<bool, E> {
    match domain {
      NumericDomain::Empty => Ok(false),
      NumericDomain::Singleton(value) => Ok(numeric_literals_equal(value, candidate)),
      NumericDomain::IntegerRange(bounds) => Ok(
        numeric_literal_integer(candidate)
          .is_some_and(|value| value >= bounds.lower && value <= bounds.upper),
      ),
      NumericDomain::Unknown => {
        let mut alternative = Type1::from(candidate.clone());
        alternative.type2 = value.clone();
        (self.validate)(&alternative, candidate)
      }
    }
  }
}
