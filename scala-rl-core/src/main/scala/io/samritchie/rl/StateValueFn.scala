/**
  Value function implementations.

  I think to really get this working, I need to find something that alternates
  between both... policy evaluation and updates. Maybe we just take a function
  to do that.

  - TODO build a max policy.
  - TODO make a function that sweeps, but picks which to do... figure 4.1 might hold the secrets here.
  */
package io.samritchie.rl

import io.samritchie.rl.util.{ExpectedValue, ToDouble}

/**
  * Trait for state or action value functions.

  We need some way for this to learn, or see new observations, that's part of
  the trait.
  */
trait StateValueFn[Obs] { self =>
  def seen: Iterable[Obs]
  def stateValue(obs: Obs): Value[Double]
  def update(state: Obs, value: Value[Double]): StateValueFn[Obs]
}

object StateValueFn {
  def apply[Obs](default: Value[Double]): StateValueFn[Obs] =
    value.StateValueMap(Map.empty[Obs, Value[Double]], default)

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs](default: Double, gamma: Double): StateValueFn[Obs] =
    apply(value.Decaying(default, gamma))

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs](gamma: Double): StateValueFn[Obs] =
    decaying(0.0, gamma)

  def isPolicyStable[Obs, A, R: ToDouble, M[_], S[_]: ExpectedValue](
      l: StateValueFn[Obs],
      r: StateValueFn[Obs],
      default: Value[Double],
      states: Traversable[State[Obs, A, R, S]]
  ): Boolean = {
    val lEstimator = Estimator.oneAhead[Obs, A, R, M, S](l, default)
    val rEstimator = Estimator.oneAhead[Obs, A, R, M, S](r, default)
    states.forall { s =>
      lEstimator.greedyOptions(s) == rEstimator.greedyOptions(s)
    }
  }

  /**
    Helper to tell if we can stop iterating. The combine function is used to
    aggregate the differences between the value functions for each
    observation... the final aggregated value must be less than epsilon to
    return true, false otherwise.
    */
  def diffBelow[Obs](
      l: StateValueFn[Obs],
      r: StateValueFn[Obs],
      epsilon: Double
  )(
      combine: (Double, Double) => Double
  ): Boolean = Ordering[Double].lt(diffValue(l, r, combine), epsilon)

  /**
    TODO consider putting this on the actual trait.
    */
  def diffValue[Obs](
      l: StateValueFn[Obs],
      r: StateValueFn[Obs],
      combine: (Double, Double) => Double
  ): Double =
    Util.diff[Obs]((l.seen ++ r.seen), l.stateValue(_).get, r.stateValue(_).get, combine),
}
