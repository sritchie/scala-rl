/**
  Value function implementations.

  I think to really get this working, I need to find something that alternates
  between both... policy evaluation and updates. Maybe we just take a function
  to do that.

  - TODO build a max policy.
  - TODO make a function that sweeps, but picks which to do... figure 4.1 might hold the secrets here.
  */
package io.samritchie.rl

import com.twitter.algebird.Monoid
import io.samritchie.rl.util.{ExpectedValue, ToDouble}
import io.samritchie.rl.value.DecayState

/**
  * Trait for state or action value functions.

  We need some way for this to learn, or see new observations, that's part of
  the trait.
  */
trait StateValueFn[Obs, T] { self =>
  def seen: Iterable[Obs]
  def stateValue(obs: Obs): T
  def update(state: Obs, value: T): StateValueFn[Obs, T]
}

object StateValueFn {
  import Module.DModule

  def apply[Obs, T](default: T): StateValueFn[Obs, T] =
    value.StateValueMap(Map.empty[Obs, T], default)

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs, T](default: T): StateValueFn[Obs, DecayState[T]] =
    apply(DecayState.DecayedValue(default))

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs, T: Monoid]: StateValueFn[Obs, DecayState[T]] =
    decaying(Monoid.zero)

  // TODO - this probably needs to take evaluators directly.
  def isPolicyStable[Obs, A, R, T: DModule: Ordering, M[_], S[_]: ExpectedValue](
      l: StateValueFn[Obs, T],
      r: StateValueFn[Obs, T],
      prepare: R => T,
      merge: (T, T) => T,
      states: Traversable[State[Obs, A, R, S]]
  ): Boolean = {
    val lEvaluator = Evaluator.oneAhead[Obs, A, R, T, M, S](l, prepare, merge)
    val rEvaluator = Evaluator.oneAhead[Obs, A, R, T, M, S](r, prepare, merge)
    states.forall { s =>
      lEvaluator.greedyOptions(s) == rEvaluator.greedyOptions(s)
    }
  }

  /**
    Helper to tell if we can stop iterating. The combine function is used to
    aggregate the differences between the value functions for each
    observation... the final aggregated value must be less than epsilon to
    return true, false otherwise.
    */
  def diffBelow[Obs, T: ToDouble](
      l: StateValueFn[Obs, T],
      r: StateValueFn[Obs, T],
      epsilon: Double
  )(
      combine: (Double, Double) => Double
  ): Boolean = Ordering[Double].lt(
    diffValue(l, r, combine),
    epsilon
  )

  /**
    TODO consider putting this on the actual trait.
    */
  def diffValue[Obs, T](
      l: StateValueFn[Obs, T],
      r: StateValueFn[Obs, T],
      combine: (Double, Double) => Double
  )(implicit T: ToDouble[T]): Double =
    Util.diff[Obs]((l.seen ++ r.seen), o => T(l.stateValue(o)), o => T(r.stateValue(o)), combine),
}
