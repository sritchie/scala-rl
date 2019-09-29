/**
  Value function implementations.

  I think to really get this working, I need to find something that alternates
  between both... policy evaluation and updates. Maybe we just take a function
  to do that.

  - TODO build a max policy.
  - TODO make a function that sweeps, but picks which to do... figure 4.1 might hold the secrets here.
  */
package io.samritchie.rl

import cats.Id
import com.stripe.rainier.compute.{Real, ToReal}
import com.stripe.rainier.core.Categorical
import Util.Instances.realOrd

/**
  * Trait for state or action value functions.

  We need some way for this to learn, or see new observations, that's part of
  the trait.
  */
trait ValueFunction[Obs] {
  def seen: Iterable[Obs]

  def stateValue(obs: Obs): Value[Real]

  /**
    Evaluate the state using the supplied policy.
    */
  def evaluate[A, R: ToReal](
      state: State[A, Obs, R, Id],
      policy: Policy[A, Obs, R, Categorical, Id]
  ): Value[Real]

  def update[A, R: ToReal](
      state: State[A, Obs, R, Id],
      value: Value[Real]
  ): ValueFunction[Obs]

  def evaluateAndUpdate[A, R: ToReal](
      state: State[A, Obs, R, Id],
      policy: Policy[A, Obs, R, Categorical, Id]
  ): ValueFunction[Obs] = update(state, evaluate(state, policy))
}

trait ActionValueFunction[A, Obs] extends ValueFunction[Obs] {
  def seen(obs: Obs): Set[A]
  def actionValue(obs: Obs, a: A): Value[Real]

  def learn[R](
      state: State[A, Obs, R, Id],
      policy: Policy[A, Obs, R, Categorical, Id],
      action: A,
      reward: R
  ): ActionValueFunction[A, Obs]
}

object ValueFunction {
  def apply[Obs](default: Value[Real]): ValueFunction[Obs] =
    value.MapValueFunction(Map.empty[Obs, Value[Real]], default)

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs](gamma: Double): ValueFunction[Obs] =
    decaying(Real.zero, gamma)

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs](default: Real, gamma: Double): ValueFunction[Obs] =
    ValueFunction[Obs](value.Decaying(default, gamma))

  /**
    This sweeps across the whole state space and updates the policy every single
    time.

    What we really want is the ability to ping between updates to the value
    function or learning steps; to insert them every so often.

    TODO add gate to random if necessary...
    TODO remove the iter stuff
    TODO test if it all works

    */
  def sweep[A, Obs, R: ToReal](
      policy: CategoricalPolicy[A, Obs, R, Id],
      valueFn: ValueFunction[Obs],
      states: Traversable[State[A, Obs, R, Id]],
      inPlace: Boolean
  ): ValueFunction[Obs] =
    states
      .foldLeft((valueFn, policy)) {
        case ((vf, p), state) =>
          val baseVf = if (inPlace) vf else valueFn
          val newFn = vf.update(state, baseVf.evaluate(state, p))
          val newP = p.learnAll(newFn)
          (newFn, newP)
      }
      ._1

  def sweepUntil[A, Obs, R: ToReal](
      policy: CategoricalPolicy[A, Obs, R, Id],
      valueFn: ValueFunction[Obs],
      states: Traversable[State[A, Obs, R, Id]],
      stopFn: (ValueFunction[Obs], ValueFunction[Obs], Long) => Boolean,
      inPlace: Boolean
  ): (ValueFunction[Obs], Long) =
    Util.loopWhile((valueFn, 0)) {
      case (fn, nIterations) =>
        val updated = ValueFunction.sweep(policy, fn, states, inPlace)
        Either.cond(
          stopFn(fn, updated, nIterations),
          (updated, nIterations),
          (updated, nIterations + 1)
        )
    }

  def isPolicyStable[A, Obs, R: ToReal](
      l: ValueFunction[Obs],
      r: ValueFunction[Obs],
      states: Traversable[State[A, Obs, R, Id]]
  ): Boolean =
    states.forall(s => greedyOptions(l, s) == greedyOptions(r, s))

  def greedyOptions[A, Obs, R: ToReal](valueFn: ValueFunction[Obs], state: State[A, Obs, R, Id]): Set[A] =
    Util.allMaxBy[A, Real](state.actions) { a =>
      state
        .act(a)
        .map {
          case (r, newState) =>
            valueFn.stateValue(newState.observation).from(ToReal(r)).get
        }
        .getOrElse(Real.negInfinity)
    }

  /**
    Helper to tell if we can stop iterating. The combine function is used to
    aggregate the differences between the value functions for each
    observation... the final aggregated value must be less than epsilon to
    return true, false otherwise.
    */
  def diff[Obs](l: ValueFunction[Obs], r: ValueFunction[Obs], epsilon: Double)(
      combine: (Real, Real) => Real
  ): Boolean =
    Ordering[Real].lt(
      Util.diff[Obs]((l.seen ++ r.seen), l.stateValue(_).get, r.stateValue(_).get, combine),
      Real(epsilon)
    )

  /**
    I think this needs actual states to check.
    */
  def isPolicyStable[Obs](l: ValueFunction[Obs], r: ValueFunction[Obs]): Boolean = ???
}
