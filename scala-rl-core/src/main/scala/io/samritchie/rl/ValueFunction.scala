/**
  Value function implementations.

  I think to really get this working, I need to find something that alternates
  between both... policy evaluation and updates. Maybe we just take a function
  to do that.

  - TODO build a max policy.
  - TODO make a function that sweeps, but picks which to do... figure 4.1 might hold the secrets here.
  */
package io.samritchie.rl

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
      policy: Policy[A, Obs, R, Categorical, Id]
  ): ValueFunction[Obs]
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
      states: Traversable[State[A, Obs, R, Id]]
  ): ValueFunction[Obs] =
    states
      .foldLeft((valueFn, policy)) {
        case ((vf, p), state) =>
          val newFn = vf.update(state, p)
          val newP = p.learnAll(newFn)
          (newFn, newP)
      }
      ._1

  def sweepUntil[A, Obs, R: ToReal](
      policy: CategoricalPolicy[A, Obs, R, Id],
      valueFn: ValueFunction[Obs],
      states: Traversable[State[A, Obs, R, Id]],
      stopFn: (ValueFunction[Obs], ValueFunction[Obs], Long) => Boolean
  ): (ValueFunction[Obs], Long) =
    Util.loopWhile((valueFn, 0)) {
      case (fn, nIterations) =>
        val updated = ValueFunction.sweep(policy, fn, states)
        Either.cond(
          stopFn(fn, updated, nIterations),
          (updated, nIterations),
          (updated, nIterations + 1)
        )
    }

  /**
    Helper to tell if we can stop iterating.
    */
  def valuesWithin[Obs](oldVF: ValueFunction[Obs], newVF: ValueFunction[Obs], epsilon: Double): Boolean =
    Ordering[Real].lt(
      Util.diff[Obs]((oldVF.seen ++ newVF.seen), oldVF.stateValue(_).get, newVF.stateValue(_).get),
      Real(epsilon)
    )
}
