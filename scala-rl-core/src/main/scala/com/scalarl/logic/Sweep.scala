package com.scalarl
package logic

import cats.{Id, Monad}
import com.scalarl.algebra.{Expectation, Module, ToDouble}
import com.scalarl.evaluate.StateValue

object Sweep {
  import Module.DModule

  sealed trait Update extends Product with Serializable
  object Update {
    final case object Single
    final case object SweepComplete
  }

  /** This sweeps across the whole state space and updates the policy every single time IF you set
    * valueIteration to true. Otherwise it creates a policy once and then uses it each time.
    *
    * What we really want is the ability to ping between updates to the value function or learning steps; to
    * insert them every so often.
    *
    * This function does NOT currently return the final policy, since you can just make it yourself, given the
    * return value and the function.
    */
  def sweep[Obs, A, R, T, M[_]: Expectation, S[_]: Expectation](
      valueFn: StateValueFn[Obs, T],
      policyFn: StateValueFn[Obs, T] => Policy[Obs, A, R, M, S],
      evaluatorFn: (StateValueFn[Obs, T], Policy[Obs, A, R, M, S]) => StateValue[Obs, A, R, T, S],
      states: Traversable[State[Obs, A, R, S]],
      inPlace: Boolean,
      valueIteration: Boolean
  ): StateValueFn[Obs, T] =
    states
      .foldLeft((valueFn, evaluatorFn(valueFn, policyFn(valueFn)), policyFn(valueFn))) {
        case ((vf, ev, p), state) =>
          val newFn = vf.update(state.observation, ev.evaluate(state))
          val newPolicy = if (valueIteration) policyFn(newFn) else p
          val newEv = if (inPlace) evaluatorFn(newFn, newPolicy) else ev
          (newFn, newEv, newPolicy)
      }
      ._1

  def sweepUntil[Obs, A, R, T, M[_]: Expectation, S[_]: Expectation](
      valueFn: StateValueFn[Obs, T],
      policyFn: StateValueFn[Obs, T] => Policy[Obs, A, R, M, S],
      evaluatorFn: (StateValueFn[Obs, T], Policy[Obs, A, R, M, S]) => StateValue[Obs, A, R, T, S],
      states: Traversable[State[Obs, A, R, S]],
      stopFn: (StateValueFn[Obs, T], StateValueFn[Obs, T], Long) => Boolean,
      inPlace: Boolean,
      valueIteration: Boolean
  ): (StateValueFn[Obs, T], Long) =
    Monad[Id].tailRecM((valueFn, 0L)) { case (fn, nIterations) =>
      val updated = sweep(fn, policyFn, evaluatorFn, states, inPlace, valueIteration)
      Either.cond(
        stopFn(fn, updated, nIterations),
        (updated, nIterations),
        (updated, nIterations + 1)
      )
    }

  // TODO - this probably needs to take evaluators directly.
  def isPolicyStable[Obs, A, R, T: DModule: Ordering, M[_], S[_]: Expectation](
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

  /** Helper to tell if we can stop iterating. The combine function is used to aggregate the differences
    * between the value functions for each observation... the final aggregated value must be less than epsilon
    * to return true, false otherwise.
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

  /** TODO consider putting this on the actual trait.
    */
  def diffValue[Obs, T](
      l: StateValueFn[Obs, T],
      r: StateValueFn[Obs, T],
      combine: (Double, Double) => Double
  )(implicit T: ToDouble[T]): Double =
    Util.diff[Obs](l.seen ++ r.seen, o => T(l.stateValue(o)), o => T(r.stateValue(o)), combine)
}
