package io.samritchie.rl
package logic

import cats.{Id, Monad}
import io.samritchie.rl.util.ExpectedValue

object Sweep {
  sealed trait Update extends Product with Serializable
  object Update {
    final case object Single
    final case object SweepComplete
  }

  /**
    This sweeps across the whole state space and updates the policy every single
    time IF you set valueIteration to true. Otherwise it creates a policy once
    and then uses it each time.

    What we really want is the ability to ping between updates to the value
    function or learning steps; to insert them every so often.

    This function does NOT currently return the final policy, since you can just
    make it yourself, given the return value and the function.
    */
  def sweep[Obs, A, R, T, M[_]: ExpectedValue, S[_]: ExpectedValue](
      valueFn: StateValueFn[Obs, T],
      policyFn: StateValueFn[Obs, T] => Policy[Obs, A, R, M, S],
      evaluatorFn: (StateValueFn[Obs, T], Policy[Obs, A, R, M, S]) => Evaluator.StateValue[Obs, A, R, T, S],
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

  def sweepUntil[Obs, A, R, T, M[_]: ExpectedValue, S[_]: ExpectedValue](
      valueFn: StateValueFn[Obs, T],
      policyFn: StateValueFn[Obs, T] => Policy[Obs, A, R, M, S],
      evaluatorFn: (StateValueFn[Obs, T], Policy[Obs, A, R, M, S]) => Evaluator.StateValue[Obs, A, R, T, S],
      states: Traversable[State[Obs, A, R, S]],
      stopFn: (StateValueFn[Obs, T], StateValueFn[Obs, T], Long) => Boolean,
      inPlace: Boolean,
      valueIteration: Boolean
  ): (StateValueFn[Obs, T], Long) =
    Monad[Id].tailRecM((valueFn, 0L)) {
      case (fn, nIterations) =>
        val updated = sweep(fn, policyFn, evaluatorFn, states, inPlace, valueIteration)
        Either.cond(
          stopFn(fn, updated, nIterations),
          (updated, nIterations),
          (updated, nIterations + 1)
        )
    }
}
