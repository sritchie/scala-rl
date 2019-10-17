package io.samritchie.rl
package logic

import cats.{Id, Monad}
import io.samritchie.rl.util.{ExpectedValue, ToDouble}

object Sweep {

  /**
    This sweeps across the whole state space and updates the policy every single
    time IF you set valueIteration to true. Otherwise it creates a policy once
    and then uses it each time.

    What we really want is the ability to ping between updates to the value
    function or learning steps; to insert them every so often.

    This function does NOT currently return the final policy, since you can just
    make it yourself, given the return value and the function.
    */
  def sweep[Obs, A, R: ToDouble, M[_]: ExpectedValue, S[_]: ExpectedValue](
      valueFn: StateValueFn[Obs],
      policyFn: StateValueFn[Obs] => Policy[Obs, A, R, M, S],
      estimatorFn: (StateValueFn[Obs], Policy[Obs, A, R, M, S]) => Estimator.StateValue[Obs, A, R, S],
      states: Traversable[State[Obs, A, R, S]],
      inPlace: Boolean,
      valueIteration: Boolean
  ): StateValueFn[Obs] =
    states
      .foldLeft((valueFn, estimatorFn(valueFn, policyFn(valueFn)), policyFn(valueFn))) {
        case ((vf, est, p), state) =>
          val newFn = vf.update(state.observation, est.estimate(state))
          val newPolicy = if (valueIteration) policyFn(newFn) else p
          val newEst = if (inPlace) estimatorFn(newFn, newPolicy) else est
          (newFn, newEst, newPolicy)
      }
      ._1

  def sweepUntil[Obs, A, R: ToDouble, M[_]: ExpectedValue, S[_]: ExpectedValue](
      valueFn: StateValueFn[Obs],
      policyFn: StateValueFn[Obs] => Policy[Obs, A, R, M, S],
      estimatorFn: (StateValueFn[Obs], Policy[Obs, A, R, M, S]) => Estimator.StateValue[Obs, A, R, S],
      states: Traversable[State[Obs, A, R, S]],
      stopFn: (StateValueFn[Obs], StateValueFn[Obs], Long) => Boolean,
      inPlace: Boolean,
      valueIteration: Boolean
  ): (StateValueFn[Obs], Long) =
    Monad[Id].tailRecM((valueFn, 0L)) {
      case (fn, nIterations) =>
        val updated = sweep(fn, policyFn, estimatorFn, states, inPlace, valueIteration)
        Either.cond(
          stopFn(fn, updated, nIterations),
          (updated, nIterations),
          (updated, nIterations + 1)
        )
    }
}
