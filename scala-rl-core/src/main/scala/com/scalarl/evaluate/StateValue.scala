package com.scalarl
package evaluate

import com.scalarl.algebra.{Expectation, Module}
import Module.DModule

/** trait for evaluating a given state.
  */
sealed trait StateValue[Obs, A, R, G, S[_]] extends Product with Serializable {

  /** Returns an evaluation of the given state.
    */
  def evaluate(state: State[Obs, A, R, S]): G

  /** Upgrades to evaluate given... what is going on?
    */
  def byStateValue(
      prepare: R => G,
      merge: (G, G) => G
  )(implicit S: Expectation[S], MV: Module[Double, G]): ActionValue[Obs, A, R, G, S] =
    ActionValue.ByStateValue(this, prepare, merge)
}

object StateValue {

  /** Returns a basic evaluator that uses a given state value function.
    */
  def fn[Obs, A, R, G, S[_]](f: StateValueFn[Obs, G]): StateValue[Obs, A, R, G, S] = Fn(f)

  /** This evaluates the state's value directly.
    */
  final case class Fn[Obs, A, R, G, S[_]](f: StateValueFn[Obs, G]) extends StateValue[Obs, A, R, G, S] {
    def evaluate(state: State[Obs, A, R, S]): G =
      f.stateValue(state.observation)
  }

  /** Evaluates the state's value by weighting evaluated action values by the policy's chance of choosing each
    * action.
    */
  final case class ByPolicy[Obs, A, R, G: DModule, M[_]: Expectation, S[_]](
      evaluator: ActionValue[Obs, A, R, G, S],
      policy: Policy[Obs, A, R, M, S]
  ) extends StateValue[Obs, A, R, G, S] {
    def evaluate(state: State[Obs, A, R, S]): G =
      Expectation[M].get(policy.choose(state)) { a =>
        evaluator.evaluate(state, a)
      }
  }
}
