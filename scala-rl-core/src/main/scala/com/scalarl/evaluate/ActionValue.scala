package com.scalarl
package evaluate

import com.scalarl.algebra.{Expectation, Module}
import Module.DModule

/** trait for evaluation of a given state, action pair.
  */
sealed trait ActionValue[Obs, A, R, G, S[_]] extends Product with Serializable {

  def evaluate(state: State[Obs, A, R, S], a: A): G

  def greedyOptions(state: State[Obs, A, R, S])(implicit
      G: Ordering[G]
  ): Set[A] =
    Util.allMaxBy[A, G](state.actions)(evaluate(state, _))

  def byPolicy[M[_]](
      policy: Policy[Obs, A, R, M, S]
  )(implicit
      M: Expectation[M],
      MV: Module[Double, G]
  ): StateValue[Obs, A, R, G, S] =
    StateValue.ByPolicy(this, policy)
}

object ActionValue {
  def fn[Obs, A, R, G, S[_]](
      f: ActionValueFn[Obs, A, G]
  ): ActionValue[Obs, A, R, G, S] = Fn(f)

  /** Evaluates the action's value directly.
    */
  final case class Fn[Obs, A, R, G, S[_]](f: ActionValueFn[Obs, A, G])
      extends ActionValue[Obs, A, R, G, S] {
    def evaluate(state: State[Obs, A, R, S], a: A): G =
      f.actionValue(state.observation, a)
  }

  /** The state under evaluation potentially offers dynamics
    */
  final case class ByStateValue[Obs, A, R, G: DModule, S[_]: Expectation](
      evaluator: StateValue[Obs, A, R, G, S],
      prepare: R => G,
      merge: (G, G) => G
  ) extends ActionValue[Obs, A, R, G, S] {
    def evaluate(state: State[Obs, A, R, S], a: A): G =
      Expectation[S].get(state.act(a)) { case (r, s) =>
        merge(evaluator.evaluate(s), prepare(r))
      }
  }
}
