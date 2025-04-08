package com.scalarl

import com.scalarl.evaluate.{ActionValue, StateValue}
import com.scalarl.algebra.{Expectation, Module}

/** Contains traits and instances for the two evaluation methods.
  */
object Evaluator {
  import Module.DModule

  /** Evaluator that uses a world's dynamics to estimate the value of a given action.
    */
  def oneAhead[Obs, A, R, G: DModule, M[_], S[_]: Expectation](
      valueFn: StateValueFn[Obs, G],
      prepare: R => G,
      merge: (G, G) => G
  ): ActionValue[Obs, A, R, G, S] =
    StateValue
      .fn[Obs, A, R, G, S](valueFn)
      .byStateValue(prepare, merge)

  /** The full bellman estimation, where we know the dynamics of the policy and of the system.
    *
    * Could also be defined by
    *
    * {{{
    * state.byPolicy(policy).byStateValue(prepare, merge).apply(valueFn)
    * }}}
    */
  def bellman[Obs, A, R, G: DModule, M[_]: Expectation, S[_]: Expectation](
      valueFn: StateValueFn[Obs, G],
      policy: Policy[Obs, A, R, M, S],
      prepare: R => G,
      merge: (G, G) => G
  ): StateValue[Obs, A, R, G, S] =
    StateValue
      .fn[Obs, A, R, G, S](valueFn) // statevalue
      .byStateValue(prepare, merge) // actionvalue
      .byPolicy(policy) // statevalue

  /** This is my attempt at getting a better builder syntax going!
    */
  def state[Obs, A, R, G, S[_]]: FromState[Obs, A, R, G, S, StateValue] =
    new FromState(f => f)

  def action[Obs, A, R, G, S[_]]: FromAction[Obs, A, R, G, S, ActionValue] =
    new FromAction(f => f)

  /** Builder class that manages conversion of an [[evaluate.StateValue]] instance into either an
    * [[evaluate.ActionValue]] or [[evaluate.StateValue]] instance.
    */
  class FromState[Obs, A, R, G, S[_], F[_, _, _, _, *[_]]] private[scalarl] (
      f: StateValue[Obs, A, R, G, S] => F[Obs, A, R, G, S]
  ) {
    def fn(vfn: StateValueFn[Obs, G]): F[Obs, A, R, G, S] =
      f(StateValue.fn(vfn))

    def byPolicy[M[_]](
        policy: Policy[Obs, A, R, M, S]
    )(implicit
        M: Expectation[M],
        MV: Module[Double, G]
    ): FromAction[Obs, A, R, G, S, F] =
      new FromAction[Obs, A, R, G, S, F](fn => f(fn.byPolicy(policy)))
  }

  /** Builder class that manages conversion of an [[evaluate.ActionValue]] instance into either an
    * [[evaluate.ActionValue]] or [[evaluate.StateValue]] instance.
    */
  class FromAction[Obs, A, R, G, S[_], F[_, _, _, _, *[_]]] private[scalarl] (
      f: ActionValue[Obs, A, R, G, S] => F[Obs, A, R, G, S]
  ) {
    def fn(vfn: ActionValueFn[Obs, A, G]): F[Obs, A, R, G, S] =
      f(ActionValue.fn(vfn))

    def byStateValue(
        prepare: R => G,
        merge: (G, G) => G
    )(implicit
        S: Expectation[S],
        MV: Module[Double, G]
    ): FromState[Obs, A, R, G, S, F] =
      new FromState[Obs, A, R, G, S, F](fn => f(fn.byStateValue(prepare, merge)))
  }
}
