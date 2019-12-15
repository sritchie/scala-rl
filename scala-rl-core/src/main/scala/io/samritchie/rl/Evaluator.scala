package io.samritchie.rl

import io.samritchie.rl.evaluate.{ActionValue, StateValue}
import io.samritchie.rl.algebra.{Expectation, Module}

/**
  Contains traits and instances for the two evaluation methods.
  */
object Evaluator {
  import Module.DModule

  /**
    Evaluator that uses a world's dynamics to estimate the value of a given
    action.
    */
  def oneAhead[Obs, A, R, G: DModule, M[_], S[_]: Expectation](
      valueFn: StateValueFn[Obs, G],
      prepare: R => G,
      merge: (G, G) => G
  ): ActionValue[Obs, A, R, G, S] =
    StateValue
      .fn[Obs, A, R, G, S](valueFn)
      .byStateValue(prepare, merge)

  /**
    The full bellman estimation, where we know the dynamics of the policy and of
    the system.
    */
  def bellman[Obs, A, R, G: DModule, M[_]: Expectation, S[_]: Expectation](
      valueFn: StateValueFn[Obs, G],
      policy: Policy[Obs, A, R, M, S],
      prepare: R => G,
      merge: (G, G) => G
  ): StateValue[Obs, A, R, G, S] =
    StateValue
      .fn[Obs, A, R, G, S](valueFn)
      .byStateValue(prepare, merge)
      .byPolicy(policy)
}
