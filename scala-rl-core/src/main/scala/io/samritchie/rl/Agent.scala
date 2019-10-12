/**
  A combo of a policy and its value function. Could be helpful navigating the
  monte carlo stuff.

  The bandits are actually agent instances. AND, they probably need to each keep
  their own internal aggregation types to use inside their value functions.
  */
package io.samritchie.rl

import cats.Monad

trait Agent[Obs, A, @specialized(Int, Long, Float, Double) R, M[_]] { self =>
  type This = Agent[Obs, A, R, M]

  def monad: Monad[M]
  def policy: Policy[Obs, A, R, M, M]
  def valueFunction: StateValueFn[Obs]

  def play(state: State[Obs, A, R, M]): M[(state.This, (Obs, A, R))] =
    monad.flatMap(policy.choose(state)) { a =>
      monad.map(state.act(a)) {
        case (r, s2) => (s2, (state.observation, a, r))
      }
    }
}

object Agent {

  /**
    Agent that can't learn.
    */
  case class StaticAgent[Obs, A, R, M[_]](
      policy: Policy[Obs, A, R, M, M],
      valueFunction: StateValueFn[Obs]
  )(implicit val monad: Monad[M])
      extends Agent[Obs, A, R, M]
}
