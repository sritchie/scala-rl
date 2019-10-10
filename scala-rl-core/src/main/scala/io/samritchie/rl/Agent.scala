/**
  A combo of a policy and its value function. Could be helpful navigating the
  monte carlo stuff.
  */
package io.samritchie.rl

import cats.Monad

trait Agent[Obs, A, @specialized(Int, Long, Float, Double) R, M[_]] { self =>
  type This = Agent[Obs, A, R, M]

  def monad: Monad[M]
  def policy: Policy[Obs, A, R, M, M]
  def valueFunction: ValueFunction[Obs]

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
      valueFunction: ValueFunction[Obs]
  )(implicit val monad: Monad[M])
      extends Agent[Obs, A, R, M]
}
