/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy

import cats.Id

/**
  * Random policy.
  */
case class Random[Obs, A, R, S[_]]() extends Policy[Obs, A, R, Cat, S] { self =>

  override def choose(state: State[Obs, A, R, S]): Cat[A] =
    Cat.fromSet(state.actions)

  override def learn(
      state: State[Obs, A, R, S],
      action: A,
      reward: R,
      nextState: State[Obs, A, R, S]
  ): Cat[This] = Cat.pure(self)
}

object Random {
  def cat[Obs, A, R]: Random[Obs, A, R, Cat] = Random()
  def id[Obs, A, R]: Random[Obs, A, R, Id] = Random()
}
