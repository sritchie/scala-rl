/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy

import cats.Id

/**
  * Random policy.
  */
case class Random[A, Obs, R, S[_]]() extends Policy[A, Obs, R, Cat, S] {
  override def choose(state: State[A, Obs, R, S]): Cat[A] =
    Cat.fromSet(state.actions)
}

object Random {
  def cat[A, Obs, R]: Random[A, Obs, R, Cat] = Random()
  def id[A, Obs, R]: Random[A, Obs, R, Id] = Random()
}
