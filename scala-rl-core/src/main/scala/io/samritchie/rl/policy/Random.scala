/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy

import cats.Id

/**
  * Random policy.
  */
case class Random[A, R, S[_]]() extends Policy[A, Any, R, Cat, S] {
  override def choose(state: State[A, Any, R, S]): Cat[A] =
    Cat.fromSet(state.actions)
}

object Random {
  def cat[A, R]: Random[A, R, Cat] = Random()
  def id[A, R]: Random[A, R, Id] = Random()
}
