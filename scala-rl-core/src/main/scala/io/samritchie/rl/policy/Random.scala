/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy

import cats.Id
import com.stripe.rainier.core.Categorical

/**
  * Random policy.
  */
case class Random[A, R, S[_]]() extends CategoricalPolicy[A, Any, R, S] {
  override def choose(state: State[A, Any, R, S]): Categorical[A] =
    Categorical.list(state.actions.toSeq)
}

object Random {
  def categorical[A, R]: Random[A, R, Categorical] = Random()
  def id[A, R]: Random[A, R, Id] = Random()
}
