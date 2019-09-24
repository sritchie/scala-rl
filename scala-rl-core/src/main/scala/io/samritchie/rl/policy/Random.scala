/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.core.{Categorical, Generator}

/**
  * Random policy.
  */
case class Random[A, R, S[+ _]]() extends CategoricalPolicy[A, Any, R, S, Random[A, R, S]] {

  def categories(state: State[A, Any, R, S]): Categorical[A] =
    Categorical.list(state.actions.toSeq)

  override def learn(
      state: State[A, Any, R, S],
      action: A,
      reward: R
  ): Random[A, R, S] = this
}

object Random {

  /**
    * Returns a policy that does NOT learn.
    */
  def generator[A, R]: Random[A, R, Generator] = Random()
  def id[A, R]: Random[A, R, Id] = Random()
}
