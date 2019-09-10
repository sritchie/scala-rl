/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.core.Categorical

/**
  * Random policy.
  */
case class Random[A, R]() extends CategoricalPolicy[A, Any, R, Random[A, R]] {

  def categories(state: State[A, Any, R]): Categorical[A] =
    Categorical.list(state.actions.toList)

  override def learn(state: State[A, Any, R], action: A, reward: R): Random[A, R] =
    this
}

object Random {

  /**
    * Returns a policy that does NOT learn.
    */
  def uniform[A, R]: Random[A, R] = Random[A, R]
}
