/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.core.{Categorical, Generator}

/**
  * Random policy.
  */
case class Random[A, R]() extends Policy[A, Any, R, Random[A, R]] {
  override def choose(state: State[A, Any, R]): Generator[A] =
    Categorical.list(state.actions.toList).generator

  override def learn(state: State[A, Any, R], action: A, reward: R): Random[A, R] = this
}

object Random {

  /**
    * Returns a policy that does NOT learn.
    */
  def uniform[A, R]: Random[A, R] = Random[A, R]
}
