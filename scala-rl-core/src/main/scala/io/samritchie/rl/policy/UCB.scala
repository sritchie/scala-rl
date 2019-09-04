/**
  * Policy that accumulates using the UCB algo.
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.core.Generator

case class UCB[A, R]() extends Policy[A, R, UCB[A, R]] {
  override def choose(state: State[A, R]): Generator[A] = ???
  override def learn(state: State[A, R], action: A, reward: R): UCB[A, R] = ???
}

object UCB {}
