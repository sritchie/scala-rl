/**
  * Policy that accumulates using the Gradient.
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.core.Generator

case class Gradient[A, R]() extends Policy[A, R, Gradient[A, R]] {
  override def choose(state: State[A, R]): Generator[A] = ???
  override def learn(state: State[A, R], action: A, reward: R): Gradient[A, R] = ???
}

object Gradient {}
