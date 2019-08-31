package io.samritchie.rl
package policy

import com.stripe.rainier.core.Generator
import com.twitter.algebird.{Monoid, MonoidAggregator}

/**
  * TODO... this is way too specific. Get rid of this bullshit with
  * the list at the end. Anyway, more on that later once I get these
  * graphs actually working.
  */
case class Instrumented[A, R: Monoid: Ordering, P <: Policy[A, R, P]](
    policy: P,
    f: P => Map[A, R],
    acc: Map[A, List[R]]
) extends Policy[A, R, Instrumented[A, R, P]] {
  override def choose(state: State[A, R]): Generator[A] = policy.choose(state)
  override def learn(state: State[A, R], action: A, reward: R): Instrumented[A, R, P] = {
    val newPolicy: P = policy.learn(state, action, reward)
    val newR = f(newPolicy).getOrElse(action, Monoid.zero)
    val newV = acc.getOrElse(action, List.empty)
    Instrumented(
      newPolicy,
      f,
      acc.updated(action, newV :+ newR)
    )
  }
}
