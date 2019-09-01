/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.core.Generator
import com.twitter.algebird.{AveragedValue, MonoidAggregator}

/**
  * This is a version that accumulates the reward using a monoid.
  *
  * @param epsilon number between 0 and 1.
  */
case class EpsilonGreedy[A, R: Ordering, T](
    epsilon: Double,
    agg: MonoidAggregator[R, T, R],
    aggState: Map[A, T]
) extends Policy[A, R, EpsilonGreedy[A, R, T]] {

  /**
    * This doesn't necessarily break ties consistently. Check, and
    * note that we might want to break them randomly.
    */
  private def greedyAction: Option[A] =
    if (aggState.isEmpty)
      None
    else
      Some(aggState.maxBy({ case (k, v) => agg.present(v) })._1)

  override def choose(state: State[A, R]): Generator[A] =
    Util.epsilonGreedy(epsilon, greedyAction, state.actions).generator

  override def learn(state: State[A, R], action: A, reward: R): EpsilonGreedy[A, R, T] = {
    val oldV = aggState.getOrElse(action, agg.monoid.zero)
    copy(aggState = aggState + (action -> agg.reduce(oldV, agg.prepare(reward))))
  }
}

object EpsilonGreedy {
  def policy[A, R: Ordering, T](
      epsilon: Double,
      agg: MonoidAggregator[R, T, R]
  ): EpsilonGreedy[A, R, T] =
    EpsilonGreedy[A, R, T](epsilon, agg, Map.empty)

  /**
    * Same as the other arity, but allowed for
    */
  def policy[A, R: Ordering, T](
      epsilon: Double,
      agg: MonoidAggregator[R, T, R],
      initial: R
  ): EpsilonGreedy[A, R, T] = {
    val prepped = agg.prepare(initial)
    EpsilonGreedy[A, R, T](epsilon, agg, Map.empty[A, T].withDefault(k => prepped))
  }

  /**
    * Returns an incremental implementation.
    */
  def incremental[A](epsilon: Double, initial: Double = 0.0): EpsilonGreedy[A, Double, AveragedValue] =
    policy(epsilon, Util.averagingAgg, initial)
}
