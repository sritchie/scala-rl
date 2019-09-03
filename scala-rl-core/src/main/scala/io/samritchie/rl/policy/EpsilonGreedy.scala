/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.core.{Categorical, Generator}
import com.twitter.algebird.{AveragedValue, Monoid}
import Util.Instances._

/**
  * This is a version that accumulates the reward using a monoid.
  *
  * @param epsilon number between 0 and 1.
  */
case class EpsilonGreedy[A, R, T: Monoid: Ordering](
    epsilon: Double,
    prepare: R => T,
    initial: T,
    aggState: Map[A, T]
) extends Policy[A, R, EpsilonGreedy[A, R, T]] {

  lazy val explore: Categorical[Boolean] =
    Categorical.boolean(epsilon)

  private def allActions(state: State[A, R]): Categorical[A] =
    Categorical.list(state.actions.toList)

  private def greedy(state: State[A, R]): Categorical[A] = {
    val pairs = state.actions.map { a =>
      (a, aggState.getOrElse(a, initial))
    }
    val max = pairs.maxBy(_._2)._2
    Categorical
      .list(
        pairs
          .filter { case (a, t) => Ordering[T].equiv(max, t) }
          .map(_._1)
          .toList
      )
  }

  override def choose(state: State[A, R]): Generator[A] =
    explore.flatMap { exploreP =>
      if (exploreP) allActions(state)
      else greedy(state)
    }.generator

  override def learn(state: State[A, R], action: A, reward: R): EpsilonGreedy[A, R, T] = {
    val oldV = aggState.getOrElse(action, Monoid.zero)
    copy(aggState = aggState + (action -> Monoid.plus(oldV, prepare(reward))))
  }
}

object EpsilonGreedy {
  def policy[A, R, T: Monoid: Ordering](epsilon: Double)(prepare: R => T): EpsilonGreedy[A, R, T] =
    EpsilonGreedy[A, R, T](epsilon, prepare, Monoid.zero, Map.empty)

  /**
    * Same as the other arity, but allowed for
    */
  def policy[A, R, T: Monoid: Ordering](
      epsilon: Double,
      initial: T
  )(prepare: R => T): EpsilonGreedy[A, R, T] =
    EpsilonGreedy[A, R, T](epsilon, prepare, initial, Map.empty)

  /**
    * Returns an incremental implementation.
    */
  def incremental[A](epsilon: Double, initial: Double = 0.0): EpsilonGreedy[A, Double, AveragedValue] =
    EpsilonGreedy(epsilon, AveragedValue(_), AveragedValue(initial), Map.empty)
}
