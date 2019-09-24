/**
  * Policy that accumulates via epsilon greedy.
  */
package io.samritchie.rl
package policy

import cats.Monad
import com.stripe.rainier.core.{Categorical, Generator}
import com.stripe.rainier.cats._
import com.twitter.algebird.{AveragedValue, Semigroup}
import Util.Instances._

/**
  * This is a version that accumulates the reward using a monoid.
  *
  * @param epsilon number between 0 and 1.
  */
case class EpsilonGreedy[A, R, T: Semigroup: Ordering](
    config: EpsilonGreedy.Config[R, T],
    actionValues: Map[A, T]
) extends CategoricalPolicy[A, Any, R, Generator, EpsilonGreedy[A, R, T]] {
  private val explore: Categorical[Boolean] =
    Categorical.boolean(config.epsilon)

  private def allActions(state: State[A, Any, R, Generator]): Categorical[A] =
    Categorical.list(state.actions.toList)

  private def greedy(state: State[A, Any, R, Generator]): Categorical[A] =
    Categorical.fromSet(
      Util.allMaxBy(state.actions)(actionValues.getOrElse(_, config.initial))
    )

  override def categories(state: State[A, Any, R, Generator]): Categorical[A] =
    Monad[Categorical]
      .ifM(explore)(
        allActions(state),
        greedy(state)
      )

  override def learn(state: State[A, Any, R, Generator], action: A, reward: R): EpsilonGreedy[A, R, T] =
    copy(actionValues = Util.mergeV(actionValues, action, config.prepare(reward)))
}

// Oh boy, this really does look like it needs an aggregator... maybe
// I build it without, but then include the algebird versions
// elsewhere? Or maybe I build to the cats interfaces, then I have an
// algebird package? More for later.
object EpsilonGreedy {
  case class Config[R, T: Semigroup: Ordering](
      epsilon: Double,
      prepare: R => T,
      initial: T
  ) {
    def policy[A]: EpsilonGreedy[A, R, T] =
      EpsilonGreedy[A, R, T](this, Map.empty)
  }

  /**
    * Returns an incremental config.
    *
    * TODO we also need a version that uses a constant step size,
    * instead of sample averages. And maybe a version that uses
    * exponential decay?
    *
    */
  def incrementalConfig(
      epsilon: Double,
      initial: Double = 0.0
  ): Config[Double, AveragedValue] = Config(
    epsilon,
    AveragedValue(_),
    AveragedValue(initial)
  )
}
