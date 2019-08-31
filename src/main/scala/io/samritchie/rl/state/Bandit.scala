/**
  * A bandit is a particular kind of state.
  */
package io.samritchie.rl
package state

import com.stripe.rainier.core.{Generator, Normal}
import com.stripe.rainier.compute.Real

/**
  * MDP with a single state.
  */
case class Bandit[A, R](rewards: Map[A, Generator[R]]) extends State[A, R] {
  override def dynamics = rewards.mapValues(_.map(r => (r, this)))
}

object Bandit {
  object Arm {
    implicit val ordering: Ordering[Arm] = Ordering.by(_.i)
  }

  case class Arm(i: Int)

  /**
    * An "Arm" is something that takes you to a new state. We just
    * happen to have only a single state here, so it always takes you
    * back to a given "bandit" problem.
    */
  def arms(k: Int): Set[Arm] = (0 until k).map(Arm(_)).toSet

  /**
    * Returns a Generator that splits out states for each of the games
    * to play.
    */
  def initialStateGen(
      k: Int,
      meanGenerator: Generator[Double],
      stdDev: Double
  ): Generator[Bandit[Arm, Double]] =
    meanGenerator.repeat(k).map { means =>
      val m = arms(k).toSeq
        .zip(means)
        .foldLeft(Map.empty[Arm, Generator[Double]]) {
          case (m, (i, mean)) =>
            m + (i -> Normal(mean, stdDev).generator)
        }
      Bandit(m)
    }
}
