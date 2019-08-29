/**
  * Code to implement a bandit.
  */
package io.samritchie.rl

import com.stripe.rainier.core.{Generator, Normal}
import com.stripe.rainier.compute.{Real, ToReal}

object Arm {
  implicit val ordering: Ordering[Arm] = Ordering.by(_.i)
}
case class Arm(i: Int)

object FakeBandit {
  /**
    * An "Arm" is something that takes you to a new state. We just
    * happen to have only a single state here, so it always takes you
    * back to a given "bandit" problem.
    */
  def arms(k: Int): Set[Arm] = (0 to k).map(Arm(_)).toSet

  /**
    * Generates a GENERATOR that splits out states for each of the
    * games to play.
    */
  def initialBanditGenerator(
    k: Int,
    meanGenerator: Generator[Real],
    stdDev: Real
  ): Generator[State[Arm, Double]] = {
    meanGenerator.map { mean =>
      val m = arms(k).foldLeft(Map.empty[Arm, Generator[Double]]) { (m, i) =>
        m + (i -> Normal(mean, stdDev).generator)
      }
      State.bandit(m)
    }
  }
}
