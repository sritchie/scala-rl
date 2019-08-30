package io.samritchie.rl

import com.twitter.algebird.{Aggregator, AveragedValue, Monoid}

object Examples {
  // This should be in Algebird.
  implicit val averageValueOrd: Ordering[AveragedValue] = Ordering.by(_.value)

  /**
    * Returns an incremental implementation.
    */
  def epsGreedyIncremental[A](epsilon: Double): EpsilonGreedy[A, Double, AveragedValue] =
    Policy.epsilonGreedy(
      epsilon,
      Aggregator.prepareMonoid[Double, AveragedValue](d => AveragedValue(d)).andThenPresent(_.value),
      0.0
    )
}

object EpsilonGreedyGraph {
  import Examples._

  val policy: EpsilonGreedy[Arm, Double, AveragedValue] = epsGreedyIncremental(0.1)
  val instrumented: InstrumentedPolicy[Arm, Double, EpsilonGreedy[Arm, Double, AveragedValue]] =
    InstrumentedPolicy(policy, _.aggState.mapValues(_.value), Map.empty[Arm, List[Double]])

  // FUCK we do need some type parameter for aggregation.
  // val state: State[Arm, AveragedValue] = State.bandit()

  /**
    * I think I'm in a place where I can actually play that first
    * game!
    *
    * I've got my:
    *
    * - epsilon greedy policy that can work with averaged values. This is a bandit.
    * - bandit STATE.
    *
    * I need:
    *
    * - A way to build the uniformly distributed bandit state that
    *   they use in the example, and get a new one each
    *   time. (DONE)
    *
    * - A way to INSTRUMENT my bandit that's independent of
    *   implementation, so I can start to get shit in there. Or
    *   instrument my state. something like that... so that I can see
    *   how the value of the map is evolving. (DONE)
    *
    * - a way to roll a game forward by some number of turns, and then
    *   example what we've seen... to examine the guts of one of these
    *   things. (NEXT DO THIS!)
    *
    */
  def main(items: Array[String]): Unit = {
    println(instrumented)
    println("Hello World again!!")
  }
}
