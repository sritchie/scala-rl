package io.samritchie.rl

import com.twitter.algebird.{AveragedValue, DecayedValue, Monoid}

object Examples {
  // This should be in Algebird.
  implicit val averageValueOrd: Ordering[AveragedValue] = Ordering.by(_.value)
  implicit val dvMonoid: Monoid[DecayedValue] = DecayedValue.monoidWithEpsilon(0.1)

  /**
    * Returns an incremental implementation.
    */
  def epsGreedyIncremental[A <: Action](epsilon: Double): EpsilonGreedy[A, AveragedValue] =
    Policy.epsilonGreedy(epsilon, Monoid.zero[AveragedValue])

  /**
    * Returns a decaying value.
    */
  def epsGreedyExponentialDecay[A <: Action](epsilon: Double): EpsilonGreedy[A, DecayedValue] =
    Policy.epsilonGreedy[A, DecayedValue](epsilon, Monoid.zero[DecayedValue])
}

object EpsilonGreedyGraph {
  import Examples._

  val policy: EpsilonGreedy[Arm, AveragedValue] = epsGreedyIncremental(0.1)
  val instrumented: InstrumentedPolicy[Arm, AveragedValue, EpsilonGreedy[Arm, AveragedValue]] =
    InstrumentedPolicy(policy, _.rewards, Map.empty[Arm, List[AveragedValue]])

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
    *   time. (building this in Bandit.scala)
    *
    * - A way to INSTRUMENT my bandit that's independent of
    *   implementation, so I can start to get shit in there. Or
    *   instrument my state. something like that... so that I can see
    *   how the value of the map is evolving.
    *
    */
  val main = "cake"
}
