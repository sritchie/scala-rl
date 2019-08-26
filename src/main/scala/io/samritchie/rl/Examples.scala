package io.samritchie.rl

import com.twitter.algebird.{AveragedValue, DecayedValue, Monoid}

object Examples {
  // This should be in Algebird.
  implicit val averageValueOrd: Ordering[AveragedValue] = Ordering.by(_.value)
  implicit val dvMonoid: Monoid[DecayedValue] = DecayedValue.monoidWithEpsilon(0.1)

  /**
    * Returns an incremental implementation.
    */
  def epsGreedyIncremental[A <: Action](epsilon: Double): Policy[A, AveragedValue] =
    Policy.epsilonGreedy(epsilon)

  /**
    * Returns a decaying value.
    */
  def epsGreedyExponentialDecay[A <: Action](epsilon: Double): Policy[A, DecayedValue] =
    Policy.epsilonGreedy[A, DecayedValue](epsilon)
}


object FirstExample {
  /**
    * I think I'm in a place where I can actually play that first game!
    */
}
