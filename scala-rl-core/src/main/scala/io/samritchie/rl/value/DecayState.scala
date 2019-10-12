package io.samritchie.rl
package value

import com.twitter.algebird.Monoid

/**
  This represents a value that's weighted as you move away from it. This is
  useful because we can KEEP GOING, and continue to weight it.
  */
// Play with this as a way to make the monoid a thing.
sealed trait DecayState extends Product with Serializable {
  def toValue: DecayedValue
  def toDouble: Double = toValue.get
}
case class Reward(r: Double) extends DecayState {
  override lazy val toValue: DecayedValue = DecayedValue(r)
}
case class DecayedValue(get: Double) extends DecayState {
  override val toValue: DecayedValue = this
}

object DecayState {
  implicit def monoidDecayState(gamma: Double): Monoid[DecayState] =
    new Monoid[DecayState] {
      override val zero = DecayedValue(0.0)
      override def plus(l: DecayState, r: DecayState) = (l, r) match {
        case (Reward(a), Reward(b))             => Reward(a + b)
        case (DecayedValue(a), Reward(b))       => DecayedValue((a * gamma) + b)
        case (Reward(a), DecayedValue(b))       => DecayedValue((b * gamma) + a)
        case (DecayedValue(a), DecayedValue(b)) => DecayedValue(a + b)
      }
    }
}
