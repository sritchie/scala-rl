package io.samritchie.rl
package value

import com.twitter.algebird.Group

/**
  This represents a value that's weighted as you move away from it. This is
  useful because we can KEEP GOING, and continue to weight it.
  */
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
  def module(gamma: Double): Module[Double, DecayState] = {
    implicit val group: Group[DecayState] = decayStateGroup(gamma)
    Module.from(
      (r, d) =>
        d match {
          case Reward(reward)  => Reward(r * reward)
          case DecayedValue(v) => DecayedValue(r * v)
        }
    )
  }

  def decayStateGroup(gamma: Double): Group[DecayState] =
    new Group[DecayState] {
      override val zero = DecayedValue(0.0)
      override def negate(d: DecayState) = d match {
        case Reward(r)       => Reward(-r)
        case DecayedValue(v) => DecayedValue(-v)
      }
      override def plus(l: DecayState, r: DecayState) = (l, r) match {
        case (Reward(a), Reward(b))             => Reward(a + b)
        case (DecayedValue(a), Reward(b))       => DecayedValue((a * gamma) + b)
        case (Reward(a), DecayedValue(b))       => DecayedValue((b * gamma) + a)
        case (DecayedValue(a), DecayedValue(b)) => DecayedValue(a + b)
      }
    }
}
