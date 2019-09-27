package io.samritchie.rl
package value

import com.stripe.rainier.compute.{Real, ToReal}

/**
  This represents a value that's weighted as you move away from it. This is
  useful because we can KEEP GOING, and continue to weight it.
  */
case class Decaying(get: Real, gamma: Double) extends Value[Real] {
  def plus(r: Value[Real]) = Decaying(get + r.get, gamma)
  def from(reward: Real) = Decaying(reward + (get * gamma), gamma)
  def weighted(r: Real): Value[Real] = Decaying(r * get, gamma)
}

object Decaying {
  implicit val decayToReal: ToReal[Decaying] =
    ToReal.fromReal.contramap(_.get)
}
