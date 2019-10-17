package io.samritchie.rl
package value

import com.twitter.algebird.Group

/**
  This represents a value that's weighted as you move away from it. This is
  useful because we can KEEP GOING, and continue to weight it.
  */
case class Decaying(get: Double, gamma: Double) extends Value[Double] {
  def plus(r: Value[Double]) = Decaying(get + r.get, gamma)
  def from(reward: Double) = Decaying(reward + (get * gamma), gamma)
  def weighted(r: Double): Value[Double] = Decaying(r * get, gamma)
}

object Decaying {
  def module(gamma: Double): Module[Double, Decaying] = {
    implicit val group: Group[Decaying] = decayingGroup(gamma)
    Module.from((r, d) => Decaying(r * d.get, d.gamma))
  }

  def decayingGroup(gamma: Double): Group[Decaying] =
    new Group[Decaying] {
      override val zero = Decaying(0.0, gamma)
      override def plus(l: Decaying, r: Decaying) = l.plus(r)
      override def negate(d: Decaying) = Decaying(-d.get, gamma)
    }
}
