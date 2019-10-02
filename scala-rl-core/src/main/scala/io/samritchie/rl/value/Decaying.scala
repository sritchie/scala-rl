package io.samritchie.rl
package value

/**
  This represents a value that's weighted as you move away from it. This is
  useful because we can KEEP GOING, and continue to weight it.
  */
case class Decaying(get: Double, gamma: Double) extends Value[Double] {
  def plus(r: Value[Double]) = Decaying(get + r.get, gamma)
  def from(reward: Double) = Decaying(reward + (get * gamma), gamma)
  def weighted(r: Double): Value[Double] = Decaying(r * get, gamma)
}
