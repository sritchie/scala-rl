package io.samritchie.rl
package algebra

import com.twitter.algebird.Monoid

/**
  Weight!
  */
case class Weight(w: Double) extends AnyVal {
  def +(r: Weight): Weight = Weight(w + r.w)
  def *(r: Weight): Weight = Weight(w * r.w)
  def /(r: Weight): Weight = Weight(w / r.w)
  def <(r: Weight): Boolean = w < r.w
}

object Weight {
  val one: Weight = Weight(1.0)
  val zero: Weight = Weight(0.0)
  implicit val timesMonoid: Monoid[Weight] = Monoid.from(one)(_ * _)
  implicit val ord: Ordering[Weight] = Ordering.by(_.w)
}
