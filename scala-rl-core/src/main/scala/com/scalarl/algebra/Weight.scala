package com.scalarl
package algebra

import com.twitter.algebird.Monoid

/**
  Value class that represents some Double-valued weight that can be applied to a
  type.
  */
case class Weight(w: Double) extends AnyVal {
  def +(r: Weight): Weight = Weight(w + r.w)
  def *(r: Weight): Weight = Weight(w * r.w)
  def /(r: Weight): Weight = Weight(w / r.w)
  def <(r: Weight): Boolean = w < r.w
}

object Weight {

  /**

    */
  val One: Weight = Weight(1.0)

  /**

    */
  val Zero: Weight = Weight(0.0)

  implicit val timesMonoid: Monoid[Weight] = Monoid.from(One)(_ * _)
  implicit val ord: Ordering[Weight] = Ordering.by(_.w)
}
