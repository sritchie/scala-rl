package io.samritchie.rl

import cats.kernel.Semigroup
import com.stripe.rainier.compute.Real

/**
  trait representing the value of some action or state.
  */
trait Value[A] { self =>
  def get: A
  def plus(r: Value[A]): Value[A]
  def from(pathValue: A): Value[A]
  def weighted(r: Real): Value[A]
}

object Value {
  implicit def valueOrd[A: Ordering]: Ordering[Value[A]] =
    Ordering.by(_.get)

  implicit def valueSemigroup[A]: Semigroup[Value[A]] =
    Semigroup.instance(_.plus(_))
}
