package io.samritchie.rl

import cats.kernel.Semigroup
import com.stripe.rainier.compute.Real

/**
  trait representing the value of some action or state.
  */
trait Value[T] { l =>
  def get: T
  def plus(r: Value[T]): Value[T]
  def from(pathValue: T): Value[T]
  def weighted(r: Real): Value[T]
}

object Value {
  implicit def valueOrd[T: Ordering]: Ordering[Value[T]] =
    Ordering.by(_.get)

  implicit def valueSemigroup[T]: Semigroup[Value[T]] =
    Semigroup.instance(_.plus(_))
}
