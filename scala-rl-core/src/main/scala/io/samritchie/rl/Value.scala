package io.samritchie.rl

import cats.kernel.Semigroup
import scala.{specialized => sp}

/**
  trait representing the value of some action or state.
  */
trait Value[@sp(Int, Long, Float, Double) A] extends Any with Serializable { self =>
  def get: A
  def plus(r: Value[A]): Value[A]
  def from(pathValue: A): Value[A]
  def weighted(r: Double): Value[A]
}

object Value {
  implicit def valueOrd[A: Ordering]: Ordering[Value[A]] =
    Ordering.by(_.get)

  implicit def valueSemigroup[A]: Semigroup[Value[A]] =
    Semigroup.instance(_.plus(_))
}
