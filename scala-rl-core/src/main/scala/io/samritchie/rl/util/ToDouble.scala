package io.samritchie.rl
package util

import scala.{specialized => sp}

trait ToDouble[@sp(Int, Long, Float, Double) A] extends Any with Serializable { self =>
  def apply(a: A): Double

  def contramap[B](f: B => A): ToDouble[B] = new ToDouble[B] {
    def apply(b: B): Double = self.apply(f(b))
  }
}

object ToDouble {
  @inline final def apply[A](implicit ev: ToDouble[A]): ToDouble[A] = ev
  @inline def instance[A](toDouble: A => Double): ToDouble[A] = new ToDouble[A] {
    override def apply(a: A): Double = toDouble(a)
  }

  implicit val fromDouble: ToDouble[Double] = instance(d => d)
  implicit def numericToDouble[A](implicit N: Numeric[A]): ToDouble[A] =
    instance(N.toDouble(_))
}
