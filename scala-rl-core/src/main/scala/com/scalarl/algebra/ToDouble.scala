package com.scalarl
package algebra

import scala.{specialized => sp}

/** Typeclass that encodes how some type A can be converted into a Double instance.
  */
trait ToDouble[@sp(Int, Long, Float, Double) A] extends Any with Serializable { self =>
  def apply(a: A): Double

  def contramap[B](f: B => A): ToDouble[B] = new ToDouble[B] {
    def apply(b: B): Double = self.apply(f(b))
  }
}

object ToDouble {

  /** Access an implicit `[[ToDouble]][A]`.
    */
  @inline final def apply[A](implicit ev: ToDouble[A]): ToDouble[A] = ev

  /** Generates an instance of[[ToDouble]] from a pure function.
    */
  @inline def instance[A](toDouble: A => Double): ToDouble[A] = new ToDouble[A] {
    override def apply(a: A): Double = toDouble(a)
  }

  /** The [[ToDouble]] instance for doubles uses the identity function.
    */
  implicit val fromDouble: ToDouble[Double] = instance(d => d)

  /** Any type A that conforms to the Numeric typeclass can be converted to double via the toDouble method on
    * that typeclass.
    */
  implicit def numericToDouble[A](implicit N: Numeric[A]): ToDouble[A] =
    instance(N.toDouble(_))
}
