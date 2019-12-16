package com.scalarl
package algebra

import com.twitter.algebird.{Group, Ring, VectorSpace}

/**
  * This class represents a module. For the required properties see:
  *
  * https://en.wikipedia.org/wiki/Module_(mathematics)
  *
  */
object Module {
  type DModule[T] = Module[Double, T]

  @inline final def apply[R, G](implicit M: Module[R, G]): Module[R, G] = M

  implicit def ringModule[R: Ring]: Module[R, R] = from(Ring.times(_, _))

  def from[R, G](scaleFn: (R, G) => G)(implicit R: Ring[R], G: Group[G]): Module[R, G] =
    new Module[R, G] {
      override def ring = R
      override def group = G
      def scale(r: R, g: G) =
        if (R.isNonZero(r)) scaleFn(r, g) else G.zero
    }

  def fromVectorSpace[F, C[_]](implicit R: Ring[F], V: VectorSpace[F, C]): Module[F, C[F]] = {
    implicit val g = V.group
    from[F, C[F]](V.scale(_, _))
  }
}

trait Module[R, G] extends Serializable {
  implicit def ring: Ring[R]
  implicit def group: Group[G]
  def scale(r: R, g: G): G
}
