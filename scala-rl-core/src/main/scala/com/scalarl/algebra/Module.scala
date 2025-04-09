package com.scalarl
package algebra

import com.twitter.algebird.{Group, Ring, VectorSpace}

/** This class represents an abstract-algebraic "module". A module is a generalization of vector
  * spaces that allows scalars to come from a ring instead of a field. It consists of:
  *
  *   - An abelian group (G, +) representing the elements that can be scaled
  *   - A ring (R, +, *) representing the scalars
  *   - A scaling operation R × G → G that satisfies:
  *   - r(g₁ + g₂) = rg₁ + rg₂ (distributivity over group addition)
  *   - (r₁ + r₂)g = r₁g + r₂g (distributivity over ring addition)
  *   - (r₁r₂)g = r₁(r₂g) (compatibility with ring multiplication)
  *   - 1g = g (identity scalar)
  *
  * For more details see: https://en.wikipedia.org/wiki/Module_(mathematics)
  */
object Module {
  // the default module!
  type DModule[T] = Module[Double, T]

  /** This method is used to get the default module for a given type.
    *
    * @param M
    *   The module to get.
    * @return
    *   The default module for the given type.
    */
  @inline final def apply[R, G](implicit M: Module[R, G]): Module[R, G] = M

  /** supplies an implicit module, given an implicitly-available Ring for some type R.
    */
  implicit def ringModule[R: Ring]: Module[R, R] = from(Ring.times(_, _))

  /** Given an implicit ring and group, accepts a scaleFn that shows how to perform scalar
    * multiplication between elements of the ring and the group and returns a new module over R and
    * G.
    */
  def from[R, G](
      scaleFn: (R, G) => G
  )(implicit R: Ring[R], G: Group[G]): Module[R, G] =
    new Module[R, G] {
      override def ring = R
      override def group = G
      def scale(r: R, g: G) =
        if (R.isNonZero(r)) scaleFn(r, g) else G.zero
    }

  /* Algebird's vector space is generic on the container type C, and implicitly pulls in a group on
  C[F]. We are a little more general.
   */
  def fromVectorSpace[F, C[_]](implicit
      R: Ring[F],
      V: VectorSpace[F, C]
  ): Module[F, C[F]] = {
    implicit val g = V.group
    from[F, C[F]](V.scale(_, _))
  }
}

trait Module[R, G] extends Serializable {
  implicit def ring: Ring[R]
  implicit def group: Group[G]
  def scale(r: R, g: G): G
}
