package com.scalarl
package algebra

import cats.Id
import com.twitter.algebird.Ring

/**
  Another attempt at a better thing, here... but I don't know if this solves my
  problem of needing to compose up the stack,
  */
trait ConvexCombination[M[_], R] {
  implicit def ring: Ring[R]
  def get[A](ma: M[A])(f: A => R): R
}

object ConvexCombination {
  // Contract is that if all A == R.one, and f = _ => R.one, the fn returns
  // R.one.
  def take[A, R: Ring](items: Iterator[(A, R)])(f: A => R)(implicit R: Ring[R]): R =
    R.sum(items.map { case (a, r) => R.times(f(a), r) })

  @inline final def apply[M[_], R](implicit M: ConvexCombination[M, R]): ConvexCombination[M, R] = M

  implicit def id[R](implicit R: Ring[R]): ConvexCombination[Id, R] =
    new ConvexCombination[Id, R] {
      implicit val ring = R
      def get[A](a: A)(f: A => R) = f(a)
    }

  implicit def fromDecomposition[M[_], R](implicit D: Decompose[M, R]): ConvexCombination[M, R] =
    new ConvexCombination[M, R] {
      implicit def ring = D.ring
      def get[A](ma: M[A])(f: A => R) = take(D.decompose(ma))(f)
    }
}
