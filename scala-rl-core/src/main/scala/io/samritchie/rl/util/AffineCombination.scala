package io.samritchie.rl
package util

import cats.Id
import com.twitter.algebird.{DoubleRing, Ring}
import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.Categorical

/**
  Another attempt at a better thing, here... but I don't know if this solves my
  problem of needing to compose up the stack,
  */
trait AffineCombination[M[_], R] {
  implicit def ring: Ring[R]
  def get[A](ma: M[A])(f: A => R): R
}

object AffineCombination {
  // Contract is that if all A == R.one, and f = _ => R.one, the fn returns
  // R.one.
  def take[A, R: Ring](items: Iterator[(A, R)])(f: A => R)(implicit R: Ring[R]): R =
    R.sum(items.map { case (a, r) => R.times(f(a), r) })

  @inline final def apply[M[_], R](implicit M: AffineCombination[M, R]): AffineCombination[M, R] = M

  implicit def id[R](implicit R: Ring[R]): AffineCombination[Id, R] =
    new AffineCombination[Id, R] {
      implicit val ring = R
      def get[A](a: A)(f: A => R) = f(a)
    }

  implicit def fromWeighted[M[_], R](implicit W: Weighted[M, R]): AffineCombination[M, R] =
    new AffineCombination[M, R] {
      implicit def ring = W.ring
      def get[A](ma: M[A])(f: A => R) = take(W.weights(ma))(f)
    }

  implicit val categoricalReal: AffineCombination[Categorical, Real] =
    new AffineCombination[Categorical, Real] {
      implicit val ring = Util.Instances.RealRing
      def get[A](a: Categorical[A])(f: A => Real) = take(a.pmf.iterator)(f)
    }

  implicit def categoricalDouble(implicit n: Numeric[Real]): AffineCombination[Categorical, Double] =
    new AffineCombination[Categorical, Double] {
      implicit val ring = DoubleRing
      def get[A](a: Categorical[A])(f: A => Double) =
        take(a.pmf.iterator.map { case (a, r) => (a, n.toDouble(r)) })(f)
    }
}
