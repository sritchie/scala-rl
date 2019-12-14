package io.samritchie.rl

import cats.Id
import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.Categorical
import com.twitter.algebird.Ring

trait Weighted[M[_], R] extends Serializable {
  def ring: Ring[R]
  def weights[A](ma: M[A]): Iterator[(A, R)]
}

object Weighted {
  @inline final def apply[M[_], R](implicit W: Weighted[M, R]): Weighted[M, R] = W

  implicit def id[R](implicit R: Ring[R]): Weighted[Id, R] =
    new Weighted[Id, R] {
      override def ring = R
      override def weights[A](a: A): Iterator[(A, R)] = Iterator((a, R.one))
    }

  implicit def categoricalDouble(implicit n: Numeric[Real]): Weighted[Categorical, Double] =
    new Weighted[Categorical, Double] {
      override val ring = Ring.doubleRing
      override def weights[A](ma: Categorical[A]): Iterator[(A, Double)] =
        ma.pmf.iterator.map { case (a, r) => (a, n.toDouble(r)) }
    }

  implicit val categoricalReal: Weighted[Categorical, Real] =
    new Weighted[Categorical, Real] {
      override val ring = Util.Instances.RealRing
      override def weights[A](ma: Categorical[A]): Iterator[(A, Real)] =
        ma.pmf.iterator
    }
}
