package io.samritchie.rl
package algebra

import cats.Id
import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.{Categorical => RCat}
import com.twitter.algebird.{DoubleRing, Ring}
import io.samritchie.rl.rainier.Categorical

trait Decompose[M[_], R] extends Serializable {
  def ring: Ring[R]
  def decompose[A](ma: M[A]): Iterator[(A, R)]
}

object Decompose {
  @inline final def apply[M[_], R](implicit W: Decompose[M, R]): Decompose[M, R] = W

  implicit def id[R](implicit R: Ring[R]): Decompose[Id, R] =
    new Decompose[Id, R] {
      override def ring = R
      override def decompose[A](a: A): Iterator[(A, R)] = Iterator((a, R.one))
    }

  implicit def rcatDouble(implicit n: Numeric[Real]): Decompose[RCat, Double] =
    new Decompose[RCat, Double] {
      override val ring = DoubleRing
      override def decompose[A](ma: RCat[A]): Iterator[(A, Double)] =
        ma.pmf.iterator.map { case (a, r) => (a, n.toDouble(r)) }
    }

  implicit val rcatReal: Decompose[RCat, Real] =
    new Decompose[RCat, Real] {
      override val ring = Util.Instances.RealRing
      override def decompose[A](ma: RCat[A]): Iterator[(A, Real)] =
        ma.pmf.iterator
    }

  implicit val categoricalDouble: Decompose[Categorical, Double] =
    new Decompose[Categorical, Double] {
      override val ring = DoubleRing
      override def decompose[A](ma: Categorical[A]): Iterator[(A, Double)] =
        ma.pmfSeq.iterator
    }
}
