/**
  * Extra stuff I'm discovering.
  */
package io.samritchie.rl

import cats.Monad
import com.twitter.algebird.{AveragedValue, Semigroup}
import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.Categorical

import scala.language.higherKinds

object Util {
  object Instances {
    implicit val averageValueOrd: Ordering[AveragedValue] =
      Ordering.by(_.value)
  }

  def updateWith[K, V](m: Map[K, V], k: K)(f: Option[V] => V): Map[K, V] =
    m.updated(k, f(m.get(k)))

  def mergeV[K, V: Semigroup](m: Map[K, V], k: K, delta: V): Map[K, V] =
    updateWith(m, k) {
      case None    => delta
      case Some(v) => Semigroup.plus[V](v, delta)
    }

  def allMaxBy[A, B: Ordering](as: Set[A])(f: A => B): Set[A] = {
    val maxB = f(as.maxBy(f))
    as.filter(a => Ordering[B].equiv(maxB, f(a)))
  }

  def categoricalFromSet[A](items: Set[A]): Categorical[A] =
    Categorical.normalize(
      items.foldLeft(Map.empty[A, Real])((m, a) => m.updated(a, Real.one))
    )

  def generatorFromSet[A](items: Set[A]) =
    categoricalFromSet(items).generator

  def iterateM[F[_]: Monad, A](n: Int)(a: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM[Tuple2[Int, A], A]((n, a)) {
      case (k, a) =>
        if (k <= 0)
          Monad[F].pure(Right(a))
        else
          Monad[F].map(f(a))(a2 => Left((k - 1, a2)))
    }
}
