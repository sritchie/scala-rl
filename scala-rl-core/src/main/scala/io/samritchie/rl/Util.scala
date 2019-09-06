/**
  * Extra stuff I'm discovering.
  */
package io.samritchie.rl

import cats.Monad
import com.twitter.algebird.{Aggregator, AveragedValue, Monoid, MonoidAggregator, Semigroup}
import com.stripe.rainier.compute.{Real, ToReal}
import com.stripe.rainier.core.Categorical

import scala.language.higherKinds

object Util {
  def prepareMonoid[A, B: Monoid: ToReal](
      prepare: A => B
  ): MonoidAggregator[A, B, Real] =
    Aggregator.prepareMonoid(prepare).andThenPresent(ToReal(_))

  object Instances {
    implicit val averageValueOrd: Ordering[AveragedValue] =
      Ordering.by(_.value)

    implicit val avToReal: ToReal[AveragedValue] =
      implicitly[ToReal[Double]].contramap(_.value)
  }

  def makeMap[K, V](keys: Set[K])(default: => V)(f: K => Option[V]): Map[K, V] =
    keys.foldLeft(Map.empty[K, V]) {
      case (m, k) =>
        m.updated(k, f(k).getOrElse(default))
    }

  def makeMap[K, V](keys: Set[K])(f: K => V): Map[K, V] =
    keys.foldLeft(Map.empty[K, V]) {
      case (m, k) =>
        m.updated(k, f(k))
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

  def generatorFromSet[A](items: Set[A]) =
    Categorical.fromSet(items).generator

  def softmax[A, B](m: Map[A, Real]): Categorical[A] =
    Categorical.normalize(m.mapValues(_.exp))

  def softmax[A: ToReal](as: Set[A]): Categorical[A] = {
    val (pmf, sum) = as.foldLeft((Map.empty[A, Real], Real.zero)) {
      case ((m, r), a) =>
        val realExp = ToReal(a).exp
        (m.updated(a, realExp), r + realExp)
    }
    Categorical.normalize(pmf.mapValues(_ / sum))
  }

  def iterateM[F[_]: Monad, A](n: Int)(a: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM[Tuple2[Int, A], A]((n, a)) {
      case (k, a) =>
        if (k <= 0)
          Monad[F].pure(Right(a))
        else
          Monad[F].map(f(a))(a2 => Left((k - 1, a2)))
    }
}
