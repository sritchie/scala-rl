/**
  * Extra stuff I'm discovering.
  */
package io.samritchie.rl

import cats.{Comonad, Id, Monad}
import cats.arrow.FunctionK
import com.twitter.algebird.{Aggregator, AveragedValue, Monoid, MonoidAggregator, Semigroup}
import io.samritchie.rl.util.ToDouble

import scala.annotation.tailrec
import scala.language.higherKinds

object Util {
  import cats.syntax.functor._

  def prepareMonoid[A, B: Monoid: ToDouble](
      prepare: A => B
  ): MonoidAggregator[A, B, Double] =
    Aggregator.prepareMonoid(prepare).andThenPresent(ToDouble[B].apply(_))

  object Instances {
    implicit val averageValueOrd: Ordering[AveragedValue] =
      Ordering.by(_.value)

    implicit val avToDouble: ToDouble[AveragedValue] =
      ToDouble.instance(_.value)
  }

  def confine[A](a: A, min: A, max: A)(implicit ord: Ordering[A]): A =
    ord.min(ord.max(a, min), max)

  def makeMap[K, V](keys: Set[K], default: => V)(f: K => Option[V]): Map[K, V] =
    keys.foldLeft(Map.empty[K, V]) {
      case (m, k) =>
        m.updated(k, f(k).getOrElse(default))
    }

  def makeMap[K, V](keys: Set[K])(f: K => V): Map[K, V] = makeMapUnsafe(keys)(f)
  def makeMapUnsafe[K, V](keys: TraversableOnce[K])(f: K => V): Map[K, V] =
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

  def allMaxBy[A, B: Ordering](as: Set[A])(f: A => B): Set[A] =
    if (as.isEmpty) Set.empty
    else {

      val maxB = f(as.maxBy(f))
      as.filter(a => Ordering[B].equiv(maxB, f(a)))
    }

  def iterateM[M[_], A](n: Int)(a: A)(f: A => M[A])(implicit M: Monad[M]): M[A] =
    M.tailRecM[Tuple2[Int, A], A]((n, a)) {
      case (k, a) =>
        if (k <= 0)
          M.pure(Right(a))
        else
          f(a).map(a2 => Left((k - 1, a2)))
    }

  def iterateUntilM[M[_], A, B, C, D](init: A, agg: MonoidAggregator[B, C, D])(
      f: A => M[(A, B)]
  )(p: A => Boolean)(implicit M: Monad[M]): M[(A, D)] =
    M.iterateWhileM((init, agg.monoid.zero)) {
        case (a, c) =>
          f(a).map {
            case (a2, b) =>
              (a2, agg.append(c, b))
          }
      }(pair => p(pair._1))
      .map { case (a, c) => (a, agg.present(c)) }

  def iterateWhileM[M[_]: Monad, A, B, C, D](init: A, agg: MonoidAggregator[B, C, D])(
      f: A => M[(A, B)]
  )(p: A => Boolean): M[(A, D)] =
    iterateUntilM(init, agg)(f)(!p(_))

  @tailrec
  def loopWhile[A, B](init: A)(f: A => Either[A, B]): B =
    f(init) match {
      case Left(a)  => loopWhile(a)(f)
      case Right(b) => b
    }

  /**
    Accumulates differences between the two for every A in the supplied
    sequence. The combine function is used to aggregate the differences.

    I recommend using max or +.
    */
  def diff[A](
      as: TraversableOnce[A],
      lf: A => Double,
      rf: A => Double,
      combine: (Double, Double) => Double
  ): Double =
    as.foldLeft(0.0) { (acc, k) =>
      combine(acc, (lf(k) - rf(k)).abs)
    }

  /**
    Cats helpers.
    */
  def idToMonad[M[_]](implicit M: Monad[M]): FunctionK[Id, M] =
    new FunctionK[Id, M] {
      def apply[A](a: A): M[A] = M.pure(a)
    }

  def mfk[M[_], N[_]](implicit M: Comonad[M], N: Monad[N]): FunctionK[M, N] =
    new FunctionK[M, N] {
      def apply[A](ma: M[A]): N[A] = N.pure(M.extract(ma))
    }
}
