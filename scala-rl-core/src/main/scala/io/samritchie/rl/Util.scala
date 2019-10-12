/**
  * Extra stuff I'm discovering.
  */
package io.samritchie.rl

import cats.{Comonad, Id, Monad}
import cats.arrow.FunctionK
import cats.data.StateT
import com.twitter.algebird.{AveragedValue, MonoidAggregator, Semigroup}
import io.samritchie.rl.util.ToDouble

import scala.language.higherKinds

object Util {
  import cats.syntax.functor._

  object Instances {
    implicit val averageValueOrd: Ordering[AveragedValue] =
      Ordering.by(_.value)

    implicit val avToDouble: ToDouble[AveragedValue] =
      ToDouble.instance(_.value)
  }

  def confine[A](a: A, min: A, max: A)(implicit ord: Ordering[A]): A =
    ord.min(ord.max(a, min), max)

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
    M.iterateWhileM((n, a)) {
        case (k, a) =>
          f(a).map((k - 1, _))
      }(_._1 > 0)
      .map(_._2)

  /**
    A version of iterateUntilM that uses an aggregator to store the auxiliary
    results kicked out by the step function.
    */
  def iterateUntilM[M[_], A, B, C, D](init: A, agg: MonoidAggregator[B, C, D])(
      f: A => M[(A, B)]
  )(p: A => Boolean)(implicit M: Monad[M]): M[(A, D)] =
    M.iterateUntilM((init, agg.monoid.zero)) {
        case (a, c) =>
          f(a).map {
            case (a2, b) =>
              (a2, agg.append(c, b))
          }
      }(pair => p(pair._1))
      .map { case (a, c) => (a, agg.present(c)) }

  /**
    A version of iterateWhileM that uses an aggregator to store the auxiliary
    results kicked out by the step function.
    */
  def iterateWhileM[M[_]: Monad, A, B, C, D](init: A, agg: MonoidAggregator[B, C, D])(
      f: A => M[(A, B)]
  )(p: A => Boolean): M[(A, D)] =
    iterateUntilM(init, agg)(f)(!p(_))

  /**
    Unused for now... TODO try this out, get the interface going in state monad style!
    */
  def runUntilM[M[_]: Monad, S, A, B, C](
      state: StateT[M, S, A],
      agg: MonoidAggregator[A, B, C]
  )(p: S => Boolean): StateT[M, S, C] =
    StateT[M, S, C] { s =>
      iterateUntilM(s, agg)(state.run(_))(p)
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
