/**
  * Extra stuff I'm discovering.
  */
package io.samritchie.rl

import cats.{Comonad, Eq, Id, Monad}
import cats.arrow.FunctionK
import com.twitter.algebird.{Aggregator, AveragedValue, Monoid, MonoidAggregator, Semigroup}
import com.stripe.rainier.cats._
import com.stripe.rainier.compute.{Real, ToReal}
import com.stripe.rainier.core.{Categorical, Generator}

import scala.annotation.tailrec
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

    implicit val realOrd: Ordering[Real] =
      Ordering.fromLessThan { (l, r) =>
        Eq[Real].eqv(
          Real.lt(l, r, Real.zero, Real.one),
          Real.zero
        )
      }
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
  def diff[A](as: TraversableOnce[A], lf: A => Real, rf: A => Real, combine: (Real, Real) => Real): Real =
    as.foldLeft(Real.zero) { (acc, k) =>
      combine(acc, (lf(k) - rf(k)).abs)
    }

  /**
    Cats helpers.
    */
  val categoricalToGen: FunctionK[Categorical, Generator] =
    new FunctionK[Categorical, Generator] {
      def apply[A](ca: Categorical[A]): Generator[A] = ca.generator
    }

  def idToMonad[M[_]](implicit M: Monad[M]): FunctionK[Id, M] =
    new FunctionK[Id, M] {
      def apply[A](a: A): M[A] = M.pure(a)
    }

  def mfk[M[_], N[_]](implicit M: Comonad[M], N: Monad[N]): FunctionK[M, N] =
    new FunctionK[M, N] {
      def apply[A](ma: M[A]): N[A] = N.pure(M.extract(ma))
    }
}
