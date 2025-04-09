package com.scalarl

import cats.{Comonad, Id, Monad}
import cats.arrow.FunctionK
import cats.data.StateT
import com.twitter.algebird.{AveragedValue, Fold, MonoidAggregator, Ring, Semigroup, VectorSpace}
import com.stripe.rainier.compute.Real
import com.scalarl.algebra.{Module, ToDouble}

import scala.language.higherKinds

object Util {
  import cats.syntax.functor._

  /** Here we provide various "missing" typeclass instances sewing together algebird typeclasses and
    * implementing typeclasses for rainier types.
    */
  object Instances {
    // this lets us sort AveragedValue instances...
    implicit val averageValueOrd: Ordering[AveragedValue] =
      Ordering.by(_.value)

    // shows how to extract the averaged value out from the accumulating data structure
    implicit val avToDouble: ToDouble[AveragedValue] =
      ToDouble.instance(_.value)

    // Module instance, representing a module that can scale AveragedValue by some scalar double.
    implicit val avModule: Module[Double, AveragedValue] =
      Module.from((r, av) => AveragedValue(av.count, r * av.value))

    // easy, just expose this implicitly.
    implicit val realRing: Ring[Real] = RealRing

    // trivial VectorSpace, showing that the cats.Id monad (and any Ring R)  form a vectorspace.
    implicit def idVectorSpace[R](implicit R: Ring[R]): VectorSpace[R, Id] =
      VectorSpace.from[R, Id](R.times(_, _))

    // Ring instance for rainer Reals.
    object RealRing extends Ring[Real] {
      override def one = Real.one
      override def zero = Real.zero
      override def negate(v: Real) = -v
      override def plus(l: Real, r: Real) = l + r
      override def minus(l: Real, r: Real) = l - r
      override def times(l: Real, r: Real) = l * r
    }
  }

  /** Clamps a value between a minimum and maximum value.
    *
    * This function ensures that the input value `a` is not less than `min` and not greater than
    * `max`, returning the clamped value.
    *
    * @param a
    *   The value to clamp.
    * @param min
    *   The minimum value.
    */
  def clamp[A](a: A, min: A, max: A)(implicit ord: Ordering[A]): A =
    ord.min(ord.max(a, min), max)

  /** Creates a Map from a set of keys using a function to generate values.
    *
    * This function takes a set of keys and a function that maps each key to a value, returning a
    * Map with the keys and their corresponding values.
    *
    * @param keys
    */
  def makeMap[K, V](keys: Set[K])(f: K => V): Map[K, V] = makeMapUnsafe(keys)(f)

  /** similar to makeMap, but doesn't guarantee that there are not duplicate keys. If keys contains
    * duplicates, later keys override earlier keys.
    *
    * @param keys
    */
  def makeMapUnsafe[K, V](keys: TraversableOnce[K])(f: K => V): Map[K, V] =
    keys.foldLeft(Map.empty[K, V]) { case (m, k) =>
      m.updated(k, f(k))
    }

  /** Update the key in the supplied map using the function - the function handles both cases, when
    * the item is there and when it's not.
    */
  def updateWith[K, V](m: Map[K, V], k: K)(f: Option[V] => V): Map[K, V] =
    m.updated(k, f(m.get(k)))

  /** Merges a key and a value into a map using a semigroup to combine values. */
  def mergeV[K, V: Semigroup](m: Map[K, V], k: K, delta: V): Map[K, V] =
    updateWith(m, k) {
      case None    => delta
      case Some(v) => Semigroup.plus[V](v, delta)
    }

  /** Finds the keys with the maximum values in a map.
    */
  def maxKeys[A, B: Ordering](m: Map[A, B]): Set[A] = allMaxBy(m.keySet)(m(_))

  /** Returns the set of keys that map (via `f`) to the maximal B, out of all `as` transformed.
    */
  def allMaxBy[A, B: Ordering](as: Set[A])(f: A => B): Set[A] =
    if (as.isEmpty) Set.empty
    else {
      val maxB = f(as.maxBy(f))
      as.filter(a => Ordering[B].equiv(maxB, f(a)))
    }

  /** Iterates a monadic function `f` `n` of times using the starting value `a`.
    */
  def iterateM[M[_], A](
      n: Int
  )(a: A)(f: A => M[A])(implicit M: Monad[M]): M[A] =
    M.iterateWhileM((n, a)) { case (k, a) =>
      f(a).map((k - 1, _))
    }(_._1 > 0)
      .map(_._2)

  /** A version of iterateUntilM that uses an aggregator to store the auxiliary results kicked out
    * by the step function.
    */
  def iterateUntilM[M[_], A, B, C, D](init: A, agg: MonoidAggregator[B, C, D])(
      f: A => M[(A, B)]
  )(p: A => Boolean)(implicit M: Monad[M]): M[(A, D)] =
    M.iterateUntilM((init, agg.monoid.zero)) { case (a, c) =>
      f(a).map { case (a2, b) =>
        (a2, agg.append(c, b))
      }
    }(pair => p(pair._1))
      .map { case (a, c) => (a, agg.present(c)) }

  /** A version of iterateUntilM that uses a Fold to store the auxiliary results kicked out by the
    * step function.
    */
  def foldUntilM[M[_], A, B, C](init: A, fold: Fold[B, C])(
      f: A => M[(A, B)]
  )(p: A => Boolean)(implicit M: Monad[M]): M[(A, C)] = {
    val foldState = fold.build()
    M.iterateUntilM((init, foldState.start)) { case (a, c) =>
      f(a).map { case (a2, b) =>
        (a2, foldState.add(c, b))
      }
    }(pair => p(pair._1))
      .map { case (a, c) => (a, foldState.end(c)) }
  }

  /** And a helper function that will let me test this out with monoid aggregators, like the ones I
    * wrote to walk trajectories.
    */
  def aggToFold[A, B, C](agg: MonoidAggregator[A, B, C]): Fold[A, C] =
    Fold.fold[B, A, C](
      start = agg.monoid.zero,
      add = (b, a) => agg.monoid.plus(b, agg.prepare(a)),
      end = agg.present(_)
    )

  /** A version of iterateWhileM that uses an aggregator to store the auxiliary results kicked out
    * by the step function.
    */
  def iterateWhileM[M[_]: Monad, A, B, C, D](
      init: A,
      agg: MonoidAggregator[B, C, D]
  )(
      f: A => M[(A, B)]
  )(p: A => Boolean): M[(A, D)] =
    iterateUntilM(init, agg)(f)(!p(_))

  /** Unused for now... TODO try this out, get the interface going in state monad style!
    */
  def runUntilM[M[_]: Monad, S, A, B, C](
      state: StateT[M, S, A],
      agg: MonoidAggregator[A, B, C]
  )(p: S => Boolean): StateT[M, S, C] =
    StateT[M, S, C] { s =>
      iterateUntilM(s, agg)(state.run(_))(p)
    }

  /** Accumulates differences between the two for every A in the supplied sequence. The combine
    * function is used to aggregate the differences.
    *
    * I recommend using max or +.
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

  /** Cats helpers.
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
