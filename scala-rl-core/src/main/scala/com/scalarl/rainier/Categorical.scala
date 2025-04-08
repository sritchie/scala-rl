package com.scalarl
package rainier

import cats.{Applicative, Monad, Monoid}
import cats.arrow.FunctionK
import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.{Categorical => RCat, Generator, ToGenerator}
import com.scalarl.algebra.ToDouble

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** A finite discrete distribution.
  *
  * @param pmfSeq
  *   A map with keys corresponding to the possible outcomes and values corresponding to the probabilities of
  *   those outcomes.
  */
final case class Categorical[+T](pmfSeq: List[(T, Double)]) {
  def pmf[U >: T]: Map[U, Double] = pmfSeq.toMap

  def map[U](fn: T => U): Categorical[U] =
    Categorical(
      pmfSeq
        .foldLeft(Map.empty[U, Double]) { case (acc, (t, p)) =>
          Util.mergeV(acc, fn(t), p)
        }
        .toList
    )

  def flatMap[U](fn: T => Categorical[U]): Categorical[U] =
    Categorical(
      (for {
        (t, p) <- pmfSeq
        (u, p2) <- fn(t).pmfSeq
      } yield (u, p * p2))
        .foldLeft(Map.empty[U, Double]) { case (acc, (u, p)) =>
          Util.mergeV(acc, u, p)
        }
        .toList
    )

  def zip[U](other: Categorical[U]): Categorical[(T, U)] =
    Categorical(
      for {
        (t, p) <- pmfSeq
        (u, p2) <- other.pmfSeq
      } yield ((t, u), p * p2)
    )

  def toRainier[U >: T]: RCat[U] =
    RCat.normalize(pmf[U].mapValues(Real(_)))
}

object Categorical extends CategoricalInstances {
  def apply[T](pmf: Map[T, Double]): Categorical[T] = Categorical(pmf.toList)

  object Poisson {
    case class Lambda(value: Double) extends AnyVal

    def gamma(z: Double): Double =
      if (z == 0.0)
        Double.PositiveInfinity
      else if (z == 1.0 || z == 2.0)
        0.0
      else
        approxGamma(z)

    private def approxGamma(z: Double): Double = {
      val v = z + 1.0
      val w = v + (1.0 / ((12.0 * v) - (1.0 / (10.0 * v))))
      (math.log(Math.PI * 2) / 2.0) - (math.log(v) / 2.0) + (v * (math.log(w) - 1.0)) - math.log(z)
    }

    def logProbability(k: Int, lambda: Double): Double =
      -lambda + (math.log(lambda) * k) - gamma(k + 1.0)

    def probability(k: Int, lambda: Double): Double =
      math.exp(logProbability(k, lambda))
  }

  def poisson(upperBound: Int, mean: Poisson.Lambda): Categorical[Int] =
    normalize(Util.makeMapUnsafe(0 until upperBound)(Poisson.probability(_, mean.value)))

  def boolean(p: Double): Categorical[Boolean] =
    Categorical(Map(true -> p, false -> (1.0 - p)))

  def pure[A](a: A): Categorical[A] = Categorical(List((a, 1.0)))

  def normalize[T](pmf: Map[T, Double]): Categorical[T] = {
    val total = pmf.values.toList.sum
    Categorical(pmf.map { case (t, p) => (t, p / total) })
  }

  def seq[T](ts: Seq[T]): Categorical[T] =
    normalize(ts.groupBy(identity).mapValues(_.size))

  def fromSet[T](ts: Set[T]): Categorical[T] = {
    val p = 1.0 / ts.size
    Categorical(
      ts.foldLeft(Map.empty[T, Double])((m, t) => m.updated(t, p))
    )
  }

  def softmax[A, B](m: Map[A, Double]): Categorical[A] =
    normalize(m.mapValues(math.exp(_)))

  def softmax[A: ToDouble](as: Set[A]): Categorical[A] = {
    val (pmf, sum) = as.foldLeft((Map.empty[A, Double], 0.0)) { case ((m, r), a) =>
      val aExp = math.exp(ToDouble[A].apply(a))
      (m.updated(a, aExp), r + aExp)
    }
    normalize(pmf.mapValues(_ / sum))
  }
}

trait CategoricalInstances {
  implicit val catMonad: Monad[Categorical] = CategoricalMonad
  implicit def catMonoid[A: Monoid]: Monoid[Categorical[A]] = Applicative.monoid[Categorical, A]
  implicit def gen[T]: ToGenerator[Categorical[T], T] =
    new ToGenerator[Categorical[T], T] {
      def apply(c: Categorical[T]) = c.toRainier.generator
    }

  val setToCategorical: FunctionK[Set, Categorical] =
    new FunctionK[Set, Categorical] {
      def apply[A](sa: Set[A]) = Categorical.fromSet(sa)
    }

  val toRainierCategorical: FunctionK[Categorical, RCat] =
    new FunctionK[Categorical, RCat] {
      def apply[A](ca: Categorical[A]) = ca.toRainier
    }

  val catToGenerator: FunctionK[Categorical, Generator] =
    new FunctionK[Categorical, Generator] {
      def apply[A](ca: Categorical[A]): Generator[A] = ca.toRainier.generator
    }
}

private[scalarl] object CategoricalMonad extends Monad[Categorical] {
  def pure[A](x: A): Categorical[A] = Categorical.pure(x)

  override def map[A, B](fa: Categorical[A])(f: A => B): Categorical[B] =
    fa.map(f)

  override def product[A, B](
      fa: Categorical[A],
      fb: Categorical[B]
  ): Categorical[(A, B)] = fa.zip(fb)

  override def flatMap[A, B](fa: Categorical[A])(f: A => Categorical[B]): Categorical[B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(f: A => Categorical[Either[A, B]]): Categorical[B] = {
    @tailrec
    def run(acc: Map[B, Double], queue: Queue[(Either[A, B], Double)]): Map[B, Double] =
      queue.headOption match {
        case None => acc
        case Some((Left(a), v)) =>
          run(acc, queue.drop(1) ++ f(a).pmfSeq.map { case (eab, d) => (eab, d * v) })
        case Some((Right(b), v)) =>
          run(Util.mergeV(acc, b, v), queue.drop(1))
      }
    val pmf = run(Map.empty, f(a).pmfSeq.to[Queue])
    Categorical[B](pmf)
  }
}
