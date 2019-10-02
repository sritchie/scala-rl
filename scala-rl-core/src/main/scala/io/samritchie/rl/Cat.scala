package io.samritchie.rl

import cats.{Applicative, Monad, Monoid}
import cats.kernel.Semigroup
import cats.instances.double._
import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.{Categorical, ToGenerator}
import io.samritchie.rl.util.ExpectedValue

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
  * A finite discrete distribution.
  *
  * @param pmf A map with keys corresponding to the possible outcomes and values
  * corresponding to the probabilities of those outcomes
  */
final case class Cat[T](pmf: Map[T, Double]) {
  def map[U](fn: T => U): Cat[U] =
    Cat(
      pmf.foldLeft(Map.empty[U, Double]) {
        case (acc, (t, p)) =>
          Util.mergeV(acc, fn(t), p)
      }
    )

  def flatMap[U](fn: T => Cat[U]): Cat[U] =
    Cat(
      (for {
        (t, p) <- pmf.iterator
        (u, p2) <- fn(t).pmf.iterator
      } yield (u, p * p2)).foldLeft(Map.empty[U, Double]) {
        case (acc, (u, p)) =>
          Util.mergeV(acc, u, p)
      }
    )

  def zip[U](other: Cat[U]): Cat[(T, U)] =
    Cat(
      for {
        (t, p) <- pmf
        (u, p2) <- other.pmf
      } yield ((t, u), p * p2)
    )

  def toRainier: Categorical[T] =
    Categorical.normalize(pmf.mapValues(Real(_)))
}

object Cat extends CatInstances {
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

  def poisson(upperBound: Int, mean: Poisson.Lambda): Cat[Int] =
    normalize(Util.makeMapUnsafe((0 until upperBound))(Poisson.probability(_, mean.value)))

  def boolean(p: Double): Cat[Boolean] =
    Cat(Map(true -> p, false -> (1.0 - p)))

  def normalize[T](pmf: Map[T, Double]): Cat[T] = {
    val total = (pmf.values.toList).sum
    Cat(pmf.map { case (t, p) => (t, p / total) })
  }

  def seq[T](ts: Seq[T]): Cat[T] =
    normalize(ts.groupBy(identity).mapValues(_.size))

  def fromSet[T](ts: Set[T]): Cat[T] = {
    val p = 1.0 / ts.size
    Cat(
      ts.foldLeft(Map.empty[T, Double])((m, t) => m.updated(t, p))
    )
  }
}

trait CatInstances {
  implicit val catMonad: Monad[Cat] = CatMonad
  implicit def catMonoid[A: Monoid]: Monoid[Cat[A]] = Applicative.monoid[Cat, A]
  implicit def gen[T]: ToGenerator[Cat[T], T] =
    new ToGenerator[Cat[T], T] {
      def apply(c: Cat[T]) = c.toRainier.generator
    }
  implicit val expectedValue: ExpectedValue[Cat] =
    new ExpectedValue[Cat] {
      def get[A](a: Cat[A], default: Double)(f: A => Double): Double =
        Semigroup[Double]
          .combineAllOption(
            a.pmf.iterator.map {
              case (a, weight) => f(a) * weight
            }
          )
          .getOrElse(default)
    }
}

private[rl] object CatMonad extends Monad[Cat] {
  def pure[A](x: A): Cat[A] = Cat(Map(x -> 1.0))

  override def map[A, B](fa: Cat[A])(f: A => B): Cat[B] =
    fa.map(f)

  override def product[A, B](
      fa: Cat[A],
      fb: Cat[B]
  ): Cat[(A, B)] = fa.zip(fb)

  override def flatMap[A, B](fa: Cat[A])(f: A => Cat[B]): Cat[B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(f: A => Cat[Either[A, B]]): Cat[B] = {
    @tailrec
    def run(acc: Map[B, Double], queue: Queue[(Either[A, B], Double)]): Map[B, Double] =
      if (queue.isEmpty) acc
      else {
        queue.head match {
          case (Left(a), v) =>
            run(acc, queue.tail ++ f(a).pmf.mapValues(_ * v))
          case (Right(b), v) =>
            run(Util.mergeV(acc, b, v), queue.tail)
        }
      }
    val pmf = run(Map.empty, f(a).pmf.to[Queue])
    Cat[B](pmf)
  }
}
