package io.samritchie.rl

import cats.{Applicative, Monad, Monoid}
import cats.arrow.FunctionK
import cats.kernel.Semigroup
import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.{Categorical, Generator, ToGenerator}
import io.samritchie.rl.util.{ExpectedValue, ToDouble}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
  * A finite discrete distribution.
  *
  * @param pmfSeq A map with keys corresponding to the possible outcomes and
  * values corresponding to the probabilities of those outcomes.
  */
final case class Cat[+T](pmfSeq: List[(T, Double)]) {
  def pmf[U >: T]: Map[U, Double] = pmfSeq.toMap

  def map[U](fn: T => U): Cat[U] =
    Cat(
      pmfSeq
        .foldLeft(Map.empty[U, Double]) {
          case (acc, (t, p)) =>
            Util.mergeV(acc, fn(t), p)
        }
        .toList
    )

  def flatMap[U](fn: T => Cat[U]): Cat[U] =
    Cat(
      (for {
        (t, p) <- pmfSeq
        (u, p2) <- fn(t).pmfSeq
      } yield (u, p * p2))
        .foldLeft(Map.empty[U, Double]) {
          case (acc, (u, p)) =>
            Util.mergeV(acc, u, p)
        }
        .toList
    )

  def zip[U](other: Cat[U]): Cat[(T, U)] =
    Cat(
      for {
        (t, p) <- pmfSeq
        (u, p2) <- other.pmfSeq
      } yield ((t, u), p * p2)
    )

  def toRainier[U >: T]: Categorical[U] =
    Categorical.normalize(pmf[U].mapValues(Real(_)))
}

object Cat extends CatInstances {
  def apply[T](pmf: Map[T, Double]): Cat[T] = Cat(pmf.toList)

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

  def pure[A](a: A): Cat[A] = Cat(List((a, 1.0)))

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

  def softmax[A, B](m: Map[A, Double]): Cat[A] =
    normalize(m.mapValues(math.exp(_)))

  def softmax[A: ToDouble](as: Set[A]): Cat[A] = {
    val (pmf, sum) = as.foldLeft((Map.empty[A, Double], 0.0)) {
      case ((m, r), a) =>
        val aExp = math.exp(ToDouble[A].apply(a))
        (m.updated(a, aExp), r + aExp)
    }
    normalize(pmf.mapValues(_ / sum))
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
      def get[A](a: Cat[A], default: Value[Double])(f: A => Value[Double]): Value[Double] =
        Semigroup[Value[Double]]
          .combineAllOption(
            a.pmfSeq.map {
              case (a, weight) => f(a).weighted(weight)
            }
          )
          .getOrElse(default)
    }

  val catToCategorical: FunctionK[Cat, Categorical] =
    new FunctionK[Cat, Categorical] {
      def apply[A](ca: Cat[A]) = ca.toRainier
    }

  val catToGenerator: FunctionK[Cat, Generator] =
    new FunctionK[Cat, Generator] {
      def apply[A](ca: Cat[A]): Generator[A] = ca.toRainier.generator
    }
}

private[rl] object CatMonad extends Monad[Cat] {
  def pure[A](x: A): Cat[A] = Cat.pure(x)

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
            run(acc, queue.tail ++ f(a).pmfSeq.map { case (eab, d) => (eab, d * v) })
          case (Right(b), v) =>
            run(Util.mergeV(acc, b, v), queue.tail)
        }
      }
    val pmf = run(Map.empty, f(a).pmfSeq.to[Queue])
    Cat[B](pmf)
  }
}
