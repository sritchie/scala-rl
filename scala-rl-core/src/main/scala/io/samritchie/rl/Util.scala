/**
  * Extra stuff I'm discovering.
  */
package io.samritchie.rl

import cats.Monad
import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.Categorical
import com.twitter.algebird.AveragedValue
import scala.language.higherKinds

object Util {
  object Instances {
    implicit val averageValueOrd: Ordering[AveragedValue] =
      Ordering.by(_.value)
  }

  def categoricalFromSet[T](ts: Set[T]): Categorical[T] =
    Categorical.normalize(
      ts.foldLeft(Map.empty[T, Real])((m, t) => m.updated(t, Real.one))
    )

  def allMaxBy[A, B: Ordering](as: Set[A])(f: A => B): Set[A] = {
    val maxB = f(as.maxBy(f))
    as.filter(a => Ordering[B].equiv(maxB, f(a)))
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
