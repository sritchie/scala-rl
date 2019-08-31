/**
  * Extra stuff I'm discovering.
  */
package io.samritchie.rl

import cats.Monad
import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.Categorical
import com.twitter.algebird.{Aggregator, AveragedValue, MonoidAggregator}
import scala.language.higherKinds

object Util {
  object Instances {
    implicit val averageValueOrd: Ordering[AveragedValue] =
      Ordering.by(_.value)
  }

  def iterateM[F[_]: Monad, A](n: Int)(a: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM[Tuple2[Int, A], A]((n, a)) {
      case (k, a) =>
        if (k <= 0)
          Monad[F].pure(Right(a))
        else
          Monad[F].map(f(a))(a2 => Left((k - 1, a2)))
    }

  /**
    * epsilon-greedy distribution, boom.
    *
    * This should probably be Real.
    */
  def epsilonGreedy[A](epsilon: Double, optGreedy: Option[A], other: Set[A]): Categorical[A] =
    optGreedy match {
      case None => Categorical.list(other.toSeq)
      case Some(greedy) =>
        Categorical
          .normalize {
            Map(
              (other + greedy).toSeq -> epsilon,
              Seq(greedy) -> (1 - epsilon)
            )
          }
          .flatMap(Categorical.list(_))
    }

  def averagingAgg: MonoidAggregator[Double, AveragedValue, Double] =
    Aggregator
      .prepareMonoid[Double, AveragedValue](d => AveragedValue(d))
      .andThenPresent(_.value)
}
