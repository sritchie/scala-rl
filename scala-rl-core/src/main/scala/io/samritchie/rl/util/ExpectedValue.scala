package io.samritchie.rl
package util

import cats.Id
import cats.kernel.Semigroup

/**
  This definitely works, but I need to think through how we're going to be able
  to return things like Futures, that have to communicate over the network. Does
  the value return type cover it?

  NOTE the implementation is responsible for normalizing.
  */
trait ExpectedValue[M[_]] {
  def get[A](ma: M[A], default: Value[Double])(f: A => Value[Double]): Value[Double]
}

object ExpectedValue extends LowPriorityEVInstances {
  @inline final def apply[M[_]](implicit M: ExpectedValue[M]): ExpectedValue[M] = M

  implicit val id: ExpectedValue[Id] = new ExpectedValue[Id] {
    def get[A](a: A, default: Value[Double])(f: A => Value[Double]): Value[Double] = f(a)
  }
}

trait LowPriorityEVInstances {
  implicit def fromWeighted[M[_]](implicit W: Weighted[M, Double]): ExpectedValue[M] =
    new ExpectedValue[M] {
      def get[A](a: M[A], default: Value[Double])(f: A => Value[Double]): Value[Double] =
        Semigroup[Value[Double]]
          .combineAllOption(
            W.weights(a).map { case (a, weight) => f(a).weighted(weight) }
          )
          .getOrElse(default)
    }
}
