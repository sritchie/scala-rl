package io.samritchie.rl
package util

import cats.Id

/**
  This definitely works, but I need to think through how we're going to be able
  to return things like Futures, that have to communicate over the network. Does
  the value return type cover it?

  NOTE the implementation is responsible for normalizing.
  */
trait ExpectedValue[M[_]] {
  def get[A, B](a: M[A])(f: A => B)(implicit M: Module[Double, B]): B
}

object ExpectedValue extends ExpectedValueImplicits {
  @inline final def apply[M[_]](implicit M: ExpectedValue[M]): ExpectedValue[M] = M

  implicit val id: ExpectedValue[Id] = new ExpectedValue[Id] {
    def get[A, B](a: A)(f: A => B)(implicit M: Module[Double, B]): B = f(a)
  }
}

trait ExpectedValueImplicits {
  implicit def fromWeighted[M[_]](implicit W: Weighted[M, Double]): ExpectedValue[M] =
    new ExpectedValue[M] {
      def get[A, B](a: M[A])(f: A => B)(implicit M: Module[Double, B]): B =
        M.group.sum(
          W.weights(a).map { case (a, weight) => M.scale(weight, f(a)) }
        )
    }
}
