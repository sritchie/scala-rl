package io.samritchie.rl
package algebra

import cats.Id

/**
  This definitely works, but I need to think through how we're going to be able
  to return things like Futures, that have to communicate over the network. Does
  the value return type cover it?

  NOTE the implementation is responsible for normalizing.
  */
trait Expectation[M[_]] {
  def get[A, B](a: M[A])(f: A => B)(implicit M: Module[Double, B]): B
}

object Expectation extends ExpectationImplicits {
  @inline final def apply[M[_]](implicit M: Expectation[M]): Expectation[M] = M

  implicit val id: Expectation[Id] = new Expectation[Id] {
    def get[A, B](a: A)(f: A => B)(implicit M: Module[Double, B]): B = f(a)
  }
}

trait ExpectationImplicits {
  implicit def fromDecomposition[M[_]](implicit D: Decompose[M, Double]): Expectation[M] =
    new Expectation[M] {
      def get[A, B](a: M[A])(f: A => B)(implicit M: Module[Double, B]): B =
        M.group.sum(
          D.decompose(a).map { case (a, coef) => M.scale(coef, f(a)) }
        )
    }
}
