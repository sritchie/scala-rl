package io.samritchie.rl
package util

import cats.Id
import cats.kernel.Semigroup
import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.Categorical

/**
  This definitely works, but I need to think throug how we're going to be able
  to return things like Futures, that have to communicate over the network. Does
  the value return type cover it?

  NOTE the implementation is responsible for normalizing.
  */
trait ExpectedValue[M[_]] {
  def get[A](ma: M[A], default: Value[Double])(f: A => Value[Double]): Value[Double]
}

object ExpectedValue {
  @inline final def apply[M[_]](implicit M: ExpectedValue[M]): ExpectedValue[M] = M

  implicit val id: ExpectedValue[Id] = new ExpectedValue[Id] {
    def get[A](a: A, default: Value[Double])(f: A => Value[Double]): Value[Double] = f(a)
  }

  /**
    This is for the Rainier categorical. I have an implementation for Cat in its
    own object.
    */
  implicit def categorical(implicit n: Numeric[Real]): ExpectedValue[Categorical] =
    new ExpectedValue[Categorical] {
      def get[A](a: Categorical[A], default: Value[Double])(f: A => Value[Double]): Value[Double] =
        Semigroup[Value[Double]]
          .combineAllOption(
            a.pmf.iterator.map {
              case (a, weight) =>
                f(a).weighted(n.toDouble(weight))
            }
          )
          .getOrElse(default)
    }
}
