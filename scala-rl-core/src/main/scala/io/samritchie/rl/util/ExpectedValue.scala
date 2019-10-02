package io.samritchie.rl
package util

import cats.Id
import cats.kernel.Semigroup
import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.Categorical

trait ExpectedValue[M[_]] {
  def get[A](ma: M[A], default: Value[Double])(f: A => Value[Double]): Value[Double]
}

object ExpectedValue {
  @inline final def apply[M[_]](implicit M: ExpectedValue[M]): ExpectedValue[M] = M

  implicit val id: ExpectedValue[Id] = new ExpectedValue[Id] {
    def get[A](a: A, default: Value[Double])(f: A => Value[Double]): Value[Double] = f(a)
  }

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
