/**
  * Extra stuff I'm discovering.
  */
package io.samritchie.rl

import com.stripe.rainier.compute.Real
import com.stripe.rainier.core.Categorical

object Util {

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

}
