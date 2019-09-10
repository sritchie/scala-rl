package io.samritchie.rl

import cats.Eq
import com.stripe.rainier.cats._
import com.stripe.rainier.compute.Real

/**
  * I'll put this here for now. We need something that can track
  * action and state values when there are totally different
  * states... not just the single action values from the previous
  * setup.
  */
object ValueFunction {
  def diffMaps[A](l: Map[A, Real], r: Map[A, Real]): Real =
    (l.keys ++ r.keys).foldLeft(Real.zero) { (acc, k) =>
      (l.get(k), r.get(k)) match {
        case (None, None)       => acc
        case (Some(l), None)    => acc + l
        case (None, Some(r))    => acc + r
        case (Some(l), Some(r)) => acc + (l - r).abs
      }
    }

  /**
    * Proper stopping conditions for a state value function.
    */
  def shouldWeStop[A](
      oldM: Map[A, Real],
      newM: Map[A, Real],
      epsilon: Double
  ): Boolean =
    Eq[Real].eqv(
      Real.lt(diffMaps(oldM, newM), Real(epsilon), Real.zero, Real.one),
      Real.zero
    )
}

/**
  * TODO Figure out how to make something that tracks action and state
  * values here.
  */
trait ValueFunction {}
