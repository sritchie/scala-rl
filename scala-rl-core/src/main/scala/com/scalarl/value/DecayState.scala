package com.scalarl
package value

import com.twitter.algebird.{Group, Ring, VectorSpace}
import com.scalarl.algebra.{Expectation, Module, ToDouble}
import com.scalarl.evaluate.StateValue

/** This represents a value that's weighted as you move away from it. This is useful because we can
  * KEEP GOING, and continue to weight it.
  */
sealed trait DecayState[A] extends Product with Serializable {
  def toValue: DecayState.DecayedValue[A]
  def get: A
}

object DecayState {
  import Module.DModule

  case class Reward[A](get: A) extends DecayState[A] {
    override lazy val toValue: DecayedValue[A] = DecayedValue(get)
  }
  case class DecayedValue[A](get: A) extends DecayState[A] {
    override val toValue: DecayedValue[A] = this
  }

  /** Filling in.
    */
  def bellmanFn[Obs, A, R: DModule, T, M[_]: Expectation, S[_]: Expectation](
      gamma: Double
  ): (
      StateValueFn[Obs, DecayState[R]],
      Policy[Obs, A, R, M, S]
  ) => StateValue[Obs, A, R, DecayState[R], S] = {
    val group = decayStateGroup[R](gamma)
    implicit val module = decayStateModule[R](gamma)
    (f, p) =>
      Evaluator.bellman[Obs, A, R, DecayState[R], M, S](
        f,
        p,
        Reward(_),
        group.plus(_, _)
      )
  }

  def decayStateModule[A](
      gamma: Double
  )(implicit M: Module[Double, A]): Module[Double, DecayState[A]] = {
    implicit val group: Group[DecayState[A]] = decayStateGroup(gamma)
    Module.from((r, d) =>
      d match {
        case Reward(reward)  => Reward(M.scale(r, reward))
        case DecayedValue(v) => DecayedValue(M.scale(r, v))
      }
    )
  }

  // This is just sort of silly and probably can go.
  def decayStateVectorSpace[A: Ring](
      gamma: Double
  )(implicit M: Module[Double, A]): VectorSpace[A, DecayState] = {
    implicit val group: Group[DecayState[A]] = decayStateGroup(gamma)
    VectorSpace.from((r, d) =>
      d match {
        case Reward(reward)  => Reward(Ring.times(r, reward))
        case DecayedValue(v) => DecayedValue(Ring.times(r, v))
      }
    )
  }

  def decayStateGroup[A](
      gamma: Double
  )(implicit M: Module[Double, A]): Group[DecayState[A]] =
    new Group[DecayState[A]] {
      private val GA = M.group
      override val zero = DecayedValue(GA.zero)
      override def negate(d: DecayState[A]) = d match {
        case Reward(a)       => Reward(GA.negate(a))
        case DecayedValue(a) => DecayedValue(GA.negate(a))
      }
      override def plus(l: DecayState[A], r: DecayState[A]) = (l, r) match {
        case (Reward(a), Reward(b)) => Reward(GA.plus(a, b))
        case (DecayedValue(a), Reward(b)) =>
          DecayedValue(GA.plus(M.scale(gamma, a), b))
        case (Reward(a), DecayedValue(b)) =>
          DecayedValue(GA.plus(M.scale(gamma, b), a))
        case (DecayedValue(a), DecayedValue(b)) => DecayedValue(GA.plus(a, b))
      }
    }

  implicit def toDouble[A](implicit A: ToDouble[A]): ToDouble[DecayState[A]] =
    A.contramap(_.get)

  implicit def dsOrd[A: Ordering]: Ordering[DecayState[A]] =
    Ordering.by(_.get)
}
