package io.samritchie.rl

import cats.{Eq, Eval}
import com.stripe.rainier.cats._
import com.stripe.rainier.compute.{Real, ToReal}

import scala.annotation.tailrec

/**
  * TODO Figure out how to make something that tracks action and state
  * values here.
  */
sealed trait ValueFunction[Obs] {}
case class StateValue[Obs]() extends ValueFunction[Obs]
case class ActionValue[Obs]() extends ValueFunction[Obs]

/**
  * I'll put this here for now. We need something that can track
  * action and state values when there are totally different
  * states... not just the single action values from the previous
  * setup.
  */
object ValueFunction {

  /**
    * Runs a single step of policy evaluation.
    *
    * This next bit generates the value if we go by the Bellman
    * equation... weight each move by its chance of happening.
    */
  def evaluateState[A, Obs, R: ToReal, P <: BaseCategoricalPolicy[A, Obs, R, Eval, P]](
      policy: P,
      state: NowState[A, Obs, R],
      stateValue: Map[Obs, Real],
      gamma: Double
  )(): Map[Obs, Real] = {
    val pmf = policy.categories(state).pmf
    val dynamics = state.dynamics
    val newV = state.actions.foldLeft(Real.zero) { (acc, m) =>
      val (r, newState) = dynamics(m).value
      val newPos = newState.observation
      acc + (pmf(m) * (ToReal(r) + gamma * stateValue.getOrElse(newPos, Real.zero)))
    }
    stateValue.updated(state.observation, newV)
  }

  @tailrec
  def evaluateSweep[A, Obs, R: ToReal, P <: BaseCategoricalPolicy[A, Obs, R, Eval, P]](
      policy: P,
      states: Traversable[NowState[A, Obs, R]],
      stateValue: Map[Obs, Real],
      gamma: Double
  ): Map[Obs, Real] =
    if (states.isEmpty)
      stateValue
    else {
      val newStateValue = evaluateState(policy, states.head, stateValue, gamma)
      evaluateSweep(policy, states.tail, newStateValue, gamma)
    }

  /**
    * Helpful utility functions for value calculations.
    */
  object Util {
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
    def shouldHalt[A](
        oldM: Map[A, Real],
        newM: Map[A, Real],
        epsilon: Double
    ): Boolean =
      Eq[Real].eqv(
        Real.lt(diffMaps(oldM, newM), Real(epsilon), Real.zero, Real.one),
        Real.zero
      )
  }
}
