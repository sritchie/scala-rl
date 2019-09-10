package io.samritchie.rl

import cats.{Eq, Eval}
import com.stripe.rainier.cats._
import com.stripe.rainier.compute.{Real, ToReal}

import scala.annotation.tailrec

/**
  * Trait for state or action value functions.
  */
sealed trait ValueFunction[Obs] {
  def value()
}
case class StateValue[Obs](m: Map[Obs, Real]) extends ValueFunction[Obs] {
  def shouldHalt(previous: StateValue[Obs], epsilon: Double): Boolean =
    ValueFunction.Util.shouldHalt(previous.m, m, epsilon)
}

case class ActionValue[Obs]() extends ValueFunction[Obs]

/**
  * I'll put this here for now. We need something that can track
  * action and state values when there are totally different
  * states... not just the single action values from the previous
  * setup.
  */
object ValueFunction {
  def emptyState[Obs]: StateValue[Obs] = StateValue(Map.empty)

  /**
    * Runs a single step of policy evaluation.
    *
    * This next bit generates the value if we go by the Bellman
    * equation... weight each move by its chance of happening.
    */
  def evaluateState[A, Obs, R: ToReal, P <: BaseCategoricalPolicy[A, Obs, R, Eval, P]](
      policy: P,
      state: NowState[A, Obs, R],
      stateValue: StateValue[Obs],
      gamma: Double
  ): StateValue[Obs] = {
    val pmf = policy.categories(state).pmf
    val dynamics = state.dynamics
    val newV = state.actions.foldLeft(Real.zero) { (acc, m) =>
      val (r, newState) = dynamics(m).value
      val newPos = newState.observation

      // This evaluates the state using the Bellman equation. This
      // needs to get absorbed into the value calculation.
      acc + (pmf(m) * (ToReal(r) + gamma * stateValue.m.getOrElse(newPos, Real.zero)))
    }
    StateValue(stateValue.m.updated(state.observation, newV))
  }

  @tailrec
  def evaluateSweep[A, Obs, R: ToReal, P <: BaseCategoricalPolicy[A, Obs, R, Eval, P]](
      policy: P,
      states: Traversable[NowState[A, Obs, R]],
      stateValue: StateValue[Obs],
      gamma: Double
  ): StateValue[Obs] =
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
