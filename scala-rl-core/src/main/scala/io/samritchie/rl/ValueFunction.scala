package io.samritchie.rl

import cats.{Eq, Eval}
import com.stripe.rainier.cats._
import com.stripe.rainier.compute.{Real, ToReal}

import scala.annotation.tailrec

/**
  * Trait for state or action value functions.
  */
trait ValueFunction[Obs] {
  def m: Map[Obs, Real]
  def shouldHalt(previous: StateValue[Obs], epsilon: Double): Boolean =
    ValueFunction.Util.shouldHalt(previous.m, m, epsilon)
}

trait StateValue[Obs] extends ValueFunction[Obs]
trait ActionValue[A, Obs] extends ValueFunction[Obs]

/**
  * This implements policy evaluation, NO update for the policy
  * itself.
  *
  * BUT we know that each policy needs to have a state or value
  * function! That's certainly how I thought it had to work. I guess a
  * policy should really learn from a value function... maybe not from
  * a state directly. Keep playing, look at the bellman equation
  * version NEXT to the policy updating version.
  */
case class MapStateV[Obs](
    m: Map[Obs, Real],
    gamma: Double
) extends StateValue[Obs] {
  def evaluate[A, R: ToReal, P <: BaseCategoricalPolicy[A, Obs, R, Eval, P]](
      policy: P,
      state: BaseState[A, Obs, R, Eval]
  ): MapStateV[Obs] = {
    val pmf = policy.categories(state).pmf
    val dynamics = state.dynamics
    val newV = state.actions.foldLeft(Real.zero) { (acc, action) =>
      val (r, newState) = dynamics(action).value
      val newPos = newState.observation

      // This evaluates the state using the Bellman equation. This
      // needs to get absorbed into the value calculation.
      acc + (pmf(action) * (ToReal(r) + gamma * m.getOrElse(newPos, Real.zero)))
    }
    MapStateV(m.updated(state.observation, newV), gamma)
  }
}

/**
  * I'll put this here for now. We need something that can track
  * action and state values when there are totally different
  * states... not just the single action values from the previous
  * setup.
  */
object ValueFunction {
  def emptyState[Obs](gamma: Double): MapStateV[Obs] =
    MapStateV(Map.empty[Obs, Real], gamma)

  @tailrec
  def evaluateSweep[A, Obs, R: ToReal, P <: BaseCategoricalPolicy[A, Obs, R, Eval, P]](
      policy: P,
      states: Traversable[NowState[A, Obs, R]],
      stateValue: MapStateV[Obs]
  ): MapStateV[Obs] =
    if (states.isEmpty)
      stateValue
    else {
      val newSV = stateValue.evaluate(policy, states.head)
      evaluateSweep(policy, states.tail, newSV)
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
