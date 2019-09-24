/**
  Value function implementations.
  */
package io.samritchie.rl

import cats.Eq
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

  /**
    So, reminding myself here... this takes in an actual state and calculates
    the new value for that particular state, given some weight assigned per
    action. But this assumes you get to see what to do next.
    */
  def newV[A, R: ToReal](
      state: NowState[A, Obs, R],
      weight: A => Real,
      combine: (Real, Real) => Real
  ): Real = {
    val dynamics = state.dynamics
    state.actions.foldLeft(Real.zero) { (acc, action) =>
      val (r, newState) = dynamics(action).value
      val newPos = newState.observation
      combine(acc, weight(action) * (ToReal(r) + gamma * m.getOrElse(newPos, Real.zero)))
    }
  }

  /**
    * THIS now is value iteration. This actually chooses the top value
    * for each thing.
    *
    *  Okay... so, yeah. We need to split out these ideas, again. We need to handle:
    *

    TODO:

    * - estimating the values for each action, and then:
    * - immediately updating the value of the state to be the value of the top one.
    *
    * This is equivalent to updating the policy immediately to maximize
    * value, and then updating the value function immediately.
    */
  def evaluateIter[A, R: ToReal](state: NowState[A, Obs, R]): MapStateV[Obs] = {
    val value = newV(state, (_: A) => Real.one, _.max(_))
    MapStateV(m.updated(state.observation, value), gamma)
  }

  /**
    This implements iterative policy evaluation. This is on page 75 in the RL
    textbook.
    */
  def evaluate[A, R: ToReal](
      policy: CategoricalPolicy[A, Obs, R, Id],
      state: NowState[A, Obs, R]
  ): MapStateV[Obs] = {
    val pmf = policy.categories(state).pmf
    val value = newV(state, pmf(_), _ + _)
    MapStateV(m.updated(state.observation, value), gamma)
  }
}

object ValueFunction {
  def emptyState[Obs](gamma: Double): MapStateV[Obs] =
    MapStateV(Map.empty[Obs, Real], gamma)

  /**
    This sweeps across a whole state space and updates the value function
    completely based on the current policy. There is no change in policy
    happening here.
    */
  @tailrec def evaluateSweep[A, Obs, R: ToReal](
      policy: CategoricalPolicy[A, Obs, R, Id],
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
    This does NOT take a policy, since there is an implicit policy going on -
    the policy that decides to maximize its actions based on what it's seeing
    here.

    I think to really get this working, I need to find something that alternates
    between both... policy evaluation and updates. Maybe we just take a function
    to do that.

    - TODO build a max policy.
    - TODO make a function that sweeps, but picks which to do... figure 4.1 might hold the secrets here.
    */
  @tailrec def evaluateSweepIter[A, Obs, R: ToReal](
      states: Traversable[NowState[A, Obs, R]],
      stateValue: MapStateV[Obs]
  ): MapStateV[Obs] =
    if (states.isEmpty)
      stateValue
    else {
      val newSV = stateValue.evaluateIter(states.head)
      evaluateSweepIter(states.tail, newSV)
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
