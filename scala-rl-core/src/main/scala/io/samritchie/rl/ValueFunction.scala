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
      state: NowState[A, Obs, R]
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
  *
// THIS now is value iteration. This actually chooses the top value
for each thing.

    Okay... so, yeah. We need to split out these ideas, again. We need to handle:

    - estimating the values for each action, and then:
    - immediately updating the value of the state to be the value of the top one.

    This is equivalent to updating the policy immediately to maximize
    value, and then updating the value function immediately. But there
    are really two ideas going on here. This can be a project for
    tomorrow.

    TODO: Get this cleaned up and coded.

        new_value = np.zeros_like(value)
        for i in range(WORLD_SIZE):
            for j in range(WORLD_SIZE):
                values = []
                for action in ACTIONS:
                    (next_i, next_j), reward = step([i, j], action)
                    # value iteration
                    values.append(reward + DISCOUNT * value[next_i, next_j])
                new_value[i, j] = np.max(values)
        value = new_value
  */
/**
  * So this sort of has to update the POLICY too, so that the next
  * time around we evaluate it better.
  *
  * Maybe make a method on policy that takes in a value function and
  * updates?
  */
case class ValueIteration[Obs](
    m: Map[Obs, Real],
    gamma: Double
) extends StateValue[Obs] {
  def evaluate[A, R: ToReal, P <: BaseCategoricalPolicy[A, Obs, R, Eval, P]](
      policy: P,
      state: NowState[A, Obs, R]
  ): ValueIteration[Obs] = {
    val pmf = policy.categories(state).pmf
    val dynamics = state.dynamics
    val newV = state.actions.foldLeft(Real.zero) { (acc, action) =>
      val (r, newState) = dynamics(action).value
      val newPos = newState.observation
      acc + (pmf(action) * (ToReal(r) + gamma * m.getOrElse(newPos, Real.zero)))
    }
    ValueIteration(m.updated(state.observation, newV), gamma)
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
