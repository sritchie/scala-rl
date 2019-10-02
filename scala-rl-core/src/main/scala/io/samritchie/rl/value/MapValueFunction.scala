/**
  Value function implementations.

  TODO:

  * - estimating the values for each action, and then:
  * - immediately updating the value of the state to be the value of the top one.
  *
  * This is equivalent to updating the policy immediately to maximize
  * value, and then updating the value function immediately.
  */
package io.samritchie.rl
package value

import cats.Id
import cats.kernel.Semigroup

/**
  This implements policy evaluation, NO update for the policy itself. And I
  guess... that means that we should actually take a policy to evaluate.

  This assumes you get to:

  - See what comes next.
  - Have some categorical policy.

  If you DO know those things, this will give you the value of the current
  state.
  */
case class MapValueFunction[Obs](
    m: Map[Obs, Value[Double]],
    default: Value[Double]
) extends ValueFunction[Obs, Cat, Id] {
  def seen: Iterable[Obs] = m.keys

  override def stateValue(obs: Obs): Value[Double] =
    m.getOrElse(obs, default)

  override def evaluate[A, R: Numeric](
      state: State[A, Obs, R, Id],
      policy: Policy[A, Obs, R, Cat, Id]
  ): Value[Double] = {
    val pmf = policy.choose(state).pmf
    val dynamics = state.dynamics

    Semigroup[Value[Double]]
      .combineAllOption(
        state.actions.toList.map { action =>
          val (r, newState) = dynamics(action)
          stateValue(newState.observation)
            .from(implicitly[Numeric[R]].toDouble(r))
            .weighted(pmf.getOrElse(action, 0.0))
        }
      )
      .getOrElse(default)
  }

  /**
    This is currently an 'expected update', because it's using expectations vs any
    sampling.
    */
  override def update[A, R: Numeric](
      state: State[A, Obs, R, Id],
      value: Value[Double]
  ): ValueFunction[Obs, Cat, Id] =
    copy(m = m.updated(state.observation, value))
}
