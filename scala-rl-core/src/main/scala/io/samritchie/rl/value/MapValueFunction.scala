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
import com.stripe.rainier.compute.{Real, ToReal}

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
    m: Map[Obs, Value[Real]],
    default: Value[Real]
) extends ValueFunction[Obs] {
  def seen: Iterable[Obs] = m.keys

  override def stateValue(obs: Obs): Value[Real] =
    m.getOrElse(obs, default)

  override def evaluate[A, R: ToReal](
      state: State[A, Obs, R, Id],
      policy: CategoricalPolicy[A, Obs, R, Id]
  ): Value[Real] = {
    val pmf = policy.choose(state).pmf
    val dynamics = state.dynamics

    Semigroup[Value[Real]]
      .combineAllOption(
        state.actions.toList.map { action =>
          val (r, newState) = dynamics(action)
          val obs = newState.observation
          stateValue(obs)
            .from(ToReal(r))
            .weighted(pmf.getOrElse(action, Real.zero))
        }
      )
      .getOrElse(default)
  }

  /**
    This is currently an 'expected update', because it's using expectations vs any
    sampling.
    */
  override def update[A, R: ToReal](
      state: State[A, Obs, R, Id],
      policy: CategoricalPolicy[A, Obs, R, Id]
  ): ValueFunction[Obs] =
    copy(m = m.updated(state.observation, evaluate(state, policy)))
}
