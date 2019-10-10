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

import io.samritchie.rl.util.{ExpectedValue, ToDouble}

/**
  This implements policy evaluation, NO update for the policy itself. And I
  guess... that means that we should actually take a policy to evaluate.

  This assumes you get to:

  - See what comes next.
  - Have some categorical policy.

  If you DO know those things, this will give you the value of the current
  state.

  TODO note that this only lets you look forward a single step. We're going to
  want to be able to look forward many steps in the model, and create an algebra
  that will let us talk well about this.

  I made a note in ValueFunction about how we should probably split out the
  value function storage abstraction from the actual evaluator.
  */
case class Bellman[Obs](
    m: Map[Obs, Value[Double]],
    default: Value[Double]
) extends ValueFunction[Obs] {
  def seen: Iterable[Obs] = m.keys

  override def stateValue(obs: Obs): Value[Double] =
    m.getOrElse(obs, default)

  // TODO This function currently uses the default, for states it hasn't seen,
  // as the value for final states AND for states that have no current actions.
  // This needs some work.
  override def evaluate[A, R: ToDouble, M[_]: ExpectedValue, S[_]: ExpectedValue](
      state: State[Obs, A, R, S],
      policy: Policy[Obs, A, R, M, S]
  ): Value[Double] =
    ValueFunction.expectedActionValue(
      this,
      policy.choose(state),
      (a: A) => state.dynamics(a),
      default,
      default
    )

  /**
    This is currently an 'expected update', because it's using expectations vs any
    sampling.
    */
  override def update(observation: Obs, value: Value[Double]): ValueFunction[Obs] =
    copy(m = m.updated(observation, value))
}
