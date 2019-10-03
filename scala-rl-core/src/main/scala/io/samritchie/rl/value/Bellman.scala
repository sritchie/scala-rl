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
  */
case class Bellman[Obs, M[_]: ExpectedValue, S[_]: ExpectedValue](
    m: Map[Obs, Value[Double]],
    default: Value[Double]
) extends ValueFunction[Obs, M, S] {
  def seen: Iterable[Obs] = m.keys

  override def stateValue(obs: Obs): Value[Double] =
    m.getOrElse(obs, default)

  // TODO... should this move to the trait? Is anyone ever going to implement
  // this differently? And how about the other methods?
  override def evaluate[A, R: ToDouble](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, M, S]
  ): Value[Double] =
    ValueFunction.expectedActionValue(
      this,
      policy.choose(state),
      (a: A) => state.dynamics(a),
      default
    )

  /**
    This is currently an 'expected update', because it's using expectations vs any
    sampling.
    */
  override def update[A, R](
      state: State[A, Obs, R, S],
      value: Value[Double]
  ): ValueFunction[Obs, M, S] =
    copy(m = m.updated(state.observation, value))
}
