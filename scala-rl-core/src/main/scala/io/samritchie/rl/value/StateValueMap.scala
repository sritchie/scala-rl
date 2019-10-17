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

  I made a note in StateValueFn about how we should probably split out the
  value function storage abstraction from the actual evaluator.
  */
case class StateValueMap[Obs](
    m: Map[Obs, Value[Double]],
    default: Value[Double]
) extends StateValueFn[Obs] {
  def seen: Iterable[Obs] = m.keys

  override def stateValue(obs: Obs): Value[Double] =
    m.getOrElse(obs, default)

  override def update(observation: Obs, value: Value[Double]): StateValueFn[Obs] =
    copy(m = m.updated(observation, value))

  override def evaluate[A, R: ToDouble, M[_]: ExpectedValue, S[_]: ExpectedValue](
      state: State[Obs, A, R, S],
      policy: Policy[Obs, A, R, M, S]
  ): Value[Double] = ???

  override def evaluateAndUpdate[A, R: ToDouble, M[_]: ExpectedValue, S[_]: ExpectedValue](
      state: State[Obs, A, R, S],
      policy: Policy[Obs, A, R, M, S]
  ): StateValueFn[Obs] = ???
}
