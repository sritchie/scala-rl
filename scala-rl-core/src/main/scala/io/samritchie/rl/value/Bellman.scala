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
case class Bellman[Obs](
    m: Map[Obs, Value[Double]],
    default: Value[Double]
) extends ValueFunction[Obs, Cat, Cat] {
  def seen: Iterable[Obs] = m.keys

  override def stateValue(obs: Obs): Value[Double] =
    m.getOrElse(obs, default)

  override def evaluate[A, R: Numeric](
      state: State[A, Obs, R, Cat],
      policy: Policy[A, Obs, R, Cat, Cat]
  ): Value[Double] = {
    val pmf = policy.choose(state).pmf
    Semigroup[Value[Double]]
      .combineAllOption(
        state.actions.toList.map { action =>
          val policyWeight = pmf.getOrElse(action, 0.0)
          ValueFunction
            .actionValue(this, state, action, default)
            .weighted(policyWeight)
        }
      )
      .getOrElse(default)
  }

  /**
    This is currently an 'expected update', because it's using expectations vs any
    sampling.
    */
  override def update[A, R: Numeric](
      state: State[A, Obs, R, Cat],
      value: Value[Double]
  ): ValueFunction[Obs, Cat, Cat] =
    copy(m = m.updated(state.observation, value))
}
