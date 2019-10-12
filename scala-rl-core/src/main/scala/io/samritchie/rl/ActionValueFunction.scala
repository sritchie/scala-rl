/**
  Action value function implementation. This is different enough that I'm going
  to keep it in its own file for now.
  */
package io.samritchie.rl

import io.samritchie.rl.util.ExpectedValue

trait ActionValueFunction[Obs, A, R] { self =>
  def seen(obs: Obs): Iterable[A]
  def actionValue(obs: Obs, a: A): Value[Double]

  // This obviously does not fit with the others, since it makes no assumptions
  // about Double rewards, etc, like we've seen above in ValueFunction. Let's
  // see how it settles out.
  def learn(obs: Obs, action: A, value: R): ActionValueFunction[Obs, A, R]

  def toValueFunction[M[_]: ExpectedValue](
      policy: Policy[Obs, A, R, M, Any],
      default: Value[Double]
  ): ValueFunction[Obs]
}
