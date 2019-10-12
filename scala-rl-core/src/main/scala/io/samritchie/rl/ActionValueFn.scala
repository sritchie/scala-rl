/**
  Action value function implementation. This is different enough that I'm going
  to keep it in its own file for now.
  */
package io.samritchie.rl

import io.samritchie.rl.util.{ExpectedValue, ToDouble}

trait ActionValueFn[Obs, A, T] { self =>
  def seen(obs: Obs): Iterable[A]
  def actionValue(obs: Obs, a: A): Value[Double]

  // So this receives some ALREADY AGGREGATED THING??
  def learn(obs: Obs, action: A, value: T): ActionValueFn[Obs, A, T]

  def toValueFunction[R: ToDouble, M[_]: ExpectedValue](
      policy: Policy[Obs, A, R, M, Any],
      default: Value[Double]
  ): StateValueFn[Obs]
}
