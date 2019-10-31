/**
  Action value function implementation. This is different enough that I'm going
  to keep it in its own file for now.
  */
package io.samritchie.rl

import io.samritchie.rl.util.ExpectedValue

/**
  TODO for the morning:

  - convert the action value functions to directly return an R, not a
    Value[Double]. They can use a T to aggregate internally... then we don't
    need the weighted thing.
  - Make the Bellman and ActionValueMap implementations take an aggregator.
  - make DecayState work with a RING, not with anything so generic! And
    specialize it.
  - we want the aggregator that currently deals with Value instances to take a
    Double only in the case with gamma = 1.0, a Left(instance) in the case where
    gamma = 0.0, and some generic thing...

  Remember that the goal here is to lock down the types [R, T, R] for the monte
  carlo stuff so that I can actually get that shit working.

  Then convert the bandits to use it.

  */
trait ActionValueFn[Obs, A, R, T] { self =>
  def seen(obs: Obs): Iterable[A]
  def actionValue(obs: Obs, a: A): T
  def learn(obs: Obs, action: A, value: T): ActionValueFn[Obs, A, R, T]
  def toValueFunction[M[_]: ExpectedValue](
      policy: Policy[Obs, A, R, M, Any],
      default: T
  ): StateValueFn[Obs, T]
}
