/**
  Action value function implementation. This is different enough that I'm going
  to keep it in its own file for now.
  */
package io.samritchie.rl

/**
  TODO:

  - Make the ActionValueMap implementation take an aggregator.
  - make DecayState work with a RING, not with anything so generic! And
    specialize it.
  - we want the aggregator that currently deals with Value instances to take a
    Double only in the case with gamma = 1.0, a Left(instance) in the case where
    gamma = 0.0, and some generic thing...

  Remember that the goal here is to lock down the types [R, T, T] for the monte
  carlo stuff so that I can actually get that shit working.
  */
trait ActionValueFn[Obs, A, T] { self =>
  // TODO should this be a set??
  def seenStates: Iterable[Obs]
  def seen(obs: Obs): Iterable[A]
  def actionValue(obs: Obs, a: A): T
  def learn(obs: Obs, action: A, value: T): ActionValueFn[Obs, A, T]
}
