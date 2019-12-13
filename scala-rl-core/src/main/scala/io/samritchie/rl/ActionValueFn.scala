/**
  Action value function implementation. This is different enough that I'm going
  to keep it in its own file for now.
  */
package io.samritchie.rl

/**
  TODO:

  - make DecayState work with a RING, not with anything so generic! And
    specialize it. (investigate what this means.)
  - we want the aggregator that currently deals with Value instances to take a
    Double only in the case with gamma = 1.0, a Left(instance) in the case where
    gamma = 0.0, and some generic thing...
  */
trait ActionValueFn[Obs, A, T] { self =>
  // TODO should this be a set??
  def seenStates: Iterable[Obs]
  def seen(obs: Obs): Iterable[A]
  def actionValue(obs: Obs, a: A): T
  def learn(obs: Obs, action: A, value: T): ActionValueFn[Obs, A, T]
  def fold[U](prepare: U => T, present: T => U): ActionValueFn[Obs, A, U] =
    new ActionValueFn.Folded[Obs, A, T, U](self, prepare, present)
}

object ActionValueFn {
  class Folded[Obs, A, T, U](
      m: ActionValueFn[Obs, A, T],
      prepare: U => T,
      present: T => U
  ) extends ActionValueFn[Obs, A, U] {
    override def seenStates: Iterable[Obs] = m.seenStates
    override def seen(obs: Obs): Iterable[A] = m.seen(obs)
    override def actionValue(obs: Obs, a: A): U = present(m.actionValue(obs, a))
    override def learn(obs: Obs, action: A, value: U): ActionValueFn[Obs, A, U] =
      new Folded(m.learn(obs, action, prepare(value)), prepare, present)
  }
}
