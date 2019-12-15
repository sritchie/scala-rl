/**
  Action value function implementation. This is different enough that I'm going
  to keep it in its own file for now.
  */
package io.samritchie.rl

import com.twitter.algebird.{Monoid, Semigroup}

/**
  TODO:

  - make DecayState work with a RING, not with anything so generic! And
    specialize it. (investigate what this means.)
  - we want the aggregator that currently deals with Value instances to take a
    Double only in the case with gamma = 1.0, a Left(instance) in the case where
    gamma = 0.0, and some generic thing...
  - rewrite ActionValueMap in terms of a default and a base.
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

  /**
  Base version that does NOTHING, just stores.
    */
  class Base[Obs, A, T](
      m: Map[Obs, Map[A, T]],
      default: T
  ) extends ActionValueFn[Obs, A, T] { self =>
    def seenStates: Iterable[Obs] = m.keySet

    override def seen(obs: Obs): Iterable[A] = m.get(obs) match {
      case None    => Seq.empty
      case Some(m) => m.keySet
    }

    override def actionValue(obs: Obs, a: A): T =
      m.get(obs).flatMap(_.get(a)).getOrElse(default)

    override def learn(obs: Obs, action: A, value: T): Base[Obs, A, T] = {
      val newM =
        m.getOrElse(obs, Map.empty[A, T])
          .updated(action, value)
      new Base(m.updated(obs, newM), default)
    }
  }

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

  /**
    Version of the class that can merge into a base map. The current
    ActionValueMap bakes a monoid in now, which feels like an overreach.
    */
  class Merging[Obs, A, T](
      m: ActionValueFn[Obs, A, T]
  )(implicit T: Semigroup[T])
      extends ActionValueFn[Obs, A, T] {
    override def seenStates: Iterable[Obs] = m.seenStates
    override def seen(obs: Obs): Iterable[A] = m.seen(obs)
    override def actionValue(obs: Obs, a: A): T = m.actionValue(obs, a)
    override def learn(obs: Obs, action: A, value: T): ActionValueFn[Obs, A, T] = {
      val merged = T.plus(actionValue(obs, action), value)
      new Merging(m.learn(obs, action, merged))
    }
  }
}
