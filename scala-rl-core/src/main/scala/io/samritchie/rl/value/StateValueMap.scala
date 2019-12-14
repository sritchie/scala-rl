package io.samritchie.rl
package value

import com.twitter.algebird.{Monoid, Semigroup}

case class StateValueMap[Obs, T](
    m: Map[Obs, T],
    default: T
) extends StateValueFn[Obs, T] {
  def seen: Iterable[Obs] = m.keys

  override def stateValue(obs: Obs): T = m.getOrElse(obs, default)

  /**
    This is just a straight-up update, no merge at all.
    */
  override def update(observation: Obs, value: T): StateValueFn[Obs, T] =
    copy(m = m.updated(observation, value))
}

object StateValueMap {
  /**
    Returns an empty state value fn.
    */
  def empty[Obs, T](default: T): StateValueMap[Obs, T] =
    StateValueMap(Map.empty, default)

  /**
    Returns an empty statevaluefn. that will merge in values using the supplied
    monoid.

    TODO move these to the main function!
    */
  def merging[Obs, T](implicit T: Monoid[T]): StateValueFn[Obs, T] =
    new Merging[Obs, T](empty(T.zero))

  class Folded[Obs, T, U](
      m: StateValueFn[Obs, T],
      prepare: U => T,
      present: T => U
  ) extends StateValueFn[Obs, U] {
    override def seen: Iterable[Obs] = m.seen
    override def stateValue(obs: Obs): U = present(m.stateValue(obs))
    override def update(obs: Obs, value: U): StateValueFn[Obs, U] =
      new Folded(m.update(obs, prepare(value)), prepare, present)
  }

  /**
    Class that uses a Monoid instanc
    */
  class Merging[Obs, T](
      m: StateValueFn[Obs, T]
  )(implicit T: Semigroup[T])
      extends StateValueFn[Obs, T] {
    override def seen: Iterable[Obs] = m.seen
    override def stateValue(obs: Obs): T = m.stateValue(obs)
    override def update(obs: Obs, value: T): StateValueFn[Obs, T] = {
      val merged = T.plus(stateValue(obs), value)
      new Merging(m.update(obs, merged))
    }
  }

}
