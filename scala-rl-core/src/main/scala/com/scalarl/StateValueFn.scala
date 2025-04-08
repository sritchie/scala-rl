package com.scalarl

import com.twitter.algebird.{Aggregator, Monoid, MonoidAggregator, Semigroup}
import com.scalarl.evaluate.StateValue

/** Along with [[ActionValueFn]], this is the main trait in tabular reinforcement learning for tracking the
  * value of a state as evidenced by the observation it returns.
  *
  * We need some way for this to learn, or see new observations, that's part of the trait.
  *
  * @tparam Obs
  *   Observation returned by the [[State]] instances tracked by [[StateValueFn]].
  * @tparam T
  *   type of values tracked by [[StateValueFn]].
  */
trait StateValueFn[Obs, T] { self =>

  /** Returns an Iterable of all observations associated with some internally tracked value T.
    */
  def seen: Iterable[Obs]

  /** Returns the stored value associated with the given observation.
    */
  def stateValue(obs: Obs): T

  /** Absorb a new value for the supplied observation. The behavior of this function is implementation
    * dependent; some might ignore the value, some might merge it in to an existing set of values, some might
    * completely replace the stored state.
    */
  def update(state: Obs, value: T): StateValueFn[Obs, T]

  /** Transforms this [[StateValueFn]] into a new instance that applies the supplied `prepare` to all incoming
    * values before they're learned, and presents tracked T instances using the `present` fn before returning
    * them via [[stateValue]].
    *
    * @tparam the
    *   type of value stored by the returned [[StateValueFn]].
    */
  def fold[U](prepare: U => T, present: T => U): StateValueFn[Obs, U] =
    new StateValueFn.Folded[Obs, T, U](self, prepare, present)

  /** Returns a [[StateValueFn]] instance that uses the supplied semigroup T to merge values into this current
    * [[StateValueFn]].
    *
    * @param T
    *   Semigroup instance used to merge values.
    */
  def mergeable(implicit T: Semigroup[T]): StateValueFn[Obs, T] =
    new StateValueFn.Mergeable(self)

  /** TODO fill in.
    */
  def toEvaluator[A, R, S[_]]: StateValue[Obs, A, R, T, S] =
    StateValue.fn(self)
}

/** Constructors and classes associated with [[StateValueFn]].
  */
object StateValueFn {

  /** Returns an empty [[StateValueFn]] backed by an immutable map.
    */
  def empty[Obs, T]: StateValueFn[Obs, Option[T]] =
    empty[Obs, Option[T]](None)

  /** Returns an empty [[StateValueFn]] backed by an immutable map. The supplied default value will be
    * returned by [[StateValueFn.stateValue]] for any obs that's not been seen by the [[StateValueFn]].
    */
  def empty[Obs, T](default: T): StateValueFn[Obs, T] =
    Base(Map.empty, default)

  /** Returns an empty [[StateValueFn]] backed by an immutable map that uses the zero of the supplied Monoid
    * as a default value, and merges new learned values into the value in the underlying map using the
    * Monoid's `plus` function.
    */
  def mergeable[Obs, T](implicit T: Monoid[T]): StateValueFn[Obs, T] =
    mergeable(T.zero)

  /** Returns an empty [[StateValueFn]] backed by an immutable map that uses the supplied `default` as a
    * default value, and merges new learned values into the value in the underlying map using the Semigroup's
    * `plus` function.
    */
  def mergeable[Obs, T](default: T)(implicit T: Semigroup[T]): StateValueFn[Obs, T] =
    empty(default).mergeable

  /** Returns a [[StateValueFn]] that:
    *
    * \- uses the supplied default as an initial value \- merges values in using the aggregator's semigroup \-
    * prepares and presents using the aggregator's analogous functions
    */
  def fromAggregator[Obs, T, U](
      default: T,
      agg: Aggregator[U, T, U]
  ): StateValueFn[Obs, U] =
    empty(default)
      .mergeable(agg.semigroup)
      .fold(agg.prepare, agg.present)

  /** Returns a [[StateValueFn]] that:
    *
    * \- uses the MonoidAggregator's monoid.zero as an initial value \- merges values in using the
    * aggregator's monoid \- prepares and presents using the aggregator's analogous functions
    */
  def fromAggregator[Obs, T, U](
      agg: MonoidAggregator[U, T, U]
  ): StateValueFn[Obs, U] =
    mergeable(agg.monoid).fold(agg.prepare, agg.present)

  /** Basic implementation of a [[StateValueFn]] that stores any value supplied to [[update]] in an internal
    * immutable map.
    *
    * @param m
    *   the immutable map used for storage.
    * @param default
    *   value returned by [[Base]] when queried for some observation it hasn't yet seen.
    */
  case class Base[Obs, T](m: Map[Obs, T], default: T) extends StateValueFn[Obs, T] { self =>
    override def seen: Iterable[Obs] = m.keySet
    override def stateValue(obs: Obs): T = m.getOrElse(obs, default)

    /** @inheritdoc
      * This implementation replaces any existing value with no merge or logic.
      */
    override def update(obs: Obs, value: T): Base[Obs, T] =
      Base(m.updated(obs, value), default)
  }

  /** [[StateValueFn]] implementation that implements a fold.
    *
    * Any value supplied to [[update]] will be transformed first by prepare before being passed to the base
    * [[StateValueFn]]. Any value retrieved by [[stateValue]] will be passed to `present` before being
    * returned.
    */
  case class Folded[Obs, T, U](
      base: StateValueFn[Obs, T],
      prepare: U => T,
      present: T => U
  ) extends StateValueFn[Obs, U] {
    override def seen: Iterable[Obs] = base.seen
    override def stateValue(obs: Obs): U = present(base.stateValue(obs))
    override def update(obs: Obs, value: U): StateValueFn[Obs, U] =
      Folded(base.update(obs, prepare(value)), prepare, present)
  }

  /** [[StateValueFn]] implementation that merges values passed to [[update]] into the value stored by the
    * base [[StateValueFn]] using the supplied Semigroup's `plus` function.
    */
  case class Mergeable[Obs, T](
      base: StateValueFn[Obs, T]
  )(implicit T: Semigroup[T])
      extends StateValueFn[Obs, T] {
    override def seen: Iterable[Obs] = base.seen
    override def stateValue(obs: Obs): T = base.stateValue(obs)

    /** @inheritdoc
      *
      * This implementation replaces uses a Semigroup[T] to merge the supplied value in to whatever value is
      * stored in the underlying m.
      */
    override def update(obs: Obs, value: T): StateValueFn[Obs, T] = {
      val merged = T.plus(stateValue(obs), value)
      Mergeable(base.update(obs, merged))
    }
  }
}
