package io.samritchie.rl

import com.twitter.algebird.{Aggregator, Monoid, MonoidAggregator, Semigroup}

/**
  Along with [[StateValueFn]], this is the main trait in tabular reinforcement
  learning for tracking the value of an (observation, action) pair.

  @tparam Obs Observation returned by the [[State]] instances tracked by
  [[ActionValueFn]].
  @tparam A Action type on the [[State]] instances tracked by [[ActionValueFn]].
  @tparam T type of values tracked by [[ActionValueFn]].
  */
trait ActionValueFn[Obs, A, T] { self =>

  /**
    Returns an Iterable of all observations associated with some internally
    tracked value T.
    */
  def seenStates: Iterable[Obs]

  /**
    Returns an iterable of all actions available from the supplied observation
    associated with any tracked value T.
    */
  def seen(obs: Obs): Iterable[A]

  /**
    Returns the stored value associated with the given obs, a pair.
    */
  def actionValue(obs: Obs, a: A): T

  /**
    Absorb a new value for the supplied obs, action pair. The behavior of this
    function is implementation dependent; some might ignore the value, some
    might merge it in to an existing set of values, some might completely
    replace the stored state.
    */
  def update(obs: Obs, action: A, value: T): ActionValueFn[Obs, A, T]

  /**
    Transforms this [[ActionValueFn]] into a new instance that applies the
    supplied `prepare` to all incoming values before they're learned, and
    presents tracked T instances using the `present` fn before returning them
    via [[actionValue]].

    @tparam the type of value stored by the returned [[ActionValueFn]].
    */
  def fold[U](prepare: U => T, present: T => U): ActionValueFn[Obs, A, U] =
    new ActionValueFn.Folded[Obs, A, T, U](self, prepare, present)

  /**
    Returns an [[ActionValueFn]] instance that uses the supplied semigroup T to
    merge values into this current [[ActionValueFn]].

    @param T Semigroup instance used to merge values.
    */
  def mergeable(implicit T: Semigroup[T]): ActionValueFn[Obs, A, T] =
    new ActionValueFn.Mergeable(self)
}

/**
  Constructors and classes associated with [[ActionValueFn]].
  */
object ActionValueFn {

  /**
    Returns an empty [[ActionValueFn]] backed by an immutable map.
    */
  def empty[Obs, A, T]: ActionValueFn[Obs, A, Option[T]] =
    empty[Obs, A, Option[T]](None)

  /**
    Returns an empty [[ActionValueFn]] backed by an immutable map. The supplied
    default value will be returned by [[ActionValueFn.actionValue]] for any
    (obs, action) pair that's not been seen by the [[ActionValueFn]].
    */
  def empty[Obs, A, T](default: T): ActionValueFn[Obs, A, T] =
    new Base(Map.empty[Obs, Map[A, T]], default)

  /**
    Returns an empty [[ActionValueFn]] backed by an immutable map that uses the
    zero of the supplied Monoid as a default value, and merges new learned
    values into the value in the underlying map using the Monoid's `plus`
    function.
    */
  def mergeable[Obs, A, T](implicit T: Monoid[T]): ActionValueFn[Obs, A, T] =
    mergeable(T.zero)

  /**
    Returns an empty [[ActionValueFn]] backed by an immutable map that uses the
    supplied `default` as a default value, and merges new learned values into
    the value in the underlying map using the Semigroup's `plus` function.
    */
  def mergeable[Obs, A, T](default: T)(implicit T: Semigroup[T]): ActionValueFn[Obs, A, T] =
    empty(default).mergeable

  /**
    Returns an [[ActionValueFn]] that:

    - uses the supplied default as an initial value
    - merges values in using the aggregator's semigroup
    - prepares and presents using the aggregator's analogous functions
    */
  def fromAggregator[Obs, A, T, U](
      default: T,
      agg: Aggregator[U, T, U]
  ): ActionValueFn[Obs, A, U] =
    empty(default)
      .mergeable(agg.semigroup)
      .fold(agg.prepare, agg.present)

  /**
    Returns an [[ActionValueFn]] that:

    - uses the MonoidAggregator's monoid.zero as an initial value
    - merges values in using the aggregator's monoid
    - prepares and presents using the aggregator's analogous functions
    */
  def fromAggregator[Obs, A, T, U](
      agg: MonoidAggregator[U, T, U]
  ): ActionValueFn[Obs, A, U] =
    mergeable(agg.monoid).fold(agg.prepare, agg.present)

  /**
  Basic implementation of an [[ActionValueFn]] that stores any value supplied to
  [[learn]] in an internal immutable map.

    @param m the immutable map used for storage.
    @param default value returned by [[Base]] when queried for (obs, a) pairs it
    hasn't yet seen.
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

    /**
      @inheritdoc
      This implementation replaces any existing value with no merge or logic.
      */
    override def update(obs: Obs, action: A, value: T): Base[Obs, A, T] = {
      val newM =
        m.getOrElse(obs, Map.empty[A, T])
          .updated(action, value)
      new Base(m.updated(obs, newM), default)
    }
  }

  /**
    [[ActionValueFn]] implementation that implements a fold.

    Any value supplied to [[learn]] will be transformed first by prepare before
    being passed to the base [[ActionValueFn]]. Any value retrieved by
    [[actionValue]] will be passed to `present` before being returned.
    */
  class Folded[Obs, A, T, U](
      base: ActionValueFn[Obs, A, T],
      prepare: U => T,
      present: T => U
  ) extends ActionValueFn[Obs, A, U] {
    override def seenStates: Iterable[Obs] = base.seenStates
    override def seen(obs: Obs): Iterable[A] = base.seen(obs)
    override def actionValue(obs: Obs, a: A): U = present(base.actionValue(obs, a))
    override def update(obs: Obs, action: A, value: U): ActionValueFn[Obs, A, U] =
      new Folded(base.update(obs, action, prepare(value)), prepare, present)
  }

  /**
    [[ActionValueFn]] implementation that merges values passed to [[learn]] into
    the value stored by the base [[ActionValueFn]] using the supplied
    Semigroup's `plus` function.
    */
  class Mergeable[Obs, A, T](
      base: ActionValueFn[Obs, A, T]
  )(implicit T: Semigroup[T])
      extends ActionValueFn[Obs, A, T] {
    override def seenStates: Iterable[Obs] = base.seenStates
    override def seen(obs: Obs): Iterable[A] = base.seen(obs)
    override def actionValue(obs: Obs, a: A): T = base.actionValue(obs, a)

    /**
      @inheritdoc

      This implementation replaces uses a Semigroup[T] to merge the supplied
      value in to whatever value is stored in the underlying m.
      */
    override def update(obs: Obs, action: A, value: T): ActionValueFn[Obs, A, T] = {
      val merged = T.plus(actionValue(obs, action), value)
      new Mergeable(base.update(obs, action, merged))
    }
  }
}
