package io.samritchie.rl

import com.twitter.algebird.{Monoid, Semigroup}
import io.samritchie.rl.evaluate.StateValue
import io.samritchie.rl.value.DecayState

/**
  Along with [[ActionValueFn]], this is the main trait in tabular reinforcement
  learning for tracking the value of a state as evidenced by the observation it
  returns.

  We need some way for this to learn, or see new observations, that's part of
  the trait.

  @tparam Obs Observation returned by the [[State]] instances tracked by
  [[StateValueFn]].
  @tparam T type of values tracked by [[StateValueFn]].
  */
trait StateValueFn[Obs, T] { self =>

  /**
    Returns an Iterable of all observations associated with some internally
    tracked value T.
    */
  def seen: Iterable[Obs]

  /**
    Returns the stored value associated with the given observation.
    */
  def stateValue(obs: Obs): T

  /**
    Absorb a new value for the supplied observation. The behavior of this
    function is implementation dependent; some might ignore the value, some
    might merge it in to an existing set of values, some might completely
    replace the stored state.
    */
  def update(state: Obs, value: T): StateValueFn[Obs, T]

  /**
    Transforms this [[StateValueFn]] into a new instance that applies the
    supplied `prepare` to all incoming values before they're learned, and
    presents tracked T instances using the `present` fn before returning them
    via [[stateValue]].

    @tparam the type of value stored by the returned [[StateValueFn]].
    */
  def fold[U](prepare: U => T, present: T => U): StateValueFn[Obs, U] =
    new StateValueFn.Folded[Obs, T, U](self, prepare, present)

  /**
    Returns a [[StateValueFn]] instance that uses the supplied semigroup T to
    merge values into this current [[StateValueFn]].

    @param T Semigroup instance used to merge values.
    */
  def mergeable(implicit T: Semigroup[T]): StateValueFn[Obs, T] =
    new StateValueFn.Mergeable(self)

  /**
    TODO fill in.
    */
  def toEvaluator[A, R, S[_]]: StateValue[Obs, A, R, T, S] =
    StateValue.fn(self)
}

/**
  Constructors and classes associated with [[StateValueFn]].
  */
object StateValueFn {

  def apply[Obs, T](default: T): StateValueFn[Obs, T] =
    value.StateValueMap(Map.empty[Obs, T], default)

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs, T](default: T): StateValueFn[Obs, DecayState[T]] =
    apply(DecayState.DecayedValue(default))

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs, T: Monoid]: StateValueFn[Obs, DecayState[T]] =
    decaying(Monoid.zero)

  /**
  Basic implementation of an [[StateValueFn]] that stores any value supplied to
  [[learn]] in an internal immutable map.

    @param m the immutable map used for storage.
    @param default value returned by [[Base]] when queried for some observation
    it hasn't yet seen.
    */
  class Base[Obs, T](m: Map[Obs, T], default: T) extends StateValueFn[Obs, T] { self =>
    override def seen: Iterable[Obs] = m.keySet
    override def stateValue(obs: Obs): T = m.getOrElse(obs, default)

    /**
      @inheritdoc
      This implementation replaces any existing value with no merge or logic.
      */
    override def update(obs: Obs, value: T): Base[Obs, T] =
      new Base(m.updated(obs, value), default)
  }

  /**
    [[StateValueFn]] implementation that implements a fold.

    Any value supplied to [[learn]] will be transformed first by prepare before
    being passed to the base [[StateValueFn]]. Any value retrieved by
    [[stateValue]] will be passed to `present` before being returned.
    */
  class Folded[Obs, T, U](
      base: StateValueFn[Obs, T],
      prepare: U => T,
      present: T => U
  ) extends StateValueFn[Obs, U] {
    override def seen: Iterable[Obs] = base.seen
    override def stateValue(obs: Obs): U = present(base.stateValue(obs))
    override def update(obs: Obs, value: U): StateValueFn[Obs, U] =
      new Folded(base.update(obs, prepare(value)), prepare, present)
  }

  /**
    [[StateValueFn]] implementation that merges values passed to [[learn]] into
    the value stored by the base [[StateValueFn]] using the supplied
    Semigroup's `plus` function.
    */
  class Mergeable[Obs, T](
      base: StateValueFn[Obs, T]
  )(implicit T: Semigroup[T])
      extends StateValueFn[Obs, T] {
    override def seen: Iterable[Obs] = base.seen
    override def stateValue(obs: Obs): T = base.stateValue(obs)

    /**
      @inheritdoc

      This implementation replaces uses a Semigroup[T] to merge the supplied
      value in to whatever value is stored in the underlying m.
      */
    override def update(obs: Obs, value: T): StateValueFn[Obs, T] = {
      val merged = T.plus(stateValue(obs), value)
      new Mergeable(base.update(obs, merged))
    }
  }
}
