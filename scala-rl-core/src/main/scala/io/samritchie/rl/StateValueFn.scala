package io.samritchie.rl

import com.twitter.algebird.Monoid
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
}
