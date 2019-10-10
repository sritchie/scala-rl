/**
  Value function implementations.

  I think to really get this working, I need to find something that alternates
  between both... policy evaluation and updates. Maybe we just take a function
  to do that.

  - TODO build a max policy.
  - TODO make a function that sweeps, but picks which to do... figure 4.1 might hold the secrets here.
  */
package io.samritchie.rl

import io.samritchie.rl.util.{ExpectedValue, ToDouble}

/**
  * Trait for state or action value functions.

  We need some way for this to learn, or see new observations, that's part of
  the trait.
  */
trait ValueFunction[Obs] { self =>
  def seen: Iterable[Obs]
  def stateValue(obs: Obs): Value[Double]
  def update(state: Obs, value: Value[Double]): ValueFunction[Obs]

  /**
    Evaluate the state using the supplied policy.

    TODO should an evaluator be its own thing? We could take the evaluation
    strategy from Bellman, for example, and use it with all sorts of different
    strategies for evaluating far forward. Why not look two steps into the
    future?
    */
  def evaluate[A, R: ToDouble, M[_]: ExpectedValue, S[_]: ExpectedValue](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, M, S]
  ): Value[Double]

  def evaluateAndUpdate[A, R: ToDouble, M[_]: ExpectedValue, S[_]: ExpectedValue](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, M, S]
  ): ValueFunction[Obs] =
    update(state.observation, evaluate(state, policy))
}

trait ActionValueFunction[Obs, A, R] extends ValueFunction[Obs] { self =>
  def seen(obs: Obs): Set[A]
  def actionValue(obs: Obs, a: A): Value[Double]

  // This obviously does not fit with the others, since it makes no assumptions
  // about Double rewards, etc, like we've seen above in ValueFunction. Let's
  // see how it settles out.
  def learn(obs: Obs, action: A, value: R): ActionValueFunction[Obs, A, R]
}

object ValueFunction {
  def apply[Obs](default: Value[Double]): ValueFunction[Obs] =
    value.Bellman(Map.empty[Obs, Value[Double]], default)

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs](gamma: Double): ValueFunction[Obs] =
    decaying(0.0, gamma)

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs](default: Double, gamma: Double): ValueFunction[Obs] =
    ValueFunction[Obs](value.Decaying(default, gamma))

  def isPolicyStable[A, Obs, R: ToDouble, M[_], S[_]: ExpectedValue](
      l: ValueFunction[Obs],
      r: ValueFunction[Obs],
      default: Value[Double],
      states: Traversable[State[A, Obs, R, S]]
  ): Boolean =
    states.forall(s => greedyOptions(l, s, default) == greedyOptions(r, s, default))

  /**
    NOTE: The default action value would NOT be necessary of we were looking at
    an action value function. Working.
    */
  def greedyOptions[A, Obs, R: ToDouble, M[_], S[_]: ExpectedValue](
      valueFn: ValueFunction[Obs],
      state: State[A, Obs, R, S],
      defaultActionValue: Value[Double]
  ): Set[A] =
    Util.allMaxBy[A, Value[Double]](state.actions) { a =>
      actionValue(valueFn, state.dynamics(a), defaultActionValue)
    }

  /**
    This returns the value of the action, given categorical dynamics of the
    state.
    */
  def actionValue[A, Obs, R, M[_], S[_]](
      valueFn: ValueFunction[Obs],
      state: S[(R, State[A, Obs, R, S])],
      finalStateValue: Value[Double]
  )(implicit toDouble: ToDouble[R], EVS: ExpectedValue[S]): Value[Double] =
    EVS.get(state, finalStateValue) {
      case (reward, newState) =>
        // THIS is where you can descend deeper.
        valueFn.stateValue(newState.observation).from(toDouble(reward))
    }

  // TODO what would it mean here to go TWO levels deep?
  def expectedActionValue[A, Obs, R, M[_], S[_]](
      valueFn: ValueFunction[Obs],
      action: M[A],
      next: A => S[(R, State[A, Obs, R, S])],
      finalStateValue: Value[Double],
      noActionValue: Value[Double]
  )(implicit toDouble: ToDouble[R], EVM: ExpectedValue[M], EVS: ExpectedValue[S]): Value[Double] =
    EVM.get(action, noActionValue) { a =>
      actionValue(valueFn, next(a), finalStateValue)
    }

  /**
    Helper to tell if we can stop iterating. The combine function is used to
    aggregate the differences between the value functions for each
    observation... the final aggregated value must be less than epsilon to
    return true, false otherwise.
    */
  def diffBelow[Obs](
      l: ValueFunction[Obs],
      r: ValueFunction[Obs],
      epsilon: Double
  )(
      combine: (Double, Double) => Double
  ): Boolean = Ordering[Double].lt(diffValue(l, r, combine), epsilon)

  /**
    TODO consider putting this on the actual trait.
    */
  def diffValue[Obs](
      l: ValueFunction[Obs],
      r: ValueFunction[Obs],
      combine: (Double, Double) => Double
  ): Double =
    Util.diff[Obs]((l.seen ++ r.seen), l.stateValue(_).get, r.stateValue(_).get, combine),
}
