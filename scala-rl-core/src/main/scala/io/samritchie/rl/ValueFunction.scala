/**
  Value function implementations.

  I think to really get this working, I need to find something that alternates
  between both... policy evaluation and updates. Maybe we just take a function
  to do that.

  - TODO build a max policy.
  - TODO make a function that sweeps, but picks which to do... figure 4.1 might hold the secrets here.
  */
package io.samritchie.rl

import cats.Id
import cats.arrow.FunctionK
import io.samritchie.rl.util.{ExpectedValue, ToDouble}

/**
  * Trait for state or action value functions.

  We need some way for this to learn, or see new observations, that's part of
  the trait.
  */
trait ValueFunction[Obs, M[_], S[_]] { self =>
  def seen: Iterable[Obs]
  def stateValue(obs: Obs): Value[Double]

  /**
    Evaluate the state using the supplied policy.
    */
  def evaluate[A, R: ToDouble](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, M, S]
  ): Value[Double]

  def update[A, R](
      state: State[A, Obs, R, S],
      value: Value[Double]
  ): ValueFunction[Obs, M, S]

  def evaluateAndUpdate[A, R: ToDouble](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, M, S]
  ): ValueFunction[Obs, M, S] = update(state, evaluate(state, policy))

  def contramapK[N[_]](f: FunctionK[N, M]): ValueFunction[Obs, N, S] =
    new value.Contramapped[Obs, M, N, S](self, f)
}

trait ActionValueFunction[A, Obs, M[_], S[_]] extends ValueFunction[Obs, M, S] { self =>
  def seen(obs: Obs): Set[A]
  def actionValue(obs: Obs, a: A): Value[Double]

  def learn[R](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, M, S],
      action: A,
      reward: R
  ): ActionValueFunction[A, Obs, M, S]

  override def contramapK[N[_]](f: FunctionK[N, M]): ActionValueFunction[A, Obs, N, S] =
    new value.ContramappedAV[A, Obs, M, N, S](self, f)
}

object ValueFunction {
  def apply[Obs](default: Value[Double]): ValueFunction[Obs, Cat, Id] =
    value.Bellman(Map.empty[Obs, Value[Double]], default)

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs](gamma: Double): ValueFunction[Obs, Cat, Id] =
    decaying(0.0, gamma)

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs](default: Double, gamma: Double): ValueFunction[Obs, Cat, Id] =
    ValueFunction[Obs](value.Decaying(default, gamma))

  /**
    This sweeps across the whole state space and updates the policy every single
    time IF you set valueIteration to true. Otherwise it creates a policy once
    and then uses it each time.

    What we really want is the ability to ping between updates to the value
    function or learning steps; to insert them every so often.

    This function does NOT currently return the final policy, since you can just
    make it yourself, given the return value and the function.
    */
  def sweep[A, Obs, R: ToDouble, M[_], S[_]](
      valueFn: ValueFunction[Obs, M, S],
      policyFn: ValueFunction[Obs, M, S] => Policy[A, Obs, R, M, S],
      states: Traversable[State[A, Obs, R, S]],
      inPlace: Boolean,
      valueIteration: Boolean
  ): ValueFunction[Obs, M, S] =
    states
      .foldLeft((valueFn, policyFn(valueFn))) {
        case ((vf, p), state) =>
          val baseVf = if (inPlace) vf else valueFn
          val newFn = vf.update(state, baseVf.evaluate(state, p))
          val newPolicy = if (valueIteration) policyFn(newFn) else p
          (newFn, newPolicy)
      }
      ._1

  def sweepUntil[A, Obs, R: ToDouble, M[_], S[_]](
      valueFn: ValueFunction[Obs, M, S],
      policyFn: ValueFunction[Obs, M, S] => Policy[A, Obs, R, M, S],
      states: Traversable[State[A, Obs, R, S]],
      stopFn: (ValueFunction[Obs, M, S], ValueFunction[Obs, M, S], Long) => Boolean,
      inPlace: Boolean,
      valueIteration: Boolean
  ): (ValueFunction[Obs, M, S], Long) =
    Util.loopWhile((valueFn, 0)) {
      case (fn, nIterations) =>
        val updated =
          ValueFunction.sweep(fn, policyFn, states, inPlace, valueIteration)
        Either.cond(
          stopFn(fn, updated, nIterations),
          (updated, nIterations),
          (updated, nIterations + 1)
        )
    }

  // Is there some way to make an ExpectedValue typeclass or something?
  def isPolicyStable[A, Obs, R: ToDouble, M[_], S[_]: ExpectedValue](
      l: ValueFunction[Obs, M, S],
      r: ValueFunction[Obs, M, S],
      default: Value[Double],
      states: Traversable[State[A, Obs, R, S]]
  ): Boolean =
    states.forall(s => greedyOptions(l, s, default) == greedyOptions(r, s, default))

  /**
    NOTE: The default action value would NOT be necessary of we were looking at
    an action value function. Working.
    */
  def greedyOptions[A, Obs, R: ToDouble, M[_], S[_]: ExpectedValue](
      valueFn: ValueFunction[Obs, M, S],
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
      valueFn: ValueFunction[Obs, M, S],
      state: S[(R, State[A, Obs, R, S])],
      default: Value[Double]
  )(implicit toDouble: ToDouble[R], EV: ExpectedValue[S]): Value[Double] =
    EV.get(state, default) {
      case (reward, newState) =>
        valueFn.stateValue(newState.observation).from(toDouble(reward))
    }

  def expectedActionValue[A, Obs, R, M[_], S[_]](
      valueFn: ValueFunction[Obs, M, S],
      action: M[A],
      next: A => S[(R, State[A, Obs, R, S])],
      // TODO what exactly does this mean?
      default: Value[Double]
  )(implicit toDouble: ToDouble[R], EVM: ExpectedValue[M], EVS: ExpectedValue[S]): Value[Double] =
    EVM.get(action, default) { a =>
      actionValue(valueFn, next(a), default)
    }

  /**
    Helper to tell if we can stop iterating. The combine function is used to
    aggregate the differences between the value functions for each
    observation... the final aggregated value must be less than epsilon to
    return true, false otherwise.
    */
  def diffBelow[Obs, A[_], B[_], C[_], D[_]](
      l: ValueFunction[Obs, A, B],
      r: ValueFunction[Obs, C, D],
      epsilon: Double
  )(
      combine: (Double, Double) => Double
  ): Boolean = Ordering[Double].lt(diffValue(l, r, combine), epsilon)

  def diffValue[Obs, A[_], B[_], C[_], D[_]](
      l: ValueFunction[Obs, A, B],
      r: ValueFunction[Obs, C, D],
      combine: (Double, Double) => Double
  ): Double =
    Util.diff[Obs]((l.seen ++ r.seen), l.stateValue(_).get, r.stateValue(_).get, combine),
}
