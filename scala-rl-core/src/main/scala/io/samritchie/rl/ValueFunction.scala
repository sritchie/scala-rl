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
import cats.kernel.Semigroup
import io.samritchie.rl.util.ToDouble

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
    value.MapValueFunction(Map.empty[Obs, Value[Double]], default)

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
    time.

    What we really want is the ability to ping between updates to the value
    function or learning steps; to insert them every so often.

    TODO This is some crazy shit right now. We're updating the value function
    internally... but we never actually pass the new one back out? I guess we
    can fix that if we just... give access to the function over and over.
    */
  def sweep[A, Obs, R: ToDouble, M[_], S[_]](
      valueFn: ValueFunction[Obs, M, S],
      policy: Policy[A, Obs, R, M, S],
      policyFn: ValueFunction[Obs, M, S] => Policy[A, Obs, R, M, S],
      states: Traversable[State[A, Obs, R, S]],
      inPlace: Boolean
  ): ValueFunction[Obs, M, S] =
    states
      .foldLeft((valueFn, policy)) {
        case ((vf, p), state) =>
          val baseVf = if (inPlace) vf else valueFn
          val newFn = vf.update(state, baseVf.evaluate(state, p))
          (newFn, policyFn(newFn))
      }
      ._1

  def sweepUntil[A, Obs, R: ToDouble, M[_], S[_]](
      valueFn: ValueFunction[Obs, M, S],
      policyFn: ValueFunction[Obs, M, S] => Policy[A, Obs, R, M, S],
      states: Traversable[State[A, Obs, R, S]],
      stopFn: (ValueFunction[Obs, M, S], ValueFunction[Obs, M, S], Long) => Boolean,
      inPlace: Boolean
  ): (ValueFunction[Obs, M, S], Long) = {
    val policy = policyFn(valueFn)
    Util.loopWhile((valueFn, 0)) {
      case (fn, nIterations) =>
        val updated = ValueFunction.sweep(fn, policy, policyFn, states, inPlace)
        Either.cond(
          stopFn(fn, updated, nIterations),
          (updated, nIterations),
          (updated, nIterations + 1)
        )
    }
  }

  // Is there some way to make an ExpectedValue typeclass or something?
  def isPolicyStable[A, Obs, R: ToDouble, M[_]](
      l: ValueFunction[Obs, M, Id],
      r: ValueFunction[Obs, M, Id],
      states: Traversable[State[A, Obs, R, Id]]
  ): Boolean =
    states.forall(s => greedyOptions(l, s) == greedyOptions(r, s))

  def isPolicyStableStochastic[A, Obs, R: ToDouble, M[_]](
      l: ValueFunction[Obs, M, Cat],
      r: ValueFunction[Obs, M, Cat],
      default: Value[Double],
      states: Traversable[State[A, Obs, R, Cat]]
  ): Boolean =
    states.forall(s => greedyOptionsStochastic(l, s, default) == greedyOptionsStochastic(r, s, default))

  def greedyOptions[A, Obs, R, M[_]](
      valueFn: ValueFunction[Obs, M, Id],
      state: State[A, Obs, R, Id]
  )(implicit toDouble: ToDouble[R]): Set[A] = Util.allMaxBy[A, Value[Double]](state.actions) { a =>
    val (reward, newState) = state.dynamics(a)
    valueFn.stateValue(newState.observation).from(toDouble(reward))
  }

  // Something is going on here... this is a way of evaluating the best options
  // available from the state. But the evaluation of options DEFINITELY has to
  // be shared with the Bellman implementation, or the MapValueFunction. How can
  // we collapse it all down and share some code?
  def greedyOptionsStochastic[A, Obs, R: ToDouble, M[_]](
      valueFn: ValueFunction[Obs, M, Cat],
      state: State[A, Obs, R, Cat],
      default: Value[Double]
  ): Set[A] = Util.allMaxBy[A, Value[Double]](state.actions) { a =>
    actionValue(valueFn, state, a, default)
  }

  /**
    This returns the value of the action, given categorical dynamics of the
    state.
    */
  def actionValue[A, Obs, R, M[_]](
      valueFn: ValueFunction[Obs, M, Cat],
      state: State[A, Obs, R, Cat],
      action: A,
      default: Value[Double]
  )(implicit toDouble: ToDouble[R]): Value[Double] =
    Semigroup[Value[Double]]
      .combineAllOption(
        state.dynamics(action).pmfSeq.map {
          case ((reward, newState), weight) =>
            valueFn
              .stateValue(newState.observation)
              .from(toDouble(reward))
              .weighted(weight)
        }
      )
      .getOrElse(default)

  /**
    Helper to tell if we can stop iterating. The combine function is used to
    aggregate the differences between the value functions for each
    observation... the final aggregated value must be less than epsilon to
    return true, false otherwise.
    */
  def diff[Obs, A[_], B[_], C[_], D[_]](
      l: ValueFunction[Obs, A, B],
      r: ValueFunction[Obs, C, D],
      epsilon: Double
  )(
      combine: (Double, Double) => Double
  ): Boolean =
    Ordering[Double].lt(
      Util.diff[Obs]((l.seen ++ r.seen), l.stateValue(_).get, r.stateValue(_).get, combine),
      epsilon
    )
}
