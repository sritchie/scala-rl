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
import com.stripe.rainier.compute.{Real, ToReal}
import com.stripe.rainier.core.Categorical
import Util.Instances.realOrd

/**
  * Trait for state or action value functions.

  We need some way for this to learn, or see new observations, that's part of
  the trait.
  */
trait ValueFunction[Obs, M[_], S[_]] { self =>
  def seen: Iterable[Obs]
  def stateValue(obs: Obs): Value[Real]

  /**
    Evaluate the state using the supplied policy.
    */
  def evaluate[A, R: ToReal](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, M, S]
  ): Value[Real]

  def update[A, R: ToReal](
      state: State[A, Obs, R, S],
      value: Value[Real]
  ): ValueFunction[Obs, M, S]

  def evaluateAndUpdate[A, R: ToReal](
      state: State[A, Obs, R, S],
      policy: Policy[A, Obs, R, M, S]
  ): ValueFunction[Obs, M, S] = update(state, evaluate(state, policy))

  def contramapK[N[_]](f: FunctionK[N, M]): ValueFunction[Obs, N, S] =
    new value.Contramapped[Obs, M, N, S](self, f)
}

trait ActionValueFunction[A, Obs, M[_], S[_]] extends ValueFunction[Obs, M, S] { self =>
  def seen(obs: Obs): Set[A]
  def actionValue(obs: Obs, a: A): Value[Real]

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
  def apply[Obs](default: Value[Real]): ValueFunction[Obs, Categorical, Id] =
    value.MapValueFunction(Map.empty[Obs, Value[Real]], default)

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs](gamma: Double): ValueFunction[Obs, Categorical, Id] =
    decaying(Real.zero, gamma)

  /**
    Returns a new value function that absorbs rewards with decay.
    */
  def decaying[Obs](default: Real, gamma: Double): ValueFunction[Obs, Categorical, Id] =
    ValueFunction[Obs](value.Decaying(default, gamma))

  /**
    This sweeps across the whole state space and updates the policy every single
    time.

    What we really want is the ability to ping between updates to the value
    function or learning steps; to insert them every so often.

    TODO add gate to random if necessary...
    TODO remove the iter stuff
    TODO test if it all works

    */
  def sweep[A, Obs, R: ToReal, M[_], S[_]](
      policy: Policy[A, Obs, R, M, S],
      valueFn: ValueFunction[Obs, M, S],
      states: Traversable[State[A, Obs, R, S]],
      inPlace: Boolean
  ): ValueFunction[Obs, M, S] =
    states
      .foldLeft((valueFn, policy)) {
        case ((vf, p), state) =>
          val baseVf = if (inPlace) vf else valueFn
          val newFn = vf.update(state, baseVf.evaluate(state, p))
          val newP = p.learnAll(newFn)
          (newFn, newP)
      }
      ._1

  def sweepUntil[A, Obs, R: ToReal, M[_], S[_]](
      policy: Policy[A, Obs, R, M, S],
      valueFn: ValueFunction[Obs, M, S],
      states: Traversable[State[A, Obs, R, S]],
      stopFn: (ValueFunction[Obs, M, S], ValueFunction[Obs, M, S], Long) => Boolean,
      inPlace: Boolean
  ): (ValueFunction[Obs, M, S], Long) =
    Util.loopWhile((valueFn, 0)) {
      case (fn, nIterations) =>
        val updated = ValueFunction.sweep(policy, fn, states, inPlace)
        Either.cond(
          stopFn(fn, updated, nIterations),
          (updated, nIterations),
          (updated, nIterations + 1)
        )
    }

  // Is there some way to make an ExpectedValue typeclass or something?
  def isPolicyStable[A, Obs, R: ToReal, M[_]](
      l: ValueFunction[Obs, M, Id],
      r: ValueFunction[Obs, M, Id],
      states: Traversable[State[A, Obs, R, Id]]
  ): Boolean =
    states.forall(s => greedyOptions(l, s) == greedyOptions(r, s))

  def greedyOptions[A, Obs, R: ToReal, M[_]](
      valueFn: ValueFunction[Obs, M, Id],
      state: State[A, Obs, R, Id]
  ): Set[A] = Util.allMaxBy[A, Value[Real]](state.actions) { a =>
    val (reward, newState) = state.dynamics(a)
    valueFn.stateValue(newState.observation).from(ToReal(reward))
  }

  // Something is going on here... this is a way of evaluating the best options
  // available from the state. But the evaluation of options DEFINITELY has to
  // be shared with the Bellman implementation, or the MapValueFunction. How can
  // we collapse it all down and share some code?
  def greedyOptionsStochastic[A, Obs, R: ToReal, M[_]](
      valueFn: ValueFunction[Obs, M, Categorical],
      state: State[A, Obs, R, Categorical],
      default: Value[Real]
  ): Set[A] = Util.allMaxBy[A, Value[Real]](state.actions) { a =>
    actionValue(valueFn, state, a, default)
  }

  /**
    This returns the value of the action, given categorical dynamics of the
    state.
    */
  def actionValue[A, Obs, R: ToReal, M[_]](
      valueFn: ValueFunction[Obs, M, Categorical],
      state: State[A, Obs, R, Categorical],
      action: A,
      default: Value[Real]
  ): Value[Real] =
    Semigroup[Value[Real]]
      .combineAllOption(
        state.dynamics(action).pmf.toList.map {
          case ((reward, newState), weight) =>
            valueFn
              .stateValue(newState.observation)
              .from(ToReal(reward))
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
      combine: (Real, Real) => Real
  ): Boolean =
    Ordering[Real].lt(
      Util.diff[Obs]((l.seen ++ r.seen), l.stateValue(_).get, r.stateValue(_).get, combine),
      Real(epsilon)
    )
}
