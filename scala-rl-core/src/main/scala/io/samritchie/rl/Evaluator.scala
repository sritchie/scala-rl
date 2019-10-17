package io.samritchie.rl

import io.samritchie.rl.util.{ExpectedValue, ToDouble}

/**
  The trick here is to figure out what is going on with aggregation, and with
  the aggregation types. I think I can solve this this evening, given a bit of
  time.

  One problem is aggregating back across an episode.

  One guess I had is that the trajectory is Agg[R, T, T]
  and that internal to the valuefn, we have Agg[T, U, T]
  */
object Evaluator {

  /**
    Evaluator that uses a world's dynamics to estimate the value of a given
    action.
    */
  def oneAhead[Obs, A, R: ToDouble, M[_], S[_]: ExpectedValue](
      valueFn: StateValueFn[Obs],
      finalStateValue: Value[Double]
  ): ActionValue[Obs, A, R, S] =
    StateValue
      .fn[Obs, A, R, S](valueFn)
      .byStateValue(finalStateValue)

  /**
    The full bellman estimation, where we know the dynamics of the policy and of
    the system.
    */
  def bellman[Obs, A, R: ToDouble, M[_]: ExpectedValue, S[_]: ExpectedValue](
      valueFn: StateValueFn[Obs],
      policy: Policy[Obs, A, R, M, S],
      finalStateValue: Value[Double],
      noActionValue: Value[Double]
  ): StateValue[Obs, A, R, S] =
    StateValue
      .fn[Obs, A, R, S](valueFn)
      .byStateValue(finalStateValue)
      .byPolicy(policy, noActionValue)

  sealed trait StateValue[Obs, A, R, S[_]] extends Product with Serializable {
    def evaluate(state: State[Obs, A, R, S]): Value[Double]
    def byStateValue(
        finalStateValue: Value[Double]
    )(implicit R: ToDouble[R], S: ExpectedValue[S]): ActionValue[Obs, A, R, S] =
      ActionValue.ByStateValue(this, finalStateValue)
  }
  sealed trait ActionValue[Obs, A, R, S[_]] extends Product with Serializable {
    def evaluate(state: State[Obs, A, R, S], a: A): Value[Double]

    def greedyOptions(state: State[Obs, A, R, S]): Set[A] =
      Util.allMaxBy[A, Value[Double]](state.actions)(evaluate(state, _))

    def byPolicy[M[_]: ExpectedValue](
        policy: Policy[Obs, A, R, M, S],
        noActionValue: Value[Double]
    ): StateValue[Obs, A, R, S] =
      StateValue.ByPolicy(this, policy, noActionValue)
  }

  object StateValue {
    def fn[Obs, A, R, S[_]](f: StateValueFn[Obs]): StateValue[Obs, A, R, S] = Fn(f)

    /**
    This evaluates the state's value directly.
      */
    final case class Fn[Obs, A, R, S[_]](f: StateValueFn[Obs]) extends StateValue[Obs, A, R, S] {
      def evaluate(state: State[Obs, A, R, S]): Value[Double] =
        f.stateValue(state.observation)
    }

    /**
    Evaluates the state's value by weighting evaluated action values by the
    policy's chance of choosing each action.
      */
    final case class ByPolicy[Obs, A, R, M[_]: ExpectedValue, S[_]](
        evaluator: ActionValue[Obs, A, R, S],
        policy: Policy[Obs, A, R, M, S],
        noActionValue: Value[Double]
    ) extends StateValue[Obs, A, R, S] {
      def evaluate(state: State[Obs, A, R, S]): Value[Double] =
        ExpectedValue[M].get(policy.choose(state), noActionValue) { a =>
          evaluator.evaluate(state, a)
        }

    }
  }

  object ActionValue {

    /**
    Evaluates the action's value directly.
      */
    final case class Fn[Obs, A, R, S[_]](f: ActionValueFn[Obs, A, R]) extends ActionValue[Obs, A, R, S] {
      def evaluate(state: State[Obs, A, R, S], a: A): Value[Double] =
        f.actionValue(state.observation, a)
    }

    /**
    Evaluates the action's value by using the dynamics of the new state, plus a
    state value evaluator, to figure out the value.
      */
    final case class ByStateValue[Obs, A, R: ToDouble, M[_], S[_]: ExpectedValue](
        evaluator: StateValue[Obs, A, R, S],
        finalStateValue: Value[Double]
    ) extends ActionValue[Obs, A, R, S] {
      def evaluate(state: State[Obs, A, R, S], a: A): Value[Double] =
        ExpectedValue[S].get(state.act(a), finalStateValue) {
          case (r, s) => evaluator.evaluate(s).from(ToDouble[R].apply(r))
        }
    }
  }
}
