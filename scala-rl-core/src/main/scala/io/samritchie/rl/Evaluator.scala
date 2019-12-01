package io.samritchie.rl

import io.samritchie.rl.util.ExpectedValue

/**
  The trick here is to figure out what is going on with aggregation, and with
  the aggregation types. I think I can solve this this evening, given a bit of
  time.

  One problem is aggregating back across an episode.

  One guess I had is that the trajectory is Agg[R, T, T]
  and that internal to the valuefn, we have Agg[T, U, T]
  */
object Evaluator {
  import Module.DModule

  /**
    Evaluator that uses a world's dynamics to estimate the value of a given
    action.
    */
  def oneAhead[Obs, A, R, T: DModule, M[_], S[_]: ExpectedValue](
      valueFn: StateValueFn[Obs, T],
      prepare: R => T,
      merge: (T, T) => T
  ): ActionValue[Obs, A, R, T, S] =
    StateValue
      .fn[Obs, A, R, T, S](valueFn)
      .byStateValue(prepare, merge)

  /**
    The full bellman estimation, where we know the dynamics of the policy and of
    the system.
    */
  def bellman[Obs, A, R, T: DModule, M[_]: ExpectedValue, S[_]: ExpectedValue](
      valueFn: StateValueFn[Obs, T],
      policy: Policy[Obs, A, R, M, S],
      prepare: R => T,
      merge: (T, T) => T
  ): StateValue[Obs, A, R, T, S] =
    StateValue
      .fn[Obs, A, R, T, S](valueFn)
      .byStateValue(prepare, merge)
      .byPolicy(policy)

  sealed trait StateValue[Obs, A, R, T, S[_]] extends Product with Serializable {
    def evaluate(state: State[Obs, A, R, S]): T
    def byStateValue(
        prepare: R => T,
        merge: (T, T) => T
    )(implicit S: ExpectedValue[S], MV: Module[Double, T]): ActionValue[Obs, A, R, T, S] =
      ActionValue.ByStateValue(this, prepare, merge)
  }
  sealed trait ActionValue[Obs, A, R, T, S[_]] extends Product with Serializable {
    def evaluate(state: State[Obs, A, R, S], a: A): T

    def greedyOptions(state: State[Obs, A, R, S])(implicit T: Ordering[T]): Set[A] =
      Util.allMaxBy[A, T](state.actions)(evaluate(state, _))

    def byPolicy[M[_]](
        policy: Policy[Obs, A, R, M, S]
    )(implicit M: ExpectedValue[M], MV: Module[Double, T]): StateValue[Obs, A, R, T, S] =
      StateValue.ByPolicy(this, policy)
  }

  object StateValue {
    def fn[Obs, A, R, T, S[_]](f: StateValueFn[Obs, T]): StateValue[Obs, A, R, T, S] = Fn(f)

    /**
    This evaluates the state's value directly.
      */
    final case class Fn[Obs, A, R, T, S[_]](f: StateValueFn[Obs, T]) extends StateValue[Obs, A, R, T, S] {
      def evaluate(state: State[Obs, A, R, S]): T =
        f.stateValue(state.observation)
    }

    /**
    Evaluates the state's value by weighting evaluated action values by the
    policy's chance of choosing each action.
      */
    final case class ByPolicy[Obs, A, R, T: DModule, M[_]: ExpectedValue, S[_]](
        evaluator: ActionValue[Obs, A, R, T, S],
        policy: Policy[Obs, A, R, M, S]
    ) extends StateValue[Obs, A, R, T, S] {
      def evaluate(state: State[Obs, A, R, S]): T =
        ExpectedValue[M].get(policy.choose(state)) { a =>
          evaluator.evaluate(state, a)
        }
    }
  }

  object ActionValue {
    /**
    Evaluates the action's value directly.
      */
    final case class Fn[Obs, A, R, T, S[_]](f: ActionValueFn[Obs, A, T])
        extends ActionValue[Obs, A, R, T, S] {
      def evaluate(state: State[Obs, A, R, S], a: A): T =
        f.actionValue(state.observation, a)
    }

    /**
    Evaluates the action's value by using the dynamics of the new state, plus a
    state value evaluator, to figure out the value.
      */
    final case class ByStateValue[Obs, A, R, T: DModule, M[_], S[_]: ExpectedValue](
        evaluator: StateValue[Obs, A, R, T, S],
        prepare: R => T,
        merge: (T, T) => T
    ) extends ActionValue[Obs, A, R, T, S] {
      def evaluate(state: State[Obs, A, R, S], a: A): T =
        ExpectedValue[S].get(state.act(a)) {
          case (r, s) => merge(evaluator.evaluate(s), prepare(r))
        }
    }
  }
}
