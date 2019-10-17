package io.samritchie.rl

import io.samritchie.rl.util.ExpectedValue

/**
  The trick here is to figure out what is going on with aggregation, and with
  the aggregation types. I think I can solve this this evening, given a bit of
  time.

  One problem is aggregating back across an episode.

  One guess I had is that the trajectory is Agg[R, T, T]
  and that internal to the valuefn, we have Agg[T, U, T]

  Let's see how this plays out.

   Tasks:

  - Get this business compiling.
  - Remove the Value[Double] wrapper around Double.
  - get the Double type more general...
  - replace Bellman with a think that uses an evaluator
  - add a "val bellman" to this file

  */
sealed trait StateValueEstimator[Obs, A, R, S[_]] {
  def estimate(state: State[Obs, A, R, S]): Double
}

object StateValueEstimator {

  /**
    This is a total cached estimate.
    */
  case class Fn[Obs](f: StateValueFn[Obs]) extends StateValueEstimator[Obs, Any, Any, Any] {
    def estimate(state: State[Obs, Any, Any, Any]): Double =
      f.stateValue(state.observation).get
  }

  /**
    A policy is the only way out of a state.
    */
  case class PolicyAV[Obs, A, R, M[_]: ExpectedValue, S[_]](
      policy: Policy[Obs, A, R, M, S],
      valueFn: ActionValueEstimator[Obs, A, R, S]
  ) extends StateValueEstimator[Obs, A, R, S] {
    def estimate(state: State[Obs, A, R, S]): Double = ???
  }

  /**
    TODO Holy fuck...if I stack two of these... does that get me an OFF POLICY
    UPDATE??? If I do an expected value of both?
    */
  case class PolicySV[Obs, A, R, M[_]: ExpectedValue, S[_]: ExpectedValue](
      policy: Policy[Obs, A, R, M, S],
      estimator: StateValueEstimator[Obs, A, R, S]
  ) extends StateValueEstimator[Obs, A, R, S] {
    // This one should use the policy, then get an expected value using the next
    // level down in the tree.
    def estimate(state: State[Obs, A, R, S]): Double = ???
  }
}

sealed trait ActionValueEstimator[Obs, A, R, S[_]] {
  def estimate(state: State[Obs, A, R, S], a: A): Double
}

object ActionValueEstimator {
  case class Fn[Obs, A, R](f: ActionValueFn[Obs, A, R]) extends ActionValueEstimator[Obs, A, R, Any] {
    def estimate(state: State[Obs, A, R, Any], a: A): Double =
      f.actionValue(state.observation, a).get
  }

  /**
    FIX THESE TWO BELOW!! Follow this:

    - For action values, you can:
    - use the estimate directly,
    - update the estimate given:
      - a model of the environment and a state value fn
      - a model of the environment and a new policy
    */
  /**
    A dynamics map is the only way out of a state.
    */
  case class BySV[Obs, A, R, M[_], S[_]: ExpectedValue](
      valueFn: StateValueEstimator[Obs, A, R, S]
  ) extends ActionValueEstimator[Obs, A, R, S] {
    def estimate(state: State[Obs, A, R, S], a: A): Double = ???
    // ExpectedValue[S].get(state.act(a), ???) {
    //   case (r, s) =>
    //     valueFn.estimate(s)
    // }
  }

  case class PolicyAV[Obs, A, R, M[_]: ExpectedValue, S[_]: ExpectedValue](
      policy: Policy[Obs, A, R, M, S],
      estimator: ActionValueEstimator[Obs, A, R, S]
  ) extends ActionValueEstimator[Obs, A, R, S] {
    // This one should use the policy, then get an expected value using the next
    // level down in the tree.
    def estimate(state: State[Obs, A, R, S], a: A): Double = ???
  }
}
