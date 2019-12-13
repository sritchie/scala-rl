/**
  Logic for playing episodic games.
  */
package io.samritchie.rl
package logic

import cats.Monad
import cats.implicits._
import com.twitter.algebird.{Aggregator, MonoidAggregator}
import io.samritchie.rl.util.FrequencyTracker

object MonteCarlo {
  case class ShouldUpdateState(get: Boolean) extends AnyVal
  object ShouldUpdateState {
    val yes = ShouldUpdateState(true)
    val no = ShouldUpdateState(false)
  }

  type Snap[Obs, A, R, M[_]] = (State[Obs, A, R, M], A, R)
  type Trajectory[Obs, A, R, M[_]] = Iterator[(Snap[Obs, A, R, M], ShouldUpdateState)]
  type Tracker[Obs, A, R, T, M[_]] = MonoidAggregator[Snap[Obs, A, R, M], T, Trajectory[Obs, A, R, M]]

  object Tracker {
    type FirstVisit[Obs, A, R, M[_]] = Tracker[Obs, A, R, FrequencyTracker[Snap[Obs, A, R, M], Obs], M]
    type EveryVisit[Obs, A, R, M[_]] = Tracker[Obs, A, R, Vector[Snap[Obs, A, R, M]], M]

    def firstVisit[Obs, A, R, M[_]]: FirstVisit[Obs, A, R, M] = {
      implicit val m = FrequencyTracker.monoid[Snap[Obs, A, R, M], Obs](_._1.observation)
      Aggregator.appendMonoid(
        appnd = _ :+ _,
        pres = _.reverseIterator.map { case (t, seen) => (t, ShouldUpdateState(seen == 0)) }
      )
    }

    /**
      Aggregator that returns an iterator of a trajectory, with each state
      paired with a boolean signalling whether or not it should trigger a state
      update.
      */
    def everyVisit[Obs, A, R, M[_]]: EveryVisit[Obs, A, R, M] =
      Aggregator.appendMonoid(
        appnd = _ :+ _,
        pres = _.reverseIterator.map((_, ShouldUpdateState.yes))
      )
  }

  def play[Obs, A, R, M[_]: Monad](
      policy: Policy[Obs, A, R, M, M],
      state: State[Obs, A, R, M]
  ): M[(state.This, Snap[Obs, A, R, M])] =
    policy.choose(state).flatMap { a =>
      state
        .act(a)
        .map { case (r, s2) => (s2, (state, a, r)) }
    }

  /**
    Takes a static policy and a starting state and returns an M containing the
    final state and the trajectory that got us there.
    */
  def playEpisode[Obs, A, R, M[_]: Monad, T](
      policy: Policy[Obs, A, R, M, M],
      state: State[Obs, A, R, M],
      tracker: Tracker[Obs, A, R, T, M]
  ): M[(state.This, Trajectory[Obs, A, R, M])] =
    Util.iterateUntilM(state, tracker)(
      play(policy, _)
    )(_.isTerminal)

  /**
    Specialized version that keeps track of frequencies too.
    */
  def firstVisit[Obs, A, R, M[_]: Monad](
      policy: Policy[Obs, A, R, M, M],
      state: State[Obs, A, R, M]
  ): M[(state.This, Trajectory[Obs, A, R, M])] =
    playEpisode[Obs, A, R, M, FrequencyTracker[Snap[Obs, A, R, M], Obs]](
      policy,
      state,
      Tracker.firstVisit
    )

  /**
    We almost have all of the pieces. Now, to do it right, we need to zip back
    and forth along trajectories, updating the value function each time... maybe
    not updating the policy though.
    */
  def processTrajectory[Obs, A, R, G, M[_]](
      trajectory: Trajectory[Obs, A, R, M],
      valueFn: ActionValueFn[Obs, A, G],
      agg: MonoidAggregator[R, G, G]
  ): ActionValueFn[Obs, A, G] =
    // I think we HAVE to start with zero here, since we always have some sort
    // of zero value for the final state, even if we use a new aggregation type.
    trajectory
      .foldLeft((valueFn, agg.monoid.zero)) {
        case ((vf, g), ((s, a, r), shouldUpdate)) =>
          val g2 = agg.append(g, r)
          if (shouldUpdate.get) {
            (vf.learn(s.observation, a, agg.present(g2)), g2)
          } else (vf, g2)
      }
      ._1

  /**
    So if you have G, your return...

    first let's just do the weights.
    */
  def processTrajectoryWeighted[Obs, A, R, G, M[_]](
      trajectory: Trajectory[Obs, A, R, M],
      valueFn: ActionValueFn[Obs, A, G],
      agg: MonoidAggregator[R, G, G]
  ): ActionValueFn[Obs, A, G] =
    // I think we HAVE to start with zero here, since we always have some sort
    // of zero value for the final state, even if we use a new aggregation type.
    trajectory
      .foldLeft((valueFn, agg.monoid.zero, 1)) {
        case ((vf, g, w), ((s, a, r), shouldUpdate)) =>
          val g2 = agg.append(g, r)
          if (shouldUpdate.get) {
            (vf.learn(s.observation, a, agg.present(g2)), g2, w)
          } else (vf, g2, w)
      }
      ._1
}
