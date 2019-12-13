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

  type Trajectory[Obs, A, R] = Iterator[((Obs, A, R), ShouldUpdateState)]
  type Tracker[Obs, A, R, T] = MonoidAggregator[(Obs, A, R), T, Trajectory[Obs, A, R]]

  object Tracker {
    type FirstVisit[Obs, A, R] = Tracker[Obs, A, R, FrequencyTracker[(Obs, A, R), Obs]]
    type EveryVisit[Obs, A, R] = Tracker[Obs, A, R, Vector[(Obs, A, R)]]

    def firstVisit[Obs, A, R]: FirstVisit[Obs, A, R] = {
      implicit val m = FrequencyTracker.monoid[(Obs, A, R), Obs](_._1)
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
    def everyVisit[Obs, A, R]: EveryVisit[Obs, A, R] =
      Aggregator.appendMonoid(
        appnd = _ :+ _,
        pres = _.reverseIterator.map((_, ShouldUpdateState.yes))
      )
  }

  def play[Obs, A, R, M[_]: Monad](
      policy: Policy[Obs, A, R, M, M],
      state: State[Obs, A, R, M]
  ): M[(state.This, (Obs, A, R))] =
    policy.choose(state).flatMap { a =>
      state
        .act(a)
        .map { case (r, s2) => (s2, (state.observation, a, r)) }
    }

  /**
    Takes a static policy and a starting state and returns an M containing the
    final state and the trajectory that got us there.
    */
  def playEpisode[Obs, A, R, M[_]: Monad, T](
      policy: Policy[Obs, A, R, M, M],
      state: State[Obs, A, R, M],
      tracker: Tracker[Obs, A, R, T]
  ): M[(state.This, Trajectory[Obs, A, R])] =
    Util.iterateUntilM(state, tracker)(
      play(policy, _)
    )(_.isTerminal)

  /**
    Specialized version that keeps track of frequencies too.
    */
  def firstVisit[Obs, A, R, M[_]: Monad](
      policy: Policy[Obs, A, R, M, M],
      state: State[Obs, A, R, M]
  ): M[(state.This, Trajectory[Obs, A, R])] =
    playEpisode[Obs, A, R, M, FrequencyTracker[(Obs, A, R), Obs]](
      policy,
      state,
      Tracker.firstVisit
    )

  /**
    We almost have all of the pieces. Now, to do it right, we need to zip back
    and forth along trajectories, updating the value function each time... maybe
    not updating the policy though.


    */
  def processTrajectory[Obs, A, R, T](
      trajectory: Trajectory[Obs, A, R],
      valueFn: ActionValueFn[Obs, A, T],
      agg: MonoidAggregator[R, T, T]
  ): ActionValueFn[Obs, A, T] =
    // I think we HAVE to start with zero here, since we always have some sort
    // of zero value for the final state, even if we use a new aggregation type.
    trajectory
      .foldLeft((valueFn, agg.monoid.zero)) {
        case ((vf, t), ((obs, a, r), shouldUpdate)) =>
          val t2 = agg.append(t, r)
          if (shouldUpdate.get) {
            (vf.learn(obs, a, agg.present(t2)), t2)
          } else (vf, t2)
      }
      ._1
}
