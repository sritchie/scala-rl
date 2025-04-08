/** Logic for playing episodic games.
  */
package com.scalarl
package logic

import cats.{Functor, Monad}
import cats.implicits._

object Episode {
  import cats.syntax.functor._

  /** Wrapper around a combination of state and policy. A moment in time. this wraps up a common
    * thing that we interact with...
    */
  case class Moment[Obs, A, R, M[_]](
      policy: Policy[Obs, A, R, M, M],
      state: State[Obs, A, R, M]
  ) {
    def choice: M[A] = policy.choose(state)

    def act(
        a: A
    )(implicit M: Functor[M]): M[(Moment[Obs, A, R, M], SARS[Obs, A, R, M])] =
      state.act(a).map { case (r, s2) =>
        val sars = SARS(state, a, r, s2)
        (Moment(policy.learn(sars), s2), sars)
      }

    /** Play a single round of a game. Returns M of:
      *
      * \- pair of (the new policy that's learned, the new state you end up in) \- triple of (state
      * you came from, action you took, reward you received).
      */
    def play(implicit
        M: Monad[M]
    ): M[(Moment[Obs, A, R, M], SARS[Obs, A, R, M])] =
      policy.choose(state).flatMap(act)
  }

  /** Takes a policy and a starting state and returns an M containing the final policy, final state
    * and the trajectory that got us there.
    */
  def playEpisode[Obs, A, R, M[_]: Monad, T](
      moment: Moment[Obs, A, R, M],
      tracker: MonteCarlo.Tracker[Obs, A, R, T, M]
  ): M[(Moment[Obs, A, R, M], MonteCarlo.Trajectory[Obs, A, R, M])] =
    Util.iterateUntilM(moment, tracker)(_.play)(_.state.isTerminal)

  /** Specialized version of playEpisode that only updates every first time a state is seen.
    */
  def firstVisit[Obs, A, R, M[_]: Monad](
      moment: Moment[Obs, A, R, M]
  ): M[(Moment[Obs, A, R, M], MonteCarlo.Trajectory[Obs, A, R, M])] =
    Episode.playEpisode(moment, MonteCarlo.Tracker.firstVisit)

  // Below this we have the functions that have been useful for tracking bandit
  // problems. I wonder if there is some nice primitive we can develop for
  // clicking many agents forward at once. Is that an interesting thing to do?

  /** Takes a list of policy, initial state pairs and plays a single episode of a game with each of
    * them.
    */
  def playMany[Obs, A, R, M[_]: Monad](
      moments: List[Moment[Obs, A, R, M]]
  )(
      rewardSum: List[SARS[Obs, A, R, M]] => R
  ): M[(List[Moment[Obs, A, R, M]], R)] =
    moments.traverse(_.play).map { results =>
      (
        // this could actually build a nice trajectory for many items at once.
        results.map(_._1),
        rewardSum(results.map(_._2))
      )
    }

  /** Takes an initial set of policies and astate... we could definitely adapt this to do some
    * serious learning on the policies, and use the MonoidAggregator stuff.
    */
  def playManyN[Obs, A, R, M[_]: Monad](
      moments: List[Moment[Obs, A, R, M]],
      nTimes: Int
  )(
      rewardSum: List[SARS[Obs, A, R, M]] => R
  ): M[(List[Moment[Obs, A, R, M]], List[R])] =
    Util.iterateM(nTimes)((moments, List.empty[R])) { case (ps, rs) =>
      playMany(ps)(rewardSum).map { case (newMoment, r) =>
        (newMoment, rs :+ r)
      }
    }
}
