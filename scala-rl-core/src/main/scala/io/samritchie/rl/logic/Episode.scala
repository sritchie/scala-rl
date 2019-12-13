/**
  Logic for playing episodic games.
  */
package io.samritchie.rl
package logic

import cats.Monad
import cats.implicits._

object Episode {
  import cats.syntax.functor._

  /**
    Commonly used item.
    */
  case class SARS[Obs, A, R, M[_]](
      state: State[Obs, A, R, M],
      a: A,
      r: R,
      nextState: State[Obs, A, R, M]
  )

  /**
    Wrapper around a combination of state and policy. A moment in time. this
    wraps up a common thing that we interact with...
    */
  case class Moment[Obs, A, R, M[_]](
      policy: Policy[Obs, A, R, M, M],
      state: State[Obs, A, R, M]
  ) {
    def choice: M[A] = policy.choose(state)

    def act(a: A)(implicit M: Monad[M]): M[(Moment[Obs, A, R, M], SARS[Obs, A, R, M])] =
      state.act(a).flatMap {
        case (r, s2) =>
          policy.learn(state, a, r, s2).map { p =>
            (Moment(p, s2), SARS(state, a, r, s2))
          }
      }

    /**
    Play a single round of a game. Returns M of:

    - pair of (the new policy that's learned, the new state you end up in)
    - triple of (state you came from, action you took, reward you received).
      */
    def play(implicit M: Monad[M]): M[(Moment[Obs, A, R, M], SARS[Obs, A, R, M])] =
      policy.choose(state).flatMap(act)
  }

  /**
    Takes a policy and a starting state and returns an M containing the final
    policy, final state and the trajectory that got us there.
    */
  def playEpisode[Obs, A, R, M[_]: Monad, T](
      moment: Moment[Obs, A, R, M],
      tracker: MonteCarlo.Tracker[Obs, A, R, T, M]
  ): M[(Moment[Obs, A, R, M], MonteCarlo.Trajectory[Obs, A, R, M])] =
    Util.iterateUntilM(moment, tracker)(_.play)(_.state.isTerminal)

  /**
    * Takes a list of policy, initial state pairs and plays a single episode of
    * a game with each of them.
    *
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

  /**
    * Takes an initial set of policies and astate... we could definitely adapt
    * this to do some serious learning on the policies, and use the
    * MonoidAggregator stuff.
    */
  def playManyN[Obs, A, R, M[_]: Monad](
      moments: List[Moment[Obs, A, R, M]],
      nTimes: Int
  )(
      rewardSum: List[SARS[Obs, A, R, M]] => R
  ): M[(List[Moment[Obs, A, R, M]], List[R])] =
    Util.iterateM(nTimes)((moments, List.empty[R])) {
      case (ps, rs) =>
        playMany(ps)(rewardSum).map {
          case (newMoment, r) =>
            (newMoment, rs :+ r)
        }
    }
}
