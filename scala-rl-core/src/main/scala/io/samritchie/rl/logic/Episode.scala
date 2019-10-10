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
    * Plays a single turn and returns an M containing the reward and the next
    * state. If the chosen state's not allowed, returns the supplied penalty and
    * sends the agent back to the initial state.
    */
  def play[A, Obs, R, M[_]](
      policy: Policy[A, Obs, R, M, M],
      state: State[A, Obs, R, M]
  )(implicit M: Monad[M]): M[(policy.This, R, state.This)] =
    policy.choose(state).flatMap { a =>
      state.act(a).map {
        case (r, s) =>
          (policy.learn(state, a, r), r, s)
      }
    }

  /**
    * Returns the final policy, a sequence of the rewards received and
    * the final state.
    */
  def playN[A, Obs, R, M[_]: Monad](
      policy: Policy[A, Obs, R, M, M],
      state: State[A, Obs, R, M],
      nTimes: Int
  ): M[(policy.This, Seq[R], state.This)] =
    Util.iterateM(nTimes)((policy, Seq.empty[R], state)) {
      case (p, rs, s) =>
        play(p, s).map {
          case (newP, r, newS) =>
            (newP, rs :+ r, newS)
        }
    }

  /**
    * Takes an initial set of policies and a state...
    */
  def playMany[A, Obs, R, M[_]: Monad](
      pairs: List[(Policy[A, Obs, R, M, M], State[A, Obs, R, M])]
  )(
      rewardSum: List[R] => R
  ): M[(List[(Policy[A, Obs, R, M, M], State[A, Obs, R, M])], R)] =
    pairs.toList
      .traverse {
        case (p, s) => play(p, s)
      }
      .map { results =>
        (results.map { case (a, b, c) => (a, c) }, rewardSum(results.map(_._2)))
      }

  /**
    * Takes an initial set of policies and a state...
    */
  def playManyN[A, Obs, R, M[_]: Monad](
      pairs: List[(Policy[A, Obs, R, M, M], State[A, Obs, R, M])],
      nTimes: Int
  )(
      rewardSum: List[R] => R
  ): M[(List[(Policy[A, Obs, R, M, M], State[A, Obs, R, M])], List[R])] =
    Util.iterateM(nTimes)((pairs, List.empty[R])) {
      case (ps, rs) =>
        playMany(ps)(rewardSum).map {
          case (newPS, r) =>
            (newPS, rs :+ r)
        }
    }
}
