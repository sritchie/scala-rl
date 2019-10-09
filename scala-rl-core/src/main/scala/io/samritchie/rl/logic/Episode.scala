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
    * Plays a single turn and returns a generator that returns the
    * reward and the next state. If the chosen state's not allowed,
    * returns the supplied penalty and sends the agent back to the
    * initial state.
    */
  def play[A, Obs, R, M[_]](
      policy: Policy[A, Obs, R, M, M],
      state: State[A, Obs, R, M],
      penalty: R
  )(implicit M: Monad[M]): M[(Policy[A, Obs, R, M, M], R, State[A, Obs, R, M])] =
    M.flatMap(policy.choose(state)) { a =>
      val next = state.act(a).getOrElse(M.pure((penalty, state)))
      M.map(next) { rs =>
        (policy.learn(state, a, rs._1), rs._1, rs._2)
      }
    }

  /**
    * Returns the final policy, a sequence of the rewards received and
    * the final state.
    */
  def playN[A, Obs, R, M[_]: Monad](
      policy: Policy[A, Obs, R, M, M],
      state: State[A, Obs, R, M],
      penalty: R,
      nTimes: Int
  ): M[(Policy[A, Obs, R, M, M], Seq[R], State[A, Obs, R, M])] =
    Util.iterateM(nTimes)((policy, Seq.empty[R], state)) {
      case (p, rs, s) =>
        play(p, s, penalty).map {
          case (newP, r, newS) =>
            (newP, rs :+ r, newS)
        }
    }

  /**
    * Takes an initial set of policies and a state...
    */
  def playMany[A, Obs, R, M[_]: Monad](
      pairs: List[(Policy[A, Obs, R, M, M], State[A, Obs, R, M])],
      penalty: R
  )(
      rewardSum: List[R] => R
  ): M[(List[(Policy[A, Obs, R, M, M], State[A, Obs, R, M])], R)] =
    pairs.toList
      .traverse {
        case (p, s) => play(p, s, penalty)
      }
      .map { results =>
        (results.map { case (a, b, c) => (a, c) }, rewardSum(results.map(_._2)))
      }

  /**
    * Takes an initial set of policies and a state...
    */
  def playManyN[A, Obs, R, M[_]: Monad](
      pairs: List[(Policy[A, Obs, R, M, M], State[A, Obs, R, M])],
      penalty: R,
      nTimes: Int
  )(
      rewardSum: List[R] => R
  ): M[(List[(Policy[A, Obs, R, M, M], State[A, Obs, R, M])], List[R])] =
    Util.iterateM(nTimes)((pairs, List.empty[R])) {
      case (ps, rs) =>
        playMany(ps, penalty)(rewardSum).map {
          case (newPS, r) =>
            (newPS, rs :+ r)
        }
    }
}
