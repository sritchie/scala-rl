/**
  Policies are key.
  */
package io.samritchie.rl

import cats.{Functor, Monad}
import cats.arrow.FunctionK
import cats.implicits._

import scala.language.higherKinds

/**
  * This is how agents actually choose what comes next. This is a
  * stochastic policy. We have to to be able to match this up with a
  * state that has the same monadic return type, but for now it's
  * hardcoded.
  *
  * A - Action
  * Obs - the observation offered by this state.
  * R - reward
  * M - the monadic type offered by the policy.
  * S - the monad for the state.
  */
trait Policy[A, Obs, @specialized(Int, Long, Float, Double) R, M[_], S[_]] { self =>
  def choose(state: State[A, Obs, R, S]): M[A]

  /**
    TODO Note - I can imagine that we wouldn't want to present a reward,
    necessarily, but some aggregated thing.

    By default this just returns itself, no learning happening.
    */
  def learn(state: State[A, Obs, R, S], action: A, reward: R): Policy[A, Obs, R, M, S] = self

  def contramapObservation[P](f: P => Obs)(implicit S: Functor[S]): Policy[A, P, R, M, S] =
    new Policy[A, P, R, M, S] {
      override def choose(state: State[A, P, R, S]) = self.choose(state.mapObservation(f))
      override def learn(state: State[A, P, R, S], action: A, reward: R) =
        self.learn(state.mapObservation(f), action, reward).contramapObservation(f)
    }

  def contramapReward[T](f: T => R)(implicit S: Functor[S]): Policy[A, Obs, T, M, S] =
    new Policy[A, Obs, T, M, S] {
      override def choose(state: State[A, Obs, T, S]) = self.choose(state.mapReward(f))
      override def learn(state: State[A, Obs, T, S], action: A, reward: T) =
        self.learn(state.mapReward(f), action, f(reward)).contramapReward(f)
    }

  /**
    * Just an idea to see if I can make stochastic deciders out of
    * deterministic deciders. We'll see how this develops.
    */
  def mapK[N[_]](f: FunctionK[M, N]): Policy[A, Obs, R, N, S] =
    new Policy[A, Obs, R, N, S] {
      override def choose(state: State[A, Obs, R, S]): N[A] = f(self.choose(state))
      override def learn(state: State[A, Obs, R, S], action: A, reward: R): Policy[A, Obs, R, N, S] =
        self.learn(state, action, reward).mapK(f)
    }
}

object Policy {
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
