/**
  Policies are key.
  */
package io.samritchie.rl

import cats.Functor
import cats.arrow.FunctionK

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
  type This = Policy[A, Obs, R, M, S]

  def choose(state: State[A, Obs, R, S]): M[A]

  /**
    TODO Note - I can imagine that we wouldn't want to present a reward,
    necessarily, but some aggregated thing.

    By default this just returns itself, no learning happening.
    */
  def learn(state: State[A, Obs, R, S], action: A, reward: R): This = self

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
