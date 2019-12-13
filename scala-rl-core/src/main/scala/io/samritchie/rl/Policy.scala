/**
  Policies are key.

  TODO maybe we should make an Agent that is actually a policy AND a value
  function.
  */
package io.samritchie.rl

import cats.{Functor, Id}
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
trait Policy[Obs, A, @specialized(Int, Long, Float, Double) R, M[_], S[_]] { self =>
  type This = Policy[Obs, A, R, M, S]

  def choose(state: State[Obs, A, R, S]): M[A]

  /**
    TODO Note - I can imagine that we wouldn't want to present a reward,
    necessarily, but some aggregated thing.

    By default this just returns itself, no learning happening.
    */
  def learn(state: State[Obs, A, R, S], action: A, reward: R): This = self

  def contramapObservation[P](f: P => Obs)(implicit S: Functor[S]): Policy[P, A, R, M, S] =
    new Policy[P, A, R, M, S] {
      override def choose(state: State[P, A, R, S]) = self.choose(state.mapObservation(f))
      override def learn(state: State[P, A, R, S], action: A, reward: R) =
        self.learn(state.mapObservation(f), action, reward).contramapObservation(f)
    }

  def contramapReward[T](f: T => R)(implicit S: Functor[S]): Policy[Obs, A, T, M, S] =
    new Policy[Obs, A, T, M, S] {
      override def choose(state: State[Obs, A, T, S]) = self.choose(state.mapReward(f))
      override def learn(state: State[Obs, A, T, S], action: A, reward: T) =
        self.learn(state.mapReward(f), action, f(reward)).contramapReward(f)
    }

  /**
    * Just an idea to see if I can make stochastic deciders out of
    * deterministic deciders. We'll see how this develops.
    */
  def mapK[N[_]](f: FunctionK[M, N]): Policy[Obs, A, R, N, S] =
    new Policy[Obs, A, R, N, S] {
      override def choose(state: State[Obs, A, R, S]): N[A] = f(self.choose(state))
      override def learn(state: State[Obs, A, R, S], action: A, reward: R): Policy[Obs, A, R, N, S] =
        self.learn(state, action, reward).mapK(f)
    }
}

object Policy {
  def constant[Obs, A, R, S[_]](a: A) = new Policy[Obs, A, R, Id, S] {
    override def choose(state: State[Obs, A, R, S]): A = a
  }
}
