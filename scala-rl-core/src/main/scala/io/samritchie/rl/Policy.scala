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

  /**
    Full exploration. mapK(Cat.setToCat) to get the usual Greedy.
    */
  def random[Obs, A, R, T: Ordering, S[_]] = new Policy[Obs, A, R, Set, S] { self =>
    override def choose(state: State[Obs, A, R, S]): Set[A] = state.actions
  }

  /**
    Full greed. mapK(Cat.setToCat) to get the usual Greedy.
    */
  def greedy[Obs, A, R, T: Ordering, S[_]](
      evaluator: Evaluator.ActionValue[Obs, A, R, T, S]
  ) = new Policy[Obs, A, R, Set, S] { self =>
    override def choose(state: State[Obs, A, R, S]): Set[A] =
      evaluator.greedyOptions(state)
  }

  /**
    In between. This is equal to

    {{{
    epsilonGreedy(evaluator, 1.0) == greedy(evaluator).mapK(Cat.setToCat)
    epsilonGreedy(evaluator, 0.0) == random.mapK(Cat.setToCat)
    }}}
    */
  def epsilonGreedy[Obs, A, R, T: Ordering, S[_]](
      evaluator: Evaluator.ActionValue[Obs, A, R, T, S],
      epsilon: Double
  ): policy.Greedy[Obs, A, R, T, S] = new policy.Greedy(evaluator, epsilon)

  /**
    Always return the same.
    */
  def constant[Obs, A, R, S[_]](a: A) = new Policy[Obs, A, R, Id, S] {
    override def choose(state: State[Obs, A, R, S]): A = a
  }
}
