/**
  Policy implementation, get some!
  */
package io.samritchie.rl

import cats.{Applicative, Functor, Id}
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
  def learn(sars: SARS[Obs, A, R, S]): M[This]

  def contramapObservation[P](f: P => Obs)(implicit M: Functor[M], S: Functor[S]): Policy[P, A, R, M, S] =
    new Policy[P, A, R, M, S] {
      override def choose(state: State[P, A, R, S]) = self.choose(state.mapObservation(f))
      override def learn(sars: SARS[P, A, R, S]) =
        M.map(self.learn(sars.mapObservation(f)))(
          _.contramapObservation(f)
        )
    }

  def contramapReward[T](f: T => R)(implicit M: Functor[M], S: Functor[S]): Policy[Obs, A, T, M, S] =
    new Policy[Obs, A, T, M, S] {
      override def choose(state: State[Obs, A, T, S]) = self.choose(state.mapReward(f))
      override def learn(sars: SARS[Obs, A, T, S]) =
        M.map(self.learn(sars.mapReward(f)))(
          _.contramapReward(f)
        )
    }

  /**
    * Just an idea to see if I can make stochastic deciders out of
    * deterministic deciders. We'll see how this develops.
    */
  def mapK[N[_]](f: FunctionK[M, N])(implicit N: Functor[N]): Policy[Obs, A, R, N, S] =
    new Policy[Obs, A, R, N, S] { r =>
      override def choose(state: State[Obs, A, R, S]): N[A] = f(self.choose(state))
      override def learn(
          sars: SARS[Obs, A, R, S]
      ): N[Policy[Obs, A, R, N, S]] =
        N.map(f(self.learn(sars)))(_.mapK(f))
    }
}

object Policy {

  /**
    Build an instance.
    */
  def build[Obs, A, R, M[_], S[_]](
      chooseFn: State[Obs, A, R, S] => M[A],
      learnFn: (Policy[Obs, A, R, M, S], SARS[Obs, A, R, S]) => M[Policy[Obs, A, R, M, S]]
  ): Policy[Obs, A, R, M, S] =
    new Policy[Obs, A, R, M, S] { self =>
      override def choose(state: State[Obs, A, R, S]): M[A] = chooseFn(state)
      override def learn(sars: SARS[Obs, A, R, S]): M[This] = learnFn(self, sars)
    }

  /**
    If all you care about is a choose fn.
    */
  def choose[Obs, A, R, M[_], S[_]](
      chooseFn: State[Obs, A, R, S] => M[A]
  )(implicit M: Applicative[M]): Policy[Obs, A, R, M, S] =
    build(chooseFn, (self, _) => M.pure(self))

  /**
    Full exploration. mapK(Cat.setToCat) to get the usual Greedy.
    */
  def random[Obs, A, R, S[_]]: Policy[Obs, A, R, Cat, S] =
    Policy.choose(s => Cat.fromSet(s.actions))

  /**
    Full greed. mapK(Cat.setToCat) to get the usual Greedy.
    */
  def greedy[Obs, A, R, T: Ordering, S[_]](
      evaluator: Evaluator.ActionValue[Obs, A, R, T, S]
  ): Policy[Obs, A, R, Cat, S] =
    choose(s => Cat.fromSet(evaluator.greedyOptions(s)))

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
  def constant[Obs, A, R, S[_]](a: A): Policy[Obs, A, R, Id, S] =
    choose[Obs, A, R, Id, S](_ => a)
}
