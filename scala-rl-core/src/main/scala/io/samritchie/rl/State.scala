/**
  * Okay, let's see what we can do for this bandit thing. We want
  * something that can run as a bandit, and then a world to run it in.
  *
  *
  * FIRST STEP:
  *
  * - Recreate figure 2_2, playing the bandit for a while. see what happens.
  * - epsilon-greedy
  * - ucb
  * - gradient
  *
  *
  * I would use it but I think it's old.
  *
  * Then... do we go to the state monad, on TOP of the generator, to
  * return the reward? Instead of returning the pair directly?
  *
  * Let's give it a try!
  */
package io.samritchie.rl

import cats.Functor
import cats.arrow.FunctionK

/**
  * A world should probably have a generator of states and
  * actions... and then you can use that to get to the next
  * thing. The state here is going to be useful in the Markov
  * model; for the bandit we only have a single state, not that
  * useful.
  */
trait State[A, +Obs, R, M[_]] { self =>
  def observation: Obs

  /**
    * For every action you could take, returns a generator of the next
    * set of rewards. This is a real world, or a sample model. If we
    * want the full distribution we're going to have to build out a
    * better interface. Good enough for now.
    */
  def dynamics[O2 >: Obs]: Map[A, M[(R, State[A, O2, R, M])]]

  /**
    * Return None if it's an invalid action, otherwise gives us the
    * next state. (Make this better later.)
    */
  def act[O2 >: Obs](action: A): Option[M[(R, State[A, O2, R, M])]] =
    dynamics.get(action)

  /**
    * Returns a list of possible actions to take from this state. To specify the
    * terminal state, return an empty set.
    */
  def actions: Set[A] = dynamics.keySet

  def isTerminal: Boolean = actions.isEmpty

  /**
    * Just an idea to see if I can make stochastic deciders out of
    * deterministic deciders. We'll see how this develops.
    */
  def mapK[N[_]: Functor](f: FunctionK[M, N]): State[A, Obs, R, N] = new State[A, Obs, R, N] {

    private def mapPair[T, U >: T](pair: M[(R, State[A, T, R, M])]): N[(R, State[A, U, R, N])] =
      Functor[N].map(f(pair)) { case (r, s) => (r, s.mapK(f)) }

    def observation = self.observation

    def dynamics[O2 >: Obs]: Map[A, N[(R, State[A, O2, R, N])]] =
      self.dynamics.mapValues(mapPair[O2, O2](_))

    override def act[O2 >: Obs](action: A): Option[N[(R, State[A, O2, R, N])]] =
      self.act(action).map(mapPair[Obs, O2](_))

    override def actions: Set[A] = self.actions
  }
}
