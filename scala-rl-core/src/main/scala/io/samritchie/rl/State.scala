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
trait State[A, Obs, @specialized(Int, Long, Float, Double) R, M[_]] { self =>
  type This = State[A, Obs, R, M]

  def observation: Obs

  /**
    * For every action you could take, returns a generator of the next
    * set of rewards. This is a real world, or a sample model. If we
    * want the full distribution we're going to have to build out a
    * better interface. Good enough for now.
    */
  def dynamics: Map[A, M[(R, This)]]
  def actions: Set[A] = dynamics.keySet

  /**
    NOTE - The state is responsible for returning a penalty if the action is
    invalid.
    */
  def act(action: A): Option[M[(R, This)]] =
    dynamics.get(action)

  /**
    * Returns a list of possible actions to take from this state. To specify the
    * terminal state, return an empty set.
    */
  def isTerminal: Boolean = actions.isEmpty

  def mapObservation[P](f: Obs => P)(implicit M: Functor[M]): State[A, P, R, M] =
    new State[A, P, R, M] {
      private def innerMap(pair: M[(R, State[A, Obs, R, M])]) =
        M.map(pair) { case (r, s) => (r, s.mapObservation(f)) }
      override def observation = f(self.observation)
      override def dynamics = self.dynamics.mapValues(innerMap(_))
      override def act(action: A) = self.act(action).map(innerMap(_))
      override def actions: Set[A] = self.actions
    }

  def mapReward[T](f: R => T)(implicit M: Functor[M]): State[A, Obs, T, M] =
    new State[A, Obs, T, M] {
      private def innerMap(pair: M[(R, State[A, Obs, R, M])]) =
        M.map(pair) { case (r, s) => (f(r), s.mapReward(f)) }

      override def observation = self.observation
      override def dynamics = self.dynamics.mapValues(innerMap(_))
      override def act(action: A) = self.act(action).map(innerMap(_))
      override def actions: Set[A] = self.actions
    }

  def mapK[N[_]](f: FunctionK[M, N])(implicit N: Functor[N]): State[A, Obs, R, N] = new State[A, Obs, R, N] {
    private def innerMap(pair: M[(R, State[A, Obs, R, M])]) =
      N.map(f(pair)) { case (r, s) => (r, s.mapK(f)) }
    override def observation = self.observation
    override def dynamics = self.dynamics.mapValues(innerMap(_))
    override def act(action: A) = self.act(action).map(innerMap(_))
    override def actions: Set[A] = self.actions
  }
}
