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

import com.stripe.rainier.core.Generator

/**
  * A world should probably have a generator of states and
  * actions... and then you can use that to get to the next
  * thing. The state here is going to be useful in the Markov
  * model; for the bandit we only have a single state, not that
  * useful.
  */
trait State[A, Reward] {

  /**
    * For every action you could take, returns a generator of the next
    * set of rewards. This is a real world, or a sample model. If we
    * want the full distribution we're going to have to build out a
    * better interface. Good enough for now.
    */
  def dynamics: Map[A, Generator[(Reward, State[A, Reward])]]

  /**
    * Return None if it's an invalid action, otherwise gives us the
    * next state. (Make this better later.)
    */
  def act(action: A): Option[Generator[(Reward, State[A, Reward])]] = dynamics.get(action)

  /**
    * Returns a list of possible actions to take from this state.
    */
  def actions: Set[A] = dynamics.keySet
}

/**
  * Then we have a bandit... a single state thing.
  */
object State {

  /**
    * MDP with state derived from a map.
    */
  case class MapState[A, R](dynamics: Map[A, Generator[(R, State[A, R])]]) extends State[A, R]

  /**
    * This creates a State object directly from a dynamics map.
    */
  def fromMap[A, R](dynamics: Map[A, Generator[(R, State[A, R])]]): MapState[A, R] =
    MapState[A, R](dynamics)
}
