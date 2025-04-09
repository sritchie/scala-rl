package com.scalarl

import cats.Functor

/** Represents a single step in a reinforcement learning episode.
  *
  * SARS stands for State-Action-Reward-State, capturing the complete transition:
  *   - The initial state the agent was in
  *   - The action the agent took
  *   - The reward received for taking that action
  *   - The next state the environment transitioned to
  */
final case class SARS[Obs, A, R, S[_]](
    state: State[Obs, A, R, S],
    action: A,
    reward: R,
    nextState: State[Obs, A, R, S]
) {

  /** Maps the observation type of this SARS to a new type.
    *
    * @param f
    *   The function to transform the observation from type Obs to type P
    * @param S
    *   Evidence that S has a Functor instance
    */
  def mapObservation[P](f: Obs => P)(implicit S: Functor[S]): SARS[P, A, R, S] =
    SARS(state.mapObservation(f), action, reward, nextState.mapObservation(f))

  /** Maps the reward type of this SARS to a new type.
    *
    * @param f
    *   The function to transform the reward from type R to type T
    * @param S
    *   Evidence that S has a Functor instance
    */
  def mapReward[T](f: R => T)(implicit S: Functor[S]): SARS[Obs, A, T, S] =
    SARS(state.mapReward(f), action, f(reward), nextState.mapReward(f))

}
