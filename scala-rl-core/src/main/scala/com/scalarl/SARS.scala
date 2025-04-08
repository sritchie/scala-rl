package com.scalarl

import cats.Functor

/** Chunk that you get back for playing an episode.
  */
final case class SARS[Obs, A, R, S[_]](
    state: State[Obs, A, R, S],
    action: A,
    reward: R,
    nextState: State[Obs, A, R, S]
) {
  def mapObservation[P](f: Obs => P)(implicit S: Functor[S]): SARS[P, A, R, S] =
    SARS(state.mapObservation(f), action, reward, nextState.mapObservation(f))

  def mapReward[T](f: R => T)(implicit S: Functor[S]): SARS[Obs, A, T, S] =
    SARS(state.mapReward(f), action, f(reward), nextState.mapReward(f))

}
