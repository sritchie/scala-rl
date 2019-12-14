/**
  * A TickState is limited in the number of ticks it can perform.
  */
package io.samritchie.rl
package state

import cats.Functor
import cats.arrow.FunctionK
import cats.syntax.functor._

/**
  * State that ends after a certain number of interactions. This is useful for
  * turning a non-episodic into an episodic task.
  */
case class TickState[Obs, A, R, S[_]: Functor](
    state: State[Obs, A, R, S],
    tick: Int,
    limit: Int
) extends State[Obs, A, R, S] {
  override def observation: Obs = state.observation

  override def dynamics: Map[A, S[(R, This)]] =
    if (isTerminal) Map.empty
    else {
      state.dynamics.mapValues { v =>
        v.map { case (r, innerState) => (r, TickState[Obs, A, R, S](innerState, tick - 1, limit)) }
      }
    }

  override def invalidMove: S[(R, This)] = state.invalidMove

  override def actions: Set[A] = if (isTerminal) Set.empty else state.actions

  override def act(action: A): S[(R, This)] = dynamics.getOrElse(action, invalidMove)

  override def isTerminal: Boolean = (tick >= limit) || state.isTerminal

  override def mapObservation[P](f: Obs => P)(implicit S: Functor[S]): State[P, A, R, S] =
    TickState(state.mapObservation(f)(S), tick, limit)(S)

  override def mapReward[T](f: R => T)(implicit S: Functor[S]): State[Obs, A, T, S] =
    TickState(state.mapReward(f)(S), tick, limit)(S)

  override def mapK[N[_]](f: FunctionK[S, N])(implicit N: Functor[N]): State[Obs, A, R, N] =
    TickState(state.mapK(f), tick, limit)
}
