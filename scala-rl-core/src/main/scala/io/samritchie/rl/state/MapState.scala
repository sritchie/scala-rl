/**
  * A MapState is a particular kind of state.
  *
  * TODO - maybe get a way to do observations in here?
  */
package io.samritchie.rl
package state

import cats.Functor
import com.stripe.rainier.cats._
import com.stripe.rainier.core.Generator

/**
  * MapState that doesn't evolve.
  */
case class StaticMapState[A, R, S[_]](
    rewards: Map[A, S[R]]
)(implicit S: Functor[S])
    extends State[A, Unit, R, S] {
  override val observation: Unit = ()

  override def dynamics[O2 >: Unit]: Map[A, S[(R, State[A, O2, R, S])]] =
    rewards.mapValues(S.map(_)(r => (r, this)))
}

/**
  * MDP with a single state.
  */
case class MapState[A, Obs, R, S[_]](
    observation: Obs,
    rewards: Map[A, S[R]],
    step: (A, Obs, R, S[R]) => (Obs, S[R])
)(implicit S: Functor[S])
    extends State[A, Obs, R, S] {

  private def updateForA[O2 >: Obs](a: A, r: R): State[A, O2, R, S] = {
    val (newObservation, newGen) = step(a, observation, r, rewards(a))
    MapState(
      newObservation,
      rewards.updated(a, newGen),
      step
    )
  }

  override def dynamics[O2 >: Obs]: Map[A, S[(R, State[A, O2, R, S])]] =
    rewards.map {
      case (a, g) => (a, S.map(g)(r => (r, updateForA[O2](a, r))))
    }
}

object MapState {
  private def genMap[A, R](
      actions: Set[A],
      gen: Generator[Generator[R]]
  ): Generator[Map[A, Generator[R]]] =
    gen.repeat(actions.size).map(actions.zip(_).toMap)

  /**
    * One of the two ways to construct a MapState.
    */
  def static[A, Obs, R](
      actions: Set[A],
      gen: Generator[Generator[R]]
  ): Generator[StaticMapState[A, R, Generator]] =
    genMap(actions, gen).map(StaticMapState(_))

  /**
    * The second of two ways to construct a MapState.
    */
  def updating[A, Obs, R](
      actions: Set[A],
      initialObservation: Obs,
      gen: Generator[Generator[R]],
      step: (A, Obs, R, Generator[R]) => (Obs, Generator[R])
  ): Generator[MapState[A, Obs, R, Generator]] =
    genMap(actions, gen).map(MapState(initialObservation, _, step))
}
