/**
  * A MapState is a particular kind of state.
  *
  * TODO - maybe get a way to do observations in here?
  */
package io.samritchie.rl
package state

import cats.Functor
import cats.syntax.functor._
import com.stripe.rainier.cats._
import com.stripe.rainier.core.Generator

/**
  * MapState that doesn't evolve.
  */
case class StaticMapState[A, R, S[_]: Functor](
    rewards: Map[A, S[R]],
    penalty: S[R]
) extends State[A, Unit, R, S] {
  override val observation: Unit = ()
  override val dynamics: Map[A, S[(R, State[A, Unit, R, S])]] =
    rewards.mapValues(_.map((_, this)))
  override val invalidMove = penalty.map((_, this))
}

/**
  * MDP with a single state.
  */
case class MapState[A, Obs, R, S[_]: Functor](
    observation: Obs,
    rewards: Map[A, S[R]],
    penalty: S[R],
    // TODO make this more like sarsa!
    step: (A, Obs, R, S[R]) => (Obs, S[R])
) extends State[A, Obs, R, S] {

  private def updateForA(a: A, r: R): State[A, Obs, R, S] = {
    val (newObservation, newGen) = step(a, observation, r, rewards(a))
    MapState(
      newObservation,
      rewards.updated(a, newGen),
      penalty,
      step
    )
  }

  override def dynamics: Map[A, S[(R, State[A, Obs, R, S])]] =
    rewards.map {
      case (a, g) => (a, g.map(r => (r, updateForA(a, r))))
    }

  override val invalidMove = penalty.map((_, this))
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
  def static[A, R](
      actions: Set[A],
      penalty: Generator[R],
      gen: Generator[Generator[R]]
  ): Generator[StaticMapState[A, R, Generator]] =
    genMap(actions, gen).map(StaticMapState(_, penalty))

  /**
    * The second of two ways to construct a MapState.
    */
  def updating[A, Obs, R](
      actions: Set[A],
      initialObservation: Obs,
      penalty: Generator[R],
      gen: Generator[Generator[R]],
      step: (A, Obs, R, Generator[R]) => (Obs, Generator[R])
  ): Generator[MapState[A, Obs, R, Generator]] =
    genMap(actions, gen).map(MapState(initialObservation, _, penalty, step))
}
