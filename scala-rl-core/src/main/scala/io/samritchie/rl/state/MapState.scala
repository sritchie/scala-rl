/**
  * A MapState is a particular kind of state.
  *
  * TODO - maybe get a way to do observations in here?
  */
package io.samritchie.rl
package state

import com.stripe.rainier.core.Generator

/**
  * MapState that doesn't evolve.
  */
case class StaticMapState[A, R](
    rewards: Map[A, Generator[R]]
) extends State[A, Unit, R, Generator] {
  override val observation: Unit = ()
  override lazy val dynamics = rewards.mapValues(_.map(r => (r, this)))
}

/**
  * MDP with a single state.
  */
case class MapState[A, Obs, R](
    observation: Obs,
    rewards: Map[A, Generator[R]],
    step: (A, Obs, R, Generator[R]) => (Obs, Generator[R])
) extends State[A, Obs, R, Generator] {

  private def updateForA(a: A, r: R): State[A, Obs, R, Generator] = {
    val (newObservation, newGen) = step(a, observation, r, rewards(a))
    MapState(
      newObservation,
      rewards.updated(a, newGen),
      step
    )
  }

  override lazy val dynamics = rewards.map {
    case (a, g) => (a, g.map(r => (r, updateForA(a, r))))
  }
}

/**
  * TODO - make Generator contravariant, and change these back to
  * returning MapState.
  */
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
  ): Generator[State[A, Unit, R, Generator]] =
    genMap(actions, gen).map(StaticMapState[A, R](_))

  /**
    * The second of two ways to construct a MapState.
    */
  def updating[A, Obs, R](
      actions: Set[A],
      initialObservation: Obs,
      gen: Generator[Generator[R]],
      step: (A, Obs, R, Generator[R]) => (Obs, Generator[R])
  ): Generator[State[A, Obs, R, Generator]] =
    genMap(actions, gen).map { m =>
      MapState[A, Obs, R](initialObservation, m, step)
    }
}
