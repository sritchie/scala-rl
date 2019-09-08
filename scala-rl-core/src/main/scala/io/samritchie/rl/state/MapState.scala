/**
  * A MapState is a particular kind of state.
  */
package io.samritchie.rl
package state

import com.stripe.rainier.core.Generator

/**
  * MapState that doesn't evolve.
  */
case class StaticMapState[A, R](
    rewards: Map[A, Generator[R]]
) extends State[A, R] {
  override lazy val dynamics = rewards.mapValues(_.map(r => (r, this)))
}

/**
  * MDP with a single state.
  */
case class MapState[A, R](
    rewards: Map[A, Generator[R]],
    step: (A, R, Generator[R]) => Generator[R]
) extends State[A, R] {

  private def updateForA(a: A, r: R): State[A, R] =
    MapState(
      rewards.updated(a, step(a, r, rewards(a))),
      step
    )

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
  def fromSet[A, R](
      actions: Set[A],
      gen: Generator[Generator[R]]
  ): Generator[State[A, R]] =
    genMap(actions, gen).map(StaticMapState[A, R](_))

    /**
    * The second of two ways to construct a MapState.
    */
  def fromSet[A, R](
      actions: Set[A],
      gen: Generator[Generator[R]],
      step: (A, R, Generator[R]) => Generator[R]
  ): Generator[State[A, R]] =
    genMap(actions, gen).map(MapState[A, R](_, step))

}
