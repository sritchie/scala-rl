/**
  * A bandit is a particular kind of state.
  */
package io.samritchie.rl
package state

import com.stripe.rainier.core.Generator

object Bandit {
  object Arm {
    implicit val ordering: Ordering[Arm] = Ordering.by(_.i)
  }

  case class Arm(i: Int)

  /**
    * An "Arm" is something that takes you to a new state. We just
    * happen to have only a single state here, so it always takes you
    * back to a given "bandit" problem.
    */
  def arms(k: Int): Set[Arm] = (0 until k).map(Arm(_)).toSet

  /**
    * Returns a Generator that splits out states for each of the games
    * to play.
    */
  def stationary(
      nArms: Int,
      gen: Generator[Generator[Double]]
  ): Generator[State[Arm, Unit, Double]] =
    MapState.static(arms(nArms), gen)

  /**
    * Returns a Generator that splits out states for each of the games
    * to play. This generator evolves in a non-stationary way.
    *
    * The set below is totally fucked... it's returning a SINGLE
    * generator each time, not the good stuff that we need.
    *
    */
  def nonStationary(
      nArms: Int,
      gen: Generator[Generator[Double]],
      updater: (Arm, Double, Generator[Double]) => Generator[Double]
  ): Generator[State[Arm, Unit, Double]] =
    MapState.updating[Arm, Unit, Double](arms(nArms), (), gen, { (a, obs, r, gen) =>
      ((), updater(a, r, gen))
    })
}
