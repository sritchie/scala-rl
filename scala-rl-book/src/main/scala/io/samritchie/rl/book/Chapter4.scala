/**
  Dynamic programming chapter. This should have similar examples to what we
  generated for Chapter 3.
  */
package io.samritchie.rl
package book

import cats.Id
import com.stripe.rainier.core.Categorical
import io.samritchie.rl.policy.Random
import io.samritchie.rl.world.{CarRental, GridWorld}

object Chapter4 {
  import io.samritchie.rl.util.Grid.{Bounds, Move, Position}

  val gridConf = GridWorld
    .Config(Bounds(4, 4), default = -1.0, penalty = -1.0)
    .withTerminalState(Position.of(0, 0))
    .withTerminalState(Position.of(3, 3))

  val allowedIterations: Long = 10000
  val gamma: Double = 1.0
  val epsilon: Double = 1e-4
  val emptyFn = ValueFunction.decaying[Position](gamma)

  def shouldStop[Obs, M[_], S[_]](
      l: ValueFunction[Obs, M, S],
      r: ValueFunction[Obs, M, S],
      iterations: Long
  ): Boolean = {
    println(
      s"Max diff seen: ${Util.diff[Obs]((l.seen ++ r.seen), l.stateValue(_).get, r.stateValue(_).get, _.max(_))}"
    )
    Chapter3.notConverging(iterations, allowedIterations) ||
    ValueFunction.diff(l, r, epsilon)(_.max(_))
  }

  def fourOne(inPlace: Boolean): (ValueFunction[Position, Categorical, Id], Long) =
    ValueFunction.sweepUntil[Move, Position, Double, Categorical, Id](
      Random.id[Move, Double],
      emptyFn,
      gridConf.stateSweep,
      shouldStop _,
      inPlace = inPlace
    )

  def fourTwo(inPlace: Boolean): (ValueFunction[CarRental.InvPair, Categorical, Categorical], Long) = {
    import CarRental.PoissonConfig
    import Util.Poisson.Lambda

    val locationA = CarRental.Location(
      PoissonConfig(11, Lambda(3)),
      PoissonConfig(11, Lambda(3)),
      maxCars = 2
    )
    val locationB = CarRental.Location(
      PoissonConfig(11, Lambda(4)),
      PoissonConfig(11, Lambda(2)),
      maxCars = 2
    )
    val config = CarRental.Config.apply(
      aConfig = locationA,
      bConfig = locationB,
      maxMoves = CarRental.Move(1),
      rentalCredit = 10,
      moveCost = 2
    )
    val sweep = config.stateSweep
    val gamma = 0.9
    val zeroValue = value.Decaying(0.0, gamma)
    val empty = value.Bellman[CarRental.InvPair](
      Map.empty,
      zeroValue
    )

    // Build a Stochastic version of the greedy policy.
    val stochasticGreedy: CategoricalPolicy[CarRental.Move, CarRental.InvPair, Double, Categorical] =
      policy.Greedy
        .Config[Double](0.0, zeroValue)
        .stochastic(empty)

    ValueFunction.sweepUntil[CarRental.Move, CarRental.InvPair, Double, Categorical, Categorical](
      stochasticGreedy,
      empty,
      sweep,
      shouldStop _,
      inPlace = inPlace
    )
  }

  def main(items: Array[String]): Unit = {
    println("Hello, chapter 4!")
    // Chapter3.printFigure(gridConf, fourOne(true), "Figure 4.1 (in-place)")
    // Chapter3.printFigure(gridConf, fourOne(false), "Figure 4.1 (not in-place)")

    println(fourTwo(true))
    // I think we'd only go three iterations if we had stopped when the policy
    // was stable.
  }
}
