/**
  Dynamic programming chapter. This should have similar examples to what we
  generated for Chapter 3.
  */
package io.samritchie.rl
package book

import cats.Id
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
      iterations: Long,
      verbose: Boolean = false
  ): Boolean = {
    if (verbose)
      println(
        s"Max diff seen: ${Util.diff[Obs]((l.seen ++ r.seen), l.stateValue(_).get, r.stateValue(_).get, _.max(_))}"
      )
    Chapter3.notConverging(iterations, allowedIterations) ||
    ValueFunction.diff(l, r, epsilon)(_.max(_))
  }

  def fourOne(inPlace: Boolean): (ValueFunction[Position, Cat, Id], Long) =
    ValueFunction.sweepUntil[Move, Position, Double, Cat, Id](
      emptyFn,
      _ => Random.id[Move, Double],
      gridConf.stateSweep,
      shouldStop(_, _, _),
      inPlace = inPlace
    )

  def fourTwo(
      inPlace: Boolean,
      valueIteration: Boolean = false
  ): (ValueFunction[CarRental.InvPair, Cat, Cat], Long) = {
    import CarRental.{ConstantConfig, PoissonConfig}
    import Cat.Poisson.Lambda

    val locationA = CarRental.Location(
      PoissonConfig(11, Lambda(3)),
      ConstantConfig(3),
      maxCars = 20
    )
    val locationB = CarRental.Location(
      PoissonConfig(11, Lambda(4)),
      ConstantConfig(2),
      maxCars = 20
    )
    val config = CarRental.Config.apply(
      aConfig = locationA,
      bConfig = locationB,
      maxMoves = CarRental.Move(5),
      rentalCredit = 10,
      moveCost = 2
    )

    val sweep = config.stateSweep
    val gamma = 0.9
    val zeroValue = value.Decaying(0.0, gamma)
    val empty = value.Bellman[CarRental.InvPair, Cat, Cat](
      Map.empty,
      zeroValue
    )

    // Build a Stochastic version of the greedy policy.
    val stochasticConf = policy.Greedy.Config[Double](0.0, zeroValue)

    // This simulates a version that does NOT update itself.
    val p = ValueFunction.sweepUntil[CarRental.Move, CarRental.InvPair, Double, Cat, Cat](
      empty,
      _ => stochasticConf.stochastic[CarRental.Move, CarRental.InvPair](empty),
      sweep,
      shouldStop(_, _, _, true),
      inPlace = inPlace,
      valueIteration = valueIteration
    )
    println(
      s"""Stable? ${ValueFunction.isPolicyStable(
        empty,
        p._1,
        zeroValue,
        sweep
      )}"""
    )
    val p2 = ValueFunction.sweepUntil[CarRental.Move, CarRental.InvPair, Double, Cat, Cat](
      p._1,
      _ => stochasticConf.stochastic[CarRental.Move, CarRental.InvPair](p._1),
      sweep,
      shouldStop(_, _, _, true),
      inPlace = inPlace
    )
    p2
  }

  def main(items: Array[String]): Unit = {
    println("Hello, chapter 4!")
    // Chapter3.printFigure(gridConf, fourOne(true), "Figure 4.1 (in-place)")
    // Chapter3.printFigure(gridConf, fourOne(false), "Figure 4.1 (not in-place)")

    fourTwo(true)
    // I think we'd only go three iterations if we had stopped when the policy
    // was stable.
  }
}
