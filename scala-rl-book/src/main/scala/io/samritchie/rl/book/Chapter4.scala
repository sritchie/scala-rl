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
      inPlace,
      valueIteration = false
    )

  def fourTwo(inPlace: Boolean): (ValueFunction[CarRental.InvPair, Cat, Cat], Long) = {
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

    /**
      The big differences from the book version are:

      - Currently our Poisson distribution normalizes over the allowed values,
        rather than just truncating the chance of a value greater than the max
        to zero.
      - our Greedy policy randomly chooses from the entire greedy set, vs just
        choosing the "first" thing, like Numpy does.

      The Python version also keeps an actual greedy policy, which means that
      the policy starts by returning 0 no matter what, by design, instead of by
      acting as a random policy until it knows any better.

      Without that the generated values match.

      TODO ALSO... currently, the sweepUntil function only supports
      valueIteration or updating on every single sweep. The book actually wants
      to do a full round of policy evaluation before doing any policy
      improvement.

      We need to support that.

      */
    val (roundOne, _) = ValueFunction.sweepUntil[CarRental.Move, CarRental.InvPair, Double, Cat, Cat](
      empty,
      _ => stochasticConf.stochastic(empty),
      sweep,
      shouldStop(_, _, _, true),
      inPlace,
      valueIteration = false
    )
    println(
      s"""Stable? ${ValueFunction.isPolicyStable(
        empty,
        roundOne,
        zeroValue,
        sweep
      )}"""
    )
    ValueFunction.sweepUntil[CarRental.Move, CarRental.InvPair, Double, Cat, Cat](
      roundOne,
      _ => stochasticConf.stochastic(roundOne),
      sweep,
      shouldStop(_, _, _, true),
      inPlace,
      valueIteration = false
    )
  }

  def main(items: Array[String]): Unit = {
    println("Hello, chapter 4!")
    //Chapter3.printFigure(gridConf, fourOne(true), "Figure 4.1 (in-place)")
    //Chapter3.printFigure(gridConf, fourOne(false), "Figure 4.1 (not in-place)")
    fourTwo(true)
  }
}
