/**
  Dynamic programming chapter. This should have similar examples to what we
  generated for Chapter 3.
  */
package io.samritchie.rl
package book

import cats.Id
import io.samritchie.rl.logic.Sweep
import io.samritchie.rl.plot.Plot
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
  val emptyFn = StateValueFn.decaying[Position](gamma)

  def shouldStop[Obs](
      l: StateValueFn[Obs],
      r: StateValueFn[Obs],
      iterations: Long,
      verbose: Boolean = false
  ): Boolean = {
    if (verbose)
      println(
        s"Max diff seen: ${Util.diff[Obs]((l.seen ++ r.seen), l.stateValue(_).get, r.stateValue(_).get, _.max(_))}"
      )
    Chapter3.notConverging(iterations, allowedIterations) ||
    StateValueFn.diffBelow(l, r, epsilon)(_.max(_))
  }

  val defaultVal = value.Decaying(0.0, gamma)

  def fourOne(inPlace: Boolean): (StateValueFn[Position], Long) =
    Sweep.sweepUntil[Position, Move, Double, Cat, Id](
      emptyFn,
      _ => Random.id[Position, Move, Double],
      (fn, p) => Estimator.bellman(fn, p, defaultVal, defaultVal),
      gridConf.stateSweep,
      shouldStop(_, _, _),
      inPlace,
      valueIteration = false
    )

  def fourTwo(inPlace: Boolean): (StateValueFn[CarRental.InvPair], CarRental.Config, Long) = {
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
    val empty = StateValueFn[CarRental.InvPair](zeroValue)

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
    val (roundOne, _) = Sweep.sweepUntil[CarRental.InvPair, CarRental.Move, Double, Cat, Cat](
      empty,
      _ => stochasticConf.stochastic(empty),
      (fn, p) => Estimator.bellman(fn, p, zeroValue, zeroValue),
      sweep,
      shouldStop(_, _, _, true),
      inPlace,
      valueIteration = false
    )
    println(
      s"""Stable? ${StateValueFn.isPolicyStable(
        empty,
        roundOne,
        zeroValue,
        sweep
      )}"""
    )
    val (vf, iter) = Sweep.sweepUntil[CarRental.InvPair, CarRental.Move, Double, Cat, Cat](
      roundOne,
      _ => stochasticConf.stochastic(roundOne),
      (fn, p) => Estimator.bellman(fn, p, zeroValue, zeroValue),
      sweep,
      shouldStop(_, _, _, true),
      inPlace,
      valueIteration = false
    )
    (vf, config, iter)
  }

  /**
    This currently is not great because we don't have a way of automatically
    binning the data and generating that graph. This is custom.
    */
  def vfToSeqPoints(vf: StateValueFn[CarRental.InvPair]): Seq[Seq[Double]] =
    (0 to 20).map { row =>
      (0 to 20).map { col =>
        vf.stateValue((CarRental.Inventory(row, 20), CarRental.Inventory(col, 20))).get
      }.toSeq
    }.toSeq

  def figureFourOne(): Unit = {
    Chapter3.printFigure(gridConf, fourOne(true), "Figure 4.1 (in-place)")
    Chapter3.printFigure(gridConf, fourOne(false), "Figure 4.1 (not in-place)")
    ()
  }

  /**
    I'm leaving this in a nightmare state for now. To finish this out, we really need to:

    - add support for policy evaluation and policy stability checks, alternating.
    - come up with some way of actually turning a particular policy's decisions into a heat map that's not so hardcoded
    - NOT have the graph library explode when I cancel a run, for Heatmap.
    */
  def runCarRental(): Unit = {
    val (vf, config, _) = fourTwo(true)
    val zero = value.Decaying(0.0, 0.9)

    val estimator: Estimator.ActionValue[CarRental.InvPair, CarRental.Move, Double, Cat] =
      Estimator.oneAhead(vf, zero)

    val dataMap = config.stateSweep.foldLeft(Map.empty[CarRental.InvPair, Int]) { (acc, state) =>
      acc.updated(
        state.observation,
        estimator.greedyOptions(state).head.n
      )
    }
    val inputs = (0 to 20).map { row =>
      (0 to 20).map { col =>
        dataMap((CarRental.Inventory(row, 20), CarRental.Inventory(col, 20))).toDouble
      }.toSeq
    }.toSeq

    // The default color palette doesn't have enough colors to properly
    // represent things here.
    Plot.heatMap(inputs, 20)
  }
  def main(items: Array[String]): Unit = {
    println("Hello, chapter 4!")
    runCarRental()
    ()
  }
}
