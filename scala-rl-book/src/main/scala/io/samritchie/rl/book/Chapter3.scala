/**
  This chapter plays a couple of gridworld games. Current goal is to get this
  all building, and printing nicely.

  This chapter introduces the idea of the Markov Decision Process.
  */
package io.samritchie.rl
package book

import cats.Id
import io.samritchie.rl.logic.Sweep
import io.samritchie.rl.plot.Tabulator
import io.samritchie.rl.policy.{Greedy, Random}
import io.samritchie.rl.util.Grid
import io.samritchie.rl.world.GridWorld

object Chapter3 {
  import io.samritchie.rl.util.Grid.{Bounds, Move, Position}

  // Configuration for the gridworld used in the examples.
  val gridConf = GridWorld
    .Config(Bounds(5, 5))
    .withJump(Position.of(0, 1), Position.of(4, 1), 10)
    .withJump(Position.of(0, 3), Position.of(2, 3), 5)

  val allowedIterations: Long = 10000
  val epsilon: Double = 1e-4
  val gamma: Double = 0.9
  val emptyFn = StateValueFn.decaying[Position](gamma)
  val zero = value.Decaying(0.0, gamma)

  def notConverging(iterations: Long, allowed: Long): Boolean =
    iterations >= allowed

  /**
    Note... this version, following the python code, checks that the sum of all
    differences is less than epsilon. In the next chapter we use the max
    function instead here to get this working, to check that the maximum delta
    is less than epsilon.
    */
  def valueFunctionConverged[Obs](
      l: StateValueFn[Obs],
      r: StateValueFn[Obs]
  ): Boolean = StateValueFn.diffBelow(l, r, epsilon)(_ + _)

  def shouldStop[Obs](
      l: StateValueFn[Obs],
      r: StateValueFn[Obs],
      iterations: Long
  ): Boolean =
    notConverging(iterations, allowedIterations) ||
      valueFunctionConverged(l, r)

  def toTable(conf: GridWorld.Config, f: Position => Double): Iterable[Iterable[Double]] =
    Grid
      .allStates(conf.bounds)
      .map(g => f(g.position))
      .toArray
      .grouped(conf.bounds.numRows)
      .toSeq
      .map(_.toSeq)

  def printFigure(
      conf: GridWorld.Config,
      pair: (StateValueFn[Position], Long),
      title: String
  ): Unit = {
    val (valueFn, iterations) = pair
    println(s"${title}:")
    println(Tabulator.format(toTable(conf, valueFn.stateValue(_).get)))
    println(s"That took $iterations iterations, for the record.")
  }

  /**
    * This is Figure 3.2, with proper stopping conditions and
    * everything. Lots of work to go.
    *   */
  def threeTwo: (StateValueFn[Position], Long) =
    Sweep.sweepUntil(
      emptyFn,
      _ => Random.id[Position, Move, Double],
      (fn: StateValueFn[Position], p: Policy[Position, Move, Double, Cat, Id]) =>
        Evaluator.bellman(fn, p, zero, zero),
      gridConf.stateSweep,
      shouldStop _,
      inPlace = true,
      valueIteration = false
    )

  /**
    * This is Figure 3.5. This is currently working!
    */
  def threeFive: (StateValueFn[Position], Long) =
    Sweep.sweepUntil[Position, Move, Double, Cat, Id](
      emptyFn,
      fn => Greedy.Config[Double](0.0, zero).id(fn),
      (fn, p) => Evaluator.bellman(fn, p, zero, zero),
      gridConf.stateSweep,
      shouldStop _,
      inPlace = true,
      valueIteration = true
    )

  /**
    * This currently works, and displays rough tables for each of the required
    * bits.
    */
  def main(items: Array[String]): Unit = {
    printFigure(gridConf, threeTwo, "Figure 3.2")
    printFigure(gridConf, threeFive, "Figure 3.5")
  }
}
