/**
  This chapter plays a couple of gridworld games. Current goal is to get this
  all building, and printing nicely.

  This chapter introduces the idea of the Markov Decision Process.
  */
package io.samritchie.rl
package book

import com.stripe.rainier.compute.Real
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
  val emptyFn = ValueFunction.decaying[Position](gamma)

  def notConverging(iterations: Long): Boolean = iterations >= allowedIterations

  /**
    Note... this version, following the python code, checks that the sum of all
    differences is less than epsilon. In the next chapter we use the max
    function instead here to get this working, to check that the maximum delta
    is less than epsilon.
    */
  def valueFunctionConverged[Obs](
      l: ValueFunction[Obs],
      r: ValueFunction[Obs]
  ): Boolean = ValueFunction.diff(l, r, epsilon)(_ + _)

  def shouldStop[Obs](
      l: ValueFunction[Obs],
      r: ValueFunction[Obs],
      iterations: Long
  ): Boolean =
    notConverging(iterations) || valueFunctionConverged(l, r)

  def toTable(conf: GridWorld.Config, f: Position => Real): Iterable[Iterable[Real]] =
    Grid
      .allStates(conf.bounds)
      .map(g => f(g.position))
      .toArray
      .grouped(conf.bounds.numRows)
      .toSeq
      .map(_.toSeq)

  def printFigure(pair: (ValueFunction[Position], Long), title: String): Unit = {
    val (valueFn, iterations) = pair
    println(s"${title}:")
    println(Tabulator.format(toTable(gridConf, valueFn.stateValue(_).get)))
    println(s"That took $iterations iterations, for the record.")
  }

  /**
    * This is Figure 3.2, with proper stopping conditions and
    * everything. Lots of work to go.
    *   */
  def threeTwo: (ValueFunction[Position], Long) =
    ValueFunction.sweepUntil[Move, Position, Double](
      Random.id[Move, Double],
      emptyFn,
      gridConf.stateSweep,
      shouldStop _,
      inPlace = true
    )

  /**
    * This is Figure 3.5. This is currently working!
    */
  def threeFive: (ValueFunction[Position], Long) =
    ValueFunction.sweepUntil[Move, Position, Double](
      Greedy[Move, Position, Double](emptyFn),
      emptyFn,
      gridConf.stateSweep,
      shouldStop _,
      inPlace = true
    )

  /**
    * This currently works, and displays rough tables for each of the required
    * bits.
    */
  def main(items: Array[String]): Unit = {
    printFigure(threeTwo, "Figure 3.2")
    printFigure(threeFive, "Figure 3.5")
  }
}
