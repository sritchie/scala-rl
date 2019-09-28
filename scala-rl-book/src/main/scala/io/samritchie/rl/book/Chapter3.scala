package io.samritchie.rl
package book

import com.stripe.rainier.compute.Real
import io.samritchie.rl.plot.Tabulator
import io.samritchie.rl.policy.{Greedy, Random}
import io.samritchie.rl.util.Grid
import io.samritchie.rl.world.GridWorld

/**
  * This chapter plays a couple of gridworld games. Current goal is to get this
  * all building, and printing nicely.
  */
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

  def hasConverged[Obs](
      l: ValueFunction[Obs],
      r: ValueFunction[Obs],
      iterations: Long
  ): Boolean =
    (iterations >= allowedIterations) ||
      ValueFunction.valuesWithin(l, r, epsilon)

  def toTable(conf: GridWorld.Config, f: Position => Real): Iterable[Iterable[Real]] =
    Grid
      .allStates(conf.bounds)
      .map(g => f(g.position))
      .toArray
      .grouped(conf.bounds.numRows)
      .toSeq
      .map(_.toSeq)

  /**
    * This is Figure 3.2, with proper stopping conditions and
    * everything. Lots of work to go.
    *   */
  def figureThreeTwo(): Unit = {
    val (valueFn, iterations) =
      ValueFunction.sweepUntil[Move, Position, Double](
        Random.id[Move, Double],
        emptyFn,
        gridConf.stateSweep,
        hasConverged _
      )

    // Display stats for threeTwo...
    println("Figure 3.2:")
    println(Tabulator.format(toTable(gridConf, valueFn.stateValue(_).get)))
    println(s"That took $iterations iterations, for the record.")
  }

  /**
    * This is Figure 3.5. This is currently working!
    */
  def figureThreeFive(): Unit = {
    val (valueFn, iterations) =
      ValueFunction.sweepUntil[Move, Position, Double](
        Greedy[Move, Position, Double](emptyFn),
        emptyFn,
        gridConf.stateSweep,
        hasConverged _
      )

    // Display stats for threeFive...
    println("Figure 3.5:")
    println(Tabulator.format(toTable(gridConf, valueFn.stateValue(_).get)))
    println(s"That took $iterations iterations, for the record.")

  }

  /**
    * This currently works, and displays rough tables for each of the required
    * bits.
    */
  def main(items: Array[String]): Unit = {
    figureThreeTwo()
    figureThreeFive()
  }
}
