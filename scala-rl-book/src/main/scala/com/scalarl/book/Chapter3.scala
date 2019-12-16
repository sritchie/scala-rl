/**
  This chapter plays a couple of gridworld games. Current goal is to get this
  all building, and printing nicely.

  This chapter introduces the idea of the Markov Decision Process.
  */
package com.scalarl
package book

import cats.Id
import com.scalarl.algebra.ToDouble
import com.scalarl.logic.Sweep
import com.scalarl.plot.Tabulator
import com.scalarl.policy.Greedy
import com.scalarl.value.DecayState
import com.scalarl.world.GridWorld
import com.scalarl.world.util.Grid

object Chapter3 {
  import com.scalarl.world.util.Grid.{Bounds, Move, Position}

  // Configuration for the gridworld used in the examples.
  val gridConf = GridWorld
    .Config(Bounds(5, 5))
    .withJump(Position.of(0, 1), Position.of(4, 1), 10)
    .withJump(Position.of(0, 3), Position.of(2, 3), 5)

  val allowedIterations: Long = 10000
  val epsilon: Double = 1e-4
  val gamma: Double = 0.9

  val emptyFn = StateValueFn.empty[Position, DecayState[Double]](
    DecayState.DecayedValue(0.0)
  )

  def notConverging(iterations: Long, allowed: Long): Boolean =
    iterations >= allowed

  /**
    Note... this version, following the python code, checks that the sum of all
    differences is less than epsilon. In the next chapter we use the max
    function instead here to get this working, to check that the maximum delta
    is less than epsilon.
    */
  def valueFunctionConverged[Obs, T: ToDouble](
      l: StateValueFn[Obs, T],
      r: StateValueFn[Obs, T]
  ): Boolean = Sweep.diffBelow(l, r, epsilon)(_ + _)

  def shouldStop[Obs, T: ToDouble](
      l: StateValueFn[Obs, T],
      r: StateValueFn[Obs, T],
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

  def printFigure[T: ToDouble](
      conf: GridWorld.Config,
      pair: (StateValueFn[Position, T], Long),
      title: String
  ): Unit = {
    val (valueFn, iterations) = pair
    println(s"${title}:")
    println(Tabulator.format(toTable(conf, p => ToDouble[T].apply(valueFn.stateValue(p)))))
    println(s"That took $iterations iterations, for the record.")
  }

  /**
    * This is Figure 3.2, with proper stopping conditions and
    * everything. Lots of work to go.
    *   */
  def threeTwo: (StateValueFn[Position, DecayState[Double]], Long) =
    Sweep.sweepUntil[Position, Move, Double, DecayState[Double], Cat, Id](
      emptyFn,
      _ => Policy.random[Position, Move, Double, Id],
      DecayState.bellmanFn(gamma),
      gridConf.stateSweep,
      shouldStop _,
      inPlace = true,
      valueIteration = false
    )

  /**
    * This is Figure 3.5. This is currently working!
    */
  def threeFive: (StateValueFn[Position, DecayState[Double]], Long) = {
    implicit val dm = DecayState.decayStateModule(gamma)
    Sweep.sweepUntil[Position, Move, Double, DecayState[Double], Cat, Id](
      emptyFn,
      fn =>
        Greedy
          .Config[Double, DecayState[Double]](
            0.0,
            DecayState.Reward(_),
            (a, b) => DecayState.decayStateGroup[Double](gamma).plus(a, b),
            DecayState.DecayedValue(0.0)
          )
          .id(fn),
      DecayState.bellmanFn(gamma),
      gridConf.stateSweep,
      shouldStop _,
      inPlace = true,
      valueIteration = true
    )
  }

  /**
    * This currently works, and displays rough tables for each of the required
    * bits.
    */
  def main(items: Array[String]): Unit = {
    printFigure(gridConf, threeTwo, "Figure 3.2")
    printFigure(gridConf, threeFive, "Figure 3.5")
  }
}
