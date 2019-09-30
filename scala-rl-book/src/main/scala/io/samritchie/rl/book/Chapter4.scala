/**
  Dynamic programming chapter. This should have similar examples to what we
  generated for Chapter 3.
  */
package io.samritchie.rl
package book

import cats.Id
import com.stripe.rainier.core.Categorical
import io.samritchie.rl.policy.Random
import io.samritchie.rl.world.GridWorld

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
  ): Boolean =
    Chapter3.notConverging(iterations, allowedIterations) ||
      ValueFunction.diff(l, r, epsilon)(_.max(_))

  def fourOne(inPlace: Boolean): (ValueFunction[Position, Categorical, Id], Long) =
    ValueFunction.sweepUntil[Move, Position, Double, Categorical, Id](
      Random.id[Move, Double],
      emptyFn,
      gridConf.stateSweep,
      shouldStop _,
      inPlace = inPlace
    )

  def main(items: Array[String]): Unit = {
    println("Hello, chapter 4!")
    Chapter3.printFigure(gridConf, fourOne(true), "Figure 4.1 (in-place)")
    Chapter3.printFigure(gridConf, fourOne(false), "Figure 4.1 (not in-place)")

    // I think we'd only go three iterations if we had stopped when the policy
    // was stable.
  }
}
