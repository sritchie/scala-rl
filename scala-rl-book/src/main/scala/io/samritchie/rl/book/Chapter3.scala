package io.samritchie.rl
package book

import com.stripe.rainier.compute.Real
import io.samritchie.rl.policy.Random
import io.samritchie.rl.util.Grid
import io.samritchie.rl.world.GridWorld

/**
  * This chapter plays a couple of gridworld games.
  */
object Chapter3 {
  import io.samritchie.rl.util.Grid.{Bounds, Move, Position}

  val gridConf = GridWorld
    .Config(Bounds(5, 5))
    .withJump(Position.of(0, 1), Position.of(4, 1), 10)
    .withJump(Position.of(0, 3), Position.of(2, 3), 5)

  val figureThreeTwoState: NowState[Move, Position, Double] =
    gridConf.buildUnsafe(Position.of(0, 0))

  val policy = Random.eval[Grid.Move, Double]

  /**
    * This is Figure 3.2, with proper stopping conditions and
    * everything. Lots of work to go.
    *   */
  def figureThreeTwo: (StateValue[Position], Long) = {
    val allowedIterations: Long = 10000
    val epsilon: Double = 1e-4
    val gamma: Double = 0.9

    Util.loopWhile((ValueFunction.emptyState[Position], 0)) {
      case (m, iterLeft) =>
        val newM = ValueFunction.evaluateSweep(
          policy,
          gridConf.stateSweep,
          m,
          gamma
        )
        if ((iterLeft >= allowedIterations) || newM.shouldHalt(m, epsilon))
          Right((newM, iterLeft))
        else
          Left((newM, iterLeft + 1))
    }
  }

  def main(items: Array[String]): Unit = {
    val (doneStateValue, iterations) = figureThreeTwo
    val done = doneStateValue.m

    gridConf.stateSweep.foreach { gw =>
      println(s"Position: ${gw.grid.position}, Value: ${done.getOrElse(gw.grid.position, Real.zero)}")
    }
    println(s"That took $iterations iterations, for the record.")
  }

}
