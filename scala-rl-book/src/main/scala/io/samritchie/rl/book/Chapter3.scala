package io.samritchie.rl
package book

import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.sampler.RNG
import io.samritchie.rl.policy.Random
import io.samritchie.rl.util.Grid
import io.samritchie.rl.world.{GridPolicy, GridWorld}

import scala.annotation.tailrec

/**
  * This chapter plays a couple of gridworld games.
  */
object Chapter3 {
  import io.samritchie.rl.util.Grid.{Bounds, Move, Position}

  implicit val rng: RNG = RNG.default
  implicit val evaluator: Numeric[Real] = new Evaluator(Map.empty)

  val gridConf = GridWorld
    .Config(Bounds(5, 5))
    .withJump(Position.of(0, 1), Position.of(4, 1), 10)
    .withJump(Position.of(0, 3), Position.of(2, 3), 5)

  val figureThreeTwoState: NowState[Move, Position, Double] =
    gridConf.buildUnsafe(Position.of(0, 0))

  val policy = Random.eval[Grid.Move, Double]

  @tailrec
  def loopWhile[A, B](init: A)(f: A => Either[A, B]): B =
    f(init) match {
      case Left(a)  => loopWhile(a)(f)
      case Right(b) => b
    }

  /**
    * This is Figure 3.2, with proper stopping conditions and
    * everything. Lots of work to go.
    *   */
  def figureThreeTwo: (Map[Position, Real], Long) = {
    val allowedIterations: Long = 10000
    val epsilon: Double = 1e-4
    val gamma: Double = 0.9

    loopWhile((Map.empty[Position, Real], 0)) {
      case (m, iterLeft) =>
        val newM = GridPolicy.evaluateSweep(
          policy,
          gridConf.stateSweep,
          m,
          gamma
        )
        if ((iterLeft >= allowedIterations) || ValueFunction.shouldWeStop(m, newM, epsilon))
          Right((newM, iterLeft))
        else
          Left((newM, iterLeft + 1))
    }
  }

  def main(items: Array[String]): Unit = {
    val (done, iterations) = figureThreeTwo

    gridConf.stateSweep.foreach { gw =>
      println(s"Position: ${gw.grid.position}, Value: ${done.getOrElse(gw.grid.position, Real.zero)}")
    }
    println(s"That took $iterations iterations, for the record.")
  }

}
