package io.samritchie.rl
package book

import io.samritchie.rl.util.Grid.{Bounds, Position}
import io.samritchie.rl.world.GridWorld

/**
  * This chapter plays a couple of gridworld games.
  */
object Chapter3 {
  import GridWorld._

  val figureThreeTwoState: State[Move, Reward] =
    GridWorld
      .config(Bounds(5, 5))
      .withJump(Position.of(0, 1), Position.of(4, 1), 10)
      .withJump(Position.of(0, 3), Position.of(2, 3), 5)
      .buildUnsafe(Position.of(0, 0))

  def main(items: Array[String]): Unit = println(
    "Hello, chapter 3!"
  )
}
