package io.samritchie.rl
package book

import io.samritchie.rl.world.GridWorld

/**
  * This chapter plays a couple of gridworld games.
  */
object Chapter3 {
  import GridWorld.{Move, Position, Reward}

  val figureThreeTwoState: State[Move, Reward] =
    GridWorld(5, 5)
      .withJump(Position(0, 1), Position(4, 1), 10)
      .withJump(Position(0, 3), Position(2, 3), 5)
      .toState

  def main(items: Array[String]): Unit = println(
    "Hello, chapter 3!"
  )
}
