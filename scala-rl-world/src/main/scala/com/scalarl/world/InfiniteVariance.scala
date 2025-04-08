/** Infinite variance world from Chapter 5. In this game, you can go left or right; if you go left,
  * you have some odds of winning or losing, and if you go right you immediately lose.
  */
package com.scalarl
package world

import com.scalarl.rainier.Categorical

sealed trait InfiniteVariance
    extends State[InfiniteVariance.View, InfiniteVariance.Move, Int, Cat] {
  override val invalidMove = Categorical.pure((0, this))
}

object InfiniteVariance {
  val startingState: InfiniteVariance = AliveState

  sealed trait Move extends Product with Serializable
  object Move {
    final case object Left extends Move
    final case object Right extends Move

    val all: Set[Move] = Set(Left, Right)
  }

  sealed trait View extends Product with Serializable
  object View {
    final case object Alive extends View
    final case object Dead extends View
  }

  object AliveState extends InfiniteVariance {
    override val observation = View.Alive
    override val dynamics: Map[Move, Cat[(Int, InfiniteVariance)]] = Map(
      Move.Left -> Categorical(
        Map(
          (0, AliveState) -> 0.1,
          (1, DeadState) -> 0.9
        )
      ),
      Move.Right -> Categorical.pure((0, DeadState))
    )
  }

  object DeadState extends InfiniteVariance {
    override val observation = InfiniteVariance.View.Dead
    override val dynamics = Map.empty
  }
}
