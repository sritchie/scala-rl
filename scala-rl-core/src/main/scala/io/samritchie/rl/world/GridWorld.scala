/**
  * Gridworld implementation, based on what we need from Chapter 3. I
  * think this can get much more complicated, especially once
  * observations come into the picture.
  */
package io.samritchie.rl
package world

import cats.Eval
import io.samritchie.rl.util.Grid
import scala.util.{Success, Try}

object GridWorld {
  import Grid.{Bounds, Position}

  object Jumps {
    def empty: Jumps = Jumps(Map.empty)
  }

  case class Jumps(jumps: Map[Position, (Position, Double)]) {
    def get(p: Position): Option[(Position, Double)] = jumps.get(p)

    /**
      * TODO
      *
      * - validate that there are no cycles!
      * - validate that we're within the bounds for all endpoints!
      *
      */
    def validate(bounds: Bounds): Try[Jumps] = Success(this)
    def and(from: Position, to: Position, reward: Double): Jumps =
      Jumps(jumps.updated(from, (to, reward)))
  }

  case class Config(
      bounds: Bounds,
      default: Double = 0.0,
      penalty: Double = -1.0,
      jumps: Jumps = Jumps.empty
  ) {
    def withJump(from: Position, to: Position, reward: Double): Config =
      copy(jumps = jumps.and(from, to, reward))

    /**
      * Build by projecting a row or column outside of the specified
      * bounds onto the boundary.
      */
    def buildConfined(start: Position): GridWorld =
      buildUnsafe(start.confine(bounds))

    /**
      * Build, assuming that everything is legit!
      */
    def buildUnsafe(start: Position): GridWorld =
      GridWorld(Grid(start, bounds), default, penalty, jumps)

    /**
      * Returns a Try that's successful if supplied position is within
      * bounds, false otherwise.
      */
    def build(start: Position): Try[GridWorld] =
      start.assertWithin(bounds).map(buildUnsafe(_))

    def stateSweep: Traversable[GridWorld] =
      Grid.allStates(bounds).map {
        GridWorld(_, default, penalty, jumps)
      }
  }
}

/**
  * TODO - redo this to store dynamics ALL OVER, so we don't have to
  * recalculate them? lazily build up the map... but don't REPLACE
  * once it's there? That should slightly speed us up.
  */
case class GridWorld(
    grid: Grid,
    defaultReward: Double,
    penalty: Double,
    jumps: GridWorld.Jumps
) extends NowState[Grid.Move, Grid.Position, Double] {
  import Grid.{Move, Position}

  val observation: Position = grid.position

  def dynamics: Map[Move, Eval[(Double, NowState[Move, Position, Double])]] =
    Util.makeMap(Grid.Move.all)(m => Eval.later(actNow(m)))

  /**
    * This is the NON-monadic action, since we can do it
    * immediately. The dynamics are where it all gets passed down to the user.
    *
    * There is still a wall, though! The user can't look ahead. If you
    * CAN look ahead, and don't hide it behind a delay, then boom, we
    * have the ability to do the checkers example.
    */
  def actNow(move: Move): (Double, NowState[Move, Position, Double]) =
    jumps.get(grid.position) match {
      case None =>
        grid
          .move(move)
          .map(g => (defaultReward, copy(grid = g)))
          .getOrElse((penalty, this))
      case Some((newPosition, reward)) =>
        (reward, copy(grid = grid.teleportUnsafe(newPosition)))
    }
}
