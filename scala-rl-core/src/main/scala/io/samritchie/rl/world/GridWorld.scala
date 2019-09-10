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
  type Reward = Double
  type Move = Grid.Move
  type Bounds = Grid.Bounds
  type Position = Grid.Position
  type JumpMap = Map[Position, (Position, Reward)]

  object Jumps {
    def empty: Jumps = Jumps(Map.empty)
  }
  case class Jumps(jumps: Map[Position, (Position, Reward)]) {
    def get(p: Position): Option[(Position, Reward)] = jumps.get(p)

    /**
      * TODO
      *
      * - validate that there are no cycles!
      * - validate that we're within the bounds for all endpoints!
      *
      */
    def validate(bounds: Bounds): Try[Jumps] = Success(this)
    def and(from: Position, to: Position, reward: Reward): Jumps =
      Jumps(jumps.updated(from, (to, reward)))
  }

  case class Config(
      bounds: Bounds,
      default: Reward = 0.0,
      penalty: Reward = -1.0,
      jumps: Jumps = Jumps.empty
  ) {
    def withJump(from: Position, to: Position, reward: Reward): Config =
      copy(jumps = jumps.and(from, to, reward))

    /**
      * Build by projecting a row or column outside of the specified
      * bounds onto the boundary.
      */
    def buildConfined(start: Position): GridWorld =
      buildUnsafe(start.confine(bounds))

    /**
      * Build, assuming that everything is legit!
      *
      * TODO remove hardcoded penalty,
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
    defaultReward: GridWorld.Reward,
    penalty: GridWorld.Reward,
    jumps: GridWorld.Jumps
) extends NowState[GridWorld.Move, Grid.Position, GridWorld.Reward] {
  import GridWorld._

  val observation: Position = grid.position

  def dynamics: Map[Move, Eval[(Reward, NowState[Move, Position, Reward])]] =
    Util.makeMap(Grid.Move.all)(m => Eval.later(actNow(m)))

  /**
    * This is the NON-monadic action, since we can do it
    * immediately. The dynamics are where it all gets passed down to the user.
    *
    * There is still a wall, though! The user can't look ahead. If you
    * CAN look ahead, and don't hide it behind a delay, then boom, we
    * have the ability to do the checkers example.
    */
  def actNow(move: Move): (Reward, NowState[Move, Position, Reward]) =
    grid
      .move(move)
      .map(runJumps(_))
      .getOrElse((penalty, this))

  /**
    * Executes any jumps that may exist in this world.
    */
  private def runJumps(newGrid: Grid): (Reward, GridWorld) = {
    val (r, g) = jumps.get(newGrid.position) match {
      case None =>
        (defaultReward, newGrid)
      case Some((newPosition, reward)) =>
        (reward, grid.teleportUnsafe(newPosition))
    }
    (r, copy(grid = g))
  }

  /**
    * Draws a plot.
    *
    *
    def draw_image(image):
    fig, ax = plt.subplots()
    ax.set_axis_off()
    tb = Table(ax, bbox=[0, 0, 1, 1])

    nrows, ncols = image.shape
    width, height = 1.0 / ncols, 1.0 / nrows

    # Add cells
    for (i, j), val in np.ndenumerate(image):
        tb.add_cell(i, j, width, height, text=val,
                    loc='center', facecolor='white')

    # Row and column labels...
    for i in range(len(image)):
        tb.add_cell(i, -1, width, height, text=i+1, loc='right',
                    edgecolor='none', facecolor='none')
        tb.add_cell(-1, i, width, height/2, text=i+1, loc='center',
                    edgecolor='none', facecolor='none')

    ax.add_table(tb)
    */
  def render(): Unit = {
    println("For now, we'll just print a string...")
    println("Rendered!")
  }

}
