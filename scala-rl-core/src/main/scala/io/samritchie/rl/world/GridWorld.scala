/**
  * Gridworld implementation, based on what we need from Chapter 3. I
  * think this can get much more complicated, especially once
  * observations come into the picture.
  */
package io.samritchie.rl
package world

import com.stripe.rainier.core.Generator
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
  }
}

case class GridWorld(
    grid: Grid,
    defaultReward: GridWorld.Reward,
    penalty: GridWorld.Reward,
    jumps: GridWorld.Jumps
) extends State[GridWorld.Move, Grid.Position, GridWorld.Reward] {
  import GridWorld._

  val observation: Position = grid.position

  def dynamics: Map[Move, Generator[(Reward, State[Move, Position, Reward])]] =
    Util.makeMap(Grid.Move.all)(m => Util.delayedGenerator(actNow(m)))

  /**
    * This is the NON-monadic action, since we can do it
    * immediately. The dynamics are where it all gets passed down to the user.
    *
    * TODO - should this NOT be a stochastic policy, since we really
    * don't care here and are just using the constant generator?
    *
    * There is still a wall, though! The user can't look ahead. If you
    * CAN look ahead, and don't hide it behind a delay, then boom, we
    * have the ability to do the checkers example.
    */
  def actNow(move: Move): (Reward, State[Move, Position, Reward]) =
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
    */
  def render(): Unit =
    /**
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
    ???

}

case class GridPolicy(m: Map[Grid.Position, Map[Grid.Move, Double]], initialValue: Double)
    extends Policy[Grid.Move, Grid.Position, Double, GridPolicy] {
  import Grid.{Move, Position}

  override def choose(s: State[Move, Position, Double]): Generator[Move] = {
    val nMoves = s.actions.size
    val position = s.observation
    val subMap: Map[Move, Double] = m.getOrElse(position, Map.empty)

    /**
    s.actions.foldLeft((Map.empty[Grid.Move, Double], Real.zero)) { (move, sum) =>
      subMap.getOrElse(initialValue)
      m.getOrElse((position, move), initialValue)
    }

    // m.getOrElse((s.observation,
      */
    println(s"Current position is ${s.observation}")
    ???
  }

  override def learn(s: State[Move, Position, Double], move: Move, reward: Double): GridPolicy = this
}

/**
  * This next bit generates the value if we go by the Bellman
  * equation... weight each move by its chance of happening.
  *
  *
def figure_3_2():
    value = np.zeros((WORLD_SIZE, WORLD_SIZE))
    while True:
        # keep iteration until convergence
        new_value = np.zeros_like(value)
        for i in range(WORLD_SIZE):
            for j in range(WORLD_SIZE):
                for action in ACTIONS:
                    (next_i, next_j), reward = step([i, j], action)
                    # bellman equation
                    new_value[i, j] += ACTION_PROB * (reward + DISCOUNT * value[next_i, next_j])
        if np.sum(np.abs(value - new_value)) < 1e-4:
            draw_image(np.round(new_value, decimals=2))
            plt.savefig('../images/figure_3_2.png')
            plt.close()
            break
        value = new_value

// THIS now is value iteration. This actually chooses the top value
for each thing.

def figure_3_5():
    value = np.zeros((WORLD_SIZE, WORLD_SIZE))
    while True:
        # keep iteration until convergence
        new_value = np.zeros_like(value)
        for i in range(WORLD_SIZE):
            for j in range(WORLD_SIZE):
                values = []
                for action in ACTIONS:
                    (next_i, next_j), reward = step([i, j], action)
                    # value iteration
                    values.append(reward + DISCOUNT * value[next_i, next_j])
                new_value[i, j] = np.max(values)
        if np.sum(np.abs(new_value - value)) < 1e-4:
            draw_image(np.round(new_value, decimals=2))
            plt.savefig('../images/figure_3_5.png')
            plt.close()
            break
        value = new_value
  */
