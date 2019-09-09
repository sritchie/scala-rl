/**
  * Gridworld implementation, based on what we need from Chapter 3. I
  * think this can get much more complicated, especially once
  * observations come into the picture.
  */
package io.samritchie.rl
package world

import com.stripe.rainier.core.Generator
import io.samritchie.rl.util.Grid
import scala.util.Try

object GridWorld {
  type Move = Grid.Move
  type Bounds = Grid.Bounds
  type Position = Grid.Position
  type Reward = Double
  type JumpMap = Map[Position, (Position, Reward)]

  def config(bounds: Bounds): Config =
    Config(bounds, Map.empty)

  case class Config(
      bounds: Bounds,
      jumps: JumpMap
  ) {

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
      GridWorld(Grid(start, bounds), jumps)

    def withJump(from: Position, to: Position, withReward: Reward): Config =
      copy(jumps = jumps.updated(from, (to, withReward)))

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
    jumps: GridWorld.JumpMap
) extends State[GridWorld.Move, GridWorld.Reward] {
  import GridWorld._

  def render(): Unit = {
    val x = 10

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

  // Of course we don't actually NEED the generator monad here. But
  // we do want it to generate an initial state! How does that
  // bullshit interact? Multiple levels of generation.
  def dynamics: Map[Move, Generator[(Reward, State[Move, Reward])]] = ???

  /**
    *
    def step(state, action):
    if state == A_POS:
        return A_PRIME_POS, 10
    if state == B_POS:
        return B_PRIME_POS, 5

    next_state = (np.array(state) + action).tolist()
    x, y = next_state
    if x < 0 or x >= WORLD_SIZE or y < 0 or y >= WORLD_SIZE:
        reward = -1.0
        next_state = state
    else:
        reward = 0
    return next_state, reward
    */
  override def act(move: Move): Option[Generator[(Reward, State[Move, Reward])]] =
    ???

  def toState: State[Move, Reward] = this
}

case class GridPolicy() {}

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
