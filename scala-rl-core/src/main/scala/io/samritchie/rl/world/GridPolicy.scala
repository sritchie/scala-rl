/**
  * Policy for the gridworld.
  */
package io.samritchie.rl
package world

import cats.Eval
import io.samritchie.rl.util.Grid
import com.stripe.rainier.compute.Real

/**
  * Right now we're not actually creating a policy... we're evaluating
  * an existing policy, which we happen to know has a 25% chance of
  * taking action on any of the supplied objects.
  */
object GridPolicy {
  type AVMap = Map[Grid.Position, Map[Grid.Move, GridWorld.Reward]]

  /**
    * Runs a single step of policy evaluation.
    *
    *
    * This next bit generates the value if we go by the Bellman
    * equation... weight each move by its chance of happening.
    *
    * The policy that we're evaluating here is the random policy.
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

    */
  def evaluate[P <: BaseCategoricalPolicy[Grid.Move, Any, GridWorld.Reward, Eval, P]](
      policy: P,
      av: AVMap,
      world: NowState[GridWorld.Move, Grid.Position, GridWorld.Reward]
  ): AVMap = {
    val pmf = policy.categories(world).pmf
    val dynamics = world.dynamics
    Grid.Move.all.foldLeft(Real.zero) { (acc, m) =>
      val (r, newState) = dynamics(m).value
      val newPos = newState.observation

      // This is the version that weights by the action probability.
      acc + (pmf(m) * (r + 0.9 * av(newPos).values.sum))
    }
    av
  }

  /**
    *
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
  def valueIterate[P <: BaseCategoricalPolicy[Grid.Move, Any, GridWorld.Reward, Eval, P]](
      policy: P,
      av: AVMap,
      world: NowState[GridWorld.Move, Grid.Position, GridWorld.Reward]
  ): AVMap = {
    val dynamics = world.dynamics
    Grid.Move.all.foldLeft(Real.zero) { (acc, m) =>
      val (r, newState) = dynamics(m).value
      val newPos = newState.observation
      acc + (r + 0.9 * av(newPos).values.sum)
    }
    av
  }

  def evaluateSweep[P <: BaseCategoricalPolicy[Grid.Move, Any, GridWorld.Reward, Eval, P]](
      policy: P,
      av: AVMap,
      worlds: Traversable[GridWorld]
  ): AVMap =
    worlds match {
      case Nil          => av
      case head :: tail => evaluateSweep(policy, evaluate(policy, av, head), tail)
    }
}
case class GridPolicy(m: GridPolicy.AVMap, initialValue: Double)
    extends NowPolicy[Grid.Move, Grid.Position, GridWorld.Reward, GridPolicy] {
  import Grid.{Move, Position}
  import GridWorld.Reward

  override def choose(s: NowState[Move, Position, Reward]): Eval[Move] = {
    val position = s.observation
    val subMap: Map[Move, Double] = m.getOrElse(position, Map.empty)

    /**
    s.actions.foldLeft((Map.empty[Grid.Move, Double], Real.zero)) { (move, sum) =>
      subMap.getOrElse(initialValue)
      m.getOrElse((position, move), initialValue)
    }

    // m.getOrElse((s.observation,
      */
    println(s"Current position is ${s.observation}, $subMap")
    ???
  }

  override def learn(
      s: NowState[Move, Position, Reward],
      move: Move,
      reward: Reward
  ): GridPolicy = this
}
