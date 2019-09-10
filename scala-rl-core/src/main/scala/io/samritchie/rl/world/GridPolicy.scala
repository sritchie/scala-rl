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

  /**
    *
// THIS now is value iteration. This actually chooses the top value
for each thing.

    Okay... so, yeah. We need to split out these ideas, again. We need to handle:

    - estimating the values for each action, and then:
    - immediately updating the value of the state to be the value of the top one.

    This is equivalent to updating the policy immediately to maximize
    value, and then updating the value function immediately. But there
    are really two ideas going on here. This can be a project for
    tomorrow.

    TODO: Get this cleaned up and coded.

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
  def valueIterate[A, Obs, P <: BaseCategoricalPolicy[A, Obs, Double, Eval, P]](
      policy: P,
      state: NowState[A, Obs, Double],
      stateValue: Map[Obs, Real],
      gamma: Double
  ): Map[Obs, Real] = {
    val dynamics = state.dynamics
    state.actions.foldLeft(Real.zero) { (acc, m) =>
      val (r, newState) = dynamics(m).value
      val newPos = newState.observation
      acc + (r + gamma * stateValue.getOrElse(newPos, Real.zero))
    }
    stateValue
  }
}

case class GridPolicy(m: Map[Grid.Position, Map[Grid.Move, Double]], initialValue: Double)
    extends NowPolicy[Grid.Move, Grid.Position, Double, GridPolicy] {
  import Grid.{Move, Position}

  override def choose(s: NowState[Move, Position, Double]): Eval[Move] = {
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
      s: NowState[Move, Position, Double],
      move: Move,
      reward: Double
  ): GridPolicy = this
}
