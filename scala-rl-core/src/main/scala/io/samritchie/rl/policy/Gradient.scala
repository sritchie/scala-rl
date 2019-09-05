/**
  * Policy that accumulates using the Gradient.
  *
  * TODO get this converted to an actual softmax distro.
  *
  * TODO NEXT - Make a softmax distribution, based on the numbers //
  we're keeping around.
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.compute.ToReal
import com.stripe.rainier.core.Generator

/**
  * This thing needs to track its average reward internally... then,
  * if we have the gradient baseline set, use that thing to generate
  * the notes.
  *
  * - take e^x for each of the actions.
  * - divide each one by the sum of all the exponents... that
  *   generates the probabilities.
  *
  * T is the "average" type.
  *
  */
case class Gradient[A, R, T](
    config: Gradient.Config[R, T],
    actionValues: Map[A, T]
) extends Policy[A, R, Gradient[A, R, T]] {

  override def choose(state: State[A, R]): Generator[A] =
    Util
      .softmax(
        state.actions.map(a => a -> actionValues.getOrElse(a, config.initial)).toMap
      )(t => ToReal(config.present(t)))
      .generator


  // TODO: I THINK this is where the final bit of magic is going to
  // happen... above we just have the softmax, but below we need to
  // figure out how to properly update the values.

  // T in this world is the Q estimation, which we're going to track
  // slightly differently.
  override def learn(state: State[A, R], action: A, reward: R): Gradient[A, R, T] =
    // This aggregates slightly differently...

    // elif self.gradient:
    //     one_hot = np.zeros(self.k)
    //     one_hot[action] = 1
    //     if self.gradient_baseline:
    //         baseline = self.average_reward
    //     else:
    //         baseline = 0
    //     self.q_estimation += self.step_size * (reward - baseline) * (one_hot - self.action_prob)
    ???
}

object Gradient {
  case class Config[R, T](
      initial: T,
      prepare: R => T,
      plus: (T, T) => T,
      present: T => Double
  ) {
    def policy[A]: Gradient[A, R, T] = Gradient(this, Map.empty)
  }
}
