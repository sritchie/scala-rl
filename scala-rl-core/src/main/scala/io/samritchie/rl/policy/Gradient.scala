/**
  * Policy that accumulates using the Gradient.
  */
package io.samritchie.rl
package policy

import com.stripe.rainier.core.Generator

/**
  * This thing needs to track its average reward internally... then,
  * if we have the gradient baseline set, use that thing to generate
  * the notes.
  *
  * - take e^x for each of the actions.
  * - divide each one by the sum of all the exponents... that
  *   generates the probabilities.
  */
case class Gradient[A, R](averageReward: R) extends Policy[A, R, Gradient[A, R]] {
  override def choose(state: State[A, R]): Generator[A] =
    // state.actions.toList
    // Categorical.normalize

    // scala.math.exp()
    // def act_gradient(self):
    // exp_est = np.exp(self.q_estimation)
    // self.action_prob = exp_est / np.sum(exp_est)
    // return np.random.choice(self.indices, p=self.action_prob)
    ???

  override def learn(state: State[A, R], action: A, reward: R): Gradient[A, R] =
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

object Gradient {}
