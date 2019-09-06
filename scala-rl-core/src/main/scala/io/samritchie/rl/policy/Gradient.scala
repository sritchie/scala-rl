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

import com.stripe.rainier.compute.Real
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
    actionValues: Map[A, Gradient.Item[T]]
) extends Policy[A, R, Gradient[A, R, T]] {

  // We need to think here about how to handle the defaults. If
  // nothing's been visited yet, well, that's a solid initial value!
  // We need to be able to build these maps based on the action values
  // and the state set.
  //
  // TODO Get a util function for that going.
  override def choose(state: State[A, R]): Generator[A] =
    Util
      .softmax(state.actions)(
        a => actionValues.getOrElse(a, config.initial).q
      )
      .generator

  override def learn(state: State[A, R], action: A, reward: R): Gradient[A, R, T] = {
    val rewardT = config.prepare(reward)
    val rewardV = config.present(rewardT)

    // create a one_hot... array of zeros for each arm, EXCEPT one for each action.
    val m: Map[A, Real] = state.actions
      .map(
        a => a -> actionValues.getOrElse(a, config.initial).q.exp
      )
      .toMap

    // This is busted because ALL of the action probabilities get updated...
    val total: Real = Real.sum(m.values)
    val actionProb: Real = m(action) / total

    val updated = Util.updateWith[A, Gradient.Item[T]](actionValues, action) {
      case None =>
        val Gradient.Item(q, t) = config.initial
        Gradient.Item(
          q + config.stepSize * (rewardV - config.present(t)) * actionProb,
          config.plus(t, rewardT)
        )
      case Some(Gradient.Item(q, t)) =>
        Gradient.Item(
          q + config.stepSize * (rewardV - config.present(t)) * actionProb,
          config.plus(t, rewardT)
        )
    }
    copy(actionValues = updated)
  }
}

object Gradient {
  case class Item[T](q: Real, t: T)

  case class Config[R, T](
      initial: Item[T],
      stepSize: Double,
      prepare: R => T,
      plus: (T, T) => T,
      present: T => Double
  ) {
    def policy[A]: Gradient[A, R, T] = Gradient(this, Map.empty)
  }
}
