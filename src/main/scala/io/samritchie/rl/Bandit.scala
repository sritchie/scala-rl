/**
  * Code to implement a bandit.
  */
package io.samritchie.rl

import com.stripe.rainier.core.Generator

object Arm {
  implicit val ordering: Ordering[Arm] = Ordering.by(_.i)
}
case class Arm(i: Int)

object FakeBandit {

  /**
    * An "Arm" is something that takes you to a new state. We just
    * happen to have only a single state here, so it always takes you
    * back to a given "bandit" problem.
    */
  def arms(k: Int): Set[Arm] = (0 to k).map(Arm(_)).toSet

  /**
    * TODO - implement this thing.
    */
  def banditState[R](k: Int, meanGenerator: Generator[R], stdDev: R): State[Arm, R] =
    // generate a distribution for each of the incoming ints.
    State.bandit(???)
}

case class FakeBandit() {
  // This guy is going to have some policy that is able to get the
  // values of each of the arms, or actions, for the state that it is
  // in.
  //
  // Using the baked in epsilon he can decide then whether to take the
  // greedy option or not.

  /**
    * Randomly choose from among the set of arms with probability less
    * than epsilon.
    *
    * If you DON'T choose from, then you need to go figure out what
    * the greedy choice will be. How to do that?
    *
    * q_estimation variable... look at how that is updated.
    *
    * That gets set with, potentially, an optimistic initial value,
    * but otherwise, usually, it is always set with zeros.
    *
    * So "act" is not doing any updating... it is just returning the
    * good stuff to you.
    *
    * Then "step" is what actually implements the world bump.
    *
    * - WHEN THE BANDIT WAS CREATED, its true reward was initialized
    *   around some value, true_reward, but selected at "reset" time
    *   to be a normally distributed core reward. That's the actual,
    *   true value of q-*.
    * - Then, when the bandit chooses that, we get back a value that's
    *   again normally distributed, but this time around the "true
    *   reward".
    * - The estimates for q-* always start at 0, here, unless we
    *   provide an optimistic initial framework. But this is the thing
    *   we're learning... this is the model of the world.
    * - Then we track things like, number of times each action has
    *   been chosen.
    * - We also know that the BEST action to choose is the one for
    *   which we maximize q_true. The goal is to learn that action.
    */
  /**
    * This needs to return the arm, the action, to take. This actually
    * seems to be something that a policy would want to choose for
    * you.
    *
    * But it's going to return what you should actually do.
    *
    * And I think instead of just returning an arm, it would be nice
    * if we created a probability distribution of how the bandit is
    * going to act, maybe?
    *
    * Build up to the big thing and then experiment with that.
    */
  def act: Arm = ???

  /**
    * This is going to take an arm and decide what to do next.
    *
    * - First, generate, really the world should be doing this, but
    *   generate an actual reward that happens for that arm. This is
    *   normally distributed, but shifted up by the true reward for
    *   that arm.
    *
    * - track the time and the action count. (InstrumentedWorld
    *   combinator.)
    *
    * - track the averagereward TOTAL that has come back, over
    *   time. This also could be tracked by the world.
    *
    * Then, aggregate!!!
    *
    * - If using sample averages use the number of items we've seen,
    *   get it in that way.
    * - else use constant step size
    * - else use the gradient method
    *
    *
    * But the key is that at this point we aggregate the total reward
    * for each (state, action) pair. We return the actual reward that
    * we received, but internally we update our q_estimations, which
    * we're going to use later to figure this shit out.
    */
  def step(arm: Arm): Unit = ???
}
