package io.samritchie.rl

import com.stripe.rainier.core.{Categorical, Generator}
import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.sampler.RNG

object Game {

  /**
    * These are needed to actually call get on anything.
    */
  implicit val rng: RNG = RNG.default
  implicit val evaluator: Numeric[Real] = new Evaluator(Map.empty)

  /**
    * Plays a single turn and returns a generator that returns the
    * reward and the next state. If the chosen state's not allowed,
    * returns the supplied penalty and sends the agent back to the
    * initial state.
    */
  def play[A, R, P <: Policy[A, R, P]](
      state: State[A, R],
      policy: Policy[A, R, P],
      penalty: R
  ): Generator[(R, State[A, R])] =
    policy.choose(state).flatMap { a =>
      state.act(a).getOrElse(Generator.constant((penalty, state)))
    }
}
