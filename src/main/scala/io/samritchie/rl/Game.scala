package io.samritchie.rl

import com.stripe.rainier.core.{Categorical, Generator, Normal}
import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.sampler.RNG
import com.twitter.algebird.AveragedValue

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
  ): Generator[(Policy[A, R, P], State[A, R])] =
    for {
      a <- policy.choose(state)
      rs <- state.act(a).getOrElse(Generator.constant((penalty, state)))
    } yield (policy.learn(state, a, rs._1), rs._2)

  def playBandit: Unit = {
    val stateGen = FakeBandit.initialBanditStateGenerator(
      10,
      Normal(0.0, 1.0).generator,
      1.0
    )

    // Continue from here, play lots of times then inspect what we've got.
    val playedState
        : Generator[(Policy[Arm, Double, EpsilonGreedy[Arm, Double, AveragedValue]], State[Arm, Double])] =
      stateGen.flatMap { x =>
        play[Arm, Double, EpsilonGreedy[Arm, Double, AveragedValue]](
          x,
          Examples.epsGreedyIncremental(0.01),
          0.0
        )
      }
  }
}
