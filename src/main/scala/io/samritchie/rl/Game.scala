package io.samritchie.rl

import cats.implicits._
import RainierMonad._
import com.stripe.rainier.core.{Categorical, Generator, Normal}
import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.sampler.RNG
import com.twitter.algebird.AveragedValue
import com.twitter.util.Stopwatch

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
      policy: P,
      penalty: R
  ): Generator[(P, State[A, R])] =
    for {
      a <- policy.choose(state)
      rs <- state.act(a).getOrElse(Generator.constant((penalty, state)))
    } yield (policy.learn(state, a, rs._1), rs._2)


  def playN[A, R, P <: Policy[A, R, P]](
    state: State[A, R],
    policy: P,
    penalty: R,
    nTimes: Int
  ): Generator[(P, State[A, R])] =
    if (nTimes == 0)
      Generator.constant((policy, state))
    else
      play(state, policy, penalty).flatMap {
        case (p, s) => playN(s, p, penalty, nTimes - 1)
      }

  // initial state generator.
  val stateGen = FakeBandit.initialBanditStateGenerator(
    10,
    Normal(0.0, 1.0).generator,
    1.0
  )

  // empty starting policy.
  val policy = EpsilonGreedyGraph.instrumented

  type EG = EpsilonGreedy[Arm,Double,AveragedValue]
  type IEG = InstrumentedPolicy[Arm,Double,EG]

  def playBandit(nRuns: Int, timeSteps: Int): Generator[List[(IEG, State[Arm,Double])]] = {
    // Generates runs of a 100 times.
    val elapsed = Stopwatch.start()
    val runsGenerator = (0 to nRuns).toList.map { _ =>
      stateGen.flatMap(playN(_, policy, 0.0, timeSteps))
    }.sequence
    runsGenerator.get
    println(s"Time to play $nRuns runs of $timeSteps time steps each: ${elapsed()}")

    runsGenerator
  }

  def playAndPrintOnce(nRuns: Int, timeSteps: Int): Map[Arm, List[Double]] = {
    val (done, finalState) = playBandit(1, timeSteps).get.head

    println("Initially aggregated state:")
    println(policy.policy.aggState)

    println("acc initial:")
    println(policy.acc)

    println("Initially aggregated state:")
    println(done.policy.aggState)

    println("acc initial:")
    println(done.acc.size)

    done.acc
  }
}
