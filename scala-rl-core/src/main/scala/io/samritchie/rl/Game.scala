package io.samritchie.rl

import cats.implicits._
import com.stripe.rainier.cats._
import com.stripe.rainier.core.{Generator, Normal}
import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.sampler.RNG
import com.twitter.algebird.AveragedValue
import com.twitter.util.Stopwatch
import io.samritchie.rl.state.Bandit
import io.samritchie.rl.policy.{EpsilonGreedy, Instrumented}

/**
  * Playing the game, currently. This is my test harness.
  */
object Game {

  /**
    * These are needed to actually call get on anything.
    */
  implicit val rng: RNG = RNG.default
  implicit val evaluator: Numeric[Real] = new Evaluator(Map.empty)

  // initial state generator.
  val stateGen = Bandit.initialStateGen(
    10,
    Normal(0.0, 1.0).generator,
    1.0
  )

  import Bandit.Arm

  // empty starting policy.
  val policy: Instrumented[Arm, Double, EpsilonGreedy[Arm, Double, AveragedValue]] =
    Instrumented(
      EpsilonGreedy.incremental(0.1),
      _.aggState.mapValues(_.value),
      Map.empty[Arm, List[Double]]
    )

  type EG = EpsilonGreedy[Arm, Double, AveragedValue]
  type IEG = Instrumented[Arm, Double, EG]

  def playBandit(nRuns: Int, timeSteps: Int): Generator[List[(IEG, State[Arm, Double])]] = {
    // Generates runs of a 100 times.
    val elapsed = Stopwatch.start()
    val runsGenerator = (0 to nRuns).toList.map { _ =>
      stateGen.flatMap(Policy.playN(policy, _, 0.0, timeSteps))
    }.sequence
    runsGenerator.get
    println(s"Time to play $nRuns runs of $timeSteps time steps each: ${elapsed()}")

    runsGenerator
  }

  def playAndPrintOnce(nRuns: Int, timeSteps: Int): Map[Arm, List[Double]] = {
    val (done, finalState) = playBandit(nRuns, timeSteps).get.head

    println("Initially aggregated state:")
    println(policy.policy.aggState)

    println("acc initial:")
    println(policy.acc)

    println("Final aggregated state:")
    println(done.policy.aggState)

    println("acc final:")
    println(done.acc.size)

    println("acc final:")
    println(finalState.dynamics)

    done.acc
  }

  def main(items: Array[String]): Unit =
    playAndPrintOnce(nRuns = 1, timeSteps = 10000)
}
