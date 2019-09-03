package io.samritchie.rl

import cats.implicits._
import com.stripe.rainier.cats._
import com.stripe.rainier.core.{Generator, Normal}
import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.sampler.RNG
import com.twitter.algebird.AveragedValue
import com.twitter.util.Stopwatch
import io.samritchie.rl.state.Bandit
import io.samritchie.rl.policy.EpsilonGreedy

/**
  * Playing the game, currently. This is my test harness.
  *
  * What we REALLY NEED here is both the top and bottom graphs,
  * getting it done.
  *
  * The top graph is the average reward across GAMES per step.
  *
  * So we really want to march them ALL forward and grab the average
  * reward...
  */
object Game {
  import Bandit.Arm
  type EG = EpsilonGreedy[Arm, Double, AveragedValue]

  /**
    * These are needed to actually call get on anything.
    */
  implicit val rng: RNG = RNG.default
  implicit val evaluator: Numeric[Real] = new Evaluator(Map.empty)

  def average(s: Iterable[Double]): Double = {
    val (sum, n) = s.foldLeft((0.0, 0)) { case ((sum, n), i) => (sum + i, n + 1) }
    sum / n
  }

  def playBandit[A, R, P <: Policy[A, R, P]](
      policy: P,
      stateGen: Generator[Bandit[A, R]],
      nRuns: Int,
      timeSteps: Int,
      penalty: R
  )(reduce: List[R] => R): (List[(P, State[A, R])], List[R]) = {
    val rewardSeqGen: Generator[(List[(P, State[A, R])], List[R])] =
      (0 until nRuns).toList
        .map(i => stateGen.map(s => (policy, s)))
        .sequence
        .flatMap { pairs =>
          Policy.playManyN[A, R, P](
            pairs,
            penalty,
            timeSteps
          )(reduce)
        }

    val elapsed = Stopwatch.start()
    val rewardSeq = rewardSeqGen.get
    println(s"Time to play $nRuns runs of $timeSteps time steps each: ${elapsed()}")

    rewardSeq
  }

  val stateGen = Bandit.initialStateGen(
    10,
    Normal(0.0, 1.0).generator,
    1.0
  )

  def cake(policy: EG): List[Double] =
    playBandit(
      policy,
      stateGen,
      nRuns = 200,
      timeSteps = 1000,
      penalty = 0.0
    )(average(_))._2

  def main(items: Array[String]): Unit =
    Plot.lineChartSeq(
      (cake(EpsilonGreedy.incremental(0.0)), "0.0"),
      (cake(EpsilonGreedy.incremental(0.01)), "0.01"),
      (cake(EpsilonGreedy.incremental(0.1)), "0.1")
    )
}
