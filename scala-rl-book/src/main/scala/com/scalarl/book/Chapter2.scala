package com.scalarl
package book

import cats.implicits._
import com.stripe.rainier.cats._
import com.stripe.rainier.core.{Generator, Normal}
import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.sampler.RNG
import com.twitter.util.Stopwatch
import com.scalarl.logic.Episode
import com.scalarl.plot.Plot
import com.scalarl.policy.bandit.Greedy
import com.scalarl.rainier.Categorical
import com.scalarl.world.Bandit

/** # Introduction to Chapter 2
  *
  * This chapter is about Bandits. These are markov processes that know about a single state,
  * really. The trick here is going to be getting the stuff that plays these particular states to be
  * more general, and work with the same machinery that rolls states forward.
  *
  * What we REALLY NEED here is both the top and bottom graphs, getting it done.
  *
  * The top graph is the average reward across GAMES per step.
  *
  * So we really want to march them ALL forward and grab the average reward...
  */
object Chapter2 {
  import Bandit.Arm
  import Episode.Moment

  /** These are needed to actually call get on anything.
    */
  implicit val rng: RNG = RNG.default
  implicit val evaluator: Numeric[Real] = new Evaluator(Map.empty)

  // Implementing it the way it does in the book.
  def average(s: Iterable[Double]): Double = {
    val (sum, n) = s.foldLeft((0.0, 0)) { case ((sum, n), i) =>
      (sum + i, n + 1)
    }
    sum / n
  }

  def playBandit[Obs, A, R](
      policy: Policy[Obs, A, R, Generator, Generator],
      stateGen: Generator[State[Obs, A, R, Generator]],
      nRuns: Int,
      timeSteps: Int
  )(
      reduce: List[SARS[Obs, A, R, Generator]] => R
  ): (List[Moment[Obs, A, R, Generator]], List[R]) = {
    val rewardSeqGen =
      (0 until nRuns).toList
        .map(i => stateGen.map(s => Episode.Moment(policy, s)))
        .sequence
        .flatMap { pairs =>
          Episode.playManyN[Obs, A, R, Generator](
            pairs,
            timeSteps
          )(reduce)
        }

    val elapsed = Stopwatch.start()
    val rewardSeq = rewardSeqGen.get
    println(
      s"Time to play $nRuns runs of $timeSteps time steps each: ${elapsed()}"
    )

    rewardSeq
  }

  /** Generates the n-armed testbed.
    */
  def nArmedTestbed(
      nArms: Int,
      meanMean: Double,
      stdDev: Double
  ): Generator[State[Unit, Arm, Double, Generator]] = Bandit.stationary(
    nArms,
    Normal(meanMean, stdDev).generator
      .map(mean => Normal(mean, stdDev).generator)
  )

  /** Generates a non-stationary distribution.
    */
  def nonStationaryTestbed(
      nArms: Int,
      mean: Double,
      stdDev: Double
  ): Generator[State[Unit, Arm, Double, Generator]] =
    Bandit.nonStationary(
      nArms,
      Generator.constant(Normal(mean, stdDev).generator),
      { case (_, r, _) => Normal(r, stdDev).generator }
    )

  def play(policy: Policy[Unit, Arm, Double, Cat, Generator]): List[Double] =
    playBandit(
      policy.mapK(Categorical.catToGenerator),
      nArmedTestbed(10, 0.0, 1.0),
      nRuns = 200,
      timeSteps = 1000
    ) { case items => average(items.map(_.reward)) }._2

  def main(items: Array[String]): Unit =
    Plot.lineChartSeq(
      (play(Greedy.incrementalConfig(0.0).policy), "0.0"),
      (play(Greedy.incrementalConfig(0.01).policy), "0.01"),
      (play(Greedy.incrementalConfig(0.1).policy), "0.1")
    )
}
