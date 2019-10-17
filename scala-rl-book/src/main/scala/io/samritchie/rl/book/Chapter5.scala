/**
  Monte Carlo methods. This will be tough... a second layer of aggregation as we
  move through episodes and aggregate across game.
  */
package io.samritchie.rl
package book

import cats.{Id, Monad}
import cats.implicits._
import com.stripe.rainier.cats._
import com.stripe.rainier.compute.{Evaluator, Real}
import com.stripe.rainier.core.Generator
import com.stripe.rainier.sampler.RNG
import com.twitter.algebird.Aggregator
import io.samritchie.rl.logic.MonteCarlo
import io.samritchie.rl.policy.Random
import io.samritchie.rl.util.CardDeck
import io.samritchie.rl.world.Blackjack

object Chapter5 {
  import Blackjack.{Action, AgentView, Result}

  /**
    This is the figure that explores the stickHigh strategy over a bunch of
    states, tracking what happens with a usable ace and with no usable ace.
    */
  def figureFiveOne(): Unit = ()

  /**
    This uses exploring starts to capture the optimal policy.
    */
  def figureFiveTwo(): Unit = ()

  /**
    this checks using the random policy to check the stickHigh behavior policy,
    and compares ordinary and weighted off-policy sampling.
    */
  def figureFiveThree(): Unit = ()

  def stickHigh[S[_]](hitBelow: Int): Policy[AgentView, Action, Double, Id, S] =
    new Policy[AgentView, Action, Double, Id, S] {
      override def choose(state: State[AgentView, Action, Double, S]): Action = {
        val score = state.observation.playerSum
        if (score < 20) Action.Hit else Action.Stay
      }
    }

  // I need it in this form for off-policy sampling, etc... but to do the
  // stochastic thing I need to be able to get a generator out of it. Or some
  // monadic thing that does not let me get expected value.
  def stickHighCat[S[_]](hitBelow: Int): Policy[AgentView, Action, Double, Cat, S] =
    stickHigh(hitBelow).mapK(Util.idToMonad[Cat])

  def random[M[_]]: Policy[AgentView, Action, Double, Cat, M] = Random()

  /**
    Is this appreciably slower? This is going to be useful, in any case, when I'm working with the tests.
    */
  def limitedM[M[_]: Monad](state: M[Blackjack[M]]): M[State[AgentView, Action, Double, M]] =
    state.map(_.mapObservation(_.agentView).mapReward {
      case Result.Draw | Result.Pending => 0
      case Result.Win                   => 1
      case Result.Lose                  => -1
    })

  val starter: Generator[Blackjack[Generator]] = Blackjack.Config[Generator](CardDeck.basic).stateM
  val limited: Generator[State[AgentView, Action, Double, Generator]] = limitedM(starter)

  def main(items: Array[String]): Unit = {
    implicit val rng: RNG = RNG.default
    implicit val evaluator: Numeric[Real] = new Evaluator(Map.empty)

    println("Hello, chapter 5!")
    println("Let's play blackjack!")

    val (s, trajectory) = limited.flatMap { state =>
      MonteCarlo
        .firstVisit[AgentView, Action, Double, Generator](
          random[Generator].mapK(Cat.catToGenerator),
          state
        )
    }.get
    val processed =
      MonteCarlo.processTrajectory[AgentView, Action, Double, Double](
        trajectory,
        value.ActionValueMap(
          Map.empty,
          x => x,
          0.0
        ),
        Aggregator.fromMonoid
      )
    println((processed, s.observation))
    ()
  }
}
