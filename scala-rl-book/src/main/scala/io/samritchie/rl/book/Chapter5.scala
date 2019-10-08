/**
  Monte Carlo methods. This will be tough... a second layer of aggregation as we
  move through episodes and aggregate across game.
  */
package io.samritchie.rl
package book

import cats.Id
import io.samritchie.rl.policy.Random
import io.samritchie.rl.world.Blackjack

object Chapter5 {
  import Blackjack.{Action, AgentView}

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

  def stickHigh[S[_]](hitBelow: Int): Policy[Action, AgentView, Double, Id, S] =
    new Policy[Action, AgentView, Double, Id, S] {
      override def choose(state: State[Action, AgentView, Double, S]): Action = {
        val score = state.observation.playerSum
        if (score < 20) Action.Hit else Action.Stay
      }
    }

  // I need it in this form for off-policy sampling, etc... but to do the
  // stochastic thing I need to be able to get a generator out of it. Or some
  // monadic thing that does not let me get expected value.
  def stickHighCat[S[_]](hitBelow: Int): Policy[Action, AgentView, Double, Cat, S] =
    stickHigh(hitBelow).mapK(Util.idToMonad[Cat])

  def random[S[_]]: Policy[Action, AgentView, Double, Cat, S] = Random()

  def main(items: Array[String]): Unit = {
    println("Hello, chapter 5!")
    println("Let's play blackjack!")
  }
}
