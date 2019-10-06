/**
  Gambler's Problem! Chapter 4 again; this generates Figure 4.3.
  */
package io.samritchie.rl
package world

import com.stripe.rainier.core.Generator

object Blackjack {
  case class Config() {
    val nextCard: Generator[Card] =
      Cat.seq((1 to 14).map(n => Card(math.min(10, n)))).toRainier.generator

    def build(startingState: Observation): Blackjack =
      Blackjack(this, startingState)

    def allStates: Generator[Observation] =
      Cat
        .seq(
          for {
            currentSum <- 12 to 21
            aceCount <- 0 to 1
            dealerSum <- 1 to 10
          } yield Observation(Sum(currentSum), Sum(aceCount), Card(dealerSum))
        )
        .toRainier
        .generator

    // Get this into a better generator state.
    def stateGen: Generator[Blackjack] =
      allStates.map(Blackjack(this, _))
  }

  sealed trait Action extends Product with Serializable
  object Action {
    final case object Hit extends Action
    final case object Stay extends Action
  }
  case class Sum(n: Int) extends AnyVal
  case class Card(value: Int) extends AnyVal {
    def isAce: Boolean = value == 1
  }
  case class Observation(
      currentSum: Sum,
      aceCount: Sum,
      dealerCard: Card
  )
}

/**
  So this is PROBABLY a place where I actually need the full state, so I can
  track that the dealer has two cards, generated randomly.
  */
case class Blackjack(
    config: Blackjack.Config,
    observation: Blackjack.Observation
) extends State[Blackjack.Action, Blackjack.Observation, Double, Generator] {
  import Blackjack.{Action, Card, Observation}

  def dealerAce: Boolean = observation.dealerCard.isAce

  // I think we're not going to be able to ever need to call this from the
  // current set of techniques... so maybe we move this to some place where we
  // have an expected value?
  def dynamics[O2 >: Observation]: Map[Action, Generator[(Double, State[Action, O2, Double, Generator])]] =
    if (someoneWon)
      Map.empty
    else
      Map(
        Action.Hit -> config.nextCard.map(hit(_)),
        Action.Stay -> config.nextCard.map(dealerTurn(_))
      )

  def someoneWon: Boolean = ???
  def hit(card: Card): (Double, Blackjack) = ???
  def dealerTurn(card: Card): (Double, Blackjack) = ???
}
