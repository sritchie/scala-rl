/**
  Gambler's Problem! Chapter 4 again; this generates Figure 4.3.
  */
package io.samritchie.rl
package world

object Blackjack {
  case class Config() {
    val nextCard: Cat[Card] =
      Cat.seq((1 to 14).map(n => Card(math.min(10, n))))

    def build(startingState: Observation): Blackjack =
      Blackjack(this, startingState)

    def allStates: Traversable[Observation] =
      for {
        currentSum <- 12 to 21
        aceCount <- 0 to 1
        dealerSum <- 1 to 10
      } yield Observation(Sum(currentSum), Sum(aceCount), Card(dealerSum))

    def stateSweep: Traversable[State[Action, Observation, Double, Cat]] =
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
) extends State[Blackjack.Action, Blackjack.Observation, Double, Cat] {
  import Blackjack.{Action, Card, Observation}

  def dealerAce: Boolean = observation.dealerCard.isAce

  // I think we're not going to be able to ever need to call this from the
  // current set of techniques... so maybe we move this to some place where we
  // have an expected value?
  def dynamics[O2 >: Observation]: Map[Action, Cat[(Double, State[Action, O2, Double, Cat])]] =
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
