/**
  Gambler's Problem! Chapter 4 again; this generates Figure 4.3.
  */
package io.samritchie.rl
package world

import cats.{Id, Monad}
import com.stripe.rainier.cats._
import com.stripe.rainier.core.Generator
import io.samritchie.rl.util.CardDeck

object Blackjack {
  import CardDeck.{Card, Rank}

  // TODO - to make this more legit we need to add the ability to bet.
  sealed trait Action extends Product with Serializable
  object Action {
    final case object Hit extends Action
    final case object Stay extends Action
  }

  def cardValue(card: Card): Int = card.rank match {
    case Rank.Ace                           => 11
    case Rank.Jack | Rank.Queen | Rank.King => 10
    case Rank.Number(n)                     => n
  }

  /**
    TODO - to make this solid, the Hand should actually maintain a sorted state,
    so that we can use it as the key in a hashmap. It's fine for now, since this
    state is actually going to get dropped down into a state viewable by a
    policy.
    */
  case class Hand(showing: Seq[Card], hidden: Seq[Card]) {
    def takeCard(card: Card, isShowing: Boolean): Hand =
      if (isShowing)
        copy(showing = showing :+ card)
      else
        copy(hidden = hidden :+ card)

    val usableAce: Boolean = Hand.aceCount(cards) > 0
    val totalScore: Int = Hand.score(cards)
    val showingScore: Int = Hand.score(showing)

    def cards: Seq[Card] = showing ++ hidden
    def busted: Boolean = totalScore > 21
    def showAll: Hand = if (hidden.isEmpty) this else Hand(cards, Seq.empty)
  }

  object Hand {
    val empty = Hand(Seq.empty, Seq.empty)

    def aceCount(cards: Seq[Card]): Int = cards.filter(_.rank == Rank.Ace).size
    def maxPoints(cards: Seq[Card]): Int = cards.foldLeft(0)((acc, c) => acc + cardValue(c))

    def score(cards: Seq[Card]): Int = {
      def loop(points: Int, acesLeft: Int): Int =
        if (acesLeft <= 0)
          points
        else if (points > 21)
          loop(points - 10, acesLeft - 1)
        else points

      loop(maxPoints(cards), aceCount(cards))
    }
  }

  /**
    This is the actual, full rich game.
    */
  case class Game(player: Hand, dealer: Hand) {
    def agentView: AgentView = AgentView(
      player.usableAce,
      player.totalScore,
      dealer.showingScore
    )
    def showAll: Game = Game(player.showAll, dealer.showAll)
  }
  object Game {
    val empty = Game(Hand.empty, Hand.empty)
  }

  def dealerHand(getCard: Generator[Card]): Generator[Hand] =
    for {
      showing <- getCard
      hidden <- getCard
    } yield Hand(Seq(showing), Seq(hidden))

  def playerHand(getCard: Generator[Card]): Generator[Hand] =
    Monad[Generator].tailRecM[Hand, Hand](Hand.empty) { hand =>
      if (hand.totalScore < 12)
        getCard.map(card => Left(hand.takeCard(card, true)))
      else
        Generator.constant(Right(hand))
    }

  def gameGenerator(getCard: Generator[Card]): Generator[Game] =
    for {
      player <- playerHand(getCard)
      dealer <- dealerHand(getCard)
    } yield Game(player, dealer)

  case class Config(getCard: Generator[Card]) {
    def build(startingState: Game): Blackjack =
      Alive(this, startingState)

    def stateGen: Generator[Blackjack] =
      gameGenerator(getCard).map(build(_))
  }

  /**
    This is what the agent is allowed to see.
    */
  case class AgentView(
      usableAce: Boolean,
      playerSum: Int,
      dealerSum: Int
  )

  def dealerPolicy[S[_]](hitBelow: Int): Policy[Action, Game, Double, Id, S] =
    new Policy[Action, Game, Double, Id, S] {
      override def choose(state: State[Action, Game, Double, S]): Action = {
        val hand = state.observation.dealer
        if (hand.totalScore < hitBelow) Action.Hit else Action.Stay
      }
    }
}

sealed trait Blackjack extends State[Blackjack.Action, Blackjack.Game, Double, Generator]

case class Dead(game: Blackjack.Game) extends Blackjack {
  override val observation = game.showAll
  override val dynamics = Map.empty
}

/**
  So this is PROBABLY a place where I actually need the full state, so I can
  track that the dealer has two cards, generated randomly.
  */
case class Alive(config: Blackjack.Config, game: Blackjack.Game) extends Blackjack {
  import Blackjack.{Action, Game, Hand}
  import CardDeck.Card

  override val observation: Game = game

  // I think we're not going to be able to ever need to call this from the
  // current set of techniques... so maybe we move this to some place where we
  // have an expected value?
  override def dynamics: Map[Action, Generator[(Double, Blackjack)]] =
    Map(
      Action.Hit -> config.getCard.map(hit(_)),
      Action.Stay -> dealerTurn(config.getCard)
    )

  private def hit(card: Card): (Double, Blackjack) = {
    val newGame = Game(game.player.takeCard(card, true), game.dealer)
    if (newGame.player.busted)
      (-1, Dead(newGame))
    else
      (0, copy(game = newGame))
  }

  private def endingReward(player: Hand, dealer: Hand): Int =
    if (dealer.busted || player.totalScore > dealer.totalScore)
      1
    else if (game.player.totalScore == dealer.totalScore)
      0
    else
      -1

  // This is really a policy interaction... we should allow these to ping back
  // and forth. Can I just PLAY a policy?
  // TODO convert this to use the dealer policy that's in the config.
  private def dealerTurn(getCard: Generator[Card]): Generator[(Double, Blackjack)] =
    Monad[Generator].tailRecM[Hand, (Double, Blackjack)](game.dealer) { hand =>
      if (hand.totalScore < 17)
        getCard.map(card => Left(hand.takeCard(card, true)))
      else {
        val reward = endingReward(game.player, hand)
        val dead = Dead(game.copy(dealer = hand))
        Generator.constant(Right((reward, dead)))
      }
    }
}
