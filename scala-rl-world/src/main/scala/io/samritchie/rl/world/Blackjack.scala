/**
  Monadic Blackjack!
  */
package io.samritchie.rl
package world

import cats.{Id, Monad}
import cats.implicits._
import io.samritchie.rl.world.util.CardDeck

object Blackjack {
  import CardDeck.{Card, Rank}

  // TODO - to make this more legit we need to add the ability to bet.
  sealed trait Action extends Product with Serializable
  object Action {
    final case object Hit extends Action
    final case object Stay extends Action
  }

  sealed trait Result extends Product with Serializable
  object Result {
    final case object Win extends Result
    final case object Draw extends Result
    final case object Lose extends Result
    final case object Pending extends Result
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

  def dealerHand[M[_]: Monad](getCard: M[Card]): M[Hand] =
    for {
      showing <- getCard
      hidden <- getCard
    } yield Hand(Seq(showing), Seq(hidden))

  def playerHand[M[_]: Monad](getCard: M[Card]): M[Hand] =
    Monad[M].tailRecM[Hand, Hand](Hand.empty) { hand =>
      if (hand.totalScore < 12)
        getCard.map(card => Left(hand.takeCard(card, true)))
      else
        Monad[M].pure(Right(hand))
    }

  def gameGenerator[M[_]: Monad](getCard: M[Card]): M[Game] =
    for {
      player <- playerHand(getCard)
      dealer <- dealerHand(getCard)
    } yield Game(player, dealer)

  case class Config[M[_]: Monad](getCard: M[Card]) {
    def build(startingState: Game): Blackjack[M] =
      Alive(this, startingState)

    def stateM: M[Blackjack[M]] = gameGenerator(getCard).map(build(_))
  }

  /**
    This is what the agent is allowed to see.
    */
  case class AgentView(
      usableAce: Boolean,
      playerSum: Int,
      dealerSum: Int
  )

  /**
    Generate a simple fixed policy for an agent.
    */
  def policy[S[_]](f: AgentView => Action): Policy[AgentView, Action, Double, Id, S] =
    Policy.choose[AgentView, Action, Double, Id, S](s => f(s.observation))

  // TODO get the game below to use this as the "opponent" instead of manually
  // doing it.
  def dealerPolicy[S[_]](hitBelow: Int): Policy[Game, Action, Double, Id, S] =
    Policy.choose { state =>
      val hand = state.observation.dealer
      if (hand.totalScore < hitBelow) Action.Hit else Action.Stay
    }
}

sealed trait Blackjack[M[_]] extends State[Blackjack.Game, Blackjack.Action, Blackjack.Result, M] {
  def game: Blackjack.Game
}

case class Dead[M[_]: Monad](game: Blackjack.Game) extends Blackjack[M] {
  override val invalidMove = Monad[M].pure((Blackjack.Result.Lose, this))
  override val observation = game.showAll
  override val dynamics = Map.empty
}

/**
  So this is PROBABLY a place where I actually need the full state, so I can
  track that the dealer has two cards, generated randomly.
  */
case class Alive[M[_]: Monad](config: Blackjack.Config[M], game: Blackjack.Game) extends Blackjack[M] {
  import Blackjack.{Action, Game, Hand, Result}
  import CardDeck.Card

  override val observation: Game = game

  // I think we're not going to be able to ever need to call this from the
  // current set of techniques... so maybe we move this to some place where we
  // have an expected value?
  override def dynamics: Map[Action, M[(Result, This)]] =
    Map(
      Action.Hit -> config.getCard.map(hit(_)),
      Action.Stay -> dealerTurn(config.getCard)
    )
  override val invalidMove = Monad[M].pure((Blackjack.Result.Pending, this))

  private def hit(card: Card): (Result, This) = {
    val newGame = Game(game.player.takeCard(card, true), game.dealer)
    if (newGame.player.busted)
      (Result.Lose, Dead(newGame))
    else
      (Result.Pending, copy(game = newGame))
  }

  private def endingResult(player: Hand, dealer: Hand): Result =
    if (dealer.busted || player.totalScore > dealer.totalScore)
      Result.Win
    else if (game.player.totalScore == dealer.totalScore)
      Result.Draw
    else
      Result.Lose

  // This is really a policy interaction... we should allow these to ping back
  // and forth. Can I just PLAY a policy?
  // TODO convert this to use the dealer policy that's in the config.
  private def dealerTurn(getCard: M[Card]) =
    Monad[M].tailRecM[Hand, (Result, This)](game.dealer) { hand =>
      if (hand.totalScore < 17)
        getCard.map(card => Left(hand.takeCard(card, true)))
      else {
        val result = endingResult(game.player, hand)
        val dead = Dead[M](game.copy(dealer = hand))
        Monad[M].pure(Right((result, dead)))
      }
    }
}
