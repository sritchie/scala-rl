/**
  Card deck, for card games.
  */
package io.samritchie.rl
package util

import com.stripe.rainier.core.Generator

object CardDeck {
  sealed trait Rank extends Any with Product with Serializable
  object Rank {
    val all: Vector[Rank] = Vector(Jack, King, Queen, Ace) ++ (2 to 10).map(Number(_))
    final case object Jack extends Rank
    final case object Queen extends Rank
    final case object King extends Rank
    final case object Ace extends Rank
    final case class Number(value: Int) extends AnyVal with Rank

    implicit val rankOrd: Ordering[Rank] =
      Ordering.by(all.indexOf(_))
  }

  sealed trait Suit extends Product with Serializable
  object Suit {
    val all: Vector[Suit] = Vector(Spades, Hearts, Clubs, Diamonds)
    final case object Spades extends Suit
    final case object Hearts extends Suit
    final case object Clubs extends Suit
    final case object Diamonds extends Suit

    implicit val suitOrd: Ordering[Suit] =
      Ordering.by(all.indexOf(_))
  }

  case class Card(suit: Suit, rank: Rank)
  object Card {
    val all: Vector[Card] = for {
      suit <- Suit.all
      rank <- Rank.all
    } yield Card(suit, rank)

    val deck: Set[Card] = all.to[Set]

    implicit val ordering: Ordering[Card] =
      Ordering.by(card => (card.suit, card.rank))
  }

  /**
    Generates cards from an infinite stream, with replacement.
    */
  val basic: Generator[Card] = {
    val size = Card.deck.size
    Generator.from((r, _) => Card.all(r.int(size)))
  }
}
