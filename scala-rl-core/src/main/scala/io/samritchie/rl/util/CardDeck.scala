/**
  Card deck, for card games.
  */
package io.samritchie.rl
package util

import com.stripe.rainier.core.Generator

object CardDeck {
  sealed trait Rank extends Any with Product with Serializable
  final case object Jack extends Rank
  final case object Queen extends Rank
  final case object King extends Rank
  final case object Ace extends Rank
  final case class Number(value: Int) extends AnyVal with Rank

  sealed trait Suit extends Product with Serializable
  final case object Spades extends Suit
  final case object Hearts extends Suit
  final case object Clubs extends Suit
  final case object Diamonds extends Suit

  case class Card(suit: Suit, rank: Rank)

  val suits: Vector[Suit] = Vector(Spades, Hearts, Clubs, Diamonds)
  val ranks: Vector[Rank] = Vector(Jack, King, Queen, Ace) ++ (2 to 10).map(Number(_))
  val sortedDeck: Vector[Card] = for {
    suit <- suits
    rank <- ranks
  } yield Card(suit, rank)

  val deck: Set[Card] = sortedDeck.to[Set]

  /**
    Generates cards from an infinite stream, with replacement.
    */
  val basic: Generator[Card] = {
    val size = deck.size
    Generator.from((r, _) => sortedDeck(r.int(size)))
  }
}
