package adt

import scala.collection.SortedSet

object AlgebraicDataStructures {

  sealed abstract class Suit

  object Suit {
    final case class Hearts(value: Char) extends Suit
    final case class Diamonds(value: Char) extends Suit
    final case class Clubs(value: Char) extends Suit
    final case class Spades(value: Char) extends Suit
    def create(value: Char): Option[Suit] = value.toLower match {
          case 'h' =>Some(Hearts(value))
          case 'd' => Some(Diamonds(value))
          case 'c' => Some(Clubs(value))
          case 's' => Some(Spades(value))
          case _ => None
        }
  }

  final case class Rank(value: Char) extends AnyVal

  object Rank {
    def create(value: Char): Option[Rank] = value match {
      case v if (2 to 9).map(_.toString.head) contains v => Some(Rank(value))
      case 'T' | 'J' | 'Q' | 'K' | 'A' => Some(Rank(value))
      case _ => None
    }
  }

  final case class Card(suit: Suit, rank: Rank)

  sealed abstract class Hand

  object Hand {
    final case class OmahaHand(cards: Set[Card]) extends Hand
    final case class TexasHand(cards: Set[Card]) extends Hand
    def create(cards: Set[Card]): Option[Hand] =
      cards.size match {
        case 2 => Some(TexasHand(cards))
        case 4 => Some(OmahaHand(cards))
        case _ => None
      }
  }

  final case class Board(cards: Set[Card]) extends AnyVal

  object Board {
    def create(cards: Set[Card]): Option[Board] = {
      if (cards.toList.length == 5) Some(Board(cards))
      else None
    }
  }

  sealed trait PokerCombination

  object PokerCombination {
    final case class HighCard() extends PokerCombination
    final case class Pair() extends PokerCombination
    final case class ThreeOfAKind() extends PokerCombination
    final case class Straight() extends PokerCombination
    final case class Flush() extends PokerCombination
    final case class FullHouse() extends PokerCombination
    final case class FourOfAKind() extends PokerCombination
    final case class StraightFlush() extends PokerCombination
  }

  final case class TestCase(board: Board, hands: Set[Hand])

  final case class TestResult(board: Board, hands: SortedSet[Hand])

}
