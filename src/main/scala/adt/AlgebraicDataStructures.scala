package adt

import scala.collection.SortedSet

object AlgebraicDataStructures {

  sealed abstract class Suit {
    def value: Char
  }

  object Suit {
    case object Hearts extends Suit {
      val value: Char = 'h'
    }
    case object Diamonds extends Suit {
      val value: Char = 'd'
    }
    case object Clubs extends Suit {
      val value: Char = 'c'
    }
    case object Spades extends Suit {
      val value: Char = 's'
    }
  }

  sealed abstract class Rank {
    def value: Char
  }

  object Rank {
    case object Two extends Rank {
      override def value: Char = '2'
    }
    case object Three extends Rank {
      override def value: Char = '3'
    }
    case object Four extends Rank {
      override def value: Char = '4'
    }
    case object Five extends Rank {
      override def value: Char = '5'
    }
    case object Six extends Rank {
      override def value: Char = '6'
    }
    case object Seven extends Rank {
      override def value: Char = '7'
    }
    case object Eight extends Rank {
      override def value: Char = '8'
    }
    case object Nine extends Rank {
      override def value: Char = '9'
    }
    case object Ten extends Rank {
      override def value: Char = 'T'
    }
    case object Jack extends Rank {
      override def value: Char = 'J'
    }
    case object Queen extends Rank {
      override def value: Char = 'Q'
    }
    case object King extends Rank {
      override def value: Char = 'K'
    }
    case object Ace extends Rank {
      override def value: Char = 'A'
    }
  }

  final case class Card(suit: Suit, rank: Rank)

  sealed abstract class Hand

  object Hand {
    final case class OmahaHand(cards: Set[Card]) extends Hand
    object OmahaHand{
      def create(cards: Set[Card]): Option[Hand] = {
        if (cards.size == 4) Some(OmahaHand(cards))
        else None
      }
    }
    final case class TexasHand(cards: Set[Card]) extends Hand
    object TexasHand{
      def create(cards: Set[Card]): Option[Hand] = {
        if (cards.size == 2) Some(OmahaHand(cards))
        else None
      }
    }
  }

    final case class Board(cards: Set[Card]) extends AnyVal

    object Board {
      def create(cards: Set[Card]): Option[Board] = {
        if (cards.size == 5) Some(Board(cards))
        else None
      }
    }

    sealed trait PokerCombination

    object PokerCombination {
      case object HighCard extends PokerCombination
      case object Pair extends PokerCombination
      case object ThreeOfAKind extends PokerCombination
      case object Straight extends PokerCombination
      case object Flush extends PokerCombination
      case object FullHouse extends PokerCombination
      case object FourOfAKind extends PokerCombination
      case object StraightFlush extends PokerCombination
    }

    final case class TestCase(board: Board, hands: Set[Hand])

    final case class TestResult(board: Board, hands: SortedSet[Hand])

  }
