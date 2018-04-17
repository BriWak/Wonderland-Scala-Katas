import scala.util.Random

case class Card(suit: String, rank: String)

case class Deck(cards: List[Card])

case class Player(name: String, deck: Deck)

object CardGameWar {
  // Feel free to use these cards or use your own data structure
  val suits = List("Spade","Club", "Diamond", "Heart")
  val ranks = List("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")

  // Creates two shuffled decks of cards
  def createDecks: (Deck, Deck) = {
    val allCards =
      new Random shuffle (for {
        suit <- suits
        rank <- ranks
      } yield Card(suit, rank))

    val List(d1, d2) = allCards.grouped(allCards.length / 2).toList
    (Deck(d1), Deck(d2))
  }

  def playRound(player1: Card, player2: Card): Card = {
    val player1Score = ranks.indexOf(player1.rank)
    val player2Score = ranks.indexOf(player2.rank)
    val player1TieBreak = suits.indexOf(player1.suit)
    val player2TieBreak = suits.indexOf(player2.suit)

    (player1Score, player2Score, player1TieBreak, player2TieBreak) match {
      case (p1,p2,_,_) if p1 > p2 => player1
      case (p1,p2,_,_) if p1 < p2 => player2
      case (p1,p2,t1,t2) if p1 == p2 & t1 > t2 => player1
      case (p1,p2,t1,t2) if p1 == p2 & t1 < t2 => player2
    }
  }

  def playGame(player1: Player, player2: Player): String = {
    val player1Deck: List[Card] = player1.deck.cards
    val player2Deck: List[Card] = player2.deck.cards

    (player1Deck.length, player2Deck.length) match {
      case (p1, _) if p1 == 0 => s"${player1.name} wins"
      case (_, p2) if p2 == 0 => s"${player2.name} wins"
      case (_, _) =>
        if (playRound(player1Deck.head, player2Deck.head) == player1Deck.head) {
          val player1NewDeck: List[Card] = player1Deck ::: List(player1Deck.head, player2Deck.head)
          val player2NewDeck: List[Card] = player2Deck.tail
          playGame(Player(player1.name, Deck(player1NewDeck)), Player(player2.name, Deck(player2NewDeck)))
        } else {
          val player1NewDeck: List[Card] = player1Deck.tail
          val player2NewDeck: List[Card] = player2Deck ::: List(player1Deck.head, player2Deck.head)
          playGame(Player(player1.name, Deck(player1NewDeck)), Player(player2.name, Deck(player2NewDeck)))
        }
    }
  }
}