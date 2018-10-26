
object PokerGame {
  def defineTypeOfGame(hand: Hand): TypeOfGame = {
    if (hand.first == hand.second) {
      Pair
    } else {
      HighCard
    }
  }

  def defineWinner(firstHand: Hand, secondHand: Hand): Option[Winner] = {
    (defineTypeOfGame(firstHand), defineTypeOfGame(secondHand)) match {
      case (Pair, HighCard) => Some(Player1)
      case (HighCard, Pair) => Some(Player2)
      case (Pair, Pair) => computeHigherHand(scoreCard(firstHand.first), scoreCard(secondHand.first))
      case (HighCard, HighCard) => defineWinnerFromHighCard(firstHand, secondHand)
      case _ => None
    }
  }

  private def defineWinnerFromHighCard(firstHand: Hand, secondHand: Hand): Option[Winner] = {
    val firstHandHigherCard = computeHigherCardFromSameHand(scoreCard(firstHand.first), scoreCard(firstHand.second))
    val secondHandHigherCard = computeHigherCardFromSameHand(scoreCard(secondHand.first), scoreCard(secondHand.second))

    computeHigherHand(firstHandHigherCard, secondHandHigherCard)
  }

  private def computeHigherCardFromSameHand(firstHandScore: Int, secondHandScore: Int): Int = {
    if (firstHandScore > secondHandScore) {
      firstHandScore
    } else {
      secondHandScore
    }
  }

  private def computeHigherHand(firstHandScore: Int, secondHandScore: Int) = {
    if (firstHandScore > secondHandScore) {
      Some(Player1)
    } else if (secondHandScore > firstHandScore) {
      Some(Player2)
    } else {
      None
    }
  }

  private def scoreCard(cardNumber: String) = {
    cardNumber match {
      case "2" => 1
      case "3" => 2
      case "4" => 3
      case "5" => 4
      case "6" => 5
      case "7" => 6
      case "8" => 7
      case "9" => 8
      case "10" => 9
      case "J" => 10
      case "Q" => 11
      case "K" => 12
      case "1" => 13
    }
  }
}

trait TypeOfGame
case object HighCard extends TypeOfGame
case object Pair extends TypeOfGame

case class Hand(first: String, second: String)

trait Winner
case object Player1 extends Winner
case object Player2 extends Winner
