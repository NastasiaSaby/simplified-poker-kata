import org.specs2.mutable.Specification

class PokerSpec extends Specification {
  "PokerGame" should {
    "Detect that 2 same cards corresponds to a pair" in {

      PokerGame.defineTypeOfGame(Hand("1", "1")) must beEqualTo(Pair)
    }

    "Detect that 2 different cards correspond to a high card" in {

      PokerGame.defineTypeOfGame(Hand("1", "2")) must beEqualTo(HighCard)
    }

    "Make player 1 win: player1 has a pair and player2 a highCard" in {

      PokerGame.defineWinner(Hand("1", "1"), Hand("1", "2")) must beSome(Player1)
    }

    "Make player 2 win: player1 has a highCard and player2 a pair" in {

      PokerGame.defineWinner(Hand("1", "2"), Hand("1", "1")) must beSome(Player2)
    }

    "Declare no winner: player 1 and player 2 has a same pair" in {

      PokerGame.defineWinner(Hand("1", "1"), Hand("1", "1")) must beNone
    }

    "Make player 1 win: player 1 has a higher pair than player2" in {

      PokerGame.defineWinner(Hand("3", "3"), Hand("2", "2")) must beSome(Player1)
    }

    "Make player 2 win: player 2 has a higher pair than player" in {

      PokerGame.defineWinner(Hand("3", "3"), Hand("1", "1")) must beSome(Player2)
    }

    "Make player 1 win: no pair but player 1 has a higher card that player 2" in {

      PokerGame.defineWinner(Hand("10", "3"), Hand("2", "3")) must beSome(Player1)
    }

    "Make player 2 win: no pair but player 2 has a higher card that player 1" in {

      PokerGame.defineWinner(Hand("1", "3"), Hand("10", "3")) must beSome(Player1)
    }

    "Declare no winner: same cards" in {

      PokerGame.defineWinner(Hand("1", "3"), Hand("3", "1")) must beNone
    }

    "Make player 1 win player 2 has higher cards but player 1 has a pair" in {

      PokerGame.defineWinner(Hand("2", "2"), Hand("Q", "10")) must beSome(Player1)
    }
  }
}
