import org.scalatest.FunSuite

class PokerTest extends FunSuite {
  test("2 same cards corresponds to a pair") {
    assert(PokerGame.defineTypeOfGame(Hand("1", "1")) === Pair)
  }

  test("2 different cards corresponds to a high card") {
    assert(PokerGame.defineTypeOfGame(Hand("1", "2")) === HighCard)
  }

  test("player 1 wins: player1 has a pair and player2 a highCard") {
    assert(PokerGame.defineWinner(Hand("1", "1"), Hand("1", "2")) === Some(Player1))
  }

  test("player 2 wins: player1 has a highCard and player2 a pair") {
    assert(PokerGame.defineWinner(Hand("1", "2"), Hand("1", "1")) === Some(Player2))
  }

  test("no winner: player1 has a pair and player2 a pair") {
    assert(PokerGame.defineWinner(Hand("1", "1"), Hand("1", "1")) === None)
  }

  test("player 1 wins: player 1 has a higher pair than player2") {
    assert(PokerGame.defineWinner(Hand("3", "3"), Hand("2", "2")) === Some(Player1))
  }

  test("player 2 wins: player 2 has a higher pair than player1") {
    assert(PokerGame.defineWinner(Hand("3", "3"), Hand("1", "1")) === Some(Player2))
  }

  test("no winner: player 1 and player 2 has a same pair") {
    assert(PokerGame.defineWinner(Hand("3", "3"), Hand("3", "3")) === None)
  }

  test("player 1 wins: no pair but player 1 has a higher card that player 2") {
    assert(PokerGame.defineWinner(Hand("10", "3"), Hand("2", "3")) === Some(Player1))
  }

  test("player 2 wins: no pair but player 2 has a higher card that player 1") {
    assert(PokerGame.defineWinner(Hand("1", "3"), Hand("10", "3")) === Some(Player1))
  }

  test("no winner: same cards") {
    assert(PokerGame.defineWinner(Hand("1", "3"), Hand("3", "1")) === None)
  }

  test("player 1 wins: player 2 has higher cards but player 1 has a pair") {
    assert(PokerGame.defineWinner(Hand("2", "2"), Hand("Q", "10")) === Some(Player1))
  }
}
