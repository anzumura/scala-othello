package com.github.anzumura.game

import com.github.anzumura.game.Color.*
import com.github.anzumura.game.Column.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoardSpec extends AnyFlatSpec with Matchers:
  private val emptyRow = " . . . . . . . .\n"
  private val board = new Board

  private def print = board.print(boarders = false)

  behavior of "Board"

  it should "start empty with current player set to Black" in {
    val b = new Board
    b.color shouldBe Black
    b.print(boarders = false) shouldBe emptyRow.repeat(8)
  }

  it should "print boarders by default" in {
    val b = new Board
    val boarder = "+-+-----------------+-+\n"
    val header = boarder + "| | A B C D E F G H | |\n" + boarder
    val expected = header + (for (i <- 1 to 8)
      yield s"|$i|" + " .".repeat(8) + s" |$i|\n").mkString("") + header
    b.print() shouldBe expected
  }

  it should "have correct state after being initialized" in {
    board.initialSetup()
    print shouldBe emptyRow.repeat(3) + " . . . o x . . .\n" +
      " . . . x o . . .\n" + emptyRow.repeat(3)
  }

  it should "show correct set of valid moves" in {
    board.initialSetup()
    board.print(GameState(board), false) shouldBe emptyRow.repeat(2) +
      " . . . * . . . .\n" +
      " . . * o x . . .\n" +
      " . . . x o * . .\n" +
      " . . . . * . . .\n" +
      emptyRow.repeat(2)
  }

  it should "correctly update board after horizontal flip" in {
    board.initialSetup()
    board.set(C, 3) // black makes a move
    board.color shouldBe White
    board.print(GameState(board), false) shouldBe emptyRow.repeat(2) +
      " . . * . * . . .\n" +
      " . . x x x . . .\n" +
      " . . * x o . . .\n" +
      emptyRow.repeat(3)
  }

  it should "correctly update board after diagonal flip" in {
    board.initialSetup()
    board.set(C, 3) // black makes a move
    board.color shouldBe White
    board.set(C, 2) // white makes a move
    board.color shouldBe Black
    board.print(GameState(board), false) shouldBe emptyRow.repeat(1) +
      " . . * . . . . .\n" +
      " . . o * . . . .\n" +
      " . . x o x . . .\n" +
      " . . . x o * . .\n" +
      " . . . . * . . .\n" +
      emptyRow.repeat(2)
  }
