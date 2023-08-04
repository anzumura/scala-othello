package com.github.anzumura.game

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BoardSpec extends AnyFlatSpec with should.Matchers:
  import Board.*
  val emptyRow = " . . . . . . . .\n"
  val board = new Board

  private def print = board.printBoard(boarders = false)

  behavior of "Board"

  it should "start with current player set to Black" in {
    board.currentColor shouldBe Board.Black
  }

  it should "start with other player set to White" in {
    board.otherColor shouldBe Board.White
  }

  it should "start empty" in {
    print shouldBe emptyRow.repeat(8)
  }

  it should "print boarders by default" in {
    val boarder = "\n+-+-----------------+-+\n"
    val header = boarder + "| | A B C D E F G H | |" + boarder
    val expected = header + (for (i <- 1 to 8)
      yield s"|$i|" + " .".repeat(8) + s" |$i|").mkString("\n") + header
    board.printBoard() shouldBe expected
  }

  it should "have correct state after being initialized" in {
    board.initialSetup()
    print shouldBe emptyRow.repeat(3) + " . . . o x . . .\n" +
      " . . . x o . . .\n" + emptyRow.repeat(3)
  }
