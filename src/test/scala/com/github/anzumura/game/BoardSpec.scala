package com.github.anzumura.game

import com.github.anzumura.game.Color.*
import com.github.anzumura.game.Column.*
import com.github.anzumura.game.Row.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoardSpec extends AnyFlatSpec with Matchers:
  private val emptyRow = " . . . . . . . .\n"
  private val board = new Board
  // initial cells
  private val D4 = Cell(D, R4)
  private val D5 = Cell(D, R5)
  private val E4 = Cell(E, R4)
  private val E5 = Cell(E, R5)
  // other cells used in tests
  private val C3 = Cell(C, R3)
  private val C4 = Cell(C, R4)

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
    (for {
      c <- Cell.values
      x = board.get(c)
      color <- x
    } yield (c, color)) shouldBe Array((D4, White), (D5, Black), (E4, Black),
      (E5, White))
  }

  it should "convert to debug string" in {
    board.initialSetup()
    board.toString shouldBe "turn=black" +
      "\n1=0 0 0 0 0 0 0 0" +
      "\n2=0 0 0 0 0 0 0 0" +
      "\n3=0 0 0 0 0 0 0 0" +
      "\n4=0 0 0 2 1 0 0 0" +
      "\n5=0 0 0 1 2 0 0 0" +
      "\n6=0 0 0 0 0 0 0 0" +
      "\n7=0 0 0 0 0 0 0 0" +
      "\n8=0 0 0 0 0 0 0 0"
  }

  it should "print correctly after being initialized" in {
    board.initialSetup()
    print shouldBe emptyRow.repeat(3) + " . . . o x . . .\n" +
      " . . . x o . . .\n" + emptyRow.repeat(3)
  }

  it should "print valid moves" in {
    board.initialSetup()
    board.print(GameState(board), false) shouldBe emptyRow.repeat(2) +
      " . . . * . . . .\n" +
      " . . * o x . . .\n" +
      " . . . x o * . .\n" +
      " . . . . * . . .\n" +
      emptyRow.repeat(2)
  }

  it should "update board after colMove flip" in {
    board.initialSetup()
    board.set(C4) // black makes a move
    board.color shouldBe White
    board.print(GameState(board), false) shouldBe emptyRow.repeat(2) +
      " . . * . * . . .\n" +
      " . . x x x . . .\n" +
      " . . * x o . . .\n" +
      emptyRow.repeat(3)
  }

  it should "update board after diagonal flip" in {
    board.initialSetup()
    board.set(C4) // black makes a move
    board.color shouldBe White
    board.set(C3) // white makes a move
    board.color shouldBe Black
    board.print(GameState(board), false) shouldBe emptyRow.repeat(1) +
      " . . * . . . . .\n" +
      " . . o * . . . .\n" +
      " . . x o x . . .\n" +
      " . . . x o * . .\n" +
      " . . . . * . . .\n" +
      emptyRow.repeat(2)
  }
