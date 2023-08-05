package com.github.anzumura.game

import scala.io.StdIn.readLine

class HumanPlayer(color: Color) extends Player(color):
  override def makeMove(board: Board, state: GameState): Boolean =
    print("enter " + board.color + "'s move: ")
    val line = readLine()
    line match
      case "v" => showValid = !showValid; true
      case "q" => false
      case _ => processMove(board, state, line)

  private def processMove(
      board: Board, state: GameState, line: String): Boolean =
    if (line.matches("^[a-hA-H][1-8]$"))
      tryMove(board, state, line.charAt(0), line.substring(1, 2))
    else if (line.matches("^[1-8][a-hA-H]$"))
      tryMove(board, state, line.charAt(1), line.substring(0, 1))
    else
      println("  unrecognized input: " + line)
      println("  enter location (eg a1, C5 or 8h) or q (quit) or v (show/hide" +
        " valid moves)")
      makeMove(board, state)

  private def tryMove(
      board: Board, state: GameState, col: Char, row: String): Boolean =
    // convert from external interface to internal representation for columns
    // and rows
    if (doTryMove(board, state, Column(col).id, row.toInt - 1)) true
    else
      println("  " + col.toUpper + row + " is not a valid move")
      makeMove(board, state)

  private def doTryMove(
      board: Board, state: GameState, col: Int, row: Int): Boolean =
    state.isValid(col, row) && {
      board.setCell(col, row); true
    }
