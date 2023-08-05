package com.github.anzumura.game

abstract class Player(val color: Color):
  protected var showValid = false

  def makeMove(board: Board, state: GameState): Boolean

  def printBoard(board: Board, state: GameState): String =
    board.printBoard(if (showValid) state else null) + state.printSummary()

  override def toString: String = color.toString
