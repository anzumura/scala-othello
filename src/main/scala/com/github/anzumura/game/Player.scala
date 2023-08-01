package com.github.anzumura.game

object PlayerColor extends Enumeration:
  type Color = Value
  val Black, White = Value

abstract class Player(val color: PlayerColor.Value):
  protected var showValid = false

  def makeMove(board: Board, state: GameState): Boolean

  def printBoard(board: Board, state: GameState): Unit =
    board.printBoard(if (showValid) state else null)
    state.printSummary()

  override def toString: String = color.toString
