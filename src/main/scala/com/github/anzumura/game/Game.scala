package com.github.anzumura.game

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Game:
  private val players = PlayerColor.values.toList.map(createPlayer)

  def begin(): Unit =
    val board = new Board
    board.initialSetup()
    while
      var state = GameState(board)
      // if there are no valid moves flip the color and see if the other player
      // has any
      if (!state.hasMoves) state = GameState(state)
      val player = players(board.currentColor - 1)
      player.printBoard(board, state)
      state.hasMoves && player.makeMove(board, state)
    do ()
    players.foreach(p => println("Total time for " + p + ": " + p.timeString))

  @tailrec
  private def createPlayer(color: PlayerColor.Value): Player with TurnTiming =
    print("Choose type for " + color + " player (h=human, c=computer): ")
    readLine() match
      case "h" => new HumanPlayer(color) with TurnTiming
      case "c" => new ComputerPlayer(color) with TurnTiming
      case _ => createPlayer(color)
