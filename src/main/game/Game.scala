package main.game

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Game:
  private var state: GameState = _
  private val board = new Board
  private val players = for (c <- PlayerColor.values.toList) yield 
    createPlayer(c)

  def begin(): Unit =
    board.initialSetup()
    while (makeMove()) {}
    players.foreach(p => println("Total time for " + p + ": " + p
      .timeInSeconds()))

  @tailrec
  private def createPlayer(color: PlayerColor.Value): Player with TurnTiming =
    print("Choose type for " + color + " player (h=human, c=computer): ")
    readLine() match
      case "h" => new HumanPlayer(color) with TurnTiming
      case "c" => new ComputerPlayer(color) with TurnTiming
      case _ => createPlayer(color)

  private def makeMove(): Boolean =
    state = GameState(board)
    // if there are no valid moves flip the color and see if the other player
    // has any
    if (!state.hasMoves)
      state = GameState(state)
    val player = players(board.playerNumber)
    player.printBoard(board, state)
    state.hasMoves && player.makeMove(board, state)
