package main.game

import util.Random
import java.text.SimpleDateFormat
import java.util.Calendar
import scala.io.StdIn.readLine

class ComputerPlayer(color: PlayerColor.Value) extends Player(color) {
  private val gen = new Random()
  private var search = -1
  private val searchDebug = 6

  do {
    print("enter computer type (0=random, 1-9=moves to search): ")
    val line = readLine()
    if (line.length == 1 && line(0).isDigit)
      search = line.toInt
  } while (search == -1)

  override def makeMove(board: Board, state: GameState): Boolean = {
    val move = if (search == 0)
      state.validMoves(gen.nextInt(state.validMoves.length))
    else
      findMove(board, state)
    board.setCell(move._1, move._2)
    println("\n" + color + " played at: " + Column(move._1) + (move._2 + 1))
    true
  }

  override def toString: String = super.toString + " with search=" + search

  private def findMove(board: Board, state: GameState): (Int, Int) = {
    var bestMove: (Int, Int) = null
    var bestScore = -ScoreCalculator.WIN
    var move = 1
    if (search > searchDebug) print(getTime + ": scanning " + state.validMoves.length + " moves: ")
    for (i <- state.validMoves) {
      if (search > searchDebug) {
        print(".")
        move += 1
      }
      val score = ScoreCalculator.minLevel(search, GameState(state, i), bestScore, ScoreCalculator.WIN, board.turn)
      if (bestMove == null || score > bestScore) {
        bestScore = score
        bestMove = i
      }
    }
    if (search > searchDebug) println("\n" + getTime + ": scan complete")
    bestMove
  }

  private def getTime: String = {
    val now = Calendar.getInstance().getTime
    val f = new SimpleDateFormat("HH:mm:ss")
    f.format(now)
  }
}