package main.game

import java.text.SimpleDateFormat
import java.util.Calendar
import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random

class ComputerPlayer(color: PlayerColor.Value) extends Player(color):
  private val gen = new Random()
  private val search = getSearch
  private val searchDebug = 6

  @tailrec
  private def getSearch: Int =
    print("enter computer type (0=random, 1-9=moves to search): ")
    val line = readLine()
    if line.length == 1 && line(0).isDigit then line.toInt else getSearch

  override def makeMove(board: Board, state: GameState): Boolean =
    val move = if (search == 0)
      state.validMoves(gen.nextInt(state.validMoves.length))
    else
      findMove(board, state)
    board.setCell(move._1, move._2)
    println("\n" + color + " played at: " + Column(move._1) + (move._2 + 1))
    true

  override def toString: String = super.toString + " with search=" + search

  private def findMove(board: Board, state: GameState): (Int, Int) =
    var bestMove: (Int, Int) = null
    var bestScore = -ScoreCalculator.WIN
    var move = 1
    if (search > searchDebug) print(getTime + ": scanning " + state
      .validMoves.length + " moves: ")
    for (i <- state.validMoves)
      if (search > searchDebug)
        print(".")
        move += 1
      val score = ScoreCalculator.minLevel(search, GameState(state, i),
        bestScore, ScoreCalculator.WIN, board.turn)
      if (bestMove == null || score > bestScore)
        bestScore = score
        bestMove = i
    if (search > searchDebug) println("\n" + getTime + ": scan complete")
    bestMove

  private def getTime: String =
    SimpleDateFormat("HH:mm:ss").format(Calendar.getInstance().getTime)
