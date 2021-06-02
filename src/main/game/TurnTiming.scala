package main.game

import java.util.Calendar

trait TurnTiming extends Player {
  private var totalTime = 0L
  abstract override def makeMove(board: Board, state: GameState): Boolean = {
    val now = Calendar.getInstance().getTimeInMillis
    val result = super.makeMove(board, state)
    totalTime += Calendar.getInstance().getTimeInMillis - now
    result
  }

  def timeInSeconds(): String = {
    (totalTime.asInstanceOf[Double] / 1000).toString + " seconds"
  }
}
