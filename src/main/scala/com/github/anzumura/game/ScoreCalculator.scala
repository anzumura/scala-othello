package com.github.anzumura.game

import com.github.anzumura.game.Column.*

import scala.annotation.tailrec
import scala.math.{max, min}
import scala.util.boundary
import scala.util.boundary.break

object ScoreCalculator:
  val WIN = 1000000
  private val CORNER_SCORE = 2048
  private val SAFE_EDGE_SCORE = 64
  private val EDGE_SCORE = 16
  private val CENTER_SCORE = 1
  private val NEXT_TO_EDGE = -4
  private val NEXT_TO_CORNER = -16

  @tailrec
  private def maxLevel(depth: Int, state: GameState, alpha: Int, beta: Int,
                       color: Int): Int =
    if (state.gameOver)
      finalScore(state.getPoints(color))
    else if (depth < 2)
      checkScore(state.board, color)
    else if (!state.hasMoves)
      val s = GameState(state) // flip since no valid moves available
      if (!s.hasMoves)
        finalScore(s.getPoints(color))
      else
        maxLevel(depth - 2, s, alpha, beta, color)
    else
      var newAlpha = alpha
      state.validMoves.takeWhile(move => {
        newAlpha = max(newAlpha, minLevel(depth - 1, GameState(state, move),
          newAlpha, beta, color))
        newAlpha < beta
      })
      newAlpha

  @tailrec
  def minLevel(depth: Int, state: GameState, alpha: Int, beta: Int,
               color: Int): Int =
    if (state.gameOver)
      finalScore(state.getPoints(color))
    else if (depth < 2)
      checkScore(state.board, color)
    else if (!state.hasMoves)
      val s = GameState(state)
      if (!s.hasMoves)
        finalScore(s.getPoints(color)) // flips since no valid moves available
      else
        minLevel(depth - 2, s, alpha, beta, color)
    else
      var newBeta = beta
      state.validMoves.takeWhile(move => {
        newBeta = min(newBeta, maxLevel(depth - 1, GameState(state, move),
          alpha, newBeta, color))
        newBeta > alpha
      })
      newBeta

  // not private because called by tests
  private def scoreCell(myColor: Boolean, col: Int, row: Int, board: Board,
                        cell: Int): Int =
    (if (isCorner(col, row))
      CORNER_SCORE
    else if (isSafeEdge(col, row, board, cell))
      SAFE_EDGE_SCORE
    else if (isEdge(col, row, board, cell))
      EDGE_SCORE
    else if (isCenter(col, row, board, cell))
      CENTER_SCORE
    else if (isNextToEdge(col, row, board))
      NEXT_TO_EDGE
    else
      NEXT_TO_CORNER) * (if (myColor) 1 else -1)

  private def checkScore(board: Board, color: Int): Int =
    var score = 0
    for (i <- 0 to 7; j <- Column.values)
      val cell = board.getCell(j.id, i)
      score += (if (cell > 0) scoreCell(cell == color, j.id, i, board, cell)
      else 0)
    score

  private def finalScore(points: Int): Int =
    if (points > 0)
      WIN
    else if (points < 0)
      -WIN
    else
      0

  private def isCorner(col: Int, row: Int): Boolean =
    col == A.id && (row == 0 || row == 7) ||
      col == H.id && (row == 0 || row == 7)

  private def isSafeEdge(col: Int, row: Int, board: Board, cell: Int): Boolean =
    (col == A.id || col == H.id) && checkVertical(col, row, board, cell) ||
      (row == 0 || row == 7) && checkHorizontal(col, row, board, cell) ||
      (col == B.id && row == 1 && checkSafeNextToCorner(board, cell, col,
        row, -2, -1)) ||
      (col == B.id && row == 6 && checkSafeNextToCorner(board, cell, col,
        row, -2, 1)) ||
      (col == G.id && row == 1 && checkSafeNextToCorner(board, cell, col,
        row, 2, -1)) ||
      (col == G.id && row == 6 && checkSafeNextToCorner(board, cell, col,
        row, 2, 1))

  private def checkVertical(col: Int, row: Int, board: Board, cell: Int)
  : Boolean =
    boundary:
      for (i <- row + 1 to 7)
        board.getCell(col, i) match
          case `cell` => // keep going since same color
          case Board.Empty =>
            // if space found then every piece on the other side must
            // be the same color
            for (j <- 0 until row)
              if (board.getCell(col, j) != cell) break(false)
            break(true)
          case _ =>
            // if other color found then everything must be same color
            // on other side or all set
            for (j <- (0 until row).reverse)
              board.getCell(col, j) match
                case `cell` => // keep going since same color
                case Board.Empty => break(false)
                case _ =>
                  for (k <- i + 1 to 7)
                    if (board.getCell(col, k) == 0) break(false)
                  for (l <- 0 until j)
                    if (board.getCell(col, l) == 0) break(false)
                  break(true)
            break(true)
      true

  private def checkHorizontal(col: Int, row: Int, board: Board, cell: Int)
  : Boolean =
    boundary:
      for (i <- col + 2 to H.id by 2)
        board.getCell(i, row) match
          case `cell` => // keep going since same color
          case Board.Empty =>
            // if space found then every piece on the other side must be the
            // same color
            for (j <- A.id to col - 2 by 2)
              if (board.getCell(j, row) != cell) break(false)
            break(true)
          case _ =>
            // if other color found then everything must be same color on
            // other side or all set
            for (j <- (A.id to col - 2 by 2).reverse)
              board.getCell(j, row) match
                case `cell` => // keep going since same color
                case Board.Empty => break(false)
                case _ =>
                  for (k <- i + 2 to H.id by 2)
                    if (board.getCell(k, row) == 0) break(false)
                  for (l <- A.id to j - 2 by 2)
                    if (board.getCell(l, row) == 0) break(false)
                  break(true)
            break(true)
      true

  private def isEdge(col: Int, row: Int, board: Board, cell: Int): Boolean =
    (col == A.id || col == H.id) && row > 1 && row < 6 ||
      (row == 0 || row == 7) && col > B.id && col < G.id ||
      // treat edge cells next to corners that are already taken as normal
      // edge cells
      ((col == A.id && row == 1 || col == B.id && row == 0) && board.getCell
      (A.id, 0) != 0) ||
      ((col == A.id && row == 6 || col == B.id && row == 7) && board.getCell
      (A.id, 7) != 0) ||
      ((col == H.id && row == 1 || col == G.id && row == 0) && board.getCell
      (H.id, 0) != 0) ||
      ((col == H.id && row == 6 || col == G.id && row == 7) && board.getCell
      (H.id, 7) != 0)

  private def isCenter(col: Int, row: Int, board: Board, cell: Int): Boolean =
    col > B.id && col < G.id && row > 1 && row < 6 ||
      // treat cells next to edges as center if the edges are already taken
      (col == B.id && checkSide(board, cell, col, row, -2, 0)) ||
      (col == G.id && checkSide(board, cell, col, row, 2, 0)) ||
      (row == 1 && checkSide(board, cell, col, row, 0, -1)) ||
      (row == 6 && checkSide(board, cell, col, row, 0, 1))

  private def isNextToEdge(col: Int, row: Int, board: Board): Boolean =
    (col == B.id || col == G.id) && row > 1 && row < 6 ||
      (row == 1 || row == 6) && col > B.id && col < G.id ||
      // treat diagonal next to corners as just 'next to edge' if the corner
      // is already taken
      (col == B.id && row == 1 && board.getCell(A.id, 0) != 0) ||
      (col == B.id && row == 6 && board.getCell(A.id, 7) != 0) ||
      (col == G.id && row == 1 && board.getCell(H.id, 0) != 0) ||
      (col == G.id && row == 6 && board.getCell(H.id, 7) != 0)

  private def checkSafeNextToCorner(board: Board, cell: Int, col: Int,
                                    row: Int, colMove: Int, rowMove: Int)
  : Boolean =
    board.getCell(col + colMove, row) == cell &&
      board.getCell(col, row + rowMove) == cell &&
      board.getCell(col + colMove, row + rowMove) == cell &&
      (board.getCell(col - colMove, row + rowMove) == cell ||
        board.getCell(col + colMove, row - rowMove) == cell ||
        (board.getCell(col - colMove, row + rowMove) != 0 &&
          board.getCell(col + colMove, row - rowMove) != 0))

  private def checkSide(board: Board, cell: Int, col: Int, row: Int,
                        colMove: Int, rowMove: Int): Boolean =
  // to keep it simple consider this center if adjacent edge is same color or
  // adjacent edge is op color and both of the diagonal edges are set (ie all
  // three relevant edges are set)
    board.getCell(col + colMove, row + rowMove) match
      case `cell` => true
      case 0 => false
      case _ =>
        (colMove != 0 && board.getCell(col + colMove, row + 1) != 0 &&
          board.getCell(col + colMove, row - 1) != 0) ||
          (rowMove != 0 && board.getCell(col + 2, row + rowMove) != 0 &&
            board.getCell(col - 2, row + rowMove) != 0)
