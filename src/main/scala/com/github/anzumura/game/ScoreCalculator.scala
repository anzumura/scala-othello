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
      color: Color): Int =
    if (state.gameOver) finalScore(state.getPoints(color))
    else if (depth < 2) checkScore(state.board, color)
    else if (!state.hasMoves)
      val s = GameState(state) // flip since no valid moves available
      if (!s.hasMoves)
        finalScore(s.getPoints(color))
      else
        maxLevel(depth - 2, s, alpha, beta, color)
    else
      var newAlpha = alpha
      state.validMoves.takeWhile(move => {
        newAlpha = max(newAlpha,
          minLevel(depth - 1, GameState(state, move), newAlpha, beta, color))
        newAlpha < beta
      })
      newAlpha

  @tailrec
  def minLevel(depth: Int, state: GameState, alpha: Int, beta: Int,
      color: Color): Int =
    if (state.gameOver) finalScore(state.getPoints(color))
    else if (depth < 2) checkScore(state.board, color)
    else if (!state.hasMoves)
      val s = GameState(state)
      if (!s.hasMoves)
        finalScore(s.getPoints(color)) // flips since no valid moves available
      else
        minLevel(depth - 2, s, alpha, beta, color)
    else
      var newBeta = beta
      state.validMoves.takeWhile(move => {
        newBeta = min(newBeta,
          maxLevel(depth - 1, GameState(state, move), alpha, newBeta, color))
        newBeta > alpha
      })
      newBeta

  // not private because called by tests
  private def scoreCell(myColor: Boolean, col: Column, row: Int, board: Board,
      color: Color): Int =
    (if (isCorner(col, row))
       CORNER_SCORE
     else if (isSafeEdge(col, row, board, color))
       SAFE_EDGE_SCORE
     else if (isEdge(col, row, board, color))
       EDGE_SCORE
     else if (isCenter(col, row, board, color))
       CENTER_SCORE
     else if (isNextToEdge(col, row, board))
       NEXT_TO_EDGE
     else
       NEXT_TO_CORNER) * (if (myColor) 1 else -1)

  private def checkScore(board: Board, color: Color): Int =
    (for (i <- 0 to 7; j <- Column.values)
      yield board
        .get(j, i)
        .map(c => scoreCell(c == color, j, i, board, color))
        .getOrElse(0)).sum

  private def finalScore(points: Int): Int =
    if (points > 0)
      WIN
    else if (points < 0)
      -WIN
    else
      0

  private def isCorner(col: Column, row: Int): Boolean =
    col == A && (row == 0 || row == 7) ||
      col == H && (row == 0 || row == 7)

  private def isSafeEdge(
      col: Column, row: Int, board: Board, color: Color): Boolean =
    (col == A || col == H) && checkVertical(col, row, board, color) ||
      (row == 0 || row == 7) && checkHorizontal(col, row, board, color) ||
      (col == B && row == 1 && checkSafeNextToCorner(board, color, col, row, -1,
        -1)) ||
      (col == B && row == 6 && checkSafeNextToCorner(board, color, col, row, -1,
        1)) ||
      (col == G && row == 1 && checkSafeNextToCorner(board, color, col, row, 1,
        -1)) ||
      (col == G && row == 6 && checkSafeNextToCorner(board, color, col, row, 1,
        1))

  private def checkVertical(
      col: Column, row: Int, board: Board, color: Color): Boolean =
    boundary:
      for (i <- row + 1 to 7)
        board.get(col, i) match
          case Some(x) if x == color => // keep going since same color
          case None =>
            // if space found then every piece on the other side must
            // be the same color
            for (j <- 0 until row)
              if (!board.get(col, j).contains(color)) break(false)
            break(true)
          case _ =>
            // if other color found then everything must be same color
            // on other side or all set
            for (j <- (0 until row).reverse)
              board.get(col, j) match
                case Some(x) if x == color => // keep going since same color
                case None => break(false)
                case _ =>
                  for (k <- i + 1 to 7)
                    if (board.get(col, k).isEmpty) break(false)
                  for (l <- 0 until j)
                    if (board.get(col, l).isEmpty) break(false)
                  break(true)
            break(true)
      true

  private def checkHorizontal(
      col: Column, row: Int, board: Board, color: Color): Boolean =
    if (col == A || col == H) return true
    boundary:
      for (i <- col + 1 to H)
        board.get(i, row) match
          case Some(x) if x == color => // keep going since same color
          case None =>
            // if space found then every piece on the other side must be the
            // same color
            for (j <- A to col - 1)
              if (!board.get(j, row).contains(color)) break(false)
            break(true)
          case _ =>
            // if other color found then everything must be same color on
            // other side or all set
            for (j <- (A to col - 1).reverse)
              board.get(j, row) match
                case Some(x) if x == color => // keep going since same color
                case None => break(false)
                case _ =>
                  if (i.canAdd(1))
                    for (k <- i + 1 to H)
                      if (board.get(k, row).isEmpty) break(false)
                  if (j.canAdd(-1))
                    for (l <- A to j - 1)
                      if (board.get(l, row).isEmpty) break(false)
                  break(true)
            break(true)
      true

  private def isEdge(
      col: Column, row: Int, board: Board, color: Color): Boolean =
    (col == A || col == H) && row > 1 && row < 6 ||
      (row == 0 || row == 7) && col > B && col < G ||
      // treat edge cells next to corners that are already taken as normal
      // edge cells
      ((col == A && row == 1 || col == B && row == 0) && board.has(A, 0)) ||
      ((col == A && row == 6 || col == B && row == 7) && board.has(A, 7)) ||
      ((col == H && row == 1 || col == G && row == 0) && board.has(H, 0)) ||
      ((col == H && row == 6 || col == G && row == 7) && board.has(H, 7))

  private def isCenter(
      col: Column, row: Int, board: Board, color: Color): Boolean =
    col > B && col < G && row > 1 && row < 6 ||
      // treat cells next to edges as center if the edges are already taken
      (col == B && checkSide(board, color, col, row, -1, 0)) ||
      (col == G && checkSide(board, color, col, row, 1, 0)) ||
      (row == 1 && checkSide(board, color, col, row, 0, -1)) ||
      (row == 6 && checkSide(board, color, col, row, 0, 1))

  private def isNextToEdge(col: Column, row: Int, board: Board): Boolean =
    (col == B || col == G) && row > 1 && row < 6 ||
      (row == 1 || row == 6) && col > B && col < G ||
      // treat diagonal next to corners as just 'next to edge' if the corner
      // is already taken
      (col == B && row == 1 && board.get(A, 0).nonEmpty) ||
      (col == B && row == 6 && board.get(A, 7).nonEmpty) ||
      (col == G && row == 1 && board.get(H, 0).nonEmpty) ||
      (col == G && row == 6 && board.get(H, 7).nonEmpty)

  private def checkSafeNextToCorner(board: Board, color: Color, col: Column,
      row: Int, colMove: Int, rowMove: Int): Boolean =
    board.get(col + colMove, row).contains(color) &&
      board.get(col, row + rowMove).contains(color) &&
      board.get(col + colMove, row + rowMove).contains(color) &&
      (board.get(col - colMove, row + rowMove).contains(color) ||
        board.get(col + colMove, row - rowMove).contains(color) ||
        (board.get(col - colMove, row + rowMove).nonEmpty &&
          board.get(col + colMove, row - rowMove).nonEmpty))

  private def checkSide(board: Board, color: Color, col: Column, row: Int,
      colMove: Int, rowMove: Int): Boolean =
    // to keep it simple consider this center if adjacent edge is same color or
    // adjacent edge is op color and both of the diagonal edges are set (ie all
    // three relevant edges are set)
    board.get(col + colMove, row + rowMove) match
      case Some(x) if x == color => true
      case None => false
      case _ =>
        (colMove != 0 && board.get(col + colMove, row + 1).nonEmpty &&
          board.get(col + colMove, row - 1).nonEmpty) ||
        (rowMove != 0 && board.get(col + 1, row + rowMove).nonEmpty &&
          board.get(col - 1, row + rowMove).nonEmpty)
