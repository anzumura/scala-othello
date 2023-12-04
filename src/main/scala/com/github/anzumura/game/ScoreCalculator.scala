package com.github.anzumura.game

import com.github.anzumura.game.Column.*
import com.github.anzumura.game.Move.*
import com.github.anzumura.game.Row.*

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

  // some commonly used cells when calculating score

  // corners
  private val A1 = Cell(A, R1)
  private val A8 = Cell(A, R8)
  private val H1 = Cell(H, R1)
  private val H8 = Cell(H, R8)

  // next to corners
  private val A2 = Cell(A, R2)
  private val A7 = Cell(A, R7)
  private val B1 = Cell(B, R1)
  private val B8 = Cell(B, R8)
  private val H2 = Cell(H, R2)
  private val H7 = Cell(H, R7)
  private val G1 = Cell(G, R1)
  private val G8 = Cell(G, R8)

  // diagonally next to corners
  private val B2 = Cell(B, R2)
  private val B7 = Cell(B, R7)
  private val G2 = Cell(G, R2)
  private val G7 = Cell(G, R7)

  // helper functions for getting values from Board
  inline private def get(board: Board, c: Cell, col: Column) =
    board.get(Cell(col, c.row))
  inline private def get(board: Board, c: Cell, row: Row) =
    board.get(Cell(c.col, row))

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

  private def scoreCell(
      myColor: Boolean, c: Cell, board: Board, color: Color): Int =
    (if (c.isCorner)
       CORNER_SCORE
     else if (isSafeEdge(c, board, color))
       SAFE_EDGE_SCORE
     else if (isEdge(c, board, color))
       EDGE_SCORE
     else if (isCenter(c, board, color))
       CENTER_SCORE
     else if (isNextToEdge(c, board))
       NEXT_TO_EDGE
     else
       NEXT_TO_CORNER) * (if (myColor) 1 else -1)

  private def checkScore(board: Board, color: Color): Int =
    (for (c <- Cell.values)
      yield board
        .get(c)
        .map(x => scoreCell(x == color, c, board, color))
        .getOrElse(0)).sum

  private def finalScore(points: Int): Int =
    if (points > 0)
      WIN
    else if (points < 0)
      -WIN
    else
      0

  private def isSafeEdge(c: Cell, board: Board, color: Color): Boolean =
    c.isEdgeCol && checkVertical(c, board, color) ||
      c.isEdgeRow && checkHorizontal(c, board, color) ||
      (c == B2 && checkSafeNextToCorner(board, color, c, Prev, Prev)) ||
      (c == B7 && checkSafeNextToCorner(board, color, c, Prev, Next)) ||
      (c == G2 && checkSafeNextToCorner(board, color, c, Next, Prev)) ||
      (c == G7 && checkSafeNextToCorner(board, color, c, Next, Next))

  private def checkVertical(c: Cell, board: Board, color: Color): Boolean =
    boundary:
      for (i <- c.row + 1 to R8)
        get(board, c, i) match
          case Some(x) if x == color => // keep going since same color
          case None =>
            // if space found then every piece on the other side must
            // be the same color
            for (j <- R1 to c.row - 1)
              if (!get(board, c, j).contains(color)) break(false)
            break(true)
          case _ =>
            // if other color found then everything must be same color
            // on other side or all set
            for (j <- (R1 to c.row - 1).reverse)
              get(board, c, j) match
                case Some(x) if x == color => // keep going since same color
                case None => break(false)
                case _ =>
                  if (i.canMove(Next))
                    for (k <- i + 1 to R8)
                      if (get(board, c, k).isEmpty) break(false)
                  if (j.canMove(Prev))
                    for (l <- R1 to j - 1)
                      if (get(board, c, l).isEmpty) break(false)
                  break(true)
            break(true)
      true

  private def checkHorizontal(c: Cell, board: Board, color: Color): Boolean =
    if (c.isEdgeCol) return true
    boundary:
      for (i <- c.col + 1 to H)
        get(board, c, i) match
          case Some(x) if x == color => // keep going since same color
          case None =>
            // if space found then every piece on the other side must be the
            // same color
            for (j <- A to c.col - 1)
              if (!get(board, c, j).contains(color)) break(false)
            break(true)
          case _ =>
            // if other color found then everything must be same color on
            // other side or all set
            for (j <- (A to c.col - 1).reverse)
              get(board, c, j) match
                case Some(x) if x == color => // keep going since same color
                case None => break(false)
                case _ =>
                  if (i.canMove(Next))
                    for (k <- i + 1 to H)
                      if (get(board, c, k).isEmpty) break(false)
                  if (j.canMove(Prev))
                    for (l <- A to j - 1)
                      if (get(board, c, l).isEmpty) break(false)
                  break(true)
            break(true)
      true

  private def isEdge(c: Cell, board: Board, color: Color): Boolean =
    c.isEdgeCol && c.row > R2 && c.row < R7 ||
      c.isEdgeRow && c.col > B && c.col < G ||
      // treat edge cells next to corners that are already taken as normal
      // edge cells
      ((c == A2 || c == B1) && board.has(A1)) ||
      ((c == A7 || c == B8) && board.has(A8)) ||
      ((c == H2 || c == G1) && board.has(H1)) ||
      ((c == H7 || c == G8) && board.has(H8))

  private def isCenter(c: Cell, board: Board, color: Color): Boolean =
    c.isCenter ||
      // treat cells next to edges as center if the edges are already taken
      (c.col == B && checkSide(board, color, c, Prev, Same)) ||
      (c.col == G && checkSide(board, color, c, Next, Same)) ||
      (c.row == R2 && checkSide(board, color, c, Same, Prev)) ||
      (c.row == R7 && checkSide(board, color, c, Same, Next))

  private def isNextToEdge(c: Cell, board: Board): Boolean =
    c.isNextToEdge ||
      // treat diagonal next to corner as 'next to edge' if corner is non-empty
      (c == B2 && board.get(A1).nonEmpty) ||
      (c == B7 && board.get(A8).nonEmpty) ||
      (c == G2 && board.get(H1).nonEmpty) ||
      (c == G7 && board.get(H8).nonEmpty)

  private def checkSafeNextToCorner(board: Board, color: Color, c: Cell,
      colMove: Move, rowMove: Move): Boolean =
    get(board, c, c.col.move(colMove)).contains(color) &&
      get(board, c, c.row.move(rowMove)).contains(color) &&
      board.get(c.move(colMove, rowMove)).contains(color) &&
      (board.get(c.move(-colMove, rowMove)).contains(color) ||
        board.get(c.move(colMove, -rowMove)).contains(color) ||
        (board.get(c.move(-colMove, rowMove)).nonEmpty &&
          board.get(c.move(colMove, -rowMove)).nonEmpty))

  private def checkSide(board: Board, color: Color, c: Cell, colMove: Move,
      rowMove: Move): Boolean =
    // to keep it simple consider this center if adjacent edge is same color or
    // adjacent edge is op color and both of the diagonal edges are set (ie all
    // three relevant edges are set)
    board.get(c.move(colMove, rowMove)) match
      case Some(x) if x == color => true
      case None => false
      case _ =>
        (colMove != Same && board.get(c.move(colMove, Next)).nonEmpty &&
          board.get(c.move(colMove, Prev)).nonEmpty) ||
        (rowMove != Same && board.get(c.move(Next, rowMove)).nonEmpty &&
          board.get(c.move(Prev, rowMove)).nonEmpty)
