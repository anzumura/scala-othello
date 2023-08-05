package com.github.anzumura.game

import com.github.anzumura.game.Board.*
import com.github.anzumura.game.Column.*

import scala.annotation.tailrec

class GameState(val totalBlack: Int, val totalWhite: Int, val board: Board):
  private var cachedValidMoves: IndexedSeq[(Int, Int)] = _
  private val blackMsg = "\nscore black(" + Color.Black.symbol + "): "
  private val whiteMsg = "\nscore white(" + Color.White.symbol + "): "

  def printSummary(): String =
    val result = StringBuilder(blackMsg + totalBlack + whiteMsg + totalWhite)
    if (!hasMoves)
      result ++= "\nGame Over - "
      result ++= (if (totalBlack > totalWhite) "black wins!"
                  else if (totalWhite > totalBlack) "white wins!"
                  else "draw")
    result += '\n'
    result.toString

  def gameOver: Boolean = totalBlack == 0 || totalWhite == 0 ||
    (totalWhite + totalBlack) == 64

  def hasMoves: Boolean = validMoves.nonEmpty

  def isValid(col: Int, row: Int): Boolean = validMoves.contains((col, row))

  def getPoints(color: Int): Int =
    if (color == 1) totalBlack - totalWhite
    else totalWhite - totalBlack

  def validMoves: IndexedSeq[(Int, Int)] =
    if (cachedValidMoves == null) cachedValidMoves = for (
      i <- GameState.allMoves if (board.getCell(i._1, i._2) match
        case Black | White => false
        case _ => GameState.checkValid(board, i._1, i._2)
      )
    ) yield i
    cachedValidMoves

object GameState:
  private val corner = IndexedSeq((A.id, 0), (H.id, 0), (A.id, 7), (H.id, 7))
  private val horizontal =
    for (i <- C.id to F.id by 2; j <- IndexedSeq(0, 7))
      yield (i, j)
  private val vertical =
    for (i <- IndexedSeq(A.id, H.id); j <- 2 to 5)
      yield (i, j)
  private val center = for (i <- C.id to F.id by 2; j <- 2 to 5) yield (i, j)
  private val nextToHorizontal = for (
    i <- C.id to F.id by 2; j <- IndexedSeq(1, 6)
  ) yield (i, j)
  private val nextToVertical =
    for (i <- IndexedSeq(B.id, G.id); j <- 2 to 5)
      yield (i, j)
  private val nextToCorner =
    IndexedSeq((A.id, 1), (A.id, 6), (B.id, 0), (B.id, 1), (B.id, 6), (B.id, 7),
      (G.id, 0), (G.id, 1), (G.id, 6), (G.id, 7), (H.id, 1), (H.id, 6))
  // create 'allMoves' with potentially best moves first and use this for
  // generating validMoves to increase pruning with alphaBeta. Tests at 6 ply
  // showed full game time fall from 43 secs to 13 for black and 56 to 14 for
  // white - so 3 to 4 times lower than scanning from A1 to H8
  private val allMoves = corner ++ horizontal ++ vertical ++ center ++
    nextToHorizontal ++ nextToVertical ++ nextToCorner

  def apply(board: Board): GameState =
    var totalBlack = 0
    var totalWhite = 0
    for (i <- 0 to 7; j <- Column.values) board.getCell(j.id, i) match
      case Black => totalBlack += 1
      case White => totalWhite += 1
      case _ =>
    new GameState(totalBlack, totalWhite, board)

  def apply(state: GameState): GameState =
    state.board.flipColor()
    apply(state.board)

  def apply(state: GameState, move: (Int, Int)): GameState =
    apply(new Board(state.board, move))

  private def checkValid(board: Board, col: Int, row: Int): Boolean =
    (if (col < C.id) checkHorizontal(board, col, row, 2)
     else if (col > F.id) checkHorizontal(board, col, row, -2)
     else
       checkHorizontal(board, col, row, 2) ||
       checkHorizontal(board, col, row, -2)) ||
      (row match
        case 0 | 1 => check(board, col, row, 0, 1)
        case 6 | 7 => check(board, col, row, 0, -1)
        case _ => check(board, col, row, 0, 1) || check(board, col, row, 0, -1)
      )

  private def checkHorizontal(board: Board, col: Int, row: Int,
      horizontal: Int) = check(board, col, row, horizontal, 0) ||
    (row match
      case 0 | 1 => check(board, col, row, horizontal, 1)
      case 6 | 7 => check(board, col, row, horizontal, -1)
      case _ =>
        check(board, col, row, horizontal, 1) ||
        check(board, col, row, horizontal, -1)
    )

  private def check(board: Board, col: Int, row: Int, horizontal: Int,
      vertical: Int): Boolean =
    doCheck(board, col + horizontal, row + vertical, horizontal, vertical)

  private def doCheck(board: Board, col: Int, row: Int, horizontal: Int,
      vertical: Int): Boolean =
    // make sure the next in this direction is the other color and then start
    // looking for my color
    board.getCell(col, row) == board.color.other.id && findSameColor(board,
      col + horizontal, row + vertical, horizontal, vertical)

  @tailrec
  private def findSameColor(board: Board, col: Int, row: Int, horizontal: Int,
      vertical: Int): Boolean =
    if (col < A.id || col > H.id || row < 0 || row > 7) return false
    val cell = board.getCell(col, row)
    if (cell == board.color.other.id)
      findSameColor(board, col + horizontal, row + vertical, horizontal,
        vertical)
    else cell == board.color.id
