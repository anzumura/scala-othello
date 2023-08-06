package com.github.anzumura.game

import com.github.anzumura.game.Color.*
import com.github.anzumura.game.Column.*

import scala.annotation.tailrec

class GameState(val totalBlack: Int, val totalWhite: Int, val board: Board):
  private var cachedValidMoves: IndexedSeq[(Column, Int)] = _
  private val blackMsg = "\nscore black(" + Black.symbol + "): "
  private val whiteMsg = "\nscore white(" + White.symbol + "): "

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

  def isValid(col: Column, row: Int): Boolean = validMoves.contains((col, row))

  def getPoints(color: Color): Int =
    if (color == Black) totalBlack - totalWhite
    else totalWhite - totalBlack

  def validMoves: IndexedSeq[(Column, Int)] =
    if (cachedValidMoves == null)
      cachedValidMoves = for (
        i <- GameState.allMoves
        if board.get(i(0), i(1)).isEmpty && GameState
          .checkValid(board, i(0), i(1))
      ) yield i
    cachedValidMoves

object GameState:
  private val corner = Vector((A, 0), (H, 0), (A, 7), (H, 7))
  private val horizontal = for (i <- C to F; j <- Vector(0, 7)) yield (i, j)
  private val vertical = for (i <- Vector(A, H); j <- 2 to 5) yield (i, j)
  private val center = for (i <- C to F; j <- 2 to 5) yield (i, j)
  private val nextToHorizontal =
    for (i <- C to F; j <- Vector(1, 6)) yield (i, j)
  private val nextToVertical = for (i <- Vector(B, G); j <- 2 to 5) yield (i, j)
  private val nextToCorner =
    IndexedSeq((A, 1), (A, 6), (B, 0), (B, 1), (B, 6), (B, 7), (G, 0), (G, 1),
      (G, 6), (G, 7), (H, 1), (H, 6))
  // create 'allMoves' with potentially best moves first and use this for
  // generating validMoves to increase pruning with alphaBeta. Tests at 6 ply
  // showed full game time fall from 43 secs to 13 for black and 56 to 14 for
  // white - so 3 to 4 times lower than scanning from A1 to H8
  private val allMoves = corner ++ horizontal ++ vertical ++ center ++
    nextToHorizontal ++ nextToVertical ++ nextToCorner

  def apply(board: Board): GameState =
    var totalBlack = 0
    var totalWhite = 0
    for (i <- 0 to 7; j <- Column.values) board.get(j, i) match
      case Some(Black) => totalBlack += 1
      case Some(White) => totalWhite += 1
      case _ =>
    new GameState(totalBlack, totalWhite, board)

  def apply(state: GameState): GameState =
    state.board.flipColor()
    apply(state.board)

  def apply(state: GameState, move: (Column, Int)): GameState =
    apply(new Board(state.board, move))

  private def checkValid(board: Board, col: Column, row: Int): Boolean =
    (if (col < C) checkHorizontal(board, col, row, 1)
     else if (col > F) checkHorizontal(board, col, row, -1)
     else
       checkHorizontal(board, col, row, 1) ||
       checkHorizontal(board, col, row, -1)) ||
      (row match
        case 0 | 1 => check(board, col, row, 0, 1)
        case 6 | 7 => check(board, col, row, 0, -1)
        case _ => check(board, col, row, 0, 1) || check(board, col, row, 0, -1)
      )

  private def checkHorizontal(board: Board, col: Column, row: Int,
      horizontal: Int) = check(board, col, row, horizontal, 0) ||
    (row match
      case 0 | 1 => check(board, col, row, horizontal, 1)
      case 6 | 7 => check(board, col, row, horizontal, -1)
      case _ =>
        check(board, col, row, horizontal, 1) ||
        check(board, col, row, horizontal, -1)
    )

  private def check(board: Board, col: Column, row: Int, horizontal: Int,
      vertical: Int): Boolean =
    doCheck(board, col + horizontal, row + vertical, horizontal, vertical)

  private def doCheck(board: Board, col: Column, row: Int, horizontal: Int,
      vertical: Int): Boolean =
    // make sure the next in this direction is the other color and then start
    // looking for my color
    board.get(col, row).contains(board.color.other) && findSameColor(board, col,
      row, horizontal, vertical)

  @tailrec
  private def findSameColor(board: Board, colIn: Column, rowIn: Int,
      horizontal: Int, vertical: Int): Boolean =
    val row = rowIn + vertical
    if (!colIn.canAdd(horizontal) || row < 0 || row > 7) return false
    val col = colIn + horizontal
    board.get(col, row) match
      case Some(x) if x == board.color.other =>
        findSameColor(board, col, row, horizontal, vertical)
      case x => x.contains(board.color)
