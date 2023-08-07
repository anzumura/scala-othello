package com.github.anzumura.game

import com.github.anzumura.game.Color.*
import com.github.anzumura.game.Column.*
import com.github.anzumura.game.Row.*

import scala.annotation.tailrec

class GameState(val totalBlack: Int, val totalWhite: Int, val board: Board):
  import GameState.*
  private var cachedValidMoves: Vector[Cell] = _

  def printSummary: String =
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

  def isValid(c: Cell): Boolean = validMoves.contains(c)

  def getPoints(color: Color): Int =
    if (color == Black) totalBlack - totalWhite
    else totalWhite - totalBlack

  def validMoves: IndexedSeq[Cell] =
    if (cachedValidMoves == null) cachedValidMoves = for (
      i <- allMoves if board.get(i).isEmpty && checkValid(board, i)
    ) yield i
    cachedValidMoves

object GameState:
  private val blackMsg = "\nscore black(" + Black.symbol + "): "
  private val whiteMsg = "\nscore white(" + White.symbol + "): "

  // create 'allMoves' with potentially best moves first and use this for
  // generating validMoves to increase pruning with alphaBeta. Tests at 6 ply
  // showed full game time fall from 43 secs to 13 for black and 56 to 14 for
  // white - so 3 to 4 times lower than scanning from A1 to H8
  private val allMoves = {
    val corner = Vector(Cell(A, R1), Cell(H, R1), Cell(A, R8), Cell(H, R8))
    val horizontal = for (i <- C to F; j <- Vector(R1, R8)) yield Cell(i, j)
    val vertical = for (i <- Vector(A, H); j <- R3 to R6) yield Cell(i, j)
    val centerHorizontal =
      for (i <- C to F; j <- Vector(R3, R6)) yield Cell(i, j)
    val centerVertical = for (i <- Vector(C, F); j <- R4 to R5) yield Cell(i, j)
    val nextToHorizontal =
      for (i <- C to F; j <- Vector(R2, R7)) yield Cell(i, j)
    val nextToVertical = for (i <- Vector(B, G); j <- R3 to R6) yield Cell(i, j)
    val nextToCorner = Vector(Cell(A, R2), Cell(A, R7), Cell(B, R1),
      Cell(B, R2), Cell(B, R7), Cell(B, R8), Cell(G, R1), Cell(G, R2),
      Cell(G, R7), Cell(G, R8), Cell(H, R2), Cell(H, R7))
    corner ++ horizontal ++ vertical ++ centerHorizontal ++ centerVertical ++
      nextToHorizontal ++ nextToVertical ++ nextToCorner
  }
  // must have 60 unique cells (64 minus the 4 cells set by Board.initialSetup)
  assert(allMoves.length == allMoves.toSet.size && allMoves.length == 60)

  def apply(board: Board): GameState =
    var totalBlack = 0
    var totalWhite = 0
    for (c <- Cell.values) board.get(c) match
      case Some(Black) => totalBlack += 1
      case Some(White) => totalWhite += 1
      case _ =>
    new GameState(totalBlack, totalWhite, board)

  def apply(state: GameState): GameState =
    state.board.flipColor()
    apply(state.board)

  def apply(state: GameState, move: Cell): GameState =
    apply(new Board(state.board, move))

  private def checkValid(board: Board, c: Cell): Boolean =
    (if (c.col < C) checkHorizontal(board, c, 1)
     else if (c.col > F) checkHorizontal(board, c, -1)
     else
       checkHorizontal(board, c, 1) ||
       checkHorizontal(board, c, -1)) ||
      (c.row match
        case R1 | R2 => check(board, c, 0, 1)
        case R7 | R8 => check(board, c, 0, -1)
        case _ => check(board, c, 0, 1) || check(board, c, 0, -1)
      )

  private def checkHorizontal(board: Board, c: Cell, horizontal: Int) =
    check(board, c, horizontal, 0) || (c.row match
      case R1 | R2 => check(board, c, horizontal, 1)
      case R7 | R8 => check(board, c, horizontal, -1)
      case _ =>
        check(board, c, horizontal, 1) ||
        check(board, c, horizontal, -1)
    )

  private def check(
      board: Board, c: Cell, horizontal: Int, vertical: Int): Boolean =
    doCheck(board, c.add(horizontal, vertical), horizontal, vertical)

  private def doCheck(
      board: Board, c: Cell, horizontal: Int, vertical: Int): Boolean =
    // make sure the next in this direction is the other color and then start
    // looking for my color
    board.get(c).contains(board.color.other) && findSameColor(board, c,
      horizontal, vertical)

  @tailrec
  private def findSameColor(board: Board, cIn: Cell, horizontal: Int,
      vertical: Int): Boolean =
    if (!cIn.canAdd(horizontal, vertical)) return false
    val c = cIn.add(horizontal, vertical)
    board.get(c) match
      case Some(x) if x == board.color.other =>
        findSameColor(board, c, horizontal, vertical)
      case x => x.contains(board.color)
