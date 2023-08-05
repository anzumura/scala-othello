package com.github.anzumura.game

import com.github.anzumura.game.Column.*

class Board:
  import Board.*

  private val cells = new Array[Char](8)
  private var turn: Int = Black

  def this(board: Board) =
    this()
    this.turn = board.turn
    for (i <- cells.indices) this.cells(i) = board.cells(i)

  def this(board: Board, move: (Int, Int)) =
    this(board)
    setCell(move._1, move._2)

  def currentColor: Int = turn

  def otherColor: Int = turn ^ Flip

  def initialSetup(): Unit =
    cells(3) = ((White << D.id) + (Black << E.id)).asInstanceOf[Char]
    cells(4) = ((Black << D.id) + (White << E.id)).asInstanceOf[Char]

  def flipColor(): Unit = turn = otherColor

  def printBoard(state: GameState = null, boarders: Boolean = true): String =
    val res = new StringBuilder
    if (boarders) res ++= header
    for (i <- 1 to cells.length)
      res ++= (if boarders then s"|$i| " else " ")
      res ++= Column.ids.map(printCell(_, i - 1, state)).mkString(" ")
      res ++= (if boarders then s" |$i|\n" else "\n")
    if (boarders) res ++= header
    res.toString

  private def printCell(col: Int, row: Int, state: GameState): Char =
    getCell(col, row) match
      case Black => BlackSymbol
      case White => WhiteSymbol
      case _ => if (state != null && state.isValid(col, row)) '*' else '.'

  def getCell(col: Int, row: Int): Int = (cells(row) & (Flip << col))
    .asInstanceOf[Char] >> col

  def currentPlayer: String = if (turn == Black) "black" else "white"

  def setCell(col: Int, row: Int): Unit =
    val x = (cells(row) | (Flip << col)).asInstanceOf[Char]
    cells(row) = (x ^ (otherColor << col)).asInstanceOf[Char]
    calcFlips(col, row)
    turn = otherColor

  private def calcFlips(col: Int, row: Int): Unit =
    if (col > B.id) flipHorizontal(col, row, -2)
    if (col < G.id) flipHorizontal(col, row, 2)
    if (row > 1) flipDirection(col, row, 0, -1)
    if (row < 6) flipDirection(col, row, 0, 1)

  private def flipHorizontal(col: Int, row: Int, horizontal: Int): Unit =
    flipDirection(col, row, horizontal, 0)
    if (row > 1) flipDirection(col, row, horizontal, -1)
    if (row < 6) flipDirection(col, row, horizontal, 1)

  private def flipDirection(
      col: Int, row: Int, horizontal: Int, vertical: Int): Unit =
    doFlipDirection(col + horizontal, row + vertical, horizontal, vertical)

  private def doFlipDirection(col: Int, row: Int, horizontal: Int,
      vertical: Int): Unit =
    if (
      getCell(col, row) != turn &&
      flipFound(col + horizontal, row + vertical, horizontal, vertical)
    ) flipCell(col, row)

  private def flipFound(
      col: Int, row: Int, horizontal: Int, vertical: Int): Boolean =
    if (col < A.id || col > H.id || row < 0 || row > 7) return false
    val cell = getCell(col, row)
    // while cells are opposite color then recurse until my color is found
    if (cell == otherColor)
      if (flipFound(col + horizontal, row + vertical, horizontal, vertical))
        flipCell(col, row) // flip on the way back
        return true
      return false
    cell == turn

  private def flipCell(col: Int, row: Int): Unit =
    cells(row) = (cells(row) ^ (Flip << col)).asInstanceOf[Char]

object Board:
  val BlackSymbol = 'x'
  val WhiteSymbol = 'o'
  // cell values
  val Empty = 0
  val Black = 1
  val White = 2
  // for printing
  private val Border = "+-+-----------------+-+\n"
  private val LeftEdge = "| | "
  private val RightEdge = " | |\n"
  // for flipping cells and changing the current turn
  private val Flip = 3

  private val header = Border + LeftEdge + Column.values.mkString(" ") +
    RightEdge + Border
