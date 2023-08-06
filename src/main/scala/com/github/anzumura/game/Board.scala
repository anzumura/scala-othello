package com.github.anzumura.game

import com.github.anzumura.game.Column.*

class Board:
  import Board.*
  import com.github.anzumura.game.Color.*

  private val cells = new Array[Char](8)
  private var turn = Black

  def this(board: Board) =
    this()
    this.turn = board.turn
    for (i <- cells.indices) this.cells(i) = board.cells(i)

  def this(board: Board, move: (Column, Int)) =
    this(board)
    set(move(0), move(1))

  def color: Color = turn

  def initialSetup(): Unit =
    turn = Black
    (0 to 2).foreach(cells(_) = 0)
    cells(3) = ((White.id << D.id) + (Black.id << E.id)).asInstanceOf[Char]
    cells(4) = ((Black.id << D.id) + (White.id << E.id)).asInstanceOf[Char]
    (5 to 7).foreach(cells(_) = 0)

  def flipColor(): Unit = turn = turn.other

  def print(state: GameState = null, boarders: Boolean = true): String =
    val res = new StringBuilder
    if (boarders) res ++= header
    for (i <- 1 to cells.length)
      res ++= (if boarders then s"|$i| " else " ")
      res ++= Column.values.map(printCell(_, i - 1, state)).mkString(" ")
      res ++= (if boarders then s" |$i|\n" else "\n")
    if (boarders) res ++= header
    res.toString

  def get(col: Column, row: Int): Option[Color] =
    (cells(row) & (Flip << col.id)).asInstanceOf[Char] >> col.id match
      case Black.id => Option(Black)
      case White.id => Option(White)
      case _ => Option.empty[Color]

  def set(col: Column, row: Int): Unit =
    val x = (cells(row) | (Flip << col.id)).asInstanceOf[Char]
    cells(row) = (x ^ (turn.other.id << col.id)).asInstanceOf[Char]
    calcFlips(col, row)
    flipColor()

  def has(col: Column, row: Int): Boolean = get(col, row).nonEmpty

  private def printCell(col: Column, row: Int, state: GameState): Char =
    get(col, row) match
      case Some(c) => c.symbol
      case _ => if (state != null && state.isValid(col, row)) '*' else '.'

  private def calcFlips(col: Column, row: Int): Unit =
    if (col > B) flipHorizontal(col, row, -1)
    if (col < G) flipHorizontal(col, row, 1)
    if (row > 1) flipDirection(col, row, 0, -1)
    if (row < 6) flipDirection(col, row, 0, 1)

  private def flipHorizontal(col: Column, row: Int, horizontal: Int): Unit =
    flipDirection(col, row, horizontal, 0)
    if (row > 1) flipDirection(col, row, horizontal, -1)
    if (row < 6) flipDirection(col, row, horizontal, 1)

  private def flipDirection(col: Column, row: Int, horizontal: Int,
      vertical: Int): Unit =
    doFlipDirection(col + horizontal, row + vertical, horizontal, vertical)

  private def doFlipDirection(col: Column, row: Int, horizontal: Int,
      vertical: Int): Unit =
    if (
      !get(col, row).contains(turn) && flipFound(col, row, horizontal, vertical)
    ) flipCell(col, row)

  private def flipFound(colIn: Column, rowIn: Int, horizontal: Int,
      vertical: Int): Boolean =
    val row = rowIn + vertical
    if (!colIn.canAdd(horizontal) || row < 0 || row > 7) return false
    val col = colIn + horizontal
    val cell = get(col, row)
    // while cells are opposite color then recurse until my color is found
    if (cell.contains(turn.other))
      if (flipFound(col, row, horizontal, vertical))
        flipCell(col, row) // flip on the way back
        return true
      return false
    cell.contains(turn)

  private def flipCell(col: Column, row: Int): Unit =
    cells(row) = (cells(row) ^ (Flip << col.id)).asInstanceOf[Char]

object Board:
  // for printing
  private val Border = "+-+-----------------+-+\n"
  private val LeftEdge = "| | "
  private val RightEdge = " | |\n"
  // for flipping cells and changing the current turn
  private val Flip = 3

  private val header = Border + LeftEdge + Column.values.mkString(" ") +
    RightEdge + Border
