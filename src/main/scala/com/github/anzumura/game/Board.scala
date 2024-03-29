package com.github.anzumura.game

import com.github.anzumura.game.Color.*
import com.github.anzumura.game.Column.*
import com.github.anzumura.game.Move.*
import com.github.anzumura.game.Row.*

class Board:
  import Board.*

  private val cells = new Array[Char](8)
  private var turn = Black

  def this(board: Board) =
    this()
    turn = board.turn
    cells.indices.foreach(i => cells(i) = board.cells(i))

  def this(board: Board, move: Cell) =
    this(board)
    set(move)

  inline def color: Color = turn

  def initialSetup(): Unit =
    turn = Black
    cells.indices.foreach(i => cells(i) = InitialSetup(i))

  inline def flipColor(): Unit = turn = turn.other

  def print(state: GameState = null, boarders: Boolean = true): String =
    val res = new StringBuilder
    if (boarders) res ++= Header
    for (i <- 1 to cells.length)
      res ++= (if boarders then s"|$i| " else " ")
      res ++= Column.values
        .map(j => printCell(Cell(j, Row(i)), state))
        .mkString(" ")
      res ++= (if boarders then s" |$i|\n" else "\n")
    if (boarders) res ++= Header
    res.toString

  def get(c: Cell): Option[Color] =
    (cells(c.rId) & (Flip << c.cId)).toChar >> c.cId match
      case 0 => Option.empty[Color]
      case x => Option(Color(x))

  def set(c: Cell): Unit =
    cellOrEqual(c.rId, turn.id << c.cId)
    if (c.col > B) flipHorizontal(c, Prev)
    if (c.col < G) flipHorizontal(c, Next)
    if (c.row > R2) flipDirection(c, Same, Prev)
    if (c.row < R7) flipDirection(c, Same, Next)
    flipColor()

  inline def has(c: Cell): Boolean = get(c).nonEmpty

  // create a compact representation for debugging, actual game uses 'print'
  override def toString: String =
    val res = new StringBuilder("turn=" + turn)
    for (i <- cells.indices)
      res ++= s"\n${i + 1}=${(for (j <- 0 to 14 by 2)
          yield (cells(i) & (Flip << j)) >> j).mkString(" ")}"
    res.toString

  private def printCell(c: Cell, state: GameState): Char =
    get(c)
      .map(_.symbol)
      .getOrElse(if state != null && state.isValid(c) then '*' else '.')

  private def flipHorizontal(c: Cell, colMove: Move): Unit =
    flipDirection(c, colMove, Same)
    if (c.row > R2) flipDirection(c, colMove, Prev)
    if (c.row < R7) flipDirection(c, colMove, Next)

  private def flipDirection(cIn: Cell, colMove: Move, rowMove: Move): Unit =
    val c = cIn.move(colMove, rowMove)
    if (get(c).contains(turn.other) && flipFound(c, colMove, rowMove))
      flipCell(c)

  private def flipFound(cIn: Cell, colMove: Move, rowMove: Move): Boolean =
    if (cIn.canMove(colMove, rowMove))
      val c = cIn.move(colMove, rowMove)
      get(c) match
        case None => false
        case Some(x) if x == turn => true
        case _ => // if other color, recurse until my color or empty is found
          if (flipFound(c, colMove, rowMove))
            flipCell(c) // flip on the way back
            true
          else false
    else false

  inline private def flipCell(c: Cell): Unit =
    cellXOrEqual(c.rId, Flip << c.cId)

  inline private def cellOrEqual(index: Int, value: Int): Unit =
    cells(index) = (cells(index) | value).toChar

  inline private def cellXOrEqual(index: Int, value: Int): Unit =
    cells(index) = (cells(index) ^ value).toChar

object Board:
  // for getting and flipping cells
  inline private val Flip = 3
  // for printing
  private val Header = {
    val border = "+-+-----------------+-+\n"
    border + "| | " + Column.values.mkString(" ") + " | |\n" + border
  }
  // array used for initial board setup
  private val InitialSetup =
    Array(0, 0, 0, (White.id << D.id) + (Black.id << E.id),
      (Black.id << D.id) + (White.id << E.id), 0, 0, 0).map(_.toChar)
