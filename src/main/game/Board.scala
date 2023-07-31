package main.game

import main.game.Column.*

class Board:

  import Board.*

  private val cells = new Array[Char](8)

  // used by GameState
  var turn: Int = Black

  def otherColor: Int = turn ^ Flip

  def this(board: Board) =
    this()
    this.turn = board.turn
    for (i <- cells.indices)
      this.cells(i) = board.cells(i)

  def this(board: Board, move: (Int, Int)) =
    this(board)
    setCell(move._1, move._2)

  def initialSetup(): Unit =
    cells(3) = ((White << D.id) + (Black << E.id)).asInstanceOf[Char]
    cells(4) = ((Black << D.id) + (White << E.id)).asInstanceOf[Char]

  def playerNumber: Int = if (turn == Black) 0 else 1

  def flipColor(): Unit = turn = otherColor

  def printBoard(state: GameState = null, boarders: Boolean = true): Unit =
    if (boarders)
      println("\n" + Border)
      println(LeftEdge + (Column.values mkString " ") + RightEdge)
      println(Border)
    for (i <- 1 to cells.length)
      if (boarders)
        print("|" + i + "|")
      for (j <- Column.values)
        printCell(j.id, i - 1, state)
      println(if boarders then " |" + i + "|" else "")
    if (boarders)
      println(Border)
      println(LeftEdge + (Column.values mkString " ") + RightEdge)
      println(Border)

  def currentPlayer(): String = if (turn == Black) "black" else "white"

  def getCell(col: Int, row: Int): Int =
    var x = cells(row)
    x = (x & (Flip << col)).asInstanceOf[Char]
    x >> col

  def setCell(col: Int, row: Int): Unit =
    var x = cells(row)
    x = (x | (Flip << col)).asInstanceOf[Char]
    cells(row) = (x ^ (otherColor << col)).asInstanceOf[Char]
    calcFlips(col, row)
    turn = otherColor

  private def calcFlips(col: Int, row: Int): Unit =
    if (col > B.id)
      flipHorizontal(col, row, -2)
    if (col < G.id)
      flipHorizontal(col, row, 2)
    if (row > 1)
      flipDirection(col, row, 0, -1)
    if (row < 6)
      flipDirection(col, row, 0, 1)

  private def flipHorizontal(col: Int, row: Int, horizontal: Int): Unit =
    flipDirection(col, row, horizontal, 0)
    if (row > 1)
      flipDirection(col, row, horizontal, -1)
    if (row < 6)
      flipDirection(col, row, horizontal, 1)

  private def flipDirection(col: Int, row: Int, horizontal: Int,
                            vertical: Int): Unit =
    doFlipDirection(col + horizontal, row + vertical, horizontal, vertical)

  private def doFlipDirection(col: Int, row: Int, horizontal: Int,
                              vertical: Int): Unit =
    if (getCell(col, row) != turn && flipFound(col + horizontal, row +
      vertical, horizontal, vertical))
      flipCell(col, row)

  private def flipFound(col: Int, row: Int, horizontal: Int, vertical: Int)
  : Boolean =
    if (col < A.id || col > H.id || row < 0 || row > 7)
      return false
    val cell = getCell(col, row)
    // while the cells are the opposite color then recurse until my color is
    // found
    if (cell == otherColor)
      if (flipFound(col + horizontal, row + vertical, horizontal, vertical))
        flipCell(col, row) // flip on the way back
        return true
      return false
    cell == turn

  private def flipCell(col: Int, row: Int): Unit =
    var x = cells(row)
    x = (x ^ (Flip << col)).asInstanceOf[Char]
    cells(row) = x

  private def printCell(col: Int, row: Int, state: GameState): Unit =
    val cell = getCell(col, row)
    val char = cell match
      case Black => BlackSymbol
      case White => WhiteSymbol
      case _ => if (state != null && state.isValid(col, row)) '*' else '.'
    print(" " + char)

object Board:
  // create a Board populated from an Array of Array[Int] (not all rows need
  // to be specified)
  def apply(x: Array[Int]*): Board =
    val b = new Board
    var i = 0
    for (row <- x)
      var value = 0
      for (cell <- row.reverse)
        value = (value << 2) + cell
      b.cells(i) = value.asInstanceOf[Char]
      i += 1
    b

  // for printing
  private val Border = "+-+-----------------+-+"
  private val LeftEdge = "| | "
  private val RightEdge = " | |"
  val BlackSymbol = 'x'
  val WhiteSymbol = 'o'
  // cell values
  val Empty = 0
  val Black = 1
  val White = 2
  // for flipping cells and changing the current turn
  private val Flip = 3
