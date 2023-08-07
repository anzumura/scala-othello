package com.github.anzumura.game

import com.github.anzumura.game.Column.*
import com.github.anzumura.game.Row.*

case class Cell(col: Column, row: Row):
  // rId and cId are used for converting to and from Board.cells 'Char' array
  inline def cId: Int = col.id
  inline def rId: Int = row.ordinal

  inline def canMove(colMove: Move, rowMove: Move): Boolean =
    col.canMove(colMove) && row.canMove(rowMove)

  inline def move(colMove: Move, rowMove: Move): Cell =
    Cell(col.move(colMove), row.move(rowMove))

  override def toString: String = "" + col + row

  inline def isEdgeCol: Boolean = col == A || col == H
  inline def isEdgeRow: Boolean = row == R1 || row == R8

  // cell is one of the four corners
  inline def isCorner: Boolean = isEdgeCol && isEdgeRow

  // cell is in the center block of 16, i.e., not an edge nor next to an edge
  inline def isCenter: Boolean = col > B && col < G && row > R2 && row < R7

  // cell is next to an edge, but not diagonal from a corner. The 16 cells are:
  // - four in the second row
  // - four in the second column
  // - four in the second last row
  // - four in the second last column
  inline def isNextToEdge: Boolean = (col == B || col == G) && row > R2 &&
    row < R7 || (row == R2 || row == R7) && col > B && col < G

object Cell:
  val values: Array[Cell] =
    for (i <- Column.values; j <- Row.values) yield Cell(i, j)
  assert(values.length == 64)
