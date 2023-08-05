package com.github.anzumura.game

enum Column:
  case A, B, C, D, E, F, G, H

  val id: Int = ordinal * 2

object Column:
  val ids: Array[Int] = values.map(_.id)

  def apply(id: Int): Column = Column.fromOrdinal(id / 2)

  def apply(c: Char): Column = Column.valueOf(c.toUpper.toString)
