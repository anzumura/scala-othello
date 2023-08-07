package com.github.anzumura.game

enum Column extends OrderedEnum[Column]:
  case A, B, C, D, E, F, G, H

  protected def maxOrdinal: Int = Column.values.length
  protected def create(x: Int): Column = Column.fromOrdinal(x)

  val id: Int = ordinal * 2

object Column:
  def apply(c: Char): Column = Column.valueOf(c.toUpper.toString)
