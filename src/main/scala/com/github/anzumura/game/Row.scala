package com.github.anzumura.game

enum Row extends OrderedEnum[Row] with MovableEnum[Row]:
  case R1, R2, R3, R4, R5, R6, R7, R8

  protected def maxOrdinal: Int = Row.values.length
  protected def create(x: Int): Row = Row.fromOrdinal(x)

  override def toString: String = (ordinal + 1).toString

object Row:
  def apply(c: Int): Row = Row.fromOrdinal(c - 1)
