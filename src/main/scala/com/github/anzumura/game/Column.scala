package com.github.anzumura.game

import scala.annotation.targetName

enum Column extends Ordered[Column]:
  case A, B, C, D, E, F, G, H

  val id: Int = ordinal * 2

  def canAdd(x: Int): Boolean =
    val res = ordinal + x
    res < Column.values.length && res >= 0

  @targetName("plus") def +(x: Int): Column =
    Column.fromOrdinal(ordinal + x)

  @targetName("minus") def -(x: Int): Column =
    Column.fromOrdinal(ordinal - x)

  def to(end: Column): IndexedSeq[Column] =
    (ordinal to end.ordinal).map(Column.fromOrdinal)

  override def compare(that: Column): Int = this.ordinal - that.ordinal

object Column:
  def apply(c: Char): Column = Column.valueOf(c.toUpper.toString)
