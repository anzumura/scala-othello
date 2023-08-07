package com.github.anzumura.game

import scala.annotation.targetName

trait OrderedEnum[T <: OrderedEnum[T]] extends Ordered[T]:
  protected def maxOrdinal: Int
  protected def create(x: Int): T

  def ordinal: Int
  def canAdd(x: Int): Boolean =
    val res = ordinal + x
    res < maxOrdinal && res >= 0

  @targetName("plus") def +(x: Int): T = create(ordinal + x)
  @targetName("minus") def -(x: Int): T = create(ordinal - x)
  def to(end: T): IndexedSeq[T] = (ordinal to end.ordinal).map(create)
  override def compare(that: T): Int = ordinal - that.ordinal
