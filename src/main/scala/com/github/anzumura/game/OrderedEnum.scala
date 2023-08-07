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

// for moving by single steps (like in Board class when doing flips)
enum Move:
  case Prev, Same, Next

  inline def value: Int = ordinal - 1

  @targetName("unary_minus") def unary_- : Move =
    if this == Prev then Next
    else if this == Next then Prev
    else this

// can be mixed in to an enum to allow type safe moving by single steps
trait MovableEnum[T <: OrderedEnum[T]] extends OrderedEnum[T]:
  inline def canMove(move: Move): Boolean = canAdd(move.value)
  inline def move(move: Move): T = this + move.value
