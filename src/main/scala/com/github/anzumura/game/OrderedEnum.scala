package com.github.anzumura.game

import scala.annotation.targetName

trait OrderedEnum[T <: OrderedEnum[T]] extends Ordered[T]:
  protected def maxOrdinal: Int
  protected def create(x: Int): T

  def ordinal: Int
  override def compare(that: T): Int = ordinal - that.ordinal

  def canAdd(x: Int): Boolean = x == 0 || {
    val res = ordinal + x
    res < maxOrdinal && res >= 0
  }

  @targetName("plus") inline def +(x: Int): T = create(ordinal + x)
  @targetName("minus") inline def -(x: Int): T = create(ordinal - x)
  inline def to(end: T): Vector[T] =
    ordinal.to(end.ordinal).map(create).toVector

// for moving by single steps (like in Board class when doing flips)
enum Move:
  case Prev, Same, Next

  inline def value: Int = ordinal - 1

  @targetName("unary_minus") def unary_- : Move =
    if this == Prev then Next
    else if this == Next then Prev
    else this

// can be mixed in to an enum to allow type safe moving by single steps
trait MovableEnum[T <: MovableEnum[T]] extends OrderedEnum[T]:
  inline def canMove(move: Move): Boolean = canAdd(move.value)
  inline def move(move: Move): T = this + move.value
