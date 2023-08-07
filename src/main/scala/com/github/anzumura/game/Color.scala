package com.github.anzumura.game

enum Color(val symbol: Char):
  case Black extends Color('x')
  case White extends Color('o')

  val id: Int = ordinal + 1

  lazy val other: Color = if symbol == 'x' then White else Black

  override val toString: String = if symbol == 'x' then "black" else "white"

object Color:
  def apply(id: Int): Color = Color.fromOrdinal(id - 1)
