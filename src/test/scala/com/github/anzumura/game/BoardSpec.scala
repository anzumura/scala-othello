package com.github.anzumura.game

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BoardSpec extends AnyFlatSpec with should.Matchers {
  import Board.*

  behavior of "Board"

  it should "start with current player set to Black" in {
    val b = new Board
    b.turn shouldBe Board.Black
  }
}
