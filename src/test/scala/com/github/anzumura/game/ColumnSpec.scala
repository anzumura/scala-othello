package com.github.anzumura.game

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ColumnSpec extends AnyFlatSpec with should.Matchers:
  behavior of "Column"

  it should "have expected values" in {
    import com.github.anzumura.game.Column.*
    Column.values shouldBe Set(A, B, C, D, E, F, G, H)
  }

  it should "have expected ids" in {
    Column.ids shouldBe List(0, 2, 4, 6, 8, 10, 12, 14)
  }
