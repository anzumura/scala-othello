package com.github.anzumura.game

import com.github.anzumura.game.Row.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RowSpec extends AnyFlatSpec with Matchers:
  behavior of "Row"

  it should "have expected values" in {
    values shouldBe Array(R1, R2, R3, R4, R5, R6, R7, R8)
  }

  it should "have expected string values" in {
    values.map(_.toString) shouldBe Array("1", "2", "3", "4", "5", "6", "7",
      "8")
  }

  it should "allow creation from 1 based index" in {
    (1 to 8).map(Row(_)) shouldBe values
  }

  it should "compare values" in {
    R1 should be < R2
    R8 should be > R3
  }

  it should "allow checking addition via canAdd" in {
    R4.canAdd(4) shouldBe true
    R4.canAdd(5) shouldBe false
    R2.canAdd(-1) shouldBe true
    R2.canAdd(-2) shouldBe false
  }

  it should "support addition" in {
    R1 + 2 shouldBe R3
    R7 + 1 shouldBe R8
  }

  it should "support subtraction" in {
    R2 - 1 shouldBe R1
    R8 - 2 shouldBe R6
  }

  it should "support generating a range" in {
    R4 to R6 shouldBe Vector(R4, R5, R6)
    R8 to R2 shouldBe Vector()
    R3 to R3 shouldBe Vector(R3)
  }
