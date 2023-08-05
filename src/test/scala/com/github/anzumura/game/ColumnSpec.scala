package com.github.anzumura.game

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ColumnSpec extends AnyFlatSpec with should.Matchers:
  import com.github.anzumura.game.Column.*
  behavior of "Column"

  it should "have expected values" in {
    Column.values shouldBe Array(A, B, C, D, E, F, G, H)
  }

  it should "have expected ids" in {
    Column.ids shouldBe Array(0, 2, 4, 6, 8, 10, 12, 14)
  }

  it should "allow creation from upper case Char name" in {
    Column('A') shouldBe A
  }

  it should "allow creation from lower case Char name" in {
    Column('c') shouldBe C
  }

  it should "allow creation from Int id" in {
    Column(12) shouldBe G
  }
