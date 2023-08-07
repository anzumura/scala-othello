package com.github.anzumura.game

import com.github.anzumura.game.Color.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ColorSpec extends AnyFlatSpec with Matchers:
  behavior of "Color"

  it should "have expected values" in {
    values shouldBe Array(Black, White)
  }

  it should "have expected symbols" in {
    values.map(_.symbol) shouldBe Array('x', 'o')
  }

  it should "have expected ordinals" in {
    values.map(_.ordinal) shouldBe Array(0, 1)
  }

  it should "have expected ids" in {
    values.map(_.id) shouldBe Array(1, 2)
  }

  it should "return expected other value" in {
    values.map(_.other) shouldBe Array(White, Black)
  }

  it should "allow creation from id" in {
    (1 to 2).map(Color(_)) shouldBe Array(Black, White)
  }
