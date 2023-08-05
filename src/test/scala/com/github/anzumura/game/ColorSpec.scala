package com.github.anzumura.game

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ColorSpec extends AnyFlatSpec with should.Matchers:
  import com.github.anzumura.game.Color.*
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
