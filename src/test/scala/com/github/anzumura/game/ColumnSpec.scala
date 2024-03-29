package com.github.anzumura.game

import com.github.anzumura.game.Column.*
import com.github.anzumura.game.Move.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ColumnSpec extends AnyFlatSpec with Matchers:
  behavior of "Column"

  it should "have expected values" in {
    Column.values shouldBe Array(A, B, C, D, E, F, G, H)
  }

  it should "have expected ids" in {
    Column.values.map(_.id) shouldBe Array(0, 2, 4, 6, 8, 10, 12, 14)
  }

  it should "allow creation from upper case Char name" in {
    Column('A') shouldBe A
  }

  it should "allow creation from lower case Char name" in {
    Column('c') shouldBe C
  }

  it should "compare values" in {
    A should be < B
    H should be > C
  }

  it should "allow checking addition via canAdd" in {
    D.canAdd(4) shouldBe true
    D.canAdd(5) shouldBe false
    B.canAdd(-1) shouldBe true
    B.canAdd(-2) shouldBe false
  }

  it should "support addition" in {
    A + 2 shouldBe C
    G + 1 shouldBe H
  }

  it should "support subtraction" in {
    B - 1 shouldBe A
    H - 2 shouldBe F
  }

  it should "support generating a range" in {
    D to F shouldBe Vector(D, E, F)
    H to B shouldBe Vector()
    C to C shouldBe Vector(C)
  }

  it should "support canMove" in {
    (A to G).forall(_.canMove(Next)) shouldBe true
    (B to H).forall(_.canMove(Prev)) shouldBe true
    (A to H).forall(_.canMove(Same)) shouldBe true
    A.canMove(Prev) shouldBe false
    H.canMove(Next) shouldBe false
  }

  it should "support move" in {
    A.move(Next) shouldBe B
    D.move(Prev) shouldBe C
    E.move(Same) shouldBe E
  }

  it should "support unary minus move" in {
    A.move(-Prev) shouldBe B
    D.move(-Next) shouldBe C
    E.move(-Same) shouldBe E
  }
