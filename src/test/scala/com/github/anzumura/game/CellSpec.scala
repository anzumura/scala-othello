package com.github.anzumura.game

import com.github.anzumura.game.Column.*
import com.github.anzumura.game.Row.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CellSpec extends AnyFlatSpec with Matchers:
  private val D3 = Cell(D, R3)

  behavior of "Cell"

  it should "have expected column id" in {
    D3.cId shouldBe D3.col.id
  }

  it should "have expected row id" in {
    D3.rId shouldBe D3.row.ordinal
  }

  it should "return all expected values" in {
    Cell.values.toSet.size shouldBe 64
  }

  it should "support canAdd" in {
    D3.canAdd(4, 5) shouldBe true
    D3.canAdd(-3, 5) shouldBe true
    D3.canAdd(5, 3) shouldBe false
    D3.canAdd(-2, -6) shouldBe false
  }

  it should "support add" in {
    D3.add(2, 3) shouldBe Cell(F, R6)
    D3.add(-3, -1) shouldBe Cell(A, R2)
  }

  it should "convert to expected string value" in {
    D3.toString shouldBe "D3"
    Cell(H, R2).toString shouldBe "H2"
  }

  it should "correctly identify edge (A and H) columns" in {
    Cell(A, R2).isEdgeCol shouldBe true
    Cell(H, R4).isEdgeCol shouldBe true
    Cell(B, R1).isEdgeCol shouldBe false
    Cell(G, R8).isEdgeCol shouldBe false
  }

  it should "correctly identify edge (R1 and R8) rows" in {
    Cell(A, R2).isEdgeRow shouldBe false
    Cell(H, R4).isEdgeRow shouldBe false
    Cell(B, R1).isEdgeRow shouldBe true
    Cell(G, R8).isEdgeRow shouldBe true
  }

  it should "correctly identify corners" in {
    Cell.values.filter(_.isCorner) shouldBe Array(Cell(A, R1), Cell(A, R8),
      Cell(H, R1), Cell(H, R8))
  }

  it should "correctly identify center cells" in {
    val res = Cell.values.filter(_.isCenter)
    res.length shouldBe 16
    res shouldBe (for (i <- C to F; j <- R3 to R6) yield Cell(i, j))
  }

  it should "correctly identify next-to-edge cells" in {
    val expected = {
      val r2 = for (i <- C to F) yield Cell(i, R2)
      val r7 = for (i <- C to F) yield Cell(i, R7)
      val c2 = for (i <- R3 to R6) yield Cell(B, i)
      val c7 = for (i <- R3 to R6) yield Cell(G, i)
      r2 ++ r7 ++ c2 ++ c7
    }.toSet
    val res = Cell.values.filter(_.isNextToEdge)
    res.length shouldBe 16
    res.toSet shouldBe expected
  }
