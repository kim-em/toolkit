package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
import net.tqft.toolkit.algebra.matrices.Matrices

@RunWith(classOf[JUnitRunner])
class PartialFusionRingTest extends FlatSpec with ShouldMatchers {

  //  "children" should "all be singly generated" in {
  //    val pfr = PartialFusionRing(2, FusionRing(Vector(Matrix(5, IndexedSeq(IndexedSeq(1, 0, 0, 0, 0), IndexedSeq(0, 1, 0, 0, 0), IndexedSeq(0, 0, 1, 0, 0), IndexedSeq(0, 0, 0, 1, 0), IndexedSeq(0, 0, 0, 0, 1))), Matrix(5, IndexedSeq(IndexedSeq(0, 1, 0, 0, 0), IndexedSeq(0, 0, 0, 1, 0), IndexedSeq(0, 0, 0, 0, 1), IndexedSeq(0, 0, 1, 0, 0), IndexedSeq(1, 0, 0, 0, 0))), Matrix(5, IndexedSeq(IndexedSeq(0, 0, 1, 0, 0), IndexedSeq(0, 0, 0, 0, 1), IndexedSeq(0, 1, 0, 0, 0), IndexedSeq(1, 0, 0, 0, 0), IndexedSeq(0, 0, 0, 1, 0))), Matrix(5, IndexedSeq(IndexedSeq(0, 0, 0, 1, 0), IndexedSeq(0, 0, 1, 0, 0), IndexedSeq(1, 0, 0, 0, 0), IndexedSeq(0, 0, 0, 0, 1), IndexedSeq(0, 1, 0, 0, 0))), Matrix(5, IndexedSeq(IndexedSeq(0, 0, 0, 0, 1), IndexedSeq(1, 0, 0, 0, 0), IndexedSeq(0, 0, 0, 1, 0), IndexedSeq(0, 1, 0, 0, 0), IndexedSeq(0, 0, 1, 0, 0))))), 6.0)
  //    pfr.children should have size (1)
  //  }

  {
    val Zmod = {
      import net.tqft.toolkit.functions.Memo._
      { n: Int => PartialFusionRing(n / 2, FusionRings.Examples.representationsOf(FiniteGroups.cyclicGroup(n)).relabelForGenerator(), (n + 1).toDouble) }.memo
    }

    val Sym = {
      import net.tqft.toolkit.functions.Memo._
      { n: Int =>
        {
          val ring = FusionRings.Examples.representationsOf(FiniteGroups.symmetricGroup(n)).relabelForGenerator()
          val generator = ring.add(ring.basis(1), ring.basis(ring.structureCoefficients(1).entries.indexWhere(_.head == 1)))
          val depth = ring.depthWithRespectTo(generator).max
          import net.tqft.toolkit.arithmetic.Factorial
          PartialFusionRing(depth, ring, (Factorial(n) + 1).toDouble)
        }
      }.memo
    }

    "verifyAncestry" should "work on cyclic groups" in {
      for (n <- 4 to 7) {
        Zmod(n).verifyAncestry should be(true)
      }
    }
    "verifyAncestry" should "work on symmetric groups" in {
      for (n <- 2 to 4) {
        Sym(n).verifyAncestry should be(true)
      }
    }

    "progenitor" should "always be A1" in {
      for (n <- 4 to 7) {
        Zmod(n).progenitor.ring.rank should be(1)
      }
    }
  }
}
