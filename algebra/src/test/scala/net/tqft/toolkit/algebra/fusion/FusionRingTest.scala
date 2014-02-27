package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import net.tqft.toolkit.algebra.matrices.Matrix

@RunWith(classOf[JUnitRunner])
class FusionRingTest extends FlatSpec with ShouldMatchers {

  "relabel" should "do nothing, when the labelling is an automorphism" in {
    val ring = FusionRing(List[Matrix[Int]](
      List(List(1, 0, 0, 0), List(0, 1, 0, 0), List(0, 0, 1, 0), List(0, 0, 0, 1)),
      List(List(0, 1, 0, 0), List(1, 0, 1, 1), List(0, 1, 0, 0), List(0, 1, 0, 0)),
      List(List(0, 0, 1, 0), List(0, 1, 0, 0), List(1, 0, 0, 0), List(0, 0, 0, 0)),
      List(List(0, 0, 0, 1), List(0, 1, 0, 0), List(0, 0, 0, 0), List(1, 0, 0, 0))))
    ring.relabel(IndexedSeq(0, 1, 3, 2)) should equal(ring)
  }

  "canonicalRelabelling" should "not break associativity" in {
    FusionRings.Examples.AH1.canonicalRelabelling().verifyAssociativity should be(true)
  }

  "globalDimensionLowerBound" should "be large when there are large multiplicities" in {
    val ring = FusionRing(List(Matrix(6, List(List(1, 0, 0, 0, 0, 0), List(0, 1, 0, 0, 0, 0), Vector(0, 0, 1, 0, 0, 0), Vector(0, 0, 0, 1, 0, 0), Vector(0, 0, 0, 0, 1, 0), Vector(0, 0, 0, 0, 0, 1))), Matrix(6, List(List(0, 1, 0, 0, 0, 0), List(1, 0, 1, 1, 0, 0), Vector(0, 1, 0, 0, 4, 0), Vector(0, 1, 0, 0, 0, 0), Vector(0, 0, 4, 0, 0, 51), Vector(0, 0, 0, 0, 51, 0))), Matrix(6, Vector(Vector(0, 0, 1, 0, 0, 0), Vector(0, 1, 0, 0, 4, 0), Vector(1, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 1, 0), Vector(0, 4, 0, 1, 0, 0), Vector(0, 0, 0, 0, 0, 0))), Matrix(6, Vector(Vector(0, 0, 0, 1, 0, 0), Vector(0, 1, 0, 0, 0, 0), Vector(0, 0, 0, 0, 1, 0), Vector(1, 0, 0, 0, 0, 0), Vector(0, 0, 1, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0))), Matrix(6, Vector(Vector(0, 0, 0, 0, 1, 0), Vector(0, 0, 4, 0, 0, 51), Vector(0, 4, 0, 1, 0, 0), Vector(0, 0, 1, 0, 0, 0), Vector(1, 0, 0, 0, 0, 0), Vector(0, 51, 0, 0, 0, 0))), Matrix(6, Vector(Vector(0, 0, 0, 0, 0, 1), Vector(0, 0, 0, 0, 51, 0), Vector(0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0), Vector(0, 51, 0, 0, 0, 0), Vector(1, 0, 0, 0, 0, 0)))))
    ring.globalDimensionLowerBound > 50 should be(true)
  }

  "independent_?" should "say no" in {
    val ring = FusionRing(Seq(
      Matrix(6, Seq(
        List(1, 0, 0, 0, 0, 0),
        List(0, 1, 0, 0, 0, 0),
        List(0, 0, 1, 0, 0, 0),
        List(0, 0, 0, 1, 0, 0),
        List(0, 0, 0, 0, 1, 0),
        List(0, 0, 0, 0, 0, 1))),
      Matrix(6, Seq(
        List(0, 1, 0, 0, 0, 0),
        List(0, 0, 1, 0, 0, 0),
        List(1, 0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 0, 0),
        List(0, 0, 0, 1, 0, 0))),
      Matrix(6, Seq(
        List(0, 0, 1, 0, 0, 0),
        List(1, 0, 0, 0, 0, 0),
        List(0, 1, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 0, 0),
        List(0, 0, 0, 1, 0, 0))),
      Matrix(6, Seq(
        List(0, 0, 0, 1, 0, 0),
        List(0, 0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 0, 1),
        List(1, 0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0, 0),
        List(0, 1, 1, 0, 0, 0))),
      Matrix(6, Seq(
        List(0, 0, 0, 0, 1, 0),
        List(0, 0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0, 0),
        List(1, 0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0, 0))),
      Matrix(6, Seq(
        List(0, 0, 0, 0, 0, 1),
        List(0, 0, 0, 1, 0, 0),
        List(0, 0, 0, 1, 0, 0),
        List(0, 1, 1, 0, 0, 0),
        List(0, 0, 0, 0, 0, 0),
        List(1, 0, 0, 0, 0, 0)))))
        
        ring.independent_?(Set(1,2,3,4,5)) should equal(false)
  }
}
