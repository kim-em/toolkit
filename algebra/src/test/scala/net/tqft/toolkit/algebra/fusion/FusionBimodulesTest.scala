package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

import net.tqft.toolkit.algebra.matrices._

@RunWith(classOf[JUnitRunner])
class FusionBimodulesTest extends FlatSpec with ShouldMatchers {

//  "withGenerator" should "correctly find all fusion bimodules with a given generator" in {
//    // this is Asaeda-Haagerup
//    
//    val leftGenerator: Matrix[Int] = List(List(1, 0, 0, 0, 0, 0), List(1, 1, 0, 0, 0, 0), List(0, 1, 1, 0, 0, 0), List(0, 0, 1, 1, 0, 0), List(0, 0, 1, 0, 1, 0), List(0, 0, 0, 1, 0, 1), List(0, 0, 0, 0, 1, 0), List(0, 0, 0, 0, 1, 0), List(0, 0, 0, 0, 0, 1))
//    val rightGenerator: Matrix[Int] = List(List(1, 0, 0, 0, 0, 0), List(1, 1, 0, 0, 0, 0), List(0, 1, 1, 0, 0, 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 0, 0, 1, 1))
//    val leftDuality = IndexedSeq(0, 1, 2, 3, 4, 6, 5, 7, 8)
//    val rightDuality = (0 until 6).toIndexedSeq
//
//    FusionBimodules.withGenerator(leftGenerator, rightGenerator, leftDuality, rightDuality).size should equal(1)
//  }
  
  "withGenerator" should "correctly find all fusion bimodules with a given generator, in string format" in {
    FusionBimodules.withGenerator("bwd1v1v1v1v1v1p1v1x0p0x1v1x0p0x1p0x1v1x0x0v1duals1v1v1v1x2v2x1x3v1", "bwd1v1v1v1v1v1p1v0x1p0x1v0x1v1duals1v1v1v1x2v1").size should equal(1)
  }
}
