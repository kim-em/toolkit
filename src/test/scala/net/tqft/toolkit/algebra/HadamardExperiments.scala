package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HadamardExperiments extends FlatSpec with ShouldMatchers {

  val generators = Set(((Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), IndexedSeq(0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1)), (Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), IndexedSeq(0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1))), ((Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), IndexedSeq(0, 1, 4, 7, 10, 2, 5, 8, 11, 3, 6, 9)), (Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), IndexedSeq(0, 1, 4, 7, 10, 2, 5, 8, 11, 3, 6, 9))), ((Seq(1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1), IndexedSeq(1, 0, 11, 6, 8, 9, 3, 10, 4, 5, 7, 2)), (Seq(0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1), IndexedSeq(1, 0, 11, 6, 8, 9, 3, 10, 4, 5, 7, 2))), ((Seq(0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0), IndexedSeq(0, 1, 6, 3, 4, 2, 10, 11, 7, 8, 5, 9)), (Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), IndexedSeq(1, 0, 11, 2, 7, 3, 8, 6, 4, 10, 9, 5))))
//  val h12automorphisms = FiniteGroups.product(FiniteGroups.signedPermutationGroup(12), FiniteGroups.signedPermutationGroup(12)).subgroupGeneratedBy(generators)
  val half = FiniteGroups.signedPermutationGroup(12).subgroupGeneratedBy(generators.map(_._1))
  
  "the 12x12 Hadamard automorphism group" should "have the right size" in {
//   h12automorphisms.size should equal(190080)
   half.size should equal(190080)
  }
  
  "the 12x12 Hadamard automorphism group" should "be able to compute a character table" in {
   println(half.characterTable)
   println(half.tensorProductMultiplicities)
  }
}
