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
    println(half.conjugacyClasses.map(_.size))
  }

  "the automorphism group of H_12" should "have one copy of each of the 11d irreps in the tensor square of the 12d irrep" in {

    import Implicits.integersAsRationals
    import Implicits.Rationals
    import Implicits.Integers;

//    val chi4 : Seq[Fraction[Int]] = Seq(11, 11, 3, 3, -1, 2, 2, -1, -1, -1, 3, 0, 0, 0,
//      0, -1, -1, 1, 1, -1, 1, -1, 1, -1, 0, 0)
//    val chi5 = Seq(11, 11, 3, 3, -1, 2, 2, -1, -1, 3, -1, 0, 0, 0,
//      0, -1, -1, 1, 1, 1, -1, 1, -1, -1, 0, 0)
//    Representations.tensorPower(Representations.signedPermutationRepresentation(half)(Integers), 2).basisForIsotypicComponent(chi4) should have size (1)
//    Representations.tensorPower(Representations.signedPermutationRepresentation(half)(Integers), 2).basisForIsotypicComponent(chi5) should have size (1)
  }

  "the automorphism group of H_12" should "have 18 copies of the 54d irrep in the 4-th tensor power of the 12d irrep" in {

    import Implicits.integersAsRationals
    import Implicits.Rationals
    import Implicits.Integers

    val chi13 = Seq(54, 54, 6, 6, 6, 0, 0, 0, 0, 2, 2, -1, -1, -1, -1, 1, 1, -1, -1, 0,
      0, 0, 0, 0, 0, 0)

//    Representations.tensorPower(Representations.signedPermutationRepresentation(half)(Integers), 4).basisForIsotypicComponent(chi13) should have size (18)
  }
  //  "the 12x12 Hadamard automorphism group" should "be able to compute a character table" in {
  //   println(half.characterTable)
  //   println(half.tensorProductMultiplicities)
  //  }
}
