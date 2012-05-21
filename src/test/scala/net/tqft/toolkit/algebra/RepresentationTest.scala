package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RepresentationTest extends FlatSpec with ShouldMatchers {

  "the permutation representation of S_3" should "include one copy of the trivial representation" in {
    implicit val rationals = Gadgets.Rationals
    import Implicits.integersAsRationals;
    val S_3 = FiniteGroups.symmetricGroup(3)
    val V = Representations.permutationRepresentation(S_3)
    V.irrepMultiplicities should equal(Seq(1,0,1))
    V.basisForIsotypicComponent(Seq[Fraction[Int]](1,1,1)).size should equal(1)
    for((c, n) <- S_3.reducedCharacters zip V.irrepMultiplicities; if c.isInstanceOf[S_3.RationalCharacter]; lc = c.asInstanceOf[S_3.RationalCharacter]) {
      println(lc.character)
      V.basisForIsotypicComponent(lc.character).size should equal(n)
    }
  }
  "the tensor square of the permutation representation of S_3" should "include two copies of the trivial representation" in {
    implicit val rationals = Gadgets.Rationals
    import Implicits.integersAsRationals;
//    Representations.tensorPower(Representations.permutationRepresentation(3), 2).basisForIsotypicComponent(Seq(1, 1, 1)) should have size (2)
  }

}
