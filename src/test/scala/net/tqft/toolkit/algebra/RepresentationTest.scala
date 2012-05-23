package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RepresentationTest extends FlatSpec with ShouldMatchers {

  val S_3 = FiniteGroups.symmetricGroup(3)
  implicit val rationals = Gadgets.Rationals
  import Implicits.integersAsRationals;

  "the permutation representation of S_3" should "include one copy of the trivial representation" in {
    val V = Representations.permutationRepresentation(S_3)
    V.irrepMultiplicities should equal(Seq(1, 0, 1))

    (S_3.reducedCharacters zip V.irrepMultiplicities).collect({ case (c: S_3.RationalCharacter, n) => V.basisForIsotypicComponent(c.character).size should equal(n * c.degree) })
  } 
  "the tensor cube of the permutation representation of S_3" should "decompose correctly" in {
    val V = Representations.tensorPower(Representations.permutationRepresentation(3), 7)
    (S_3.reducedCharacters zip V.irrepMultiplicities).collect({ case (c: S_3.RationalCharacter, n) => V.basisForIsotypicComponent(c.character).size should equal(n * c.degree) })
  }

}
