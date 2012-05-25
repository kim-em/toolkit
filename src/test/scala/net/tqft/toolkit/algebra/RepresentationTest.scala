package net.tqft.toolkit.algebra

import org.junit.Test
import org.junit.Assert._
import net.tqft.toolkit.Logging

class RepresentationTest {

  val S_3 = FiniteGroups.symmetricGroup(3)
  implicit val rationals = Gadgets.Rationals
  import Implicits.integersAsRationals;

  @Test
  def testIrrepMultiplicities = {
    val V = Representations.permutationRepresentation(S_3)
    assertEquals(Seq(1, 0, 1), V.irrepMultiplicities)
  }

  @Test
  def testTensorPowersDecompose = {
    for (k <- 1 to 7) {
      Logging.info("Looking at the " + k + "-th tensor power.")
      val V = Representations.tensorPower(Representations.permutationRepresentation(3), k)
      (S_3.reducedCharacters zip V.irrepMultiplicities).collect({ case (c: S_3.RationalCharacter, n) => assertEquals(n * c.degree, V.basisForIsotypicComponent(c.character).size) })
    }
  }

}


