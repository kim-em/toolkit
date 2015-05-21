package net.tqft.toolkit.algebra.grouptheory

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.Logging
import org.junit.Assert.assertEquals
import org.junit.Test

class RepresentationTest {

  val S_3 = FiniteGroups.symmetricGroup(3)

  @Test
  def testIrrepMultiplicities = {
    val V = Representations.permutationRepresentation[Fraction[BigInt]](S_3)
    assertEquals(Seq(1, 0, 1), V.irrepMultiplicities)
  }

  @Test
  def testTensorPowersDecompose = {
    for (k <- 1 to 7) {
      Logging.info("Looking at the " + k + "-th tensor power.")
      val V = Representations.tensorPower(Representations.permutationRepresentation[Fraction[BigInt]](3), k)
      (S_3.reducedCharacters zip V.irrepMultiplicities).collect({ case (c: S_3.RationalCharacter, n) => assertEquals(n * c.degree, V.basisForIsotypicComponent(c.character).size) })
    }
  }

}


