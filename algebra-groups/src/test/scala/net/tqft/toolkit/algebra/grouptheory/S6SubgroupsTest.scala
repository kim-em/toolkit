package net.tqft.toolkit.algebra.grouptheory

import net.tqft.toolkit.algebra.Fraction.whole
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.Fraction
import org.scalatest._

class S6SubgroupsTest extends FlatSpec with Matchers {

  "characters" should "give the right character table for (S_3 x S_3)_b \\subset S_6" in {
    val G = FiniteGroups.symmetricGroup(6).subgroupGeneratedBy(Seq(IndexedSeq(1, 2, 0, 4, 5, 3), IndexedSeq(3, 5, 4, 0, 2, 1), IndexedSeq(3, 5, 4, 1, 0, 2), IndexedSeq(3, 4, 5, 0, 1, 2)))
    println(G.characters)
    for(c1 <- G.reducedCharacters) { for(c2 <- G.reducedCharacters) print(G.characterPairing(c1, c2)); println }
    require(G.verifyOrthogonalityOfCharacters)
  }
  
 
}
