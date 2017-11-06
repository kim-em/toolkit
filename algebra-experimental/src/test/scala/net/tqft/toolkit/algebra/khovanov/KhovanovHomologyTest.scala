package net.tqft.toolkit.algebra.khovanov

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class KhovanovHomologyTest extends FlatSpec with Matchers {
    "Reidemeister 1" should "have a chain equivalence" in {
      val r1 = Twist(1, 2, 3)
      KhovanovHomology.applyToMorphism(Cobordism(r1))
    }
}
