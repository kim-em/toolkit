package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RepresentationTest extends FlatSpec with ShouldMatchers {

  "the permutation representation of S_3" should "include one copy of the trivial representation" in {
    implicit val rationals = Gadgets.Rationals
    import Implicits.integersAsRationals
    Representations.permutationRepresentation(3).basisForIsotypicComponent(Seq(1, 1, 1)) should have size (1)
  }
  "the tensor square of the permutation representation of S_3" should "include two copies of the trivial representation" in {
    implicit val rationals = Gadgets.Rationals
    import Implicits.integersAsRationals
    Representations.tensorPower(Representations.permutationRepresentation(3), 2).basisForIsotypicComponent(Seq(1, 1, 1)) should have size (2)
  }

}
