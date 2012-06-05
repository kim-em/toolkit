package net.tqft.toolkit.permutations

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class PermutationTest extends FlatSpec with ShouldMatchers {

  "of" should "0 should have size 1" in {
    Permutations.of(0) should have size (1)
  }
  "of" should "1 should have size 1" in {
    Permutations.of(1) should have size (1)
  }

  "inverse" should "give the inverse permutation" in {
    import Permutations._
    for (p <- randomPermutationsOf(7).take(5)) {
      p permute inverse(p) should equal(identity(7))
    }
  }

}

