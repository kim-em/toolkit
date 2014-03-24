package net.tqft.toolkit.permutations

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class PermutationTest extends FlatSpec with Matchers {

  import Permutations._

  "of" should "0 should have size 1" in {
    Permutations.of(0) should have size (1)
  }
  "of" should "1 should have size 1" in {
    Permutations.of(1) should have size (1)
  }

  "inverse" should "give the inverse permutation" in {
    for (p <- randomPermutationsOf(7).take(5)) {
      p permute inverse(p) should equal(identity(7))
    }
  }

  "A Permutation" should "correctly permute a List" in {
    val p: Permutation = IndexedSeq(1, 0, 3, 4, 2)
    p.permute(List("a", "b", "c", "d", "e")) should equal(List("b", "a", "d", "e", "c"))
  }

  "A Permutation" should "correctly find its inverse" in {
    IndexedSeq(1, 0, 3, 4, 2).inverse should equal(List(1, 0, 4, 2, 3))
  }

  "Permutations" should "generate all permutations in lexicographic order" in {
    Permutations.of(3).size should equal(6)
    Permutations.of(1).toList should equal(List(List(0)))
    Permutations.of(2).toList should equal(List(List(0, 1), List(1, 0)))
    import net.tqft.toolkit.collections.LexicographicOrdering._
    Permutations.of(5).toList.sorted should equal(Permutations.of(5).toList)
  }

  "Permutations" should "find all permutations preserving a list" in {
    Permutations.preserving(List("A", "A", "B", "B", "A")).size should equal(12)
    Permutations.preserving(List(0, 1, 0, 1)) should contain(IndexedSeq(0, 1, 2, 3))
    Permutations.preserving(List("A", "A", "B", "B", "A")) should contain(IndexedSeq(0, 1, 2, 3, 4))
  }

  "Permutations" should "return non-strict iterables (1)" in {
    Permutations.of(1000).take(5).size should equal(5)
  }

  "Permutations" should "return non-strict iterables (2)" in {
    Permutations.preserving(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)).take(5).size should equal(5)
  }

  "Permutations" should "find all permutations mapping one list to another (1)" in {
    Permutations.mapping(List("A", "B", "B", "C"), List("B", "B", "C", "A")).size should equal(2)
    Permutations.mapping(List("A", "B", "B", "C"), List("B", "B", "C", "A")).toList should equal(List(List(1, 2, 3, 0), List(2, 1, 3, 0)))
    Permutations.mapping(List("A", "B", "B", "C"), List("B", "B", "C", "A")) map { p => (p permute List("A", "B", "B", "C")) == List("B", "B", "C", "A") } reduceLeft { _ && _ } should equal(true)
    Permutations.mapping(List("A", "B", "B", "C"), List("A", "B", "D", "C")).size should equal(0)
  }
  "Permutations" should "find all permutations mapping one list to another (2)" in {
    Permutations.findOneMapping(List(0, 1, 0, 1), List(0, 1, 0, 1)) should equal(Some(List(0, 1, 2, 3)))
    val p = Permutations.mapping(List(0, 1, 0, 1), List(0, 1, 0, 1))
    p.size should equal(4)
    p.should(contain(IndexedSeq(0, 1, 2, 3)))
  }
  "Permutations" should "find all permutations preserving a large list without duplicates without crashing" in {
    Permutations.preserving((1 to 12).toList).size should equal(1)
    Permutations.preserving(List(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)).size should equal(24 * 24 * 24)
  }

}

