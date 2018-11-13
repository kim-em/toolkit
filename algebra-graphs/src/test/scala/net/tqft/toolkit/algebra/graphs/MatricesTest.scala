package net.tqft.toolkit.algebra.graphs

import org.scalatest._

class MatricesTest extends FlatSpec with Matchers {

  "" should "" in {
    val m1 = List(List(0, 1), List(1, 2))
    val m2 = List(List(2, 1), List(1, 0))
    MatricesUpToSimultaneousRowAndColumnPermutations.isomorphism(m1, m2) should equal(Some(List(1, 0)))
  }
}
