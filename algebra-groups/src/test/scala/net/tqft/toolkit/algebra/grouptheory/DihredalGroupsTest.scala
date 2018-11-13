package net.tqft.toolkit.algebra.grouptheory

import org.scalatest._

class DihedralGroupsTest extends FlatSpec with Matchers {
  "D_28" should "compute tensor product multiplicities without choking" in {
    FiniteGroups.dihedralGroup(28).tensorProductMultiplicities
  }
}
 

