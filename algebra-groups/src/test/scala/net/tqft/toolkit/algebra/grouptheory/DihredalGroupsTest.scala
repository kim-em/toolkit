package net.tqft.toolkit.algebra.grouptheory

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class DihedralGroupsTest extends FlatSpec with Matchers {
  "D_28" should "compute tensor product multiplicities without choking" in {
    FiniteGroups.dihedralGroup(28).tensorProductMultiplicities
  }
}
 

