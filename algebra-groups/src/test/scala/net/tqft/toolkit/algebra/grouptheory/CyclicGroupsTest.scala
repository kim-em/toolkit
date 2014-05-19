package net.tqft.toolkit.algebra.grouptheory

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class CyclicGroupsTest extends FlatSpec with Matchers {
  "C_2" should "compute tensor product multiplicities without choking" in {
    FiniteGroups.cyclicGroup(2).tensorProductMultiplicities
  }
  "C_7" should "compute tensor product multiplicities without choking" in {
    FiniteGroups.cyclicGroup(7).tensorProductMultiplicities
  }
}
 

