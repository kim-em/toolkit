package net.tqft.toolkit.algebra.grouptheory

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

@RunWith(classOf[JUnitRunner])
class CyclicGroupsTest extends FlatSpec with ShouldMatchers {
  "C_2" should "compute tensor product multiplicities without choking" in {
    FiniteGroups.cyclicGroup(2).tensorProductMultiplicities
  }
}
 

