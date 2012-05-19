package net.tqft.toolkit.algebra

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CyclicGroupsTest extends FlatSpec with ShouldMatchers {
  "C_2" should "compute character table without choking" in {
    FiniteGroups.cyclicGroup(2).characterTable
  }
}
 

