package net.tqft.toolkit.algebra.magma

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class LoadSmallGroupsTest extends FlatSpec with Matchers {

  "LoadSmallGroups" should "return all the small groups, from a local database" in {
    val smallGroups = LoadSmallGroups()
    smallGroups(16).size should equal (14)
    smallGroups(1)(0)._2.size should equal(1)
  }
  
}
  

