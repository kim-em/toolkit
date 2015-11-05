package net.tqft.toolkit.algebra.magma

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.language.implicitConversions

@RunWith(classOf[JUnitRunner])
class MagmaFiniteGroupTest extends FlatSpec with Matchers {

  "smallGroup" should "return a finitely generated finite group" in {
    Magma.smallGroup(16, 14).size should equal(16)
    val (group, actions) = Magma.smallGroupWithActions(16, 14)
    actions(12).elements
  }
  
}
 

