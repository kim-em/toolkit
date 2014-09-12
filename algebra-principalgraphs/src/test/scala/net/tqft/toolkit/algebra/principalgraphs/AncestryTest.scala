package net.tqft.toolkit.algebra.principalgraphs

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.math._

@RunWith(classOf[JUnitRunner])
class AncestryTest extends FlatSpec with Matchers {

  "" should "" in {
    val w = SubfactorWeed(5.25, PairOfBigraphsWithDuals(BigraphWithDuals("bwd1v1v1v1p1p1v1x0x0p1x0x0p0x1x0v0x0x1duals1v1v1x2x3v1"), BigraphWithDuals("bwd1v1v1v1p1p1v1x0x0p0x1x0p0x0x1v0x0x1p0x1x0p1x0x0duals1v1v1x2x3v1x3x2")))
    
    for(p <- w.ancestry) {
      println(p)
      println(p.verifyParent)
      if(!p.verifyParent) {
        println("parent's children: ")
        for(c <- p.parent.get.children) {
          println(s"   $c")
        }
      }
    }
    
    w.verifyAncestry should equal(true)
  }

}