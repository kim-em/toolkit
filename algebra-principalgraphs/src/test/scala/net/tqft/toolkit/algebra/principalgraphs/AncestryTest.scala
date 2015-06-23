package net.tqft.toolkit.algebra.principalgraphs

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.math._
import net.tqft.toolkit.algebra.graphs.Dreadnaut

@RunWith(classOf[JUnitRunner])
class AncestryTest extends FlatSpec with Matchers {

  "" should "" in {
    val w = SubfactorWeed(5.25, PairOfBigraphsWithDuals(BigraphWithDuals("bwd1v1v1v1p1v1x0p1x0v0x1p1x0v0x1p1x0p0x1p1x0v1x0x0x0p0x0x0x1duals1v1v1x2v1x2v2x1"), BigraphWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1p1x0p0x1v0x0x0x1p0x1x0x0p1x0x0x0p0x0x1x0v1x0x0x0p0x0x0x1duals1v1v1x2v1x2x4x3v1x2")))
    
    println(w.pair.nautyGraph)
    println(Dreadnaut.automorphismGroup(w.pair.nautyGraph).generators)
    println(w.automorphisms.generators)
    
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