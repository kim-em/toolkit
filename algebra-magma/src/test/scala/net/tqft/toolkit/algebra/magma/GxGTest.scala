package net.tqft.toolkit.algebra.magma

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.language.implicitConversions

@RunWith(classOf[JUnitRunner])
class GxGTest extends FlatSpec with Matchers {

  import Magma._
  val N = 16
  val G = MagmaGroup("SmallGroup(16,2)")
  val GxG = G.Square
  println(GxG.numberOfSubgroups(N))
//  for (x <- G.asScalaGroup.generators) {
//    println(GxG.leftInclusion(x))
//  }
//  for (x <- G.asScalaGroup.generators) {
//    println(GxG.rightInclusion(x))
//  }
  for (i <- 1 to GxG.numberOfSubgroups(N)) {
    val A= GxG.Action(i)
    println(s"i = $i: #L\\G/R = ${A.doubleCosets.size}")
//    println("H = " + GxG.MagmaSubgroup(i).asScalaGroup.elements)
//    println(A.leftStabilizer.elements)
//    println(A.rightStabilizer.elements)
//    
//    println(A.leftActionOnRightCosets.orbits(A.rightAction.orbits(A.asScalaAction.elements.toSet)))
//    println(A.rightActionOnLeftCosets.orbits(A.leftAction.orbits(A.asScalaAction.elements.toSet)))
//    
//    println(A.L.elements)
//    println(A.R.elements)
//    for (c <- A.doubleCosets) {
//      println(c)
//    }
  }

}
 

