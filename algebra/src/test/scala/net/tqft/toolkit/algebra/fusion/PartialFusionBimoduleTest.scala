package net.tqft.toolkit.algebra.fusion

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

import net.tqft.toolkit.algebra.matrices._

@RunWith(classOf[JUnitRunner])
class PartialFusionBimoduleTest extends FlatSpec with ShouldMatchers {
  
  "foo" should "bar" in {
    val b = FusionBimodule[Int](Seq(List(List(1,0,0),List(0,1,0),List(0,0,1)), List(List(0,1,0),List(0,0,1),List(1,0,0)), List(List(0,0,1),List(1,0,0),List(0,1,0))),Seq(List(List(1),List(1),List(1))), Seq(List(List(1))), Seq(List(List(1))))
    val m = PartialFusionBimodule.addMysteryObjects(b, 2)
    println(m.associativityConstraints.collect({case (Left(a),Left(b)) => (a,b)}).toList)
    println(m.identityConstraints.collect({case (Left(a),Left(b)) => (a,b)}).toList)
    println(m.dualityConstraints(IndexedSeq(0,2,1,3), IndexedSeq(0,1)).collect({case (Left(a),Left(b)) => (a,b)}).toList)
    println(m.admissibilityConstraints.collect({case (Left(a),Left(b)) => (a,b)}).toList)
  }
   
  def globalDimensionAssumingNoIntermediates(b: FusionBimodule[Int]) = {
	  def ofRing(m: FusionRing[Int]#FusionModule): Double = {
	    def v(k: Int) = Seq.fill(m.rank)(0).updated(k, 1)
	    (for(i <- 0 to m.depth by 2; j <- m.objectsAtDepth(i)) yield {
	      i match {
	        case 0 => 1d
	        case 2 => {
	          List(m.dimensionLowerBounds(v(j)), scala.math.sqrt(2)).max
	        }
	        case _ => {
	          m.dimensionLowerBounds(v(j))
	        }
	      }
	    }).map(d => d * d).sum
	  }
    List(ofRing(b.leftModule), ofRing(b.rightModule), b.leftModule.globalDimensionLowerBound, b.rightModule.globalDimensionLowerBound).max
  }
  
  "addLeftObject" should "correctly all ways to add a single object to A1, up to a global dimension limit" in {
	 for(x <- EvenPartialFusionBimodule(2, Seq(List(List(1))), Seq(List(List(1))), Seq(List(List(1))), Seq(List(List(1)))).addObjects(b => globalDimensionAssumingNoIntermediates(b) < 6)) {
	   println(x)
	   println(globalDimensionAssumingNoIntermediates(x.fusionBimodule))
	   println(x.fusionBimodule.globalDimensionLowerBound)
	 }
  }
} 
