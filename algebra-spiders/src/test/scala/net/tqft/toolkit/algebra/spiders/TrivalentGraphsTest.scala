package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class TrivalentGraphsTest extends FlatSpec with Matchers with IsomorphismMatchers {
	"withoutSmallFaces" should "find 1 diagrams with 3 boundary points and 1 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(3,3,1).size should equal(1)
	}  
	"withoutSmallFaces" should "find 2 diagrams with 4 boundary points and no vertices" in {
	  TrivalentGraphs.withoutSmallFaces(4,4,0).size should equal(2)
	}
	"withoutSmallFaces" should "find 4 diagrams with 4 boundary points and 2 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(4,4,2).size should equal(2)
	}
	"withoutSmallFaces" should "find 0 diagrams with 4 boundary points and 4 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(4,4,4).size should equal(0)
	}
	"withoutTinyFaces" should "find 1 diagrams with 4 boundary points and 4 vertices" in {
	  TrivalentGraphs.withoutTinyFaces(4,4,4).size should equal(1)
	}
	"withoutSmallFaces" should "find 5 diagrams with 5 boundary points and 1 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(5,5,1).size should equal(5)
	}  
	"withoutSmallFaces" should "find 5 diagrams with 5 boundary points and 3 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(5,5,3).size should equal(5)
	}  
	"withoutSmallFaces" should "find 1 diagrams with 5 boundary points and 5 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(5,5,5).size should equal(1)
	}  
	"withoutTinyFaces" should "find 1 diagrams with 5 boundary points and 5 vertices" in {
	  TrivalentGraphs.withoutTinyFaces(5,5,5).size should equal(6)
	}  
	"withoutSmallFaces" should "find 5 diagrams with 6 boundary points and 0 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(6,6,0).size should equal(5)
	}  
	"withoutSmallFaces" should "find 15 diagrams with 6 boundary points and 2 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(6,6,2).size should equal(15)
	}  
	"withoutSmallFaces" should "find 14 diagrams with 6 boundary points and 4 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(6,6,4).size should equal(14)
	}  
	"withoutSmallFaces" should "find 7 diagrams with 6 boundary points and 6 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(6,6,6).size should equal(7)
	}  
	"withoutSmallFaces" should "find 3 diagrams with 6 boundary points and 8 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(6,6,8).size should equal(3)
	}  
	"withoutSmallFaces" should "find 2 diagrams with 6 boundary points and 10 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(6,6,10).size should equal(2)
	}  
	"withoutSmallFaces" should "find 3 diagrams with 6 boundary points and 12 vertices" in {
	  TrivalentGraphs.withoutSmallFaces(6,6,12).size should equal(3)
	}  
}