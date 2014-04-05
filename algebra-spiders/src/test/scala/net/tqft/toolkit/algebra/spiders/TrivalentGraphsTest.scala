package net.tqft.toolkit.algebra.spiders

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class TrivalentGraphsTest extends FlatSpec with Matchers with IsomorphismMatchers {
	"byNumberOfVertices" should "find 1 diagrams with 0 boundary points and 0 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(0,0,0).size should equal(1)
	}  
	"byNumberOfVertices" should "find 1 diagrams with 2 boundary points and 0 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(2,2,0).size should equal(1)
	}  
	"byNumberOfVertices" should "find 1 diagrams with 3 boundary points and 1 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(3,3,1).size should equal(1)
	}  
	"byNumberOfVertices" should "find 2 diagrams with 4 boundary points and no vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(4,4,0).size should equal(2)
	}
	"byNumberOfVertices" should "find 4 diagrams with 4 boundary points and 2 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(4,4,2).size should equal(2)
	}
	"byNumberOfVertices" should "find 0 diagrams with 4 boundary points and 4 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(4,4,4).size should equal(0)
	}
	"byNumberOfVertices" should "find 1 diagrams with 4 boundary points and 4 vertices" in {
	  TrivalentGraphs.withoutTinyFaces.byNumberOfVertices(4,4,4).size should equal(1)
	}
	"byNumberOfVertices" should "find 5 diagrams with 5 boundary points and 1 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(5,5,1).size should equal(5)
	}  
	"byNumberOfVertices" should "find 5 diagrams with 5 boundary points and 3 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(5,5,3).size should equal(5)
	}  
	"byNumberOfVertices" should "find 1 diagrams with 5 boundary points and 5 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(5,5,5).size should equal(1)
	}  
	"withoutTinyFaces.byNumberOfVertices" should "find 1 diagrams with 5 boundary points and 5 vertices" in {
	  TrivalentGraphs.withoutTinyFaces.byNumberOfVertices(5,5,5).size should equal(6)
	}  
	"byNumberOfVertices" should "find 5 diagrams with 6 boundary points and 0 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(6,6,0).size should equal(5)
	}  
	"byNumberOfVertices" should "find 15 diagrams with 6 boundary points and 2 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(6,6,2).size should equal(15)
	}  
	"byNumberOfVertices" should "find 14 diagrams with 6 boundary points and 4 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(6,6,4).size should equal(14)
	}  
	"byNumberOfVertices" should "find 7 diagrams with 6 boundary points and 6 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(6,6,6).size should equal(7)
	}  
	"byNumberOfVertices" should "find 3 diagrams with 6 boundary points and 8 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(6,6,8).size should equal(3)
	}  
	"byNumberOfVertices" should "find 2 diagrams with 6 boundary points and 10 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(6,6,10).size should equal(2)
	}  
	"byNumberOfVertices" should "find 3 diagrams with 6 boundary points and 12 vertices" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfVertices(6,6,12).size should equal(3)
	}  
	
	"byNumberOfFaces" should "find 4 diagrams in D[4,0]" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(4,0).size should equal(4)
	}
	"byNumberOfFaces" should "find 10 diagrams in D[5,0]" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(5,0).size should equal(10)
	}
	"byNumberOfFaces" should "find 34 diagrams in D[6,0]" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(6,0).size should equal(34)
	}
	"byNumberOfFaces" should "find 41 diagrams in D[6,1]" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(6,1).size should equal(41)
	}
	"byNumberOfFaces" should "find 44 diagrams in D[6,2]" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(6,2).size should equal(44)
	}
	"byNumberOfFaces" should "find 46 diagrams in D[6,3]" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(6,3).size should equal(46)
	}
	"byNumberOfFaces" should "find 112 diagrams in D[7,0]" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(7,0).size should equal(112)
	}
	"byNumberOfFaces" should "find 155 diagrams in D[7,1]" in {
	  TrivalentGraphs.withoutSmallFaces.byNumberOfFaces(7,1).size should equal(155)
	}
}