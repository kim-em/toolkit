package net.tqft.toolkit.algebra.spiders.examples

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import net.tqft.toolkit.algebra.spiders._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra._


@RunWith(classOf[JUnitRunner])
class TrivalentGraphsTest extends FlatSpec with Matchers with IsomorphismMatchers {

	
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