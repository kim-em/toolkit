package net.tqft.toolkit.algebra.enumeration

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.math._

@RunWith(classOf[JUnitRunner])
class OdometerTest extends FlatSpec with ShouldMatchers  {
		
  "Odometer" should "emulate a real odometer, as in a car!" in {
	  implicit val carLimit = { l: List[Int] => !(l.exists(_ > 9)) }
	  import ListOdometer._
	  Odometer(List(0,0,0)).toList.size should equal (1000)
  }
 
  "Odometer" should "find small vectors in the positive cone of a lattice" in {
	  implicit val normList = { l: List[Int] => ((l map { pow(_,2) }) reduceLeft { _ + _ }) <= 5 }
	  import ListOdometer._
	  Odometer(List(0,0,0)).toList.size should equal (17)
  }
  
  "Odometer" should "find matrices with small row sums" in {
	  val sumSmall = { l: List[Int] => (l reduceLeft { _ + _ }) <= 2 }
	  val allSumsSmall = { m: List[List[Int]] => (m map sumSmall).foldLeft(true)(_ && _) }
	  val matrixOdometer = { matrix: List[List[Int]] => ListOdometer.odometerA(matrix)(ListOdometer.odometerInt _) }
	  
	  Odometer(List(List(0,0), List(0,0)))(matrixOdometer, allSumsSmall).toList.size should equal (36)
  }
  
  // FIXME return this to the fusionatlas package
//  "Odometer" should "find some graphs extending Haagerup" in {
//	implicit val limit: Bigraph => Boolean = { _.isFPEigenvaluePossiblyBelow(sqrt(5)) }
//		
//	val Haagerup = Bigraph("gbg1v1v1v1p1v1x0p0x1v1x0p0x1")
//	Odometer(Bigraph(Haagerup, new RectangularMatrix(List(List(0,0))))).size should equal (4)
//  }

}