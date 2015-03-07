package net.tqft.toolkit.algebra.principalgraphs

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.math._

@RunWith(classOf[JUnitRunner])
class BigraphTest extends FlatSpec with Matchers {

  "Companion object" should "parse string notation" in {
    Bigraph("gbg1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p0x1p1x0")
  }

  "downUpNeighbours" should "count neighbours" in {
    val g = Bigraph("gbg1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p0x1p1x0")
    g.downUpNeighbours(7, 0).toList should equal(List(0, 2))
    g.downUpNeighbours(7, 1).size should equal(1)
    g.downUpNeighbours(7, 2).size should equal(2)
  }
  
  "nextToNeighbours" should "count neighbours" in {
    val g = Bigraph("gbg1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p0x1p1x0")
    g.nextToNeighbours(7, 0).size should equal(3)   
  }
}