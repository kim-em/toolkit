package net.tqft.toolkit.collections

import org.scalatest._

import scala.math._

class BreadthFirstTest extends FlatSpec with Matchers {

  "BreadthFirst" should "work correctly" in {
    import BreadthFirst._
    val N = NonStrictNaturalNumbers
    val i1 = N map { 3 * _ + 1 }
    val i2 = N map { 3 * _ + 2 }
    val i3 = N map { 3 * _ + 3 }

    List(i1, i2, i3).breadthFirstSearch(_ % 2 == 0).take(11).toList should equal(List(Some(2), Some(4), None, Some(10), Some(8), Some(16), Some(6), Some(22), Some(14), Some(28), None))
  }

}

