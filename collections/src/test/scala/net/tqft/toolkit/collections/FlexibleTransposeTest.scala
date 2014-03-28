package net.tqft.toolkit.collections

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class FlexibleTransposeTest extends FlatSpec with Matchers {

  "FlexibleTranspose" should "work correctly" in {
    import FlexibleTranspose._

    val v1 = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    val r1 = List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))
    val v2 = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9, 10, 11))
    val r2 = List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9), List(10), List(11))
    val v3 = List(List(1, 2, 3, 4), List(4, 5, 6), List(7, 8, 9, 10, 11))
    val r3 = List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9), List(4, 10), List(11))

//    v1.flexibleTranspose should equal(r1)
//    v2.flexibleTranspose should equal(r2)
//    v3.flexibleTranspose should equal(r3)
//    
    NonStrictIterable.from(v1).flexibleTranspose.map(_.toList).takeWhile(_.nonEmpty).toList.map(_.toList) should equal(r1)
    NonStrictIterable.from(v2).flexibleTranspose.map(_.toList).takeWhile(_.nonEmpty).toList should equal(r2)
    NonStrictIterable.from(v3).flexibleTranspose.map(_.toList).takeWhile(_.nonEmpty).toList.map(_.toList) should equal(r3)

    val N = NonStrictNaturalNumbers
    
    def echo(i: Int) = { println(i); i }
    val i1 = N map { n => echo(3 * n + 1) }
    val i2 = N map { n => echo(3 * n + 2) }
    val i3 = N map { n => echo(3 * n + 3) }

//    List(i1, i2, i3).flexibleTranspose.head.toList should equal(List(1, 2, 3))
//    
    val list = List(i1, i2, i3).flexibleTranspose.take(3).toList
    list(0).take(2).toList should equal(List(1,2))
    list(1).take(1).toList should equal(List(4))
    list(2).take(3).toList should equal(List(7, 5, 3))
  }

}

