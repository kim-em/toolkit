package net.tqft.toolkit.algebra.fusion3

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

@RunWith(classOf[JUnitRunner])
class EnumerationTest extends FlatSpec with Matchers {

  def results(e: Enumeration) = {
    Await.result(e.root.interruptibleDescendants({ _ => })._1, Duration.Inf)._2.map(_.canonicalize).groupBy(_.toString.split(" ").init.mkString(" ")).values.toSeq.map(_.head).distinct
  }

//  "Enumeration" should "find all the rank 5 self dual fusion rings with global dimension at most 40" in {
//    val r = results(Enumeration(5, 0, 40.0, false, None, None,None))
//    r.size should equal(31)
//  }
//  "Enumeration" should "find all the rank 5 fusion rings with 3 self dual objects and with global dimension at most 40" in {
//    val r = results(Enumeration(3, 1, 40.0, false, None, None,None))
//    r.size should equal(18)
//  }
//  "Enumeration" should "find all the rank 5 fusion rings with just 1 self dual object and with global dimension at most 40" in {
//    val r = results(Enumeration(1, 2, 40.0, false, None, None,None))
//    r.size should equal(2)
//  }
//  "Enumeration" should "find all the rank 3 self dual unitary MTCs with global dimension at most 4.1" in {
//    val r = results(Enumeration(3, 0, 4.1, umtc = true, None, None,None))
//    for(ring <- r) {
//      println(ring)
//    }
//    r.size should equal(1)
//  }
//  "Enumeration" should "find all the rank 3 non-self dual unitary MTCs with global dimension at most 3.1" in {
//    val r = results(Enumeration(1, 1, 3.1, umtc = true, None, None,None))
//    for(ring <- r) {
//      println(ring)
//    }
//    r.size should equal(1)
//  }
  "Enumeration" should "find all the rank 5 self dual unitary MTCs with global dimension at most 40" in {
    val r = results(Enumeration(5, 0, 40.0, umtc = true, None, None,None))
    println(r.size)
    r.size >= 3 should equal(true)
    r.size <= 31 should equal(true)
  }
}