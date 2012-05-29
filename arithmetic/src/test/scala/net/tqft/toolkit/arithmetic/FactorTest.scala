package net.tqft.toolkit.arithmetic

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FactorTest extends FlatSpec with ShouldMatchers {

  "factor" should "factor 60" in {
    Factor(0) should equal(List(0))
    Factor(1) should equal(List())
    Factor(60) should equal(List(2, 2, 3, 5))
  }
  "ecm.factor" should "factor 60" in {
    ecm.Factor(0) should equal(List(0))
    ecm.Factor(1) should equal(List())
    ecm.Factor(-1) should equal(List(-1))
    ecm.Factor(60) should equal(List(2, 2, 3, 5))
    ecm.Factor(17) should equal(List(17))
  }
  
  "speed test!" should "" in {
    var lastPrintStatement = System.nanoTime
    var lasti = 0
    var sieveTotal = 0L
    var ecmTotal = 0L
    for (i <- 1 until Integer.MAX_VALUE by 15121) {
      val diff = System.nanoTime - lastPrintStatement
      if (diff > 1000000000) {
        println(i + " " + diff / (i - lasti) + " " + (sieveTotal - ecmTotal))
        lasti = i
        lastPrintStatement = System.nanoTime
      }
      def timing[A](f: => A): Long = {
        val t0 = System.nanoTime
        f
        System.nanoTime - t0
      }
      sieveTotal = sieveTotal + timing(Factor(i))
      ecmTotal = ecmTotal + timing(ecm.Factor(i))
    }
  }
  
  "factor" should "factor everything" in {
    var lastPrintStatement = System.nanoTime
    var lasti = 0
    for (i <- 0 until Integer.MAX_VALUE) {
      val diff = System.nanoTime - lastPrintStatement
      if (diff > 1000000000) {
        println(i + " " + diff / (i - lasti))
        lasti = i
        lastPrintStatement = System.nanoTime
      }
      Factor(i)
    }
  }

  "ecm.factor" should "factor everything" in {
    var lastPrintStatement = System.nanoTime
    var lasti = 0
    for (i <- 0 until Integer.MAX_VALUE) {
      val diff = System.nanoTime - lastPrintStatement
      if (diff > 1000000000) {
        println(i + " " + diff / (i - lasti))
        lasti = i
        lastPrintStatement = System.nanoTime
      }
      ecm.Factor(i)
    }
  }

}
   