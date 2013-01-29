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
    Factor(-1) should equal(List(-1))
    Factor(60) should equal(List(2, 2, 3, 5))
    Factor(17) should equal(List(17))
  }
  
   
//  "factor" should "factor everything" in {
//    var lastPrintStatement = System.nanoTime
//    var lasti = 0
//    val p = 617
//    for (i <- 1 until Integer.MAX_VALUE by p) {
//      val diff = System.nanoTime - lastPrintStatement
//      if (diff > 1000000000) {
//        println(i + " " + diff * p / (i - lasti))
//        lasti = i
//        lastPrintStatement = System.nanoTime
//        println(Factor(i))
//      }
//      Factor(i).ensuring({ j => i == j.fold(1)(_ * _) })
//    }
//  }

  "factor" should "factor big stuff too" in {
    Factor(BigInt("517455095467917407322131"))
    Factor(BigInt("1864798491686174386003108681281345504959"))
  }
  
}
   