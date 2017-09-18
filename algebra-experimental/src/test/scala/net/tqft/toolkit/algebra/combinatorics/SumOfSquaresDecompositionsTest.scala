package net.tqft.toolkit.algebra.combinatorics

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.math._

@RunWith(classOf[JUnitRunner])
class SumOfSquaresDecompositionsTest extends FlatSpec with Matchers {

  "SumOfSquaresDecompositions" should "find all ways to write a positive integer as a sum of squares" in {
    SumOfSquaresDecompositions(5).size should equal(2)
  }

}