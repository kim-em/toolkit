package net.tqft.toolkit.algebra.combinatorics

import org.scalatest._
import scala.math._

class SumOfSquaresDecompositionsTest extends FlatSpec with Matchers {

  "SumOfSquaresDecompositions" should "find all ways to write a positive integer as a sum of squares" in {
    SumOfSquaresDecompositions(5).size should equal(2)
  }

}