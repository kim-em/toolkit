package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.Implicits.Integers;
import net.tqft.toolkit.algebra.Implicits.Rationals;
import net.tqft.toolkit.algebra.Implicits.integersAsRationals;

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatricesTest extends FlatSpec with ShouldMatchers {
  import Implicits.Integers
  import Implicits.Rationals
import Implicits.integersAsRationals

  "Matrix operations" should "be correct" in {

    val m: Matrix[Fraction[Int]] = Matrix(3, List(List(1, 2, 3), List(4, 5, 6), List(5, 7, 9)))

    m.rank() should equal(2)
  }

  "echelonForm" should "work" in {
    val m: Matrix[Fraction[Int]] = Matrix(3, List(List(20, 12,1), List(12, 60,1)))
    val e: Matrix[Fraction[Int]] = Matrix(3, List(List(1,0,Fraction(1,22)), List(0,1,Fraction(1,132))))

    m.reducedRowEchelonForm should equal(e)
  }

  "preimageOf" should "work" in {
    val m: Matrix[Fraction[Int]] = Matrix(2, List(List(20, 12), List(12, 60)))

    m.rank() should equal(2)
    m.preimageOf(List(9, 12)) should equal(Some(List(Fraction(3, 8), Fraction(1, 8))))
  }
}
