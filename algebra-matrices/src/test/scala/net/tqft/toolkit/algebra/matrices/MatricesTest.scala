package net.tqft.toolkit.algebra.matrices

import org.scalatest._
import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials.Polynomial

class MatricesTest extends FlatSpec with Matchers {

  "Matrix operations" should "be correct" in {
    val m: Matrix[Fraction[Int]] = Matrix(3, List(List(1, 2, 3), List(4, 5, 6), List(5, 7, 9)))

    m.rank() should equal(2)

    m.nullSpace.size should equal(1)

  }

  "echelonForm" should "work" in {
    val m: Matrix[Fraction[Int]] = Matrix(3, List(List(20, 12, 1), List(12, 60, 1)))
    //    val r: Matrix[Fraction[Int]] = Matrix(3, List(List(1,Fraction(3,5),Fraction(1,20)), List(0,1,Fraction(1,132))))
    val r: Matrix[Fraction[Int]] = Matrix(3, List(List(20, 12, 1), List(0, Fraction(264, 5), Fraction(2, 5))))
    val e: Matrix[Fraction[Int]] = Matrix(3, List(List(1, 0, Fraction(1, 22)), List(0, 1, Fraction(1, 132))))

    m.rowEchelonForm should equal(r)
    m.reducedRowEchelonForm should equal(e)

    val md: Matrix[Double] = Matrix(3, List(List(20, 12, 1), List(12, 60, 1)))
    md.rowEchelonForm should equal(Matrix(3, List(List(20, 12, 1), List(0, 52.8, 0.4))))
  }

  "preimageOf" should "work" in {
    val m: Matrix[Fraction[Int]] = Matrix(2, List(List(20, 12), List(12, 60)))

    m.rank() should equal(2)
    m.preimageOf(List(9, 12)) should equal(Some(List(Fraction(3, 8), Fraction(1, 8))))

    implicit val integersAsBigRationals = Conversions.integersAsBigInts andThen Conversions.bigIntegersAsBigRationals
    implicit val rationalsAsBigRationals = Conversions.rationalsAsBigRationals

    val m2: Matrix[Fraction[BigInt]] = Matrix.from(List(List(13, 9, 14, 7, 4), List(0, 12, 12, 19, 15), List(3, 10, 0, 1, 15), List(17, 0, 11, 14, 19), List(8, 4, 6, 11, 13)))
    val a2: List[Fraction[BigInt]] = List(72, 65, 74, 18, 87)
    val b2: List[Fraction[BigInt]] = List(Fraction(152938, 10207), Fraction(684843, 40828), Fraction(-1020225, 40828), Fraction(685601, 40828), Fraction(-423201, 40828))
    m2.preimageOf(a2) should equal(Some(b2))
  }

  "characteristicPolynomial" should "work" in {
    val m: Matrix[Fraction[Int]] = Matrix(2, List(List(20, 12), List(12, 60)))
    m.characteristicPolynomial should equal(Polynomial((0, Fraction(1056, 1)), (1, Fraction(-80, 1)), (2, Fraction(1, 1))))
  }
}
