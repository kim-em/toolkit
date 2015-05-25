package net.tqft.toolkit.algebra.combinatorics

import scala.language.implicitConversions
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.SeqPolynomial
import net.tqft.toolkit.algebra.numberfields.NumberField
import org.apfloat.Apint

object PositiveSymmetricDecomposition2App extends App {

  import net.tqft.toolkit.algebra.apfloat.Gadgets._
  implicit def constantApintPolynomial(i: Int): Polynomial[Fraction[Apint]] = Polynomial(0 -> (i: Fraction[Apint]))

  object H {
    val annularHoms = Seq(Seq(3, 1, 0, 1), Seq(1, 4, 4, 2), Seq(0, 4, 6, 1), Seq(1, 2, 1, 8))
    val numberField = NumberField(Polynomial(0 -> -13, 2 -> 1))
    val globalDimension = Polynomial(0 -> Fraction(39, 2), 1 -> Fraction(9, 2))
    val dimensions = Seq(
      Polynomial(0 -> Fraction(13, 2), 1 -> Fraction(3, 2)),
      Polynomial(0 -> Fraction(-9, 2), 1 -> Fraction(-3, 2)),
      Polynomial(0 -> Fraction(11, 2), 1 -> Fraction(3, 2)),
      Polynomial(0 -> Fraction(9, 2), 1 -> Fraction(3, 2)))
  }

  object EH {
    val annularHoms = Seq(Seq(6, 6, 3, 5, 9, 13),
      Seq(6, 8, 3, 5, 8, 13),
      Seq(3, 3, 15, 11, 30, 48),
      Seq(5, 5, 11, 17, 29, 45),
      Seq(9, 8, 30, 29, 79, 115),
      Seq(13, 13, 48, 45, 115, 181))
    val numberField = NumberField.cyclotomic[Apint](13)
    val globalDimension = SeqPolynomial[Fraction[Apint]](170, 0, 50, 50, -125, 0, -125, -125, 0, -125, 50, 50)
    val dimensions = Seq[Polynomial[Fraction[Apint]]](
      SeqPolynomial(-2, 0, -1, -1, 3, 0, 3, 3, 0, 3, -1, -1),
      SeqPolynomial(3, 0, 1, 1, -3, 0, -3, -3, 0, -3, 1, 1),
      SeqPolynomial(11, 0, 3, 3, -9, 0, -9, -9, 0, -9, 3, 3),
      SeqPolynomial(5, 0, 2, 2, -1, 0, -1, -1, 0, -1, 2, 2),
      SeqPolynomial(6, 0, 2, 2, -6, 0, -6, -6, 0, -6, 2, 2),
      SeqPolynomial(4, 0, 1, 1, -3, 0, -3, -3, 0, -3, 1, 1))
  }

  object `4442` {
    val annularHoms = Seq(Seq(8, 2, 2, 7, 4, 6, 3, 3, 12),
      Seq(2, 8, 2, 1, 4, 3, 6, 3, 12),
      Seq(2, 2, 8, 1, 4, 3, 3, 6, 12),
      Seq(7, 1, 1, 8, 5, 7, 4, 4, 11),
      Seq(4, 4, 4, 5, 20, 12, 12, 12, 36),
      Seq(6, 3, 3, 7, 12, 26, 17, 17, 52),
      Seq(3, 6, 3, 4, 12, 17, 26, 17, 52),
      Seq(3, 3, 6, 4, 12, 17, 17, 26, 52),
      Seq(12, 12, 12, 11, 36, 52, 52, 52, 164))

    val numberField = NumberField(Polynomial(0 -> -5, 2 -> 1).mapValues(x => x: Apint))
    val globalDimension = Polynomial(0 -> Fraction(120, 1), 1 -> Fraction(48, 1)).mapValues(x => x: Fraction[Apint])
    val dimensions = Seq[Polynomial[Fraction[Apint]]](1, 1, 1, 0, 3, Polynomial(0 -> 2, 1 -> 1), Polynomial(0 -> 2, 1 -> 1), Polynomial(0 -> 2, 1 -> 1), Polynomial(0 -> 6, 1 -> 3))
  }

  for (A <- PositiveSymmetricDecomposition2.apply(H.annularHoms, H.numberField, H.globalDimension, H.dimensions)) {
    println(A)
  }
//  for (A <- PositiveSymmetricDecomposition2.apply(EH.annularHoms, EH.numberField, EH.globalDimension, EH.dimensions)) {
//    println(A)
//  }
  for (A <- PositiveSymmetricDecomposition2.apply(`4442`.annularHoms, `4442`.numberField, `4442`.globalDimension, `4442`.dimensions)) {
    println(A)
  }

}