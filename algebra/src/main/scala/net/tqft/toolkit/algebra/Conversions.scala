package net.tqft.toolkit.algebra

import net.tqft.toolkit.algebra.categories._

object Conversions {
  val integersAsRationals = Fields.embeddingInFieldOfFractions(Integers)
  val bigIntegersAsBigRationals = Fields.embeddingInFieldOfFractions(BigIntegers)

  val integersAsBigInts = new HomomorphismImpl[EuclideanRing, Int, BigInt] {
    override def apply(k: Int) = BigInt(k)
  }
  val rationalsAsBigRationals = new HomomorphismImpl[Field, Fraction[Int], Fraction[BigInt]] {
    override def apply(f: Fraction[Int]) = Fraction(BigInt(f.numerator), BigInt(f.denominator))(BigIntegers)
  }

  val rationalsAsDoubles = new HomomorphismImpl[Field, Fraction[Int], Double] {
    override def apply(f: Fraction[Int]) = f.numerator.toDouble / f.denominator.toDouble
  }
  
  val bigRationalsAsDoubles = new HomomorphismImpl[Field, Fraction[BigInt], Double] {
    override def apply(f: Fraction[BigInt]) = f.numerator.toDouble / f.denominator.toDouble
  }

  val integersAsBigRationals = integersAsRationals.andThen(rationalsAsBigRationals)
  val integersAsDoubles = integersAsRationals.andThen(rationalsAsDoubles)
  
  def doublesAsBigDecimals(precision: Int = 128) = {
    implicit def bigDecimals = ApproximateReals.BigDecimals(precision)
    new HomomorphismImpl[Field, Double, BigDecimal] {
      override def apply(f: Double) = BigDecimal(f, new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN))
    }
  }
}

