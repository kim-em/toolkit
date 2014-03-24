//package net.tqft.toolkit.algebra
//
////import net.tqft.toolkit.algebra.ApproximateReals
//
//object Conversions {
//   val integersAsRationals = { x: Int => Fraction.whole(x) }
//  val bigIntegersAsBigRationals = { x: BigInt => Fraction.whole(x) }
//
//  val integersAsBigInts = { x: Int => BigInt(x) }
//
//  val rationalsAsBigRationals = { f: Fraction[Int] => Fraction.alreadyReduced(BigInt(f.numerator), BigInt(f.denominator)) }
//
//  val rationalsAsDoubles = { f: Fraction[Int] => f.numerator.toDouble / f.denominator.toDouble }
//  
//  val bigRationalsAsDoubles = { f: Fraction[BigInt] => f.numerator.toDouble / f.denominator.toDouble }
//
//  val integersAsBigRationals = integersAsRationals.andThen(rationalsAsBigRationals)
//  val integersAsDoubles = integersAsRationals.andThen(rationalsAsDoubles)
//  
////  def doublesAsBigDecimals(precision: Int = 128) = {
////    implicit def bigDecimals = ApproximateReals.BigDecimals(precision)
////    new HomomorphismImpl[Field, Double, BigDecimal] {
////      override def apply(f: Double) = BigDecimal(f, new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN))
////    }
////  }
//}
//
