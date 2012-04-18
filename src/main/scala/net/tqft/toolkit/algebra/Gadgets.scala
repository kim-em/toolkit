package net.tqft.toolkit.algebra

import net.tqft.toolkit.mathematica.MathematicaExpression
import java.math.BigInteger

object Gadgets {
  class NumericRing[T](numeric: Numeric[T]) extends Ring[T] {
    def multiply(x: T, y: T) = numeric.times(x, y)
    def add(x: T, y: T) = numeric.plus(x, y)
    def negate(x: T) = numeric.negate(x)

    val one = numeric.one
    val zero = numeric.zero    

    def compare(x: T, y: T) = numeric.compare(x, y)    
  }
  
  class IntegralEuclideanDomain[T](numeric: Integral[T]) extends NumericRing(numeric) with OrderedEuclideanDomain[T] {    
    def quotientRemainder(x: T, y: T) = (numeric.quot(x, y), numeric.rem(x, y))    
  }
  
  class FractionalField[T](numeric: Fractional[T]) extends NumericRing(numeric) with OrderedField[T] {
    def inverse(x: T) = numeric.div(one, x)
  }
  
  val Integers: OrderedEuclideanDomain[Int] = new IntegralEuclideanDomain(scala.math.Numeric.IntIsIntegral)
  val BigIntegers: OrderedEuclideanDomain[BigInt] = new IntegralEuclideanDomain(scala.math.Numeric.BigIntIsIntegral)
  val Doubles: OrderedField[Double] = new FractionalField(scala.math.Numeric.DoubleIsFractional)
  val BigDecimals: OrderedField[BigDecimal] = new FractionalField(scala.math.Numeric.BigDecimalIsFractional)
  
//  val Integers = new OrderedEuclideanDomain[Int] {
//    def multiply(x: Int, y: Int) = x * y
//    def add(x: Int, y: Int) = x + y
//    def negate(x: Int) = -x
//    val one = 1
//    val zero = 0
//
//    def quotientRemainder(x: Int, y: Int) = (x / y, x % y)
//    
//    def compare(x: Int, y: Int) = x.compareTo(y)
//  }
//
//  val BigIntegers = new OrderedEuclideanDomain[BigInt] {
//    def multiply(x: BigInt, y: BigInt) = x * y
//    def add(x: BigInt, y: BigInt) = x + y
//    def negate(x: BigInt) = -x
//    val one = BigInt(1)
//    val zero = BigInt(0)
//
//    def quotientRemainder(x: BigInt, y: BigInt) = (x / y, x % y)
//    override def gcd(x: BigInt, y: BigInt) = x.gcd(y) * y.signum
//
//    def compare(x: BigInt, y: BigInt) = x.compare(y)
//  }
  
  
  val Rationals = Fields.fieldOfFractions(Integers)
  val BigRationals = Fields.fieldOfFractions(BigIntegers)
  
  val integersAsRationals = Fields.embeddingInFieldOfFractions(Integers)
  val bigIntegersAsBigRationals = Fields.embeddingInFieldOfFractions(BigIntegers)
  
  val IntegerPolynomials = Polynomials.over(Integers)
  val RationalPolynomials = Polynomials.over(Rationals)

  val integersAsBigInts = new Homomorphism[EuclideanDomain, Int, BigInt] {
    val source = Integers
    val target = BigIntegers
    def apply(k: Int) = BigInt(k)
  }
  
  val bigRationalsAsDoubles = new Homomorphism[Field, Fraction[BigInt], Double] {
    def source = BigRationals
    def target = Doubles
    def apply(f: Fraction[BigInt]) = f.numerator.toDouble / f.denominator.toDouble
  }
  
  val BigIntegerPolynomials = Polynomials.over(BigIntegers)
  val BigRationalPolynomials = Polynomials.over(BigRationals)
}
