package net.tqft.toolkit.algebra

import net.tqft.toolkit.mathematica.MathematicaExpression
import java.math.BigInteger
import java.util.Comparator

object Gadgets {
  class NumericRing[T](numeric: Numeric[T]) extends Ring[T] with Comparator[T] {
    override def multiply(x: T, y: T) = numeric.times(x, y)
    override def add(x: T, y: T) = numeric.plus(x, y)
    override def negate(x: T) = numeric.negate(x)

    override val one = numeric.one
    override val zero = numeric.zero    

    override def compare(x: T, y: T) = numeric.compare(x, y)    
  }
  
  class IntegralEuclideanDomain[T](numeric: Integral[T]) extends NumericRing(numeric) with OrderedEuclideanDomain[T] {    
    override def quotientRemainder(x: T, y: T) = (numeric.quot(x, y), numeric.rem(x, y))    
  }
  
  class FractionalField[T](numeric: Fractional[T]) extends NumericRing(numeric) with OrderedField[T] {
    override def inverse(x: T) = numeric.div(one, x)
  }
  
  val Integers: OrderedEuclideanDomain[Int] = new IntegralEuclideanDomain(scala.math.Numeric.IntIsIntegral)
  val BigIntegers: OrderedEuclideanDomain[BigInt] = new IntegralEuclideanDomain(scala.math.Numeric.BigIntIsIntegral)
  val Doubles: OrderedField[Double] = new FractionalField(scala.math.Numeric.DoubleIsFractional)
  

  
  def BigDecimals(precision: Int = 128): ApproximateField[BigDecimal] = BigDecimals(new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN))
  def BigDecimals(mc: java.math.MathContext): ApproximateField[BigDecimal] = new FractionalField(scala.math.Numeric.BigDecimalIsFractional) with ApproximateField[BigDecimal] {
    override val one = BigDecimal(1, mc)
    override val zero = BigDecimal(0, mc)
    override def abs(x: BigDecimal) = x.abs
    override val epsilon = BigDecimal(0.1, mc).pow(mc.getPrecision - 1)
  }

  import java.math.{ BigDecimal => jBigDecimal } 
  def JavaBigDecimals(mc: java.math.MathContext): ApproximateField[jBigDecimal] = new ApproximateField[jBigDecimal] {
    override val one = new jBigDecimal(1, mc)
    override val zero = new jBigDecimal(0, mc)
    override def abs(x: jBigDecimal) = x.abs(mc)
    override val epsilon = new jBigDecimal(0.1, mc).pow(mc.getPrecision / 2)
    override def compare(x: jBigDecimal, y: jBigDecimal) = x.compareTo(y)
    override def inverse(x: jBigDecimal) = x.pow(-1, mc)
    override def negate(x: jBigDecimal) = x.negate(mc)
    override def add(x: jBigDecimal, y: jBigDecimal) = x.add(y, mc)
    override def multiply(x: jBigDecimal, y: jBigDecimal) = x.multiply(y, mc)
  }
  
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
  val rationalsAsBigRationals = new Homomorphism[Field, Fraction[Int], Fraction[BigInt]] {
    val source = Rationals
    val target = BigRationals
    def apply(f: Fraction[Int]) = Fraction(BigInt(f.numerator), BigInt(f.denominator))(BigIntegers)
  }
  
  val bigRationalsAsDoubles = new Homomorphism[Field, Fraction[BigInt], Double] {
    def source = BigRationals
    def target = Doubles
    def apply(f: Fraction[BigInt]) = f.numerator.toDouble / f.denominator.toDouble
  }
  
  def doublesAsBigDecimals(precision: Int = 128) = new Homomorphism[Field, Double, BigDecimal] {
    def source = Doubles
    def target = BigDecimals(precision)
    def apply(f: Double) = BigDecimal(f, new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN))
  }
  
  val BigIntegerPolynomials = Polynomials.over(BigIntegers)
  val BigRationalPolynomials = Polynomials.over(BigRationals)
}
