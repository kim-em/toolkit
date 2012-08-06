package net.tqft.toolkit.algebra

import net.tqft.toolkit.mathematica.MathematicaExpression
import java.math.BigInteger
import java.util.Comparator
import org.apfloat.Apfloat
import org.apfloat.ApfloatMath
import org.apfloat.FixedPrecisionApfloatHelper

object Gadgets {
  class NumericRing[T](numeric: Numeric[T]) extends Ring[T] with Comparator[T] {
    override def multiply(x: T, y: T) = numeric.times(x, y)
    override def add(x: T, y: T) = numeric.plus(x, y)
    override def negate(x: T) = numeric.negate(x)

    override def fromInt(x: Int) = numeric.fromInt(x)
    override val one = numeric.one
    override val zero = numeric.zero

    override def compare(x: T, y: T) = numeric.compare(x, y)
  }

  class IntegralEuclideanDomain[T](numeric: Integral[T]) extends NumericRing(numeric) with OrderedEuclideanDomain[T] {
    override def quotientRemainder(x: T, y: T) = (numeric.quot(x, y), numeric.rem(x, y))
    override def quotient(x: T, y: T) = numeric.quot(x, y)
    override def remainder(x: T, y: T) = numeric.rem(x, y)
  }

  class FractionalField[T](numeric: Fractional[T]) extends NumericRing(numeric) with OrderedField[T] {
    override def inverse(x: T) = numeric.div(one, x)
    override def quotientRemainder(x: T, y: T) = (numeric.div(x, y), zero)
    override def quotient(x: T, y: T) = numeric.div(x, y)
    override def remainder(x: T, y: T) = zero
  }

  object Integers extends IntegralEuclideanDomain(scala.math.Numeric.IntIsIntegral) {
    /**
     * returns an iterator of {(a_i, b_i)}_i, with \sum b_i a_i^2 = n
     */
    def sumOfSquaresDecomposition(n: Int): Iterator[Seq[(Int, Int)]] = {
      def sqrt(k: Int) = {
        if (k <= 0) {
          0
        } else {
          val closest = scala.math.sqrt(k).round.intValue
          if(closest * closest > k) {
            closest - 1
          } else {
            closest
          }
        }
      }

      def extend(limit: Int, remainder: Int, partial: Seq[(Int, Int)]): Iterator[Seq[(Int, Int)]] = {
        limit match {
          case 0 => {
            remainder match {
              case 0 => Iterator(partial)
              case _ => Iterator.empty
            }
          }
          case 1 => Iterator((1, remainder) +: partial)
          case limit => {
            for (
              b <- (0 to (remainder / (limit * limit))).iterator;
              next = (limit, b) +: partial;
              nextRemainder = remainder - b * limit * limit;
              nextLimit = scala.math.min(limit - 1, sqrt(nextRemainder));
              result <- extend(nextLimit, nextRemainder, next)
            ) yield result
          }
        }
      }

      extend(sqrt(n), n, Nil)
    }
  }
  val Longs: OrderedEuclideanDomain[Long] = new IntegralEuclideanDomain(scala.math.Numeric.LongIsIntegral)
  val BigIntegers: OrderedEuclideanDomain[BigInt] = new IntegralEuclideanDomain(scala.math.Numeric.BigIntIsIntegral)
  val Doubles: OrderedField[Double] = new FractionalField(scala.math.Numeric.DoubleIsFractional)

  def BigDecimals(precision: Int = 128): ApproximateReals[BigDecimal] = BigDecimals(new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN))
  def BigDecimals(mc: java.math.MathContext): ApproximateReals[BigDecimal] = new FractionalField(scala.math.Numeric.BigDecimalIsFractional) with ApproximateReals[BigDecimal] {
    override def fromInt(x: Int) = BigDecimal(x, mc)
    override def fromDouble(x: Double) = BigDecimal(x, mc)
    override def fromBigDecimal(x: BigDecimal) = x(mc)
    override def setPrecision(x: BigDecimal) = x(mc)
    override def bigDecimalValue(x: BigDecimal) = x
    override val one = BigDecimal(1, mc)
    override val zero = BigDecimal(0, mc)
    override def abs(x: BigDecimal) = x.abs
    override val epsilon = BigDecimal(0.1, mc).pow(mc.getPrecision - 1)
    override def inverse(x: BigDecimal) = x.pow(-1)
    override def power(x: BigDecimal, k: Int) = x.pow(k)
  }

  // mysteriously broken at present?
  import java.math.{ BigDecimal => jBigDecimal }
  def JavaBigDecimals(precision: Int): ApproximateReals[jBigDecimal] = JavaBigDecimals(new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN))
  def JavaBigDecimals(mc: java.math.MathContext): ApproximateReals[jBigDecimal] = new ApproximateReals[jBigDecimal] {
    override def fromInt(x: Int) = new jBigDecimal(x, mc)
    override def fromDouble(x: Double) = new jBigDecimal(x, mc)
    override def fromBigDecimal(x: BigDecimal) = x(mc).underlying
    override def setPrecision(x: jBigDecimal) = x.round(mc)
    override def bigDecimalValue(x: jBigDecimal) = BigDecimal(x, mc)
    override val one = new jBigDecimal(1, mc)
    override val zero = new jBigDecimal(0, mc)
    override def abs(x: jBigDecimal) = x.abs(mc)
    override val epsilon = new jBigDecimal(0.1, mc).pow(mc.getPrecision - 2)
    override def compare(x: jBigDecimal, y: jBigDecimal) = x.compareTo(y)
    override def inverse(x: jBigDecimal) = x.pow(-1, mc)
    override def negate(x: jBigDecimal) = x.negate(mc)
    override def add(x: jBigDecimal, y: jBigDecimal) = x.add(y, mc)
    override def multiply(x: jBigDecimal, y: jBigDecimal) = x.multiply(y, mc)
    override def power(x: jBigDecimal, k: Int) = x.pow(k)
  }

  def Apfloats(precision: Int = 128): ApproximateReals[Apfloat] = new ApproximateReals[Apfloat] {
    val helper = new FixedPrecisionApfloatHelper(precision)

    override def fromInt(x: Int) = new Apfloat(x, precision)
    override def fromDouble(x: Double) = new Apfloat(x, precision)
    override def fromBigDecimal(x: BigDecimal) = new Apfloat(x.underlying, precision)
    override def setPrecision(x: Apfloat) = x.precision(precision)
    override def bigDecimalValue(x: Apfloat) = BigDecimal(x.toString(false), new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN))
    override val one = new Apfloat(1, precision)
    override val zero = new Apfloat(0, precision)
    override def abs(x: Apfloat) = if (compare(x, zero) < 0) helper.negate(x) else x
    override val epsilon = helper.pow(new Apfloat(0.1, precision), precision - 2)
    override def compare(x: Apfloat, y: Apfloat) = x.compareTo(y)
    override def inverse(x: Apfloat) = helper.pow(x, -1)
    override def negate(x: Apfloat) = helper.negate(x)
    override def add(x: Apfloat, y: Apfloat) = helper.add(x, y)
    override def multiply(x: Apfloat, y: Apfloat) = helper.multiply(x, y)
    override def power(x: Apfloat, k: Int) = helper.pow(x, k)
    override def sqrt(x: Apfloat) = helper.sqrt(x)
  }

  val Rationals = Fields.Rationals
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
