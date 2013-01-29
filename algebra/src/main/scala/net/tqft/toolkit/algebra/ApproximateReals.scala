package net.tqft.toolkit.algebra

import org.apfloat.Apfloat
import org.apfloat.ApfloatMath
import org.apfloat.FixedPrecisionApfloatHelper

trait ApproximateReals[A] extends ApproximateField[A] {
  def fromInteger[I: IntegerModel](i: I): A = {
    i match {
      case i: Int => fromInt(i)
      case i => fromBigDecimal(BigDecimal(implicitly[IntegerModel[I]].toBigInt(i)))
    }
  }
  def fromDouble(x: Double): A
  def fromBigDecimal(x: BigDecimal): A
  def setPrecision(x: A): A
  def bigDecimalValue(x: A): BigDecimal
}

object ApproximateReals {
  implicit object Doubles extends NumericTypes.FractionalField(scala.math.Numeric.DoubleIsFractional) with ApproximateReals[Double] {
    override def bigDecimalValue(x: Double) = BigDecimal(x)
    override def setPrecision(x: Double) = x
    override def fromBigDecimal(x: BigDecimal) = x.doubleValue
    override def fromDouble(x: Double) = x
    override def epsilon = 1.0E-8
    override def sqrt(x: Double) = scala.math.sqrt(x)
  }

  def BigDecimals(precision: Int = 128): ApproximateReals[BigDecimal] = BigDecimals(new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN))
  def BigDecimals(mc: java.math.MathContext): ApproximateReals[BigDecimal] = new NumericTypes.FractionalField(scala.math.Numeric.BigDecimalIsFractional) with ApproximateReals[BigDecimal] {
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

}
