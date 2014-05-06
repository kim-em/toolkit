package net.tqft.toolkit.algebra.apfloat

import scala.language.implicitConversions

import net.tqft.toolkit.algebra._
import org.apfloat.Apint
import org.apfloat.ApintMath

object Gadgets {
  implicit object Apints extends IntegerModel[Apint] with ArbitraryPrecisionIntegerModel[Apint] {
    override val zero = new Apint(0)
    override val one = new Apint(1)
    override def add(x: Apint, y: Apint) = x.add(y)
    override def multiply(x: Apint, y: Apint) = x.multiply(y)
    override def gcd(x: Apint, y: Apint) = {
      val builtInGCD = ApintMath.gcd(x, y)
      if (y.signum * builtInGCD.signum == 1) {
        builtInGCD
      } else {
        negate(builtInGCD)
      }
    }
    override def lcm(x: Apint, y: Apint) = ApintMath.lcm(x, y)
    override def fromInt(x: Int) = new Apint(x)
    override def negate(x: Apint) = x.negate()
    override def fromBigInt(x: BigInt) = new Apint(x.underlying)
    override def toBigInt(x: Apint) = x.toBigInteger()
    override def compare(x: Apint, y: Apint) = x.compareTo(y)
    override def quotientRemainder(x: Apint, y: Apint) = (x.divide(y), x.mod(y))
    override def quotient(x: Apint, y: Apint) = x.divide(y)
    override def remainder(x: Apint, y: Apint) = x.mod(y)
    override def abs(x: Apint) = ApintMath.abs(x)
    override def signum(x: Apint) = x.signum()

    override def toString = "Apints"
  }

  implicit def lift(i: Int): Apint = new Apint(i)
  implicit def liftFraction(f: Fraction[Int]): Fraction[Apint] = Fraction(new Apint(f.numerator), new Apint(f.denominator))
  implicit def liftToFraction(i: Int): Fraction[Apint] = Fraction.whole(new Apint(i))
}