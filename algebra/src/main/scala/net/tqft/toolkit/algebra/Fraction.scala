package net.tqft.toolkit.algebra

import scala.language.implicitConversions

sealed trait Fraction[@specialized(Int, Long) A] extends Serializable {
  def denominator: A
  def numerator: A

  override def toString = s"Fraction($numerator, $denominator)"
  override def equals(other: Any) = {
    other match {
      case other: Fraction[_] => numerator == other.numerator && denominator == other.denominator
      case _ => false
    }
  }
  override def hashCode = (numerator, denominator).hashCode
}

object Fraction {

  implicit def whole[@specialized(Int, Long) A: Rig](x: A): Fraction[A] = FractionWhole(x)

  def alreadyReduced[A](numerator: A, denominator: A): Fraction[A] = FractionRatio(numerator, denominator)

  def apply[@specialized(Int, Long) A: GCDRing](numerator: A, denominator: A): Fraction[A] = {
    def ring = implicitly[GCDRing[A]]
    if (denominator == ring.one) {
      FractionWhole(numerator)
    } else {
      def _numerator = numerator
      def _denominator = denominator
      val gcd = ring.gcd(_numerator, _denominator)
      if (gcd == ring.one) {
        FractionRatio(numerator, denominator)
      } else {
        ring.exactQuotient(_denominator, gcd) match {
          case q if q == ring.one => FractionWhole(ring.exactQuotient(_numerator, gcd))
          case q => FractionRatio(ring.exactQuotient(_numerator, gcd), q)
        }
      }
    }
  }

  private case class FractionWhole[A: Rig](numerator: A) extends Fraction[A] {
    override def denominator = implicitly[Rig[A]].one
  }
  private case class FractionRatio[A](numerator: A, denominator: A) extends Fraction[A] {
	  denominator match {
	    case i: Int => require(i > 0)
	    case _ =>
	  }
  }

  //  implicit def constant[A: EuclideanRing](x: A): RationalFunction[A] = Fraction.whole(Polynomial.constant(Fraction.whole(x)))

  //  implicit def toMathematicaExpression[A <% net.tqft.toolkit.mathematica.MathematicaExpression](f: Fraction[A]) = new net.tqft.toolkit.mathematica.ShortMathematicaExpression {
  //    def toMathematicaInputString = "(" + f.numerator.toMathematicaInputString + ")/(" + f.denominator.toMathematicaInputString + ")"
  //  }
  
  implicit class IntegerFraction[I:IntegerModel](f: Fraction[I]) {
    import IntegerModel._
    def toDouble = f.numerator.toDouble / f.denominator.toDouble
  }
}
