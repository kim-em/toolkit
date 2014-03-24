package net.tqft.toolkit.algebra

import scala.language.implicitConversions

sealed trait Fraction[@specialized(Int, Long) A] extends Serializable {
  def denominator: A
  def numerator: A

  override def toString = numerator.toString + " / " + denominator.toString
  override def equals(other: Any) = {
    other match {
      case other: Fraction[_] => numerator == other.numerator && denominator == other.denominator
      case _ => false
    }
  }
  override def hashCode = (numerator, denominator).hashCode
}

object Fraction {

  implicit def whole[@specialized(Int, Long) A: EuclideanRing](x: A): Fraction[A] = FractionWhole(x)
  
  def alreadyReduced[A](numerator: A, denominator: A): Fraction[A] = FractionRatio(numerator, denominator)
  
  def apply[@specialized(Int, Long) A: EuclideanRing](numerator: A, denominator: A): Fraction[A] = {
    val _numerator = numerator
    val _denominator = denominator
    val ring = implicitly[EuclideanRing[A]]
    val gcd = ring.gcd(_numerator, _denominator)
    ring.quotient(_denominator, gcd) match {
      case q if q == ring.one => FractionWhole(ring.quotient(_numerator, gcd))
      case _ => FractionRatio(ring.quotient(_numerator, gcd), ring.quotient(_denominator, gcd))
    }
  }

  private case class FractionWhole[A: Ring](numerator: A) extends Fraction[A] {
    override def toString = numerator.toString
    override def denominator = implicitly[Ring[A]].one
  }
  private case class FractionRatio[A](numerator: A, denominator: A) extends Fraction[A]

//  implicit def constant[A: EuclideanRing](x: A): RationalFunction[A] = Fraction.whole(Polynomial.constant(Fraction.whole(x)))

//  implicit def toMathematicaExpression[A <% net.tqft.toolkit.mathematica.MathematicaExpression](f: Fraction[A]) = new net.tqft.toolkit.mathematica.ShortMathematicaExpression {
//    def toMathematicaInputString = "(" + f.numerator.toMathematicaInputString + ")/(" + f.denominator.toMathematicaInputString + ")"
//  }
}
