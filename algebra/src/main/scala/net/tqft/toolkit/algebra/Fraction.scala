package net.tqft.toolkit.algebra

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
  
  implicit def whole[@specialized(Int, Long) A: EuclideanRing](x: A): Fraction[A] = alreadyReduced(x, implicitly[EuclideanRing[A]].one)
  def apply[@specialized(Int, Long) A:EuclideanRing](numerator: A, denominator: A): Fraction[A] = {
    val _numerator = numerator
    val _denominator = denominator
    val ring = implicitly[EuclideanRing[A]]
    val gcd = ring.gcd(_numerator, _denominator)
    ring.quotient(_denominator, gcd) match {
      case q if q == ring.one =>
        new Fraction[A] {
          override val numerator = ring.quotient(_numerator, gcd)
          override val denominator = ring.one
          override def toString = numerator.toString
        }
      case _ => alreadyReduced(ring.quotient(_numerator, gcd), ring.quotient(_denominator, gcd))
    }
  }

  def alreadyReduced[@specialized(Int, Long) A](_numerator: A, _denominator: A): Fraction[A] = new Fraction[A] { val numerator = _numerator; val denominator = _denominator }

  implicit def toMathematicaExpression[A <% net.tqft.toolkit.mathematica.MathematicaExpression](f: Fraction[A]) = new net.tqft.toolkit.mathematica.ShortMathematicaExpression {
    def toMathematicaInputString = "(" + f.numerator.toMathematicaInputString + ")/(" + f.denominator.toMathematicaInputString + ")"
  }
}
