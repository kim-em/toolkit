package net.tqft.toolkit.algebra

sealed trait Fraction[A] extends Serializable {
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
  def apply[A](_numerator: A, _denominator: A)(implicit ring: EuclideanDomain[A]): Fraction[A] = {
    val gcd = ring.gcd(_numerator, _denominator)
    ring.quotient(_denominator, gcd) match {
      case q if q == ring.one =>
        new Fraction[A] {
          val numerator = ring.quotient(_numerator, gcd)
          val denominator = ring.one
          override def toString = numerator.toString
        }
      case _ =>
        new Fraction[A] {
          val numerator = ring.quotient(_numerator, gcd)
          val denominator = ring.quotient(_denominator, gcd)

          if (denominator.isInstanceOf[BigInt]) {
            if (denominator.asInstanceOf[BigInt].compare(BigInt(0)) < 0) require(false)
          }
        }
    }
  }

  def alreadyReduced[A](_numerator: A, _denominator: A): Fraction[A] = new Fraction[A] { val numerator = _numerator; val denominator = _denominator }

  implicit def toMathematicaExpression[A <% net.tqft.toolkit.mathematica.MathematicaExpression](f: Fraction[A]) = new net.tqft.toolkit.mathematica.ShortMathematicaExpression {
    def toMathematicaInputString = "(" + f.numerator.toMathematicaInputString + ")/(" + f.denominator.toMathematicaInputString + ")"
  }
}
