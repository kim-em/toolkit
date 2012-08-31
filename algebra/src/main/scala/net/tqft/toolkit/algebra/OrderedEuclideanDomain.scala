package net.tqft.toolkit.algebra

trait OrderedEuclideanDomain[A] extends EuclideanDomain[A] with Ordering[A] {
  def signum(x: A): Int = compare(x, zero)
  def abs(x: A): A = {
    signum(x) match {
      case s if s >= 0 => x
      case s if s < 0 => negate(x)
    }
  }

  override def gcd(x: A, y: A) = {
    val gcd = super.gcd(x, y)
    multiply(gcd, multiply(fromInt(signum(gcd) * signum(y))))
  }
}

object OrderedEuclideanDomain {
  implicit def forgetOrderedField[A: OrderedField]: OrderedEuclideanDomain[A] = implicitly[OrderedField[A]]
}

