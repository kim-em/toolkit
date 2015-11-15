package net.tqft.toolkit.algebra

trait OrderedEuclideanRing[A] extends EuclideanRing[A] with Ordering[A] {
  def signum(x: A): Int = compare(x, zero)
  def abs(x: A): A = {
    signum(x) match {
      case s if s >= 0 => x
      case s if s < 0 => negate(x)
    }
  }
  def nonnegative_?(x: A) = compare(x, zero) >= 0
  def positive_?(x: A) = compare(x, zero) > 0

  override def gcd(x: A, y: A) = {
    val gcd = super.gcd(x, y)
    multiplyByInt(gcd, signum(gcd) * signum(y))
  }

  def log(x: A, b: Int) = {
    var k = -1;
    var p = one
    while (compare(x, p) > 0) {
      p = multiplyByInt(p, b)
      k += 1
    }
    k
  }
}

object OrderedEuclideanRing {
  implicit def forgetOrderedField[A: OrderedField]: OrderedEuclideanRing[A] = implicitly[OrderedField[A]]
  implicit def forgetIntegerModel[A: IntegerModel]: OrderedEuclideanRing[A] = implicitly[IntegerModel[A]]
}

