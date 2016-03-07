package net.tqft.toolkit.algebra

trait PartialOrdering[A] {
  def partialCompare(x: A, y: A): Option[Int]
}

trait TotalOrdering[A] extends Ordering[A] { this: PartialOrdering[A] =>
  override def partialCompare(x: A, y: A): Option[Int] = Some(compare(x, y))
}

trait PartiallyOrderedEuclideanRing[A] extends EuclideanRing[A] with PartialOrdering[A] {
  
}

trait OrderedEuclideanRing[A] extends PartiallyOrderedEuclideanRing[A] with TotalOrdering[A] {
  def signum(x: A): Int = compare(x, zero)
  def abs(x: A): A = {
    signum(x) match {
      case s if s >= 0 => x
      case s if s < 0 => negate(x)
    }
  }
  def minimum(x: A*): A = x.reduce(min)
  def maximum(x: A*): A = x.reduce(max)
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

