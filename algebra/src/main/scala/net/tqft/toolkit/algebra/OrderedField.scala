package net.tqft.toolkit.algebra


trait OrderedField[A] extends Field[A] with Ordering[A] { self =>
  def abs(x: A): A = {
    signum(x) match {
      case s if s >= 0 => x
      case s if s < 0 => negate(x)
    }
  }
  def signum(x: A) = compare(x, zero)
  def chop(x: A, epsilon: A): A = {
    if (compare(abs(x), epsilon) < 0) zero else x
  }
}
