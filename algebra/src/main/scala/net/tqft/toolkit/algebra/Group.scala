package net.tqft.toolkit.algebra

trait WithInverses[A] {
  def inverseOption(x: A): Option[A]
  def inverse(x: A): A
}

trait Group[A] extends Monoid[A] with WithInverses[A] {
  override def inverseOption(x: A): Option[A] = Some(inverse(x))
  override def power(x: A, k: Int): A = {
    if (k < 0) {
      super.power(inverse(x), -k)
    } else {
      super.power(x, k)
    }
  }
}

object Group {
  implicit def forget[A: AdditiveGroup]: Group[A] = {
    val additiveGroup = implicitly[AdditiveGroup[A]]
    new Group[A] {
      def one = additiveGroup.zero
      def multiply(a1: A, a2: A) = additiveGroup.add(a1, a2)
      def inverse(a: A) = additiveGroup.negate(a)
    }
  }
}