package net.tqft.toolkit.algebra

trait WithInverses[A] {
  def inverse(x: A): A
}

trait Group[A] extends Monoid[A] with WithInverses[A] {
  override def power(x: A, k: Int): A = {
    if (k < 0) {
      super.power(inverse(x), -k)
    } else {
      super.power(x, k)
    }
  }
}
