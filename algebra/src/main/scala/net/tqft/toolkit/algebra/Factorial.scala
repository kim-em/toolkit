package net.tqft.toolkit.algebra

trait Factorial[I] {
  def factorial(i: I): I
}

trait FactorialAlgorithm {
  implicit class operation[I: IntegerModel](i: I) {
    def ! = implementation(i)
  }
  implicit class provideFactorial[I](integers: IntegerModel[I]) extends Factorial[I] {
    override def factorial(i: I) = implementation(i)(integers)
  }
  def implementation[I: IntegerModel](i: I): I
}

object SchoolFactorial extends FactorialAlgorithm {
  override def implementation[I: IntegerModel](i: I) = {
    def integers = implicitly[IntegerModel[I]]
    integers.product((2 to Integers.from(i)(integers)).map(integers.fromInt))
  }
}