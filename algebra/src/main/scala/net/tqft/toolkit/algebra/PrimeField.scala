package net.tqft.toolkit.algebra

object PrimeField {
  def apply[I: IntegerModel](p: I): Field[I] with Finite[I] = {
    def integers = implicitly[IntegerModel[I]]
    new Field[I] with Finite[I] {
      private def normalForm(x: I) = {
        val r = integers.remainder(x, p)
        if(integers.compare(r, integers.zero) < 0) {
          integers.add(r, p)
        } else {
          r
        }
      }
      
      override def elements = (0 until Integers.from(p)).map(integers.fromInt).toSet
      override def inverse(x: I): I = {
        if (x == integers.zero) throw new ArithmeticException("/ by zero")
        normalForm(integers.extendedEuclideanAlgorithm(x, p)._1)
      }
      override def negate(x: I) = normalForm(integers.negate(x))
      override val zero = integers.zero
      override val one = integers.one
      override def multiply(x: I, y: I) = normalForm(integers.multiply(x, y))
      override def add(x: I, y: I) = normalForm(integers.add(x, y))
      override def fromInt(x: Int) = normalForm(integers.fromInt(x))
      
      override def toString = s"PrimeField($p)"
    }
  }
}
