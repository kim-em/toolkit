package net.tqft.toolkit.algebra


object Mod {
  def apply(p: Int): Field[Int] with Finite[Int] = {
    require(p < scala.math.sqrt(Integer.MAX_VALUE))
    require(BigInt(p).isProbablePrime(20))
    new Field[Int] with Finite[Int] {
      import net.tqft.toolkit.arithmetic.Mod._

      override def elements = (0 until p).toSet
      override def inverse(x: Int) = {
        if (x == 0) throw new ArithmeticException("/ by zero")
        Integers.extendedEuclideanAlgorithm(x, p)._1
      }
      override def negate(x: Int) = (p - x) mod p
      override val zero = 0
      override val one = 1
      override def multiply(x: Int, y: Int) = (x * y) mod p
      override def add(x: Int, y: Int) = (x + y) mod p
      override def fromInt(x: Int) = x mod p
    }
  }
}
