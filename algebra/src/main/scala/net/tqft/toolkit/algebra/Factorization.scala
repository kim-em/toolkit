package net.tqft.toolkit.algebra

import net.tqft.toolkit.arithmetic.Factor

trait Factorization[A] {
  def factor(a: A): Map[A, Int]
  def divisors(a: A)(implicit ring: Ring[A]): Iterator[A] = {
    factor(a).foldLeft(Iterator(ring.one))({ case (divs: Iterator[A], (b, k: Int)) => divs.flatMap(div => (0 to k).map(x => ring.multiply(div, ring.power(b, x)))) })
  }
}

object Factorization {
  implicit def provideECMFactorizationByDefault[I:IntegerModel]: Factorization[I] = new ECMFactorization.provideFactorization[I](implicitly[IntegerModel[I]])
}

trait FactorizationAlgorithm {
  implicit class operation[I: IntegerModel](i: I) {
    def factor = implementation(i)
  }
  implicit class provideFactorization[I](integers: IntegerModel[I]) extends Factorization[I] {
    override def factor(i: I) = implementation(i)(integers)
  }
  def implementation[I: IntegerModel](i: I): Map[I, Int]
}

object ECMFactorization extends FactorizationAlgorithm {
  override def implementation[I: IntegerModel](x: I) = {
    def integers = implicitly[IntegerModel[I]]
    Factor(integers.toBigInt(x)).groupBy(x => x).map(p => integers.fromBigInt(p._1) -> p._2.size)
  }
}
