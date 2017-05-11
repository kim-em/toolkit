package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.polynomials.{ RationalExpression => MultivariableRationalFunction }
import net.tqft.toolkit.algebra.matrices.Matrices

abstract class PlanarGraphReductionSpiderOverField[R: Field] extends PlanarGraphReductionSpider[R] { spider =>
  override def ring = implicitly[Field[R]]
}

trait FunctionSpider[A, F] extends PlanarGraphReductionSpider[F] {
  implicit def coefficientRing: Ring[A]
}

trait MultivariableRationalFunctionSpider[A] extends PlanarGraphReductionSpiderOverField[MultivariableRationalFunction[A, String]] with FunctionSpider[A, MultivariableRationalFunction[A, String]]  {
  override implicit def coefficientRing: OrderedField[A]
  override final lazy val ring = implicitly[Field[MultivariableRationalFunction[A, String]]]
}

trait BigIntMultivariableRationalFunctionSpider extends MultivariableRationalFunctionSpider[Fraction[BigInt]] {
  override final lazy val coefficientRing = implicitly[OrderedField[Fraction[BigInt]]]
}


