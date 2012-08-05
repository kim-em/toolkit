package net.tqft.toolkit.algebra

// this is just scratch work for now!

trait FusionRing[A] extends Rig[Seq[A]] {
  def basis: Seq[Seq[A]]
}

trait FusionRingWithDimensions[A] extends FusionRing[A] {
  def field: AlgebraicNumberField[Int, Double]
  def dimension(x: Seq[A]): Polynomial[Int]

  def candidateBrauerPicardGroupoids: Seq[Groupoid] = {
    def objectsSmallEnoughToBeAlgebras: Iterator[Seq[A]] = ???

    val admissibleModules: Seq[FusionModule] = ???
    val bimodules: Seq[(FusionModule, Seq[FusionBimodule])] = ???
    ???
  }

  trait FusionModule
  trait FusionBimodule

}

object AlgebraicNumberField {
  def apply[I: EuclideanDomain, D: ApproximateReals](minimalPolynomial: Polynomial[I], approximation: D): AlgebraicNumberField[I, D] = {
    new AlgebraicNumberField[I, D] {
      override val generator = minimalPolynomial.coefficientsAsFractions
      override val goodEnoughApproximation = approximation

      override var bestApproximation = approximation
      override var errorBound = implicitly[Field[D]].one

      override val integers = implicitly[EuclideanDomain[I]]
      override val approximateReals = implicitly[ApproximateReals[D]]
      override val coefficientField = Fields.fieldOfFractions(integers)
    }
  }
}

trait AlgebraicNumberField[I, D] extends NumberField[Fraction[I]] with OrderedField[Polynomial[Fraction[I]]] {
  val goodEnoughApproximation: D
  val integers: EuclideanDomain[I]
  val approximateReals: ApproximateReals[D]

  protected var bestApproximation: D
  protected var errorBound: D
  private def errorBoundOnLargestPower: D = ???

  def approximateWithin(epsilon: D)(p: Polynomial[Fraction[I]]): D = {
    ???
  }

  override def compare(x: Polynomial[Fraction[I]], y: Polynomial[Fraction[I]]) = {
    if (x == y) {
      0
    } else {
      var epsilon = approximateReals.fromDouble(0.0001)
      def gap = approximateReals.subtract(approximateWithin(epsilon)(x), approximateWithin(epsilon)(y))
      while (approximateReals.compare(approximateReals.abs(gap), approximateReals.multiplyByInt(epsilon, 4)) < 0) {
        epsilon = approximateReals.quotientByInt(epsilon, 10)
      }
      approximateReals.compare(gap, approximateReals.zero).ensuring(_ != 0)
    }
  }
}

trait Groupoid

object FusionRing {
  def fromStructureCoefficients(rank: Int, m: (Int, Int) => Seq[Int]): FusionRing[Int] = new StructureCoefficientFusionRing(rank, m)

  private class FreeRigModule[A: Rig](rank: Int) extends CommutativeMonoid[Seq[A]] {
    val zero = Seq.fill(rank)(implicitly[Rig[A]].zero)
    def add(x: Seq[A], y: Seq[A]) = x.zip(y).map(p => implicitly[Rig[A]].add(p._1, p._2))
  }

  import Implicits.Integers
  private class StructureCoefficientFusionRing(rank: Int, multiplicity: (Int, Int) => Seq[Int]) extends FreeRigModule[Int](rank) with FusionRing[Int] {
    def basis = for (i <- 0 to rank) yield for (j <- 0 to rank) yield if (i == j) 1 else 0

    def fromInt(x: Int) = x +: Seq.fill(rank - 1)(0)
    val one = fromInt(1)
    def multiply(x: Seq[Int], y: Seq[Int]) = {
      for ((xi, i) <- x.zipWithIndex; (yj, j) <- y.zipWithIndex) yield ???
    }
  }
}

trait IndeterminateFusionRing extends FusionRing[Option[Int]] {

}

object Goals {
  val AH: FusionRingWithDimensions[Int] = ???
  AH.candidateBrauerPicardGroupoids
}