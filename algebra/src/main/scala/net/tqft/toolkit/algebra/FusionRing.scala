package net.tqft.toolkit.algebra

// this is just scratch work for now!

trait FusionRing[A] extends Rig[Seq[A]] {
  def basis: Seq[Seq[A]]
}

trait FusionRingWithDimensions[A] extends FusionRing[A] {
  def fieldGenerator: RootOfMinimalPolynomial[Int, Double]
  def dimension(x: Seq[A]): AlgebraicNumber[Int, Double]

  private val field = {
    import Implicits.Integers
    import Implicits.Doubles
    AlgebraicNumberField(fieldGenerator)
  }

  def candidateBrauerPicardGroupoids: Seq[Groupoid] = {
    def objectsSmallEnoughToBeAlgebras: Iterator[Seq[A]] = ???

    val admissibleModules: Seq[FusionModule] = ???
    val bimodules: Seq[(FusionModule, Seq[FusionBimodule])] = ???
    ???
  }

  trait FusionModule
  trait FusionBimodule

}

trait AlgebraicNumber[I, D] {
  def approximateWithin(epsilon: D)(implicit integers: EuclideanDomain[I], reals: OrderedField[D]): D = ???
}
case class RootOfMinimalPolynomial[I, D](coefficients: Polynomial[I], goodEnoughApproximation: D) extends AlgebraicNumber[I, D]
case class PolynomialAlgebraicNumber[I, D](root: RootOfMinimalPolynomial[I, D], polynomial: Polynomial[Fraction[I]]) extends AlgebraicNumber[I, D]

object AlgebraicNumberField {
  def apply[I: EuclideanDomain, D: OrderedField](generator: RootOfMinimalPolynomial[I, D]): AlgebraicNumberField[I, D] = {
    val _generator = generator
    new AlgebraicNumberField[I, D] {
      override val approximateGenerator = _generator
      override val coefficientField = Fields.fieldOfFractions(implicitly[EuclideanDomain[I]])
    }
  }
}

trait AlgebraicNumberField[I, D] extends NumberField[Fraction[I]] with OrderedField[Polynomial[Fraction[I]]] {
  type P = Polynomial[Fraction[I]]

  val approximateGenerator: RootOfMinimalPolynomial[I, D]
  override val generator = approximateGenerator.coefficients.coefficientsAsFractions(???)
  override def compare(x: P, y: P) = ???
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