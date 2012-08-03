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
trait RootOfMinimalPolynomial[I, D] extends AlgebraicNumber[I, D] {
  def coefficients: Seq[I]
  def goodEnoughApproximation: D
}
trait PolynomialAlgebraicNumber[I, D] extends AlgebraicNumber[I, D] {
  def root: RootOfMinimalPolynomial[I, D]
  def coefficients: Seq[I]
}

object AlgebraicNumberField {
  def apply[I: EuclideanDomain, D: OrderedField](generator: RootOfMinimalPolynomial[I, D]): OrderedField[PolynomialAlgebraicNumber[I, D]] = ???
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