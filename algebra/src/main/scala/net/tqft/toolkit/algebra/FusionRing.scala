package net.tqft.toolkit.algebra

// this is just scratch work for now!

// Usually A = Int, for a concrete fusion ring. We allow other possibilities so we can write fusion solvers, etc.
trait FusionRing[A] extends Rig[Seq[A]] {
  def coefficients: Rig[A]

  val rank: Int

  // TODO a lot of this should be implemented somewhere higher up.
  def basis = for (i <- 0 until rank) yield for (j <- 0 until rank) yield if (i == j) coefficients.one else coefficients.zero
  def regularRepresentation(x: Seq[A]) = new Matrix(rank, for (b <- basis) yield multiply(x, b))

  override def fromInt(x: Int) = coefficients.fromInt(x) +: Seq.fill(rank - 1)(coefficients.zero)
  override val one = fromInt(1)
  override val zero = Seq.fill(rank)(coefficients.zero)
  override def add(x: Seq[A], y: Seq[A]) = x.zip(y).map(p => coefficients.add(p._1, p._2))

  def verifyAssociativity = (for(x <- basis; y <- basis; z <- basis) yield multiply(x, multiply(y, z)) == multiply(multiply(x,y), z)).reduce(_ && _)
  def verifyIdentity = (for(x <- basis) yield x == multiply(one, x) && x == multiply(x, one)).reduce(_ && _)
}

trait FusionRingWithDimensions extends FusionRing[Int] {
  // TODO
  //  def field: RealNumberField[Int, Double] = ???
  //  def dimension(x: Seq[A]): Polynomial[Int] = ???

  def dimensionBounds(x: Seq[Int]): Double

  def objectsSmallEnoughToBeAlgebras: Iterator[Seq[Int]] = {
    val start = Iterator(Seq(1))
    basis.tail.foldLeft(start)({
      (i: Iterator[Seq[Int]], b: Seq[Int]) =>
        i.flatMap({
          a: Seq[Int] => for (m <- 0 to dimensionBounds(b).floor.intValue) yield a :+ m
        })
    })
  }

  def candidateAlgebras: Iterator[Seq[Int]] = {
    for (
      o <- objectsSmallEnoughToBeAlgebras;
      a = regularRepresentation(o);
      if Matrices.positiveSymmetricDecompositions(a).nonEmpty
    ) yield o
  }

  def candidateBrauerPicardGroupoids: Seq[Groupoid] = {
    val admissibleModules: Seq[FusionModule] = ???
    val bimodules: Seq[(FusionModule, Seq[FusionBimodule])] = ???
    ???
  }

  trait FusionModule
  trait FusionBimodule

}

trait Groupoid

object FusionRing {
  def apply[A: Rig](multiplicities: Seq[Seq[Seq[A]]]): FusionRing[A] = new StructureCoefficientFusionRing(multiplicities)
  def apply(multiplicities: Seq[Seq[Seq[Int]]], dimensionBoundsForSimples: Seq[Double]): FusionRingWithDimensions = new StructureCoefficientFusionRingWithDimensions(multiplicities, dimensionBoundsForSimples)

  private class StructureCoefficientFusionRing[A: Rig](multiplicities: Seq[Seq[Seq[A]]]) extends FusionRing[A] {
    override lazy val coefficients = implicitly[Rig[A]]
    override lazy val rank = multiplicities.size

    override def multiply(x: Seq[A], y: Seq[A]) = {
      val zero = coefficients.zero
      val terms = for ((xi, i) <- x.zipWithIndex; if(xi != zero); (yj, j) <- y.zipWithIndex; if(yj != zero)) yield for (k <- 0 until rank) yield coefficients.multiply(xi, yj, multiplicities(i)(j)(k)) 
      val result = add(terms)
      result
    }
  }

  private class StructureCoefficientFusionRingWithDimensions(multiplicities: Seq[Seq[Seq[Int]]], dimensionBoundsForSimples: Seq[Double]) extends StructureCoefficientFusionRing[Int](multiplicities)(Gadgets.Integers) with FusionRingWithDimensions {
    override def dimensionBounds(x: Seq[Int]) = (for ((c, d) <- x.zip(dimensionBoundsForSimples)) yield (c * d)).sum
  }
}

object Goals extends App {
  val haagerupFusionRing: FusionRingWithDimensions = {
    val haagerupMultiplicities: Seq[Seq[Seq[Int]]] =
      List(
        List(List(1, 0, 0, 0), List(0, 1, 0, 0), List(0, 0, 1, 0), List(0, 0, 0, 1)),
        List(List(0, 1, 0, 0), List(1, 2, 2, 1), List(0, 2, 1, 1), List(0, 1, 1, 1)),
        List(List(0, 0, 1, 0), List(0, 2, 1, 1), List(1, 1, 1, 1), List(0, 1, 1, 0)),
        List(List(0, 0, 0, 1), List(0, 1, 1, 1), List(0, 1, 1, 0), List(1, 1, 0, 0)))
    val haagerupDimensionBounds: Seq[Double] = Seq(1.0, 4.31, 3.31, 2.31)
    FusionRing(haagerupMultiplicities, haagerupDimensionBounds).ensuring(_.verifyAssociativity).ensuring(_.verifyIdentity)
  }
  println(haagerupFusionRing.candidateAlgebras.toList)
  haagerupFusionRing.candidateBrauerPicardGroupoids
}