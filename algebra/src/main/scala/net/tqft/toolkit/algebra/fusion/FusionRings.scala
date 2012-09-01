package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.algebra.diophantine._
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebras
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial

object FusionRings {
  def withObject(m: Matrix[Int], knownRing: Option[FusionRing[Int]] = None ): Iterable[FusionRing[Int]] = {
    val rank = m.numberOfColumns
    require(m.entries.head == 0 :: 1 :: List.fill(rank - 2)(0))

    type V = (Int, Int, Int)

    implicit val polynomialAlgebra = MultivariablePolynomialAlgebras.over[Int, V]

    val identity = Matrix.identityMatrix[MultivariablePolynomial[Int, V]](rank)
    val generator = m.mapEntries(polynomialAlgebra.constant)
    val unknowns = for (i <- 2 until rank) yield {
      Matrix(rank,
        for (j <- 0 until rank) yield {
          for (k <- 0 until rank) yield polynomialAlgebra.monomial((i, j, k))
        })
    }
    val variableStructureCoefficients = identity +: generator +: unknowns
    val variableRing = FusionRing(identity +: generator +: unknowns)

    val variables = for (i <- 2 until rank; j <- 0 until rank; k <- 0 until rank) yield (i, j, k)

    val knownSolution = knownRing.map({ ring =>
      val rsc = ring.structureCoefficients
      (for (i <- 2 until rank; j <- 0 until rank; k <- 0 until rank) yield (i, j, k) -> rsc(i).entries(j)(k)).toMap
    })

    val (solutions, tooHard) = BoundedDiophantineSolver.solve(variableRing.associativityConstraints.flatten.toSeq, variables, knownSolution = knownSolution)
    for (th <- tooHard) {
      for (e <- th) println(e)
    }
    require(tooHard.isEmpty)

    for (solution <- solutions) yield {
      val structureCoefficients = variableStructureCoefficients.map(_.mapEntries({
        case p if p.totalDegree == Some(1) => solution(p.terms.head._1.keySet.iterator.next)
        case p => p.constantTerm
      }))
      FusionRing(structureCoefficients).ensuring(_.structureCoefficients(1) == m)
    }
  }

  object Examples {
    val H1: FusionRingWithDimensions = {
      val haagerup4Multiplicities: Seq[Matrix[Int]] =
        List(
          List(List(1, 0, 0, 0), List(0, 1, 0, 0), List(0, 0, 1, 0), List(0, 0, 0, 1)),
          List(List(0, 1, 0, 0), List(1, 2, 2, 1), List(0, 2, 1, 1), List(0, 1, 1, 1)),
          List(List(0, 0, 1, 0), List(0, 2, 1, 1), List(1, 1, 1, 1), List(0, 1, 1, 0)),
          List(List(0, 0, 0, 1), List(0, 1, 1, 1), List(0, 1, 1, 0), List(1, 1, 0, 0)))

      val haagerup4FieldGenerator: Polynomial[Int] = {
        Polynomial(2 -> 1, 0 -> -13)
      }
      val haagerup4FieldGeneratorApproximation: Double = 3.60555
      val haagerup4FieldGeneratorEpsilon: Double = 0.00001
      val haagerup4Dimensions: Seq[Polynomial[Fraction[Int]]] = {
        import Implicits.{ integersAsRationals }
        Seq(
          Polynomial(0 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(5, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(3, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(1, 2), 1 -> Fraction(1, 2)))
      }
      FusionRing(haagerup4Multiplicities, haagerup4FieldGenerator, haagerup4FieldGeneratorApproximation, haagerup4FieldGeneratorEpsilon, haagerup4Dimensions).ensuring(_.verifyAssociativity).ensuring(_.verifyIdentity)
    }
    val AH1: FusionRingWithDimensions = {
      val AH1Multiplicities: Seq[Matrix[Int]] =
        List(
          List(List(1, 0, 0, 0, 0, 0), List(0, 1, 0, 0, 0, 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 1, 0), List(0, 0, 0, 0, 0, 1)),
          List(List(0, 1, 0, 0, 0, 0), List(1, 1, 0, 0, 1, 0), List(0, 0, 1, 0, 0, 1), List(0, 0, 0, 0, 1, 1), List(0, 1, 0, 1, 1, 1), List(0, 0, 1, 1, 1, 2)),
          List(List(0, 0, 1, 0, 0, 0), List(0, 0, 1, 0, 0, 1), List(1, 1, 1, 0, 0, 1), List(0, 0, 0, 1, 1, 1), List(0, 0, 0, 1, 1, 2), List(0, 1, 1, 1, 2, 2)),
          List(List(0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 1, 1), List(0, 0, 0, 1, 1, 1), List(1, 0, 1, 1, 1, 1), List(0, 1, 1, 1, 1, 2), List(0, 1, 1, 1, 2, 3)),
          List(List(0, 0, 0, 0, 1, 0), List(0, 1, 0, 1, 1, 1), List(0, 0, 0, 1, 1, 2), List(0, 1, 1, 1, 1, 2), List(1, 1, 1, 1, 2, 3), List(0, 1, 2, 2, 3, 4)),
          List(List(0, 0, 0, 0, 0, 1), List(0, 0, 1, 1, 1, 2), List(0, 1, 1, 1, 2, 2), List(0, 1, 1, 1, 2, 3), List(0, 1, 2, 2, 3, 4), List(1, 2, 2, 3, 4, 6)))

      val AH1FieldGenerator: Polynomial[Int] = {
        Polynomial(2 -> 1, 0 -> -17)
      }
      val AH1FieldGeneratorApproximation: Double = 4.12311
      val AH1FieldGeneratorEpsilon: Double = 0.00001
      val AH1Dimensions: Seq[Polynomial[Fraction[Int]]] = {
        import Implicits.{ integersAsRationals }
        Seq(
          Polynomial(0 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(3, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(5, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(7, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(4, 1), 1 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(11, 2), 1 -> Fraction(3, 2)))
      }
      FusionRing(AH1Multiplicities, AH1FieldGenerator, AH1FieldGeneratorApproximation, AH1FieldGeneratorEpsilon, AH1Dimensions).ensuring(_.verifyAssociativity).ensuring(_.verifyIdentity)
    }

  }
}
