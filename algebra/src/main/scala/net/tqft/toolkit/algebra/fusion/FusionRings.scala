package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.algebra.diophantine._
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebra
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial

object FusionRings {
  def withObject(m: Matrix[Int], knownRing: Option[FusionRing[Int]] = None): Iterable[FusionRing[Int]] = {
    val rank = m.numberOfColumns
    require(m.entries.head == 0 :: 1 :: List.fill(rank - 2)(0))

    type V = (Int, Int, Int)

    implicit val polynomialAlgebra = implicitly[MultivariablePolynomialAlgebra[Int, V]]

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

    val polynomials = variableRing.associativityConstraints.map(p => polynomialAlgebra.subtract(p._1, p._2)).toSeq
    val (solutions, tooHard) = BoundedDiophantineSolver.solve(polynomials, variables, knownSolution = knownSolution)
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
        Seq(
          Polynomial(0 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(5, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(3, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(1, 2), 1 -> Fraction(1, 2)))
      }
      FusionRing(haagerup4Multiplicities, haagerup4FieldGenerator, haagerup4FieldGeneratorApproximation, haagerup4FieldGeneratorEpsilon, haagerup4Dimensions)//.ensuring(_.verifyAssociativity).ensuring(_.verifyIdentity)
    }

    val AHFieldGenerator: Polynomial[Int] = {
      Polynomial(2 -> 1, 0 -> -17)
    }
    val AHFieldGeneratorApproximation: Double = 4.12311
    val AHFieldGeneratorEpsilon: Double = 0.00001

    val AH1: FusionRingWithDimensions = {
      val AH1Multiplicities: Seq[Matrix[Int]] =
        List(
          List(List(1, 0, 0, 0, 0, 0), List(0, 1, 0, 0, 0, 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 1, 0), List(0, 0, 0, 0, 0, 1)),
          List(List(0, 1, 0, 0, 0, 0), List(1, 1, 0, 0, 1, 0), List(0, 0, 1, 0, 0, 1), List(0, 0, 0, 0, 1, 1), List(0, 1, 0, 1, 1, 1), List(0, 0, 1, 1, 1, 2)),
          List(List(0, 0, 1, 0, 0, 0), List(0, 0, 1, 0, 0, 1), List(1, 1, 1, 0, 0, 1), List(0, 0, 0, 1, 1, 1), List(0, 0, 0, 1, 1, 2), List(0, 1, 1, 1, 2, 2)),
          List(List(0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 1, 1), List(0, 0, 0, 1, 1, 1), List(1, 0, 1, 1, 1, 1), List(0, 1, 1, 1, 1, 2), List(0, 1, 1, 1, 2, 3)),
          List(List(0, 0, 0, 0, 1, 0), List(0, 1, 0, 1, 1, 1), List(0, 0, 0, 1, 1, 2), List(0, 1, 1, 1, 1, 2), List(1, 1, 1, 1, 2, 3), List(0, 1, 2, 2, 3, 4)),
          List(List(0, 0, 0, 0, 0, 1), List(0, 0, 1, 1, 1, 2), List(0, 1, 1, 1, 2, 2), List(0, 1, 1, 1, 2, 3), List(0, 1, 2, 2, 3, 4), List(1, 2, 2, 3, 4, 6)))

      val AH1Dimensions: Seq[Polynomial[Fraction[Int]]] = {
        Seq(
          Polynomial(0 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(3, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(5, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(7, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(4, 1), 1 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(11, 2), 1 -> Fraction(3, 2)))
      }
      FusionRing(AH1Multiplicities, AHFieldGenerator, AHFieldGeneratorApproximation, AHFieldGeneratorEpsilon, AH1Dimensions)//.ensuring(_.verifyAssociativity).ensuring(_.verifyIdentity)
    }
    val AH2: FusionRingWithDimensions = {
      val multiplicityString = """0 0 0 0 0 0 0 0 
0 0 0 1 0 0 0 0 
0 1 0 0 0 0 0 0 
0 0 0 0 1 0 0 0 
0 0 1 0 0 0 0 0 
0 0 0 0 0 0 1 0 
0 0 0 0 0 1 0 0 
0 0 0 0 0 0 0 1 

0 0 1 0 0 0 0 0 
0 1 0 0 0 1 0 0 
0 0 0 0 1 0 0 1 
1 0 1 0 0 0 1 0 
0 0 0 1 0 0 0 1 
0 1 0 0 0 1 1 1 
0 0 1 0 0 1 1 1 
0 0 0 1 1 1 1 1 

0 1 0 0 0 0 0 0 
0 0 0 0 1 0 0 1 
0 1 0 0 0 1 0 0 
0 0 0 1 0 0 0 1 
1 0 1 0 0 0 1 0 
0 0 1 0 0 1 1 1 
0 1 0 0 0 1 1 1 
0 0 0 1 1 1 1 1 

0 0 0 0 1 0 0 0 
1 0 0 1 0 0 1 0 
0 0 1 0 0 0 0 1 
0 0 0 0 1 1 0 0 
0 1 0 0 0 0 0 1 
0 0 0 1 0 1 1 1 
0 0 0 0 1 1 1 1 
0 1 1 0 0 1 1 1 

0 0 0 1 0 0 0 0 
0 0 1 0 0 0 0 1 
1 0 0 1 0 0 1 0 
0 1 0 0 0 0 0 1 
0 0 0 0 1 1 0 0 
0 0 0 0 1 1 1 1 
0 0 0 1 0 1 1 1 
0 1 1 0 0 1 1 1 

0 0 0 0 0 0 1 0 
0 1 0 0 0 1 1 1 
0 0 0 1 0 1 1 1 
0 0 1 0 0 1 1 1 
0 0 0 0 1 1 1 1 
0 1 1 1 1 2 2 2 
1 1 1 1 1 2 2 2 
0 1 1 1 1 2 2 3 

0 0 0 0 0 1 0 0 
0 0 0 1 0 1 1 1 
0 1 0 0 0 1 1 1 
0 0 0 0 1 1 1 1 
0 0 1 0 0 1 1 1 
1 1 1 1 1 2 2 2 
0 1 1 1 1 2 2 2 
0 1 1 1 1 2 2 3 

0 0 0 0 0 0 0 1 
0 0 1 0 1 1 1 1 
0 0 1 0 1 1 1 1 
0 1 0 1 0 1 1 1 
0 1 0 1 0 1 1 1 
0 1 1 1 1 2 2 3 
0 1 1 1 1 2 2 3 
1 1 1 1 1 3 3 2 """

      import net.tqft.toolkit.Extractors.Int
      val parsedMultiplicities: Seq[Seq[Seq[Int]]] = multiplicityString.split("\n\n").toSeq.map(p => p.split("\n").toSeq.map(_.split(" ").toSeq.collect({ case Int(n) => n })))
      
      val dualData = Seq(0,1,2,4,3,5,6,7,8)
      
      val AH2Multiplicities: Seq[Matrix[Int]] = Matrix.identityMatrix[Int](9) +: (for (k <- 0 until 8) yield {
        val m = Matrix(8, parsedMultiplicities.map(_(k)).transpose)
        val firstRow = Seq.fill(8)(0).updated(k, 1)
        val firstColumn = Seq.fill(9)(0).updated(dualData(k + 1), 1)
        m.prependRow(firstRow).prependColumn(firstColumn)
      })

      val AH2Dimensions: Seq[Polynomial[Fraction[Int]]] = {
        Seq(
          Polynomial(0 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(3, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(3, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(3, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(3, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(4, 1), 1 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(4, 1), 1 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(5, 1), 1 -> Fraction(1, 1)))
      }
      FusionRing(AH2Multiplicities, AHFieldGenerator, AHFieldGeneratorApproximation, AHFieldGeneratorEpsilon, AH2Dimensions)//.ensuring(_.verifyAssociativity).ensuring(_.verifyIdentity)
    }

  }
}
