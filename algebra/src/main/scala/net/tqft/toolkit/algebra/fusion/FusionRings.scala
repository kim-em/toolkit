package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.algebra.diophantine._
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebra
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.grouptheory.FiniteGroup

object FusionRings {
  def withObject(m: Matrix[Int], knownRing: Option[FusionRing[Int]] = None): Iterator[FusionRing[Int]] = {
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
    val variableRing = FusionRing(variableStructureCoefficients)

    val variables = for (i <- 2 until rank; j <- 0 until rank; k <- 0 until rank) yield (i, j, k)

    val knownSolution = knownRing.map({ ring =>
      val rsc = ring.structureCoefficients
      (for (i <- 2 until rank; j <- 0 until rank; k <- 0 until rank) yield (i, j, k) -> rsc(i).entries(j)(k)).toMap
    })

    // we don't use the duality constraints here, because we don't know how duality works!
    // we don't use the identity constraints here, because we've taken care of that above
    val polynomials = (variableRing.generalDualityConstraints ++ variableRing.associativityConstraints).map(p => polynomialAlgebra.subtract(p._1, p._2)).toSeq
    // TODO we should be passing a limit here --- we know the dimensions of all the objects
    val solutions = BoundedDiophantineSolver.solve(polynomials, variables, knownSolution = knownSolution)

    for (solution <- solutions) yield {
      val structureCoefficients = variableStructureCoefficients.map(_.mapEntries({
        case p if p.totalDegree == Some(1) => solution(p.terms.head._1.keySet.iterator.next)
        case p => p.constantTerm
      }))
      FusionRing(structureCoefficients).ensuring(_.structureCoefficients(1) == m)
    }
  }

  //  (xn, y) = (x, yn) = (y^* x, n) = (n y^*, x^*)

  def withAnotherSelfDualObject(ring: FusionRing[Int], maxdepth: Int, depths: Seq[Int], globalDimensionLimit: Double): Iterator[FusionRing[Int]] = {
    val rank = ring.rank
    type V = (Int, Int)

    implicit val polynomialAlgebra = implicitly[MultivariablePolynomialAlgebra[Int, V]]

    // V(i,j) = (n x_i, x_j)

    val variableRing = {
      val augmentedMatrices = for ((m, i) <- ring.structureCoefficients.zipWithIndex) yield {
        val lift = m.mapEntries(polynomialAlgebra.constant)
        val newColumn = for (k <- 0 until rank) yield {
          // (x_i x_k, n) = (x_i, n x_k*) = (n x_i, x_k*)
          polynomialAlgebra.monomial((i, ring.duality(k)))
        }
        val newRow = (for (j <- 0 until rank) yield {
          // (x_i n, x_j) = (n, x_i* x_j) = (n x_j*, x_i*)
          polynomialAlgebra.monomial((ring.duality(j), ring.duality(i)))
        }) :+ polynomialAlgebra.monomial((rank, ring.duality(i)))
        lift.appendColumn(newColumn).appendRow(newRow)
      }
      val newMatrix: Matrix[MultivariablePolynomial[Int, V]] = {
        for (i <- 0 until rank + 1) yield {
          for (j <- 0 until rank + 1) yield {
            polynomialAlgebra.monomial((i, j))
          }
        }
      }

      FusionRing(augmentedMatrices :+ newMatrix)
    }
    val variables = for (i <- 0 until rank + 1; j <- 0 until rank + 1) yield (i, j)

    val newDuality = ring.duality :+ rank

    // we could do these 'by hand', by BoundedDiophantineSolver is good enough at it
    val depthConditions = for (i <- (0 until rank).iterator; j <- 0 until rank; if depths(i) + depths(j) < maxdepth) yield (polynomialAlgebra.monomial((i, j)), polynomialAlgebra.zero)
    val identityConditions = variableRing.identityConstraints
    val dualityConditions = variableRing.dualityConstraints(newDuality)
    // the associativity conditions are actually nontrivial!
    val associativityConditions = variableRing.partialAssociativityConstraints(maxdepth, depths)

    val polynomials = (depthConditions ++ identityConditions ++ dualityConditions ++ associativityConditions).map(p => polynomialAlgebra.subtract(p._1, p._2))

    def reconstituteRing(m: Map[V, Int]): FusionRing[Int] = {
      val structureCoefficients = (IndexedSeq.tabulate(rank + 1, rank + 1, rank + 1) { (i, j, k) =>
        if (i < rank) {
          if (j < rank) {
            if (k < rank) {
              ring.structureCoefficients(i).entries(j)(k)
            } else {
              m((i, ring.duality(j)))
            }
          } else {
            if (k < rank) {
              m((ring.duality(k), ring.duality(i)))
            } else {
              m((rank, ring.duality(i)))
            }
          }
        } else {
          m((j, k))
        }
      }).map(m => m: Matrix[Int])

      //      require((variables diff m.keys.toSeq).isEmpty)
      //
      //      val structureCoefficients2 = variableRing.structureCoefficients.map(_.mapEntries({
      //        case p if p.totalDegree == Some(1) => m(p.terms.head._1.keySet.iterator.next)
      //        case p => p.constantTerm
      //      }))
      //
      //      require(structureCoefficients == structureCoefficients2, structureCoefficients + "\n" + structureCoefficients2)

      FusionRing(structureCoefficients)
    }

    // TODO evaluate whether this is actually faster!
    val limit2: Map[V, Int] => Boolean = {
      val Ss = for (i <- 0 until rank + 1; v = variableRing.structureCoefficients(i); vd = variableRing.structureCoefficients(newDuality(i))) yield {
        Matrices.over(polynomialAlgebra).compose(v, vd)
      }

      { values: Map[V, Int] =>
        val mSs = Ss.map(_.mapEntries(p => polynomialAlgebra.completelySubstituteConstants(values)(p)))
        mSs.map(m => FrobeniusPerronEigenvalues.estimateWithEigenvector(m)._1).sum - 0.0001 < globalDimensionLimit
      }
    }

    def limit(values: Map[V, Int]): Boolean = reconstituteRing(values).globalDimensionLowerBound < globalDimensionLimit

    val solutions = BoundedDiophantineSolver.solve(polynomials, variables, Some(limit2))

    def connected_?(f: FusionRing[Int]) = {
      val i = 1
      val j = f.structureCoefficients(i).entries.indexWhere(_.head == 1)
      f.structureCoefficients(rank).entries(i).take(rank).sum + f.structureCoefficients(rank).entries(j).take(rank).sum > 0
    }

    solutions.map(reconstituteRing).filter(connected_?)
  }
  def withAnotherPairOfDualObjects(ring: FusionRing[Int], maxdepth: Int, depths: Seq[Int], globalDimensionLimit: Double): Iterator[FusionRing[Int]] = {
    val rank = ring.rank
    type V = (Int, Int)

    implicit val polynomialAlgebra = implicitly[MultivariablePolynomialAlgebra[Int, V]]

    // V(j,k) = (n_0 x_j, x_k) = (n_1 x_k, x_j)

    val variableRing = {
      val augmentedMatrices = for ((m, i) <- ring.structureCoefficients.zipWithIndex) yield {
        val lift = m.mapEntries(polynomialAlgebra.constant)
        val newColumn0 = for (k <- 0 until rank) yield {
          // (x_i x_k, n_0) = (x_i, n_0 x_k*) = (n_1 x_i, x_k*)
          polynomialAlgebra.monomial((ring.duality(k), i))
        }
        val newColumn1 = for (k <- 0 until rank) yield {
          // (x_i x_k, n_1) = (x_i, n_1 x_k*) = (n_0 x_i, x_k*)
          polynomialAlgebra.monomial((i, ring.duality(k)))
        }
        val newRow0 = (for (j <- 0 until rank) yield {
          // (x_i n_0, x_j) = (n_0, x_i* x_j) = (n_0 x_j*, x_i*)
          polynomialAlgebra.monomial((ring.duality(j), ring.duality(i)))
        }) :+ polynomialAlgebra.monomial((rank + 1, ring.duality(i))) :+ polynomialAlgebra.monomial((rank, ring.duality(i)))
        val newRow1 = (for (j <- 0 until rank) yield {
          // (x_i n_1, x_j) = (n_1, x_i* x_j) = (n_1 x_j*, x_i*)
          polynomialAlgebra.monomial((ring.duality(i), ring.duality(j)))
        }) :+ polynomialAlgebra.monomial((ring.duality(i), rank + 1)) :+ polynomialAlgebra.monomial((ring.duality(i), rank))
        lift.appendColumn(newColumn0).appendColumn(newColumn1).appendRow(newRow0).appendRow(newRow1)
      }
      val newMatrices = {
        for (k <- 0 to 1) yield {
          (for (i <- 0 until rank + 2) yield {
            for (j <- 0 until rank + 2) yield {
              if (k == 0) {
                polynomialAlgebra.monomial((i, j))
              } else {
                polynomialAlgebra.monomial((j, i))
              }
            }
          }): Matrix[MultivariablePolynomial[Int, V]]
        }
      }

      FusionRing(augmentedMatrices ++ newMatrices)
    }
    val variables = for (i <- 0 until rank + 2; j <- 0 until rank + 2) yield (i, j)

    val newDuality = ring.duality :+ (rank + 1) :+ rank

    val depthConditions = for (i <- (0 until rank).iterator; j <- 0 until rank; if depths(i) + depths(j) < maxdepth) yield (polynomialAlgebra.monomial((i, j)), polynomialAlgebra.zero)
    val identityConditions = variableRing.identityConstraints
    // TODO if we put in identityConditions by hand above, I think dualityConstraints would automatically be satisfied.
    //    for(((p1,p2),t) <-variableRing.dualityConstraints(newDuality).zipWithIndex) {
    //      println(t + ": " + p1 + " == " + p2)
    //      require(p1 == p2)
    //    }
    val dualityConstraints = variableRing.dualityConstraints(newDuality)
    val associativityConditions = variableRing.partialAssociativityConstraints(maxdepth, depths)

    val polynomials = (depthConditions ++ identityConditions ++ dualityConstraints ++ associativityConditions).map(p => polynomialAlgebra.subtract(p._1, p._2))

    def reconstituteRing(m: Map[V, Int]): FusionRing[Int] = {
      def lookup(a: Int, b: Int, c: Int) = {
        a match {
          case 0 => m((b, c))
          case 1 => m((c, b))
        }
      }
      val structureCoefficients = (IndexedSeq.tabulate(rank + 2, rank + 2, rank + 2) { (i, j, k) =>
        if (i < rank) {
          if (j < rank) {
            if (k < rank) {
              ring.structureCoefficients(i).entries(j)(k)
            } else {
              lookup(k - rank, ring.duality(j), i)
            }
          } else {
            if (k < rank) {
              lookup(j - rank, ring.duality(k), ring.duality(i))
            } else {
              lookup(j - rank, 2 * rank + 1 - k, ring.duality(i))
            }
          }
        } else {
          lookup(i - rank, j, k)
        }
      }).map(m => m: Matrix[Int])

      //      require((variables diff m.keys.toSeq).isEmpty)
      //
      //      val structureCoefficients2 = variableRing.structureCoefficients.map(_.mapEntries({
      //        case p if p.totalDegree == Some(1) => m(p.terms.head._1.keySet.iterator.next)
      //        case p => p.constantTerm
      //      }))
      //
      //      require(structureCoefficients == structureCoefficients2, structureCoefficients + "\n" + structureCoefficients2)

      FusionRing(structureCoefficients)
    }

    val limit2: Map[V, Int] => Boolean = {

      val Ss = for (i <- 0 until rank + 2; v = variableRing.structureCoefficients(i); vd = variableRing.structureCoefficients(newDuality(i))) yield {
        Matrices.over(polynomialAlgebra).compose(v, vd)
      }

      { values: Map[V, Int] =>
        val mSs = Ss.map(_.mapEntries(p => polynomialAlgebra.completelySubstituteConstants(values)(p)))
        mSs.map(m => FrobeniusPerronEigenvalues.estimateWithEigenvector(m)._1).sum - 0.0001 < globalDimensionLimit
      }
    }

    def limit(values: Map[V, Int]): Boolean = reconstituteRing(values).globalDimensionLowerBound < globalDimensionLimit

    val solutions = BoundedDiophantineSolver.solve(polynomials, variables, Some(limit2))

    def connected_?(f: FusionRing[Int]) = {
      val i = 1
      val j = f.structureCoefficients(i).entries.indexWhere(_.head == 1)
      (f.structureCoefficients(rank).entries(i).take(rank).sum +
        f.structureCoefficients(rank).entries(j).take(rank).sum > 0) &&
        (f.structureCoefficients(rank + 1).entries(i).take(rank).sum +
          f.structureCoefficients(rank + 1).entries(j).take(rank).sum > 0)
    }

    solutions.map(reconstituteRing).filter(connected_?)
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
      FusionRing(haagerup4Multiplicities, haagerup4FieldGenerator, haagerup4FieldGeneratorApproximation, haagerup4FieldGeneratorEpsilon, haagerup4Dimensions) //.ensuring(_.verifyAssociativity).ensuring(_.verifyIdentity)
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
      FusionRing(AH1Multiplicities, AHFieldGenerator, AHFieldGeneratorApproximation, AHFieldGeneratorEpsilon, AH1Dimensions) //.ensuring(_.verifyAssociativity).ensuring(_.verifyIdentity)
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

      val dualData = Seq(0, 1, 2, 4, 3, 5, 6, 7, 8)

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
      FusionRing(AH2Multiplicities, AHFieldGenerator, AHFieldGeneratorApproximation, AHFieldGeneratorEpsilon, AH2Dimensions).ensuring(_.verifyAssociativity).ensuring(_.verifyIdentity)
    }
    val AH3: FusionRingWithDimensions = {
      val multiplicityString = """0 0 0 0 0 0 0 0 
0 0 0 1 0 0 0 0 
0 1 0 0 0 0 0 0 
0 0 0 0 1 0 0 0 
0 0 1 0 0 0 0 0 
0 0 0 0 0 0 1 0 
0 0 0 0 0 1 0 0 
0 0 0 0 0 0 0 1 

0 0 1 0 0 0 0 0 
0 1 0 0 0 1 0 1 
0 0 0 0 1 1 1 0 
1 0 1 0 0 0 1 1 
0 0 0 1 0 1 1 0 
0 1 0 1 1 1 1 1 
0 0 1 1 1 1 1 1 
0 1 1 0 0 1 1 1 

0 1 0 0 0 0 0 0 
0 0 0 0 1 1 1 0 
0 1 0 0 0 1 0 1 
0 0 0 1 0 1 1 0 
1 0 1 0 0 0 1 1 
0 0 1 1 1 1 1 1 
0 1 0 1 1 1 1 1 
0 1 1 0 0 1 1 1 

0 0 0 0 1 0 0 0 
1 0 0 1 0 0 1 1 
0 0 1 0 0 1 1 0 
0 0 0 0 1 1 0 1 
0 1 0 0 0 1 1 0 
0 1 1 1 0 1 1 1 
0 1 1 0 1 1 1 1 
0 0 0 1 1 1 1 1 

0 0 0 1 0 0 0 0 
0 0 1 0 0 1 1 0 
1 0 0 1 0 0 1 1 
0 1 0 0 0 1 1 0 
0 0 0 0 1 1 0 1 
0 1 1 0 1 1 1 1 
0 1 1 1 0 1 1 1 
0 0 0 1 1 1 1 1 

0 0 0 0 0 0 1 0 
0 1 1 0 1 1 1 1 
0 0 1 1 1 1 1 1 
0 1 1 1 0 1 1 1 
0 1 0 1 1 1 1 1 
0 1 1 1 1 2 2 2 
1 1 1 1 1 2 2 2 
0 1 1 1 1 2 2 1 

0 0 0 0 0 1 0 0 
0 0 1 1 1 1 1 1 
0 1 1 0 1 1 1 1 
0 1 0 1 1 1 1 1 
0 1 1 1 0 1 1 1 
1 1 1 1 1 2 2 2 
0 1 1 1 1 2 2 2 
0 1 1 1 1 2 2 1 

0 0 0 0 0 0 0 1 
0 1 0 1 0 1 1 1 
0 1 0 1 0 1 1 1 
0 0 1 0 1 1 1 1 
0 0 1 0 1 1 1 1 
0 1 1 1 1 2 2 1 
0 1 1 1 1 2 2 1 
1 1 1 1 1 1 1 2 """

      import net.tqft.toolkit.Extractors.Int
      val parsedMultiplicities: Seq[Seq[Seq[Int]]] = multiplicityString.split("\n\n").toSeq.map(p => p.split("\n").toSeq.map(_.split(" ").toSeq.collect({ case Int(n) => n })))
      val dualData = Seq(0, 1, 2, 4, 3, 5, 6, 7, 8)

      val AH3Multiplicities: Seq[Matrix[Int]] = Matrix.identityMatrix[Int](9) +: (for (k <- 0 until 8) yield {
        val m = Matrix(8, parsedMultiplicities.map(_(k)).transpose)
        val firstRow = Seq.fill(8)(0).updated(k, 1)
        val firstColumn = Seq.fill(9)(0).updated(dualData(k + 1), 1)
        m.prependRow(firstRow).prependColumn(firstColumn)
      })

      val AH3Dimensions: Seq[Polynomial[Fraction[Int]]] = {
        Seq(
          Polynomial(0 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(5, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(5, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(5, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(5, 2), 1 -> Fraction(1, 2)),
          Polynomial(0 -> Fraction(4, 1), 1 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(4, 1), 1 -> Fraction(1, 1)),
          Polynomial(0 -> Fraction(3, 1), 1 -> Fraction(1, 1)))
      }
      FusionRing(AH3Multiplicities, AHFieldGenerator, AHFieldGeneratorApproximation, AHFieldGeneratorEpsilon, AH3Dimensions).ensuring(_.verifyAssociativity).ensuring(_.verifyIdentity)
    }

    def rank1 = FusionRing(IndexedSeq(IndexedSeq(IndexedSeq(1)): Matrix[Int]))

    def rank2(m: Int) = {
      val identity: Matrix[Int] = IndexedSeq(IndexedSeq(1, 0), IndexedSeq(0, 1))
      val other: Matrix[Int] = IndexedSeq(IndexedSeq(0, 1), IndexedSeq(1, m))
      FusionRing(IndexedSeq(identity, other))
    }

    //    def selfDualGenerators(globalDimensionUpperBound: Double) = {
    //      Iterator.from(0).map(rank2).takeWhile(_.globalDimensionLowerBound < globalDimensionUpperBound)
    //    }
    //
    //    def nonSelfDualGenerators(globalDimensionUpperBound: Double) = {
    //      def R(a: Int, b: Int) = {
    //        val identity: Matrix[Int] = IndexedSeq(IndexedSeq(1, 0, 0), IndexedSeq(0, 1, 0), IndexedSeq(0, 0, 1))
    //        val V: Matrix[Int] = IndexedSeq(IndexedSeq(0, 1, 0), IndexedSeq(0, a, b), IndexedSeq(1, a, a))
    //        val Vd: Matrix[Int] = IndexedSeq(IndexedSeq(0, 0, 1), IndexedSeq(1, a, a), IndexedSeq(0, b, a))
    //        FusionRing(IndexedSeq(identity, V, Vd))
    //      }
    //      Iterator.from(0).takeWhile(a => R(a, 0).globalDimensionLowerBound < globalDimensionUpperBound).flatMap({ a =>
    //        Iterator.from(0).takeWhile(b => R(a, b).globalDimensionLowerBound < globalDimensionUpperBound).map({ b =>
    //          R(a, b)
    //        })
    //      })
    //    }
    //
    //    def allGenerators(globalDimensionUpperBound: Double) = selfDualGenerators(globalDimensionUpperBound) ++ nonSelfDualGenerators(globalDimensionUpperBound)

    def representationsOf[T](G: FiniteGroup[T]) = FusionRing(G.tensorProductMultiplicities.map(p => p: Matrix[Int]))

  }
}
