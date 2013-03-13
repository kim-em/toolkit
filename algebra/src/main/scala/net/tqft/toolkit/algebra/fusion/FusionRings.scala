package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.algebra.diophantine._
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebra
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import net.tqft.toolkit.algebra.grouptheory.FiniteGroup
import net.tqft.toolkit.algebra.enumeration._

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
    val solutions = BoundedDiophantineSolver.solve(polynomials, variables, { m: Map[V, Int] => ??? }, knownSolution = knownSolution)

    for (solution <- solutions) yield {
      val structureCoefficients = variableStructureCoefficients.map(_.mapEntries({
        case p if p.totalDegree == Some(1) => solution(p.toMap.head._1.keySet.iterator.next)
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

    // we could do these 'by hand', but BoundedDiophantineSolver is good enough at it
    // TODO actually, get rid of depth and identity conditions by solving them by hand
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

      FusionRing(structureCoefficients)
    }

    val Ss = for (i <- 0 until rank + 1; v = variableRing.structureCoefficients(i); vd = variableRing.structureCoefficients(newDuality(i))) yield {
      Matrices.over(polynomialAlgebra).compose(v, vd)
    }

    val limit: (V =>? Int) => Boolean = {

      { values: (V =>? Int) =>
        val mSs = Ss.map(_.mapEntries(p => polynomialAlgebra.completelySubstituteConstants(values)(p)))
        mSs.map(m => FrobeniusPerronEigenvalues.estimateWithEigenvector(m.entries.map(_.toArray).toArray)._1).sum - 0.0001 < globalDimensionLimit
      }
    }

    val solver = new BoundedDiophantineSolver2[V]
    import net.tqft.toolkit.Profiler

    val (t00, linearSolve) = Profiler.timing(solver.solve(polynomials).get)

    val (t0, caseBashRemainingEquations) = Profiler.timing(linearSolve.caseBashEquations(limit))

    //        val (t1, solutions1) = Profiler.timing(caseBashRemainingEquations.iterator.flatMap(_.caseBashCompletely(variables, limit)).toList)

    val (t2, solutions2) = Profiler.timing((for (e <- caseBashRemainingEquations; f <- fastCaseBash(Ss.map(_.mapEntries(p => polynomialAlgebra.substitute(e.substitutions)(p))), globalDimensionLimit)) yield {
      e.substitutions.mapValues(p => polynomialAlgebra.completelySubstituteConstants(f)(p)) ++ f
    }))

    def connected_?(f: FusionRing[Int]) = {
      if (maxdepth == 1) {
        true
      } else {
        val generator = f.sum(depths.zipWithIndex.collect({ case (1, i) => i }).map(f.basis))
        f.multiplyByBasisElement(generator, rank).zip(depths).collect({ case (k, d) if d == maxdepth - 1 => k }).sum > 0
      }
    }

    solutions2.map(reconstituteRing).filter(connected_?)
  }

  def fastCaseBash[V](matrices: Seq[Matrix[MultivariablePolynomial[Int, V]]], L: Double): Iterator[Map[V, Int]] = {
    val n = matrices.size

    val variables = matrices.flatMap(_.entries.flatten).flatMap(_.variables).distinct.toIndexedSeq
    val s = variables.size

    lazy val variablesAppearances = for (v <- variables) yield {
      (for ((m, i) <- matrices.zipWithIndex; (r, j) <- m.entries.zipWithIndex; (x, k) <- r.zipWithIndex; if x.variables.contains(v)) yield (i, j, k))
    }

    case class QuadraticForm(constant: Int, linears: List[(Int, Int)], quadratics: List[(Int, Int, Int)]) {
      def substitute(substitution: Array[Int]) = {
        constant + linears.foldLeft(0)({ (s, p) => s + p._1 * substitution(p._2) }) + quadratics.foldLeft(0)({ (s, t) => s + t._1 * substitution(t._2) * substitution(t._3) })
      }
    }

    def polynomial2QuadraticForm(p: MultivariablePolynomial[Int, V]): QuadraticForm = {
      QuadraticForm(
        p.constantTerm,
        p.termsOfDegree(1).toList.map(p => (p._2, variables.indexOf(p._1.keys.head))),
        p.termsOfDegree(2).toList.map(p => {
          val coefficient = p._2
          val vs = p._1.keys.toList
          val (v1, v2) = vs match {
            case v :: Nil => (v, v)
            case v :: w :: Nil => (v, w)
          }
          (coefficient, variables.indexOf(v1), variables.indexOf(v2))
        }))
    }

    lazy val quadraticForms = Array.tabulate(n, n, n)({ (i, j, k) =>
      polynomial2QuadraticForm(matrices(i).entries(j)(k))
    })

    var lastSubstitution = Array.fill(s)(0)
    val lastMatrices = matrices.map(m => m.mapEntries(_.constantTerm).entries.map(_.toArray).toArray).toArray
    val lastEstimates = lastMatrices.map(m => FrobeniusPerronEigenvalues.estimateWithEigenvector(m)._1)

    // TODO take as argument a bitset, instead?
    val multivariableAppearances = {
      import net.tqft.toolkit.functions.Memo._
      { s: Seq[Int] => s.flatMap(i => variablesAppearances(i)).distinct }.memo
    }

    def estimates(substitution: Array[Int]): Array[Double] = {
      val dirtyMatrices = Array.fill(n)(false)

      if (substitution != lastSubstitution) {
        for ((i, j, k) <- multivariableAppearances((0 until s).filter(i => substitution(i) != lastSubstitution(i)))) {
          val newValue = quadraticForms(i)(j)(k).substitute(substitution)
          if (lastMatrices(i)(j)(k) != newValue) {
            lastMatrices(i)(j)(k) = newValue
            dirtyMatrices(i) = true
          }
        }

        lastSubstitution = substitution
      }
      for (i <- 0 until n; if dirtyMatrices(i)) {
        lastEstimates(i) = FrobeniusPerronEigenvalues.estimateWithEigenvector(lastMatrices(i))._1
      }
      lastEstimates
    }

    def limit(substitution: Array[Int]): Boolean = {
      estimates(substitution).sum - 0.0001 < L
    }

    Odometer(Array.fill(s)(0))(ArrayOdometer.odometerInt, limit).iterator.map(variables.zip(_).toMap)

  }

  def withAnotherPairOfDualObjects(ring: FusionRing[Int], maxdepth: Int, depths: Seq[Int], globalDimensionLimit: Double): Iterator[FusionRing[Int]] = {
    val rank = ring.rank
    type V = (Int, Int)

    implicit val polynomialAlgebra = implicitly[MultivariablePolynomialAlgebra[Int, V]]

    // V(j,k) = (n_0 x_j, x_k) = (n_1 x_k, x_j)

    def v(j: Int, k: Int) = {
      if (j < rank && k < rank && depths(j) + depths(k) < maxdepth) {
        polynomialAlgebra.zero
      } else {
        if (j == 0) {
          if (k == rank) {
            polynomialAlgebra.one
          } else {
            polynomialAlgebra.zero
          }
        } else if (k == 0) {
          if (j == rank + 1) {
            polynomialAlgebra.one
          } else {
            polynomialAlgebra.zero
          }
        } else {
          polynomialAlgebra.monomial((j, k))
        }
      }
    }

    val variableRing = {
      val augmentedMatrices = for ((m, i) <- ring.structureCoefficients.zipWithIndex) yield {
        val lift = m.mapEntries(polynomialAlgebra.constant)
        val newColumn0 = for (k <- 0 until rank) yield {
          // (x_i x_k, n_0) = (x_i, n_0 x_k*) = (n_1 x_i, x_k*)
          v(ring.duality(k), i)
        }
        val newColumn1 = for (k <- 0 until rank) yield {
          // (x_i x_k, n_1) = (x_i, n_1 x_k*) = (n_0 x_i, x_k*)
          v(i, ring.duality(k))
        }
        val newRow0 = (for (j <- 0 until rank) yield {
          // (x_i n_0, x_j) = (n_0, x_i* x_j) = (n_0 x_j*, x_i*)
          v(ring.duality(j), ring.duality(i))
        }) :+ v(rank + 1, ring.duality(i)) :+ v(rank, ring.duality(i))
        val newRow1 = (for (j <- 0 until rank) yield {
          // (x_i n_1, x_j) = (n_1, x_i* x_j) = (n_1 x_j*, x_i*)
          v(ring.duality(i), ring.duality(j))
        }) :+ v(ring.duality(i), rank + 1) :+ v(ring.duality(i), rank)
        lift.appendColumn(newColumn0).appendColumn(newColumn1).appendRow(newRow0).appendRow(newRow1)
      }
      val newMatrices = {
        for (k <- 0 to 1) yield {
          (for (i <- 0 until rank + 2) yield {
            for (j <- 0 until rank + 2) yield {
              if (k == 0) {
                v(i, j)
              } else {
                v(j, i)
              }
            }
          }): Matrix[MultivariablePolynomial[Int, V]]
        }
      }

      FusionRing(augmentedMatrices ++ newMatrices)
    }
    val variables = for (i <- 0 until rank + 2; j <- 0 until rank + 2; x <- v(i, j).variables) yield x

    val newDuality = ring.duality :+ (rank + 1) :+ rank

    val dualityConstraints = variableRing.dualityConstraints(newDuality)
    val associativityConditions = variableRing.partialAssociativityConstraints(maxdepth, depths)

    val polynomials = (dualityConstraints ++ associativityConditions).map(p => polynomialAlgebra.subtract(p._1, p._2))

    def reconstituteRing(m: Map[V, Int]): FusionRing[Int] = {
      val m0 = m.orElse[(Int, Int), Int]({
        case (0, k) if k == rank => 1
        case (j, 0) if j == rank + 1 => 1
        case _ => 0
      })

      def lookup(a: Int, b: Int, c: Int) = {
        a match {
          case 0 => m0((b, c))
          case 1 => m0((c, b))
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

      FusionRing(structureCoefficients)
    }

    val Ss = for (i <- 0 until rank + 2; v = variableRing.structureCoefficients(i); vd = variableRing.structureCoefficients(newDuality(i))) yield {
      Matrices.over(polynomialAlgebra).compose(v, vd)
    }

    val limit: (V =>? Int) => Boolean = {

      { values: (V =>? Int) =>
        val mSs = Ss.map(_.mapEntries(p => polynomialAlgebra.completelySubstituteConstants(values)(p)))
        mSs.map(m => FrobeniusPerronEigenvalues.estimateWithEigenvector(m.entries.map(_.toArray).toArray)._1).sum - 0.0001 < globalDimensionLimit
      }
    }

    val solver = new BoundedDiophantineSolver2[V]
    import net.tqft.toolkit.Profiler
    val (t00, linearSolve) = Profiler.timing(solver.solve(polynomials).get)

    val (t0, caseBashRemainingEquations) = Profiler.timing(linearSolve.caseBashEquations(limit))

    //    val solutions1 = caseBashRemainingEquations.flatMap(_.caseBashCompletely(variables, limit))

    val (t2, solutions2) = Profiler.timing((for (e <- caseBashRemainingEquations; f <- fastCaseBash(Ss.map(_.mapEntries(p => polynomialAlgebra.substitute(e.substitutions)(p))), globalDimensionLimit)) yield {
      e.substitutions.mapValues(p => polynomialAlgebra.completelySubstituteConstants(f)(p)) ++ f
    }))

    def connected_?(f: FusionRing[Int]) = {
      if (maxdepth == 1) {
        true
      } else {
        val generator = f.sum(depths.zipWithIndex.collect({ case (1, i) => i }).map(f.basis))
        f.multiplyByBasisElement(generator, rank).zip(depths).collect({ case (k, d) if d == maxdepth - 1 => k }).sum > 0 &&
          f.multiplyByBasisElement(generator, rank + 1).zip(depths).collect({ case (k, d) if d == maxdepth - 1 => k }).sum > 0
      }
    }
    def ordered_?(f: FusionRing[Int]) = {
      import net.tqft.toolkit.collections.LexicographicOrdering._
      implicitly[Ordering[Seq[Seq[IndexedSeq[Int]]]]].compare(f.structureCoefficients.map(_.entries.seq), f.relabel((0 until rank) :+ (rank + 1) :+ rank).structureCoefficients.map(_.entries.seq)) <= 0
    }

    solutions2.map(reconstituteRing).filter(connected_?).filter(ordered_?)
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
