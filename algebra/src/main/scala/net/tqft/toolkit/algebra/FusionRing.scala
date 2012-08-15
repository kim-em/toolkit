package net.tqft.toolkit.algebra

import net.tqft.toolkit.permutations.Permutation

trait FiniteDimensionalFreeModule[A] extends Module[A, Seq[A]] {
  def coefficients: Ring[A]
  def rank: Int
  override lazy val zero = Seq.fill(rank)(coefficients.zero)
  override def add(x: Seq[A], y: Seq[A]) = x.zip(y).map(p => coefficients.add(p._1, p._2))
  override def scalarMultiply(a: A, b: Seq[A]) = b.map(x => coefficients.multiply(a, x))
  override def negate(x: Seq[A]) = x.map(coefficients.negate)
  def basis = for (i <- 0 until rank) yield for (j <- 0 until rank) yield if (i == j) coefficients.one else coefficients.zero
}

// Usually A = Int, for a concrete fusion ring. We allow other possibilities so we can write fusion solvers, etc.
trait FusionRing[A] extends FiniteDimensionalFreeModule[A] with Rig[Seq[A]] { fr =>

  override def fromInt(x: Int) = coefficients.fromInt(x) +: Seq.fill(rank - 1)(coefficients.zero)
  override val one = fromInt(1)

  def associativityConstraints = for (x <- basis.iterator; y <- basis; z <- basis) yield subtract(multiply(x, multiply(y, z)), multiply(multiply(x, y), z))
  def identityConstraints = (for (x <- basis.iterator) yield Seq(subtract(x, multiply(one, x)), subtract(x, multiply(x, one)))).flatten
  def dualityConstraints(duality: Permutation = duality) = for (x <- basis.iterator; y <- basis) yield {
    import net.tqft.toolkit.permutations.Permutations._
    subtract(duality.permute(multiply(x, y)), multiply(duality.permute(y), duality.permute(y)))
  }

  def duality: Permutation = {
    structureCoefficients.map(_.entries.indexWhere(_.head == coefficients.one).ensuring(_ != -1))
  }

  def verifyAssociativity = associativityConstraints.map(_ == zero).reduce(_ && _)
  def verifyIdentity = identityConstraints.map(_ == zero).reduce(_ && _)

  lazy val structureCoefficients = for (y <- basis) yield Matrix(rank, for (x <- basis) yield multiply(x, y))

  trait FusionModule extends FiniteDimensionalFreeModule[A] { fm =>
    override def coefficients = fr.coefficients

    def fusionRing = fr

    def act(x: Seq[A], m: Seq[A]): Seq[A]
    def associativityConstraints = for (x <- fr.basis.iterator; y <- fr.basis; z <- basis) yield subtract(act(x, act(y, z)), act(fr.multiply(x, y), z))
    def identityConstraints = for (x <- basis.iterator) yield subtract(x, act(fr.one, x))
    def verifyAssociativity = associativityConstraints.map(_ == zero).reduce(_ && _)
    def asMatrix(x: Seq[A]) = new Matrix(rank, for (b <- basis) yield act(x, b))

    def structureCoefficients = for (y <- basis) yield Matrix(rank, for (x <- fr.basis) yield act(x, y))

    override def equals(other: Any) = {
      other match {
        case other: fr.FusionModule => structureCoefficients == other.structureCoefficients
        case _ => false
      }
    }
    override def hashCode = (fr, structureCoefficients).hashCode

  }

  object FusionModules {
    def equivalent_?(m1: FusionModule, m2: FusionModule) = {
      if (m1.rank == m2.rank) {
        import net.tqft.toolkit.permutations.Permutations
        import net.tqft.toolkit.permutations.Permutations._
        
        val s1 = m1.structureCoefficients
        val s2 = m2.structureCoefficients
        
        Permutations.of(m1.rank).find(p => s2 == p.permute(s1.map(m => m.permuteColumns(p)))).nonEmpty
      } else {
        false
      }
    }
  }

  protected class StructureCoefficientFusionModule(matrices: Seq[Matrix[A]]) extends FusionModule {
    for (m <- matrices) {
      require(m.numberOfColumns == matrices.size)
      require(m.numberOfRows == fr.rank)
    }
    override val rank = matrices.size
    override def act(x: Seq[A], m: Seq[A]) = {
      require(m.size == rank)
      val zero = coefficients.zero
      val terms = for (
        (xi, i) <- x.zipWithIndex;
        if (xi != zero);
        (mj, j) <- m.zipWithIndex;
        if (mj != zero)
      ) yield {
        for (k <- 0 until rank) yield coefficients.multiply(xi, mj, matrices(j).entries(i)(k))
      }
      val result = add(terms)
      require(m.size == result.size)
      result
    }
  }

  def moduleFromStructureCoefficients(matrices: Seq[Matrix[A]]): FusionModule = {
    (new StructureCoefficientFusionModule(matrices)).ensuring(_.structureCoefficients == matrices)
  }

  object regularModule extends FusionModule {
    override val rank = fr.rank
    override def act(x: Seq[A], m: Seq[A]) = fr.multiply(x, m)
  }
}

trait FusionRingWithDimensions extends FusionRing[Int] { fr =>
  def dimensionField: RealNumberField[Int, Double]
  def dimensions: Seq[Polynomial[Fraction[Int]]]
  def dimensionOf(x: Seq[Int]): Polynomial[Fraction[Int]] = {
    val polynomials = Polynomials.over(Gadgets.Rationals)
    import Implicits.integersAsRationals
    polynomials.add(x.zip(dimensions).map(p => polynomials.scalarMultiply(p._1, p._2)))
  }

  def dimensionBounds(x: Seq[Int]): Double = {
    dimensionField.approximateWithin(0.0001)(dimensionOf(x)) + 0.0001
  }

  def objectsSmallEnoughToBeAlgebras: Seq[Seq[Int]] = {
    val start = Seq(Seq(1))
    basis.tail.foldLeft(start)({
      (i: Seq[Seq[Int]], b: Seq[Int]) =>
        i.flatMap({
          a: Seq[Int] => for (m <- 0 to dimensionBounds(b).floor.intValue) yield a :+ m
        })
    })
  }

  def smallObjectsWithPositiveSemidefiniteMultiplication: Seq[Seq[Int]] = {
    for (
      o <- objectsSmallEnoughToBeAlgebras;
      m = regularModule.asMatrix(o);
      if (m.mapEntries(Implicits.integersAsRationals).positiveSemidefinite_?)
    ) yield o
  }

  trait FusionMatrix {
    def algebraObject: Seq[Int]
    def matrix: Matrix[Int]
    def dimensionsSquared: Seq[Polynomial[Fraction[Int]]]

    override def toString = "FusionMatrix(" + algebraObject + ", Matrix(" + matrix.numberOfColumns + ", " + matrix.entries + "))"
    override def equals(other: Any) = {
      other match {
        case other: FusionMatrix => algebraObject == other.algebraObject && matrix == other.matrix
        case _ => false
      }
    }
  }

  object FusionMatrix {
    def apply(algebraObject: Seq[Int], unsortedMatrix: Matrix[Int]): FusionMatrix = {
      val _algebraObject = algebraObject
      new FusionMatrix {
        val algebraObject = _algebraObject
        private val sorted = {
          val unsortedDimensionsSquared = {
            val dxi = fr.dimensionOf(algebraObject)
            import Implicits.{ integersAsRationals }
            val polynomials = Polynomials.over(Gadgets.Rationals)
            val Atd = unsortedMatrix.transpose.mapEntries(polynomials.constant(_)).apply(fr.dimensions)
            Atd.map(p => dimensionField.quotient(dimensionField.power(p, 2), dxi))
          }
          unsortedMatrix.entries.seq.transpose.zip(unsortedDimensionsSquared).sortBy(_._2)(dimensionField)
        }
        val matrix = Matrix(unsortedMatrix.numberOfColumns, sorted.map(_._1).transpose)
        val dimensionsSquared = sorted.map(_._2)
      }
    }
  }

  lazy val candidateFusionMatrices: Seq[FusionMatrix] = {
    // mention some internal objects first, so we don't deadlock trying to instantiate them in parallel below...
    regularModule
    FusionMatrix

    (for (
      o <- smallObjectsWithPositiveSemidefiniteMultiplication.par;
      a = regularModule.asMatrix(o);
      m <- Matrices.positiveSymmetricDecompositions(a)
    ) yield {
      FusionMatrix(o, m)
    }).seq
  }

  def candidateAlgebraObjects: Seq[Seq[Int]] = {
    import net.tqft.toolkit.collections.RemoveDuplicates._
    candidateFusionMatrices.map(_.algebraObject).removeDuplicates()
  }

  def candidateFusionModules: Iterator[FusionModule] = {
    val matricesBySize = candidateFusionMatrices.toList.groupBy(_.matrix.numberOfColumns)
    (for (n <- (1 to matricesBySize.keys.max).iterator) yield {

      println("Looking at rank " + n + " fusion matrices.")

      val classes = {
        matricesBySize.getOrElse(n, Nil).groupBy(_.dimensionsSquared).values.toSeq.par
      }

      println("found classes: ")
      for (c <- classes) {
        println("* with dimension vector " + c.head.dimensionsSquared.map(dimensionField.approximateWithin(0.001)))
        for (cc <- c) println(cc)
      }

      val n_tuples = {
        (for (c <- classes.par; d_squared = c.head.dimensionsSquared) yield {
          def acc(j: Int, partial: List[FusionMatrix]): Iterator[List[FusionMatrix]] = {
            j match {
              case 0 => Iterator(partial)
              case j => {
                for (
                  A <- c.iterator;
                  if (dimensionOf(A.algebraObject) == d_squared(j - 1));
                  r <- acc(j - 1, A :: partial)
                ) yield r
              }
            }
          }
          acc(n, Nil)
        }).flatten
      }

      println("possible " + n + "-tuples:")
      for (t <- n_tuples.seq) println(t)

      val integerMatrices = new MatrixCategoryOverRing(Gadgets.Integers)

      import net.tqft.toolkit.collections.GroupBy._
      
      (for (t <- n_tuples) yield {
        import net.tqft.toolkit.permutations.Permutations
        import net.tqft.toolkit.permutations.Permutations._
        val permutations = Permutations.preserving(t.head.dimensionsSquared).toSeq.par
        val permutedMatrices = t.foldLeft(Iterable(Seq.empty[Matrix[Int]]))({ (i: Iterable[Seq[Matrix[Int]]], M: FusionMatrix) =>
          def permuteColumns(p: Permutation) = new Matrix(M.matrix.numberOfColumns, p.permute(M.matrix.entries.seq.transpose).transpose)
          def verifyPartialAssociativity(structureCoefficients: Seq[Matrix[Int]]): Boolean = {

            //            val partialStructureCoefficients: Seq[Matrix[Option[Int]]] = {
            //              structureCoefficients.map(_.mapEntries[Option[Int]](Some(_))) ++ Seq.fill(n - structureCoefficients.size)(new Matrix[Option[Int]](n, Seq.fill(fr.rank)(Seq.fill(n)(None))))
            //            }

            val n0 = structureCoefficients.size

            (for (
              (m, mi) <- structureCoefficients.iterator.zipWithIndex;
              yj <- 0 until rank;
              if (m.entries(yj).drop(n0).forall(_ == 0))
            ) yield {
              val `x(ym)` = (for (
                (a, mk) <- m.entries(yj).take(n0).zipWithIndex
              ) yield {
                integerMatrices.scalarMultiply(a, structureCoefficients(mk))
              }).reduce(integerMatrices.add _)

              val `(xy)m` = integerMatrices.compose(fr.structureCoefficients(yj), m)

              `x(ym)` == `(xy)m`
            }).forall(_ == true)
          }

          for (
            s <- i;
            pM <- permutations.map(permuteColumns _).distinct;
            if (pM.entries(0)(s.size) == 1);
            ns = s :+ pM;
            if verifyPartialAssociativity(ns)
          ) yield {
            ns
          }
        }).toList.distinct // these .distinct's are probably performance killers
        for (pm <- permutedMatrices) yield moduleFromStructureCoefficients(pm)
      }).flatten.chooseEquivalenceClassRepresentatives(FusionModules.equivalent_? _)

      // TODO there are still duplicates; we need to consider some more permutations...

    }).flatten
  }

  def candidateBrauerPicardGroupoids: Seq[Groupoid] = {
    val admissibleModules: Seq[FusionModule] = ???
    val bimodules: Seq[(FusionModule, Seq[FusionBimodule])] = ???
    ???
  }

  trait FusionBimodule

}

trait Groupoid

object FusionRings {
  def withObject(m: Matrix[Int]): Iterable[FusionRing[Int]] = {
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

    val (solutions, tooHard) = IntegerPolynomialProgramming.solve(variableRing.associativityConstraints.flatten.toSeq)
    require(tooHard.isEmpty)

    for (solution <- solutions) yield {
      val structureCoefficients = variableStructureCoefficients.map(_.mapEntries({
        case p if p.totalDegree == Some(1) => solution(p.terms.head._1.keySet.iterator.next)
        case p => p.constantTerm
      }))
      FusionRing(structureCoefficients).ensuring(_.structureCoefficients(1) == m)
    }
  }
}

object FusionRing {
  def apply[A: Ring](multiplicities: Seq[Matrix[A]]): FusionRing[A] = {
    val result = new StructureCoefficientFusionRing(multiplicities)
    require({
      val sc = result.structureCoefficients
      sc == multiplicities
    })
    result
  }
  def apply(multiplicities: Seq[Matrix[Int]], fieldGenerator: Polynomial[Int], fieldGeneratorApproximation: Double, fieldGeneratorEpsilon: Double, dimensions: Seq[Polynomial[Fraction[Int]]]): FusionRingWithDimensions = new StructureCoefficientFusionRingWithDimensions(multiplicities, fieldGenerator, fieldGeneratorApproximation, fieldGeneratorEpsilon, dimensions).ensuring(_.structureCoefficients == multiplicities)

  private class StructureCoefficientFusionRing[A: Ring](multiplicities: Seq[Matrix[A]]) extends FusionRing[A] {
    override lazy val coefficients = implicitly[Ring[A]]
    override lazy val rank = multiplicities.size

    override def multiply(x: Seq[A], y: Seq[A]) = {
      val zero = coefficients.zero
      val terms = for (
        (xi, i) <- x.zipWithIndex;
        if (xi != zero);
        (yj, j) <- y.zipWithIndex;
        if (yj != zero)
      ) yield {
        for (k <- 0 until rank) yield {
          coefficients.multiply(xi, yj, multiplicities(j).entries(i)(k))
        }
      }
      val result = add(terms)
      result
    }
  }

  private class StructureCoefficientFusionRingWithDimensions(multiplicities: Seq[Matrix[Int]], fieldGenerator: Polynomial[Int], fieldGeneratorApproximation: Double, fieldGeneratorEpsilon: Double, override val dimensions: Seq[Polynomial[Fraction[Int]]]) extends StructureCoefficientFusionRing[Int](multiplicities)(Gadgets.Integers) with FusionRingWithDimensions {
    override def dimensionField = {
      RealNumberField(fieldGenerator, fieldGeneratorApproximation, fieldGeneratorEpsilon)(Gadgets.Integers, Gadgets.Doubles)
    }
  }
}

trait FusionBimodule[A] extends FiniteDimensionalFreeModule[A] {
  def coefficients = leftRing.coefficients.ensuring(_ == rightRing.coefficients)

  override def rank = leftRing.rank

  val leftRing: FusionRing[A]
  val rightRing: FusionRing[A]
  def leftModule: leftRing.FusionModule
  def rightModule: rightRing.FusionModule

  def associativityConstraints = {
    leftRing.associativityConstraints ++
      rightRing.associativityConstraints ++
      leftModule.associativityConstraints ++
      rightModule.associativityConstraints ++
      (for (x <- leftRing.basis.iterator; y <- leftModule.basis; z <- rightRing.basis) yield {
        subtract(rightModule.act(z, leftModule.act(x, y)), leftModule.act(x, rightModule.act(z, y)))
      })
  }
  def identityConstraints = leftRing.identityConstraints ++ rightRing.identityConstraints ++ leftModule.identityConstraints ++ rightModule.identityConstraints
}

object FusionBimodule {
  def apply[A: Ring](leftMultiplication: Seq[Matrix[A]], leftAction: Seq[Matrix[A]], rightMultiplication: Seq[Matrix[A]], rightAction: Seq[Matrix[A]]): FusionBimodule[A] = new StructureCoefficientFusionBimodule(leftMultiplication, leftAction, rightMultiplication, rightAction)

  private class StructureCoefficientFusionBimodule[A: Ring](leftMultiplication: Seq[Matrix[A]], leftAction: Seq[Matrix[A]], rightMultiplication: Seq[Matrix[A]], rightAction: Seq[Matrix[A]]) extends FusionBimodule[A] {

    override val coefficients = implicitly[Ring[A]]
    override val leftRing = FusionRing(leftMultiplication)
    override val rightRing = FusionRing(rightMultiplication)

    override val leftModule = leftRing.moduleFromStructureCoefficients(leftAction)
    override val rightModule = rightRing.moduleFromStructureCoefficients(rightAction)
  }

}

object FusionBimodules {
  def commutants(leftModule: FusionRing[Int]#FusionModule, otherRank: Int, otherNumberOfSelfDualObjects: Int): Iterable[FusionBimodule[Int]] = {
    // first, make a FusionBimodule with variable entries

    val leftRing = leftModule.fusionRing
    type V = (Int, Int, Int, Int)

    implicit val polynomialAlgebra = MultivariablePolynomialAlgebras.over[Int, V]

    val fusionRingUnknowns = for (i <- 0 until otherRank) yield {
      Matrix(otherRank,
        for (j <- 0 until otherRank) yield {
          for (k <- 0 until otherRank) yield polynomialAlgebra.monomial((0, i, j, k))
        })
    }
    val fusionModuleUnknowns = for (i <- 0 until leftModule.rank) yield {
      Matrix(leftModule.rank,
        for (j <- 0 until otherRank) yield {
          for (k <- 0 until leftModule.rank) yield polynomialAlgebra.monomial((1, i, j, k))
        })

    }

    val leftRingStructureCoefficients = leftRing.structureCoefficients.map(_.mapEntries(x => polynomialAlgebra.constant(x)))
    val leftModuleStructureCoefficients = leftModule.structureCoefficients.map(_.mapEntries(x => polynomialAlgebra.constant(x)))

    val variableBimodule = FusionBimodule(leftRingStructureCoefficients, leftModuleStructureCoefficients, fusionRingUnknowns, fusionModuleUnknowns)

    val otherDuality = (0 until otherNumberOfSelfDualObjects) ++ (for (i <- otherNumberOfSelfDualObjects until otherRank by 2; j <- Seq(i + 1, i)) yield j)

    val (solutions, tooHard) = IntegerPolynomialProgramming.solve(
      (variableBimodule.associativityConstraints.flatten ++ variableBimodule.identityConstraints.flatten ++ variableBimodule.rightRing.dualityConstraints(otherDuality).flatten).toSeq)
    require(tooHard.isEmpty)

    for (solution <- solutions) yield {
      val rightRingStructureCoefficients = fusionRingUnknowns.map(_.mapEntries({
        case p if p.totalDegree == Some(1) => solution(p.terms.head._1.keySet.iterator.next)
        case p => p.constantTerm
      }))
      val rightModuleStructureCoefficients = fusionModuleUnknowns.map(_.mapEntries({
        case p if p.totalDegree == Some(1) => solution(p.terms.head._1.keySet.iterator.next)
        case p => p.constantTerm
      }))
      FusionBimodule(leftRing.structureCoefficients, leftModule.structureCoefficients, rightRingStructureCoefficients, rightRingStructureCoefficients)
    }
  }

}

object Goals extends App {
  val haagerup4FusionRing: FusionRingWithDimensions = {
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
  val AH1FusionRing: FusionRingWithDimensions = {
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

  println(haagerup4FusionRing.candidateFusionModules.size)

    
    for (fm <- haagerup4FusionRing.candidateFusionModules; b <- FusionBimodules.commutants(fm, 4, 4)) {
      println(b.rightRing.structureCoefficients)
    }

  //    for (r <- FusionRings.withObject(AH1FusionRing.structureCoefficients(1)); m <- r.structureCoefficients) { println(m); println() }

  def test(G: FusionRingWithDimensions) {
    println("Start: " + new java.util.Date())
    println("dimension bounds: " + G.basis.map(G.dimensionBounds))
    println(G.candidateAlgebraObjects.toList)
    for (m <- G.candidateFusionMatrices) {
      println(m.algebraObject)
      println(m.matrix)
      println(m.dimensionsSquared)
      println(m.dimensionsSquared.map(G.dimensionField.approximateWithin(0.001)))
    }
    println("Finish: " + new java.util.Date())
    //    println(G.candidateFusionModules.size)
    var count = 0
    for (fm <- G.candidateFusionModules) {
      println(fm.structureCoefficients)
      count = count + 1
      println("found " + count + " modules so far")
    }
  }
  //  test(haagerup4FusionRing)
  //  test(AH1FusionRing)

  //  val v = Seq(1, 1, 2, 3, 3, 4)
  ////  val v = Seq(1,2,3,4,5,7)
  //  println(AH1FusionRing.objectsSmallEnoughToBeAlgebras.contains(v))
  //  println(AH1FusionRing.smallObjectsWithPositiveSemidefiniteMultiplication.contains(v))
  //  println(AH1FusionRing.regularModule.asMatrix(v));
  //  {
  //    import Implicits.Rationals
  //    println(AH1FusionRing.regularModule.asMatrix(v).mapEntries(Implicits.integersAsRationals).positiveSemidefinite_?)
  //  }
  //  println(Matrices.positiveSymmetricDecompositions(AH1FusionRing.regularModule.asMatrix(v)).toList)
  //  haagerupFusionRing.candidateBrauerPicardGroupoids
}