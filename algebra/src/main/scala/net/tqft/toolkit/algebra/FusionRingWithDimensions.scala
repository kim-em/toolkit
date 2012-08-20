package net.tqft.toolkit.algebra

trait FusionRingWithDimensions extends ConcreteFusionRing { fr =>
  def dimensionField: RealNumberField[Int, Double]
  def dimensions: Seq[Polynomial[Fraction[Int]]]
  def dimensionOf(x: Seq[Int]): Polynomial[Fraction[Int]] = {
    val polynomials = Polynomials.over(Gadgets.Rationals)
    import Implicits.integersAsRationals
    polynomials.add(x.zip(dimensions).map(p => polynomials.scalarMultiply(p._1, p._2)))
  }

  def dimensionLowerBounds(x: Seq[Int]): Double = {
    FrobeniusPerronEigenvalues.estimate(regularModule.asMatrix(x)) - 0.0001
  }

  def dimensionUpperBounds(x: Seq[Int]): Double = {
    dimensionField.approximateWithin(0.0001)(dimensionOf(x)) + 0.0001
  }

  def objectsSmallEnoughToBeAlgebras: Seq[Seq[Int]] = {
    val start = Seq(Seq(1))
    basis.tail.foldLeft(start)({
      (i: Seq[Seq[Int]], b: Seq[Int]) =>
        i.flatMap({
          a: Seq[Int] => for (m <- 0 to dimensionUpperBounds(b).floor.intValue) yield a :+ m
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

  trait FusionModule extends super.FusionModule {
    override val fusionRing: fr.type = fr

    def dimensionUpperBounds(x: Seq[Int]): Double = {
      val A = asMatrix(x)
      val AAt = Matrices.over[Int].compose(A, A.transpose)
      scala.math.sqrt(fr.dimensionUpperBounds(AAt.entries.head))
    }
  }

  override def moduleFromStructureCoefficients(matrices: Seq[Matrix[Int]]): FusionModule = {
    (new StructureCoefficientFusionModule(matrices)) // .ensuring(_.structureCoefficients == matrices)
  }

  protected class StructureCoefficientFusionModule(matrices: Seq[Matrix[Int]]) extends super.StructureCoefficientFusionModule(matrices) with FusionModule

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
        import net.tqft.toolkit.permutations.Permutation
        import net.tqft.toolkit.permutations.Permutations
        import net.tqft.toolkit.permutations.Permutations._
        val permutations = Permutations.preserving(t.head.dimensionsSquared).toSeq.par
        val permutedMatrices = t.foldLeft(Iterable(Seq.empty[Matrix[Int]]))({ (i: Iterable[Seq[Matrix[Int]]], M: FusionMatrix) =>
          def permuteColumns(p: Permutation) = new Matrix(M.matrix.numberOfColumns, p.permute(M.matrix.entries.seq.transpose).transpose)
          def verifyPartialAssociativity(structureCoefficients: Seq[Matrix[Int]]): Boolean = {

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

    }).flatten
  }

  def candidateBrauerPicardGroupoids: Seq[Groupoid[FusionRing[Int], FusionBimodule[Int]]] = {
    val admissibleModules: Seq[FusionModule] = ???
    val bimodules: Seq[(FusionModule, Seq[FusionBimodule[Int]])] = ???
    ???
  }
}
