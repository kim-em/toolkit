package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.permutations.Permutations.Permutation
import net.tqft.toolkit.algebra.polynomials.Polynomials
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.algebra.numberfields.RealNumberField
import net.tqft.toolkit.algebra.polynomials.PolynomialAlgebra
import net.tqft.toolkit.algebra.categories.Groupoid
import scala.collection.parallel.ParSeq
import scala.collection.GenSeq
import net.tqft.toolkit.SHA1

trait DimensionFunction {
  def dimensionField: RealNumberField[Int, Double]
  def dimensions: Seq[Polynomial[Fraction[Int]]]
  def dimensionOf(x: Seq[Int]): Polynomial[Fraction[Int]] = {
    val polynomials = Polynomials.over(Rationals)
    polynomials.sum(x.zip(dimensions).map(p => polynomials.scalarMultiply(p._1, p._2)))
  }
  def globalDimension: Polynomial[Fraction[Int]] = {
    dimensionField.sum(for (d <- dimensions) yield dimensionField.power(d, 2))
  }
  def globalDimensionUpperBound = dimensionField.approximateWithin(0.0001)(globalDimension) + 0.0001

}

trait FusionRingWithDimensions extends FusionRing[Int] with DimensionFunction { fr =>

  trait RegularModule extends super.RegularModule with FusionModule

  override lazy val regularModule: RegularModule = new RegularModule {}

  def dimensionUpperBounds(x: Seq[Int]): Double = {
    dimensionField.approximateWithin(0.0001)(dimensionOf(x)) + 0.0001
  }

  def objectsSmallEnoughToBeAlgebras: ParSeq[Seq[Int]] = {
    val start = Seq(Seq(1)).par
    basis.tail.foldLeft(start)({
      (i: ParSeq[Seq[Int]], b: Seq[Int]) =>
        i.flatMap({
          a: Seq[Int] => for (m <- 0 to dimensionUpperBounds(b).floor.intValue) yield a :+ m
        })
    })
  }

  def smallObjectsWithPositiveSemidefiniteMultiplication: ParSeq[Seq[Int]] = {
    for (
      o <- objectsSmallEnoughToBeAlgebras;
      m = regularModule.asMatrix(o);
      if (m.mapEntries(Conversions.integersAsDoubles).positiveSemidefinite_?)
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
            val polynomials = PolynomialAlgebra.over[Fraction[Int]]
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
      //        o <- objectsSmallEnoughToBeAlgebras.par;
      a = regularModule.asMatrix(o);
      m <- Matrices.positiveSymmetricDecompositionsCached(a)
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

  lazy val hashString = SHA1(structureCoefficients.toString)

  def candidateFusionModulesCached = {
    val bucket = net.tqft.toolkit.amazon.S3("fusion-modules")

    def writeMatrix(m: Matrix[Int]): String = {
      m.entries.map(_.mkString("x")).mkString("p")
    }
    def readMatrix(m: String): Matrix[Int] = {
      import net.tqft.toolkit.Extractors.Int
      m.split("p").toSeq.map(_.split("x").toSeq.collect({ case Int(n) => n }))
    }

    def writeModule(m: FusionModule): String = m.structureCoefficients.map(writeMatrix).mkString("\n")
    def readModule(m: String): FusionModule = moduleFromStructureCoefficients(m.split("\n").toSeq.filter(_.nonEmpty).map(readMatrix))

    def writeModules(ms: Seq[FusionModule]): String = {
      "Fusion modules for " + hashString + "\n" +
        (for ((m, i) <- ms.zipWithIndex) yield {
          "Fusion module " + (i + 1) + "/" + ms.size + "\n" + writeModule(m)
        }).mkString("\n")
    }
    def readModules(ms: String): Seq[FusionModule] = {
      import net.tqft.toolkit.collections.Split._
      ms.split("\n").toSeq.tail.splitOn(_.startsWith("Fusion module ")).filter(_.nonEmpty).map(lines => readModule(lines.mkString("\n"))).toSeq
    }

    import net.tqft.toolkit.collections.MapTransformer._
    val transformedBucket = bucket.transformValues(readModules _, writeModules _)

    transformedBucket.getOrElseUpdate(hashString, candidateFusionModules.toSeq)
  }

  def candidateFusionModules: Iterator[FusionModule] = {
    val matricesBySize = candidateFusionMatrices.toList.groupBy(_.matrix.numberOfColumns)
    (for (n <- (1 to matricesBySize.keys.max).reverse.iterator) yield {

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

      val integerMatrices = new MatrixCategoryOverRing[Int]

      import net.tqft.toolkit.collections.GroupBy._

      println("processing " + n_tuples.size + " " + n + "-tuples:")
      (for (t <- n_tuples) yield {
        println(t)
        import net.tqft.toolkit.permutations.Permutations
        import net.tqft.toolkit.permutations.Permutations._
        val permutations = Permutations.preserving(t.head.dimensionsSquared).toSeq.par

        println(permutations.size + " possible permutations to consider, for each element of the tuple")
        println("there are " + Permutations.preserving(t).size + " symmetries of the tuple")
        
        val permutedMatrices = t.foldLeft(Iterator(Seq.empty[Matrix[Int]]))({
          case (i: Iterator[Seq[Matrix[Int]]], m0: FusionMatrix) =>
            def permuteColumns(p: Permutation) = new Matrix(m0.matrix.numberOfColumns, p.permute(m0.matrix.entries.seq.transpose).transpose)
            def verifyPartialAssociativity(structureCoefficients: Seq[Matrix[Int]]): Boolean = {

              val n0 = structureCoefficients.size

              (for (
                (m, mi) <- structureCoefficients.zipWithIndex.iterator;
                yj <- 0 until rank;
                if (m.entries(yj).drop(n0).forall(_ == 0)); // otherwise ym will include stuff we don't know yet
                if mi == n0 - 1 || m.entries(yj)(n0 - 1) != 0 // avoid duplicating work?
              ) yield {
                val `x(ym)` = integerMatrices.sum(for (
                  (a, mk) <- m.entries(yj).take(n0).zipWithIndex
                ) yield {
                  integerMatrices.scalarMultiply(a, structureCoefficients(mk))
                })

                val `(xy)m` = integerMatrices.compose(fr.structureCoefficients(yj), m)

                `x(ym)` == `(xy)m`
              }).forall(_ == true)
            }

            for (
              s <- i;
              pm0 <- permutations.map(permuteColumns _).distinct;
              if (pm0.entries(0)(s.size) == 1);
              ns = s :+ pm0;
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
