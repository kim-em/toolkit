package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.algebra.numberfields.RealNumberField
import net.tqft.toolkit.permutations.Permutation
import net.tqft.toolkit.permutations.Permutations
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.modules._

trait FiniteDimensionalFreeModuleOverRig[A] extends ModuleOverRig[A, Seq[A]] {
  def coefficients: Rig[A]
  def rank: Int
  override lazy val zero = Seq.fill(rank)(coefficients.zero)
  override def add(x: Seq[A], y: Seq[A]) = x.zip(y).map(p => coefficients.add(p._1, p._2))
  override def scalarMultiply(a: A, b: Seq[A]) = b.map(x => coefficients.multiply(a, x))

  def innerProduct(x: Seq[A], y: Seq[A]) = coefficients.sum(x.zip(y).map(p => coefficients.multiply(p._1, p._2)))

  def basis = for (i <- 0 until rank) yield for (j <- 0 until rank) yield if (i == j) coefficients.one else coefficients.zero
}

trait FiniteDimensionalFreeModule[A] extends FiniteDimensionalFreeModuleOverRig[A] with Module[A, Seq[A]] {
  override def coefficients: Ring[A]
  override def negate(x: Seq[A]) = x.map(coefficients.negate)
}

// Usually A = Int, for a concrete fusion ring. We allow other possibilities so we can write fusion solvers, etc.
trait FusionRing[A] extends FiniteDimensionalFreeModuleOverRig[A] with Rig[Seq[A]] { fr =>

  override def fromInt(x: Int) = coefficients.fromInt(x) +: Seq.fill(rank - 1)(coefficients.zero)
  override val one = fromInt(1)

  def associativityConstraints: Iterator[(A, A)] = (for (x <- basis.iterator; y <- basis; z <- basis) yield multiply(x, multiply(y, z)).zip(multiply(multiply(x, y), z))).flatten
  def identityConstraints: Iterator[(A, A)] = (for (x <- basis.iterator) yield Seq(x.zip(multiply(one, x)), x.zip(multiply(x, one)))).flatten.flatten
  def dualityConstraints(duality: Permutation = duality): Iterator[(A, A)] = {
    require(duality.size == rank)
    import net.tqft.toolkit.permutations.Permutations._
    (for (x <- basis.iterator; y <- basis) yield {
      duality.permute(multiply(x, y)).zip(multiply(duality.permute(y), duality.permute(x)))
    }).flatten ++
      (for (x <- basis.iterator; y <- basis; z <- basis) yield {
        Seq(
          (innerProduct(multiply(x, y), z), innerProduct(x, multiply(z, duality.permute(y)))),
          (innerProduct(multiply(x, y), z), innerProduct(y, multiply(duality.permute(x), z))))
      }).flatten
  }
  def duality: Permutation = {
    structureCoefficients.map(_.entries.indexWhere(_.head == coefficients.one)).toIndexedSeq.ensuring({ _.forall(_ != -1) })
  }

  def verifyAssociativity = associativityConstraints.forall(p => p._1 == p._2)
  def verifyIdentity = identityConstraints.forall(p => p._1 == p._2)
  def verifyDuality(duality: Permutation = duality) = dualityConstraints(duality).forall(p => p._1 == p._2)

  def structureCoefficients: Seq[Matrix[A]] = ??? // for (y <- basis) yield Matrix(rank, for (x <- basis) yield multiply(x, y))

  def dimensionLowerBounds(x: Seq[Int])(implicit ev: A =:= Int): Double = {
    regularModule.dimensionLowerBounds(x)
  }

  def globalDimensionLowerBound(implicit ev: A =:= Int): Double = {
    regularModule.globalDimensionLowerBound
  }

  trait FusionModule extends FiniteDimensionalFreeModuleOverRig[A] { fm =>
    override def coefficients = fr.coefficients

    def fusionRing = fr

    def act(x: Seq[A], m: Seq[A]): Seq[A]
    def rightMultiplicationByDuals(m: Seq[A], n: Seq[A]): Seq[A] = for (i <- 0 until fr.rank) yield {
      innerProduct(m, act(fr.basis(i), n))
    }
    def algebraObject: Seq[A] = rightMultiplicationByDuals(basis.head, basis.head)
    val objectsAtDepth: Int => Seq[Int] = {
      def impl(k: Int): Seq[Int] = {
        k match {
          case 0 => Seq(0)
          case 1 => Seq(0)
          case k if k % 2 == 0 => {
            ((for (i <- objectsAtDepth(k - 1)) yield {
              for (
                (x, j) <- rightMultiplicationByDuals(basis(i), basis.head).zipWithIndex;
                if (x != coefficients.zero)
              ) yield j
            }).flatten.toSet -- objectsAtDepth(k - 2)).toSeq.sorted
          }
          case k if k % 2 == 1 => {
            ((for (i <- objectsAtDepth(k - 1)) yield {
              for (
                (x, j) <- act(fr.basis(i), basis.head).zipWithIndex;
                if (x != coefficients.zero)
              ) yield j
            }).flatten.toSet -- objectsAtDepth(k - 2)).toSeq.sorted
          }
        }
      }
      import net.tqft.toolkit.functions.Memo
      Memo(impl _)
    }
    def depthOfRingObject(k: Int): Int = {
      (0 to (2 * (fusionRing.rank - 1)) by 2).iterator.find(i => objectsAtDepth(i).contains(k)).get
    }
    def depthOfModuleObject(k: Int): Int = {
      (1 to (2 * rank - 1) by 2).iterator.find(i => objectsAtDepth(i).contains(k)).get
    }
    lazy val depth = {
      (for (d <- 0 to 2 * rank; if objectsAtDepth(d).nonEmpty) yield d).max
    }

    def dimensionLowerBounds(x: Seq[Int])(implicit ev: A =:= Int): Double = {
      if (x.forall(_ == 0)) {
        0
      } else {
        val matrices = Matrices.over[Int]
        val A = x.zip(structureCoefficients).map(p => matrices.scalarMultiply(p._1, p._2.mapEntries(xi => xi: Int))).reduce(matrices.add)
        require(A.entries.flatten.forall(_ >= 0))
        require(A.entries.flatten.exists(_ > 0))
        val AAt = matrices.compose(A, A.transpose)
        val estimate = FrobeniusPerronEigenvalues.estimate(AAt)
        require(estimate > 0.999)
        val result = scala.math.sqrt(estimate - 0.0001)
        result
      }
    }

    def globalDimensionLowerBound(implicit ev: A =:= Int): Double = {
      (for (x <- basis; d = dimensionLowerBounds(x.map(xi => xi: Int))) yield d * d).sum
    }

    def associativityConstraints = (for (x <- fr.basis.iterator; y <- fr.basis; z <- basis) yield act(x, act(y, z)).zip(act(fr.multiply(x, y), z))).flatten
    def admissibilityConstraints = for (m <- basis.iterator; x <- fr.basis; h <- fr.basis) yield {
      (innerProduct(act(x, m), act(h, m)), fr.innerProduct(fr.multiply(x, rightMultiplicationByDuals(m, m)), h))
    }
    def identityConstraints = (for (x <- basis.iterator) yield x.zip(act(fr.one, x))).flatten
    def dualityConstraints(duality: Permutation = fusionRing.duality) = {
      import net.tqft.toolkit.permutations.Permutations._
      for (x <- fr.basis.iterator; m <- basis; n <- basis) yield {
        (innerProduct(act(x, m), n), innerProduct(m, act(duality.permute(x), n)))
      }
    }

    def verifyAssociativity = associativityConstraints.map(p => p._1 == p._2).reduce(_ && _)
    def verifyAdmissibility = admissibilityConstraints.map(p => p._1 == p._2).reduce(_ && _)

    //    def asMatrix(x: Seq[A]) = new Matrix(rank, for (b <- basis) yield act(x, b))

    def asMatrix(x: Seq[A]) = {
      Matrix(rank, for (i <- 0 until fr.rank) yield for (j <- 0 until rank) yield {
        coefficients.sum(for ((xi, mi) <- x.zip(structureCoefficients)) yield coefficients.multiply(xi, mi.entries(i)(j)))
      })
    }

    def structureCoefficients: Seq[Matrix[A]] = for (y <- basis) yield Matrix(rank, for (x <- fr.basis) yield act(x, y))

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

        Permutations.of(m1.rank).exists(p => s2 == p.permute(s1.map(m => m.permuteColumns(p))))
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
      val result = sum(terms)
      require(m.size == result.size)
      result
    }
    override val structureCoefficients = matrices
  }

  def moduleFromStructureCoefficients(matrices: Seq[Matrix[A]]): FusionModule = {
    (new StructureCoefficientFusionModule(matrices)) //.ensuring(_.structureCoefficients == matrices)
  }

  trait RegularModule extends FusionModule {
    override val rank = fr.rank
    override def act(x: Seq[A], m: Seq[A]) = fr.multiply(x, m)
    override def structureCoefficients = fr.structureCoefficients
  }

  lazy val regularModule: RegularModule = new RegularModule {}
}

object FusionRing {
  def apply[A: Rig](multiplicities: Seq[Matrix[A]]): FusionRing[A] = {
    val result = new StructureCoefficientFusionRing(multiplicities)
    result
  }
  def apply(multiplicities: Seq[Matrix[Int]], fieldGenerator: Polynomial[Int], fieldGeneratorApproximation: Double, fieldGeneratorEpsilon: Double, dimensions: Seq[Polynomial[Fraction[Int]]]): FusionRingWithDimensions = new StructureCoefficientFusionRingWithDimensions(multiplicities, fieldGenerator, fieldGeneratorApproximation, fieldGeneratorEpsilon, dimensions).ensuring(_.structureCoefficients == multiplicities)

  private class StructureCoefficientFusionRing[A: Rig](multiplicities: Seq[Matrix[A]]) extends FusionRing[A] {
    override lazy val coefficients = implicitly[Rig[A]]
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
      val result = sum(terms)
      result
    }

    override val structureCoefficients = multiplicities
  }

  private class StructureCoefficientFusionRingWithDimensions(multiplicities: Seq[Matrix[Int]], fieldGenerator: Polynomial[Int], fieldGeneratorApproximation: Double, fieldGeneratorEpsilon: Double, override val dimensions: Seq[Polynomial[Fraction[Int]]]) extends StructureCoefficientFusionRing[Int](multiplicities) with FusionRingWithDimensions {
    override def dimensionField = {
      RealNumberField(fieldGenerator, fieldGeneratorApproximation, fieldGeneratorEpsilon)
    }
  }
}

