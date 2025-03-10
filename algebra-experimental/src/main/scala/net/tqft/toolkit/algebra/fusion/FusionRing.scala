package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.algebra.RealNumberField
import net.tqft.toolkit.permutations.Permutations.Permutation
import net.tqft.toolkit.permutations.Permutations
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.graphs.Graph
import net.tqft.toolkit.algebra.graphs.ColouredGraph
import scala.collection.mutable.WrappedArray
import scala.reflect.ClassTag

trait FiniteDimensionalFreeModuleOverRig[A] extends Rig.RigSeq[A] {
  def rank: Int
  override lazy val zero = Seq.fill(rank)(coefficients.zero)

  def innerProduct(x: Seq[A], y: Seq[A]) = coefficients.sum(x.zip(y).map(p => coefficients.multiply(p._1, p._2)))

  lazy val basis = for (i <- 0 until rank) yield for (j <- 0 until rank) yield if (i == j) coefficients.one else coefficients.zero
}


abstract class FiniteDimensionalVectorSpace[A: Field] extends Ring.RingSeq[A] with FiniteDimensionalFreeModuleOverRig[A]  {
  override def coefficients: Field[A] = implicitly[Field[A]]
}

object FiniteDimensionalVectorSpace {
  def ofDimension[F:Field](dimension: Int) = {
    new FiniteDimensionalVectorSpace[F] with VectorSpace[F, Seq[F]] {
      override def coefficients = implicitly[Field[F]]
      override def rank = dimension
    }
  }
}


// Usually A = Int, for a concrete fusion ring. We allow other possibilities so we can write fusion solvers, etc.
trait FusionRing[A] extends FiniteDimensionalFreeModuleOverRig[A] with Rig[Seq[A]] { fr =>
  override implicit def coefficients: Rig[A]
  
  override lazy val hashCode = structureCoefficients.hashCode
  override def equals(other: Any) = {
    other match {
      case other: FusionRing[A] => structureCoefficients == other.structureCoefficients
      case _ => false
    }
  }
  override def toString = "FusionRing(Seq(\n" + structureCoefficients.mkString(",\n") + "\n))"

  override def fromInt(x: Int) = coefficients.fromInt(x) +: Seq.fill(rank - 1)(coefficients.zero)
  override lazy val one = fromInt(1)

  def multiplyBasisElements(x: Int, y: Int): Seq[A] = {
    structureCoefficients(x).entries(y)
  }
  def multiplyByBasisElement(x: Seq[A], j: Int): Seq[A]
  def multiplyByBasisElementOnLeft(j: Int, x: Seq[A]): Seq[A]

  def associativityConstraints: Iterator[(A, A)] = (for (x <- basis.iterator; y <- basis; z <- basis) yield multiply(x, multiply(y, z)).zip(multiply(multiply(x, y), z))).flatten
  def partialAssociativityConstraints(maxdepth: Int, depths: Seq[Int]) = {
    val p = depths.zipWithIndex.tail /* no need for identities */
    (for ((dx, xi) <- p.iterator; (dy, yi) <- p; (dz, zi) <- p; if dx + dy + dz >= maxdepth) yield {
      val pairs = multiplyByBasisElement(multiplyBasisElements(xi, yi), zi).zip(multiplyByBasisElementOnLeft(xi, multiplyBasisElements(yi, zi)))
      if (dx + dy < maxdepth && dy + dz < maxdepth) {
        pairs
      } else {
        for ((q, dw) <- pairs.zip(depths); if dw < maxdepth - dx && dw < maxdepth - dz) yield {
          q
        }
      }
    }).flatten
  }
  def identityConstraints: Iterator[(A, A)] = (for (x <- basis.iterator) yield Seq(x.zip(multiply(one, x)), x.zip(multiply(x, one)))).flatten.flatten
  def dualityConstraints(d: IndexedSeq[Int] = duality): Iterator[(A, A)] = {
    require(d.size == rank)
    import net.tqft.toolkit.permutations.Permutations._
    (for (xi <- (0 until rank).iterator; yi <- 0 until rank) yield {
      d.permute(multiplyBasisElements(xi, yi)).zip(multiplyBasisElements(d(yi), d(xi)))
    }).flatten ++
      (for (xi <- (0 until rank).iterator; yi <- 0 until rank; zi <- 0 until rank) yield {
        (multiplyBasisElements(xi, yi)(zi), multiplyBasisElements(zi, d(yi))(xi))
      })
  }

  // TODO this could be considerably optimized
  def generalDualityConstraints: Iterator[(A, A)] = {
    (for (x <- basis.iterator; y <- basis) yield (multiply(x, y).head, multiply(y, x).head)) ++
      (for (x <- basis.iterator) yield (coefficients.sum(for (y <- basis) yield multiply(x, y).head), coefficients.one)) ++
      (for (y <- basis.iterator; w <- basis; yw = multiply(y, w).head; if yw != coefficients.zero; x <- basis; z <- basis) yield {
        (
          coefficients.multiply(yw, innerProduct(multiply(x, y), z)),
          coefficients.multiply(yw, innerProduct(x, multiply(z, w))))
      })
  }

  def verifyAssociativity = associativityConstraints.forall(p => p._1 == p._2)
  def verifyIdentity = identityConstraints.forall(p => p._1 == p._2)
  def verifyDuality(duality: IndexedSeq[Int] = duality) = dualityConstraints(duality).forall(p => p._1 == p._2)

  lazy val duality: IndexedSeq[Int] = {
    structureCoefficients.map(_.entries.indexWhere(_.head == coefficients.one)).toIndexedSeq.ensuring(!_.contains(-1))
  }

  def structureCoefficients: Seq[Matrix[A]] = ??? // for (y <- basis) yield Matrix(rank, for (x <- basis) yield multiply(x, y))

  // not actually useful!
  //  def regularObjectStructureCoefficients(duality: IndexedSeq[Int] = duality): Matrix[A] = {
  //    val matrices = Matrices.over[A](coefficients)
  //    matrices.sum(
  //      for (i <- 0 until rank; v = structureCoefficients(i); vd = structureCoefficients(duality(i))) yield {
  //        matrices.compose(v, vd)
  //      })
  //  }

  def dimensionLowerBounds(x: Seq[Int])(implicit ev: A =:= Int): Double = {
    regularModule.dimensionLowerBounds(x)
  }

  private var globalDimensionLowerBoundCached: Option[Double] = None
  
  def globalDimensionLowerBound(implicit ev: A =:= Int): Double = {
    while(globalDimensionLowerBoundCached.isEmpty) {
      globalDimensionLowerBoundCached = Some(regularModule.globalDimensionLowerBound)
    }
    
    globalDimensionLowerBoundCached.get    
  }

  def graphEncoding(colouring: Seq[Int]): ColouredGraph[String] = {
    def labels(t: String) = for (i <- colouring) yield t + i
    val vertices = (labels("A") ++ labels("B") ++ labels("C") ++ structureCoefficients.map(_.entries.seq).flatten.flatten.map("M" + _)).toIndexedSeq
    //    val edges = (for (a <- 0 until rank; b <- 0 until rank; c <- 0 until rank) yield {
    //      Set(Set(a, 3 * rank + a * rank * rank + b * rank + c), Set(b + rank, 3 * rank + a * rank * rank + b * rank + c), Set(c + 2 * rank, 3 * rank + a * rank * rank + b * rank + c))
    //    }).flatten ++ (for (i <- 0 until rank) yield {
    //      Set(Set(i, rank + i), Set(i, 2 * rank + i), Set(rank + i, 2 * rank + i))
    //    }).flatten

    val adjacencies = (for (i <- 0 until rank) yield {
      IndexedSeq(i + rank, i + 2 * rank) ++ (for (j <- 0 until rank; k <- 0 until rank) yield (3 * rank + i * rank * rank + j * rank + k))
    }) ++ (for (j <- 0 until rank) yield {
      IndexedSeq(j, j + 2 * rank) ++ (for (i <- 0 until rank; k <- 0 until rank) yield (3 * rank + i * rank * rank + j * rank + k))
    }) ++ (for (k <- 0 until rank) yield {
      IndexedSeq(k, k + rank) ++ (for (i <- 0 until rank; j <- 0 until rank) yield (3 * rank + i * rank * rank + j * rank + k))
    }) ++ (for (i <- 0 until rank; j <- 0 until rank; k <- 0 until rank) yield IndexedSeq(i, j + rank, k + 2 * rank))

    ColouredGraph(3 * rank + rank * rank * rank, adjacencies, vertices)
  }

  def automorphisms(colouring: Seq[Int] = Seq.fill(rank)(0)) = {
    import net.tqft.toolkit.algebra.graphs.Dreadnaut
    import net.tqft.toolkit.algebra.grouptheory.FiniteGroups
    val graphAutomorphisms = Dreadnaut.automorphismGroup(graphEncoding(colouring))
    val generators = graphAutomorphisms.generators.map(_.take(rank))

    for (g <- generators) {
      require(this == relabel(g), "found an invalid automorphism of " + this + ": " + g)
    }

    FiniteGroups.symmetricGroup(rank).subgroupGeneratedBy(generators)
  }

  def relabel(labelling: IndexedSeq[Int]): FusionRing[A] = {
    require(labelling.size == rank)
    require(labelling.distinct.size == rank)
    require(labelling.max == rank - 1)

    import net.tqft.toolkit.permutations.Permutations._
    val inverse = labelling.inverse
    FusionRing(inverse.permute(structureCoefficients.map(m => m.permuteRows(inverse).permuteColumns(inverse))))(coefficients)
  }

  def canonicalRelabelling(colouring: Seq[Int] = Seq.fill(rank)(0)): FusionRing[A] = {
    import net.tqft.toolkit.algebra.graphs.Dreadnaut
    relabel(Dreadnaut.canonicalLabelling(graphEncoding(colouring)).take(rank))
  }

  def relabelForGenerators(elements: Set[Int]) = {
    val depths = depthWithRespectTo(elements)
    require(!depths.contains(-1))
    val p = ((0 +: elements.toSeq) ++ (1 until rank).filterNot(elements.contains).sortBy(depths)).toIndexedSeq
    import net.tqft.toolkit.permutations.Permutations._
    relabel(p.inverse)
  }

//  def generators = {
//    for (i <- 0 until rank; j = duality(i); g = add(basis(i), basis(j)); if !depthWithRespectTo(g).contains(-1)) yield i
//  }
  def generators_?(elements: Set[Int]) = {
    val withDuals = for (i <- elements; j <- Set(i, duality(i))) yield j
    depthWithRespectTo(withDuals).forall(_ != -1)
  }
  def independent_?(elements: Set[Int]) = {
    val clumps = (for (i <- elements) yield (Set(i, duality(i)))).toSet
    clumps.forall({ s =>
      val depths = depthWithRespectTo(elements.filterNot(s).toSet)
      s.forall(depths(_) == -1)
    })
  }
  def minimalGeneratingSets: Iterator[Set[Int]] = {
    // TODO this is a terrible implementation
    val clumps = (for (i <- (1 until rank)) yield (Set(i, duality(i)))).toSet
    clumps.subsets.map(_.flatten).filter(generators_?).filter(independent_?)
  }

  def depthWithRespectTo(xs: Set[Int]): Seq[Int] = {
//    val xs = x.zipWithIndex.collect({ case (a, i) if a != coefficients.zero => i })
    val depths = Array.fill(rank)(-1)
    depths(0) = 0
    var d = 0
    var latest = 0 :: Nil
    while(latest.nonEmpty) {
      d += 1
      latest = for(j <- latest; i <- xs; k <- 0 until rank; if structureCoefficients(i).entries(j)(k) != coefficients.zero; if depths(k) == -1) yield {
        depths(k) = d
        k
      }
    }
    
    // this is an old, slow implementation
//    val objectsAtDepth = for (k <- (0 until rank).toStream) yield power(x, k).zipWithIndex.collect({ case (a, i) if a != coefficients.zero => i })
//    val result = for (i <- 0 until rank) yield {
//      objectsAtDepth.indexWhere(_.contains(i))
//    }
//    
//    require(result == depths.toSeq, result + " != " + depths.toSeq)
    depths
  }

  trait FusionModule extends FiniteDimensionalFreeModuleOverRig[A] { fm =>
    override implicit def coefficients = fr.coefficients

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
//        println("computing FP eigenvalue for " + A)
        //        require(A.entries.flatten.forall(_ >= 0))
        //        require(A.entries.flatten.exists(_ > 0))
        val AAt = matrices.compose(A, A.transpose)
        val estimate = FrobeniusPerronEigenvalues.estimate(AAt.entries.map(_.toArray).toArray)
        //        require(estimate > 0.999)
        val result = scala.math.sqrt(estimate - 0.0001)
        result
      }
    }

    // FIXME this is wrong for anything but the regular module!
    def globalDimensionLowerBound(implicit ev: A =:= Int): Double = {

      val matrices = Matrices.over[Int]
      val S = structureCoefficients.asInstanceOf[Seq[Matrix[Int]]]

      (
        for (i <- 0 until rank; v = S(i); vd = S(duality(i))) yield {
          FrobeniusPerronEigenvalues.estimate(matrices.compose(v, vd).entries.map(_.toArray).toArray)
        }).sum - 0.0001

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

    def verifyAssociativity = associativityConstraints.forall(p => p._1 == p._2)
    def verifyAdmissibility = admissibilityConstraints.forall(p => p._1 == p._2)

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

  implicit val rig = coefficients
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
    (new StructureCoefficientFusionModule(matrices))
  }

  trait RegularModule extends FusionModule {
    override val rank = fr.rank
    override def act(x: Seq[A], m: Seq[A]) = fr.multiply(x, m)
    override def structureCoefficients = fr.structureCoefficients
  }

  lazy val regularModule: RegularModule = {
    new RegularModule {}
  }
}

object FusionRing {
  def apply[A: Rig](multiplicities: Seq[Matrix[A]]): FusionRing[A] = {
    new StructureCoefficientFusionRing(multiplicities)
  }
  def apply(multiplicities: Seq[Matrix[Int]]): FusionRing[Int] = {
    def opt(x: Seq[Int]) = {
      x match {
        case _: WrappedArray[_] => x
        case _ => x.toArray[Int]: Seq[Int]
      }
    }
    new IntegerStructureCoefficientFusionRing(multiplicities.toIndexedSeq.map({ m: Matrix[Int] =>
      Matrix(multiplicities.size, m.entries.map(opt).toArray[Seq[Int]]: Seq[Seq[Int]])
    }))
  }
  def apply(multiplicities: Seq[Matrix[Int]], fieldGenerator: Polynomial[Int], fieldGeneratorApproximation: Double, fieldGeneratorEpsilon: Double, dimensions: Seq[Polynomial[Fraction[Int]]]): FusionRingWithDimensions = new StructureCoefficientFusionRingWithDimensions(multiplicities, fieldGenerator, fieldGeneratorApproximation, fieldGeneratorEpsilon, dimensions).ensuring(_.structureCoefficients == multiplicities)

  private class StructureCoefficientFusionRing[A: Rig](multiplicities: Seq[Matrix[A]]) extends FusionRing[A] {
    override lazy val coefficients = implicitly[Rig[A]]
    override lazy val rank = multiplicities.size

    override def multiplyByBasisElement(x: Seq[A], j: Int) = {
      val result = scala.collection.mutable.IndexedSeq.fill(rank)(coefficients.zero)
      for ((xm, xi) <- x.zipWithIndex) {
        for (k <- 0 until rank) {
          result(k) = coefficients.add(result(k), coefficients.multiply(xm, structureCoefficients(xi).entries(j)(k)))
        }
      }
      result
    }
    override def multiplyByBasisElementOnLeft(j: Int, x: Seq[A]) = {
      val result = scala.collection.mutable.IndexedSeq.fill(rank)(coefficients.zero)
      for ((z, r) <- x.zip(structureCoefficients(j).entries)) {
        for (k <- 0 until rank) {
          result(k) = coefficients.add(result(k), coefficients.multiply(z, r(k)))
        }
      }
      result
    }

    override def multiply(x: Seq[A], y: Seq[A]) = {
      val zero = coefficients.zero
      val terms = for (
        (xi, i) <- x.zipWithIndex;
        if (xi != zero);
        (yj, j) <- y.zipWithIndex;
        if (yj != zero)
      ) yield {
        for (k <- 0 until rank) yield {
          coefficients.multiply(xi, yj, structureCoefficients(i).entries(j)(k))
        }
      }
      val result = sum(terms)
      result
    }

    override val structureCoefficients: IndexedSeq[Matrix[A]] = multiplicities.toArray[Matrix[A]]
  }
  private class IntegerStructureCoefficientFusionRing(override val structureCoefficients: IndexedSeq[Matrix[Int]]) extends FusionRing[Int] {
    override def coefficients = implicitly[Ring[Int]]
    override def rank = structureCoefficients.size

    override def multiplyByBasisElement(x: Seq[Int], j: Int) = {
      val result = Array.fill(rank)(0)
      for ((xm, xi) <- x.zipWithIndex) {
        for (k <- 0 until rank) {
          result(k) += xm * structureCoefficients(xi).entries(j)(k)
        }
      }
      result
    }
    override def multiplyByBasisElementOnLeft(j: Int, x: Seq[Int]) = {
      val result = Array.fill(rank)(0)
      for ((z, r) <- x.zip(structureCoefficients(j).entries)) {
        for (k <- 0 until rank) {
          result(k) += z * r(k)
        }
      }
      result
    }
    override def multiply(x: Seq[Int], y: Seq[Int]) = {
      val result = Array.fill(rank)(0)
      for (i <- 0 until rank; j <- 0 until rank) {
        val xi = x(i)
        val yj = y(j)
        val r = structureCoefficients(i).entries(j)
        for (k <- 0 until rank) {
          result(k) += xi * yj * r(k)
        }
      }
      result
    }

  }

  private class StructureCoefficientFusionRingWithDimensions(multiplicities: Seq[Matrix[Int]], fieldGenerator: Polynomial[Int], fieldGeneratorApproximation: Double, fieldGeneratorEpsilon: Double, override val dimensions: Seq[Polynomial[Fraction[Int]]]) extends StructureCoefficientFusionRing[Int](multiplicities) with FusionRingWithDimensions {
    override def dimensionField = {
      RealNumberField(fieldGenerator, fieldGeneratorApproximation, fieldGeneratorEpsilon)
    }
  }
}

