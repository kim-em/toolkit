package net.tqft.toolkit.algebra

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
  def identityConstraints = for (x <- basis.iterator) yield x == multiply(one, x) && x == multiply(x, one)

  def verifyAssociativity = associativityConstraints.map(_ == zero).reduce(_ && _)
  def verifyIdentity = identityConstraints.reduce(_ && _)

  lazy val structureCoefficients = for (y <- basis) yield for (x <- basis) yield multiply(x, y)

  trait FusionModule extends FiniteDimensionalFreeModule[A] {
    override def coefficients = fr.coefficients

    def act(x: Seq[A], m: Seq[A]): Seq[A]
    def associativityConstraints = for (x <- fr.basis.iterator; y <- fr.basis; z <- basis) yield subtract(act(x, act(y, z)), act(fr.multiply(x, y), z))
    def verifyAssociativity = associativityConstraints.map(_ == zero).reduce(_ && _)
    def asMatrix(x: Seq[A]) = new Matrix(rank, for (b <- basis) yield act(x, b))

    def structureCoefficients = for (y <- basis) yield for (x <- fr.basis) yield act(x, y)

    override def equals(other: Any) = {
      other match {
        case other: fr.FusionModule => structureCoefficients == other.structureCoefficients
        case _ => false
      }
    }
    override def hashCode = (fr, structureCoefficients).hashCode
  }

  protected class StructureCoefficientFusionModule(matrices: Seq[Matrix[A]]) extends FusionModule {
    override val rank = matrices.size
    override def act(x: Seq[A], m: Seq[A]) = {
      require(m.nonEmpty)
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
    (new StructureCoefficientFusionModule(matrices)).ensuring(fm => matrices.map(_.entries) == fm.structureCoefficients)
  }

  object regularModule extends FusionModule {
    override val rank = fr.rank
    override def act(x: Seq[A], m: Seq[A]) = fr.multiply(x, m)
  }
}

//
//trait FusionBimodule[A] extends FiniteDimensionalFreeModule[A] {
//  val leftFusionRing: FusionRing[A]
//  val rightFusionRing: FusionRing[A]
//  def act(left: Seq[A], m: Seq[A], right: Seq[A]): Seq[A]  
//}

trait FusionRingWithDimensions extends FusionRing[Int] { fr =>
  def dimensionField: RealNumberField[Int, Double]
  def dimensions: Seq[Polynomial[Fraction[Int]]]
  def dimensionOf(x: Seq[Int]): Polynomial[Fraction[Int]] = {
    val polynomials = Polynomials.over(Gadgets.Rationals)
    import Implicits.integersAsRationals
    polynomials.add(x.zip(dimensions).map(p => polynomials.scalarMultiply(p._1, p._2)))
  }

  def dimensionBounds(x: Seq[Int]): Double = {
    import Implicits.Integers
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
    import Implicits.Rationals

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
            import Implicits.{ Integers, integersAsRationals }
            implicit val polynomials = Polynomials.over(Gadgets.Rationals)
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

            val result2 = (for (
              (m, mi) <- structureCoefficients.iterator.zipWithIndex;
              yj <- 0 until rank;
              if (m.entries(yj).drop(n0).forall(_ == 0))
            ) yield {
              val `x(ym)` = (for (
                (a, mk) <- m.entries(yj).take(n0).zipWithIndex
              ) yield {
                integerMatrices.scalarMultiply(a, structureCoefficients(mk))
              }).reduce(integerMatrices.add _)

              val `(xy)m` = integerMatrices.compose(Matrix(fr.rank, fr.structureCoefficients(yj)), m)

              `x(ym)` == `(xy)m`
            }).forall(_ == true)

            //            val result0 = if(n == n0) {
            //              Some(fr.moduleFromStructureCoefficients(structureCoefficients).verifyAssociativity)
            //            } else {
            //              None
            //            }

            //            val constraints = Unknowns(fr).moduleFromStructureCoefficients(partialStructureCoefficients).associativityConstraints.flatten
            //            val result1 = !constraints.exists(x => x.nonEmpty && x.get > 0)
//            result1.ensuring(_ == result2)

            result2
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
      }).flatten.toList.distinct
      
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

object FusionRing {
  def apply[A: Ring](multiplicities: Seq[Seq[Seq[A]]]): FusionRing[A] = new StructureCoefficientFusionRing(multiplicities)
  def apply(multiplicities: Seq[Seq[Seq[Int]]], fieldGenerator: Polynomial[Int], fieldGeneratorApproximation: Double, fieldGeneratorEpsilon: Double, dimensions: Seq[Polynomial[Fraction[Int]]]): FusionRingWithDimensions = new StructureCoefficientFusionRingWithDimensions(multiplicities, fieldGenerator, fieldGeneratorApproximation, fieldGeneratorEpsilon, dimensions)

  private class StructureCoefficientFusionRing[A: Ring](multiplicities: Seq[Seq[Seq[A]]]) extends FusionRing[A] {
    override lazy val coefficients = implicitly[Ring[A]]
    override lazy val rank = multiplicities.size

    override def multiply(x: Seq[A], y: Seq[A]) = {
      val zero = coefficients.zero
      val terms = for ((xi, i) <- x.zipWithIndex; if (xi != zero); (yj, j) <- y.zipWithIndex; if (yj != zero)) yield for (k <- 0 until rank) yield coefficients.multiply(xi, yj, multiplicities(i)(j)(k))
      val result = add(terms)
      result
    }
  }

  private class StructureCoefficientFusionRingWithDimensions(multiplicities: Seq[Seq[Seq[Int]]], fieldGenerator: Polynomial[Int], fieldGeneratorApproximation: Double, fieldGeneratorEpsilon: Double, override val dimensions: Seq[Polynomial[Fraction[Int]]]) extends StructureCoefficientFusionRing[Int](multiplicities)(Gadgets.Integers) with FusionRingWithDimensions {
    override def dimensionField = {
      import Implicits.{ Integers, Doubles }
      RealNumberField(fieldGenerator, fieldGeneratorApproximation, fieldGeneratorEpsilon)(Gadgets.Integers, Gadgets.Doubles)
    }
  }
}

object Goals extends App {
  val haagerup4FusionRing: FusionRingWithDimensions = {
    val haagerup4Multiplicities: Seq[Seq[Seq[Int]]] =
      List(
        List(List(1, 0, 0, 0), List(0, 1, 0, 0), List(0, 0, 1, 0), List(0, 0, 0, 1)),
        List(List(0, 1, 0, 0), List(1, 2, 2, 1), List(0, 2, 1, 1), List(0, 1, 1, 1)),
        List(List(0, 0, 1, 0), List(0, 2, 1, 1), List(1, 1, 1, 1), List(0, 1, 1, 0)),
        List(List(0, 0, 0, 1), List(0, 1, 1, 1), List(0, 1, 1, 0), List(1, 1, 0, 0)))

    val haagerup4FieldGenerator: Polynomial[Int] = {
      import Implicits.Integers
      Polynomial(2 -> 1, 0 -> -13)
    }
    val haagerup4FieldGeneratorApproximation: Double = 3.60555
    val haagerup4FieldGeneratorEpsilon: Double = 0.00001
    val haagerup4Dimensions: Seq[Polynomial[Fraction[Int]]] = {
      import Implicits.{ Integers, Rationals, integersAsRationals }
      Seq(
        Polynomial(0 -> Fraction(1, 1)),
        Polynomial(0 -> Fraction(5, 2), 1 -> Fraction(1, 2)),
        Polynomial(0 -> Fraction(3, 2), 1 -> Fraction(1, 2)),
        Polynomial(0 -> Fraction(1, 2), 1 -> Fraction(1, 2)))
    }
    FusionRing(haagerup4Multiplicities, haagerup4FieldGenerator, haagerup4FieldGeneratorApproximation, haagerup4FieldGeneratorEpsilon, haagerup4Dimensions).ensuring(_.verifyAssociativity).ensuring(_.verifyIdentity)
  }
  val AH1FusionRing: FusionRingWithDimensions = {
    val AH1Multiplicities: Seq[Seq[Seq[Int]]] =
      List(
        List(List(1, 0, 0, 0, 0, 0), List(0, 1, 0, 0, 0, 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 1, 0), List(0, 0, 0, 0, 0, 1)),
        List(List(0, 1, 0, 0, 0, 0), List(1, 1, 0, 0, 1, 0), List(0, 0, 1, 0, 0, 1), List(0, 0, 0, 0, 1, 1), List(0, 1, 0, 1, 1, 1), List(0, 0, 1, 1, 1, 2)),
        List(List(0, 0, 1, 0, 0, 0), List(0, 0, 1, 0, 0, 1), List(1, 1, 1, 0, 0, 1), List(0, 0, 0, 1, 1, 1), List(0, 0, 0, 1, 1, 2), List(0, 1, 1, 1, 2, 2)),
        List(List(0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 1, 1), List(0, 0, 0, 1, 1, 1), List(1, 0, 1, 1, 1, 1), List(0, 1, 1, 1, 1, 2), List(0, 1, 1, 1, 2, 3)),
        List(List(0, 0, 0, 0, 1, 0), List(0, 1, 0, 1, 1, 1), List(0, 0, 0, 1, 1, 2), List(0, 1, 1, 1, 1, 2), List(1, 1, 1, 1, 2, 3), List(0, 1, 2, 2, 3, 4)),
        List(List(0, 0, 0, 0, 0, 1), List(0, 0, 1, 1, 1, 2), List(0, 1, 1, 1, 2, 2), List(0, 1, 1, 1, 2, 3), List(0, 1, 2, 2, 3, 4), List(1, 2, 2, 3, 4, 6)))

    val AH1FieldGenerator: Polynomial[Int] = {
      import Implicits.Integers
      Polynomial(2 -> 1, 0 -> -17)
    }
    val AH1FieldGeneratorApproximation: Double = 4.12311
    val AH1FieldGeneratorEpsilon: Double = 0.00001
    val AH1Dimensions: Seq[Polynomial[Fraction[Int]]] = {
      import Implicits.{ Integers, Rationals, integersAsRationals }
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
  test(AH1FusionRing)

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