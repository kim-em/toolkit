package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.Rig
import net.tqft.toolkit.algebra.Rigs
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomialAlgebra
import net.tqft.toolkit.algebra.diophantine.BoundedDiophantineSolver
import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial
import scala.collection.parallel.ParIterable

trait PartialFusionBimodule {
  def depth: Int

  def addObjects(boundary: FusionBimodule[Int] => Boolean): Iterable[PartialFusionBimodule]
  def increaseDepth: PartialFusionBimodule

  /*protected*/ lazy val fusionBimodule = FusionBimodule(leftRingStructureCoefficients, leftModuleStructureCoefficients, rightRingStructureCoefficients, rightModuleStructureCoefficients)

  def get: Option[FusionBimodule[Int]] = {
    fusionBimodule match {
      case b if b.verifyAdmissibility && b.verifyAssociativity => Some(b)
      case _ => None
    }
  }

  protected def leftRingStructureCoefficients: Seq[Matrix[Int]]
  protected def leftModuleStructureCoefficients: Seq[Matrix[Int]]
  protected def rightRingStructureCoefficients: Seq[Matrix[Int]]
  protected def rightModuleStructureCoefficients: Seq[Matrix[Int]]

  protected def folding[E](T: E => Iterable[E])(implicit ev: this.type <:< E): Iterable[E] = {
    def S(iterable: ParIterable[E]) = iterable.flatMap(T)
    Iterator.iterate(ParIterable[E](this))(S).takeWhile(_.nonEmpty).map(_.seq).flatten.toIterable
  }
}

object PartialFusionBimodule {
  import net.tqft.toolkit.algebra.Unknowns.?
  import net.tqft.toolkit.UnionTypes._
  private def lift[P: Rig](bimodule: FusionBimodule[P]): FusionBimodule[P or ?] = {
    val leftMultiplication = bimodule.leftRing.structureCoefficients.map(_.mapEntries(x => x: (P or ?)))
    val leftAction = bimodule.leftModule.structureCoefficients.map(_.mapEntries(x => x: (P or ?)))
    val rightMultiplication = bimodule.rightRing.structureCoefficients.map(_.mapEntries(x => x: (P or ?)))
    val rightAction = bimodule.rightModule.structureCoefficients.map(_.mapEntries(x => x: (P or ?)))
    implicit val rig = Rigs.adjoinUnknown[P]
    FusionBimodule(leftMultiplication, leftAction, rightMultiplication, rightAction)
  }

  private def addEvenMysteryObject[P: Rig](bimodule: FusionBimodule[P or ?], depth: Int): FusionBimodule[P or ?] = {
    val rig = implicitly[Rig[P]]

    def addRingObject(matrices: Seq[Matrix[P or ?]], d: Int => Int): Seq[Matrix[P or ?]] = {
      val rank = matrices.size
      // each matrix has an extra row and column, and there's one extra matrix
      (for ((matrix, i) <- matrices.zipWithIndex) yield {
        matrix.appendColumn(
          Seq.tabulate(rank)(j => if (d(i) + d(j) < depth) rig.zero else ?)).appendRow(
            Seq.fill(rank + 1)(?))
      }) :+ Matrix[P or ?](rank + 1, Seq.fill(rank + 1)(Seq.fill(rank + 1)(?)))
    }

    def addModuleObject(matrices: Seq[Matrix[P or ?]]): Seq[Matrix[P or ?]] = {
      val rank = matrices.size
      for ((matrix, i) <- matrices.zipWithIndex) yield {
        matrix.appendRow(
          Seq.fill(rank)(?))
      }
    }

    val leftMultiplication = addRingObject(bimodule.leftRing.structureCoefficients, bimodule.leftModule.depthOfRingObject _)
    val leftAction = addModuleObject(bimodule.leftModule.structureCoefficients)
    val rightMultiplication = addRingObject(bimodule.rightRing.structureCoefficients, bimodule.rightModule.depthOfRingObject _)
    val rightAction = addModuleObject(bimodule.rightModule.structureCoefficients)

    implicit val rigWith_? = Rigs.adjoinUnknown[P]

    FusionBimodule(leftMultiplication, leftAction, rightMultiplication, rightAction)
  }
  private def addOddMysteryObject[P: Rig](bimodule: FusionBimodule[P or ?], depth: Int): FusionBimodule[P or ?] = {
    val rig = implicitly[Rig[P]]

    def addModuleObject(matrices: Seq[Matrix[P or ?]], dr: Int => Int, dm: Int => Int): Seq[Matrix[P or ?]] = {
      val rank = matrices.size
      val frank = matrices.head.numberOfRows
      // each matrix has an extra column, and there's one extra matrix
      (for ((matrix, i) <- matrices.zipWithIndex) yield {
        matrix.appendColumn(
          Seq.tabulate(frank)(j => if (dm(i) + dr(j) < depth) rig.zero else ?))
      }) :+ Matrix[P or ?](rank + 1, Seq.fill(frank)(Seq.fill(rank + 1)(?)))
    }

    val leftMultiplication = bimodule.leftRing.structureCoefficients
    val leftAction = addModuleObject(bimodule.leftModule.structureCoefficients, bimodule.leftModule.depthOfRingObject _, bimodule.leftModule.depthOfModuleObject _)
    val rightMultiplication = bimodule.rightRing.structureCoefficients
    val rightAction = addModuleObject(bimodule.rightModule.structureCoefficients, bimodule.rightModule.depthOfRingObject _, bimodule.rightModule.depthOfModuleObject _)

    implicit val rigWith_? = Rigs.adjoinUnknown[P]

    FusionBimodule(leftMultiplication, leftAction, rightMultiplication, rightAction)
  }

  def addMysteryObjects[P: Rig](bimodule: FusionBimodule[P], depth: Int): FusionBimodule[P or ?] = {
    // We need to add one mystery object at depth and another at depth+1
    val lifted = lift(bimodule)
    if (depth % 2 == 0) {
      addOddMysteryObject(addEvenMysteryObject(lifted, depth), depth + 1)
    } else {
      addEvenMysteryObject(addOddMysteryObject(lifted, depth), depth + 1)
    }
  }

}

case class EvenPartialFusionBimodule(
  depth: Int,
  leftRingStructureCoefficients: Seq[Matrix[Int]],
  leftModuleStructureCoefficients: Seq[Matrix[Int]],
  rightRingStructureCoefficients: Seq[Matrix[Int]],
  rightModuleStructureCoefficients: Seq[Matrix[Int]]) extends PartialFusionBimodule {

  def switch = EvenPartialFusionBimodule(depth, rightRingStructureCoefficients, rightModuleStructureCoefficients, leftRingStructureCoefficients, leftModuleStructureCoefficients)

  override def increaseDepth = OddPartialFusionBimodule(depth + 1, leftRingStructureCoefficients, leftModuleStructureCoefficients, rightRingStructureCoefficients, rightModuleStructureCoefficients)

  def addLeftSelfDualObjects(boundary: FusionBimodule[Int] => Boolean) = folding[EvenPartialFusionBimodule]({ e => e.addLeftObjects(boundary, 1) })
  def addLeftDualObjectPairs(boundary: FusionBimodule[Int] => Boolean) = folding[EvenPartialFusionBimodule]({ e => e.addLeftObjects(boundary, 2) })
  override def addObjects(boundary: FusionBimodule[Int] => Boolean): Iterable[PartialFusionBimodule] = {
    val switchedBoundary = { b: FusionBimodule[Int] => boundary(b.switch) }
    for (left1 <- addLeftSelfDualObjects(boundary); left2 <- addLeftDualObjectPairs(boundary); right1 <- left2.switch.addLeftSelfDualObjects(switchedBoundary); right2 <- right1.addLeftDualObjectPairs(switchedBoundary)) yield right2.switch
  }

  private type V = (Int, Int, Int, Int)
  private val polynomialAlgebra = implicitly[MultivariablePolynomialAlgebra[Int, V]]
  private def asPolynomials(ms: Seq[Matrix[Int]]) = ms.map(m => m.mapEntries(x => polynomialAlgebra.constant(x)))

  def addLeftObjects(boundary: FusionBimodule[Int] => Boolean, number: Int): Iterable[EvenPartialFusionBimodule] = {

    def leftRingVariable(i: Int, j: Int, k: Int): MultivariablePolynomial[Int, V] = {
      if (i >= fusionBimodule.leftRing.rank || j >= fusionBimodule.leftRing.rank) {
        polynomialAlgebra.monomial((0, i, j, k))
      } else {
        val di = fusionBimodule.leftModule.depthOfRingObject(i)
        val dj = fusionBimodule.leftModule.depthOfRingObject(j)
        val dk = if (k >= fusionBimodule.leftRing.rank) {
          depth
        } else {
          fusionBimodule.leftModule.depthOfRingObject(k)
        }
        if (dk <= di + dj && dk >= scala.math.abs(di - dj)) {
          polynomialAlgebra.monomial((0, i, j, k))
        } else {
          polynomialAlgebra.zero
        }
      }
    }
    def leftModuleVariable(i: Int, j: Int, k: Int): MultivariablePolynomial[Int, V] = {
      require(j >= fusionBimodule.leftRing.rank)
      val di = fusionBimodule.leftModule.depthOfModuleObject(i)
      val dj = depth
      val dk = fusionBimodule.leftModule.depthOfModuleObject(k)
      if (dk <= di + dj && dk >= scala.math.abs(di - dj)) {
        polynomialAlgebra.monomial((1, i, j, k))
      } else {
        polynomialAlgebra.zero
      }
    }

    def addLeftRingVariableObject(ms: Seq[Matrix[MultivariablePolynomial[Int, V]]]): Seq[Matrix[MultivariablePolynomial[Int, V]]] = {
      val rank = ms.size
      (for ((matrix, i) <- ms.zipWithIndex) yield {
        // add an extra row and column
        matrix
          .appendColumn(Seq.tabulate(rank)(j => leftRingVariable(i, j, rank + 1)))
          .appendRow(Seq.tabulate(rank + 1)(k => leftRingVariable(i, rank + 1, k)))
      }) :+ {
        Matrix.tabulate(rank + 1, rank + 1)({ (i, j) =>
          leftRingVariable(rank + 1, i, j)
        })
      }
    }

    def addLeftModuleVariableObject(ms: Seq[Matrix[MultivariablePolynomial[Int, V]]]): Seq[Matrix[MultivariablePolynomial[Int, V]]] = {
      val frrank = ms.head.numberOfRows
      val fmrank = ms.size
      for ((matrix, i) <- ms.zipWithIndex) yield {
        // add an extra row
        matrix.appendRow(Seq.tabulate(fmrank)(k => leftModuleVariable(i, frrank + 1, k)))
      }
    }

    val variableLeftRing = Seq.fill(number)(addLeftRingVariableObject _).reduce(_.andThen(_))(asPolynomials(leftRingStructureCoefficients))
    val variableLeftModule = Seq.fill(number)(addLeftModuleVariableObject _).reduce(_.andThen(_))(asPolynomials(leftModuleStructureCoefficients))
    val variableRightRing = asPolynomials(rightRingStructureCoefficients)
    val variableRightModule = asPolynomials(rightModuleStructureCoefficients)

    def reconstituteBimodule(m: Map[V, Int]): EvenPartialFusionBimodule = {
      val newLeftRingStructureCoefficients = variableLeftRing.map(_.mapEntries({
        case p if p.totalDegree == Some(1) => m(p.terms.head._1.keySet.iterator.next)
        case p => p.constantTerm
      }))
      val newLeftModuleStructureCoefficients = variableLeftModule.map(_.mapEntries({
        case p if p.totalDegree == Some(1) => m(p.terms.head._1.keySet.iterator.next)
        case p => p.constantTerm
      }))
      EvenPartialFusionBimodule(depth, newLeftRingStructureCoefficients, newLeftModuleStructureCoefficients, rightRingStructureCoefficients, rightModuleStructureCoefficients)
    }

    val mysteryBimodule = PartialFusionBimodule.addMysteryObjects(FusionBimodule(variableLeftRing, variableLeftModule, variableRightRing, variableRightModule), depth)

    // watch out, we need to add an extra element to each of the dualities, for the mystery object
    val leftDuality = fusionBimodule.leftRing.duality.get ++ (number match {
      case 1 => Seq(fusionBimodule.leftRing.rank, fusionBimodule.leftRing.rank + 1)
      case 2 => Seq(fusionBimodule.leftRing.rank + 1, fusionBimodule.leftRing.rank, fusionBimodule.leftRing.rank + 2)
    })
    val rightDuality = fusionBimodule.rightRing.duality.get :+ fusionBimodule.rightRing.rank

    val mysteryEquations = mysteryBimodule.admissibilityConstraints ++ mysteryBimodule.associativityConstraints ++ mysteryBimodule.identityConstraints ++ mysteryBimodule.dualityConstraints(leftDuality, rightDuality)
    val polynomials = mysteryEquations.collect({
      case (Left(p), Left(q)) => polynomialAlgebra.subtract(p, q)
    }).toSeq
    val variables = variableLeftRing.flatMap(_.entries.flatten).flatMap(_.variables) ++ variableLeftModule.flatMap(_.entries.flatten).flatMap(_.variables)
    def orderingBoundary(b: FusionBimodule[Int]): Boolean = {
      boundary(b) && {
        import net.tqft.toolkit.collections.LexicographicOrdering._
        val duality = fusionBimodule.leftRing.duality.get
        number match {
          case 1 => {
            // find all other self-dual objects at the same depth, and require that the new rows of the module matrices are lexicographically less than each
            val selfDualObjectsAtDepth = for (i <- fusionBimodule.leftModule.objectsAtDepth(depth); if duality(i) == i) yield i
            val m = b.leftModule.structureCoefficients.map(_.entries.seq.last)
            selfDualObjectsAtDepth.forall({ i =>
              m <= b.leftModule.structureCoefficients.map(_.entries(i))
            })
          }
          case 2 => {
            // find all dual pairs at the same depth, and require that the new rows of the module matrices are lexicographically less than each
            val dualPairsAtDepth = for (i <- fusionBimodule.leftModule.objectsAtDepth(depth); j = duality(i); if i < j) yield (i, j)
            val m = b.leftModule.structureCoefficients.map(_.entries.seq.reverse.take(2))
            m.map(_(0)) < m.map(_(1)) && dualPairsAtDepth.forall({
              case (i, j) => {
                m <= b.leftModule.structureCoefficients.map(m => Seq(m.entries(i), m.entries(j)))
              }
            })
          }
        }
      }
    }

    val (solutions, tooHard) = BoundedDiophantineSolver.solve(polynomials, variables, boundary = Some({ m: Map[V, Int] => orderingBoundary(reconstituteBimodule(m).fusionBimodule) }))
    require(tooHard.isEmpty)

    def newObjectsConnected(b: EvenPartialFusionBimodule) = {
      b.leftModuleStructureCoefficients.map(_.entries.seq.reverse.take(number)).transpose.map(_.flatten).forall(_.exists(_ != 0))
    }

    for (solution <- solutions; b = reconstituteBimodule(solution); if newObjectsConnected(b)) yield b
  }

}

case class OddPartialFusionBimodule(
  depth: Int,
  leftRingStructureCoefficients: Seq[Matrix[Int]],
  leftModuleStructureCoefficients: Seq[Matrix[Int]],
  rightRingStructureCoefficients: Seq[Matrix[Int]],
  rightModuleStructureCoefficients: Seq[Matrix[Int]]) extends PartialFusionBimodule {
  override def increaseDepth = EvenPartialFusionBimodule(depth + 1, leftRingStructureCoefficients, leftModuleStructureCoefficients, rightRingStructureCoefficients, rightModuleStructureCoefficients)

  override def addObjects(boundary: FusionBimodule[Int] => Boolean) = folding[OddPartialFusionBimodule]({ e => e.addObject(boundary) })

  def addObject(boundary: FusionBimodule[Int] => Boolean): Iterable[OddPartialFusionBimodule] = ???
}