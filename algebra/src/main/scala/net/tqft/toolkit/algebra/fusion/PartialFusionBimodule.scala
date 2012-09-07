package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.Rig
import net.tqft.toolkit.algebra.Rigs

trait PartialFusionBimodule {
  def depth: Int

  def addObjects: Iterable[PartialFusionBimodule]
  def increaseDepth: PartialFusionBimodule

  def get: Option[FusionBimodule[Int]] = ???

  protected def leftRingStructureCoefficients: Seq[Matrix[Int]]
  protected def leftModuleStructureCoefficients: Seq[Matrix[Int]]
  protected def rightRingStructureCoefficients: Seq[Matrix[Int]]
  protected def rightModuleStructureCoefficients: Seq[Matrix[Int]]

  protected def folding[E](T: E => Iterable[E])(implicit ev: this.type <:< E): Iterable[E] = {
    def S(iterable: Iterable[E]) = iterable.flatMap(T)
    Iterator.iterate(Iterable[E](this))(S).takeWhile(_.nonEmpty).toIterable.flatten
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

    implicit val rig_? = Rigs.adjoinUnknown[P]

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

    implicit val rig_? = Rigs.adjoinUnknown[P]

    FusionBimodule(leftMultiplication, leftAction, rightMultiplication, rightAction)
  }

  def addMysteryObjects[P: Rig](bimodule: FusionBimodule[P]): FusionBimodule[P or ?] = {
    // We need to add one mystery object at the each of the next two depths
    val lifted = lift(bimodule)
    val depth = bimodule.leftModule.depth
    if (depth % 2 == 0) {
      addEvenMysteryObject(addOddMysteryObject(lifted, depth + 1), depth + 2)
    } else {
      addOddMysteryObject(addEvenMysteryObject(lifted, depth + 1), depth + 2)
    }
  }
  
}

trait EvenPartialFusionBimodule extends PartialFusionBimodule {
  def addLeftObjects = folding[EvenPartialFusionBimodule]({ e => e.addLeftObject })
  def addRightObjects = folding[EvenPartialFusionBimodule]({ e => e.addRightObject })
  override def addObjects: Iterable[PartialFusionBimodule] = {
    for (left <- addLeftObjects; right <- left.addRightObjects) yield right
  }

  def addLeftObject: Iterable[EvenPartialFusionBimodule] = {
    // let's see; we need to add some extra entries corresponding to multiplicities with the new object
    // some of these should be zero (by the grading)
    // others are variable
    // we then need to add an unknown object
    // prepare all the equations, and throw out those involving unknowns...
    // solve those
    // and finally only keep those that actually connected to the new object

    ???
  }
  def addRightObject: Iterable[EvenPartialFusionBimodule]
}

trait OddPartialFusionBimodule extends PartialFusionBimodule {
  override def addObjects = folding[OddPartialFusionBimodule]({ e => e.addObject })

  def addObject: Iterable[OddPartialFusionBimodule]
}