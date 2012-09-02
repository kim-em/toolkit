package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.Rig

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
  def addMysteryObjects[P:Rig](bimodule: FusionBimodule[P]): FusionBimodule[Option[P]] = {
    // We need to add one mystery object at the current highest depth, and then one at the next highest...
    ???
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