package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.permutations.Permutation

trait FusionBimodule[A] extends FiniteDimensionalFreeModuleOverRig[A] {
  def coefficients = leftRing.coefficients.ensuring(_ == rightRing.coefficients)

  def switch(implicit rig: Rig[A]) = FusionBimodule(rightRing.structureCoefficients, rightModule.structureCoefficients, leftRing.structureCoefficients, leftModule.structureCoefficients)
  
  override def rank = leftRing.rank

  val leftRing: FusionRing[A]
  val rightRing: FusionRing[A]
  def leftModule: leftRing.FusionModule
  def rightModule: rightRing.FusionModule

  def associativityConstraints: Iterator[(A, A)] = {
    leftRing.associativityConstraints ++
      rightRing.associativityConstraints ++
      leftModule.associativityConstraints ++
      rightModule.associativityConstraints ++
      (for (x <- leftRing.basis.iterator; y <- leftModule.basis; z <- rightRing.basis) yield {
        rightModule.act(z, leftModule.act(x, y)).zip(leftModule.act(x, rightModule.act(z, y)))
      }).flatten
  }
  def identityConstraints = leftRing.identityConstraints ++ rightRing.identityConstraints ++ leftModule.identityConstraints ++ rightModule.identityConstraints
  def admissibilityConstraints = {
    leftModule.admissibilityConstraints ++ rightModule.admissibilityConstraints ++ (
      for (m <- leftModule.basis.iterator; n <- rightModule.basis) yield {
        Seq(
          (
            leftRing.innerProduct(leftModule.rightMultiplicationByDuals(m, n), leftModule.rightMultiplicationByDuals(m, n)),
            rightRing.innerProduct(rightModule.rightMultiplicationByDuals(m, m), rightModule.rightMultiplicationByDuals(n, n))),
          (
            rightRing.innerProduct(rightModule.rightMultiplicationByDuals(n, m), rightModule.rightMultiplicationByDuals(n, m)),
            leftRing.innerProduct(leftModule.rightMultiplicationByDuals(m, m), leftModule.rightMultiplicationByDuals(n, n))))
      }).flatten
  }
  def dualityConstraints(leftDuality: Permutation, rightDuality: Permutation) = {
    leftRing.dualityConstraints(leftDuality) ++ leftModule.dualityConstraints(leftDuality) ++ rightRing.dualityConstraints(rightDuality) ++ rightModule.dualityConstraints(rightDuality)
  }

  def verifyAssociativity = {
    associativityConstraints.forall(p => p._1 == p._2)
  }
  def verifyAdmissibility = {
    admissibilityConstraints.forall(p => p._1 == p._2)
  }
  def verifyIdentity = {
    identityConstraints.forall(p => p._1 == p._2)
  }

  def globalDimensionLowerBound(implicit ev: A =:= Int): Double = {
    List(leftRing.globalDimensionLowerBound, leftModule.globalDimensionLowerBound, rightRing.globalDimensionLowerBound, rightModule.globalDimensionLowerBound).max
  }
  
}

trait FusionBimoduleWithLeftDimensions extends FusionBimodule[Int] {
  override val leftRing: FusionRingWithDimensions
  override val rightRing: FusionRing[Int]
  override def leftModule: leftRing.FusionModule
  override def rightModule: rightRing.FusionModule

  def verifyRightSmallerThanLeftInequalities = {
    (for (b <- leftModule.basis.iterator) yield {
      val lb = rightModule.dimensionLowerBounds(b)
      val ub = leftModule.dimensionUpperBounds(b)
      lb < ub
    }).forall(_ == true)
  }

  def rightRingDimensionLowerBounds(x: Seq[Int]): Double = {
    // for each simple y in the bimodule category, compute yx (another object in the bimodule category)
    // d(x) = d(yx)/d(y)

    val lowerBound = (for (y <- leftModule.basis) yield {
      val dxy = leftModule.dimensionLowerBounds(rightModule.act(x, y))
      val dy = leftModule.dimensionUpperBounds(y)
      dxy / dy
    }).max

    List(lowerBound, rightRing.dimensionLowerBounds(x)).max
  }

  def verifyGlobalDimensionInequality = {
    val leftGlobalDimensionUpperBound = (for (b <- leftRing.basis) yield leftRing.dimensionUpperBounds(b) * leftRing.dimensionUpperBounds(b)).sum
    val rightGlobalDimensionLowerBound = (for (b <- rightRing.basis) yield rightRingDimensionLowerBounds(b) * rightRingDimensionLowerBounds(b)).sum
    //    print("[ub: " + leftGlobalDimensionUpperBound+ ", lb: " + rightGlobalDimensionLowerBound + "]")
    rightGlobalDimensionLowerBound < leftGlobalDimensionUpperBound
  }
}

trait FusionBimoduleWithDimensions extends FusionBimoduleWithLeftDimensions {
  override val rightRing: FusionRingWithDimensions
  override def rightModule: rightRing.FusionModule

  def verifyLeftSmallerThanRightInequalities = {
    (for (b <- leftModule.basis.iterator) yield leftModule.dimensionLowerBounds(b) < rightModule.dimensionUpperBounds(b)).forall(_ == true)
  }
  def verifyInequalities = verifyLeftSmallerThanRightInequalities && verifyRightSmallerThanLeftInequalities
}

object FusionBimodule {
  def apply[A: Rig](leftMultiplication: Seq[Matrix[A]], leftAction: Seq[Matrix[A]], rightMultiplication: Seq[Matrix[A]], rightAction: Seq[Matrix[A]]): FusionBimodule[A] = new StructureCoefficientFusionBimodule(leftMultiplication, leftAction, rightMultiplication, rightAction)

  private class StructureCoefficientFusionBimodule[A: Rig](leftMultiplication: Seq[Matrix[A]], leftAction: Seq[Matrix[A]], rightMultiplication: Seq[Matrix[A]], rightAction: Seq[Matrix[A]]) extends FusionBimodule[A] {
    override val coefficients = implicitly[Rig[A]]
    override val leftRing = FusionRing(leftMultiplication)
    override val rightRing = FusionRing(rightMultiplication)

    override val leftModule = leftRing.moduleFromStructureCoefficients(leftAction)
    override val rightModule = rightRing.moduleFromStructureCoefficients(rightAction)
  }
}

object FusionBimoduleWithLeftDimensions {
  def apply(_leftModule: FusionRingWithDimensions#FusionModule, rightMultiplication: Seq[Matrix[Int]], rightAction: Seq[Matrix[Int]]): FusionBimoduleWithLeftDimensions = new FusionBimoduleWithLeftDimensions {
    override val coefficients = Integers
    override val leftRing = _leftModule.fusionRing
    override val rightRing = FusionRing(rightMultiplication)

    override val leftModule = leftRing.moduleFromStructureCoefficients(_leftModule.structureCoefficients)
    override val rightModule = rightRing.moduleFromStructureCoefficients(rightAction)
  }

}
