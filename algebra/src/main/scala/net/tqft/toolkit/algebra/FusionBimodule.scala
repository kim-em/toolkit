package net.tqft.toolkit.algebra

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
  
  def verifyAssociativity = {
    associativityConstraints.find(_ != zero).isEmpty
  }
  def verifyIdentity = {
    identityConstraints.find(_ != zero).isEmpty
  }  

}

trait FusionBimoduleWithLeftDimensions extends FusionBimodule[Int] {
  override val leftRing: FusionRingWithDimensions
  override val rightRing: ConcreteFusionRing
  override  def leftModule: leftRing.FusionModule
  override def rightModule: rightRing.FusionModule

  def verifyRightSmallerThanLeftInequalities = {
    (for(b <- leftModule.basis.iterator) yield {
      val lb = rightModule.dimensionLowerBounds(b)
      val ub = leftModule.dimensionUpperBounds(b)
      println(lb + " < " + ub)
      lb < ub
    }).forall(_ == true)
  }
}

trait FusionBimoduleWithDimensions extends FusionBimoduleWithLeftDimensions {
  override val rightRing: FusionRingWithDimensions
  override def rightModule: rightRing.FusionModule

  def verifyLeftSmallerThanRightInequalities = {
    (for(b <- leftModule.basis.iterator) yield leftModule.dimensionLowerBounds(b) < rightModule.dimensionUpperBounds(b)).forall(_ == true)
  }
  def verifyInequalities = verifyLeftSmallerThanRightInequalities && verifyRightSmallerThanLeftInequalities  
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

object FusionBimoduleWithLeftDimensions {
  def apply(_leftModule: FusionRingWithDimensions#FusionModule, rightMultiplication: Seq[Matrix[Int]], rightAction: Seq[Matrix[Int]]): FusionBimoduleWithLeftDimensions = new FusionBimoduleWithLeftDimensions {
    override val coefficients = Gadgets.Integers
    override val leftRing = _leftModule.fusionRing
    override val rightRing = FusionRing(rightMultiplication)

    override val leftModule = leftRing.moduleFromStructureCoefficients(_leftModule.structureCoefficients)
    override val rightModule = rightRing.moduleFromStructureCoefficients(rightAction)    
  }

  
}
