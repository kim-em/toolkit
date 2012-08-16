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
